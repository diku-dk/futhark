{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive,
-- and - it's certainly worth considering when we can express such
-- transformations in-place.
module Futhark.Transform.FirstOrderTransform
  ( transformProg

  , Transformer
  , transformBindingRecursively
  , transformLambda
  , transformSOAC

  -- * Utility
  , doLoopMapAccumL
  )
  where

import Control.Applicative
import Control.Monad.State
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import Data.List
import Prelude

import qualified Futhark.Representation.AST as AST
import Futhark.Representation.SOACS
import Futhark.Transform.Rename
import Futhark.MonadFreshNames
import Futhark.Tools

-- | Perform the first-order transformation on an Futhark program.
transformProg :: (MonadFreshNames m, Bindable tolore,
                  LetAttr SOACS ~ LetAttr tolore) =>
                 Prog -> m (AST.Prog tolore)
transformProg = intraproceduralTransformation transformFunDec

transformFunDec :: (MonadFreshNames m, Bindable tolore,
                    LetAttr SOACS ~ LetAttr tolore) =>
                   FunDec -> m (AST.FunDec tolore)
transformFunDec (FunDec fname rettype params body) = do
  (body',_) <-
    runBinderEmptyEnv $
    localScope (scopeOfFParams params) $
    insertBindingsM $
    transformBody body
  return $ FunDec fname rettype params body'

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m = (MonadBinder m,
                      Bindable (Lore m),
                      LocalScope (Lore m) m,
                      LetAttr SOACS ~ LetAttr (Lore m),
                      LParamAttr SOACS ~ LParamAttr (Lore m))

transformBody :: Transformer m =>
                 Body -> m (AST.Body (Lore m))
transformBody (Body () bnds res) = insertBindingsM $ do
  mapM_ transformBindingRecursively bnds
  return $ resultBody res

-- | First transform any nested 'Body' or 'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformBindingRecursively :: Transformer m =>
                               Binding -> m ()

transformBindingRecursively (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- localScope (scopeOfLoopForm form) $
           localScope (scopeOfFParams $ map fst merge) $
           transformBody body
  letBind_ pat $ LoopOp $ DoLoop res (map cast merge) form body'
  where cast (Param name t, se) = (Param name t, se)

transformBindingRecursively (Let pat () (Op soac)) =
  transformSOAC pat =<< mapSOACM soacTransform soac
  where soacTransform = identitySOACMapper { mapOnSOACLambda = transformLambda
                                           , mapOnSOACExtLambda = transformExtLambda
                                           }

transformBindingRecursively (Let pat () e) =
  letBind_ pat =<< mapExpM transform e
  where transform = identityMapper { mapOnBody = transformBody
                                   , mapOnRetType = return
                                   , mapOnFParam = return
                                   , mapOnOp = fail "Unhandled Op in first order transform"
                                   }

-- | Transform a single 'SOAC' into a do-loop.  The body of the lambda
-- is untouched, and may or may not contain further 'SOAC's depending
-- on the given lore.
transformSOAC :: Transformer m =>
                 AST.Pattern (Lore m)
              -> SOAC (Lore m)
              -> m ()
transformSOAC pat (Map cs width fun arrs) = do
  let i = lambdaIndex fun
      out_ts = mapType width fun
  resarr <- resultArray out_ts
  outarrs <- forM out_ts $ \t ->
             newIdent "map_outarr" t
  let outarrs_names = map identName outarrs
      merge = loopMerge outarrs (map Var resarr)
  loopbody <- runBodyBinder $
              localScope (HM.insert i IndexInfo $
                          scopeOfFParams $ map fst merge) $ do
    x <- bindLambda fun (index cs arrs (Var i))
    dests <- letwith cs outarrs_names (pexp $ Var i) $ map (PrimOp . SubExp) x
    return $ resultBody $ map Var dests
  letBind_ pat $ LoopOp $
    DoLoop outarrs_names merge (ForLoop i width) loopbody

transformSOAC pat (ConcatMap cs _ fun inputs) = do
  arrs <- forM inputs $ \input -> do
    fun' <- renameLambda fun
    let funparams = lambdaParams fun'
        (ctxparams, valparams) =
          splitAt (length funparams-length input) funparams
        fun'' = fun' { lambdaParams = valparams }
    shapemap <- shapeMapping (map paramType valparams) <$>
                mapM lookupType input
    forM_ (HM.toList shapemap) $ \(size,se) ->
      when (size `elem` map paramName ctxparams) $
        letBindNames'_ [size] $ PrimOp $ SubExp se
    input' <- forM (zip valparams input) $ \(p,v) ->
      letExp "concatMap_reshaped_input" $
      shapeCoerce [] (arrayDims $ paramType p) v
    vs <- bindLambda fun'' (map (PrimOp . SubExp . Var) input')
    mapM (letExp "concatMap_fun_res" . PrimOp . SubExp) vs
  emptyarrs <- mapM (letExp "empty")
               [ PrimOp $ ArrayLit [] t | t <- lambdaReturnType fun ]
  let concatArrays (arr, arrs') = do
        let plus x y = eBinOp Plus x y Int
        n <- arraySize 0 <$> lookupType arr
        ms <- mapM (liftM (arraySize 0) . lookupType) arrs'
        ressize <- letSubExp "concatMap_result_size" =<<
                   foldl plus
                   (pure $ PrimOp $ SubExp n)
                   (map (pure . PrimOp . SubExp) ms)
        res <- letExp "concatMap_result" $ PrimOp $ Concat cs arr arrs' ressize
        return $ PrimOp $ Copy res

      nonempty :: [VName] -> Maybe (VName, [VName])
      nonempty []     = Nothing
      nonempty (x:xs) = Just (x, xs)

  ses <-case mapM nonempty $ transpose arrs of
          Nothing ->
            return $ Constant (IntVal 0) : map Var emptyarrs
          Just arrs' -> do
            concatted_arrs <- mapM (letSubExp "concatMap_result" <=< concatArrays) arrs'
            arrts <- mapM subExpType concatted_arrs
            return $ arraysSize 0 arrts : concatted_arrs
  forM_ (zip (patternNames pat) ses) $ \(name, se) ->
    letBindNames' [name] $ PrimOp $ SubExp se

transformSOAC pat (Reduce cs width _ fun args) = do
  (acc, initacc) <- newFold $ zip accexps accts
  arrts <- mapM lookupType arrexps
  inarrs <- mapM (newIdent "reduce_inarr") arrts
  arrexps' <- mapM (copyIfArray . Var) arrexps
  let merge = loopMerge (inarrs++acc) (arrexps'++initacc)
  loopbody <- runBodyBinder $ localScope (scopeOfFParams $ map fst merge) $ do
    acc' <- bindLambda fun
            (map (PrimOp . SubExp . Var . identName) acc ++
             index cs (map identName inarrs) (Var i))
    return $ resultBody (map (Var . identName) inarrs ++ acc')
  letBind_ pat $ LoopOp $
    DoLoop (map identName acc) merge
    (ForLoop i width) loopbody
  where i = lambdaIndex fun
        (accexps, arrexps) = unzip args
        accts = map paramType $ take (length accexps) $ lambdaParams fun

transformSOAC pat (Scan cs width fun args) = do
  (acc, initacc) <- newFold $ zip accexps accts
  arrts <- mapM lookupType arrexps
  initarr <- resultArray arrts
  arr <- mapM (newIdent "scan_arr" ) arrts
  let arr_names = map identName arr
      merge = loopMerge (acc ++ arr) (initacc ++ map Var initarr)
  loopbody <- insertBindingsM $ localScope (scopeOfFParams $ map fst merge) $ do
    x <- bindLambda fun (map (PrimOp . SubExp . Var . identName) acc ++
                              index cs arrexps (Var i))
    dests <- letwith cs arr_names (pexp (Var i)) $ map (PrimOp . SubExp) x
    irows <- letSubExps "row" $ index cs dests $ Var i
    rowcopies <- mapM copyIfArray irows
    return $ resultBody $ rowcopies ++ map Var dests
  letBind_ pat $ LoopOp $
    DoLoop (map identName arr) merge
    (ForLoop i width) loopbody
  where i = lambdaIndex fun
        (accexps, arrexps) = unzip args
        accts = map paramType $ take (length accexps) $ lambdaParams fun

transformSOAC pat (Redomap cs width _ _ innerfun accexps arrexps) = do
  let map_arr_tps = drop (length accexps) $ lambdaReturnType innerfun
  maparrs <- resultArray [ arrayOf t (Shape [width]) NoUniqueness
                         | t <- map_arr_tps ]
  arrexps' <- mapM copyIfArrayName arrexps
  letBind_ pat =<< doLoopMapAccumL cs width innerfun accexps arrexps' maparrs

-- | Translation of STREAM is non-trivial and quite incomplete for the moment!
-- Assumming size of @A@ is @m@, @?0@ has a known upper bound @U@, and @?1@
-- is purely existential, the intent is to translate a stream exp such as:
-- @stream (fn {int,[int,m],[int,?0],[real,?1]@
-- @           (int chunkloc, int i, int acc, *[int] a) =>@
-- @               body in {acc', x, y, z}@
-- @       , acc0, A)@
-- into a loop in which the input array is streamed by splitting each time
-- chunk elements from it, and using the rest as a loop-merge variable.
-- 1. For an array of exact upper bound @x@, we insert a prologue loop that
--    in-place updates the local-chunk elements to a global array, which
--    is statically allocated outside the loop. The global array needs of
--    course to become a loop-merge variable.
-- 2. For an array of known outermost size upper bound U, we also introduce
--    an induction variable that is incremented at the end of the loop with
--    the current size of the local array, and obviously the copy-back-loop
--    counter is the size of the local array @y@.
-- 3. Finally for a purely-existential outer size, e.g., @z@ we initialize
--    the global array outer dimension with the outermost size of the stream,
--    and maintain two induction variables as merge variables of the loop:
--    one that keeps the allocated size of the outermost dimension and
--    one that keeps the current outermost size of the array
--    (pushback vectors)
-- A Loop pseudocode is:
-- @let chunkglb = 16 in@
-- @let (Xglb0,Yglb0,Zglb0)=(scratch(N,..),scratch(U,..),scratch(N,..)) in@
-- loop( {z_al, y_iv, z_iv, acc, Xglb, Yglb, Zglb} =   @
-- @      {0,    0,    0,    acc0,Xglb0,Yglb0,Zglb0} )=@
-- @  for i_chk < (N+chunkglb-1)/chunkglb do                        @
-- @    let i        = chunkglb * i_chk            in               @
-- @    let diff0    = i + chunkglb - N            in               @
-- @    let diff     = diff0 > 0 ? diff0 : 0       in               @
-- @    let chunkloc = chunkglb - diff             in               @
-- @    let (_,a_chg)= split((i-diff, chunkglb),A) in               @
-- @    let a_chg*   = copy(a_chg)                 in               @
-- @    let (_,a) = split((diff,chunkloc), a_chg*) in               @
-- @    ... body(a) ...                                             @
-- @    ...............                                             @
-- @    let z_iv' = z_iv + size(0,z)               in               @
-- @    let {z_al',Zglb'} =                                         @
-- @      if z_iv' <= z_al then {z_al, Zglb}                        @
-- @      else let Znew = scratch(2*z_iv') in                       @
-- @           loop(Znew) = for j < z_iv do                         @
-- @               let Znew[j] = Zglb[j]                            @
-- @               in  Znew                                         @
-- @           in {2*z_iv', Znew}                                   @
-- @    loop (Zglb') = for j < size(0,z) do                         @
-- @        let Zglb'[j+z_iv] = z[j]                                @
-- @        in  Zglb'                                               @
-- @    in {Anxt,A_nxt_sz,z_al',y_iv',z_iv',acc',Xglb',Yglb',Zglb'} @
-- @in                                                              @
-- @let {X, Y, Z} = {Xglb, split(y_iv,Yglb), split(z_iv,Zglb)} ...  @
--
-- Hope you got the idea at least because the code is terrible :-)
transformSOAC respat (Stream cs _ form lam arrexps _) = do
  -- 1.) trivial step: find and build some of the basic things you need
  let accexps = getStreamAccums    form
      lampars = extLambdaParams     lam
      lamrtps = extLambdaReturnType lam
      lambody = extLambdaBody       lam
  -- a) ilam becomes the loop_index*chunkglb,
  -- b) chunkloc is the chunk used inside the loop body
  -- c) chunkglb is the global chunk (set to 1 or a convenient number)
  -- d) inarrVsz is the variant size of the streamed input array in the loop.
  ilam <- newIdent "stream_ii" $ Basic Int
  chunkloc <- case lampars of
                chnk:_ -> return chnk
                _ -> fail "FirstOrderTransform Stream: chunk error!"
  outersz  <- arraysSize 0 <$> mapM lookupType arrexps
  let chunkglb_val = case form of
                       Sequential{} -> intconst 1
                       _            -> outersz
  chunkglb <- letExp (baseString $ paramName chunkloc) $ PrimOp $ SubExp chunkglb_val
  let acc_num = length accexps
      arrrtps = drop acc_num lamrtps
      sub_chko= HM.fromList [(paramName chunkloc, outersz)]
  -- 2.) Make the existential induction variables, allocated-size variables,
  --       and all possible instantiations of the existential types, i.e.,
  --       inside and outside the loop body!
  assocs   <- mkExistAssocs outersz arrrtps respat
  initrtps <- forM (zip arrrtps assocs) $ \ (tp,(_,mub)) -> do
                let deflt0= case mub of
                              UnknownBd -> outersz
                              UpperBd s -> s
                              ExactBd s -> s
                    deflt = if deflt0 == Var (paramName chunkloc)
                            then outersz else deflt0
                    dims  = extShapeDims $ arrayShape tp
                    dims' = map (exToNormShapeDim deflt sub_chko) dims
                    restp :: Type
                    restp = Array (elemType tp) (Shape dims') NoUniqueness
                return restp
  (mexistszs,mexistinds,botharrtps) <-
    unzip3 <$> forM (zip assocs initrtps) (mkAllExistIdAndTypes outersz)
  let (exszvar,    exszarres,  exszses  ) = unzip3 $ catMaybes mexistszs
      (exindvars,  indvarres,  exindses ) = unzip3 $ catMaybes mexistinds
      (lftedrtps1, lftedrtps2, exactrtps) = unzip3   botharrtps
      patarrnms = map (textual . identName) (fst $ unzip assocs)
  -- various result array identifiers
  outarrinit <- forM (zip initrtps  patarrnms) $ \(t,nm) ->
                    newIdent (nm++"_init") t
  outarrloop <- forM (zip lftedrtps1 patarrnms) $ \(t,nm) ->
                    newIdent (nm++"_loop") t
  strmresarrl<- forM (zip lftedrtps2 patarrnms) $ \(t,nm) ->
                    newIdent (nm++"_resL") t
  strmresarr <- forM (zip exactrtps patarrnms) $ \(t,nm) ->
                    newIdent (nm++"_resE") t
  strmresacc <- mapM (newIdent "stream_accres" <=< subExpType) accexps
  -- various stream array identifiers and outer sizes
  acc0     <- mapM (newIdent "acc" <=< subExpType) accexps
  loopind  <- newVName "stream_i"
  initacc0 <- mapM copyIfArray accexps --WHY COPY???
  let accres  = exindvars ++ acc0
      accall  = exszvar ++ accres
      initacc = exszses ++ exindses  ++ initacc0
      merge = loopMerge
              (accall++outarrloop)
              (initacc++map (Var . identName) outarrinit)
  loopcnt  <- newIdent "stream_N" $ Basic Int
  -- 3.) Transform the stream's lambda to a loop body
  loopbody <- runBodyBinder $
              localScope (HM.singleton loopind IndexInfo) $
              localScope (scopeOfFParams $ map fst merge) $ do
      let argsacc = map (PrimOp . SubExp . Var . identName) acc0
          accpars = take acc_num $ drop 1 lampars
          arrpars = drop (1 + acc_num) lampars
      accxis <- bodyBind =<< do
          -- for accumulators:
          forM_ (zip accpars argsacc) $ \(param, arg) ->
              letBindNames' [paramName param] arg
          -- ilam := i*chunk_glb, the local chunk inside the loop
          ilamexp <- eBinOp Times
                            (pure $ PrimOp $ SubExp $ Var loopind)
                            (pure $ PrimOp $ SubExp $ Var chunkglb)
                            Int
          myLetBind ilamexp ilam
          ---------------- changed from here --------------
          -- ilampch := (i+1)*chunk_glb
          ip1chgid <- newIdent "stream_ip1chg" $ Basic Int
          ip1chgexp<- eBinOp Plus
                             (pure $ PrimOp $ SubExp $ Var $ identName ilam)
                             (pure $ PrimOp $ SubExp $ Var chunkglb)
                             Int
          myLetBind ip1chgexp ip1chgid
          -- diff0   := (i+1)*ghunk_glb - total_stream_size
          diff0id  <- newIdent "stream_diff0" $ Basic Int
          diff0exp <-eBinOp Minus
                            (pure $ PrimOp $ SubExp $ Var $ identName ip1chgid)
                            (pure $ PrimOp $ SubExp outersz)
                            Int
          myLetBind diff0exp diff0id
          -- diff    := 0 < diff0 ? diff0 : 0
          diffid   <- newIdent "stream_diff" $ Basic Int
          ifdiffexp<- eIf (eBinOp Less (pure $ PrimOp $ SubExp $ Constant $ IntVal 0)
                                       (pure $ PrimOp $ SubExp $ Var $ identName diff0id)
                                  Bool)
                          (pure $ resultBody [Var $ identName diff0id])
                          (pure $ resultBody [Constant $ IntVal 0])
          myLetBind ifdiffexp diffid
          -- chunk_loc := chunk_glb - diff
          chlexp   <- eBinOp Minus
                             (pure $ PrimOp $ SubExp $ Var chunkglb)
                             (pure $ PrimOp $ SubExp $ Var $ identName diffid)
                             Int
          myLetBind chlexp $ paramIdent chunkloc
          -- diff1   := chunk_glb*i - diff
          diff1id  <- newIdent "stream_diff1" $ Basic Int
          diff1exp <- eBinOp Minus
                             (pure $ PrimOp $ SubExp $ Var $ identName ilam)
                             (pure $ PrimOp $ SubExp $ Var $ identName diffid)
                             Int
          myLetBind diff1exp diff1id
          -- split input streams into current chunk and rest of stream
          forM_ (zip arrpars arrexps) $
            \(param, inarr) -> do
                atp <- lookupType inarr
                let anm = textual inarr
                (dt1,t2,dt4) <- case atp of
                    Array bt (Shape (_:dims)) _ ->
                        return ( Array bt (Shape $ Var (identName diff1id):dims) NoUniqueness
                               , Array bt (Shape $ Var chunkglb           :dims) NoUniqueness
                               , Array bt (Shape $ Var (identName diffid ):dims) NoUniqueness)
                    _ -> fail "FirstOrderTransform(Stream): array of not array type"
                id1 <- newIdent "dead" dt1
                id2 <- newIdent (anm++"_chg" ) t2
                id3 <- newIdent (anm++"_chgu") t2
                id4 <- newIdent "dead" dt4
                -- (_,a_cg) = split((chunk_glb*i-diff, chunk_glb), inarr)
                let split1= PrimOp $ Split [] [Var $ identName diff1id, Var chunkglb] inarr
                _ <- letBindNames' [identName id1, identName id2] split1
                -- a_cg* := copy(a_cg)
                letBindNames'_ [identName id3] =<<
                  eCopy (pure (PrimOp $ SubExp $ Var $ identName id2))
                -- (_,a_cl) = split((diff,cg-diff), a_cg*)
                let split2= PrimOp $ Split [] [Var $ identName diffid,
                                               Var $ paramName chunkloc] $
                                     identName id3
                letBindNames' [identName id4, paramName param] split2
          mkBodyM (bodyBindings lambody) (bodyResult lambody)
      -- make copy-out epilogue for result arrays
      let (acc', xis) = splitAt acc_num accxis
          indszids = zip mexistszs mexistinds
      epilogue <- forM (zip3 indszids outarrloop xis) $
                       mkOutArrEpilogue cs (Var $ identName ilam)
      let (mszvars,mindvars,dests) = unzip3 epilogue
          (indvars,szvars) = (catMaybes mindvars, catMaybes mszvars)
      return $
        resultBody (map (Var . identName) (szvars ++ indvars) ++
                    acc' ++
                    map (Var . identName) dests)
  -- 4.) Build the loop
  let loopres = LoopOp $
                DoLoop (map identName $ accres++outarrloop) merge
                       (ForLoop loopind (Var $ identName loopcnt)) loopbody
      loopbnd = mkLet' exszarres (indvarres++strmresacc++strmresarrl) loopres
  -- 5.) A stream needs prologue-loop-epilogue bindings, so we make a dummy
  --     IF exp to return one expression
  outarrrshpbnds <-
    forM (zip5 strmresarr strmresarrl exactrtps mexistszs mexistinds) $
    \(arr,arrl,_,malocsz,msz) ->
    case (malocsz,msz) of
      (Nothing, Nothing)    ->
        -- regular array case!
        return $ mkLet' [] [arr] $ PrimOp $ SubExp $ Var $ identName arrl
      (_, Just (_,indvar,_)) ->
        -- array with known upper bound case!
        return $ mkLet' [] [arr] $
        PrimOp $ Split [] [Var $ identName indvar] $ identName arrl
      _ -> fail "Stream UNREACHABLE in outarrrshpbnds computation!"
  let allbnds = loopbnd : outarrrshpbnds
  thenbody <- runBodyBinder $ do
      lUBexp <- eBinOp Div
                       (eBinOp Plus (pure $ PrimOp $ SubExp outersz)
                                    (eBinOp Minus (pure $ PrimOp $ SubExp $ Var chunkglb)
                                                  (pure $ PrimOp $ SubExp $ intconst 1) Int)
                                    Int)
                       (pure $ PrimOp $ SubExp $ Var chunkglb)
                       Int
      myLetBind lUBexp loopcnt
      let outinibds= zipWith (\ idd tp ->
                                mkLet' [] [idd] $ PrimOp $
                                Scratch (elemType tp) (arrayDims tp)
                             ) outarrinit initrtps
      mapM_ addBinding (outinibds++allbnds)
      return $ resultBody (map (Var . identName) strmresacc ++
                           map (Var . identName) strmresarr)
  elsebody <- runBodyBinder $ do
      fakeoutarrs <- resultArray  initrtps
      return $ resultBody (accexps ++ map Var fakeoutarrs)
  letBind_ respat =<<
    eIf (pure $ PrimOp $ SubExp $ Constant $ LogVal True)
    (pure thenbody)
    (pure elsebody)
  where myLetBind :: Transformer m =>
                     AST.Exp (Lore m) -> Ident -> m ()
        myLetBind e idd = addBinding $ mkLet' [] [idd] e

        exToNormShapeDim :: SubExp -> HM.HashMap VName SubExp -> ExtDimSize -> SubExp
        exToNormShapeDim d _ (Ext   _) = d
        exToNormShapeDim _ _ (Free c@(Constant _)) = c
        exToNormShapeDim _ subs (Free (Var idd)) =
          HM.lookupDefault (Var idd) idd subs
        existUpperBound :: SubExp -> Bool -> MEQType
        existUpperBound outerSize b =
            if not b then UnknownBd
            else UpperBd outerSize

        -- | Assumes rtps are the array result types and pat is the
        -- pattern result of stream in let bound.  Result is a list of
        -- tuples: (1st) the ident of the array in pattern, (2rd) the
        -- exact/upper bound/unknown shape of the outer dim.
        mkExistAssocs :: MonadBinder m =>
                         SubExp -> [ExtType] -> AST.Pattern (Lore m)
                      -> m [(Ident, MEQType)]
        mkExistAssocs outerSize rtps pat = do
          let patels    = patternElements pat
              -- keep only the patterns corresponding to the array types
              arrpatels = drop (length patels - length rtps) patels
              processAssoc (rtp,patel) = do
                  let patid = patElemIdent patel
                      rtpdim= extShapeDims $ arrayShape rtp
                  case rtpdim of
                    Ext  _:_ -> return (patid, existUpperBound outerSize withUpperBound )
                    Free s:_ -> return (patid, ExactBd s            )
                    _        ->
                        fail "FirstOrderTrabsform(Stream), mkExistAssocs: Empty Array Shape!"
          forM (zip rtps arrpatels) processAssoc

        mkAllExistIdAndTypes :: Transformer m =>
                                SubExp
                             -> ( (Ident, MEQType), Type )
                             -> m (Maybe (Ident, Ident, SubExp),
                                   Maybe (Ident, Ident, SubExp),
                                   (Type, Type, Type))
        mkAllExistIdAndTypes _ ((_, ExactBd _), initrtp) =
            return ( Nothing, Nothing, (initrtp,initrtp,initrtp) )
        mkAllExistIdAndTypes _ ((p,UpperBd _), Array bt (Shape (d:dims)) u) = do
            idd1<- newIdent (textual (identName p)++"_outiv1") $ Basic Int
            idd2<- newIdent (textual (identName p)++"_outiv2") $ Basic Int
            let initrtp   = Array bt (Shape $ d:dims) u
                exacttype = Array bt (Shape $ Var (identName idd2):dims) u
            return ( Nothing
                   , Just (idd1,idd2,intconst 0)
                   , (initrtp,initrtp,exacttype) )
        mkAllExistIdAndTypes strmsz ((p,UnknownBd), Array bt (Shape (_:dims)) u) = do
            idd1 <- newIdent (textual (identName p)++"_outiv1") $ Basic Int
            idd2 <- newIdent (textual (identName p)++"_outiv2") $ Basic Int
            idal1<- newIdent (textual (identName p)++"_outsz1") $ Basic Int
            idal2<- newIdent (textual (identName p)++"_outsz2") $ Basic Int
            let lftedtype1= Array bt (Shape $ Var (identName idal1): dims) u
                lftedtype2= Array bt (Shape $ Var (identName idal2): dims) u
                exacttype = Array bt (Shape $ Var (identName idd2) : dims) u
            return ( Just (idal1,idal2,    strmsz)
                   , Just (idd1, idd2, intconst 0)
                   , (lftedtype1,lftedtype2,exacttype) )
        mkAllExistIdAndTypes _ _ =
            fail "FirstOrderTransform(Stream): failed in mkAllExistIdAndTypes"

        mkOutArrEpilogue :: Transformer m =>
                            Certificates -> SubExp
                         -> (( Maybe (Ident,Ident,SubExp)
                             , Maybe (Ident,Ident,SubExp))
                            , Ident
                            , SubExp)
                         -> m (Maybe Ident, Maybe Ident, Ident)
        mkOutArrEpilogue css iv ((allocvars,indvars),glboutid,locoutarr) = do
            locoutid <- case locoutarr of
                          Var idd -> return idd
                          _ -> fail ("FirstOrderTransform(Stream), mkOutArrEpilogue:"++
                                     " array result MUST be a Var!")
            locoutid_size <- arraySize 0 <$> lookupType locoutid
            (ivv, glboutid', mind', malloc') <-
                case indvars of
                  Nothing ->               -- exact-size case
                    return (iv, glboutid, Nothing, Nothing)
                  Just (k,_,_) -> do
                    newszid <- newIdent (textual (identName k)++"_new") $ Basic Int
                    plexp <- eBinOp Plus (pure $ PrimOp $ SubExp $ Var $ identName k)
                                         (pure $ PrimOp $ SubExp locoutid_size) Int
                    myLetBind plexp newszid
                    let oldbtp = identType glboutid
                    newallocid <- newIdent "newallocsz" $ Basic Int
                    resallocid <- newIdent "resallocsz" $ Basic Int
                    olddims <- case arrayDims oldbtp of
                                 (_:dims) -> return dims
                                 _ -> fail ("FirstOrderTransform(Stream), mkOutArrEpilogue:"++
                                            " empty array dimensions!")
                    case allocvars of
                      Nothing     ->       -- known-upper-bound case
                        return (Var $ identName k, glboutid, Just newszid, Nothing)
                      Just (alsz,_,_)-> do -- fully existential case, reallocate
                        alloclid <- newVName "allcloopiv"
                        let isempty = eBinOp Leq (pure $ PrimOp $ SubExp $ Var $ identName newszid)
                                             (pure $ PrimOp $ SubExp $ Var $ identName alsz) Bool
                            emptybranch = pure $ resultBody [Var $ identName glboutid]
                            otherbranch = runBodyBinder $ do
                                alszt2exp<- eBinOp Times (pure $ PrimOp $ SubExp $ Var $ identName newszid)
                                                         (pure $ PrimOp $ SubExp $ intconst 2 ) Int
                                myLetBind alszt2exp newallocid
                                bnew0<- letExp (textual (identName glboutid)++"_loop0") $
                                               PrimOp $ Scratch (elemType oldbtp) (Var (identName newallocid):olddims)
                                bnew <- newIdent (textual (identName glboutid)++"_loop") =<<
                                        lookupType bnew0
                                let alloc_merge = loopMerge [bnew] [Var bnew0]
                                allocloopbody <-
                                  runBodyBinder $
                                  localScope (HM.singleton alloclid IndexInfo) $
                                  localScope (scopeOfFParams $ map fst alloc_merge) $ do
                                    (aldest:_) <- letwith css [identName bnew] (pexp $ Var alloclid)
                                                  [PrimOp $ Index css (identName glboutid) [Var alloclid]]
                                    return $ resultBody [Var aldest]
                                let alloopres = LoopOp $ DoLoop [identName bnew]
                                                  (loopMerge [bnew] [Var bnew0])
                                                  (ForLoop alloclid (Var $ identName k)) allocloopbody
                                bnew' <- newIdent (textual (identName glboutid)++"_new0") =<<
                                         lookupType bnew0
                                myLetBind alloopres bnew'
                                return $ resultBody [Var $ identName bnew']
                        allocifexp <- eIf isempty emptybranch otherbranch
                        bnew'' <- newIdent (textual (identName glboutid)++"_res") $
                                           Array (elemType oldbtp)
                                           (Shape $ Var (identName resallocid):olddims) NoUniqueness
                        let patresbnd = mkLet' [resallocid] [bnew''] allocifexp
                        addBinding patresbnd
                        return (Var $ identName k, bnew'', Just newszid, Just resallocid)
            glboutLid <- newIdent (textual (identName glboutid)++"_loop") $ identType glboutid'
            glboutBdId<- newIdent (textual (identName glboutid)++"_loopbd") $ identType glboutid'
            loopid <- newVName "j"
            let outmerge = loopMerge [glboutLid] [Var $ identName glboutid']
            -- make copy-out what was written in the current iteration
            loopbody <- runBodyBinder $
                        localScope (HM.singleton loopid IndexInfo) $
                        localScope (scopeOfFParams $ map fst outmerge) $ do
                ivvplid <- newIdent "jj" $ Basic Int
                ivvplidexp <- eBinOp Plus (pure $ PrimOp $ SubExp ivv)
                                          (pure $ PrimOp $ SubExp $ Var loopid) Int
                myLetBind ivvplidexp ivvplid
                (dest:_) <- letwith css [identName glboutLid] (pexp (Var $ identName ivvplid))--[indexp]
                                        [PrimOp $ Index css locoutid [Var loopid]]
                return $ resultBody [Var dest]
            -- make loop
            let loopres = LoopOp $ DoLoop [identName glboutLid] outmerge
                          (ForLoop loopid locoutid_size) loopbody
            myLetBind loopres glboutBdId
            return (malloc', mind', glboutBdId)

-- | Recursively first-order-transform a lambda.
transformLambda :: (MonadFreshNames m,
                    Bindable lore,
                    LocalScope lore m,
                    LetAttr SOACS ~ LetAttr lore,
                    LParamAttr SOACS ~ LParamAttr lore) =>
                   Lambda -> m (AST.Lambda lore)
transformLambda (Lambda i params body rettype) = do
  body' <- runBodyBinder $
           localScope (HM.singleton i IndexInfo) $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ Lambda i params body' rettype

-- | Recursively first-order-transform a lambda.
transformExtLambda :: (MonadFreshNames m,
                       Bindable lore,
                       LocalScope lore m,
                       LetAttr SOACS ~ LetAttr lore,
                       LParamAttr SOACS ~ LParamAttr lore) =>
                      ExtLambda -> m (AST.ExtLambda lore)
transformExtLambda (ExtLambda i params body rettype) = do
  body' <- runBodyBinder $
           localScope (HM.singleton i IndexInfo) $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ ExtLambda i params body' rettype

newFold :: Transformer m =>
           [(SubExp,Type)]
        -> m ([Ident], [SubExp])
newFold accexps_and_types = do
  initacc <- mapM copyIfArray acc_exps
  acc <- mapM (newIdent "acc") acc_types
  return (acc, initacc)
  where (acc_exps, acc_types) = unzip accexps_and_types

copyIfArray :: Transformer m =>
               SubExp -> m SubExp
copyIfArray (Constant v) = return $ Constant v
copyIfArray (Var v) = Var <$> copyIfArrayName v

copyIfArrayName :: Transformer m =>
                   VName -> m VName
copyIfArrayName v = do
  t <- lookupType v
  case t of
   Array {} -> letExp (baseString v ++ "_first_order_copy") $ PrimOp $ Copy v
   _        -> return v

index :: Certificates -> [VName] -> SubExp
      -> [AST.Exp lore]
index cs arrs i = flip map arrs $ \arr ->
  PrimOp $ Index cs arr [i]

resultArray :: Transformer m => [Type] -> m [VName]
resultArray = mapM oneArray
  where oneArray t = letExp "result" $ PrimOp $ Scratch (elemType t) (arrayDims t)

letwith :: Transformer m =>
           Certificates -> [VName] -> m (AST.Exp (Lore m)) -> [AST.Exp (Lore m)]
        -> m [VName]
letwith cs ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  let update k v =
        letInPlace "lw_dest" cs k [i'] $ PrimOp $ SubExp v
  zipWithM update ks vs'

pexp :: Applicative f => SubExp -> f (AST.Exp lore)
pexp = pure . PrimOp . SubExp

bindLambda :: Transformer m =>
              AST.Lambda (Lore m) -> [AST.Exp (Lore m)]
           -> m [SubExp]
bindLambda (Lambda _ params body _) args = do
  forM_ (zip params args) $ \(param, arg) ->
    if basicType $ paramType param
    then letBindNames' [paramName param] arg
    else letBindNames' [paramName param] =<< eCopy (pure arg)
  bodyBind body

loopMerge :: [Ident] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge vars vals = [ (Param pname $ toDecl ptype Unique, val)
                      | (Ident pname ptype,val) <- zip vars vals ]


withUpperBound :: Bool
withUpperBound = False
data MEQType = ExactBd SubExp
             | UpperBd SubExp
             | UnknownBd

-- | Turn a Haskell-style mapAccumL into a sequential do-loop.  This
-- is the guts of transforming a 'Redomap'.
doLoopMapAccumL :: (LocalScope (Lore m) m, MonadBinder m, Bindable (Lore m),
                    LetAttr (Lore m) ~ Type) =>
                   Certificates
                -> SubExp
                -> AST.Lambda (Lore m)
                -> [SubExp]
                -> [VName]
                -> [VName]
                -> m (AST.Exp (Lore m))
doLoopMapAccumL cs width innerfun accexps arrexps maparrs = do
  arrts <- mapM lookupType arrexps
  -- for the MAP    part
  let i = lambdaIndex innerfun
  let acc_num     = length accexps
  let res_tps     = lambdaReturnType innerfun
  let map_arr_tps = drop acc_num res_tps
  let res_ts = [ arrayOf t (Shape [width]) NoUniqueness
               | t <- map_arr_tps ]
  let accts = map paramType $ fst $ splitAt acc_num $ lambdaParams innerfun
  outarrs <- mapM (newIdent "redomap_outarr") res_ts
  -- for the REDUCE part
  (acc, initacc) <- newFold $ zip accexps accts
  inarrs <- mapM (newIdent "redomap_inarr") arrts
  let merge = loopMerge (inarrs++acc++outarrs) (map Var arrexps++initacc++map Var maparrs)
  loopbody <- runBodyBinder $ localScope (scopeOfFParams $ map fst merge) $ do
    accxis<- bindLambda innerfun
             (map (PrimOp . SubExp . Var . identName) acc ++
              index cs (map identName inarrs) (Var i))
    let (acc', xis) = splitAt acc_num accxis
    dests <- letwith cs (map identName outarrs) (pexp (Var i)) $
             map (PrimOp . SubExp) xis
    return $ resultBody (map (Var . identName) inarrs ++ acc' ++ map Var dests)
  return $ LoopOp $
    DoLoop (map identName $ acc++outarrs) merge (ForLoop i width) loopbody
