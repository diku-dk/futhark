{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive,
-- and - it's certainly worth considering when we can express such
-- transformations in-place.
module Futhark.Transform.FirstOrderTransform
  ( transformFunDef

  , Transformer
  , transformStmRecursively
  , transformLambda
  , transformSOAC
  , transformBody

  -- * Utility
  , doLoopMapAccumL
  , doLoopMapAccumL'
  )
  where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List

import qualified Futhark.Representation.AST as AST
import Futhark.Representation.SOACS
import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.Aliases (Aliases, removeLambdaAliases)
import Futhark.Representation.AST.Attributes.Aliases
import Futhark.Util (chunks, splitAt3)

transformFunDef :: (MonadFreshNames m, Bindable tolore, BinderOps tolore,
                    LetAttr SOACS ~ LetAttr tolore,
                    CanBeAliased (Op tolore)) =>
                   FunDef -> m (AST.FunDef tolore)
transformFunDef (FunDef entry fname rettype params body) = do
  (body',_) <- modifyNameSource $ runState $ runBinderT m mempty
  return $ FunDef entry fname rettype params body'
  where m = localScope (scopeOfFParams params) $ insertStmsM $ transformBody body

-- | The constraints that a monad must uphold in order to be used for
-- first-order transformation.
type Transformer m = (MonadBinder m,
                      Bindable (Lore m), BinderOps (Lore m),
                      LocalScope (Lore m) m,
                      LetAttr SOACS ~ LetAttr (Lore m),
                      LParamAttr SOACS ~ LParamAttr (Lore m),
                      CanBeAliased (Op (Lore m)))

transformBody :: Transformer m =>
                 Body -> m (AST.Body (Lore m))
transformBody (Body () bnds res) = insertStmsM $ do
  mapM_ transformStmRecursively bnds
  return $ resultBody res

-- | First transform any nested 'Body' or 'Lambda' elements, then
-- apply 'transformSOAC' if the expression is a SOAC.
transformStmRecursively :: Transformer m =>
                               Stm -> m ()

transformStmRecursively (Let pat aux (Op soac)) =
  certifying (stmAuxCerts aux) $
  transformSOAC pat =<< mapSOACM soacTransform soac
  where soacTransform = identitySOACMapper { mapOnSOACLambda = transformLambda }

transformStmRecursively (Let pat aux e) =
  certifying (stmAuxCerts aux) $
  letBind_ pat =<< mapExpM transform e
  where transform = identityMapper { mapOnBody = \scope -> localScope scope . transformBody
                                   , mapOnRetType = return
                                   , mapOnBranchType = return
                                   , mapOnFParam = return
                                   , mapOnLParam = return
                                   , mapOnOp = fail "Unhandled Op in first order transform"
                                   }

-- | Transform a single 'SOAC' into a do-loop.  The body of the lambda
-- is untouched, and may or may not contain further 'SOAC's depending
-- on the given lore.
transformSOAC :: Transformer m =>
                 AST.Pattern (Lore m)
              -> SOAC (Lore m)
              -> m ()

transformSOAC pat CmpThreshold{} =
  letBind_ pat $ BasicOp $ SubExp $ constant False -- close enough

transformSOAC pat (Screma w form@(ScremaForm (scan_lam, scan_nes)
                                                 (_, red_lam, red_nes)
                                                 map_lam) arrs) = do
  let (scan_arr_ts, _red_ts, map_arr_ts) =
        splitAt3 (length scan_nes) (length red_nes) $ scremaType w form
  scan_arrs <- resultArray scan_arr_ts
  map_arrs <- resultArray map_arr_ts

  -- We construct a loop that contains several groups of merge
  -- parameters:
  --
  -- (0) scan accumulator.
  -- (1) scan results.
  -- (2) reduce results (and accumulator).
  -- (3) map results.
  --
  -- Inside the loop, the parameters to map_lam become for-in
  -- parameters.

  scanacc_params <- mapM (newParam "scanacc" . flip toDecl Nonunique) $ lambdaReturnType scan_lam
  scanout_params <- mapM (newParam "scanout" . flip toDecl Unique) scan_arr_ts
  redout_params <- mapM (newParam "redout" . flip toDecl Nonunique) $ lambdaReturnType red_lam
  mapout_params <- mapM (newParam "mapout" . flip toDecl Unique) map_arr_ts

  let merge = concat [zip scanacc_params scan_nes,
                      zip scanout_params $ map Var scan_arrs,
                      zip redout_params red_nes,
                      zip mapout_params $ map Var map_arrs]
  i <- newVName "i"
  let loopform = ForLoop i Int32 w []

  loop_body <- runBodyBinder $
               localScope (scopeOfFParams $ map fst merge) $
               inScopeOf loopform $ do

    forM_ (zip (lambdaParams map_lam) arrs) $ \(p, arr) -> do
      arr_t <- lookupType arr
      letBindNames_ [paramName p] $ BasicOp $ Index arr $
        fullSlice arr_t [DimFix $ Var i]

    -- Insert the statements of the lambda.  We have taken care to
    -- ensure that the parameters are bound at this point.
    mapM_ addStm $ bodyStms $ lambdaBody map_lam
    -- Split into scan results, reduce results, and map results.
    let (scan_res, red_res, map_res) =
          splitAt3 (length scan_nes) (length red_nes) $
          bodyResult $ lambdaBody map_lam

    scan_res' <- eLambda scan_lam $ map (pure . BasicOp . SubExp) $
                 map (Var . paramName) scanacc_params ++ scan_res
    red_res' <- eLambda red_lam $ map (pure . BasicOp . SubExp) $
                map (Var . paramName) redout_params ++ red_res

    -- Write the scan accumulator to the scan result arrays.
    scan_outarrs <- letwith (map paramName scanout_params) (pexp (Var i)) $
                    map (BasicOp . SubExp) scan_res'

    -- Write the map results to the map result arrays.
    map_outarrs <- letwith (map paramName mapout_params) (pexp (Var i)) $
                   map (BasicOp . SubExp) map_res

    return $ resultBody $ concat [scan_res',
                                  map Var scan_outarrs,
                                  red_res',
                                  map Var map_outarrs]

  -- We need to discard the final scan accumulators, as they are not
  -- bound in the original pattern.
  pat' <- discardPattern (map paramType scanacc_params) pat
  letBind_ pat' $ DoLoop [] merge loopform loop_body

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
transformSOAC respat (Stream outersz form lam arrexps) = do
  -- 1.) trivial step: find and build some of the basic things you need
  let accexps = getStreamAccums  form
      lampars = lambdaParams     lam
      lamrtps = lambdaReturnType lam
      lambody = lambdaBody       lam
  -- a) ilam becomes the loop_index*chunkglb,
  -- b) chunkloc is the chunk used inside the loop body
  -- c) chunkglb is the global chunk (set to 1 or a convenient number)
  -- d) inarrVsz is the variant size of the streamed input array in the loop.
  ilam <- newIdent "stream_ii" $ Prim int32
  chunkloc <- case lampars of
                chnk:_ -> return chnk
                _ -> fail "FirstOrderTransform Stream: chunk error!"
  let chunkglb_val = case form of
                       Sequential{} -> constant (1 :: Int32)
                       _            -> outersz
  chunkglb <- letExp (baseString $ paramName chunkloc) $ BasicOp $ SubExp chunkglb_val
  let acc_num = length accexps
      arrrtps = drop acc_num lamrtps
      sub_chko= M.fromList [(paramName chunkloc, outersz)]
  -- 2.) Make the existential induction variables, allocated-size variables,
  --       and all possible instantiations of the existential types, i.e.,
  --       inside and outside the loop body!
  assocs   <- mkExistAssocs arrrtps respat
  initrtps <- forM (zip arrrtps assocs) $ \ (tp,(_,mub)) -> do
                let deflt0= case mub of
                              UnknownBd -> outersz
                              UpperBd s -> s
                              ExactBd s -> s
                    deflt = if deflt0 == Var (paramName chunkloc)
                            then outersz else deflt0
                    dims  = shapeDims $ arrayShape tp
                    dims' = map (exToNormShapeDim deflt sub_chko) dims
                    restp :: Type
                    restp = Array (elemType tp) (Shape dims') NoUniqueness
                return restp
  (mexistszs,mexistinds,botharrtps) <-
    unzip3 <$> forM (zip assocs initrtps) (mkAllExistIdAndTypes outersz)
  let (exszvar,    exszarres,  exszses  ) = unzip3 $ catMaybes mexistszs
      (exindvars,  indvarres,  exindses ) = unzip3 $ catMaybes mexistinds
      (lftedrtps1, lftedrtps2, exactrtps) = unzip3   botharrtps
      patarrnms = map (baseString . identName . fst) assocs
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
  initacc0 <- mapM copyIfArray accexps --WHY COPY???
  loopind <- newVName "loopind"
  let accres  = exindvars ++ acc0
      ctxmerge = loopMerge exszvar exszses
      valmerge = loopMerge
                 (accres++outarrloop)
                 (exindses++initacc0++map (Var . identName) outarrinit)
      -- returns (map identName $ accres++outarrloop)
  loopcnt  <- newIdent "stream_N" $ Prim int32
  -- 3.) Transform the stream's lambda to a loop body
  loopbody <- runBodyBinder $
              localScope (M.singleton loopind $ IndexInfo Int32) $
              localScope (scopeOfFParams $ map fst $ ctxmerge++valmerge) $ do
      let argsacc = map (BasicOp . SubExp . Var . identName) acc0
          accpars = take acc_num $ drop 1 lampars
          arrpars = drop (1 + acc_num) lampars
      accxis <- bodyBind =<< do
          -- for accumulators:
          forM_ (zip accpars argsacc) $ \(param, arg) ->
              letBindNames [paramName param] arg
          -- ilam := i*chunk_glb, the local chunk inside the loop
          ilamexp <- eBinOp (Mul Int32)
                            (pure $ BasicOp $ SubExp $ Var loopind)
                            (pure $ BasicOp $ SubExp $ Var chunkglb)
          myLetBind ilamexp ilam
          ---------------- changed from here --------------
          -- ilampch := (i+1)*chunk_glb
          ip1chgid <- newIdent "stream_ip1chg" $ Prim int32
          ip1chgexp<- eBinOp (Add Int32)
                             (pure $ BasicOp $ SubExp $ Var $ identName ilam)
                             (pure $ BasicOp $ SubExp $ Var chunkglb)
          myLetBind ip1chgexp ip1chgid
          -- diff0   := (i+1)*ghunk_glb - total_stream_size
          diff0id  <- newIdent "stream_diff0" $ Prim int32
          diff0exp <-eBinOp (Sub Int32)
                            (pure $ BasicOp $ SubExp $ Var $ identName ip1chgid)
                            (pure $ BasicOp $ SubExp outersz)
          myLetBind diff0exp diff0id
          -- diff    := 0 < diff0 ? diff0 : 0
          diffid   <- newIdent "stream_diff" $ Prim int32
          ifdiffexp<- eIf (eCmpOp (CmpSlt Int32)
                           (pure $ BasicOp $ SubExp $ constant (0 :: Int32))
                           (pure $ BasicOp $ SubExp $ Var $ identName diff0id))
                          (pure $ resultBody [Var $ identName diff0id])
                          (pure $ resultBody [constant (0 :: Int32)])
          myLetBind ifdiffexp diffid
          -- chunk_loc := chunk_glb - diff
          chlexp   <- eBinOp (Sub Int32)
                             (pure $ BasicOp $ SubExp $ Var chunkglb)
                             (pure $ BasicOp $ SubExp $ Var $ identName diffid)
          myLetBind chlexp $ paramIdent chunkloc
          -- diff1   := chunk_glb*i - diff
          diff1id  <- newIdent "stream_diff1" $ Prim int32
          diff1exp <- eBinOp (Sub Int32)
                             (pure $ BasicOp $ SubExp $ Var $ identName ilam)
                             (pure $ BasicOp $ SubExp $ Var $ identName diffid)
          myLetBind diff1exp diff1id
          -- split input streams into current chunk and rest of stream
          forM_ (zip arrpars arrexps) $
            \(param, inarr) -> do
                let anm = baseString inarr
                -- (_,a_cg) = split((chunk_glb*i-diff, chunk_glb), inarr)
                id2 <- letExp (anm++"_chg") =<<
                       eSliceArray 0 inarr (eSubExp $ Var $ identName diff1id) (eSubExp $ Var chunkglb)
                -- a_cg* := copy(a_cg)
                id3 <- letExp (anm++"_chgu") =<<
                       eCopy (pure (BasicOp $ SubExp $ Var id2))
                -- (_,a_cl) = split((diff,cg-diff), a_cg*)
                letBindNames_ [paramName param] =<<
                  eSliceArray 0 id3 (eSubExp $ Var $ identName diffid) (eSubExp $ Var $ paramName chunkloc)

          mkBodyM (bodyStms lambody) (bodyResult lambody)
      -- make copy-out epilogue for result arrays
      let (acc', xis) = splitAt acc_num accxis
          indszids = zip mexistszs mexistinds
      epilogue <- forM (zip3 indszids outarrloop xis) $
                  mkOutArrEpilogue (Var $ identName ilam)
      let (mszvars,mindvars,dests) = unzip3 epilogue
          (indvars,szvars) = (catMaybes mindvars, catMaybes mszvars)
      return $
        resultBody (map (Var . identName) (szvars ++ indvars) ++
                    acc' ++
                    map (Var . identName) dests)
  -- 4.) Build the loop
  let loopres = DoLoop ctxmerge valmerge
                       (ForLoop loopind Int32 (Var $ identName loopcnt) []) loopbody
      loopbnd = mkLet exszarres (indvarres++strmresacc++strmresarrl) loopres
  -- 5.) A stream needs prologue-loop-epilogue bindings, so we make a dummy
  --     IF exp to return one expression
  outarrrshpbnds <-
    forM (zip5 strmresarr strmresarrl exactrtps mexistszs mexistinds) $
    \(arr,arrl,_,malocsz,msz) ->
    case (malocsz,msz) of
      (Nothing, Nothing)    ->
        -- regular array case!
        return $ mkLet [] [arr] $ BasicOp $ SubExp $ Var $ identName arrl
      (_, Just (_,indvar,_)) ->
        -- array with known upper bound case!
        mkLetNamesM [identName arr] =<<
        eSliceArray 0 (identName arrl) (eSubExp $ intConst Int32 0) (eSubExp $ Var $ identName indvar)
      _ -> fail "Stream UNREACHABLE in outarrrshpbnds computation!"
  let allbnds = loopbnd : outarrrshpbnds
  lUBexp <- eBinOp (SDiv Int32)
                   (eBinOp (Add Int32)
                    (pure $ BasicOp $ SubExp outersz)
                    (eBinOp (Sub Int32) (pure $ BasicOp $ SubExp $ Var chunkglb)
                                        (pure $ BasicOp $ SubExp $ constant (1 :: Int32))))
                   (pure $ BasicOp $ SubExp $ Var chunkglb)
  myLetBind lUBexp loopcnt
  let outinibds= zipWith (\ idd tp ->
                            mkLet [] [idd] $ BasicOp $
                            Scratch (elemType tp) (arrayDims tp)
                         ) outarrinit initrtps
  addStms $ stmsFromList $ outinibds++allbnds
  forM_ (zip (patternValueNames respat) $ strmresacc ++ strmresarr) $ \(p, v) ->
    letBindNames_ [p] $ BasicOp $ SubExp $ Var $ identName v
  let mapping = shapeMapping (patternValueTypes respat) $
                map identType $ strmresacc ++ strmresarr
  forM_ (M.toList mapping) $ \(p, se) ->
    when (p `elem` patternContextNames respat) $
    letBindNames_ [p] $ BasicOp $ SubExp se
  where myLetBind :: Transformer m =>
                     AST.Exp (Lore m) -> Ident -> m ()
        myLetBind e idd = addStm $ mkLet [] [idd] e

        exToNormShapeDim :: SubExp -> M.Map VName SubExp -> SubExp -> SubExp
        exToNormShapeDim _ _ c@(Constant _) = c
        exToNormShapeDim _ subs (Var idd) =
          M.findWithDefault (Var idd) idd subs

        -- | Assumes rtps are the array result types and pat is the
        -- pattern result of stream in let bound.  Result is a list of
        -- tuples: (1st) the ident of the array in pattern, (2rd) the
        -- exact/upper bound/unknown shape of the outer dim.
        mkExistAssocs :: MonadBinder m =>
                         [Type] -> AST.Pattern (Lore m)
                      -> m [(Ident, MEQType)]
        mkExistAssocs rtps pat = do
          let patels    = patternElements pat
              -- keep only the patterns corresponding to the array types
              arrpatels = drop (length patels - length rtps) patels
              processAssoc (rtp,patel) = do
                  let patid = patElemIdent patel
                      rtpdim= shapeDims $ arrayShape rtp
                  case rtpdim of
                    s:_ -> return (patid, ExactBd s            )
                    _   ->
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
            idd1<- newIdent (baseString (identName p)++"_outiv1") $ Prim int32
            idd2<- newIdent (baseString (identName p)++"_outiv2") $ Prim int32
            let initrtp   = Array bt (Shape $ d:dims) u
                exacttype = Array bt (Shape $ Var (identName idd2):dims) u
            return ( Nothing
                   , Just (idd1,idd2,constant (0 :: Int32))
                   , (initrtp,initrtp,exacttype) )
        mkAllExistIdAndTypes strmsz ((p,UnknownBd), Array bt (Shape (_:dims)) u) = do
            idd1 <- newIdent (baseString (identName p)++"_outiv1") $ Prim int32
            idd2 <- newIdent (baseString (identName p)++"_outiv2") $ Prim int32
            idal1<- newIdent (baseString (identName p)++"_outsz1") $ Prim int32
            idal2<- newIdent (baseString (identName p)++"_outsz2") $ Prim int32
            let lftedtype1= Array bt (Shape $ Var (identName idal1): dims) u
                lftedtype2= Array bt (Shape $ Var (identName idal2): dims) u
                exacttype = Array bt (Shape $ Var (identName idd2) : dims) u
            return ( Just (idal1,idal2,    strmsz)
                   , Just (idd1, idd2, constant (0 :: Int32))
                   , (lftedtype1,lftedtype2,exacttype) )
        mkAllExistIdAndTypes _ _ =
            fail "FirstOrderTransform(Stream): failed in mkAllExistIdAndTypes"

        mkOutArrEpilogue :: Transformer m =>
                            SubExp
                         -> (( Maybe (Ident,Ident,SubExp)
                             , Maybe (Ident,Ident,SubExp))
                            , Ident
                            , SubExp)
                         -> m (Maybe Ident, Maybe Ident, Ident)
        mkOutArrEpilogue iv ((allocvars,indvars),glboutid,locoutarr) = do
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
                    newszid <- newIdent (baseString (identName k)++"_new") $ Prim int32
                    plexp <- eBinOp (Add Int32) (pure $ BasicOp $ SubExp $ Var $ identName k)
                                                (pure $ BasicOp $ SubExp locoutid_size)
                    myLetBind plexp newszid
                    let oldbtp = identType glboutid
                    newallocid <- newIdent "newallocsz" $ Prim int32
                    resallocid <- newIdent "resallocsz" $ Prim int32
                    olddims <- case arrayDims oldbtp of
                                 (_:dims) -> return dims
                                 _ -> fail ("FirstOrderTransform(Stream), mkOutArrEpilogue:"++
                                            " empty array dimensions!")
                    case allocvars of
                      Nothing     ->       -- known-upper-bound case
                        return (Var $ identName k, glboutid, Just newszid, Nothing)
                      Just (alsz,_,_)-> do -- fully existential case, reallocate
                        alloclid <- newVName "allcloopiv"
                        let isempty = eCmpOp (CmpSlt Int32)
                                      (pure $ BasicOp $ SubExp $ Var $ identName newszid)
                                      (pure $ BasicOp $ SubExp $ Var $ identName alsz)
                            emptybranch = pure $ resultBody [Var $ identName glboutid]
                            otherbranch = runBodyBinder $ do
                                alszt2exp<- eBinOp (Mul Int32)
                                            (pure $ BasicOp $ SubExp $ Var $ identName newszid)
                                            (pure $ BasicOp $ SubExp $ constant (2 :: Int32))
                                myLetBind alszt2exp newallocid
                                bnew0<- letExp (baseString (identName glboutid)++"_loop0") $
                                               BasicOp $ Scratch (elemType oldbtp) (Var (identName newallocid):olddims)
                                bnew <- newIdent (baseString (identName glboutid)++"_loop") =<<
                                        lookupType bnew0
                                let alloc_merge = loopMerge [bnew] [Var bnew0]
                                allocloopbody <-
                                  runBodyBinder $
                                  localScope (M.singleton alloclid $ IndexInfo Int32) $
                                  localScope (scopeOfFParams $ map fst alloc_merge) $ do
                                    (aldest:_) <- letwith [identName bnew] (pexp $ Var alloclid)
                                                  [BasicOp $ Index (identName glboutid)
                                                   [DimFix $ Var alloclid]]
                                    return $ resultBody [Var aldest]
                                let alloopres = DoLoop []
                                                  (loopMerge [bnew] [Var bnew0])
                                                  (ForLoop alloclid Int32 (Var $ identName k) []) allocloopbody
                                bnew' <- newIdent (baseString (identName glboutid)++"_new0") =<<
                                         lookupType bnew0
                                myLetBind alloopres bnew'
                                return $ resultBody [Var $ identName bnew']
                        allocifexp <- eIf isempty emptybranch otherbranch
                        bnew'' <- newIdent (baseString (identName glboutid)++"_res") $
                                           Array (elemType oldbtp)
                                           (Shape $ Var (identName resallocid):olddims) NoUniqueness
                        let patresbnd = mkLet [resallocid] [bnew''] allocifexp
                        addStm patresbnd
                        return (Var $ identName k, bnew'', Just newszid, Just resallocid)
            glboutLid <- newIdent (baseString (identName glboutid)++"_loop") $ identType glboutid'
            glboutBdId<- newIdent (baseString (identName glboutid)++"_loopbd") $ identType glboutid'
            loopid <- newVName "j"
            let outmerge = loopMerge [glboutLid] [Var $ identName glboutid']
            -- make copy-out what was written in the current iteration
            loopbody <- runBodyBinder $
                        localScope (M.singleton loopid $ IndexInfo Int32) $
                        localScope (scopeOfFParams $ map fst outmerge) $ do
                ivvplid <- newIdent "jj" $ Prim int32
                ivvplidexp <- eBinOp (Add Int32)
                              (pure $ BasicOp $ SubExp ivv)
                              (pure $ BasicOp $ SubExp $ Var loopid)
                myLetBind ivvplidexp ivvplid
                locoutid_t <- lookupType locoutid
                (dest:_) <- letwith [identName glboutLid] (pexp (Var $ identName ivvplid))
                                        [BasicOp $ Index locoutid $
                                         fullSlice locoutid_t [DimFix (Var loopid)]]
                return $ resultBody [Var dest]
            -- make loop
            let loopres = DoLoop [] outmerge
                          (ForLoop loopid Int32 locoutid_size []) loopbody
            myLetBind loopres glboutBdId
            return (malloc', mind', glboutBdId)

transformSOAC pat (Scatter len lam ivs as) = do
  iter <- newVName "write_iter"

  let (_as_ws, as_ns, as_vs) = unzip3 as
  ts <- mapM lookupType as_vs
  asOuts <- mapM (newIdent "write_out") ts

  let ivsLen = length (lambdaReturnType lam) `div` 2

  -- Scatter is in-place, so we use the input array as the output array.
  let merge = loopMerge asOuts $ map Var as_vs
  loopBody <- runBodyBinder $
    localScope (M.insert iter (IndexInfo Int32) $
                scopeOfFParams $ map fst merge) $ do
    ivs' <- forM ivs $ \iv -> do
      iv_t <- lookupType iv
      letSubExp "write_iv" $ BasicOp $ Index iv $ fullSlice iv_t [DimFix $ Var iter]
    ivs'' <- bindLambda lam (map (BasicOp . SubExp) ivs')

    let indexes = chunks as_ns $ take ivsLen ivs''
        values = chunks as_ns $ drop ivsLen ivs''

    ress <- forM (zip3 indexes values (map identName asOuts)) $ \(indexes', values', arr) -> do
      let saveInArray arr' (indexCur, valueCur) =
            letExp "write_out" =<< eWriteArray arr' [eSubExp indexCur] (eSubExp valueCur)

      foldM saveInArray arr $ zip indexes' values'
    return $ resultBody (map Var ress)
  letBind_ pat $ DoLoop [] merge (ForLoop iter Int32 len []) loopBody

-- | Recursively first-order-transform a lambda.
transformLambda :: (MonadFreshNames m,
                    Bindable lore, BinderOps lore,
                    LocalScope somelore m,
                    SameScope somelore lore,
                    LetAttr SOACS ~ LetAttr lore,
                    CanBeAliased (Op lore)) =>
                   Lambda -> m (AST.Lambda lore)
transformLambda (Lambda params body rettype) = do
  body' <- runBodyBinder $
           localScope (scopeOfLParams params) $
           transformBody body
  return $ Lambda params body' rettype

newFold :: Transformer m =>
           String -> [(SubExp,Type)] -> [VName]
        -> m ([Ident], [SubExp], [Ident])
newFold what accexps_and_types arrexps = do
  initacc <- mapM copyIfArray acc_exps
  acc <- mapM (newIdent "acc") acc_types
  arrts <- mapM lookupType arrexps
  inarrs <- mapM (newIdent $ what ++ "_inarr") arrts
  return (acc, initacc, inarrs)
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
   Array {} -> letExp (baseString v ++ "_first_order_copy") $ BasicOp $ Copy v
   _        -> return v

index :: (HasScope lore m, Monad m) =>
         [VName] -> SubExp -> m [AST.Exp lore]
index arrs i = forM arrs $ \arr -> do
  arr_t <- lookupType arr
  return $ BasicOp $ Index arr $ fullSlice arr_t [DimFix i]

resultArray :: Transformer m => [Type] -> m [VName]
resultArray = mapM oneArray
  where oneArray t = letExp "result" $ BasicOp $ Scratch (elemType t) (arrayDims t)

letwith :: Transformer m =>
           [VName] -> m (AST.Exp (Lore m)) -> [AST.Exp (Lore m)]
        -> m [VName]
letwith ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  let update k v = do
        k_t <- lookupType k
        letInPlace "lw_dest" k (fullSlice k_t [DimFix i']) $ BasicOp $ SubExp v
  zipWithM update ks vs'

pexp :: Applicative f => SubExp -> f (AST.Exp lore)
pexp = pure . BasicOp . SubExp

bindLambda :: Transformer m =>
              AST.Lambda (Lore m) -> [AST.Exp (Lore m)]
           -> m [SubExp]
bindLambda (Lambda params body _) args = do
  forM_ (zip params args) $ \(param, arg) ->
    if primType $ paramType param
    then letBindNames [paramName param] arg
    else letBindNames [paramName param] =<< eCopy (pure arg)
  bodyBind body

loopMerge :: [Ident] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge vars = loopMerge' $ zip vars $ repeat Unique

loopMerge' :: [(Ident,Uniqueness)] -> [SubExp] -> [(Param DeclType, SubExp)]
loopMerge' vars vals = [ (Param pname $ toDecl ptype u, val)
                       | ((Ident pname ptype, u),val) <- zip vars vals ]

discardPattern :: (MonadFreshNames m, LetAttr (Lore m) ~ LetAttr SOACS) =>
                  [Type] -> AST.Pattern (Lore m) -> m (AST.Pattern (Lore m))
discardPattern discard pat = do
  discard_pat <- basicPattern [] <$> mapM (newIdent "discard") discard
  return $ discard_pat <> pat

data MEQType = ExactBd SubExp
             | UpperBd SubExp
             | UnknownBd

-- | Turn a Haskell-style mapAccumL into a sequential do-loop.  This
-- is the guts of transforming a 'Redomap'.
doLoopMapAccumL :: (LocalScope (Lore m) m, MonadBinder m,
                    Bindable (Lore m), BinderOps (Lore m),
                    LetAttr (Lore m) ~ Type,
                    CanBeAliased (Op (Lore m))) =>
                   SubExp
                -> AST.Lambda (Aliases (Lore m))
                -> [SubExp]
                -> [VName]
                -> [VName]
                -> m (AST.Exp (Lore m))
doLoopMapAccumL width innerfun accexps arrexps mapout_arrs = do
  (merge, i, loopbody) <-
    doLoopMapAccumL' width innerfun accexps arrexps mapout_arrs
  return $ DoLoop [] merge (ForLoop i Int32 width []) loopbody

doLoopMapAccumL' :: (LocalScope (Lore m) m, MonadBinder m,
                     Bindable (Lore m), BinderOps (Lore m),
                    LetAttr (Lore m) ~ Type,
                    CanBeAliased (Op (Lore m))) =>
                   SubExp
                -> AST.Lambda (Aliases (Lore m))
                -> [SubExp]
                -> [VName]
                -> [VName]
                -> m ([(AST.FParam (Lore m), SubExp)], VName, AST.Body (Lore m))
doLoopMapAccumL' width innerfun accexps arrexps mapout_arrs = do
  i <- newVName "i"
  -- for the MAP    part
  let acc_num     = length accexps
  let res_tps     = lambdaReturnType innerfun
  let map_arr_tps = drop acc_num res_tps
  let res_ts = [ arrayOf t (Shape [width]) NoUniqueness
               | t <- map_arr_tps ]
  let accts = map paramType $ fst $ splitAt acc_num $ lambdaParams innerfun
  outarrs <- mapM (newIdent "mapaccum_outarr") res_ts
  -- for the REDUCE part
  (acc, initacc, inarrs) <- newFold "mapaccum" (zip accexps accts) arrexps
  let consumed = consumedInBody $ lambdaBody innerfun
      withUniqueness p | identName p `S.member` consumed = (p, Unique)
                       | p `elem` outarrs = (p, Unique)
                       | otherwise = (p, Nonunique)
      merge = loopMerge' (map withUniqueness $ inarrs++acc++outarrs)
              (map Var arrexps++initacc++map Var mapout_arrs)
  loopbody <- runBodyBinder $ localScope (scopeOfFParams $ map fst merge) $ do
    accxis<- bindLambda (removeLambdaAliases innerfun) .
             (map (BasicOp . SubExp . Var . identName) acc ++) =<<
              index (map identName inarrs) (Var i)
    let (acc', xis) = splitAt acc_num accxis
    dests <- letwith (map identName outarrs) (pexp (Var i)) $
             map (BasicOp . SubExp) xis
    return $ resultBody (map (Var . identName) inarrs ++ acc' ++ map Var dests)
  return (merge, i, loopbody)
