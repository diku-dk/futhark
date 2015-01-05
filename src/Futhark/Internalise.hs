-- |
--
-- This module implements a transformation on Futhark programs that
-- simplifies various uses of tuples.  The input program must be
-- uniquely named (as by the "Futhark.Renamer" module).  The output program
-- has the following properties:
--
--    * No function accepts a tuple as an argument.  Instead, they
--    have been rewritten to accept the tuple components explicitly.
--
--    * All tuples are flat - that is, their components are not
--    tuples.  @(t1,(t2,t3))@ is rewritten to @(t1,t2,t3)@.
--
--    * There are no arrays of tuples.  @[(t1,t2)]@ is rewritten to
--    @([t1], [t2])@.
--
--    * All bindings are full.  @let v = (x,y)@ is rewritten to @let
--    (v_1, v_2) = (x,y)@.  Combined with the first property, this
--    implies that no variable is ever bound to a tuple.
--
--    * SOACs are converted to their tuple versions.
--
module Futhark.Internalise
  ( internaliseProg
  , internaliseType
  , internaliseValue
  , internaliseParamValues
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)

import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.List
import Data.Loc
import Data.Traversable (mapM)

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I
import Futhark.Renamer as I
import Futhark.MonadFreshNames
import Futhark.Tools

import Futhark.Internalise.Monad
import Futhark.Internalise.AccurateSizes
import Futhark.Internalise.TypesValues
import Futhark.Internalise.Bindings
import Futhark.Internalise.Lambdas

import Prelude hiding (mapM)

-- | Convert a program in external Futhark to a program in internal
-- Futhark.  If the boolean parameter is false, do not add bounds
-- checks to array indexing.
internaliseProg :: Bool -> E.Prog -> I.Prog
internaliseProg doBoundsCheck prog =
  I.renameProg $
  I.Prog $ runInternaliseM doBoundsCheck prog $
           mapM internaliseFun $ E.progFunctions prog

internaliseFun :: E.FunDec -> InternaliseM I.FunDec
internaliseFun (fname,rettype,params,body,loc) =
  bindingParams params $ \shapeparams params' -> do
    body' <- internaliseBody body
    let mkFParam = flip Bindee ()
    return $ FunDec
      fname rettype'
      (map mkFParam $ shapeparams ++ params')
      body' loc
  where rettype' = extShapes $ map I.toDecl $ internaliseType rettype

internaliseIdent :: E.Ident -> InternaliseM I.Ident
internaliseIdent (E.Ident name tp loc) =
  case internaliseType tp of
    [I.Basic tp'] -> return $ I.Ident name (I.Basic tp') loc
    _             -> fail "Futhark.Internalise.internaliseIdent: asked to internalise non-basic-typed ident."

internaliseCerts :: E.Certificates -> I.Certificates
internaliseCerts = map internaliseCert
  where internaliseCert (E.Ident name _ loc) =
          I.Ident name (I.Basic I.Cert) loc

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertBindingsM $ do
  ses <- internaliseExp "res" e
  return $ resultBody [] ses $ srclocOf e

internaliseExp :: String -> E.Exp -> InternaliseM [I.SubExp]

internaliseExp _ (E.Var var) = do
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  case subst of
    Nothing     -> (:[]) . I.Var <$> internaliseIdent var
    Just substs ->
      return $ concatMap insertSubst substs
  where insertSubst (DirectSubst v)   = [I.Var v]
        insertSubst (ArraySubst c ks) = c : map I.Var ks

internaliseExp desc (E.Index cs var csidx idxs loc) = do
  idxs' <- mapM (internaliseExp1 "i") idxs
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  let cs' = internaliseCerts cs
      mkCerts vs = case csidx of
                     Just csidx' -> return $ internaliseCerts csidx'
                     Nothing     -> boundsChecks vs idxs'
  case subst of
    Nothing ->
      fail $ "Futhark.Internalise.internaliseExp Index: unknown variable " ++ textual (E.identName var) ++ "."
    Just [ArraySubst c vs] -> do
      c' <- mergeSubExpCerts (c:map I.Var cs')
      csidx' <- mkCerts vs
      let index v = I.PrimOp $ I.Index (certify c' csidx') v idxs' loc
          certSubExps [] = []
          certSubExps (a:as)
            | E.arrayRank outtype == 0 = a:as
            | otherwise                = tuplit c' loc $ a:as
      certSubExps <$> letSubExps desc (map index vs)
    Just [DirectSubst var'] -> do
      csidx' <- mkCerts [var']
      letTupExp' desc $ I.PrimOp $ I.Index (cs'++csidx') var' idxs' loc
    Just _ ->
      fail $ "Futhark.Internalise.internaliseExp Index: " ++ textual (E.identName var) ++ " is not an aray."

  where outtype = E.stripArray (length idxs) $ E.identType var

internaliseExp desc (E.TupLit es _) = do
  ks <- tupsToIdentList desc es
  return $ map I.Var $ combCertExps ks

internaliseExp desc (E.ArrayLit [] et loc) =
  case internaliseType et of
    [et'] -> letTupExp' desc $ arrayLit et'
    ets -> do
      es <- letSubExps "arr_elem" $ map arrayLit ets
      return $ given loc : es
  where arrayLit et' =
          I.PrimOp $ I.ArrayLit [] (et' `annotateArrayShape` ([],loc)) loc

internaliseExp desc (E.ArrayLit es rowtype loc) = do
  aes <- tupsToIdentList "arr_elem" es
  let (cs, es'@((e':_):_)) = unzip aes --- XXX, ugh.
      Shape rowshape = arrayShape $ I.identType e'
  case internaliseType rowtype of
    [et] -> letTupExp' desc $ I.PrimOp $
            I.ArrayLit (map I.Var $ concat es')
            (et `setArrayShape` Shape rowshape) loc
    ets   -> do
      let arraylit ks et =
            I.PrimOp $ I.ArrayLit (map I.Var ks)
            (et `setArrayShape` Shape rowshape) loc
      c <- mergeCerts $ catMaybes cs
      tuplit c loc <$> letSubExps "arr_elem" (zipWith arraylit (transpose es') ets)

internaliseExp desc (E.Apply fname args _ loc)
  | "trace" <- nameToString fname = do
  args' <- tupsToIdentList "arg" $ map fst args
  let args'' = concatMap tag args'
  letTupExp' desc $
    I.Apply fname args'' (staticShapes $ map (subExpType . fst) args'')  loc
  where tag (_,vs) = [ (I.Var v, I.Observe) | v <- vs ]

internaliseExp desc (E.Apply fname args _ loc)
  | Just (rettype, _) <- HM.lookup fname builtInFunctions = do
  args' <- tupsToIdentList "arg" $ map fst args
  let args'' = concatMap tag args'
  letTupExp' desc $ I.Apply fname args'' [I.Basic rettype] loc
  where tag (_,vs) = [ (I.Var v, I.Observe) | v <- vs ]

internaliseExp desc (E.Apply fname args rettype loc) = do
  args' <- tupsToIdentList "arg" $ map fst args
  paramts <- lookupFunctionParams fname
  let args''   = concatMap addCertsAndShapes $ zip args' $ map I.diet paramts
      rettype' = extShapes $ internaliseType rettype
  letTupExp' desc $ I.Apply fname (prefixArgShapes args'') rettype' loc
  where addCertsAndShapes ((c,vs),d) =
          let observe v = (v, d)
              vs' = map (observe . I.Var) vs
          in (case c of Just c' -> [observe $ I.Var c']
                        Nothing -> []) ++ vs'

internaliseExp desc (E.LetPat pat e body loc) = do
  (c,ks) <- tupToIdentList desc e
  bindingTupIdent pat
    (Just $ certOrGiven loc c)
    (I.staticShapes $ map I.identType ks) $ \pat' -> do
    forM_ (zip (patternIdents pat') $ map I.Var ks) $ \(p,se) ->
      letBind (basicPattern [p]) $ I.PrimOp $ I.SubExp se
    internaliseExp desc body

internaliseExp desc (E.DoLoop mergepat mergeexp i bound loopbody letbody loc) = do
  bound' <- internaliseExp1 "bound" bound
  (c,mergevs) <- tupToIdentList "loop_init" mergeexp
  i' <- internaliseIdent i
  mergeparams <- map E.toParam <$> flattenPattern mergepat
  (loopbody', mergepat', respat) <-
    withNonuniqueReplacements $ bindingParams mergeparams $ \shapepat mergepat' -> do
      loopbody' <- internaliseBody loopbody
      let Result cs ses resloc = bodyResult loopbody'
          loopbody'' =
            loopbody' {
              bodyResult = Result cs (concatMap subExpShape ses++ses) resloc
              }
      return (loopbody'',
              shapepat ++ mergepat',
              mergepat')
  let mergeexp' = prefixSubExpShapes $ map I.Var $ maybeToList c ++ mergevs
      merge = [ (Bindee ident (), e) |
                (ident, e) <- zip mergepat' mergeexp' ]
      loop = I.LoopOp $ I.DoLoop respat merge i' bound' loopbody' loc
  bindingTupIdent mergepat Nothing (I.expExtType loop) $ \mergepat'' -> do
    letBind_ mergepat'' loop
    internaliseExp desc letbody

internaliseExp desc (E.LetWith cs name src idxcs idxs ve body loc) = do
  idxs' <- mapM (internaliseExp1 "idx") idxs
  (c1,srcs) <- tupToIdentList "src" $ E.Var src
  (c2,vnames) <- tupToIdentList "lw_val" ve
  let cs' = internaliseCerts cs
  idxcs' <- case idxcs of
              Just idxcs' -> return $ internaliseCerts idxcs'
              Nothing     -> boundsChecks srcs idxs'
  c <- mergeCerts (catMaybes [c1,c2]++cs')
  let comb sname vname =
        letExp "letwith_dst" $
        I.PrimOp $ I.Update (cs'++idxcs') sname idxs' (I.Var vname) loc
  dsts <- zipWithM comb srcs vnames
  bindingTupIdent (E.Id name) (Just $ certOrGiven loc c)
    (I.staticShapes $ map I.identType dsts) $ \pat' -> do
    forM_ (zip (patternIdents pat') dsts) $ \(p,dst) ->
      letBind (basicPattern [p]) $ I.PrimOp $ I.SubExp $ I.Var dst
    internaliseExp desc body

internaliseExp desc (E.Replicate ne ve loc) = do
  ne' <- internaliseExp1 "n" ne
  (_,ves) <- tupToIdentList "replicate_v" ve -- XXX - ignoring certificate?
  case ves of
    [ve'] -> letTupExp' desc $ I.PrimOp $ I.Replicate ne' (I.Var ve') loc
    _ -> do reps <- letSubExps desc [I.PrimOp $ I.Replicate ne' (I.Var ve') loc | ve' <- ves ]
            return $ given loc : reps

internaliseExp desc (E.Size _ i e loc) = do
  (_,ks) <- tupToIdentList desc e
  -- XXX: Throwing away certificates?
  case ks of
    (k:_) -> return [I.arraySize i $ I.identType k]
    _     -> return [I.intconst 0 loc] -- Will this ever happen?

internaliseExp desc (E.Unzip e _ _) = do
  (_,ks) <- tupToIdentList desc e
  return $ map I.Var ks

internaliseExp desc (E.Zip es loc) = do
  lst <- tupsToIdentList desc (map fst es)
  let (cs1, names) = splitCertExps lst
  case names of
    [] -> return []
    _ -> do
      let namevs = map I.Var names
          rows e = arraySize 0 $ I.subExpType e
          ass e1 e2 = do
            cmp <- letSubExp "zip_cmp" $ PrimOp $
                   I.BinOp I.Equal (rows e1) (rows e2) (I.Basic I.Bool) loc
            return $ I.PrimOp $ I.Assert cmp loc
      cs2 <- letExps "zip_assert" =<< zipWithM ass namevs (drop 1 namevs)
      c <- mergeCerts (cs1++cs2)
      return $ tuplit c loc namevs

internaliseExp _ (E.Transpose cs k n e loc) =
  internaliseOperation "transpose" cs e loc $ \cs' v ->
    let rank = I.arrayRank $ I.identType v
        perm = I.transposeIndex k n [0..rank-1]
    in  return $ I.Rearrange cs' perm (I.Var v) loc

internaliseExp _ (E.Rearrange cs perm e loc) =
  internaliseOperation "rearrange" cs e loc $ \cs' v ->
    return $ I.Rearrange cs' perm (I.Var v) loc

internaliseExp _ (E.Rotate cs n e loc) =
  internaliseOperation "rotate" cs e loc $ \cs' v ->
    return $ I.Rotate cs' n (I.Var v) loc

internaliseExp _ (E.Reshape cs shape e loc) = do
  shape' <- mapM (internaliseExp1 "shape") shape
  internaliseOperation "reshape" cs e loc $ \cs' v -> do
    -- The resulting shape needs to have the same number of elements
    -- as the original shape.
    shapeOk <- letExp "shape_ok" =<<
               eAssert (eBinOp I.Equal (prod $ I.arrayDims $ I.identType v)
                                       (prod shape')
                                       (I.Basic I.Bool) loc)
    return $ I.Reshape (shapeOk:cs') shape' (I.Var v) loc
  where prod l = foldBinOp I.Times (intconst 1 loc) l $ I.Basic I.Int

internaliseExp _ (E.Split cs nexp arrexp loc) = do
  let cs' = internaliseCerts cs
  nexp' <- internaliseExp1 "n" nexp
  (c, arrs) <- tupToIdentList "split_arr" arrexp
  ressize <- letSubExp "split_size" $
             PrimOp $ I.BinOp I.Minus (arraysSize 0 $ map I.identType arrs)
             nexp' (I.Basic Int) loc
  cs'' <- mergeCerts $ certify c cs'
  partnames <- forM (map I.identType arrs) $ \et -> do
    a <- fst <$> newVar loc "split_a" (et `setOuterSize` nexp')
    b <- fst <$> newVar loc "split_b" (et `setOuterSize` ressize)
    return (a, b)
  let cert = maybe (given loc) I.Var cs''
      combsplit arr (a,b) =
        letBind_ (basicPattern [a,b]) $
        PrimOp $ I.Split (certify c []) nexp' (I.Var arr) ressize loc
      els = case arrs of
              []  -> []
              [_] -> map (I.Var . fst) partnames ++
                     map (I.Var . snd) partnames
              _   -> (cert : map (I.Var . fst) partnames) ++
                     (cert : map (I.Var . snd) partnames)
  zipWithM_ combsplit arrs partnames
  return els

internaliseExp desc (E.Concat cs x y loc) = do
  (xc,xs) <- tupToIdentList "concat_x" x
  (yc,ys) <- tupToIdentList "concat_y" y
  let cs' = internaliseCerts cs
  ressize <- letSubExp "concat_size" $ I.PrimOp $
             I.BinOp I.Plus (arraysSize 0 $ map I.identType xs)
             (arraysSize 0 $ map I.identType ys)
             (I.Basic Int) loc
  let certs = catMaybes [xc,yc]++cs'
      conc xarr yarr = do
        -- The inner sizes must match.
        let matches n m =
              letExp "match" =<<
              eAssert (pure $ I.PrimOp $ I.BinOp I.Equal n m (I.Basic I.Bool) loc)
        matchcs <- zipWithM matches (drop 1 $ I.arrayDims $ I.identType xarr)
                                    (drop 1 $ I.arrayDims $ I.identType yarr)
        return $ I.PrimOp $
          I.Concat (matchcs++certs) (I.Var xarr) (I.Var yarr) ressize loc
  c' <- mergeCerts certs
  concs <- letSubExps desc =<< zipWithM conc xs ys
  return $ tuplit c' loc concs

internaliseExp desc (E.Map lam arr loc) = do
  (c,arrs) <- tupToIdentList "map_arr" arr
  let cs = certify c []
  se <- conjoinCerts cs loc
  lam' <- withNonuniqueReplacements $
          internaliseMapLambda internaliseBody se lam $ map I.Var arrs
  certifySOAC desc se $ I.Map cs lam' (map I.Var arrs) loc

internaliseExp desc e@(E.Reduce lam ne arr loc) = do
  (c1,arrs) <- tupToIdentList "reduce_arr" arr
  (c2,nes) <- tupToIdentList "reduce_ne" ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam' <- withNonuniqueReplacements $
          internaliseFoldLambda internaliseBody se lam
          (map I.identType nes) (map I.identType arrs)
  let input = zip (map I.Var nes) (map I.Var arrs)
  certifyFoldSOAC desc se t $ I.Reduce cs lam' input loc
  where t = internaliseType $ E.typeOf e

internaliseExp desc e@(E.Scan lam ne arr loc) = do
  (c1,arrs) <- tupToIdentList "scan_arr" arr
  (c2,nes) <- tupToIdentList "scan_ne" ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam' <- withNonuniqueReplacements $
          internaliseFoldLambda internaliseBody se lam
          (map I.identType nes) (map I.identType arrs)
  let input = zip (map I.Var nes) (map I.Var arrs)
  certifyFoldSOAC desc se t $ I.Scan cs lam' input loc
  where t = internaliseType $ E.typeOf e

internaliseExp desc (E.Filter lam arr loc) = do
  (c,arrs) <- tupToIdentList "filter_arr" arr
  let cs = catMaybes [c]
  se <- conjoinCerts cs loc
  lam' <- withNonuniqueReplacements $
          internaliseFilterLambda internaliseBody se lam $ map I.Var arrs
  certifySOAC desc se $ I.Filter cs lam' (map I.Var arrs) loc

internaliseExp desc e@(E.Redomap lam1 lam2 ne arrs loc) = do
  (c1,arrs') <- tupToIdentList "redomap_arr" arrs
  (c2,nes) <- tupToIdentList "redomap_ne" ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam1' <- withNonuniqueReplacements $
           internaliseFoldLambda internaliseBody se lam1
           (map I.identType nes) (map I.identType nes)
  lam2' <- withNonuniqueReplacements $
           internaliseFoldLambda internaliseBody se lam2
           (map I.identType nes) (map I.identType arrs')
  certifyFoldSOAC desc se t $
    I.Redomap cs lam1' lam2' (map I.Var nes) (map I.Var arrs') loc
  where t = internaliseType $ E.typeOf e

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp desc (E.Iota e loc) = do
  e' <- internaliseExp1 "n" e
  letTupExp' desc $ I.PrimOp $ I.Iota e' loc

internaliseExp _ (E.Literal v loc) =
  mapM (letSubExp "literal" <=< (`eValue` loc)) $ internaliseValue v

internaliseExp desc (E.If ce te fe t loc) = do
  ce' <- internaliseExp1 "cond" ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  let t' = extShapes $ internaliseType t
  letTupExp' desc $ I.If ce' te' fe' t' loc

internaliseExp desc (E.BinOp bop xe ye t loc) = do
  xe' <- internaliseExp1 "x" xe
  ye' <- internaliseExp1 "y" ye
  case internaliseType t of
    [I.Basic t'] -> letTupExp' desc $
                    I.PrimOp $ I.BinOp bop xe' ye' (I.Basic t') loc
    _            -> fail "Futhark.Internalise.internaliseExp: non-basic type in BinOp."

internaliseExp desc (E.Not e loc) = do
  e' <- internaliseExp1 "not_arg" e
  letTupExp' desc $ I.PrimOp $ I.Not e' loc

internaliseExp desc (E.Negate e loc) = do
  e' <- internaliseExp1 "negate_arg" e
  letTupExp' desc $ I.PrimOp $ I.Negate e' loc

internaliseExp desc (E.Assert e loc) = do
  e' <- internaliseExp1 "assert_arg" e
  letTupExp' desc $ I.PrimOp $ I.Assert e' loc

internaliseExp desc (E.Copy e loc) = do
  ses <- internaliseExp "copy_arg" e
  letSubExps desc [I.PrimOp $ I.Copy se loc | se <- ses]

internaliseExp desc (E.Conjoin es loc) = do
  es' <- concat <$> mapM (internaliseExp "conjoin_arg") es
  letTupExp' desc $ I.PrimOp $ I.Conjoin es' loc

internaliseExp1 :: String -> E.Exp -> InternaliseM I.SubExp
internaliseExp1 desc e = do
  vs <- internaliseExp desc e
  case vs of [se] -> return se
             _ -> fail "Internalise.internaliseExp1: was passed not just a single subexpression"

tupToIdentList :: String -> E.Exp -> InternaliseM (Maybe I.Ident, [I.Ident])
tupToIdentList desc e = do
  ses <- internaliseExp desc e
  case ses of
    [] -> return (Nothing, [])
    [I.Var var] -> return (Nothing, [var]) -- Just to avoid too many spurious bindings.
    [e'] -> do name <- letExp "val" $ PrimOp $ SubExp e'
               return (Nothing, [name])
    _ -> do
      let ts = map subExpType ses
      vs <- mapM identForType ts
      zipWithM_ letBind (map (I.basicPattern . (:[])) vs) $ map (I.PrimOp . SubExp) ses
      let (certvs, valuevs) = partition ((==I.Basic Cert) . I.identType) vs
      case certvs of
        []  -> return (Nothing, vs)
        [c] -> return (Just c, valuevs)
        _   -> do
          cert <- letExp "tup_arr_cert_comb" $
                  I.PrimOp $ I.Conjoin (map I.Var certvs) loc
          return (Just cert, valuevs)
  where loc = srclocOf e
        identForType (I.Basic Cert) = newIdent "tup_arr_cert" (I.Basic Cert) loc
        identForType t              = newIdent "tup_arr_elem" t loc

tupsToIdentList :: String -> [E.Exp] -> InternaliseM [(Maybe I.Ident, [I.Ident])]
tupsToIdentList desc = tupsToIdentList' []
  where tupsToIdentList' acc [] = return acc
        tupsToIdentList' acc (e:es) = do
          (c,e') <- tupToIdentList desc e
          tupsToIdentList' (acc++[(c,e')]) es

conjoinCerts :: I.Certificates -> SrcLoc -> InternaliseM I.Ident
conjoinCerts cs loc =
  letExp "cert" $ I.PrimOp $ I.Conjoin (map I.Var cs) loc

splitCertExps :: [(Maybe I.Ident, [I.Ident])] -> ([I.Ident], [I.Ident])
splitCertExps l = (mapMaybe fst l,
                   concatMap snd l)

combCertExps :: [(Maybe I.Ident, [I.Ident])] -> [I.Ident]
combCertExps = concatMap $ \(cert, ks) -> maybeToList cert ++ ks

mergeCerts :: [I.Ident] -> InternaliseM (Maybe I.Ident)
mergeCerts = mergeSubExpCerts . map I.Var

mergeSubExpCerts :: [I.SubExp] -> InternaliseM (Maybe I.Ident)
mergeSubExpCerts [] = return Nothing
mergeSubExpCerts [I.Var c] = return $ Just c
mergeSubExpCerts (c:cs) =
  Just <$> letExp "cert" (I.PrimOp $ I.Conjoin (c:cs) loc)
  where loc = srclocOf c

internaliseOperation :: String
                     -> E.Certificates
                     -> E.Exp
                     -> SrcLoc
                     -> (I.Certificates -> I.Ident -> InternaliseM I.PrimOp)
                     -> InternaliseM [I.SubExp]
internaliseOperation s cs e loc op = do
  (c,vs) <- tupToIdentList s e
  let cs' = internaliseCerts cs
  cs'' <- mergeCerts (certify c cs')
  es <- letSubExps s =<< mapM (liftM I.PrimOp . op (certify c cs')) vs
  return $ tuplit cs'' loc es

tuplit :: Maybe I.Ident -> SrcLoc -> [I.SubExp] -> [I.SubExp]
tuplit _ _ [e] = [e]
tuplit Nothing loc es = given loc:es
tuplit (Just c) _ es = I.Var c:es

-- Name suggested by Spectrum.
given :: SrcLoc -> SubExp
given = I.Constant I.Checked

certify :: Maybe I.Ident -> I.Certificates -> I.Certificates
certify k cs = maybeToList k ++ cs

certOrGiven :: SrcLoc -> Maybe I.Ident -> SubExp
certOrGiven loc = maybe (given loc) I.Var

certifySOAC :: String -> I.Ident -> I.LoopOp
            -> InternaliseM [I.SubExp]
certifySOAC desc c e  = do vs <- letTupExp desc $ LoopOp e
                           case vs of
                             [v] -> return [I.Var v]
                             _   -> return $ map I.Var $ c:vs

certifyFoldSOAC :: String -> I.Ident -> [I.TypeBase shape] -> I.LoopOp
                -> InternaliseM [I.SubExp]
certifyFoldSOAC desc c (I.Basic I.Cert : _) e =
  certifySOAC desc c e
certifyFoldSOAC desc _ _ e =
  map I.Var <$> letTupExp desc (I.LoopOp e)

boundsChecks :: [I.Ident] -> [I.SubExp] -> InternaliseM I.Certificates
boundsChecks []    _  = return []
boundsChecks (v:_) es = do
  doBoundsChecks <- asks envDoBoundsChecks
  if doBoundsChecks
  then zipWithM (boundsCheck v) [0..] es
  else return []

boundsCheck :: I.Ident -> Int -> I.SubExp -> InternaliseM I.Ident
boundsCheck v i e = do
  let size  = arraySize i $ I.identType v
      check = eBinOp LogAnd (pure lowerBound) (pure upperBound) bool loc
      lowerBound = I.PrimOp $
                   I.BinOp Leq (I.intconst 0 loc) e bool loc
      upperBound = I.PrimOp $
                   I.BinOp Less e size bool loc
  letExp "bounds_check" =<< eAssert check
  where bool = I.Basic Bool
        loc = srclocOf e

lookupFunctionParams :: Name -> InternaliseM [I.DeclType]
lookupFunctionParams name = do
  (_, paramts) <- lookupFunction name
  return $ internaliseParamTypes paramts
