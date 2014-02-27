{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
-- |
--
-- This module implements a transformation on L0 programs that
-- simplifies various uses of tuples.  The input program must be
-- uniquely named (as by the "L0.Renamer" module).  The output program
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
module L0C.Internalise
  ( internaliseProg
  , internaliseType
  , internaliseValue
  )
  where

import Control.Applicative
import Control.Monad.State  hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Writer hiding (mapM)

import qualified Data.Array as A
import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DL
import Data.Maybe
import Data.List
import Data.Loc
import Data.Traversable (mapM)

import L0C.ExternalRep as E
import L0C.InternalRep as I
import L0C.MonadFreshNames
import L0C.Tools

import Prelude hiding (mapM)

-- | Convert a program in external L0 to a program in internal L0.
internaliseProg :: E.Prog -> I.Prog
internaliseProg prog =
  I.Prog $ runInternaliseM $ mapM internaliseFun $ E.progFunctions prog
  where runInternaliseM m = fst $ evalState (runReaderT (runWriterT m) newEnv) newState
        newState = E.newNameSourceForProg prog
        newEnv = InternaliseEnv {
                   envSubsts = HM.empty
                 , envFtable = initialFtable `HM.union` ftable
                 }
        ftable = HM.fromList
                 [ (fname,(rettype, map E.identType params)) |
                   (fname,rettype,params,_,_) <- E.progFunctions prog ]

data Replacement = ArraySubst I.Ident [I.Ident]
                 | TupleSubst [I.Ident]
                   deriving (Show)

-- | A tuple of a return type and a list of argument types.
type FunBinding = (E.DeclType, [E.DeclType])

data InternaliseEnv = InternaliseEnv {
    envSubsts :: HM.HashMap VName Replacement
  , envFtable :: HM.HashMap Name FunBinding
  }

initialFtable :: HM.HashMap Name FunBinding
initialFtable = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = (E.Elem $ E.Basic t, map (E.Elem . E.Basic) ts)

type InternaliseM =
  WriterT (DL.DList Binding) (ReaderT InternaliseEnv (State VNameSource))

instance MonadFreshNames InternaliseM where
  getNameSource = get
  putNameSource = put

instance MonadBinder InternaliseM where
  addBinding      = addBindingWriter
  collectBindings = collectBindingsWriter

lookupFunction :: Name -> InternaliseM FunBinding
lookupFunction fname = do
  fun <- HM.lookup fname <$> asks envFtable
  case fun of Nothing   -> fail $ "Function '" ++ nameToString fname ++ "' not found"
              Just fun' -> return fun'

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

internaliseElemType :: Monoid (als VName) =>
                       E.GenElemType als -> [I.GenType als]
internaliseElemType (Tuple elemts) =
  concatMap internaliseType elemts
internaliseElemType (E.Basic bt)  = [I.Basic bt]

internaliseElemType' :: Monoid (als VName) =>
                     E.GenElemType als -> [I.GenType als]
internaliseElemType' (Tuple elemts) =
  concatMap internaliseType' elemts
internaliseElemType' t = internaliseElemType t

-- | Perform the tuple internaliseation on a single type.
--
-- Example (the 'setAliases' part is to disambiguate the type of the
-- aliasing information):
--
-- >>> internaliseType $ (Elem $ Tuple [Elem $ Tuple [Elem Int, Elem Real], Elem Char]) `setAliases` NoInfo
-- Elem (Tuple [Elem Int,Elem Int,Elem Real,Elem Real,Elem Char])
internaliseType :: Monoid (als VName) => E.GenType als -> [I.GenType als]

internaliseType t@(E.Array {}) =
  case internaliseType' t of et1:et2:ets -> I.Basic I.Cert : et1 : et2 : ets
                             t'          -> t'
internaliseType (E.Elem et) = internaliseElemType et

internaliseType' :: Monoid (als VName) => E.GenType als -> [I.GenType als]
internaliseType' (E.Array (E.Tuple elemts) size u als) =
  concatMap (internaliseType' . arr) elemts
  where arr t = E.arrayOf t size u `E.setAliases` als
internaliseType' (E.Array elemt size u als) =
  map (`I.setAliases` als) ets
  where ets = case internaliseElemType' $ elemt `E.setElemAliases` als of
                elemts -> map arr elemts
        size' = replicate (length size) Nothing
        arr t = I.arrayOf t size' $ internaliseUniqueness u
internaliseType' (E.Elem et) = internaliseElemType' et

internaliseValue :: E.Value -> [I.Value]
internaliseValue (E.ArrayVal arr rt) =
  case internaliseType $ E.addNames rt of
    [rt'] -> [I.arrayVal (concatMap internaliseValue $ A.elems arr) $ I.toDecl rt']
    ts
      | [] <- A.elems arr ->
        I.BasicVal I.Checked : map emptyOf ts
      | otherwise         ->
        I.BasicVal I.Checked : zipWith asarray ts (transpose arrayvalues)
  where emptyOf t = I.blankValue $ I.arrayType 1 t I.Nonunique
        asarray t vs = I.arrayVal vs t
        arrayvalues = map internaliseValue $ A.elems arr
        -- Above should never happen in well-typed program.
internaliseValue (E.TupVal vs) = concatMap internaliseValue vs
internaliseValue (E.BasicVal bv) = [I.BasicVal bv]

internaliseFun :: E.FunDec -> InternaliseM I.FunDec
internaliseFun (fname,rettype,params,body,loc) =
  binding (map E.fromParam params) $ \params' -> do
    body' <- internaliseBody body
    return (fname, rettype', map I.toParam params', body', loc)
  where rettype' = map I.toDecl $ internaliseType rettype

data InternaliseRes = FlatTuple [I.Ident]
                  | TupleArray I.Ident [I.Ident]
                  | Direct I.Ident

internaliseParam :: E.Ident -> InternaliseM InternaliseRes
internaliseParam param =
  case (internaliseType $ E.identType param, E.identType param) of
    (ct:t:ts, E.Array {}) -> do
      ns <- mapM (liftM fst . newVar loc base) $ t:ts
      cert <- fst <$> newVar loc ("zip_cert_" ++ base) ct
      return $ TupleArray cert ns
    ([paramt], _) -> return $ Direct $
                     I.Ident (E.identName param)
                             paramt
                             (E.identSrcLoc param)
    (ts, _) ->
      -- We know from internaliseIdent that none of the element
      -- types are themselves tuples.
      FlatTuple <$> mapM (liftM fst . newVar loc base) ts
  where loc = srclocOf param
        base = nameToString $ baseName $ E.identName param

binding :: [E.Ident] -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
binding params m = do
  (params', substs) <- runWriterT $ liftM concat . forM params $ \param -> do
    param' <- lift $ internaliseParam param
    case param' of
      Direct k -> return [k]
      FlatTuple ks -> do
        tell $ HM.singleton (E.identName param) $ TupleSubst ks
        return ks
      TupleArray c ks -> do
        tell $ HM.singleton (E.identName param) $ ArraySubst c ks
        return $ c:ks
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $ m params'

bindingPat :: E.TupIdent -> ([I.Ident] -> InternaliseM a) -> InternaliseM a
bindingPat (E.Wildcard t loc) m =
  m =<< mapM wildcard (internaliseType t)
  where wildcard = liftM fst . newVar loc "nameless"
bindingPat (E.Id k) m = do
  p <- internaliseParam k
  case p of
    Direct k'       -> m [k']
    FlatTuple ks    -> substing ks $ TupleSubst ks
    TupleArray c ks -> substing (c:ks) $ ArraySubst c ks
  where substing ks sub =
          let bind env =
                env { envSubsts = HM.insert (E.identName k) sub $ envSubsts env }
          in local bind $ m ks
bindingPat (E.TupId pats loc) m = do
  (ks, substs) <- runWriterT $ concat <$> mapM delve pats
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  local bind $ m ks
    where delve (E.Id k) = do
            p <- lift $ internaliseParam k
            case p of
              Direct k'    -> return [k']
              FlatTuple ks -> do
                tell $ HM.singleton (E.identName k) $ TupleSubst ks
                return ks
              TupleArray c ks -> do
                tell $ HM.singleton (E.identName k) $ ArraySubst c ks
                return $ c : ks
          delve (E.Wildcard t _) =
            lift $ mapM wildcard $ internaliseType t
          delve (TupId pats' _) =
            concat <$> mapM delve pats'
          wildcard = liftM fst . newVar loc "nameless"

internaliseIdent :: E.Ident -> InternaliseM I.Ident
internaliseIdent (E.Ident name tp loc) =
  case internaliseType tp of
    [tp'] -> return $ I.Ident name tp' loc
    _     -> fail "L0C.Internalise.internaliseIdent: asked to internalise tuple-typed ident."

internaliseCerts :: E.Certificates -> InternaliseM I.Certificates
internaliseCerts = mapM internaliseIdent

internaliseBody :: E.Exp -> InternaliseM Body
internaliseBody e = insertBindings $ do
  ses <- letTupExp "norm" =<< internaliseExp e
  return $ I.Result (map I.Var ses) $ srclocOf e

internaliseExp :: E.Exp -> InternaliseM I.Exp

internaliseExp (E.Var var) = do
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  case subst of
    Nothing -> I.SubExp <$> I.Var <$> internaliseIdent var
    Just (ArraySubst c ks) -> return $ I.TupLit (map I.Var $ c:ks) $ srclocOf var
    Just (TupleSubst ks)   -> return $ I.TupLit (map I.Var ks) $ srclocOf var

internaliseExp (E.Index cs var csidx idxs loc) = do
  idxs' <- letSubExps "i" =<< mapM internaliseExp idxs
  subst <- asks $ HM.lookup (E.identName var) . envSubsts
  cs' <- internaliseCerts cs
  csidx' <- mapM internaliseCerts csidx
  case subst of
    Just (ArraySubst _ [subv])
      | rt <- I.stripArray (length idxs') $ I.identType subv,
        I.arrayRank rt == 0 ->
      return $ I.Index cs' subv csidx' idxs' loc
    Just (ArraySubst c vs) -> do
      c' <- mergeCerts (c:cs')
      let index v = I.Index (certify c' []) v csidx' idxs' loc
          resultTupLit [] = I.TupLit [] loc
          resultTupLit (a:as)
            | E.arrayRank outtype == 0 = I.TupLit (a:as) loc
            | otherwise                = tuplit c' loc $ a:as
      resultTupLit <$> letSubExps "idx" (map index vs)
    _ -> do
      var' <- internaliseIdent var
      return $ I.Index cs' var' csidx' idxs' loc
  where outtype = E.stripArray (length idxs) $ E.identType var

internaliseExp (E.TupLit es loc) = do
  ks <- tupsToIdentList es
  return $ I.TupLit (map I.Var $ combCertExps ks) loc

internaliseExp (E.ArrayLit [] et loc) =
  case internaliseType et of
    [et'] -> return $ I.ArrayLit [] et' loc
    ets -> do
      es <- letSubExps "arr_elem" [ I.ArrayLit [] et' loc | et' <- ets ]
      return $ I.TupLit (given loc : es) loc

internaliseExp (E.ArrayLit es rowtype loc) = do
  aes <- tupsToIdentList es
  let (cs, es') = unzip aes
  case internaliseType rowtype of
    [et'] -> do
      ses <- letSubExps "arr_elem" $ map (tuplit Nothing loc . map I.Var) es'
      return $ I.ArrayLit ses et' loc
    ets   -> do
      let arraylit ks et = I.ArrayLit (map I.Var ks) et loc
      c <- mergeCerts $ catMaybes cs
      tuplit c loc <$> letSubExps "arr_elem" (zipWith arraylit (transpose es') ets)

internaliseExp (E.Apply fname args rettype loc) = do
  args' <- tupsToIdentList $ map fst args
  let args'' = concatMap flatten $ zip args' $ map snd args
  return $ I.Apply fname args'' rettype' loc
  where rettype' = internaliseType rettype
        flatten ((c, ks), _) =
          (case c of Just c' -> [(I.Var c', I.Observe)]
                     Nothing -> []) ++
          -- Diet wrong, but will be fixed by type-checker.
          [ (I.Var k, I.Observe) | k <- ks ]

internaliseExp (E.LetPat pat e body _) = do
  e' <- internaliseExp e
  bindingPat pat $ \pat' -> do
    letBind pat' e'
    internaliseExp body

internaliseExp (E.DoLoop mergepat mergeexp i bound loopbody letbody _) = do
  bound' <- letSubExp "bound" =<< internaliseExp bound
  mergevs <- letTupExp "merge_val" =<< internaliseExp mergeexp
  i' <- internaliseIdent i
  bindingPat mergepat $ \mergepat' -> do
    loopbody' <- internaliseBody loopbody
    loopBind (zip mergepat' $ map I.Var mergevs) i' bound' loopbody'
    internaliseExp letbody

internaliseExp (E.LetWith cs name src idxcs idxs ve body loc) = do
  idxs' <- letSubExps "idx" =<< mapM internaliseExp idxs
  (c1,srcs) <- tupToIdentList (E.Var src)
  (c2,vnames) <- tupToIdentList ve
  cs' <- internaliseCerts cs
  idxcs' <- mapM internaliseCerts idxcs
  dsts <- map fst <$> mapM (newVar loc "letwith_dst" . I.identType) srcs
  c <- mergeCerts (catMaybes [c1,c2]++cs')
  let comb (dname, sname, vname) =
        letWithBind cs' dname sname idxcs' idxs' $ I.Var vname
  mapM_ comb $ zip3 dsts srcs vnames
  bindingPat (E.Id name) $ \pat' -> do
    letBind pat' $ tuplit c loc (map I.Var dsts)
    internaliseExp body

internaliseExp (E.Replicate ne ve loc) = do
  ne' <- letSubExp "n" =<< internaliseExp ne
  (_,ves) <- tupToIdentList ve -- XXX - ignoring certificate?
  case ves of
    [ve'] -> return $ I.Replicate ne' (I.Var ve') loc
    _ -> do reps <- letSubExps "v" [I.Replicate ne' (I.Var ve') loc | ve' <- ves ]
            return $ I.TupLit (given loc : reps) loc

internaliseExp (E.Size cs i e loc) = do
  cs' <- internaliseCerts cs
  (c,ks) <- tupToIdentList e
  case ks of
    (k:_) -> return $ I.Size (certify c cs') i (I.Var k) loc
    []    -> return $ I.SubExp (I.Constant (I.BasicVal $ I.IntVal 0) loc) -- Will this ever happen?

internaliseExp (E.Unzip e _ _) = do
  (_,ks) <- tupToIdentList e
  return $ I.TupLit (map I.Var ks) $ srclocOf e

internaliseExp (E.Zip es loc) = do
  lst <- tupsToIdentList (map fst es)
  let (cs1, names) = splitCertExps lst
  case names of
    [] -> return $ I.TupLit [] loc
    _ -> do
      let namevs = map I.Var names
          rows e = I.Size [] 0 e loc
          ass e1 e2 = do e1' <- letSubExp "zip_len_x" $ rows e1
                         e2' <- letSubExp "zip_len_y" $ rows e2
                         cmp <- letSubExp "zip_cmp" $ I.BinOp I.Equal e1' e2' (I.Basic I.Bool) loc
                         pure $ I.Assert cmp loc
      cs2 <- letExps "zip_assert" =<< zipWithM ass namevs (drop 1 namevs)
      c <- mergeCerts (cs1++cs2)
      return $ tuplit c loc namevs

internaliseExp (E.Iota e loc) = do
  e' <- letSubExp "n" =<< internaliseExp e
  return $ I.Iota e' loc

internaliseExp (E.Transpose cs k n e loc) =
  internaliseOperation "transpose" cs e loc $ \cs' v ->
    let rank = I.arrayRank $ I.identType v
        perm = transposeIndex k n [0..rank-1]
    in  I.Rearrange cs' perm (I.Var v) loc

internaliseExp (E.Rearrange cs perm e loc) =
  internaliseOperation "rearrange" cs e loc $ \cs' v ->
    I.Rearrange cs' perm (I.Var v) loc

internaliseExp (E.Reshape cs shape e loc) = do
  shape' <- letSubExps "shape" =<< mapM internaliseExp shape
  internaliseOperation "reshape" cs e loc $ \cs' v ->
    I.Reshape cs' shape' (I.Var v) loc

internaliseExp (E.Split cs nexp arrexp loc) = do
  cs' <- internaliseCerts cs
  nexp' <- letSubExp "n" =<< internaliseExp nexp
  uncurry (internalise cs' nexp') =<< tupToIdentList arrexp
  where internalise _ _ _ [] = -- Will this ever happen?
          fail "L0C.Internalise.internaliseExp Split: Empty array"
        internalise cs' nexp' _ [arr] =
          return $ I.Split cs' nexp' (I.Var arr) loc
        internalise cs' nexp' c arrs = do
          cs'' <- mergeCerts (certify c cs')
          partnames <- forM (map I.identType arrs) $ \et -> do
                         a <- fst <$> newVar loc "split_a" et
                         b <- fst <$> newVar loc "split_b" et
                         return (a, b)
          let cert = maybe (given loc) I.Var cs''
              combsplit arr (a,b) =
                letBind [a,b] $ I.Split (certify c []) nexp' (I.Var arr) loc
              els = (cert : map (I.Var . fst) partnames) ++
                    (cert : map (I.Var . snd) partnames)
          zipWithM_ combsplit arrs partnames
          return $ I.TupLit els loc

internaliseExp (E.Concat cs x y loc) = do
  (xc,xs) <- tupToIdentList x
  (yc,ys) <- tupToIdentList y
  cs' <- internaliseCerts cs
  internalise cs' xc xs yc ys
  where internalise cs' _ [x'] _ [y'] =
         return $ I.Concat cs' (I.Var x') (I.Var y') loc
        internalise cs' xc xs yc ys = do
          let certs = catMaybes [xc,yc]++cs'
              conc xarr yarr =
                I.Concat certs (I.Var xarr) (I.Var yarr) loc
          c' <- mergeCerts certs
          concs <- letSubExps "concat" $ zipWith conc xs ys
          return $ tuplit c' loc concs

internaliseExp (E.Map lam arr _ loc) = do
  (c,arrs) <- tupToIdentList arr
  let cs = certify c []
  se <- conjoinCerts cs loc
  lam' <- internaliseLambda se lam
  certifySOAC se $ I.Map cs lam' (map I.Var arrs) loc

internaliseExp (E.Reduce lam ne arr _ loc) = do
  (c1,arrs) <- tupToIdentList arr
  (c2,nes) <- tupToIdentList ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam' <- internaliseLambda se lam
  return $ I.Reduce cs lam' (zip (map I.Var nes) (map I.Var arrs)) loc

internaliseExp (E.Scan lam ne arr _ loc) = do
  (c1,arrs) <- tupToIdentList arr
  (c2,nes) <- tupToIdentList ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam' <- internaliseLambda se lam
  certifySOAC se $ I.Scan cs lam' (zip (map I.Var nes) (map I.Var arrs)) loc

internaliseExp (E.Filter lam arr _ loc) = do
  (c,arrs) <- tupToIdentList arr
  let cs = catMaybes [c]
  se <- conjoinCerts cs loc
  lam' <- internaliseLambda se lam
  certifySOAC se $ I.Filter cs lam' (map I.Var arrs) loc

internaliseExp (E.Redomap lam1 lam2 ne arr _ loc) = do
  (c1,arrs) <- tupToIdentList arr
  (c2,nes) <- tupToIdentList ne
  let cs = catMaybes [c1,c2]
  se <- conjoinCerts cs loc
  lam1' <- internaliseLambda se lam1
  lam2' <- internaliseLambda se lam2
  return $ I.Redomap cs lam1' lam2'
           (map I.Var nes) (map I.Var arrs) loc

-- The "interesting" cases are over, now it's mostly boilerplate.

internaliseExp (E.Literal v loc) =
  return $ case internaliseValue v of
             [v'] -> I.SubExp $ I.Constant v' loc
             vs   -> I.TupLit (map (`I.Constant` loc) vs) loc

internaliseExp (E.If ce te fe t loc) = do
  ce' <- letSubExp "cond" =<< internaliseExp ce
  te' <- internaliseBody te
  fe' <- internaliseBody fe
  return $ I.If ce' te' fe' (internaliseType t) loc

internaliseExp (E.BinOp bop xe ye t loc) = do
  xe' <- letSubExp "x" =<< internaliseExp xe
  ye' <- letSubExp "y" =<< internaliseExp ye
  case internaliseType t of
    [t'] -> return $ I.BinOp bop xe' ye' t' loc
    _    -> fail "L0C.Internalise.internaliseExp: Tuple type in BinOp."

internaliseExp (E.Not e loc) = do
  e' <- letSubExp "not_arg" =<< internaliseExp e
  return $ I.Not e' loc

internaliseExp (E.Negate e loc) = do
  e' <- letSubExp "negate_arg" =<< internaliseExp e
  return $ I.Negate e' loc

internaliseExp (E.Assert e loc) = do
  e' <- letSubExp "assert_arg" =<< internaliseExp e
  return $ I.Assert e' loc

internaliseExp (E.Copy e loc) = do
  vs <- letTupExp "copy_arg" =<< internaliseExp e
  case vs of
    [v] -> return $ I.Copy (I.Var v) loc
    _    -> do ses <- letSubExps "copy_res" [I.Copy (I.Var v) loc | v <- vs]
               return $ I.TupLit ses loc

internaliseExp (E.Conjoin es loc) = do
  es' <- letSubExps "conjoin_arg" =<< mapM internaliseExp es
  return $ I.Conjoin es' loc

internaliseExp (E.MapT cs fun arrs loc) = do
  arrs' <- letSubExps "map_arg" =<< mapM internaliseExp arrs
  cs' <- internaliseCerts cs
  ce <- conjoinCerts cs' loc
  fun' <- internaliseTupleLambda ce fun
  return $ I.Map cs' fun' arrs' loc

internaliseExp (E.ReduceT cs fun inputs loc) = do
  arrs' <- letSubExps "red_arg" =<< mapM internaliseExp arrs
  accs' <- letSubExps "red_acc" =<< mapM internaliseExp accs
  cs' <- internaliseCerts cs
  ce <- conjoinCerts cs' loc
  fun' <- internaliseTupleLambda ce fun
  return $ I.Reduce cs' fun' (zip accs' arrs') loc
  where (accs, arrs) = unzip inputs

internaliseExp (E.ScanT cs fun inputs loc) = do
  arrs' <- letSubExps "scan_arg" =<< mapM internaliseExp arrs
  accs' <- letSubExps "scan_acc" =<< mapM internaliseExp accs
  cs' <- internaliseCerts cs
  ce <- conjoinCerts cs' loc
  fun' <- internaliseTupleLambda ce fun
  return $ I.Scan cs' fun' (zip accs' arrs') loc
  where (accs, arrs) = unzip inputs

internaliseExp (E.FilterT cs fun arrs loc) = do
  arrs' <- letSubExps "filter_arg" =<< mapM internaliseExp arrs
  cs' <- internaliseCerts cs
  ce <- conjoinCerts cs' loc
  fun' <- internaliseTupleLambda ce fun
  return $ I.Filter cs' fun' arrs' loc

internaliseExp (E.RedomapT cs fun1 fun2 accs arrs loc) = do
  accs' <- letSubExps "redomap_acc" =<< mapM internaliseExp accs
  arrs' <- letSubExps "redomap_arg" =<< mapM internaliseExp arrs
  cs' <- internaliseCerts cs
  ce <- conjoinCerts cs' loc
  fun1' <- internaliseTupleLambda ce fun1
  fun2' <- internaliseTupleLambda ce fun2
  return $ I.Redomap cs' fun1' fun2' accs' arrs' loc

tupToIdentList :: E.Exp -> InternaliseM (Maybe I.Ident, [I.Ident])
tupToIdentList e = do
  e' <- internaliseExp e
  case (I.typeOf e', E.typeOf e) of
    ([], _) -> return (Nothing, [])
    (ct:t:ts, E.Array {}) -> do
      cert <- fst <$> newVar loc "tup_arr_cert" ct
      names <- mapM (liftM fst . newVar loc "tup_arr_elem") $ t:ts
      letBind (cert : names) e'
      return (Just cert, names)
    ([t], _) -> case e' of
                  I.SubExp (I.Var var) ->
                    return (Nothing, [var]) -- Just to avoid too many spurious bindings.
                  _ -> do
                    name <- fst <$> newVar loc "val" t
                    letBind [name] e'
                    return (Nothing, [name])
    (ts, _) -> do
      names <- mapM (liftM fst . newVar loc "tup_elem") ts
      letBind names e'
      return (Nothing, names)
  where loc = srclocOf e

tupsToIdentList :: [E.Exp] -> InternaliseM [(Maybe I.Ident, [I.Ident])]
tupsToIdentList = tupsToIdentList' []
  where tupsToIdentList' acc [] = return acc
        tupsToIdentList' acc (e:es) = do
          (c,e') <- tupToIdentList e
          tupsToIdentList' (acc++[(c,e')]) es

conjoinCerts :: I.Certificates -> SrcLoc -> InternaliseM I.SubExp
conjoinCerts cs loc =
  letSubExp "cert" $ I.Conjoin (map I.Var cs) loc

splitCertExps :: [(Maybe I.Ident, [I.Ident])] -> ([I.Ident], [I.Ident])
splitCertExps l = (mapMaybe fst l,
                   concatMap snd l)

combCertExps :: [(Maybe I.Ident, [I.Ident])] -> [I.Ident]
combCertExps = concatMap $ \(cert, ks) -> maybeToList cert ++ ks

mergeCerts :: I.Certificates -> InternaliseM (Maybe I.Ident)
mergeCerts [] = return Nothing
mergeCerts [c] = return $ Just c
mergeCerts (c:cs) = do
  cert <- fst <$> newVar loc "comb_cert" (I.Basic I.Cert)
  letBind [cert] $ I.Conjoin (map I.Var $ c:cs) loc
  return $ Just cert
  where loc = srclocOf c

cleanLambdaParam :: I.SubExp -> [I.Ident] -> E.Type -> InternaliseM ([I.Ident], [I.SubExp])
cleanLambdaParam _ [] _ = return ([], [])
cleanLambdaParam ce ks@(k:_) t =
  case (ks, t, internaliseType t) of
    (_:ks', E.Array {}, _:_:_) -> do
      (names, namevs) <- unzip <$> mapM (newVar loc "arg" . I.identType) ks'
      return (names, ce : namevs)
    (_, E.Elem (E.Tuple ets), _) -> do
      let comb (ks', params, es) et =
            case internaliseType et of
              [_] -> do
                (newparams, newes) <-
                  cleanLambdaParam ce (take 1 ks') et
                return (drop 1 ks', params++newparams, es++newes)
              ets' -> do
                (newparams, newes) <-
                  cleanLambdaParam ce (take (length ets') ks') et
                return (drop (length ets') ks', params++newparams, es++newes)
      (_, params, es) <- foldM comb (ks, [], []) ets
      return (params, es)
    (_, _, _) -> do
      (names, namevs) <- unzip <$> mapM (newVar loc "arg" . I.identType) ks
      return (names, namevs)
  where loc = srclocOf k

lambdaBinding :: I.SubExp -> [E.Ident] -> InternaliseM I.Body -> InternaliseM (I.Body, [I.Ident])
lambdaBinding ce params m = do
  (params', (patpairs, substs)) <- runWriterT $ liftM concat . forM params $ \param -> do
    param' <- lift $ internaliseParam param
    case param' of
      Direct k -> return [k]
      FlatTuple [] -> return []
      FlatTuple ks@(k:_) -> do
        let loc = srclocOf k
        (ks', es) <- lift $ cleanLambdaParam ce ks $ E.identType param
        tell ([(ks, I.TupLit es loc)],
              HM.singleton (E.identName param) $ TupleSubst ks)
        return ks'
      TupleArray c ks -> do
        let loc = srclocOf c
        (ks', es) <- lift $ cleanLambdaParam ce (c:ks) $ E.identType param
        tell ([(c:ks, I.TupLit es loc)],
              HM.singleton (E.identName param) $ ArraySubst c ks)
        return ks'
  let bind env = env { envSubsts = substs `HM.union` envSubsts env }
  e <- insertBindings $ do
         mapM_ (uncurry letBind) patpairs
         local bind m
  return (e, params')

internaliseLambda :: I.SubExp -> E.Lambda -> InternaliseM I.Lambda
internaliseLambda ce (E.AnonymFun params body rettype loc) = do
  (body', params') <- lambdaBinding ce (map E.fromParam params) $ do
    (_,ks) <- tupToIdentList body
    return $ I.Result (map I.Var $ stripCert ks) loc
  return $ I.Lambda (map I.toParam params') body' rettype' loc
  where rettype' = case map I.toDecl $ internaliseType' rettype of
                     [t] -> [t]
                     ets -> ets
        stripCert (c:es)
          | I.identType c == I.Basic I.Cert = es -- XXX HACK
        stripCert es = es
internaliseLambda ce (E.CurryFun fname curargs rettype loc) = do
  (_,paramtypes) <- lookupFunction fname
  let missing = drop (length curargs) paramtypes
  params <- forM missing $ \t -> do
              s <- newNameFromString "curried"
              return E.Ident {
                         E.identType   = t
                       , E.identSrcLoc = loc
                       , E.identName   = s
                       }
  let observe x = (x, E.Observe) -- Actual diet doesn't matter here, the
                                 -- type checker will eventually fix it.
      call = E.Apply fname
             (map observe $ curargs ++ map (E.Var . E.fromParam) params)
             rettype loc
  internaliseLambda ce $ E.AnonymFun params call (E.toDecl rettype) loc

internaliseTupleLambda :: I.SubExp -> E.TupleLambda -> InternaliseM I.Lambda
internaliseTupleLambda ce = internaliseLambda ce . E.tupleLambdaToLambda

internaliseOperation :: String
                     -> E.Certificates
                     -> E.Exp
                     -> SrcLoc
                     -> (I.Certificates -> I.Ident -> I.Exp)
                     -> InternaliseM I.Exp
internaliseOperation s cs e loc op = do
  (c,vs) <- tupToIdentList e
  cs' <- internaliseCerts cs
  cs'' <- mergeCerts (certify c cs')
  es <- letSubExps s $ map (op (certify c cs')) vs
  return $ tuplit cs'' loc es

tuplit :: Maybe I.Ident -> SrcLoc -> [I.SubExp] -> I.Exp
tuplit _ _ [e] = SubExp e
tuplit Nothing loc es = I.TupLit (given loc:es) loc
tuplit (Just c) loc es = I.TupLit (I.Var c:es) loc

-- Name suggested by Spectrum.
given :: SrcLoc -> SubExp
given = I.Constant $ I.BasicVal I.Checked

certify :: Maybe I.Ident -> I.Certificates -> I.Certificates
certify k cs = maybeToList k ++ cs

certifySOAC :: SubExp -> I.Exp -> InternaliseM I.Exp
certifySOAC ce e =
  case I.typeOf e of
    [_] -> return e
    ts  -> do (ks,vs) <- unzip <$> mapM (newVar loc "soac") ts
              letBind ks e
              return $ I.TupLit (ce:vs) loc
  where loc = srclocOf e
