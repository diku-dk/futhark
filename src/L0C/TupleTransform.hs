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
module L0C.TupleTransform
  ( transformProg
  , transformType )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Array as A
import qualified Data.Map as M
import Data.List
import Data.Loc

import L0C.L0
import L0C.FreshNames

-- | Perform the tuple transformation on an entire program.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runTransformM $ mapM transformFun $ progFunctions prog
  where runTransformM m = evalState (runReaderT m newEnv) newState
        newState = TransformState $ newNameSourceForProg prog
        newEnv = TransformEnv M.empty

data TransformState = TransformState {
    envNameSrc :: VNameSource
  }

data TransformEnv = TransformEnv {
    envSubsts :: M.Map VName [Ident]
  }

type TransformM = ReaderT TransformEnv (State TransformState)

new :: String -> TransformM VName
new k = do (name, src) <- gets $ newVName k . envNameSrc
           modify $ \s -> s { envNameSrc = src }
           return name

flattenTypes :: [TypeBase als VName] -> [TypeBase als VName]
flattenTypes = concatMap flatten
  where flatten (Elem (Tuple ts)) = flattenTypes ts
        flatten t                 = [t]

transformElemType :: Monoid (als VName) =>
                     ElemTypeBase als VName -> ElemTypeBase als VName
transformElemType (Tuple elemts) =
  Tuple $ flattenTypes $ map transformType elemts
transformElemType t = t

-- | Perform the tuple transformation on a single type.
--
-- Example (the 'setAliases' part is to disambiguate the type of the
-- aliasing information):
--
-- >>> transformType $ (Elem $ Tuple [Elem $ Tuple [Elem Int, Elem Real], Elem Char]) `setAliases` NoInfo
-- Elem (Tuple [Elem Int,Elem Int,Elem Real,Elem Real,Elem Char])
transformType :: Monoid (als VName) => TypeBase als VName -> TypeBase als VName
transformType (Array (Tuple elemts) size u als) =
  Elem $ Tuple $ flattenTypes (map (transformType . arr) elemts)
  where arr t = arrayOf t size u `setAliases` als
transformType (Array elemt size u als) =
  case ets of
    [et] -> et
    _    -> Elem (Tuple ets) `setAliases` als
  where ets = case transformElemType $ elemt `setElemAliases` als of
                Tuple elemts -> flattenTypes $ map (transformType . arr) elemts
                elemt'       -> [arrayOf (Elem elemt') size u]
        arr t = arrayOf t size u
transformType (Elem et) = Elem $ transformElemType et

flattenValues :: [Value] -> [Value]
flattenValues = concatMap flatten
  where flatten (TupVal vs) = vs
        flatten v           = [v]

transformValue :: Value -> Value
transformValue (ArrayVal arr rt) =
  case addNames rt of
    Elem (Tuple ts)
      | [] <- A.elems arr ->
        TupVal $ flattenValues $ map emptyOf ts
      | otherwise         ->
        TupVal $ flattenValues $ zipWith asarray (flattenTypes ts) $ transpose arrayvalues
    rt' -> ArrayVal arr $ removeNames rt'
  where emptyOf t = blankValue $ arrayType 1 t Nonunique
        asarray t vs = transformValue $ arrayVal vs t
        arrayvalues = map (tupleValues . transformValue) $ A.elems arr
        tupleValues (TupVal vs) = vs
        tupleValues v = [v]
        -- Above should never happen in well-typed program.
transformValue (TupVal vs) = TupVal $ flattenValues $ map transformValue vs
transformValue v = v

transformIdent :: Ident -> Ident
transformIdent (Ident name t loc) = Ident name (transformType t) loc

transformFun :: FunDec -> TransformM FunDec
transformFun (fname,rettype,params,body,loc) =
  binding (map fromParam params) $ \params' -> do
    body' <- transformExp body
    return (fname, rettype', map toParam params', body', loc)
  where rettype' = transformType rettype

binding :: [Ident] -> ([Ident] -> TransformM a) -> TransformM a
binding params m = do
  (params', substs) <-
    runWriterT $ concat <$> mapM (transformParam . transformIdent) params
  let bind env = env { envSubsts = M.union substs $
                                   foldr (M.delete . identName) (envSubsts env)
                                   params
                     }
  local bind $ m params'
  where
    transformParam param =
      case identType param of
        Elem (Tuple ts) -> do
          -- We know from transformIdent that none of the element
          -- types are themselves tuples.
          let base = nameToString $ baseName $ identName param
          names <- mapM (liftM fst . lift . newVar (srclocOf param) base) ts
          tell $ M.singleton (identName param) names
          return names
        _ -> return [param]

flattenExps :: [Exp] -> [Exp]
flattenExps = concatMap flatten
  where flatten (TupLit es _) = es
        flatten e             = [e]

transformExp :: Exp -> TransformM Exp

transformExp (Var var) = do
  subst <- asks $ M.lookup (identName var) . envSubsts
  case subst of
    Nothing -> return $ Var var
    Just vs -> return $ TupLit (map Var vs) $ srclocOf var

transformExp (Index vname idxs outtype loc) = do
  idxs' <- mapM transformExp idxs
  subst <- asks $ M.lookup (identName vname) . envSubsts
  case subst of
    Just [v]
      | rt <- stripArray (length idxs') $ identType v,
        arrayDims rt == 0 ->
      return $ Index v idxs' rt loc
    Just vs -> do
      let index v = Index v idxs'
                    (stripArray (length idxs') $ identType v) loc
      return $ TupLit (map index vs) loc
    _ -> return $ Index vname idxs' outtype loc

transformExp (TupLit es loc) = do
  (wrap, es') <-
    foldM comb (id, []) =<< flattenExps <$> mapM transformExp es
  return $ wrap $ TupLit (reverse es') loc
  where
    comb (wrap, es') e =
      case typeOf e of
        Elem (Tuple ts) -> do
          (names, namevs) <- unzip <$> mapM (newVar loc "tuplit_elem") ts
          return (\body -> wrap $ LetPat (TupId (map Id names) loc) e body loc,
                  reverse namevs ++ es')
        _ -> return (wrap, e:es')

transformExp (ArrayLit [] intype loc) =
  return $ case transformType intype of
             Elem (Tuple ets) ->
               TupLit [ ArrayLit [] et loc | et <- ets ] loc
             et' -> TupLit [ArrayLit [] et' loc] loc

transformExp (ArrayLit es intype loc) = do
  es' <- mapM transformExp es
  case transformType intype of
    Elem (Tuple ets) -> do
      (e, bindings) <- foldM comb (id, replicate (length ets) []) es'
      return $ e $ tuparrexp (map reverse bindings) ets
        where comb (acce, bindings) e = do
                names <- mapM (liftM fst . newVar loc "array_tup") ets
                return (\body -> acce $ LetPat (TupId (map Id names) loc) e body loc
                       ,zipWith (:) names bindings)
              tuparrexp bindings ts = TupLit (zipWith arrexp ts bindings) loc
              arrexp t names = ArrayLit (map Var names) t loc
    et' -> return $ ArrayLit es' et' loc

transformExp (Apply fname args rettype loc) = do
  (args', bindings) <- foldM comb ([], id) args
  return $ bindings $ Apply fname (reverse args') rettype' loc
    where rettype' = transformType rettype
          comb (argexps', bindings) (arg, d) = do
            arg' <- transformExp arg
            case transformType (typeOf arg `dietingAs` d) of
              Elem (Tuple ets) -> do
                (names, namevs) <- unzip <$> mapM (newVar loc "array_tup") ets
                return (reverse (zip namevs $ map diet ets) ++ argexps',
                        \body ->
                          bindings $
                          LetPat (TupId (map Id names) loc) arg' body loc)
              _ -> return ((arg', d) : argexps', bindings)

transformExp (DoLoop mergepat mergeexp i bound loopbody letbody loc) = do
  bound' <- transformExp bound
  mergeexp' <- transformExp mergeexp
  withPattern mergepat $ \mergepat' -> do
    loopbody' <- transformExp loopbody
    letbody' <- transformExp letbody
    return $ DoLoop mergepat' mergeexp' i bound' loopbody' letbody' loc

transformExp (LetWith name src idxs ve body loc) = do
  idxs' <- mapM transformExp idxs
  ve' <- transformExp ve
  tupToIdentList (Var src) $ \srcs -> do
    (vnames, vlet) <- case typeOf ve' of
                        Elem (Tuple xts) -> do
                          vnames <- map fst <$> mapM (newVar loc "letwith_el") xts
                          let vlet inner = LetPat (TupId (map Id vnames) loc) ve' inner loc
                          return (map Var vnames, vlet)
                        _ -> return ([ve'], id)
    let comb olde (sname, v) inner =
          LetWith sname sname idxs' v (olde inner) loc
    let lws = foldl comb id $ zip srcs vnames
    inner <- transformExp $ LetPat (Id name) (tuplit (map Var srcs) loc) body loc
    return $ vlet $ lws inner

transformExp (Replicate ne ve loc) = do
  ne' <- transformExp ne
  ve' <- transformExp ve
  case typeOf ve' of
    Elem (Tuple ets) -> do
      (n, nv) <- newVar loc "n" $ Elem Int
      (names, vs) <- unzip <$> mapM (newVar loc "rep_tuple") ets
      let arrexp v = Replicate nv v loc
          nlet body = LetPat (Id n) ne' body loc
          tuplet body = LetPat (TupId (map Id names) loc) ve' body loc
      return $ nlet $ tuplet $ TupLit (map arrexp vs) loc
    _ -> return $ Replicate ne' ve' loc

transformExp (Shape e loc) = do
  e' <- transformExp e
  case typeOf e' of
    Elem (Tuple (et:ets)) -> do
      (name, namev) <- newVar loc "size_tup" et
      names <- map fst <$> mapM (newVar loc "size_tup") ets
      size <- transformExp $ Shape namev loc
      return $ LetPat (TupId (map Id $ name : names) loc) e' size loc
    _ -> return $ Shape e' loc

transformExp (Unzip e _ _) = transformExp e

transformExp (Zip es loc) = do
  es' <- mapM (transformExp . fst) es
  (dead, _) <- newVar loc "dead" $ Elem Bool
  let comb (e:es'') namevs = tupToExpList e $ \tes -> do
        (names, newnamevs) <- unzip <$> mapM (newVar loc "zip_elem" . typeOf) tes
        body <- comb es'' (namevs ++ newnamevs)
        return $ LetPat (TupId (map Id names) loc) (TupLit tes loc) body loc
      comb [] namevs = return $ LetPat (Id dead) azip (TupLit namevs loc) loc
        where azip = Apply (nameFromString "assertZip")
                           (zip namevs $ repeat Observe) (Elem Bool) loc
  comb es' []

transformExp (Iota e loc) = do
  e' <- transformExp e
  return $ Iota e' loc

transformExp (Transpose k n e loc) =
  tupToExpList e $ \es ->
    return $ tuplit [Transpose k n e' loc | e' <- es] loc

transformExp (Reshape shape e loc) =
  tupToExpList e $ \es ->
    return $ tuplit [Reshape shape e' loc | e' <- es] loc

transformExp (Split nexp arrexp eltype loc) = do
  nexp' <- transformExp nexp
  arrexp' <- transformExp arrexp
  case typeOf arrexp' of
    Elem (Tuple ets) -> do
      (n, nv) <- newVar loc "split_n" $ typeOf nexp'
      names <- map fst <$> mapM (newVar loc "split_tup") ets
      partnames <- forM ets $ \et -> do
                     a <- fst <$> newVar loc "split_a" et
                     b <- fst <$> newVar loc "split_b" et
                     return (a, b)
      let letn body = LetPat (Id n) nexp' body loc
          letarr body = LetPat (TupId (map Id names) loc) arrexp' body loc
          combsplit olde (arr, (a,b), et) inner =
            olde $ LetPat (TupId [Id a, Id b] loc) (Split nv (Var arr) et loc) inner loc
          letsplits = foldl combsplit id $
                      zip3 names partnames $
                      map (stripArray 1) ets
          res = TupLit (map (Var . fst) partnames ++
                        map (Var . snd) partnames) loc
      return $ letn $ letarr $ letsplits res
    _ -> return $ Split nexp' arrexp' (transformType eltype) loc

transformExp (Concat x y loc) = do
  x' <- transformExp x
  y' <- transformExp y
  case typeOf x' of -- Both x and y have same type.
    Elem (Tuple ets) -> do
      xnames <- map fst <$> mapM (newVar loc "concat_tup_x") ets
      ynames <- map fst <$> mapM (newVar loc "concat_tup_y") ets
      let letx body = LetPat (TupId (map Id xnames) loc) x' body loc
          lety body = LetPat (TupId (map Id ynames) loc) y' body loc
          conc xarr yarr = transformExp $
            Concat (Var xarr) (Var yarr) loc
      concs <- zipWithM conc xnames ynames
      return $ letx $ lety $ TupLit concs loc
    _ -> return $ Concat x' y' loc

transformExp (LetPat pat e body loc) = do
  e' <- transformExp e
  withPattern pat $ \pat' -> do
    body' <- transformExp body
    return $ LetPat pat' e' body' loc

transformExp e@(Map lam arr tp pmap) =
  transformLambda lam $ \lam' ->
  tupToExpList arr $ \arrs ->
    untupleExp (typeOf e) $
    Map2 (tuplifyLam lam') arrs (untupleType $ transformType tp) pmap

transformExp (Reduce lam ne arr tp pos) =
  transformLambda lam $ \lam' ->
  tupToExpList arr $ \arrs ->
  tupToExpList ne $ \nes ->
    untupleDeclExp (lambdaReturnType lam) $
      Reduce2 (tuplifyLam lam') nes arrs (untupleType $ transformType tp) pos

transformExp e@(Scan lam ne arr tp pscan) =
  transformLambda lam $ \lam' ->
  tupToExpList arr $ \arrs ->
  tupToExpList ne $ \nes ->
    untupleExp (typeOf e) $
    Scan2 (tuplifyLam lam') nes arrs (untupleType $ transformType tp) pscan

transformExp e@(Filter lam arr _ loc) =
  transformLambda lam $ \lam' ->
  tupToExpList arr $ \arrs ->
    untupleExp (typeOf e) $ Filter2 lam' arrs loc

transformExp (Redomap lam1 lam2 ne arr tp1 pos) =
  transformLambda lam1 $ \lam1' ->
  transformLambda lam2 $ \lam2' ->
  tupToExpList arr $ \arrs ->
  tupToExpList ne $ \nes ->
    untupleDeclExp (lambdaReturnType lam1) $
      Redomap2 (tuplifyLam lam1') (tuplifyLam lam2')
               nes arrs (untupleType $ transformType tp1) pos

transformExp (Map2 lam arrs tps pmap) =
  transformLambda lam $ \lam' ->
  tupsToExpList arrs $ \arrs' ->
    return $ Map2 lam' arrs' (flattenTypes $ map transformType tps) pmap

transformExp (Reduce2 lam nes arrs tps pos) =
  transformLambda lam $ \lam' ->
  tupsToExpList arrs $ \arrs' ->
  tupsToExpList nes $ \nes' ->
    return $ Reduce2 lam' nes' arrs'
             (flattenTypes $ map transformType tps) pos

transformExp (Scan2 lam nes arrs tps loc) =
  transformLambda lam $ \lam' ->
  tupsToExpList arrs $ \arrs' ->
  tupsToExpList nes $ \nes' ->
    return $ Scan2 lam' nes' arrs'
             (flattenTypes $ map transformType tps) loc

transformExp (Filter2 lam arrs loc) =
  transformLambda lam $ \lam' ->
  tupsToExpList arrs $ \arrs' ->
    return $ Filter2 lam' arrs' loc

transformExp (Redomap2 lam1 lam2 nes arrs tps pos) =
  transformLambda lam1 $ \lam1' ->
  transformLambda lam2 $ \lam2' ->
  tupsToExpList arrs $ \arrs' ->
  tupsToExpList nes $ \nes' ->
    return $ Redomap2 lam1' lam2' nes' arrs' (map transformType tps) pos

transformExp e = mapExpM transform e
  where transform = Mapper {
                      mapOnExp = transformExp
                    , mapOnType = return . transformType
                    , mapOnLambda = return -- Not used.
                    , mapOnPattern = return -- Not used.
                    , mapOnIdent = return -- Not used.
                    , mapOnValue = return . transformValue
                    }

tupToIdentList :: Exp -> ([Ident] -> TransformM Exp)
                -> TransformM Exp
tupToIdentList e m = do
  e' <- transformExp e
  case typeOf e' of
    Elem (Tuple ts) -> do
      names <-
        mapM (liftM fst . newVar loc "tup_arr_elem") ts
      LetPat (TupId (map Id names) loc) e' <$> m names <*> pure loc
    t -> do
      name <- fst <$> newVar loc "single_arr_elem" t
      LetPat (Id name) e' <$> m [name] <*> pure loc
  where loc = srclocOf e

tupToExpList :: Exp -> ([Exp] -> TransformM Exp)
                -> TransformM Exp
tupToExpList e m = do
  e' <- transformExp e
  case typeOf e' of
    Elem (Tuple ts) -> do
      (names, namevs) <-
        unzip <$> mapM (newVar loc "tup_arr_elem") ts
      LetPat (TupId (map Id names) loc) e' <$> m namevs <*> pure loc
    _ -> m [e']
  where loc = srclocOf e

tupsToExpList :: [Exp] -> ([Exp] -> TransformM Exp) -> TransformM Exp
tupsToExpList = tupsToExpList' []
  where tupsToExpList' acc [] m = m acc
        tupsToExpList' acc (e:es) m =
          tupToExpList e $ \e' ->
            tupsToExpList' (acc++e') es m

transformLambda :: Lambda -> (Lambda -> TransformM Exp) -> TransformM Exp
transformLambda (AnonymFun params body rettype loc) m = do
  lam <- binding (map fromParam params) $ \params' -> do
           body' <- transformExp body
           return $ AnonymFun (map toParam params') body' rettype' loc
  m lam
  where rettype' = toDecl $ transformType rettype
transformLambda (CurryFun fname curryargs rettype loc) m =
  transform curryargs []
  where transform [] curryargs' =
          m $ CurryFun fname curryargs' (transformType rettype) loc
        transform (a:as) curryargs' =
          tupToExpList a $ \es ->
            transform as (curryargs' ++ es)

-- | Take a lambda returning @t@ and make it return @{t}@.  If the
-- lambda already returns a tuple, return it unchanged.
tuplifyLam :: Lambda -> Lambda
tuplifyLam (CurryFun {}) = error "no curries yet"
tuplifyLam (AnonymFun params body rettype loc) =
  AnonymFun params body' (tuplifyType rettype) loc
  where body' = case rettype of
                  Elem (Tuple _) -> body
                  _              -> mapTails tuplifyLam' body
        tuplifyLam' e = TupLit [e] loc

tuplifyType :: TypeBase als vn -> TypeBase als vn
tuplifyType (Elem (Tuple ets)) = Elem $ Tuple ets
tuplifyType t                  = Elem $ Tuple [t]

flattenPattern :: TupIdent -> [Ident]
flattenPattern (TupId pats _) = concatMap flattenPattern pats
flattenPattern (Id ident)     = [ident]

withPattern :: TupIdent -> (TupIdent -> TransformM a) -> TransformM a
withPattern pat m =
  binding (flattenPattern pat) $ \idents ->
    let tup = TupId (map Id idents) $ srclocOf pat
    in m $ case (idents, patType pat) of
             (_, Elem (Tuple _)) -> tup
             ([ident], _)        -> Id ident
             _                   -> tup -- Should never be reached.

newVar :: SrcLoc -> String -> Type -> TransformM (Ident, Exp)
newVar loc name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)

untupleDeclExp :: DeclType -> Exp -> TransformM Exp
untupleDeclExp = untupleExp . fromDecl

untupleExp :: Type -> Exp -> TransformM Exp
untupleExp t e = do
  let loc = srclocOf e
  case typeOf e of
    Elem (Tuple [et])
      | et `subtypeOf` t -> do
        (v, vn) <- newVar loc "untuple" et
        return $ LetPat (TupId [Id v] loc) e vn loc
    _ -> return e

untupleType :: Type -> [Type]
untupleType (Elem (Tuple ts)) = ts
untupleType t = [t]

tuplit :: [Exp] -> SrcLoc -> Exp
tuplit [e] _ = e
tuplit es loc = TupLit es loc
