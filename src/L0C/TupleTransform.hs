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

flattenTypes :: [Type] -> [Type]
flattenTypes = concatMap flatten
  where flatten (Elem (Tuple ts)) = ts
        flatten t                 = [t]

transformElemType :: ElemType -> ElemType
transformElemType (Tuple elemts) =
  Tuple $ flattenTypes $ map transformType elemts
transformElemType t = t

transformType :: Type -> Type
transformType (Array (Tuple elemts) size u) =
  Elem $ Tuple $ flattenTypes (map (transformType . arr) elemts)
  where arr t = arrayOf t size u
transformType (Array elemt size u) =
  case transformElemType elemt of
    Tuple elemts ->
      Elem $ Tuple $ flattenTypes (map (transformType . arr) elemts)
    elemt' -> Array elemt' size u
  where arr t = arrayOf t size u
transformType (Elem et) = Elem $ transformElemType et

flattenValues :: [Value] -> [Value]
flattenValues = concatMap flatten
  where flatten (TupVal vs) = vs
        flatten v           = [v]

transformValue :: Value -> Value
transformValue (ArrayVal arr rt) =
  case transformType rt of
    Elem (Tuple ts)
      | [] <- A.elems arr ->
        TupVal $ flattenValues $ map emptyOf ts
      | otherwise         ->
        TupVal $ flattenValues $ zipWith asarray ts $ transpose arrayvalues
    rt' -> ArrayVal arr rt'
  where emptyOf t = blankValue $ arrayType 1 t Nonunique
        asarray t vs = transformValue $ arrayVal vs t
        arrayvalues = map (tupleValues . transformValue) $ A.elems arr
        tupleValues (TupVal vs) = vs
        tupleValues _ = error "L0.TupleArrayTransform.transformValue: Element of tuple array is not tuple."
        -- Above should never happen in well-typed program.
transformValue (TupVal vs) = TupVal $ flattenValues $ map transformValue vs
transformValue v = v

transformIdent :: Ident -> Ident
transformIdent (Ident name t loc) = Ident name (transformType t) loc

transformFun :: FunDec -> TransformM FunDec
transformFun (fname,rettype,params,body,loc) =
  binding params $ \params' -> do
    body' <- transformExp body
    return (fname, rettype', params', body', loc)
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
      case typeOf param of
        Elem (Tuple ts) -> do
          -- We know from transformIdent that none of the element
          -- types are themselves tuples.
          names <- mapM (liftM fst . lift . newVar (srclocOf param) "tuple_param") ts
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
    Nothing -> return $ Var $ transformIdent var
    Just vs -> return $ TupLit (map Var vs) $ srclocOf var

transformExp (Index vname idxs outtype loc) = do
  idxs' <- mapM transformExp idxs
  subst <- asks $ M.lookup (identName vname) . envSubsts
  case subst of
    Just vs -> do
      let index v = Index v idxs'
                    (stripArray (length idxs') $ typeOf v) loc
      return $ TupLit (map index vs) loc
    _ -> return $ Index vname' idxs' outtype' loc
  where outtype' = transformType outtype
        vname' = transformIdent vname

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
             et' -> ArrayLit [] et' loc

transformExp (ArrayLit es intype loc) = do
  es' <- mapM transformExp es
  case transformType intype of
    Elem (Tuple ets) -> do
      (e, bindings) <- foldM comb (id, replicate (length ets) []) es'
      e <$> tuparrexp (map reverse bindings) ets
        where comb (acce, bindings) e = do
                names <- mapM (liftM fst . newVar loc "array_tup") ets
                return (\body -> acce $ LetPat (TupId (map Id names) loc) e body loc
                       ,zipWith (:) names bindings)
              tuparrexp bindings ts = transformExp $ TupLit (zipWith arrexp ts bindings) loc
              arrexp t names = ArrayLit (map Var names) t loc
    et' -> return $ ArrayLit es' et' loc

transformExp (Apply fname argexps rettype loc) = do
  (argexps', bindings) <- foldM comb ([], id) argexps
  return $ bindings $ Apply fname (reverse argexps') rettype' loc
    where rettype' = transformType rettype
          comb (argexps', bindings) arg = do
            arg' <- transformExp arg
            case typeOf arg' of
              Elem (Tuple ets) -> do
                (names, namevs) <- unzip <$> mapM (newVar loc "array_tup") ets
                return (reverse namevs ++ argexps',
                        \body ->
                          bindings $
                          LetPat (TupId (map Id names) loc) arg' body loc)
              _ -> return (arg' : argexps', bindings)

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
  case (identType name', typeOf ve') of
    (Elem (Tuple ets), Elem (Tuple xts)) -> do
      snames <- map fst <$> mapM (newVar loc "letwith_src") ets
      vnames <- map fst <$> mapM (newVar loc "letwith_el") xts
      let xlet inner = LetPat (TupId (map Id snames) loc) (Var src') inner loc
          vlet inner = LetPat (TupId (map Id vnames) loc) ve' inner loc
          comb olde (sname, vname) inner = LetWith sname sname idxs' (Var vname) (olde inner) loc
      let lws = foldl comb id $ zip snames vnames
      transformExp $ xlet $ vlet $ lws $ LetPat (Id name') (TupLit (map Var snames) loc) body loc
    _ -> do
      body' <- transformExp body
      return $ LetWith name' src idxs' ve' body' loc
  where name' = transformIdent name
        src'  = transformIdent src

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
      arrexps' <- mapM (transformExp . arrexp) vs
      return $ nlet $ tuplet $ TupLit arrexps' loc
    _ -> return $ Replicate ne' ve' loc

transformExp (Size e loc) = do
  e' <- transformExp e
  case typeOf e' of
    Elem (Tuple (et:ets)) -> do
      (name, namev) <- newVar loc "size_tup" et
      names <- map fst <$> mapM (newVar loc "size_tup") ets
      size <- transformExp $ Size namev loc
      return $ LetPat (TupId (map Id $ name : names) loc) e' size loc
    _ -> return $ Size e' loc

transformExp (Unzip e _ _) = transformExp e

transformExp (Zip es loc) = do
  es' <- mapM (transformExp . fst) es
  (names, namevs) <- unzip <$> mapM (newVar loc "zip_elem" . typeOf) es'
  let binder body = LetPat (TupId (map Id names) loc) (TupLit es' loc) body loc
      azip = Apply (nameFromString "assertZip") namevs (Elem Bool) loc
  (dead, _) <- newVar loc "dead" (Elem Bool)
  transformExp $ binder $ LetPat (Id dead) azip (TupLit namevs loc) loc

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
          letsplits = foldl combsplit id $ zip3 names partnames ets
          res = TupLit [TupLit (map (Var . fst) partnames) loc,
                        TupLit (map (Var . snd) partnames) loc] loc
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

transformExp (Map lam arr tp pmap) = do
  lam' <- transformLambda lam
  arr' <- transformExp arr
  tupArrToListArr arr' $ \arrs ->
    return $ Map2 lam' arrs (transformType tp) pmap

transformExp (Reduce lam ne arr tp pos) = do
  lam' <- transformLambda lam
  arr' <- transformExp    arr
  ne'  <- transformExp    ne
  tupArrToListArr arr' $ \arrs ->
    return $ Reduce2 lam' ne' arrs (transformType tp) pos

transformExp (Scan lam ne arr tp pscan) = do
  lam' <- transformLambda lam
  arr' <- transformExp    arr
  ne'  <- transformExp    ne
  tupArrToListArr arr' $ \arrs ->
    return $ Scan2 lam' ne' arrs (transformType tp) pscan

transformExp (Filter lam arr _ loc) = do
  lam' <- transformLambda lam
  arr' <- transformExp    arr
  tupArrToListArr arr' $ \arrs ->
    return $ Filter2 lam' arrs loc

transformExp (Mapall lam arr pmap) = do
  lam' <- transformLambda lam
  arr' <- transformExp    arr
  tupArrToListArr arr' $ \arrs ->
    return $ Mapall2 lam' arrs pmap

transformExp (Redomap lam1 lam2 ne arr tp1 pos) = do
  lam1' <- transformLambda lam1
  lam2' <- transformLambda lam2
  arr'  <- transformExp    arr
  ne'   <- transformExp    ne
  tupArrToListArr arr' $ \arrs ->
    return $ Redomap2 lam1' lam2' ne' arrs (transformType tp1) pos

transformExp e = mapExpM transform e
  where transform = Mapper {
                      mapOnExp = transformExp
                    , mapOnType = return . transformType
                    , mapOnLambda = transformLambda
                    , mapOnPattern = return -- Not used.
                    , mapOnIdent = return . transformIdent
                    , mapOnValue = return . transformValue
                    }

tupArrToListArr :: Exp -> ([Exp] -> TransformM Exp)
                -> TransformM Exp
tupArrToListArr e m = do
  e' <- transformExp e
  case typeOf e of
    Elem (Tuple ts) -> do
      (names, namevs) <-
        unzip <$> mapM (newVar loc "tup_arr_elem") ts
      LetPat (TupId (map Id names) loc) e' <$> m namevs <*> pure loc
    _ -> m [e']
  where loc = srclocOf e

transformLambda :: Lambda -> TransformM Lambda
transformLambda (AnonymFun params body rettype loc) =
  binding params $ \params' -> do
    body' <- transformExp body
    return $ AnonymFun params' body' rettype' loc
  where rettype' = transformType rettype
transformLambda (CurryFun fname curryargs rettype loc) = do
  curryargs' <- mapM transformExp curryargs
  return $ CurryFun fname curryargs' (transformType rettype) loc

flattenPattern :: TupIdent -> [Ident]
flattenPattern (TupId pats _) = concatMap flattenPattern pats
flattenPattern (Id ident)     = [ident]

withPattern :: TupIdent -> (TupIdent -> TransformM a) -> TransformM a
withPattern pat m =
  binding (flattenPattern pat) $ \idents ->
    let pat' = case idents of
                 [ident] -> Id ident
                 _       -> TupId (map Id idents) $ srclocOf pat
    in m pat'

newVar :: SrcLoc -> String -> Type -> TransformM (Ident, Exp)
newVar loc name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)
