module L0.TupleArrayTransform
  ( transformProg
  , transformType )
  where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Array as A
import Data.Data
import Data.Generics
import Data.List
import Data.Loc
import qualified Data.Map as M

import L0.AbSyn
import L0.FreshNames

transformProg :: Prog Type -> Prog Type
transformProg = map transformFun

data TransformState = TransformState {
    envNameSrc :: NameSource
  }

data TransformEnv = TransformEnv {
    envVarSubst :: M.Map String [Ident Type]
  }

type TransformM = ReaderT TransformEnv (State TransformState)

runTransformM :: TransformM a -> a
runTransformM m = evalState (runReaderT m newEnv) newState
  where newEnv = TransformEnv M.empty
        newState = TransformState $ newNameSource []

new :: String -> TransformM String
new k = do (name, src) <- gets $ newName k . envNameSrc
           modify $ \s -> s { envNameSrc = src }
           return name

substituting :: M.Map String [Ident Type] -> TransformM a -> TransformM a
substituting substs =
  local (\s -> s { envVarSubst = substs `M.union` envVarSubst s })

transformType :: Type -> Type
transformType (Array (Tuple elemts _) size loc) =
  Tuple (map (transformType . arraytype) elemts) loc
  where arraytype t = Array t size loc
transformType (Array elemt size loc) =
  case elemt' of
    Tuple elemts _ -> Tuple (map (transformType . arraytype) elemts) loc
    _ -> Array elemt' size loc
  where elemt' = transformType elemt
        arraytype t = transformType $ Array t size loc
transformType (Tuple elemts loc) = Tuple (map transformType elemts) loc
transformType t = t -- All other types are fine.

transformValue :: Value -> Value
transformValue (ArrayVal arr et loc) =
  case transformType et of
    Tuple ets _
      | [] <- A.elems arr -> TupVal [ arrayVal [] et' loc | et' <- ets ] loc
      | otherwise         ->  TupVal (zipWith asarray ets $ transpose arrayvalues) loc
    et'         -> ArrayVal arr et' loc
  where asarray t vs = transformValue $ arrayVal vs t loc
        arrayvalues = map (tupleValues . transformValue) $ A.elems arr
        tupleValues (TupVal vs _) = vs
        tupleValues _ = error "L0.TupleArrayTransform.transformValue: Element of tuple array is not tuple."
        -- Above should never happen in well-typed program.
transformValue (TupVal vs loc) = TupVal (map transformValue vs) loc
transformValue v = v

transformPat :: TupIdent Type -> TupIdent Type
transformPat (TupId pats loc) = TupId (map transformPat pats) loc
transformPat (Id k) = Id $ transformIdent k

transformIdent :: Ident Type -> Ident Type
transformIdent (Ident name t loc) = Ident name (transformType t) loc

transformFun :: FunDec Type -> FunDec Type
transformFun (fname,rettype,params,body,loc) = (fname, rettype', params', body', loc)
  where rettype' = transformType rettype
        params' = map transformIdent params
        body' = runTransformM $ transformExp body

transformExp :: Exp Type -> TransformM (Exp Type)
transformExp (Literal val) =
  return $ Literal $ transformValue val
transformExp (Var k) = do
  substs <- asks $ M.lookup (identName k) . envVarSubst
  case substs of
    Nothing     -> return $ Var $ transformIdent k
    Just subst' -> return $ TupLit (map Var subst') (srclocOf k)
transformExp (TupLit es loc) = do
  es' <- mapM transformExp es
  return $ TupLit es' loc
transformExp (ArrayLit [] intype loc) =
  return $ case transformType intype of
             Tuple ets _ ->
               TupLit [ ArrayLit [] et loc | et <- ets ] loc
             et' -> ArrayLit [] et' loc
transformExp (ArrayLit es intype loc) = do
  es' <- mapM transformExp es
  case transformType intype of
    Tuple ets _ -> do
      (e, bindings) <- foldM comb (id, replicate (length ets) []) es'
      e <$> tuparrexp (map reverse bindings) ets
        where comb (acce, bindings) e = do
                names <- map fst <$> mapM (newVar "array_tup") ets
                return (\body -> acce $ LetPat (TupId (map Id names) loc) e body loc
                       ,zipWith (:) names bindings)
              tuparrexp bindings ts = transformExp $ TupLit (zipWith arrexp ts bindings) loc
              arrexp t names = ArrayLit (map Var names) t loc
    et' -> return $ ArrayLit es' et' loc
transformExp (Index vname idxs intype outtype loc) = do
  idxs' <- mapM transformExp idxs
  case (identType vname', intype', outtype') of
    -- If the input type is a tuple, then the output type is
    -- necessarily also.
    (Tuple ets _, Tuple its _, Tuple ots _) -> do
      -- Create names for the elements of the tuple.
      names <- map fst <$> mapM (newVar "index_tup") ets
      let indexing = TupLit (zipWith (\name (it,ot) -> Index name idxs' it ot loc) names (zip its ots)) loc
      return $ LetPat (TupId (map Id names) loc) (Var vname') indexing loc
    _ -> return $ Index vname' idxs' intype' outtype' loc
  where intype' = transformType intype
        outtype' = transformType outtype
        vname' = transformIdent vname
transformExp (DoLoop i bound body mergevars loc) = do
  bound' <- transformExp bound
  (mergevars', substs, lets) <-
    foldM procmvar ([], M.empty, id) $ map transformIdent mergevars
  substituting substs $ do
    body' <- transformExp body
    return $ lets $ DoLoop i bound' body' mergevars' loc
  where procmvar (mvars, substs, inner) mvar@(Ident name (Tuple ets _) _) = do
          names <- map fst <$> mapM (newVar "mvar") ets
          return (mvars ++ names,
                  M.insert name names substs,
                  \inner' -> inner $ LetPat (TupId (map Id names) loc) (Var mvar) inner' loc)
        procmvar (mvars, substs, inner) mvar =
          return (mvars ++ [mvar], substs, inner)
transformExp (LetWith name srcexp idxs valexp body loc) = do
  srcexp' <- transformExp srcexp
  idxs' <- mapM transformExp idxs
  body' <- transformExp body
  valexp' <- transformExp valexp
  substs <- asks $ M.lookup (identName name') . envVarSubst
  case (substs, expType srcexp', expType valexp') of
    (Just substs', Tuple sts _, Tuple ets _) -> do
      (srcs, srclet) <-
        case srcexp of -- Intentional.
          Var k | k == name -> return (substs', id)
          _ -> do (srcs, _) <- unzip <$> mapM (newVar "letwith_src") sts
                  return (srcs, \inner -> LetPat (TupId (map Id srcs) loc) (Copy srcexp' loc) inner loc)
      (vals, valvs) <- unzip <$> mapM (newVar "letwith_val") ets
      let comb olde (dest, srcv, valv) inner =
            LetWith dest srcv idxs' valv (olde inner) loc
          vallet inner = LetPat (TupId (map Id vals) loc) valexp' inner loc
          letws = foldl comb id $ zip3 substs' (map Var srcs) valvs
      return $ srclet $ vallet $ letws body'
    _ -> return $ LetWith name' srcexp' idxs' valexp' body' loc
  where name' = transformIdent name
transformExp (Replicate ne ve loc) = do
  ne' <- transformExp ne
  ve' <- transformExp ve
  case expType ve' of
    Tuple ets _ -> do
      (n, nv) <- newVar "n" (Int loc)
      (names, vs) <- unzip <$> mapM (newVar "rep_tuple") ets
      let arrexp v = Replicate nv v loc
          nlet body = LetPat (Id n) ne' body loc
          tuplet body = LetPat (TupId (map Id names) loc) ve' body loc
      return $ nlet $ tuplet $ TupLit (map arrexp vs) loc
    _ -> return $ Replicate ne' ve' loc
transformExp (Size e loc) = do
  e' <- transformExp e
  case expType e' of
    Tuple (et:ets) _ -> do
      (name, namev) <- newVar "size_tup" et
      names <- map fst <$> mapM (newVar "size_tup") ets
      return $ LetPat (TupId (map Id $ name : names) loc) e' (Size namev loc) loc
    _ -> return $ Size e' loc
transformExp (Unzip e _ _) = transformExp e
transformExp (Zip ((e,t):es) loc) = do
  -- Zip is not quite an identity, as we need to truncate the arrays
  -- to the length of the shortest one.
  e' <- transformExp e
  es' <- mapM (transformExp . fst) es
  let ets = map transformType $ t : map snd es
  (name, namev) <- newVar "zip_array" (expType e')
  names <- map fst <$> mapM (newVar "zip_array" . expType) es'
  (size, sizev) <- newVar "zip_size" $ Int loc
  let combes olde (arre,vname) inner = LetPat (Id vname) arre (olde inner) loc
      lete = foldl combes id $ reverse $ zip (e':es') (name:names)
      test vname = BinOp Less sizev (Size (Var vname) loc) (Bool loc) loc
      branch vname = If (test vname) sizev (Size (Var vname) loc) (Int loc) loc
      combsiz olde vname inner = olde $ LetPat (Id size) (branch vname) inner loc
      letsizs = foldl combsiz (\inner -> LetPat (Id size) (Size namev loc) inner loc) names
      split et vname = do
        (a, av) <- newVar "zip_a" $ identType vname
        (b, _) <- newVar "zip_b" $ identType vname
        return $ LetPat (TupId [Id a, Id b] loc) (Split sizev (Var vname) et loc) av loc
  lete . letsizs . (`TupLit` loc) <$> zipWithM split ets (name:names)
transformExp (Split nexp arrexp eltype loc) = do
  nexp' <- transformExp nexp
  arrexp' <- transformExp arrexp
  case expType arrexp' of
    Tuple ets _ -> do
      (n, nv) <- newVar "split_n" $ expType nexp'
      names <- map fst <$> mapM (newVar "split_tup") ets
      partnames <- forM ets $ \et -> do
                     a <- fst <$> newVar "split_a" et
                     b <- fst <$> newVar "split_b" et
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
transformExp (Concat x y eltype loc) = do
  x' <- transformExp x
  y' <- transformExp y
  case transformType eltype of -- Both x and y have same type.
    Tuple ets _ -> do
      xnames <- map fst <$> mapM (newVar "concat_tup_x") ets
      ynames <- map fst <$> mapM (newVar "concat_tup_y") ets
      let letx body = LetPat (TupId (map Id xnames) loc) x' body loc
          lety body = LetPat (TupId (map Id ynames) loc) y' body loc
          conc et (xarr, yarr) = Concat (Var xarr) (Var yarr) et loc
      return $ letx $ lety $ TupLit (zipWith conc ets $ zip xnames ynames) loc
    eltype' -> return $ Concat x' y' eltype' loc
transformExp e = gmapM (mkM transformExp
                       `extM` (return . transformType)
                       `extM` mapM transformExp
                       `extM` transformLambda
                       `extM` (return . transformPat)
                       `extM` mapM transformExpPair) e

transformLambda :: Lambda Type -> TransformM (Lambda Type)
transformLambda (AnonymFun params body rettype loc) = do
  body' <- transformExp body
  return $ AnonymFun params' body' rettype' loc
  where rettype' = transformType rettype
        params' = map transformIdent params
transformLambda (CurryFun fname curryargs rettype loc) = do
  curryargs' <- mapM transformExp curryargs
  return $ CurryFun fname curryargs' (transformType rettype) loc

transformExpPair :: (Exp Type, Type) -> TransformM (Exp Type, Type)
transformExpPair (e,t) = do e' <- transformExp e
                            return (e',transformType t)

newVar :: String -> Type -> TransformM (Ident Type, Exp Type)
newVar name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)
  where loc = srclocOf tp
