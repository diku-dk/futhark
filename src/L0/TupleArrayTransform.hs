module L0.TupleArrayTransform
  ( transformProg
  , transformType )
  where

import Control.Applicative
import Control.Monad.State

import qualified Data.Array as A
import Data.Data hiding (typeOf)
import Data.Generics hiding (typeOf)
import Data.List
import Data.Loc

import L0.AbSyn
import L0.FreshNames

transformProg :: Prog Type -> Prog Type
transformProg = map transformFun

data TransformState = TransformState {
    envNameSrc :: NameSource
  }

type TransformM = State TransformState

runTransformM :: TransformM a -> a
runTransformM m = evalState m newState
  where newState = TransformState $ newNameSource []

new :: String -> TransformM String
new k = do (name, src) <- gets $ newName k . envNameSrc
           modify $ \s -> s { envNameSrc = src }
           return name

transformType :: Type -> Type
transformType (Array (Tuple elemts _ _) size u loc) =
  Tuple (map (transformType . arraytype) elemts) u loc
  where arraytype t = Array t size u loc
transformType (Array elemt size u loc) =
  case elemt' of
    Tuple elemts _ _ -> Tuple (map (transformType . arraytype) elemts) u loc
    _ -> Array elemt' size u loc
  where elemt' = transformType elemt
        arraytype t = transformType $ Array t size Nonunique loc
transformType (Tuple elemts u loc) = Tuple (map transformType elemts) u loc
transformType t = t -- All other types are fine.

transformValue :: Value -> Value
transformValue (ArrayVal arr et loc) =
  case transformType et of
    Tuple ets _ _
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
transformExp (Var k) =
  return $ Var $ transformIdent k
transformExp (TupLit es loc) = do
  es' <- mapM transformExp es
  return $ TupLit es' loc
transformExp (ArrayLit [] intype loc) =
  return $ case transformType intype of
             Tuple ets _ _ ->
               TupLit [ ArrayLit [] et loc | et <- ets ] loc
             et' -> ArrayLit [] et' loc
transformExp (ArrayLit es intype loc) = do
  es' <- mapM transformExp es
  case transformType intype of
    Tuple ets _ _ -> do
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
    (Tuple ets _ _, Tuple its _ _, Tuple ots _ _) -> do
      -- Create names for the elements of the tuple.
      names <- map fst <$> mapM (newVar "index_tup") ets
      indexes' <- forM (zip3 names its ots) $ \(name, it, ot) ->
                    transformExp $ Index name idxs' it ot loc
      let indexing = TupLit indexes' loc
      return $ LetPat (TupId (map Id names) loc) (Var vname') indexing loc
    _ -> return $ Index vname' idxs' intype' outtype' loc
  where intype' = transformType intype
        outtype' = transformType outtype
        vname' = transformIdent vname
transformExp (DoLoop mergepat mergeexp i bound loopbody letbody loc) = do
  bound' <- transformExp bound
  let mergepat' = transformPat mergepat
  mergeexp' <- transformExp mergeexp
  loopbody' <- transformExp loopbody
  letbody' <- transformExp letbody
  return $ DoLoop mergepat' mergeexp' i bound' loopbody' letbody' loc
transformExp (LetWith name src idxs ve body loc) = do
  idxs' <- mapM transformExp idxs
  body' <- transformExp body
  ve' <- transformExp ve
  case (identType name', typeOf ve') of
    (Tuple ets _ _, Tuple xts _ _) -> do
      snames <- map fst <$> mapM (newVar "letwith_src") ets
      vnames <- map fst <$> mapM (newVar "letwith_el") xts
      let xlet inner = LetPat (TupId (map Id snames) loc) (Var src') inner loc
          vlet inner = LetPat (TupId (map Id vnames) loc) ve' inner loc
          comb olde (sname, vname) inner = LetWith sname sname idxs' (Var vname) (olde inner) loc
      let lws = foldl comb id $ zip snames vnames
      transformExp $ xlet $ vlet $ lws $ LetPat (Id name') (TupLit (map Var snames) loc) body' loc
    _ -> return $ LetWith name' src idxs' ve' body' loc
  where name' = transformIdent name
        src'  = transformIdent src
transformExp (Replicate ne ve loc) = do
  ne' <- transformExp ne
  ve' <- transformExp ve
  case typeOf ve' of
    Tuple ets _ _ -> do
      (n, nv) <- newVar "n" (Int loc)
      (names, vs) <- unzip <$> mapM (newVar "rep_tuple") ets
      let arrexp v = Replicate nv v loc
          nlet body = LetPat (Id n) ne' body loc
          tuplet body = LetPat (TupId (map Id names) loc) ve' body loc
      arrexps' <- mapM (transformExp . arrexp) vs
      return $ nlet $ tuplet $ TupLit arrexps' loc
    _ -> return $ Replicate ne' ve' loc
transformExp (Size e loc) = do
  e' <- transformExp e
  case typeOf e' of
    Tuple (et:ets) _ _ -> do
      (name, namev) <- newVar "size_tup" et
      names <- map fst <$> mapM (newVar "size_tup") ets
      return $ LetPat (TupId (map Id $ name : names) loc) e' (Size namev loc) loc
    _ -> return $ Size e' loc
transformExp (Unzip e _ _) = transformExp e
transformExp (Zip es loc) =
  TupLit <$> mapM (transformExp . fst) es <*> pure loc
transformExp (Split nexp arrexp eltype loc) = do
  nexp' <- transformExp nexp
  arrexp' <- transformExp arrexp
  case typeOf arrexp' of
    Tuple ets _ _ -> do
      (n, nv) <- newVar "split_n" $ typeOf nexp'
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
    Tuple ets _ _ -> do
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
