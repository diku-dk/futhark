module L0.TupleArrayTransform
  ( transformProg
  , transformType )
  where

import Control.Applicative
import Control.Monad.State

import qualified Data.Array as A
import Data.List
import Data.Loc

import L0.AbSyn
import L0.Traversals
import L0.FreshNames

transformProg :: Prog Type -> Prog Type
transformProg = map transformFun

data TransformState = TransformState {
    envNameSrc :: NameSource
  }

type TransformM = State TransformState

runTransformM :: TransformM a -> a
runTransformM m = evalState m newState
  where newState = TransformState blankNameSource

new :: String -> TransformM Name
new k = do (name, src) <- gets $ newName (nameFromString k) . envNameSrc
           modify $ \s -> s { envNameSrc = src }
           return name

transformElemType :: ElemType -> ElemType
transformElemType (Tuple elemts) = Tuple (map transformType elemts)
transformElemType t = t

transformType :: Type -> Type
transformType (Array (Tuple elemts) size u) =
  Elem $ Tuple (map (transformType . arr) elemts)
  where arr t = arrayOf t size u
transformType (Array elemt size u) =
  case transformElemType elemt of
    Tuple elemts -> Elem $ Tuple (map (transformType . arr) elemts)
    elemt' -> Array elemt' size u
  where arr t = arrayOf t size u
transformType (Elem et) = Elem $ transformElemType et

transformValue :: Value -> Value
transformValue (ArrayVal arr rt) =
  case transformType rt of
    Elem (Tuple ts)
      | [] <- A.elems arr -> TupVal $ map emptyOf ts
      | otherwise         -> TupVal (zipWith asarray ts $ transpose arrayvalues)
    rt' -> ArrayVal arr rt'
  where emptyOf t = blankValue $ arrayType 1 t Nonunique
        asarray t vs = transformValue $ arrayVal vs t
        arrayvalues = map (tupleValues . transformValue) $ A.elems arr
        tupleValues (TupVal vs) = vs
        tupleValues _ = error "L0.TupleArrayTransform.transformValue: Element of tuple array is not tuple."
        -- Above should never happen in well-typed program.
transformValue (TupVal vs) = TupVal (map transformValue vs)
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
transformExp (TupLit es loc) = do
  es' <- mapM transformExp es
  return $ TupLit es' loc
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
                names <- map fst <$> mapM (newVar loc "array_tup") ets
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
    (Elem (Tuple ets), Elem (Tuple its), Elem (Tuple ots)) -> do
      -- Create names for the elements of the tuple.
      names <- map fst <$> mapM (newVar loc "index_tup") ets
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
    (Elem (Tuple ets), Elem (Tuple xts)) -> do
      snames <- map fst <$> mapM (newVar loc "letwith_src") ets
      vnames <- map fst <$> mapM (newVar loc "letwith_el") xts
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
transformExp (Zip es loc) =
  TupLit <$> mapM (transformExp . fst) es <*> pure loc
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
transformExp e = mapExpM transform e
  where transform = Mapper {
                      mapOnExp = transformExp
                    , mapOnType = return . transformType
                    , mapOnLambda = transformLambda
                    , mapOnPattern = return . transformPat
                    , mapOnIdent = return . transformIdent
                    , mapOnValue = return . transformValue
                    }

transformLambda :: Lambda Type -> TransformM (Lambda Type)
transformLambda (AnonymFun params body rettype loc) = do
  body' <- transformExp body
  return $ AnonymFun params' body' rettype' loc
  where rettype' = transformType rettype
        params' = map transformIdent params
transformLambda (CurryFun fname curryargs rettype loc) = do
  curryargs' <- mapM transformExp curryargs
  return $ CurryFun fname curryargs' (transformType rettype) loc

newVar :: SrcLoc -> String -> Type -> TransformM (Ident Type, Exp Type)
newVar loc name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)
