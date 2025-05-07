-- | Finds instances of irregular nested data-parallelism (hopefully)
module Language.Futhark.Deps
  ( deps
  )
where

import Data.Map qualified as M
import Data.Set qualified as S
import Control.Monad
import Control.Monad.State
import Data.List (find)
import Data.Map qualified as M
import Futhark.Util.Loc (Loc (..), Pos (..))
import Language.Futhark
-- import Language.Futhark.Semantic
import Language.Futhark.Core
import Language.Futhark.Traversals
import Language.Futhark.FreeVars
import System.FilePath.Posix qualified as Posix

type Error = String

type Id = VName

newtype Ids = Ids [Id]
  deriving (Eq, Show)
  
type Deps = Ids


data DepVal
  = DepVal Deps
  | DepTuple [DepVal]
  | DepFun DepsEnv VName Exp
  deriving (Eq, Show)
  -- Evt. DepTop

-- type Env a = M.Map VName a



type DepsEnv = M.Map VName DepVal

depsEnvEmpty :: M.Map VName DepVal
depsEnvEmpty = M.singleton (VName "a" 0) (DepVal mempty)

envExtend :: VName -> DepVal -> DepsEnv -> DepsEnv
envExtend key val env = M.insert key val env 
-- envExtend v val env = (v, val) : filter ((/=v).fst) env

envLookup :: VName -> DepsEnv -> DepVal
envLookup key env =
  case M.lookup key env of
    Just x -> x
    Nothing -> DepVal mempty

newtype EvalM a = EvalM (DepsEnv -> Either Error a)

instance Functor EvalM where
  fmap = liftM

instance Applicative EvalM where
  pure x = EvalM $ \_env -> Right x
  (<*>) = ap

instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env ->
    case x env of
      Left err -> Left err
      Right x' ->
        let EvalM y = f x'
         in y env

askEnv :: EvalM DepsEnv
askEnv = EvalM $ \env -> Right env

localEnv :: (DepsEnv -> DepsEnv) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env -> m (f env)

failure :: String -> EvalM a
failure s = EvalM $ \_env -> Left s

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y  = x : merge xs (y:ys)
  | x > y  = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

instance Semigroup Ids where
  Ids x <> Ids y = Ids $ merge x y

instance Monoid Ids where
  mempty = Ids mempty

idsSingle :: VName -> Ids
idsSingle v = Ids [v]

idsWithout :: Ids -> VName -> Ids
idsWithout (Ids xs) x = Ids $ filter (/=x) xs

unIds :: Ids -> [VName]
unIds (Ids xs) = xs

-- type DepsBase a = a Info VName



depValDeps :: DepVal -> Deps
depValDeps (DepVal x) = x
depValDeps (DepTuple x) = foldMap depValDeps x
depValDeps (DepFun _ p body) = mempty -- OBS
-- depValDeps (DepFun _ p body) = freeIds (Lambda p body)

depValJoin :: DepVal -> DepVal -> DepVal
depValJoin (DepTuple xs) (DepTuple ys)
  | length xs == length ys = DepTuple $ zipWith depValJoin xs ys
depValJoin x y = DepVal $ depValDeps x <> depValDeps y

depValInj :: Deps -> DepVal -> DepVal
depValInj x (DepVal y) = DepVal $ x <> y
depValInj x (DepTuple ys) = DepTuple $ map (depValInj x) ys
depValInj x v = DepVal $ x <> depValDeps v

----

deps :: Prog -> String
deps prog = show $ (depsFreeVars prog)
  -- case runDeps depsEnvEmpty (depsProgBase prog) of
  --   Left a -> a
  --   Right a -> show a 
-- (env `envIntersect` freeVNames e)
runDeps :: DepsEnv -> EvalM a -> Either Error a
runDeps env (EvalM m) = m env

depsFreeVars :: ProgBase Info VName -> DepsEnv
depsFreeVars base =
  case last $ progDecs base of -- Last progDec is main
    ValDec valbind -> M.fromList $ map (\x -> (x, DepVal mempty)) (S.toList $ fvVars $ freeInExp $ valBindBody valbind)
    _ -> depsEnvEmpty -- OBS

depsProgBase :: ProgBase Info VName -> EvalM DepVal 
depsProgBase _ = pure $ DepVal mempty
depsProgBase base = depsDecBase $ last $ progDecs base -- obs

depsDecBase :: DecBase Info VName-> EvalM DepVal
depsDecBase (ValDec valbind) = depsExpBase $ valBindBody valbind
depsDecBase _ = pure $ DepVal mempty

depsExpBase :: ExpBase Info VName -> EvalM DepVal
depsExpBase (Literal _ _) = pure $ DepVal mempty
depsExpBase (IntLit _ _ _) = pure $ DepVal mempty
depsExpBase (FloatLit _ _ _) = pure $ DepVal mempty
depsExpBase (StringLit _ _) = pure $ DepVal mempty
depsExpBase (Hole _ _) = pure $ DepVal mempty
depsExpBase (Var qn _ _) = do
  env <- askEnv
  case envLookup (qualLeaf qn) env of
    DepVal mempty -> failure $ "Unknown variable" <> (show $ qualLeaf qn) --
    a -> pure a 
depsExpBase (Parens eb _) = depsExpBase eb
depsExpBase (QualParens qn eb _) = depsExpBase eb -- OBS
depsExpBase (TupLit eb sl) =
  case eb of
  [] -> pure $ DepVal mempty
  (h:t) -> do
    v1 <- depsExpBase $ h
    v2 <- depsExpBase $ TupLit t sl
    case v2 of
      DepVal mempty -> pure $ DepTuple [v1]
      DepTuple tpl -> pure $ DepTuple (v1 : tpl)
      _ -> failure "Tuple type malformed" -- Should not be possible
depsExpBase (RecordLit fb sl) = pure $ DepVal mempty -- OBS
depsExpBase (AppExp base _) = depsAppExpBase base

depsFieldBase :: FieldBase Info VName -> EvalM DepVal
depsFieldBase (RecordFieldExplicit _ _ _) = pure $ DepVal mempty -- 
depsFieldBase (RecordFieldImplicit _ _ _) = pure $ DepVal mempty -- 

depsAppExpBase :: AppExpBase Info VName -> EvalM DepVal
depsAppExpBase (Apply eb1 ne _) = do
  let eb2 = ne -- OBS
    in pure $ DepVal mempty --
depsAppExpBase (Range eb1 eb2 _ _) = pure $ DepVal mempty --
depsAppExpBase (LetPat _ pb eb1 eb2 _) = do
  d1 <- depsPatBase pb
  d2 <- depsExpBase eb1
  d3 <- depsExpBase eb2
  let deps_let = depValDeps d2 `depValInj` d1
    in pure $ depValDeps deps_let `depValInj` d3 
depsAppExpBase (LetFun vn _ _ _) = pure $ DepVal mempty --
depsAppExpBase (If eb1 eb2 eb3 _) = do
  d1 <- depsExpBase eb1
  d2 <- depsExpBase eb2
  d3 <- depsExpBase eb3
  pure $ depValDeps d1 `depValInj` (d2 `depValJoin` d3)
depsAppExpBase (Loop _ _ _ _ _ _) = pure $ DepVal mempty --
depsAppExpBase (BinOp _ _ eb1 eb2 _) = do
  d1 <- depsExpBase $ fst eb1
  d2 <- depsExpBase $ fst eb2
  pure $ d1 `depValJoin` d2
depsAppExpBase (LetWith _ _ _ _ _ _) = pure $ DepVal mempty -- 
depsAppExpBase (Index _ _ _) = pure $ DepVal mempty --
depsAppExpBase (Match _ _ _) = pure $ DepVal mempty --

depsPatBase :: PatBase Info VName t -> EvalM DepVal
depsPatBase (TuplePat pb sl) =
  case pb of
  [] -> pure $ DepVal mempty
  (h:t) -> do
    v1 <- depsPatBase $ h
    v2 <- depsPatBase $ TuplePat t sl
    case v2 of
      DepVal mempty -> pure $ DepTuple [v1]
      DepTuple tpl -> pure $ DepTuple (v1 : tpl)
      _ -> failure "Tuple type malformed" -- Should not be possible
-- depsPatBase (RecordPat lnpb _) =
--   case lnpb of
--   [] -> pure $ DepVal mempty
--   (h:t) -> do
--     v1 <- depsPatBase $ h
--     v2 <- depsPatBase $ TuplePat t sl
--     case v2 of
--       DepVal mempty -> pure $ DepTuple [v1]
--       DepTuple tpl -> pure $ DepTuple (v1 : tpl)
--       _ -> failure "Tuple type malformed" -- Should not be possible
depsPatBase (Id vn _ _) = pure $ DepVal $ Ids [vn]
depsPatBase _ = pure $ DepVal mempty --
-- depsPatBase (RecordPat)
-- depsPatBase (PatParens)
-- depsPatBase (Wildcard)
-- depsPatBase (PatAscription)
-- depsPatBase (PatLit)
-- depsPatBase (PatConstr)
-- depsPatBase (PatAttr)


printDeps :: String -> String
printDeps file = "Hey, you!"

