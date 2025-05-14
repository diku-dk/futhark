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
import Data.List.NonEmpty qualified as NE

type Error = String

type Id = VName

newtype Ids = Ids [Id]
  deriving (Eq, Show)
  
type Deps = Ids


data DepVal
  = DepVal Deps
  | DepTuple [DepVal]
  | DepFun DepsEnv [VName] (ExpBase Info VName )
  deriving (Eq, Show)
  -- Evt. DepTop

data NestedVName
  = Name VName
  | Nested [NestedVName]
  | WildcardName
-- type Env a = M.Map VName a



type DepsEnv = M.Map VName DepVal

depsEnvEmpty :: M.Map VName DepVal
depsEnvEmpty = M.empty

envExtend :: VName -> DepVal -> DepsEnv -> DepsEnv
envExtend key val env = M.insert key val env 
-- envExtend v val env = (v, val) : filter ((/=v).fst) env

envLookup :: VName -> DepsEnv -> EvalM DepVal
envLookup key env = do
  case M.lookup key env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " <> (show key) 

envUnion :: DepsEnv -> DepsEnv -> DepsEnv
envUnion = M.union

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

-- idsSingle :: VName -> Ids
-- idsSingle v = Ids [v]

-- idsWithout :: Ids -> VName -> Ids
-- idsWithout (Ids xs) x = Ids $ filter (/=x) xs

-- unIds :: Ids -> [VName]
-- unIds (Ids xs) = xs

-- type DepsBase a = a Info VName



depValDeps :: DepVal -> Deps
depValDeps (DepVal x) = x
depValDeps (DepTuple x) = foldMap depValDeps x
depValDeps (DepFun _ p body) = Ids $ freeVarsList body  -- OBS
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
deps prog =
  case runDeps (depsFreeVarsInProgBase prog) (depsProgBase prog) of
    Left a -> "ERROR: " ++ a 
    Right a -> show a

runDeps :: DepsEnv -> EvalM a -> Either Error a
runDeps env (EvalM m) = m env

depsFreeVarsInProgBase :: ProgBase Info VName -> DepsEnv
depsFreeVarsInProgBase base =
  case last $ progDecs base of -- Last progDec is main
    ValDec valbind -> depsFreeVarsInExpBase $ valBindBody valbind
    _ -> depsEnvEmpty -- OBS

depsFreeVarsInExpBase :: ExpBase Info VName -> DepsEnv
depsFreeVarsInExpBase eb = M.fromList $ map (\x -> (x, DepVal mempty)) $ freeVarsList eb

freeVarsList :: ExpBase Info VName -> [Id]
freeVarsList eb = S.toList $ fvVars $ freeInExp eb

-- Converts pattern bases to pure NestedVNames
stripPatBase :: PatBase Info VName t -> NestedVName
stripPatBase (TuplePat pb_n _) = Nested $ map stripPatBase pb_n
-- stripPatBase (RecordPat (pb_n) _) = Nested $ map stripPatBase pb_n
stripPatBase (PatParens pb _) = stripPatBase pb
stripPatBase (Id vn _ _) = Name vn
stripPatBase (Wildcard _ _) = WildcardName
stripPatBase (PatAscription pb te _) = stripPatBase pb -- forkast te ???? kommer i "arg : i32" exps og ikke "arg" ? 
stripPatBase (PatLit _ _ _) = WildcardName
-- stripPatBase (PatConstr)
-- stripPatBase (PatAttr)

nestedNamesToSelfEnv :: NestedVName -> DepsEnv
nestedNamesToSelfEnv (Name vn) = M.singleton vn (DepVal $ Ids [vn])
nestedNamesToSelfEnv (Nested nvn) = foldr M.union depsEnvEmpty (map nestedNamesToSelfEnv nvn) -- OBS
nestedNamesToSelfEnv WildcardName = depsEnvEmpty


depsProgBase :: ProgBase Info VName -> EvalM DepVal 
depsProgBase base = depsDecBase $ last $ progDecs base -- obs
depsProgBase _ = failure "Unrecognized program base"

depsDecBase :: DecBase Info VName-> EvalM DepVal
depsDecBase (ValDec bindings) = do
  env <- askEnv
  let env' = nestedNamesToSelfEnv $ Nested (map stripPatBase (valBindParams bindings)) 
    in localEnv (const $ env' `M.union` env) (depsExpBase $ valBindBody bindings)
    -- ^^ M.union above might be dangerous (prefers env' over env in duplicates)
depsDecBase _ = failure "Unrecognized declaration base"

depsExpBase :: ExpBase Info VName -> EvalM DepVal
depsExpBase (Literal _ _) = pure $ DepVal mempty
depsExpBase (IntLit _ _ _) = pure $ DepVal mempty
depsExpBase (FloatLit _ _ _) = pure $ DepVal mempty
depsExpBase (StringLit _ _) = pure $ DepVal mempty
depsExpBase (Hole _ _) = pure $ DepVal mempty
depsExpBase (Var qn _ _) = do
  env <- askEnv
  envLookup (qualLeaf qn) env
depsExpBase (Parens eb _) = depsExpBase eb
depsExpBase (QualParens qn eb _) = depsExpBase eb -- OBS
depsExpBase (TupLit ebn _) = do
  d_n <- mapM depsExpBase ebn
  pure $ DepTuple d_n
depsExpBase (RecordLit fb sl) = pure $ DepVal mempty -- OBS
depsExpBase (Lambda pb_n eb _ _ _) = do
  env <- askEnv
  pure $ DepFun env (map stripID pb_n) eb 
  where stripID id = case id of
                        Id vn _ _ -> vn
                        _ -> VName "a" 0 -- OBS
depsExpBase (AppExp base _) = depsAppExpBase base

depsFieldBase :: FieldBase Info VName -> EvalM DepVal
depsFieldBase (RecordFieldExplicit _ _ _) = pure $ DepVal mempty -- 
depsFieldBase (RecordFieldImplicit _ _ _) = pure $ DepVal mempty -- 

depsAppExpBase :: AppExpBase Info VName -> EvalM DepVal
depsAppExpBase (Apply eb1 lst _) = do
  d1 <- depsExpBase eb1
  case d1 of 
    DepFun env p_n body -> do
      d_n <- mapM depsExpBase $ map snd (NE.toList lst)
      localEnv (const $ foldr M.union depsEnvEmpty $ zipWith M.singleton p_n d_n) $ depsExpBase body
    deps -> pure $ DepVal mempty -- OBS $ Maybe envUnion deps d2_n
depsAppExpBase (Range eb1 maybe_eb2 _ _) = do
  d1 <- depsExpBase eb1
  d2 <- maybe (pure $ DepVal mempty) depsExpBase maybe_eb2
  pure $ d1 `depValJoin` d2
depsAppExpBase (LetPat _ pb eb1 eb2 _) = do
  d1 <- depsExpBase eb1
  env <- askEnv
  case stripPatBase pb of
    Name vn -> localEnv (const $ env `M.union` M.singleton vn d1) $ depsExpBase eb2
    a -> failure $ "Unknown variable: " <> (show pb)
depsAppExpBase (LetFun vn _ _ _) = pure $ DepVal mempty -- Not quite sure what LetFun is ????
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
depsAppExpBase (LetWith _ _ _ _ _ _) = pure $ DepVal mempty -- Not sure what this is ????
depsAppExpBase (Index eb sb _) = do
  d <- depsExpBase eb 
  d_n <- mapM depsDimIndexBase sb
  pure $ foldr depValJoin d d_n 
depsAppExpBase (Match eb ne_cb _) = do
  d1 <- depsExpBase eb
  d_n <- mapM depsCaseBase (NE.toList ne_cb)
  pure $ foldr depValJoin d1 d_n

depsCaseBase :: CaseBase Info VName -> EvalM DepVal
depsCaseBase (CasePat pb eb _) = do
  d1 <- depsPatBase pb
  d2 <- depsExpBase eb
  pure $ d1 `depValJoin` d2

depsDimIndexBase :: DimIndexBase Info VName -> EvalM DepVal
depsDimIndexBase (DimFix eb) = depsExpBase eb
depsDimIndexBase (DimSlice maybe_eb1 maybe_eb2 maybe_eb3) = do
  d1 <- maybe (pure $ DepVal mempty) depsExpBase maybe_eb1
  d2 <- maybe (pure $ DepVal mempty) depsExpBase maybe_eb2
  d3 <- maybe (pure $ DepVal mempty) depsExpBase maybe_eb3
  pure $ d1 `depValJoin` d2 `depValJoin` d3

depsPatBase :: PatBase Info VName t -> EvalM DepVal
depsPatBase (TuplePat pb_n _) = do
  d_n <- mapM depsPatBase pb_n
  pure $ DepTuple d_n 
depsPatBase (RecordPat rcrd _) = do
  d_n <- mapM (depsPatBase . snd) rcrd
  pure $ foldr depValJoin (DepVal mempty) d_n
depsPatBase (Id vn _ _) = pure $ DepVal $ Ids [vn]
depsPatBase _ = pure $ DepVal mempty --
-- depsPatBase (RecordPat)
-- depsPatBase (PatParens)
-- depsPatBase (Wildcard)
-- depsPatBase (PatAscription)
-- depsPatBase (PatLit)
-- depsPatBase (PatConstr)
-- depsPatBase (PatAttr)
