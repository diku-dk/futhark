-- | Finds dependencies between variables in programs
module Language.Futhark.Deps
  ( deps
  )
where

import Data.Set qualified as S
import Control.Monad
import Data.Map qualified as M
import Language.Futhark
import Language.Futhark.FreeVars as FV
import Data.List.NonEmpty qualified as NE
import Text.Read

type Error = String

newtype Ids = Ids [VName]
  deriving (Eq, Show)

type Deps = Ids

data DepVal
  = DepVal Ids
  | DepTuple [DepVal]
  | DepFun DepsEnv [VName] (ExpBase Info VName)
  deriving (Eq, Show)

type DepsEnv = M.Map VName DepVal

data NestedVName
  = Name VName
  | Nested [NestedVName]
  | WildcardName
  deriving (Show)

-- | General environment functions. Relies heavily on data.map
envEmpty :: M.Map VName DepVal
envEmpty = M.empty

envSingle :: VName -> DepVal -> DepsEnv
envSingle = M.singleton

envExtend :: VName -> DepVal -> DepsEnv -> DepsEnv
envExtend = M.insert

envLookup :: VName -> DepsEnv -> EvalM DepVal
envLookup vn env = do
  case M.lookup vn env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " <> (show vn) 

envUnion :: DepsEnv -> DepsEnv -> DepsEnv
envUnion = M.union

-- | Monad for evaluating environment
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


depValDeps :: DepVal -> Deps
depValDeps (DepVal x) = x
depValDeps (DepTuple x) = foldMap depValDeps x
depValDeps (DepFun _ _ body) = Ids $ freeVarsList (body)

depValJoin :: DepVal -> DepVal -> DepVal
depValJoin (DepTuple xs) (DepTuple ys)
  | length xs == length ys = DepTuple $ zipWith depValJoin xs ys
depValJoin x y = DepVal $ depValDeps x <> depValDeps y

depValInj :: Deps -> DepVal -> DepVal
depValInj x (DepVal y) = DepVal $ x <> y
depValInj x (DepTuple ys) = DepTuple $ map (depValInj x) ys
depValInj x v = DepVal $ x <> depValDeps v

depsFreeVarsInProgBase :: ProgBase Info VName -> DepsEnv
depsFreeVarsInProgBase base =
  case last $ progDecs base of -- Last progDec is main
    ValDec valbind -> depsFreeVarsInExpBase $ valBindBody valbind
    _ -> envEmpty -- OBS

depsFreeVarsInExpBase :: ExpBase Info VName -> DepsEnv
depsFreeVarsInExpBase eb = M.fromList $ map (\x -> (x, DepVal $ idsSingle x)) $ freeVarsList eb

freeVarsList :: ExpBase Info VName -> [VName]
freeVarsList eb = S.toList $ FV.fvVars $ freeInExp eb

-- Converts pattern bases to pure NestedVNames. *UNFINISHED* and potentially unnecessary  
stripPatBase :: PatBase Info VName t -> NestedVName
stripPatBase (Id vn _ _) = Name vn
stripPatBase _ = WildcardName

nestedNamesToSelfEnv :: NestedVName -> DepsEnv
nestedNamesToSelfEnv (Name vn) = envSingle vn (DepVal $ idsSingle vn)
nestedNamesToSelfEnv (Nested nvn) = foldr envUnion envEmpty (map nestedNamesToSelfEnv nvn) -- OBS
nestedNamesToSelfEnv WildcardName = envEmpty

depsProgBase :: ProgBase Info VName -> EvalM DepVal 
depsProgBase base = depsDecBase $ last $ progDecs base -- obs
depsProgBase _ = failure "Unrecognized program base"

depsDecBase :: DecBase Info VName-> EvalM DepVal
depsDecBase (ValDec bindings) = do
  env <- askEnv
  let env' = nestedNamesToSelfEnv $ Nested (map stripPatBase (valBindParams bindings)) 
    in localEnv (const $ env' `envUnion` env) (depsExpBase $ valBindBody bindings)
    -- ^^ envUnion above might be dangerous (prefers env' over env in duplicates)
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
depsExpBase (QualParens qn eb _) = do
  env <- askEnv
  d1 <- envLookup (qualLeaf $ fst qn) env
  d2 <- depsExpBase eb
  pure $ d1 `depValJoin` d2
depsExpBase (TupLit ebn _) = do
  d_n <- mapM depsExpBase ebn
  pure $ DepTuple d_n
depsExpBase (RecordLit fb_n _) = do
  d_n <- mapM depsFieldBase fb_n
  pure $ foldr depValJoin (DepVal mempty) d_n
depsExpBase (ArrayLit eb_n _ _) = do
  d_n <- mapM depsExpBase eb_n
  pure $ foldr depValJoin (DepVal mempty) d_n
depsExpBase (ArrayVal _ _ _) = pure $ DepVal mempty
depsExpBase (Attr _ eb _) = depsExpBase eb -- OBS
depsExpBase (Project name eb _ _) = do -- ???? name has to be integer?
  d1 <- depsExpBase eb
  pure $
    case (d1, readMaybe (nameToString name) :: Maybe Int) of
      (DepTuple x, Just i) | i < length x -> x !! i
      (x, _) -> x 
depsExpBase (Negate eb _) = depsExpBase eb
depsExpBase (Not eb _) = depsExpBase eb
depsExpBase (Assert eb1 eb2 _ _) = do
  d1 <- depsExpBase eb1
  d2 <- depsExpBase eb2
  pure $ d1 `depValJoin` d2
depsExpBase (Constr _ eb_n _ _) = do  -- ????
  d_n <- mapM depsExpBase eb_n
  pure $ foldr depValJoin (DepVal mempty) d_n
depsExpBase (Update eb1 sb eb2 _) = do
  d1 <- depsExpBase eb1
  d2 <- depsExpBase eb2
  d_n <- depsSliceBase sb
  pure $ foldr depValJoin (d1 `depValJoin` d2) d_n
depsExpBase (RecordUpdate eb1 _ eb2 _ _) = do
  d1 <- depsExpBase eb1
  d2 <- depsExpBase eb2
  pure $ d1 `depValJoin` d2
depsExpBase (OpSection qn _ _) = do -- ????
  env <- askEnv
  envLookup (qualLeaf qn) env
depsExpBase (OpSectionLeft qn _ eb _ _ _) = do
  env <- askEnv
  d1 <- envLookup (qualLeaf qn) env
  d2 <- depsExpBase eb
  pure $ d1 `depValJoin` d2
depsExpBase (OpSectionRight qn _ eb _ _ _) = do
  env <- askEnv
  d1 <- envLookup (qualLeaf qn) env
  d2 <- depsExpBase eb
  pure $ d1 `depValJoin` d2
depsExpBase (ProjectSection _ _ _) = pure $ DepVal mempty -- ???? I don't know what this does or how to handle it
depsExpBase (IndexSection sb _ _) = do
  d_n <- depsSliceBase sb
  pure $ foldr depValJoin (DepVal mempty) d_n
depsExpBase (Ascript eb te _) = do
  d1 <- depsExpBase eb
  d2 <- depsTypeExp te
  pure $ d1 `depValJoin` d2
depsExpBase (Coerce eb te _ _) = do
  d1 <- depsExpBase eb
  d2 <- depsTypeExp te
  pure $ d1 `depValJoin` d2  
depsExpBase (Lambda pb_n eb _ _ _) = do
  env <- askEnv
  names <- mapM (\x -> 
    case stripPatBase x of
      Name vn -> pure vn
      vn -> failure $ "Impossible pattern base for lambda" <> (show vn)
      ) pb_n -- Might certainly need more cases ????
  pure $ DepFun env names eb 
depsExpBase (AppExp aeb _) = depsAppExpBase aeb

depsFieldBase :: FieldBase Info VName -> EvalM DepVal
depsFieldBase (RecordFieldExplicit _ eb _) = depsExpBase eb
depsFieldBase (RecordFieldImplicit (L _ vn) _ _) = do
  env <- askEnv
  envLookup vn env  

depsAppExpBase :: AppExpBase Info VName -> EvalM DepVal
depsAppExpBase (Apply eb1 lst _) = do
  d1 <- depsExpBase eb1
  d_n <- mapM depsExpBase $ map snd (NE.toList lst)
  case d1 of 
    DepFun env p_n body -> do
      localEnv (const $ foldr envUnion env $ zipWith envSingle p_n d_n) $ depsExpBase body
    _ -> pure $ foldr depValJoin d1 d_n
depsAppExpBase (Range eb1 maybe_eb2 _ _) = do
  d1 <- depsExpBase eb1
  d2 <- maybe (pure $ DepVal mempty) depsExpBase maybe_eb2
  pure $ d1 `depValJoin` d2
depsAppExpBase (LetPat _ pb eb1 eb2 _) = do
  d1 <- depsExpBase eb1
  env <- askEnv
  case stripPatBase pb of
    Name vn -> localEnv (const $ envExtend vn d1 env) $ depsExpBase eb2
    _ -> failure $ "Unknown variable: " <> (show pb)
depsAppExpBase (LetFun _ _ _ _) = pure $ DepVal mempty -- Not quite sure what LetFun is ????
depsAppExpBase (If eb1 eb2 eb3 _) = do
  d1 <- depsExpBase eb1
  d2 <- depsExpBase eb2
  d3 <- depsExpBase eb3
  pure $ depValDeps d1 `depValInj` (d2 `depValJoin` d3)
depsAppExpBase (Loop vn_n (Id vn _ _) lib lfb eb  _) = do -- OBS only works for pattern bases of Id .. currently
  d1 <- depsLoopInitBase lib
  case lfb of
    For ib' eb' -> do
      d2 <- depsExpBase eb'
      env <- askEnv
      d3 <- loop (identName ib') d1
      pure $ depValDeps d2 `depValInj` d3
    ForIn (Id vn' _ _) eb' -> do -- OBS Does not cover all patterns
      d2 <- depsExpBase eb'
      d3 <- loop vn' d1
      pure $ depValDeps d2 `depValInj` d3
    While eb' -> depsExpBase eb' -- OBS wrong
  where loop i deps = do
              env <- askEnv
              let env' = envExtend i (DepVal mempty) $ envExtend vn deps env
                in do
                  deps' <- localEnv (const env') (depsExpBase eb)
                  if deps == deps'
                    then pure deps'
                    else loop i $ deps `depValJoin` deps' 

depsAppExpBase (BinOp _ _ eb1 eb2 _) = do
  d1 <- depsExpBase $ fst eb1
  d2 <- depsExpBase $ fst eb2
  pure $ d1 `depValJoin` d2
depsAppExpBase (LetWith _ _ _ _ _ _) = pure $ DepVal mempty -- Not sure what this construct is ????
depsAppExpBase (Index eb sb _) = do
  d <- depsExpBase eb 
  d_n <- depsSliceBase sb
  pure $ foldr depValJoin d d_n 
depsAppExpBase (Match eb ne_cb _) = do
  d1 <- depsExpBase eb
  d_n <- mapM depsCaseBase (NE.toList ne_cb)
  pure $ foldr depValJoin (DepVal mempty) $ map (\x -> depValInj (depValDeps x) d1) d_n
  -- OBS use of injection (might need to be removed)

depsLoopInitBase :: LoopInitBase Info  VName -> EvalM DepVal
depsLoopInitBase (LoopInitExplicit eb) = depsExpBase eb
depsLoopInitBase (LoopInitImplicit (Info eb)) = depsExpBase eb

-- depsLoopFormBase :: LoopFormBase f VName -> EvalM DepVal
-- depsLoopFormBase (For ib eb) = do
--   d1 <- depsIdentBase ib
--   d2 <- depsExpBase eb
--   pure $ d1 `depValJoin` d2
-- depsLoopFormBase (ForIn pb eb) = do
--   d1 <- depsPatBase pb
--   d2 <- depsExpBase eb
--   pure $ d1 `depValJoin` d2
-- depsLoopFormBase (While eb) = depsExpBase eb

-- depsIdentBase :: IdentBase f VName -> EvalM DepVal 
-- depsIdentBase ident = do
--   env <- askEnv
--   envLookup (identName ident) env

depsCaseBase :: CaseBase Info VName -> EvalM DepVal
depsCaseBase (CasePat pb eb _) = do 
  d1 <- depsPatBase pb
  d2 <- depsExpBase eb
  pure $ depValDeps d1 `depValInj` d2 -- OBS use of injection here (might need to be removed)

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
depsPatBase (PatParens pb _) = depsPatBase pb
depsPatBase (Id vn _ _) = pure $ DepVal $ Ids [vn]
depsPatBase (Wildcard _ _) = pure $ DepVal mempty
depsPatBase (PatAscription pb te _) = do
  d1 <- depsPatBase pb
  d2 <- depsTypeExp te
  pure $ d1 `depValJoin` d2
depsPatBase (PatLit _ _ _) = pure $ DepVal mempty
depsPatBase (PatConstr _ _ pb_n _) = do
  d_n <- mapM depsPatBase pb_n
  pure $ foldr depValJoin (DepVal mempty) d_n
depsPatBase (PatAttr _ pb _) = depsPatBase pb

depsTypeExp :: TypeExp (ExpBase Info VName) VName -> EvalM DepVal
depsTypeExp (TEVar qn _) = do
  env <- askEnv
  envLookup (qualLeaf qn) env
depsTypeExp (TEParens te _) = depsTypeExp te
depsTypeExp (TETuple te_n _) = do
  d_n <- mapM depsTypeExp te_n
  pure $ DepTuple d_n
depsTypeExp (TERecord lst _) = do
  d_n <- mapM (depsTypeExp . snd) lst
  pure $ foldr depValJoin (DepVal mempty) d_n
depsTypeExp (TEArray se te _ ) = do
   d1 <- depsSizeExp se
   d2 <- depsTypeExp te
   pure $ d1 `depValJoin` d2
depsTypeExp (TEUnique te _) = depsTypeExp te
depsTypeExp (TEApply te tae _) = do -- ????
  d1 <- depsTypeExp te
  d2 <- depsTypeArgExp tae
  pure $ d1 `depValJoin` d2
depsTypeExp (TEArrow maybe_vn te1 te2 _) = do
  d1 <- depsTypeExp te1
  d2 <- depsTypeExp te2
  case maybe_vn of
    Just x -> do
      env <- askEnv
      d3 <- envLookup x env
      pure $ d1 `depValJoin` d2 `depValJoin` d3
    Nothing -> pure $ d1 `depValJoin` d2
depsTypeExp (TESum lst _) = do
  d_n <- mapM inner (map snd lst)
  pure $ foldr depValJoin (DepVal mempty) d_n
  where inner x_n = do
                      d_n' <- mapM depsTypeExp x_n
                      pure $ foldr depValJoin (DepVal mempty) d_n'  
depsTypeExp (TEDim vn_n te _) = do
  env <- askEnv
  d_n <- mapM (\x -> envLookup x env) vn_n
  d1 <- depsTypeExp te
  pure $ foldr depValJoin d1 d_n

depsTypeArgExp :: TypeArgExp (ExpBase Info VName) VName -> EvalM DepVal
depsTypeArgExp (TypeArgExpSize se) = depsSizeExp se
depsTypeArgExp (TypeArgExpType te) = depsTypeExp te

depsSizeExp :: SizeExp (ExpBase Info VName) -> EvalM DepVal
depsSizeExp (SizeExp eb _) = depsExpBase eb
depsSizeExp (SizeExpAny _) = pure $ DepVal mempty

depsSliceBase :: SliceBase Info VName -> EvalM [DepVal]
depsSliceBase = mapM depsDimIndexBase

runDeps :: DepsEnv -> EvalM a -> Either Error a
runDeps env (EvalM m) = m env

deps :: Prog -> String
deps prog =
  case runDeps (depsFreeVarsInProgBase prog) (depsProgBase prog) of
    Left a -> "Error in dependency interpreter: " ++ a 
    Right a -> show a
