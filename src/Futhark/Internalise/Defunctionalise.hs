{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Defunctionalization of typed, monomorphic Futhark programs without modules.
module Futhark.Internalise.Defunctionalise
  ( transformProg ) where

import qualified Control.Arrow as Arrow
import           Control.Monad.RWS hiding (Sum)
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq

import           Futhark.MonadFreshNames
import           Language.Futhark
import           Futhark.Representation.AST.Pretty ()

-- | An expression or an extended 'Lambda' (with size parameters,
-- which AST lambdas do not support).
data ExtExp = ExtLambda [TypeParam] [Pattern] Exp (Aliasing, StructType) SrcLoc
            | ExtExp Exp
  deriving (Show)

-- | A static value stores additional information about the result of
-- defunctionalization of an expression, aside from the residual expression.
data StaticVal = Dynamic PatternType
               | LambdaSV [VName] Pattern StructType ExtExp Env
                 -- ^ The 'VName's are shape parameters that are bound
                 -- by the 'Pattern'.
               | RecordSV [(Name, StaticVal)]
               | SumSV Name [StaticVal] [(Name, [PatternType])]
                 -- ^ The constructor that is actually present, plus
                 -- the others that are not.
               | DynamicFun (Exp, StaticVal) StaticVal
               | IntrinsicSV
  deriving (Show)

-- | Environment mapping variable names to their associated static value.
type Env = M.Map VName StaticVal

localEnv :: Env -> DefM a -> DefM a
localEnv env = local $ Arrow.second (env<>)

-- Even when using a "new" environment (for evaluating closures) we
-- still ram the global environment of DynamicFuns in there.
localNewEnv :: Env -> DefM a -> DefM a
localNewEnv env = local $ \(globals, old_env) ->
  (globals, M.filterWithKey (\k _ -> k `S.member` globals) old_env <> env)

extendEnv :: VName -> StaticVal -> DefM a -> DefM a
extendEnv vn sv = localEnv (M.singleton vn sv)

askEnv :: DefM Env
askEnv = asks snd

isGlobal :: VName -> DefM a -> DefM a
isGlobal v = local $ Arrow.first (S.insert v)

-- | Returns the defunctionalization environment restricted
-- to the given set of variable names and types.
restrictEnvTo :: NameSet -> DefM Env
restrictEnvTo (NameSet m) = restrict <$> ask
  where restrict (globals, env) = M.mapMaybeWithKey keep env
          where keep k sv = do guard $ not $ k `S.member` globals
                               u <- M.lookup k m
                               Just $ restrict' u sv
        restrict' Nonunique (Dynamic t) =
          Dynamic $ t `setUniqueness`  Nonunique
        restrict' _ (Dynamic t) =
          Dynamic t
        restrict' u (LambdaSV dims pat t e env) =
          LambdaSV dims pat t e $ M.map (restrict' u) env
        restrict' u (RecordSV fields) =
          RecordSV $ map (fmap $ restrict' u) fields
        restrict' u (SumSV c svs fields) =
          SumSV c (map (restrict' u) svs) fields
        restrict' u (DynamicFun (e, sv1) sv2) =
          DynamicFun (e, restrict' u sv1) $ restrict' u sv2
        restrict' _ IntrinsicSV = IntrinsicSV

-- | Defunctionalization monad.  The Reader environment tracks both
-- the current Env as well as the set of globally defined dynamic
-- functions.  This is used to avoid unnecessarily large closure
-- environments.
newtype DefM a = DefM (RWS (S.Set VName, Env) (Seq.Seq ValBind) VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader (S.Set VName, Env),
            MonadWriter (Seq.Seq ValBind),
            MonadFreshNames)

-- | Run a computation in the defunctionalization monad. Returns the result of
-- the computation, a new name source, and a list of lifted function declations.
runDefM :: VNameSource -> DefM a -> (a, VNameSource, Seq.Seq ValBind)
runDefM src (DefM m) = runRWS m mempty src

collectFuns :: DefM a -> DefM (a, Seq.Seq ValBind)
collectFuns m = pass $ do
  (x, decs) <- listen m
  return ((x, decs), const mempty)

-- | Looks up the associated static value for a given name in the environment.
lookupVar :: SrcLoc -> VName -> DefM StaticVal
lookupVar loc x = do
  env <- askEnv
  case M.lookup x env of
    Just sv -> return sv
    Nothing -- If the variable is unknown, it may refer to the 'intrinsics'
            -- module, which we will have to treat specially.
      | baseTag x <= maxIntrinsicTag -> return IntrinsicSV
      | otherwise -> error $ "Variable " ++ pretty x ++ " at "
                          ++ locStr loc ++ " is out of scope."

-- Like patternDimNames, but ignores sizes that are only found in
-- funtion types.
arraySizes :: StructType -> S.Set VName
arraySizes (Scalar Arrow{}) = mempty
arraySizes (Scalar (Record fields)) = foldMap arraySizes fields
arraySizes (Scalar (Sum cs)) = foldMap (foldMap arraySizes) cs
arraySizes (Scalar TypeVar{}) = mempty
arraySizes (Scalar Prim{}) = mempty
arraySizes (Array _ _ t shape) =
  arraySizes (Scalar t) <> foldMap dimName (shapeDims shape)
  where dimName :: DimDecl VName -> S.Set VName
        dimName (NamedDim qn) = S.singleton $ qualLeaf qn
        dimName _             = mempty

patternArraySizes :: Pattern -> S.Set VName
patternArraySizes = arraySizes . patternStructType

defuncFun :: [TypeParam] -> [Pattern] -> Exp -> (Aliasing, StructType) -> SrcLoc
          -> DefM (Exp, StaticVal)
defuncFun tparams pats e0 (closure, ret) loc = do
  when (any isTypeParam tparams) $
    error $ "Received a lambda with type parameters at " ++ locStr loc
         ++ ", but the defunctionalizer expects a monomorphic input program."
  -- Extract the first parameter of the lambda and "push" the
  -- remaining ones (if there are any) into the body of the lambda.
  let (dims, pat, ret', e0') = case pats of
        [] -> error "Received a lambda with no parameters."
        [pat'] -> (map typeParamName tparams, pat', ret, ExtExp e0)
        (pat' : pats') ->
          -- Split shape parameters into those that are determined by
          -- the first pattern, and those that are determined by later
          -- patterns.
          let bound_by_pat = (`S.member` patternArraySizes pat') . typeParamName
              (pat_dims, rest_dims) = partition bound_by_pat tparams
          in (map typeParamName pat_dims, pat',
              foldFunType (map (toStruct . patternType) pats') ret,
              ExtLambda rest_dims pats' e0 (closure, ret) loc)

  -- Construct a record literal that closes over the environment of
  -- the lambda.  Closed-over 'DynamicFun's are converted to their
  -- closure representation.
  let used = freeVars (Lambda pats e0 Nothing (Info (closure, ret)) loc)
             `without` mconcat (map oneName dims)
  used_env <- restrictEnvTo used

  -- The closure parts that are sizes are proactively turned into size
  -- parameters.
  let sizes_of_arrays = foldMap (arraySizes . toStruct . typeFromSV) used_env <>
                        patternArraySizes pat
      notSize = not . (`S.member` sizes_of_arrays)
      (fields, env) = unzip $ map closureFromDynamicFun $
                      filter (notSize . fst) $ M.toList used_env
      env' = M.fromList env
      closure_dims = S.toList sizes_of_arrays

  return (RecordLit fields loc,
          LambdaSV (nub $ dims<>closure_dims) pat ret' e0' env')

  where closureFromDynamicFun (vn, DynamicFun (clsr_env, sv) _) =
          let name = nameFromString $ pretty vn
          in (RecordFieldExplicit name clsr_env noLoc, (vn, sv))

        closureFromDynamicFun (vn, sv) =
          let name = nameFromString $ pretty vn
              tp' = typeFromSV sv
          in (RecordFieldExplicit name
               (Var (qualName vn) (Info tp') noLoc) noLoc, (vn, sv))

-- | Defunctionalization of an expression. Returns the residual expression and
-- the associated static value in the defunctionalization monad.
defuncExp :: Exp -> DefM (Exp, StaticVal)

defuncExp e@Literal{} =
  return (e, Dynamic $ typeOf e)

defuncExp e@IntLit{} =
  return (e, Dynamic $ typeOf e)

defuncExp e@FloatLit{} =
  return (e, Dynamic $ typeOf e)

defuncExp e@StringLit{} =
  return (e, Dynamic $ typeOf e)

defuncExp (Parens e loc) = do
  (e', sv) <- defuncExp e
  return (Parens e' loc, sv)

defuncExp (QualParens qn e loc) = do
  (e', sv) <- defuncExp e
  return (QualParens qn e' loc, sv)

defuncExp (TupLit es loc) = do
  (es', svs) <- unzip <$> mapM defuncExp es
  return (TupLit es' loc, RecordSV $ zip tupleFieldNames svs)

defuncExp (RecordLit fs loc) = do
  (fs', names_svs) <- unzip <$> mapM defuncField fs
  return (RecordLit fs' loc, RecordSV names_svs)

  where defuncField (RecordFieldExplicit vn e loc') = do
          (e', sv) <- defuncExp e
          return (RecordFieldExplicit vn e' loc', (vn, sv))
        defuncField (RecordFieldImplicit vn _ loc') = do
          sv <- lookupVar loc' vn
          case sv of
            -- If the implicit field refers to a dynamic function, we
            -- convert it to an explicit field with a record closing over
            -- the environment and bind the corresponding static value.
            DynamicFun (e, sv') _ -> let vn' = baseName vn
                                     in return (RecordFieldExplicit vn' e loc',
                                                (vn', sv'))
            -- The field may refer to a functional expression, so we get the
            -- type from the static value and not the one from the AST.
            _ -> let tp = Info $ typeFromSV sv
                 in return (RecordFieldImplicit vn tp loc', (baseName vn, sv))

defuncExp (ArrayLit es t@(Info t') loc) = do
  es' <- mapM defuncExp' es
  return (ArrayLit es' t loc, Dynamic t')

defuncExp (Range e1 me incl t@(Info t', _) loc) = do
  e1' <- defuncExp' e1
  me' <- mapM defuncExp' me
  incl' <- mapM defuncExp' incl
  return (Range e1' me' incl' t loc, Dynamic t')

defuncExp e@(Var qn _ loc) = do
  sv <- lookupVar loc (qualLeaf qn)
  case sv of
    -- If the variable refers to a dynamic function, we return its closure
    -- representation (i.e., a record expression capturing the free variables
    -- and a 'LambdaSV' static value) instead of the variable itself.
    DynamicFun closure _ -> return closure
    -- Intrinsic functions used as variables are eta-expanded, so we
    -- can get rid of them.
    IntrinsicSV -> do
      (pats, body, tp) <- etaExpand e
      defuncExp $ Lambda pats body Nothing (Info (mempty, tp)) noLoc
    _ -> let tp = typeFromSV sv
         in return (Var qn (Info tp) loc, sv)

defuncExp (Ascript e0 tydecl loc)
  | orderZero (typeOf e0) = do (e0', sv) <- defuncExp e0
                               return (Ascript e0' tydecl loc, sv)
  | otherwise = defuncExp e0

defuncExp (Coerce e0 tydecl t loc)
  | orderZero (typeOf e0) = do (e0', sv) <- defuncExp e0
                               return (Coerce e0' tydecl t loc, sv)
  | otherwise = defuncExp e0

defuncExp (LetPat pat e1 e2 (_, retext) loc) = do
  (e1', sv1) <- defuncExp e1
  let env  = matchPatternSV pat sv1
      pat' = updatePattern pat sv1
  (e2', sv2) <- localEnv env $ defuncExp e2
  return (LetPat pat' e1' e2' (Info $ typeOf e2', retext) loc, sv2)

-- Local functions are handled by rewriting them to lambdas, so that
-- the same machinery can be re-used.
defuncExp (LetFun vn (dims, pats, _, Info ret, e1) e2 loc) = do
  (e1', sv1) <- defuncFun dims pats e1 (mempty, ret) loc
  (e2', sv2) <- localEnv (M.singleton vn sv1) $ defuncExp e2
  return (LetPat (Id vn (Info (typeOf e1')) loc) e1' e2' (Info $ typeOf e2', Info []) loc,
          sv2)

defuncExp (If e1 e2 e3 tp loc) = do
  (e1', _ ) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  (e3', _ ) <- defuncExp e3
  return (If e1' e2' e3' tp loc, sv)

defuncExp e@(Apply f@(Var f' _ _) arg d (t, ext) loc)
  | baseTag (qualLeaf f') <= maxIntrinsicTag,
    TupLit es tuploc <- arg = do
      -- defuncSoacExp also works fine for non-SOACs.
      es' <- mapM defuncSoacExp es
      return (Apply f (TupLit es' tuploc) d (t, ext) loc,
              Dynamic $ typeOf e)

defuncExp e@Apply{} = defuncApply 0 e

defuncExp (Negate e0 loc) = do
  (e0', sv) <- defuncExp e0
  return (Negate e0' loc, sv)

defuncExp (Lambda pats e0 _ (Info (closure, ret)) loc) =
  defuncFun [] pats e0 (closure, ret) loc

-- Operator sections are expected to be converted to lambda-expressions
-- by the monomorphizer, so they should no longer occur at this point.
defuncExp OpSection{}      = error "defuncExp: unexpected operator section."
defuncExp OpSectionLeft{}  = error "defuncExp: unexpected operator section."
defuncExp OpSectionRight{} = error "defuncExp: unexpected operator section."
defuncExp ProjectSection{} = error "defuncExp: unexpected projection section."
defuncExp IndexSection{}   = error "defuncExp: unexpected projection section."

defuncExp (DoLoop sparams pat e1 form e3 ret loc) = do
  (e1', sv1) <- defuncExp e1
  let env1 = matchPatternSV pat sv1
  (form', env2) <- case form of
    For v e2      -> do e2' <- defuncExp' e2
                        return (For v e2', envFromIdent v)
    ForIn pat2 e2 -> do e2' <- defuncExp' e2
                        return (ForIn pat2 e2', envFromPattern pat2)
    While e2      -> do e2' <- localEnv env1 $ defuncExp' e2
                        return (While e2', mempty)
  (e3', sv) <- localEnv (env1 <> env2) $ defuncExp e3
  return (DoLoop sparams pat e1' form' e3' ret loc, sv)
  where envFromIdent (Ident vn (Info tp) _) =
          M.singleton vn $ Dynamic tp

-- We handle BinOps by turning them into ordinary function applications.
defuncExp (BinOp (qn, qnloc) (Info t)
           (e1, Info (pt1, ext1)) (e2, Info (pt2, ext2))
           (Info ret) (Info retext) loc) =
  defuncExp $ Apply (Apply (Var qn (Info t) qnloc)
                     e1 (Info (diet pt1, ext1))
                     (Info (Scalar $ Arrow mempty Unnamed (fromStruct pt2) ret), Info []) loc)
                    e2 (Info (diet pt2, ext2)) (Info ret, Info retext) loc

defuncExp (Project vn e0 tp@(Info tp') loc) = do
  (e0', sv0) <- defuncExp e0
  case sv0 of
    RecordSV svs -> case lookup vn svs of
      Just sv -> return (Project vn e0' (Info $ typeFromSV sv) loc, sv)
      Nothing -> error "Invalid record projection."
    Dynamic _ -> return (Project vn e0' tp loc, Dynamic tp')
    _ -> error $ "Projection of an expression with static value " ++ show sv0

defuncExp (LetWith id1 id2 idxs e1 body t loc) = do
  e1' <- defuncExp' e1
  sv1 <- lookupVar (identSrcLoc id2) $ identName id2
  idxs' <- mapM defuncDimIndex idxs
  (body', sv) <- extendEnv (identName id1) sv1 $ defuncExp body
  return (LetWith id1 id2 idxs' e1' body' t loc, sv)

defuncExp expr@(Index e0 idxs info loc) = do
  e0' <- defuncExp' e0
  idxs' <- mapM defuncDimIndex idxs
  return (Index e0' idxs' info loc, Dynamic $ typeOf expr)

defuncExp (Update e1 idxs e2 loc) = do
  (e1', sv) <- defuncExp e1
  idxs' <- mapM defuncDimIndex idxs
  e2' <- defuncExp' e2
  return (Update e1' idxs' e2' loc, sv)

-- Note that we might change the type of the record field here.  This
-- is not permitted in the type checker due to problems with type
-- inference, but it actually works fine.
defuncExp (RecordUpdate e1 fs e2 _ loc) = do
  (e1', sv1) <- defuncExp e1
  (e2', sv2) <- defuncExp e2
  let sv = staticField sv1 sv2 fs
  return (RecordUpdate e1' fs e2' (Info $ typeFromSV sv1) loc,
          sv)
  where staticField (RecordSV svs) sv2 (f:fs') =
          case lookup f svs of
            Just sv -> RecordSV $
                       (f, staticField sv sv2 fs') : filter ((/=f) . fst) svs
            Nothing -> error "Invalid record projection."
        staticField (Dynamic t@(Scalar Record{})) sv2 fs'@(_:_) =
          staticField (svFromType t) sv2 fs'
        staticField _ sv2 _ = sv2

defuncExp (Unsafe e1 loc) = do
  (e1', sv) <- defuncExp e1
  return (Unsafe e1' loc, sv)

defuncExp (Assert e1 e2 desc loc) = do
  (e1', _) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  return (Assert e1' e2' desc loc, sv)

defuncExp (Constr name es (Info (Scalar (Sum all_fs))) loc) = do
  (es', svs) <- unzip <$> mapM defuncExp es
  let sv = SumSV name svs $ M.toList $
           name `M.delete` M.map (map defuncType) all_fs
  return (Constr name es' (Info (typeFromSV sv)) loc, sv)
  where defuncType :: Monoid als =>
                      TypeBase (DimDecl VName) als
                   -> TypeBase (DimDecl VName) als
        defuncType (Array as u t shape) = Array as u (defuncScalar t) shape
        defuncType (Scalar t) = Scalar $ defuncScalar t

        defuncScalar :: Monoid als =>
                        ScalarTypeBase (DimDecl VName) als
                     -> ScalarTypeBase (DimDecl VName) als
        defuncScalar (Record fs) = Record $ M.map defuncType fs
        defuncScalar Arrow{} = Record mempty
        defuncScalar (Sum fs) = Sum $ M.map (map defuncType) fs
        defuncScalar (Prim t) = Prim t
        defuncScalar (TypeVar as u tn targs) = TypeVar as u tn targs

defuncExp (Constr name _ (Info t) loc) =
  error $ "Constructor " ++ pretty name ++ " given type " ++
  pretty t ++ " at " ++ locStr loc

defuncExp (Match e cs t loc) = do
  (e', sv) <- defuncExp e
  csPairs  <- mapM (defuncCase sv) cs
  let cs' = fmap fst csPairs
      sv' = snd $ NE.head csPairs
  return (Match e' cs' t loc, sv')

-- | Same as 'defuncExp', except it ignores the static value.
defuncExp' :: Exp -> DefM Exp
defuncExp' = fmap fst . defuncExp

defuncExtExp :: ExtExp -> DefM (Exp, StaticVal)
defuncExtExp (ExtExp e) = defuncExp e
defuncExtExp (ExtLambda tparams pats e0 (closure, ret) loc) =
  defuncFun tparams pats e0 (closure, ret) loc

defuncCase :: StaticVal -> Case -> DefM (Case, StaticVal)
defuncCase sv (CasePat p e loc) = do
  let p'  = updatePattern p sv
      env = matchPatternSV p sv
  (e', sv') <- localEnv env $ defuncExp e
  return (CasePat p' e' loc, sv')

-- | Defunctionalize the function argument to a SOAC by eta-expanding if
-- necessary and then defunctionalizing the body of the introduced lambda.
defuncSoacExp :: Exp -> DefM Exp
defuncSoacExp e@OpSection{}      = return e
defuncSoacExp e@OpSectionLeft{}  = return e
defuncSoacExp e@OpSectionRight{} = return e
defuncSoacExp e@ProjectSection{} = return e

defuncSoacExp (Parens e loc) =
  Parens <$> defuncSoacExp e <*> pure loc

defuncSoacExp (Lambda params e0 decl tp loc) = do
  let env = foldMap envFromPattern params
  e0' <- localEnv env $ defuncSoacExp e0
  return $ Lambda params e0' decl tp loc

defuncSoacExp e
  | Scalar Arrow{} <- typeOf e = do
      (pats, body, tp) <- etaExpand e
      let env = foldMap envFromPattern pats
      body' <- localEnv env $ defuncExp' body
      return $ Lambda pats body' Nothing (Info (mempty, tp)) noLoc
  | otherwise = defuncExp' e

etaExpand :: Exp -> DefM ([Pattern], Exp, StructType)
etaExpand e = do
  let (ps, ret) = getType $ typeOf e
  (pats, vars) <- fmap unzip . forM ps $ \t -> do
    x <- newNameFromString "x"
    return (Id x (Info t) noLoc,
            Var (qualName x) (Info t) noLoc)
  let e' = foldl' (\e1 (e2, t2, argtypes) ->
                     Apply e1 e2 (Info (diet t2, Nothing))
                     (Info (foldFunType argtypes ret), Info []) noLoc)
           e $ zip3 vars ps (drop 1 $ tails ps)
  return (pats, e', toStruct ret)

  where getType (Scalar (Arrow _ _ t1 t2)) =
          let (ps, r) = getType t2 in (t1 : ps, r)
        getType t = ([], t)

-- | Defunctionalize an indexing of a single array dimension.
defuncDimIndex :: DimIndexBase Info VName -> DefM (DimIndexBase Info VName)
defuncDimIndex (DimFix e1) = DimFix . fst <$> defuncExp e1
defuncDimIndex (DimSlice me1 me2 me3) =
  DimSlice <$> defunc' me1 <*> defunc' me2 <*> defunc' me3
  where defunc' = mapM defuncExp'

-- | Defunctionalize a let-bound function, while preserving parameters
-- that have order 0 types (i.e., non-functional).
defuncLet :: [TypeParam] -> [Pattern] -> Exp -> StructType
          -> DefM ([TypeParam], [Pattern], Exp, StaticVal)
defuncLet dims ps@(pat:pats) body rettype
  | patternOrderZero pat = do

      let bound_by_pat = (`S.member` patternDimNames pat) . typeParamName
          -- Take care to not include more size parameters than necessary.
          (pat_dims, rest_dims) = partition bound_by_pat dims
          env = envFromPattern pat <> envFromShapeParams pat_dims
      (rest_dims', pats', body', sv) <- localEnv env $ defuncLet rest_dims pats body rettype
      closure <- defuncFun dims ps body (mempty, rettype) noLoc
      return (pat_dims ++ rest_dims', pat : pats', body', DynamicFun closure sv)
  | otherwise = do
      (e, sv) <- defuncFun dims ps body (mempty, rettype) noLoc
      return ([], [], e, sv)

defuncLet _ [] body rettype = do
  (body', sv) <- defuncExp body
  return ([], [], body', imposeType sv rettype )
  where imposeType Dynamic{} t =
          Dynamic $ fromStruct t
        imposeType (RecordSV fs1) (Scalar (Record fs2)) =
          RecordSV $ M.toList $ M.intersectionWith imposeType (M.fromList fs1) fs2
        imposeType sv _ = sv

-- | Defunctionalize an application expression at a given depth of application.
-- Calls to dynamic (first-order) functions are preserved at much as possible,
-- but a new lifted function is created if a dynamic function is only partially
-- applied.
defuncApply :: Int -> Exp -> DefM (Exp, StaticVal)
defuncApply depth e@(Apply e1 e2 d t@(Info ret, Info ext) loc) = do
  let (argtypes, _) = unfoldFunType ret
  (e1', sv1) <- defuncApply (depth+1) e1
  (e2', sv2) <- defuncExp e2
  let e' = Apply e1' e2' d t loc
  case sv1 of
    LambdaSV dims pat e0_t e0 closure_env -> do
      let env' = matchPatternSV pat sv2
          env_dim = envFromDimNames dims
      (e0', sv) <- localNewEnv (env' <> closure_env <> env_dim) $ defuncExtExp e0

      let closure_pat = buildEnvPattern closure_env
          pat' = updatePattern pat sv2

      -- Lift lambda to top-level function definition.  We put in
      -- a lot of effort to try to infer the uniqueness attributes
      -- of the lifted function, but this is ultimately all a sham
      -- and a hack.  There is some piece we're missing.
      let params = [closure_pat, pat']
          params_for_rettype = params ++ svParams sv1 ++ svParams sv2
          svParams (LambdaSV _ sv_pat _ _ _) = [sv_pat]
          svParams _                         = []
          rettype = buildRetType closure_env params_for_rettype e0_t $ typeOf e0'

          already_bound = S.fromList dims <>
                          S.map identName (foldMap patternIdents params)
          more_dims = S.toList $
                      S.filter (`S.notMember` already_bound) $
                      foldMap patternArraySizes params

          -- Embed some information about the original function
          -- into the name of the lifted function, to make the
          -- result slightly more human-readable.
          liftedName i (Var f _ _) =
            "lifted_" ++ show i ++ "_" ++ baseString (qualLeaf f)
          liftedName i (Apply f _ _ _ _) =
            liftedName (i+1) f
          liftedName _ _ = "lifted"

      fname <- newNameFromString $ liftedName (0::Int) e1
      liftValDec fname rettype (dims ++ more_dims) params e0'

      let t1 = toStruct $ typeOf e1'
          t2 = toStruct $ typeOf e2'
          fname' = qualName fname
          fname'' = Var fname' (Info (Scalar $ Arrow mempty Unnamed (fromStruct t1) $
                                      Scalar $ Arrow mempty Unnamed (fromStruct t2) rettype))
                    loc

          -- FIXME: what if this application returns both a function
          -- and a value?
          callret | orderZero ret = (Info ret, Info ext)
                  | otherwise     = (Info rettype, Info ext)

      return (Parens (Apply (Apply fname'' e1'
                              (Info (Observe, Nothing))
                              (Info $ Scalar $ Arrow mempty Unnamed (fromStruct t2) rettype,
                               Info [])
                              loc)
                      e2' d callret loc) noLoc, sv)

    -- If e1 is a dynamic function, we just leave the application in place,
    -- but we update the types since it may be partially applied or return
    -- a higher-order term.
    DynamicFun _ sv ->
      let (argtypes', rettype) = dynamicFunType sv argtypes
          restype = foldFunType argtypes' rettype `setAliases` aliases ret
          -- FIXME: what if this application returns both a function
          -- and a value?
          callret | orderZero ret = (Info ret, Info ext)
                  | otherwise     = (Info restype, Info ext)
          apply_e = Apply e1' e2' d callret loc
      in return (apply_e, sv)

    -- Propagate the 'IntrinsicsSV' until we reach the outermost application,
    -- where we construct a dynamic static value with the appropriate type.
    IntrinsicSV
      | depth == 0 ->
          -- If the intrinsic is fully applied, then we are done.
          -- Otherwise we need to eta-expand it and recursively
          -- defunctionalise. XXX: might it be better to simply
          -- eta-expand immediately any time we encounter a
          -- non-fully-applied intrinsic?
          if null argtypes
            then return (e', Dynamic $ typeOf e)
            else do (pats, body, tp) <- etaExpand e'
                    defuncExp $ Lambda pats body Nothing (Info (mempty, tp)) noLoc
      | otherwise -> return (e', IntrinsicSV)

    _ -> error $ "Application of an expression that is neither a static lambda "
              ++ "nor a dynamic function, but has static value: " ++ show sv1

defuncApply depth e@(Var qn (Info t) loc) = do
    let (argtypes, _) = unfoldFunType t
    sv <- lookupVar loc (qualLeaf qn)
    case sv of
      DynamicFun _ _
        | fullyApplied sv depth ->
            -- We still need to update the types in case the dynamic
            -- function returns a higher-order term.
            let (argtypes', rettype) = dynamicFunType sv argtypes
            in return (Var qn (Info (foldFunType argtypes' rettype)) loc, sv)

        | otherwise -> do
            fname <- newName $ qualLeaf qn
            let (dims, pats, e0, sv') = liftDynFun sv depth
                (argtypes', rettype) = dynamicFunType sv' argtypes
            liftValDec fname (fromStruct rettype) dims pats e0
            return (Var (qualName fname)
                    (Info (foldFunType argtypes' $ fromStruct rettype)) loc, sv')

      IntrinsicSV -> return (e, IntrinsicSV)

      _ -> return (Var qn (Info (typeFromSV sv)) loc, sv)

defuncApply depth (Parens e _) = defuncApply depth e

defuncApply _ expr = defuncExp expr

-- | Check if a 'StaticVal' and a given application depth corresponds
-- to a fully applied dynamic function.
fullyApplied :: StaticVal -> Int -> Bool
fullyApplied (DynamicFun _ sv) depth
  | depth == 0   = False
  | depth >  0   = fullyApplied sv (depth-1)
fullyApplied _ _ = True

-- | Converts a dynamic function 'StaticVal' into a list of
-- dimensions, a list of parameters, a function body, and the
-- appropriate static value for applying the function at the given
-- depth of partial application.
liftDynFun :: StaticVal -> Int -> ([VName], [Pattern], Exp, StaticVal)
liftDynFun (DynamicFun (e, sv) _) 0 = ([], [], e, sv)
liftDynFun (DynamicFun clsr@(_, LambdaSV dims pat _ _ _) sv) d
  | d > 0 =  let (dims', pats, e', sv') = liftDynFun sv (d-1)
             in (nub $ dims ++ dims', pat : pats, e', DynamicFun clsr sv')
liftDynFun sv _ = error $ "Tried to lift a StaticVal " ++ show sv
                       ++ ", but expected a dynamic function."

-- | Converts a pattern to an environment that binds the individual names of the
-- pattern to their corresponding types wrapped in a 'Dynamic' static value.
envFromPattern :: Pattern -> Env
envFromPattern pat = case pat of
  TuplePattern ps _       -> foldMap envFromPattern ps
  RecordPattern fs _      -> foldMap (envFromPattern . snd) fs
  PatternParens p _       -> envFromPattern p
  Id vn (Info t) _        -> M.singleton vn $ Dynamic t
  Wildcard _ _            -> mempty
  PatternAscription p _ _ -> envFromPattern p
  PatternLit{}            -> mempty
  PatternConstr _ _ ps _  -> foldMap envFromPattern ps

-- | Create an environment that binds the shape parameters.
envFromShapeParams :: [TypeParamBase VName] -> Env
envFromShapeParams = envFromDimNames . map dim
  where dim (TypeParamDim vn _) = vn
        dim tparam = error $
          "The defunctionalizer expects a monomorphic input program,\n" ++
          "but it received a type parameter " ++ pretty tparam ++
          " at " ++ locStr (srclocOf tparam) ++ "."

envFromDimNames :: [VName] -> Env
envFromDimNames = M.fromList . flip zip (repeat $ Dynamic $ Scalar $ Prim $ Signed Int32)

-- | Create a new top-level value declaration with the given function name,
-- return type, list of parameters, and body expression.
liftValDec :: VName -> PatternType -> [VName] -> [Pattern] -> Exp -> DefM ()
liftValDec fname rettype dims pats body = tell $ Seq.singleton dec
  where dims' = map (flip TypeParamDim noLoc) dims
        -- FIXME: this pass is still not correctly size-preserving, so
        -- forget those return sizes that we forgot to propagate along
        -- the way.  Hopefully the internaliser is conservative and
        -- will insert reshapes...
        bound_here = S.fromList dims <> S.map identName (foldMap patternIdents pats)
        anyDimIfNotBound (NamedDim v)
          | qualLeaf v `S.member` bound_here = NamedDim v
          | otherwise = AnyDim
        anyDimIfNotBound d = d
        rettype_st = first anyDimIfNotBound $ toStruct rettype

        dec = ValBind
          { valBindEntryPoint = Nothing
          , valBindName       = fname
          , valBindRetDecl    = Nothing
          , valBindRetType    = Info (rettype_st, [])
          , valBindTypeParams = dims'
          , valBindParams     = pats
          , valBindBody       = body
          , valBindDoc        = Nothing
          , valBindLocation   = noLoc
          }

-- | Given a closure environment, construct a record pattern that
-- binds the closed over variables.
buildEnvPattern :: Env -> Pattern
buildEnvPattern env = RecordPattern (map buildField $ M.toList env) noLoc
  where buildField (vn, sv) =
          (nameFromString (pretty vn),
           Id vn (Info $ typeFromSV sv) noLoc)

-- | Given a closure environment pattern and the type of a term,
-- construct the type of that term, where uniqueness is set to
-- `Nonunique` for those arrays that are bound in the environment or
-- pattern (except if they are unique there).  This ensures that a
-- lifted function can create unique arrays as long as they do not
-- alias any of its parameters.  XXX: it is not clear that this is a
-- sufficient property, unfortunately.
buildRetType :: Env -> [Pattern] -> StructType -> PatternType -> PatternType
buildRetType env pats = comb
  where bound = foldMap oneName (M.keys env) <> foldMap patternVars pats
        boundAsUnique v =
          maybe False (unique . unInfo . identType) $
          find ((==v) . identName) $ S.toList $ foldMap patternIdents pats
        problematic v = (v `member` bound) && not (boundAsUnique v)
        comb (Scalar (Record fs_annot)) (Scalar (Record fs_got)) =
          Scalar $ Record $ M.intersectionWith comb fs_annot fs_got
        comb (Scalar Arrow{}) t =
          descend t
        comb got et =
          descend $ fromStruct got `setUniqueness` uniqueness et `setAliases` aliases et

        descend t@Array{}
          | any (problematic . aliasVar) (aliases t) = t `setUniqueness` Nonunique
        descend (Scalar (Record t)) = Scalar $ Record $ fmap descend t
        descend t = t

-- | Compute the corresponding type for a given static value.
typeFromSV :: StaticVal -> PatternType
typeFromSV (Dynamic tp)           = tp
typeFromSV (LambdaSV _ _ _ _ env) = typeFromEnv env
typeFromSV (RecordSV ls)          = Scalar $ Record $ M.fromList $ map (fmap typeFromSV) ls
typeFromSV (DynamicFun (_, sv) _) = typeFromSV sv
typeFromSV (SumSV name svs fields) =
  Scalar $ Sum $ M.insert name (map typeFromSV svs) $ M.fromList fields
typeFromSV IntrinsicSV            = error $ "Tried to get the type from the "
                                         ++ "static value of an intrinsic."

typeFromEnv :: Env -> PatternType
typeFromEnv = Scalar . Record . M.fromList .
              map (bimap (nameFromString . pretty) typeFromSV) . M.toList

-- | Construct the type for a fully-applied dynamic function from its
-- static value and the original types of its arguments.
dynamicFunType :: StaticVal -> [PatternType] -> ([PatternType], PatternType)
dynamicFunType (DynamicFun _ sv) (p:ps) =
  let (ps', ret) = dynamicFunType sv ps in (p : ps', ret)
dynamicFunType sv _ = ([], typeFromSV sv)

-- | Match a pattern with its static value. Returns an environment with
-- the identifier components of the pattern mapped to the corresponding
-- subcomponents of the static value.
matchPatternSV :: PatternBase Info VName -> StaticVal -> Env
matchPatternSV (TuplePattern ps _) (RecordSV ls) =
  mconcat $ zipWith (\p (_, sv) -> matchPatternSV p sv) ps ls
matchPatternSV (RecordPattern ps _) (RecordSV ls)
  | ps' <- sortOn fst ps, ls' <- sortOn fst ls,
    map fst ps' == map fst ls' =
      mconcat $ zipWith (\(_, p) (_, sv) -> matchPatternSV p sv) ps' ls'
matchPatternSV (PatternParens pat _) sv = matchPatternSV pat sv
matchPatternSV (Id vn (Info t) _) sv =
  -- When matching a pattern with a zero-order STaticVal, the type of
  -- the pattern wins out.  This is important when matching a
  -- nonunique pattern with a unique value.
  if orderZeroSV sv
  then M.singleton vn $ Dynamic t
  else M.singleton vn sv
matchPatternSV (Wildcard _ _) _ = mempty
matchPatternSV (PatternAscription pat _ _) sv = matchPatternSV pat sv
matchPatternSV PatternLit{} _ = mempty
matchPatternSV (PatternConstr c1 _ ps _) (SumSV c2 ls fs)
  | c1 == c2 =
      mconcat $ zipWith matchPatternSV ps ls
  | Just ts <- lookup c1 fs =
      mconcat $ zipWith matchPatternSV ps $ map svFromType ts
  | otherwise =
      error $ "matchPatternSV: missing constructor in type: " ++ pretty c1
matchPatternSV (PatternConstr c1 _ ps _) (Dynamic (Scalar (Sum fs)))
  | Just ts <- M.lookup c1 fs =
      mconcat $ zipWith matchPatternSV ps $ map svFromType ts
  | otherwise =
      error $ "matchPatternSV: missing constructor in type: " ++ pretty c1
matchPatternSV pat (Dynamic t) = matchPatternSV pat $ svFromType t
matchPatternSV pat sv = error $ "Tried to match pattern " ++ pretty pat
                             ++ " with static value " ++ show sv ++ "."

orderZeroSV :: StaticVal -> Bool
orderZeroSV Dynamic{} = True
orderZeroSV (RecordSV fields) = all (orderZeroSV . snd) fields
orderZeroSV _ = False

-- | Given a pattern and the static value for the defunctionalized argument,
-- update the pattern to reflect the changes in the types.
updatePattern :: Pattern -> StaticVal -> Pattern
updatePattern (TuplePattern ps loc) (RecordSV svs) =
  TuplePattern (zipWith updatePattern ps $ map snd svs) loc
updatePattern (RecordPattern ps loc) (RecordSV svs)
  | ps' <- sortOn fst ps, svs' <- sortOn fst svs =
      RecordPattern (zipWith (\(n, p) (_, sv) ->
                                (n, updatePattern p sv)) ps' svs') loc
updatePattern (PatternParens pat loc) sv =
  PatternParens (updatePattern pat sv) loc
updatePattern pat@(Id vn (Info tp) loc) sv
  | orderZero tp = pat
  | otherwise = Id vn (Info $ typeFromSV sv `setUniqueness` Nonunique) loc
updatePattern pat@(Wildcard (Info tp) loc) sv
  | orderZero tp = pat
  | otherwise = Wildcard (Info $ typeFromSV sv) loc
updatePattern (PatternAscription pat tydecl loc) sv
  | orderZero . unInfo $ expandedType tydecl =
      PatternAscription (updatePattern pat sv) tydecl loc
  | otherwise = updatePattern pat sv
updatePattern p@PatternLit{} _ = p
updatePattern pat@(PatternConstr c1 (Info t) ps loc) sv@(SumSV _ svs _)
  | orderZero t = pat
  | otherwise = PatternConstr c1 (Info t') ps' loc
  where t' = typeFromSV sv `setUniqueness` Nonunique
        ps' = zipWith updatePattern ps svs
updatePattern (PatternConstr c1 _ ps loc) (Dynamic t) =
  PatternConstr c1 (Info t) ps loc
updatePattern pat (Dynamic t) = updatePattern pat (svFromType t)
updatePattern pat sv =
  error $ "Tried to update pattern " ++ pretty pat
       ++ "to reflect the static value " ++ show sv

-- | Convert a record (or tuple) type to a record static value. This is used for
-- "unwrapping" tuples and records that are nested in 'Dynamic' static values.
svFromType :: PatternType -> StaticVal
svFromType (Scalar (Record fs)) = RecordSV . M.toList $ M.map svFromType fs
svFromType t                    = Dynamic t

-- A set of names where we also track uniqueness.
newtype NameSet = NameSet (M.Map VName Uniqueness) deriving (Show)

instance Semigroup NameSet where
  NameSet x <> NameSet y = NameSet $ M.unionWith max x y

instance Monoid NameSet where
  mempty = NameSet mempty

without :: NameSet -> NameSet -> NameSet
without (NameSet x) (NameSet y) = NameSet $ x `M.difference` y

member :: VName -> NameSet -> Bool
member v (NameSet m) = v `M.member` m

ident :: Ident -> NameSet
ident v = NameSet $ M.singleton (identName v) (uniqueness $ unInfo $ identType v)

oneName :: VName -> NameSet
oneName v = NameSet $ M.singleton v Nonunique

names :: S.Set VName -> NameSet
names = foldMap oneName

-- | Compute the set of free variables of an expression.
freeVars :: Exp -> NameSet
freeVars expr = case expr of
  Literal{}            -> mempty
  IntLit{}             -> mempty
  FloatLit{}           -> mempty
  StringLit{}          -> mempty
  Parens e _           -> freeVars e
  QualParens _ e _     -> freeVars e
  TupLit es _          -> foldMap freeVars es

  RecordLit fs _       -> foldMap freeVarsField fs
    where freeVarsField (RecordFieldExplicit _ e _)  = freeVars e
          freeVarsField (RecordFieldImplicit vn t _) = ident $ Ident vn t noLoc

  ArrayLit es _ _      -> foldMap freeVars es
  Range e me incl _ _  -> freeVars e <> foldMap freeVars me <>
                          foldMap freeVars incl
  Var qn (Info t) _    -> NameSet $ M.singleton (qualLeaf qn) $ uniqueness t
  Ascript e t _        -> freeVars e <> names (typeDimNames $ unInfo $ expandedType t)
  Coerce e t _ _       -> freeVars e <> names (typeDimNames $ unInfo $ expandedType t)
  LetPat pat e1 e2 _ _ -> freeVars e1 <> ((names (patternDimNames pat) <> freeVars e2)
                                          `without` patternVars pat)

  LetFun vn (_, pats, _, _, e1) e2 _ ->
    ((freeVars e1 <> names (foldMap patternDimNames pats))
      `without` foldMap patternVars pats) <>
    (freeVars e2 `without` oneName vn)

  If e1 e2 e3 _ _           -> freeVars e1 <> freeVars e2 <> freeVars e3
  Apply e1 e2 _ _ _         -> freeVars e1 <> freeVars e2
  Negate e _                -> freeVars e
  Lambda pats e0 _ _ _      -> (names (foldMap patternDimNames pats) <> freeVars e0)
                               `without` foldMap patternVars pats
  OpSection{}                 -> mempty
  OpSectionLeft _  _ e _ _ _  -> freeVars e
  OpSectionRight _ _ e _ _ _  -> freeVars e
  ProjectSection{}            -> mempty
  IndexSection idxs _ _       -> foldMap freeDimIndex idxs

  DoLoop sparams pat e1 form e3 _ _ ->
    let (e2fv, e2ident) = formVars form
    in freeVars e1 <> e2fv <>
       (freeVars e3 `without`
        (names (S.fromList sparams) <> patternVars pat <> e2ident))
    where formVars (For v e2) = (freeVars e2, ident v)
          formVars (ForIn p e2)   = (freeVars e2, patternVars p)
          formVars (While e2)     = (freeVars e2, mempty)

  BinOp (qn, _) _ (e1, _) (e2, _) _ _ _ -> oneName (qualLeaf qn) <>
                                           freeVars e1 <> freeVars e2
  Project _ e _ _                -> freeVars e

  LetWith id1 id2 idxs e1 e2 _ _ ->
    ident id2 <> foldMap freeDimIndex idxs <> freeVars e1 <>
    (freeVars e2 `without` ident id1)

  Index e idxs _ _    -> freeVars e  <> foldMap freeDimIndex idxs
  Update e1 idxs e2 _ -> freeVars e1 <> foldMap freeDimIndex idxs <> freeVars e2
  RecordUpdate e1 _ e2 _ _ -> freeVars e1 <> freeVars e2

  Unsafe e _          -> freeVars e
  Assert e1 e2 _ _    -> freeVars e1 <> freeVars e2
  Constr _ es _ _     -> foldMap freeVars es
  Match e cs _ _      -> freeVars e <> foldMap caseFV cs
    where caseFV (CasePat p eCase _) = (names (patternDimNames p) <> freeVars eCase)
                                       `without` patternVars p

freeDimIndex :: DimIndexBase Info VName -> NameSet
freeDimIndex (DimFix e) = freeVars e
freeDimIndex (DimSlice me1 me2 me3) =
  foldMap (foldMap freeVars) [me1, me2, me3]

-- | Extract all the variable names bound in a pattern.
patternVars :: Pattern -> NameSet
patternVars = mconcat . map ident . S.toList . patternIdents

-- | Defunctionalize a top-level value binding. Returns the
-- transformed result as well as an environment that binds the name of
-- the value binding to the static value of the transformed body.  The
-- boolean is true if the function is a 'DynamicFun'.
defuncValBind :: ValBind -> DefM (ValBind, Env, Bool)

-- Eta-expand entry points with a functional return type.
defuncValBind (ValBind entry@Just{} name _ (Info (rettype, retext)) tparams params body _ loc)
  | (rettype_ps, rettype') <- unfoldFunType rettype,
    not $ null rettype_ps = do
      (body_pats, body', _) <- etaExpand body
      -- FIXME: we should also handle non-constant size annotations
      -- here.
      defuncValBind $ ValBind entry name Nothing
        (Info (onlyConstantDims rettype', retext))
        tparams (params <> body_pats) body' Nothing loc
  where onlyConstantDims = first onDim
        onDim (ConstDim x) = ConstDim x
        onDim _            = AnyDim

defuncValBind valbind@(ValBind _ name retdecl (Info (rettype, retext)) tparams params body _ _) = do
  (tparams', params', body', sv) <- defuncLet tparams params body rettype
  let rettype' = anyDimShapeAnnotations $ toStruct $ typeOf body'
  return ( valbind { valBindRetDecl    = retdecl
                   , valBindRetType    = Info (combineTypeShapes rettype rettype',
                                               retext)
                   , valBindTypeParams = tparams'
                   , valBindParams     = params'
                   , valBindBody       = body'
                   }
         , M.singleton name sv
         , case sv of DynamicFun{} -> True
                      _            -> False)

-- | Defunctionalize a list of top-level declarations.
defuncVals :: [ValBind] -> DefM (Seq.Seq ValBind)
defuncVals [] = return mempty
defuncVals (valbind : ds) = do
  ((valbind', env, dyn), defs) <- collectFuns $ defuncValBind valbind
  ds' <- localEnv env $ if dyn
                        then isGlobal (valBindName valbind') $ defuncVals ds
                        else defuncVals ds
  return $ defs <> Seq.singleton valbind' <> ds'

-- | Transform a list of top-level value bindings. May produce new
-- lifted function definitions, which are placed in front of the
-- resulting list of declarations.
transformProg :: MonadFreshNames m => [ValBind] -> m [ValBind]
transformProg decs = modifyNameSource $ \namesrc ->
  let (decs', namesrc', liftedDecs) = runDefM namesrc $ defuncVals decs
  in (toList $ liftedDecs <> decs', namesrc')
