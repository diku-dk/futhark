{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Defunctionalization of typed, monomorphic Futhark programs without modules.
module Futhark.Internalise.Defunctionalise
  ( transformProg ) where

import           Control.Monad.RWS
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.Loc
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import qualified Data.Sequence as Seq

import           Futhark.MonadFreshNames
import           Language.Futhark

-- | A static value stores additional information about the result of
-- defunctionalization of an expression, aside from the residual expression.
data StaticVal = Dynamic CompType
               | LambdaSV [VName] Pattern Exp Env
                 -- ^ The 'VName's are shape parameters that are bound
                 -- by the 'Pattern'.
               | RecordSV [(Name, StaticVal)]
               | DynamicFun (Exp, StaticVal) StaticVal
               | IntrinsicSV
  deriving (Show)

-- | Environment mapping variable names to their associated static value.
type Env = M.Map VName StaticVal

localEnv :: Env -> DefM a -> DefM a
localEnv env = local (env <>)

extendEnv :: VName -> StaticVal -> DefM a -> DefM a
extendEnv vn sv = local (M.insert vn sv)

-- | Returns the defunctionalization environment restricted
-- to the given set of variable names and types.
restrictEnvTo :: NameSet -> DefM Env
restrictEnvTo (NameSet m) = asks restrict
  where restrict = M.mapMaybeWithKey $ \k sv -> do
          u <- M.lookup k m
          Just $ restrict' u sv
        restrict' Nonunique (Dynamic t) =
          Dynamic $ t `setUniqueness`  Nonunique
        restrict' _ (Dynamic t) =
          Dynamic t
        restrict' u (LambdaSV dims pat e env) =
          LambdaSV dims pat e $ M.map (restrict' u) env
        restrict' u (RecordSV fields) =
          RecordSV $ map (fmap $ restrict' u) fields
        restrict' u (DynamicFun (e, sv1) sv2) =
          DynamicFun (e, restrict' u sv1) $ restrict' u sv2
        restrict' _ IntrinsicSV = IntrinsicSV

-- | Defunctionalization monad.
newtype DefM a = DefM (RWS Env (Seq.Seq ValBind) VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadReader Env,
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
  env <- ask
  case M.lookup x env of
    Just sv -> return sv
    Nothing -- If the variable is unknown, it may refer to the 'intrinsics'
            -- module, which we will have to treat specially.
      | baseTag x <= maxIntrinsicTag -> return IntrinsicSV
      | otherwise -> error $ "Variable " ++ pretty x ++ " at "
                          ++ locStr loc ++ " is out of scope."

-- | Defunctionalization of an expression. Returns the residual expression and
-- the associated static value in the defunctionalization monad.
defuncExp :: Exp -> DefM (Exp, StaticVal)

defuncExp e@Literal{} =
  return (e, Dynamic $ typeOf e)

defuncExp e@IntLit{} =
  return (e, Dynamic $ typeOf e)

defuncExp e@FloatLit{} =
  return (e, Dynamic $ typeOf e)

defuncExp (Parens e loc) = do
  (e', sv) <- defuncExp e
  return (Parens e' loc, sv)

defuncExp (QualParens qn e loc) = do
  (e', sv) <- defuncExp e
  return (QualParens qn e' loc, sv)

defuncExp (TupLit es loc) = do
  (es', svs) <- unzip <$> mapM defuncExp es
  return (TupLit es' loc, RecordSV $ zip fields svs)
  where fields = map (nameFromString . show) [(1 :: Int) ..]

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

defuncExp (Range e1 me incl t@(Info t') loc) = do
  e1' <- defuncExp' e1
  me' <- mapM defuncExp' me
  incl' <- mapM defuncExp' incl
  return (Range e1' me' incl' t loc, Dynamic t')

defuncExp e@Empty{} =
  return (e, Dynamic $ typeOf e)

defuncExp (Var qn _ loc) = do
  sv <- lookupVar loc (qualLeaf qn)
  case sv of
    -- If the variable refers to a dynamic function, we return its closure
    -- representation (i.e., a record expression capturing the free variables
    -- and a 'LambdaSV' static value) instead of the variable itself.
    DynamicFun closure _ -> return closure
    _ -> let tp = typeFromSV sv
         in return (Var qn (Info (vacuousShapeAnnotations tp)) loc, sv)

defuncExp (Ascript e0 tydecl loc)
  | orderZero (typeOf e0) = do (e0', sv) <- defuncExp e0
                               return (Ascript e0' tydecl loc, sv)
  | otherwise = defuncExp e0

defuncExp (LetPat tparams pat e1 e2 loc) = do
  let env_dim = envFromShapeParams tparams
  (e1', sv1) <- localEnv env_dim $ defuncExp e1
  let env  = matchPatternSV pat sv1
      pat' = updatePattern pat sv1
  (e2', sv2) <- localEnv (env <> env_dim) $ defuncExp e2
  return (LetPat tparams pat' e1' e2' loc, sv2)

defuncExp (LetFun vn (dims, pats, _, rettype@(Info ret), e1) e2 loc) = do
  let env_dim = envFromShapeParams dims
  (pats', e1', sv1) <- localEnv env_dim $ defuncLet dims pats e1 rettype
  (e2', sv2) <- extendEnv vn sv1 $ defuncExp e2
  case pats' of
    []  -> let t1 = combineTypeShapes (fromStruct ret) $
                    vacuousShapeAnnotations $ typeOf e1'
           in return (LetPat dims (Id vn (Info t1) noLoc) e1' e2' loc, sv2)
    _:_ -> let t1 = combineTypeShapes ret $
                    vacuousShapeAnnotations . toStruct $ typeOf e1'
           in return (LetFun vn (dims, pats', Nothing, Info t1, e1') e2' loc, sv2)

defuncExp (If e1 e2 e3 tp loc) = do
  (e1', _ ) <- defuncExp e1
  (e2', sv) <- defuncExp e2
  (e3', _ ) <- defuncExp e3
  return (If e1' e2' e3' tp loc, sv)

defuncExp e@Apply{} = defuncApply 0 e

defuncExp (Negate e0 loc) = do
  (e0', sv) <- defuncExp e0
  return (Negate e0' loc, sv)

defuncExp e@(Lambda tparams pats e0 decl tp loc) = do
  when (any isTypeParam tparams) $
    error $ "Received a lambda with type parameters at " ++ locStr loc
         ++ ", but the defunctionalizer expects a monomorphic input program."
  -- Extract the first parameter of the lambda and "push" the
  -- remaining ones (if there are any) into the body of the lambda.
  let (dims, pat, e0') = case pats of
        [] -> error "Received a lambda with no parameters."
        [pat'] -> (map typeParamName tparams, pat', e0)
        (pat' : pats') ->
          -- Split shape parameters into those that are determined by
          -- the first pattern, and those that are determined by later
          -- patterns.
          let bound_by_pat = (`S.member` patternDimNames pat') . typeParamName
              (pat_dims, rest_dims) = partition bound_by_pat tparams
          in (map typeParamName pat_dims, pat',
              Lambda rest_dims pats' e0 decl tp loc)

  -- Construct a record literal that closes over the environment of
  -- the lambda.  Closed-over 'DynamicFun's are converted to their
  -- closure representation.
  env <- restrictEnvTo (freeVars e)
  let (fields, env') = unzip $ map closureFromDynamicFun $ M.toList env
  return (RecordLit fields loc, LambdaSV dims pat e0' $ M.fromList env')

  where closureFromDynamicFun (vn, DynamicFun (clsr_env, sv) _) =
          let name = nameFromString $ pretty vn
          in (RecordFieldExplicit name clsr_env noLoc, (vn, sv))

        closureFromDynamicFun (vn, sv) =
          let name = nameFromString $ pretty vn
              tp' = vacuousShapeAnnotations $ typeFromSV sv
          in (RecordFieldExplicit name
               (Var (qualName vn) (Info tp') noLoc) noLoc, (vn, sv))

-- Operator sections are expected to be converted to lambda-expressions
-- by the monomorphizer, so they should no longer occur at this point.
defuncExp OpSection{}      = error "defuncExp: unexpected operator section."
defuncExp OpSectionLeft{}  = error "defuncExp: unexpected operator section."
defuncExp OpSectionRight{} = error "defuncExp: unexpected operator section."

defuncExp (DoLoop tparams pat e1 form e3 loc) = do
  let env_dim = envFromShapeParams tparams
  (e1', sv1) <- defuncExp e1
  let env1 = matchPatternSV pat sv1
  (form', env2) <- case form of
    For v e2      -> do e2' <- defuncExp' e2
                        return (For v e2', envFromIdent v)
    ForIn pat2 e2 -> do e2' <- defuncExp' e2
                        return (ForIn pat2 e2', envFromPattern pat2)
    While e2      -> do e2' <- localEnv (env1 <> env_dim) $ defuncExp' e2
                        return (While e2', mempty)
  (e3', sv) <- localEnv (env1 <> env2 <> env_dim) $ defuncExp e3
  return (DoLoop tparams pat e1' form' e3' loc, sv)
  where envFromIdent (Ident vn (Info tp) _) = M.singleton vn $ Dynamic tp

-- We handle BinOps by turning them into ordinary function applications.
defuncExp (BinOp qn (Info t) (e1, Info pt1) (e2, Info pt2) (Info ret) loc) =
  defuncExp $ Apply (Apply (Var qn (Info t) loc)
                     e1 (Info (diet pt1)) (Info (Arrow mempty Nothing (fromStruct pt2) ret)) loc)
                    e2 (Info (diet pt2)) (Info ret) loc

defuncExp (Project vn e0 tp@(Info tp') loc) = do
  (e0', sv0) <- defuncExp e0
  case sv0 of
    RecordSV svs -> case lookup vn svs of
      Just sv -> return (Project vn e0' (Info $ typeFromSV sv) loc, sv)
      Nothing -> error "Invalid record projection."
    Dynamic _ -> return (Project vn e0' tp loc, Dynamic tp')
    _ -> error $ "Projection of an expression with static value " ++ show sv0

defuncExp (LetWith id1 id2 idxs e1 body loc) = do
  e1' <- defuncExp' e1
  sv1 <- lookupVar (identSrcLoc id2) $ identName id2
  idxs' <- mapM defuncDimIndex idxs
  (body', sv) <- extendEnv (identName id1) sv1 $ defuncExp body
  return (LetWith id1 id2 idxs' e1' body' loc, sv)

defuncExp expr@(Index e0 idxs info loc) = do
  e0' <- defuncExp' e0
  idxs' <- mapM defuncDimIndex idxs
  return (Index e0' idxs' info loc, Dynamic $ typeOf expr)

defuncExp (Update e1 idxs e2 loc) = do
  (e1', sv) <- defuncExp e1
  idxs' <- mapM defuncDimIndex idxs
  e2' <- defuncExp' e2
  return (Update e1' idxs' e2' loc, sv)

defuncExp e@(Concat i e1 e2 loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (Concat i e1' e2' loc, Dynamic $ typeOf e)

defuncExp e@(Reshape e1 e2 info loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (Reshape e1' e2' info loc, Dynamic $ typeOf e)

defuncExp e@(Rearrange is e0 loc) = do
  e0' <- defuncExp' e0
  return (Rearrange is e0' loc, Dynamic $ typeOf e)

defuncExp e@(Rotate i e1 e2 loc) = do
  e1' <- defuncExp' e1
  e2' <- defuncExp' e2
  return (Rotate i e1' e2' loc, Dynamic $ typeOf e)

defuncExp e@(Map fun arr t loc) = do
  fun' <- defuncSoacExp fun
  arr' <- defuncExp' arr
  return (Map fun' arr' t loc, Dynamic $ typeOf e)

defuncExp e@(Reduce comm fun ne arr loc) = do
  fun' <- defuncSoacExp fun
  ne' <- defuncExp' ne
  arr' <- defuncExp' arr
  return (Reduce comm fun' ne' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Scan fun ne arr loc) =
  (,) <$> (Scan <$> defuncSoacExp fun <*> defuncExp' ne <*> defuncExp' arr
                <*> pure loc)
      <*> pure (Dynamic $ typeOf e)

defuncExp e@(Filter fun arr loc) = do
  fun' <- defuncSoacExp fun
  arr' <- defuncExp' arr
  return (Filter fun' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Partition k fun arr loc) = do
  fun' <- defuncSoacExp fun
  arr' <- defuncExp' arr
  return (Partition k fun' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Stream form lam arr loc) = do
  form' <- case form of
             MapLike _          -> return form
             RedLike so comm e' -> RedLike so comm <$> defuncSoacExp e'
  lam' <- defuncSoacExp lam
  arr' <- defuncExp' arr
  return (Stream form' lam' arr' loc, Dynamic $ typeOf e)

defuncExp e@(Zip i e1 es t loc) = do
  e1' <- defuncExp' e1
  es' <- mapM defuncExp' es
  return (Zip i e1' es' t loc, Dynamic $ typeOf e)

defuncExp e@(Unzip e0 tps loc) = do
  e0' <- defuncExp' e0
  return (Unzip e0' tps loc, Dynamic $ typeOf e)

defuncExp (Unsafe e1 loc) = do
  (e1', sv) <- defuncExp e1
  return (Unsafe e1' loc, sv)

-- | Same as 'defuncExp', except it ignores the static value.
defuncExp' :: Exp -> DefM Exp
defuncExp' = fmap fst . defuncExp

-- | Defunctionalize the function argument to a SOAC by eta-expanding if
-- necessary and then defunctionalizing the body of the introduced lambda.
defuncSoacExp :: Exp -> DefM Exp
defuncSoacExp e@OpSection{}      = return e
defuncSoacExp e@OpSectionLeft{}  = return e
defuncSoacExp e@OpSectionRight{} = return e

defuncSoacExp (Parens e loc) =
  Parens <$> defuncSoacExp e <*> pure loc

defuncSoacExp (Lambda tparams params e0 decl tp loc) = do
  let env_dim = envFromShapeParams tparams
      env = foldMap envFromPattern params
  e0' <- localEnv (env <> env_dim) $ defuncSoacExp e0
  return $ Lambda tparams params e0' decl tp loc

defuncSoacExp e
  | Arrow{} <- typeOf e = do
      (pats, body, tp) <- etaExpand e
      let env = foldMap envFromPattern pats
      body' <- localEnv env $ defuncExp' body
      return $ Lambda [] pats body' Nothing (Info (mempty, tp)) noLoc
  | otherwise = defuncExp' e

etaExpand :: Exp -> DefM ([Pattern], Exp, StructType)
etaExpand e = do
  let (ps, ret) = getType $ typeOf e
  (pats, vars) <- fmap unzip . forM ps $ \t -> do
    x <- newNameFromString "x"
    let t' = vacuousShapeAnnotations t
    return (Id x (Info t') noLoc,
            Var (qualName x) (Info t') noLoc)
  let ps_st = map vacuousShapeAnnotations ps
      e' = foldl' (\e1 (e2, t2, argtypes) ->
                     Apply e1 e2 (Info $ diet t2)
                     (Info (foldFunType argtypes (vacuousShapeAnnotations ret))) noLoc)
           e $ zip3 vars ps (drop 1 $ tails ps_st)
  return (pats, e', vacuousShapeAnnotations $ toStruct ret)

  where getType (Arrow _ _ t1 t2) =
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
defuncLet :: [TypeParam] -> [Pattern] -> Exp -> Info StructType
          -> DefM ([Pattern], Exp, StaticVal)
defuncLet dims ps@(pat:pats) body (Info rettype)
  | patternOrderZero pat = do
      let env = envFromPattern pat
          bound_by_pat = (`S.member` patternDimNames pat) . typeParamName
          (_pat_dims, rest_dims) = partition bound_by_pat dims
      (pats', body', sv) <- localEnv env $ defuncLet rest_dims pats body (Info rettype)
      closure <- defuncExp $ Lambda dims ps body Nothing (Info (mempty, rettype)) noLoc
      return (pat : pats', body', DynamicFun closure sv)
  | otherwise = do
      (e, sv) <- defuncExp $ Lambda dims ps body Nothing (Info (mempty, rettype)) noLoc
      return ([], e, sv)
defuncLet _ [] body _ = do
  (body', sv) <- defuncExp body
  return ([], body', sv)

-- | Defunctionalize an application expression at a given depth of application.
-- Calls to dynamic (first-order) functions are preserved at much as possible,
-- but a new lifted function is created if a dynamic function is only partially
-- applied.
defuncApply :: Int -> Exp -> DefM (Exp, StaticVal)
defuncApply depth e@(Apply e1 e2 d t@(Info ret) loc) = do
  let (argtypes, _) = unfoldFunType ret
  (e1', sv1) <- defuncApply (depth+1) e1
  (e2', sv2) <- defuncExp e2
  let e' = Apply e1' e2' d t loc
  case sv1 of
    LambdaSV dims pat e0 closure_env -> do
      let env' = matchPatternSV pat sv2
          env_dim = envFromDimNames dims
      (e0', sv) <- local (const $ env' <> closure_env <> env_dim) $ defuncExp e0

      let closure_pat = buildEnvPattern closure_env
          pat' = updatePattern pat sv2

      -- Inline certain trivial lifted functions immediately.  This is
      -- purely an optimisation to avoid having the rest of the
      -- compiler spend a lot if time processing them (they will end
      -- up being inlined later anyway).  We also try to simplify away
      -- some let-bindings, to make the generated code look slightly
      -- more comprehensible.
      --
      -- If you are debugging the defunctionaliser, you may want to
      -- turn this off to see the original structure of the generated
      -- code instead.
      let letPat (RecordPattern [] _) _ pbody = pbody
          letPat (PatternParens pp _) pe pbody = letPat pp pe pbody
          letPat (Id v1 _ _) pe (RecordLit [RecordFieldExplicit f (Var v2 _ _) floc] rloc)
            | v1 == qualLeaf v2 =
                RecordLit [RecordFieldExplicit f pe floc] rloc
          letPat (RecordPattern [(pf,p)] _) (RecordLit [RecordFieldExplicit f pe _] _) pbody
            | pf == f =
                letPat p pe pbody
          letPat pp pe pbody = LetPat [] pp pe pbody noLoc

          inline RecordLit{} = True
          inline TupLit{} = True
          inline (Apply x y _ _ _) = inline x && inline y
          inline (BinOp _ _ (x, _) (y, _) _ _) = inline x && inline y
          inline Var{} = True
          inline Literal{} = True
          inline (LetPat _ _ x y _) = inline x && inline y
          inline Negate{} = True
          inline _ = False
      if inline e0' && null dims
        then return (letPat closure_pat e1' $ letPat pat' e2' e0', sv)
        else do
          -- Lift lambda to top-level function definition.
          let params = [closure_pat, pat']
              rettype = buildRetType closure_env params $ typeOf e0'
          fname <- newNameFromString "lifted"
          liftValDec fname rettype dims params e0'

          let t1 = vacuousShapeAnnotations . toStruct $ typeOf e1'
              t2 = vacuousShapeAnnotations . toStruct $ typeOf e2'
              fname' = qualName fname
          return (Parens (Apply (Apply (Var fname' (Info (Arrow mempty Nothing (fromStruct t1) $
                                                          Arrow mempty Nothing (fromStruct t2) rettype)) loc)
                                 e1' (Info Observe) (Info $ Arrow mempty Nothing (fromStruct t2) rettype) loc)
                          e2' d (Info rettype) loc) noLoc, sv)

    -- If e1 is a dynamic function, we just leave the application in place,
    -- but we update the types since it may be partially applied or return
    -- a higher-order term.
    DynamicFun _ sv ->
      let (argtypes', rettype) = dynamicFunType sv argtypes
      in return (Apply e1' e2' d (Info $ foldFunType argtypes' rettype) loc, sv)

    -- Propagate the 'IntrinsicsSV' until we reach the outermost application,
    -- where we construct a dynamic static value with the appropriate type.
    IntrinsicSV
      | depth == 0 -> return (e', Dynamic $ typeOf e)
      | otherwise  -> return (e', IntrinsicSV)

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
            liftValDec fname rettype dims pats e0
            return (Var (qualName fname)
                    (Info (foldFunType argtypes' rettype)) loc, sv')

      IntrinsicSV -> return (e, IntrinsicSV)

      _ -> return (Var qn (Info (vacuousShapeAnnotations $ typeFromSV sv)) loc, sv)

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
liftDynFun (DynamicFun clsr@(_, LambdaSV dims pat _ _) sv) d
  | d > 0 =  let (dims', pats, e', sv') = liftDynFun sv (d-1)
             in (dims ++ dims', pat : pats, e', DynamicFun clsr sv')
liftDynFun sv _ = error $ "Tried to lift a StaticVal " ++ show sv
                       ++ ", but expected a dynamic function."

-- | Converts a pattern to an environment that binds the individual names of the
-- pattern to their corresponding types wrapped in a 'Dynamic' static value.
envFromPattern :: Pattern -> Env
envFromPattern pat = case pat of
  TuplePattern ps _       -> foldMap envFromPattern ps
  RecordPattern fs _      -> foldMap (envFromPattern . snd) fs
  PatternParens p _       -> envFromPattern p
  Id vn (Info t) _        -> M.singleton vn $ Dynamic $ removeShapeAnnotations t
  Wildcard _ _            -> mempty
  PatternAscription p _ _ -> envFromPattern p

-- | Create an environment that binds the shape parameters.
envFromShapeParams :: [TypeParamBase VName] -> Env
envFromShapeParams = envFromDimNames . map dim
  where dim (TypeParamDim vn _) = vn
        dim tparam = error $
          "The defunctionalizer expects a monomorphic input program,\n" ++
          "but it received a type parameter " ++ pretty tparam ++
          " at " ++ locStr (srclocOf tparam) ++ "."

envFromDimNames :: [VName] -> Env
envFromDimNames = M.fromList . flip zip (repeat $ Dynamic $ Prim $ Signed Int32)

-- | Create a new top-level value declaration with the given function name,
-- return type, list of parameters, and body expression.
liftValDec :: VName -> PatternType -> [VName] -> [Pattern] -> Exp -> DefM ()
liftValDec fname rettype dims pats body = tell $ Seq.singleton dec
  where dims' = map (flip TypeParamDim noLoc) dims
        rettype_st = vacuousShapeAnnotations $ toStruct rettype
        dec = ValBind
          { valBindEntryPoint = False
          , valBindName       = fname
          , valBindRetDecl    = Nothing
          , valBindRetType    = Info rettype_st
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
  where buildField (vn, sv) = let tp = vacuousShapeAnnotations (typeFromSV sv)
                              in (nameFromString (pretty vn),
                                  Id vn (Info tp) noLoc)

-- | Given a closure environment pattern and the type of a term,
-- construct the type of that term, where uniqueness is set to
-- `Nonunique` for those arrays that are bound in the environment or
-- pattern (except if they are unique there).  This ensures that a
-- lifted function can create unique arrays as long as they do not
-- alias any of its parameters.  XXX: it is not clear that this is a
-- sufficient property, unfortunately.
buildRetType :: Env -> [Pattern] -> CompType -> PatternType
buildRetType env pats = vacuousShapeAnnotations . descend
  where bound = foldMap oneName (M.keys env) <> foldMap patternVars pats
        boundAsUnique v =
          maybe False (unique . unInfo . identType) $
          find ((==v) . identName) $ S.toList $ foldMap patIdentSet pats
        problematic v = (v `member` bound) && not (boundAsUnique v)
        descend t@Array{}
          | any problematic (aliases t) = t `setUniqueness` Nonunique
        descend (Record t) = Record $ fmap descend t
        descend t = t

-- | Compute the corresponding type for a given static value.
typeFromSV :: StaticVal -> CompType
typeFromSV (Dynamic tp)           = tp
typeFromSV (LambdaSV _ _ _ env)   = typeFromEnv env
typeFromSV (RecordSV ls)          = Record $ M.fromList $ map (fmap typeFromSV) ls
typeFromSV (DynamicFun (_, sv) _) = typeFromSV sv
typeFromSV IntrinsicSV            = error $ "Tried to get the type from the "
                                         ++ "static value of an intrinsic."

typeFromEnv :: Env -> CompType
typeFromEnv = Record . M.fromList .
              map (bimap (nameFromString . pretty) typeFromSV) . M.toList

-- | Construct the type for a fully-applied dynamic function from its
-- static value and the original types of its arguments.
dynamicFunType :: StaticVal -> [PatternType] -> ([PatternType], PatternType)
dynamicFunType (DynamicFun _ sv) (p:ps) =
  let (ps', ret) = dynamicFunType sv ps in (p : ps', ret)
dynamicFunType sv _ = ([], vacuousShapeAnnotations $ typeFromSV sv)

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
matchPatternSV (Id vn _ _) sv = M.singleton vn sv
matchPatternSV (Wildcard _ _) _ = mempty
matchPatternSV (PatternAscription pat _ _) sv = matchPatternSV pat sv
matchPatternSV pat (Dynamic t) = matchPatternSV pat $ svFromType t
matchPatternSV pat sv = error $ "Tried to match pattern " ++ pretty pat
                             ++ " with static value " ++ show sv ++ "."

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
  | otherwise = Id vn (Info . vacuousShapeAnnotations $
                       typeFromSV sv `setUniqueness` Nonunique) loc
updatePattern pat@(Wildcard (Info tp) loc) sv
  | orderZero tp = pat
  | otherwise = Wildcard (Info . vacuousShapeAnnotations $ typeFromSV sv) loc
updatePattern (PatternAscription pat tydecl loc) sv
  | orderZero . unInfo $ expandedType tydecl =
      PatternAscription (updatePattern pat sv) tydecl loc
  | otherwise = updatePattern pat sv
updatePattern pat (Dynamic t) = updatePattern pat (svFromType t)
updatePattern pat sv =
  error $ "Tried to update pattern " ++ pretty pat
       ++ "to reflect the static value " ++ show sv

-- | Convert a record (or tuple) type to a record static value. This is used for
-- "unwrapping" tuples and records that are nested in 'Dynamic' static values.
svFromType :: CompType -> StaticVal
svFromType (Record fs) = RecordSV . M.toList $ M.map svFromType fs
svFromType t           = Dynamic t

-- A set of names where we also track uniqueness.
newtype NameSet = NameSet (M.Map VName Uniqueness)

instance Sem.Semigroup NameSet where
  NameSet x <> NameSet y = NameSet $ M.unionWith max x y

instance Monoid NameSet where
  mempty = NameSet mempty
  mappend = (Sem.<>)

without :: NameSet -> NameSet -> NameSet
without (NameSet x) (NameSet y) = NameSet $ x `M.difference` y

member :: VName -> NameSet -> Bool
member v (NameSet m) = v `M.member` m

ident :: Ident -> NameSet
ident v = NameSet $ M.singleton (identName v) (uniqueness $ unInfo $ identType v)

oneName :: VName -> NameSet
oneName v = NameSet $ M.singleton v Nonunique

names :: Names -> NameSet
names = foldMap oneName

-- | Compute the set of free variables of an expression.
freeVars :: Exp -> NameSet
freeVars expr = case expr of
  Literal{}            -> mempty
  IntLit{}             -> mempty
  FloatLit{}           -> mempty
  Parens e _           -> freeVars e
  QualParens _ e _     -> freeVars e
  TupLit es _          -> foldMap freeVars es

  RecordLit fs _       -> foldMap freeVarsField fs
    where freeVarsField (RecordFieldExplicit _ e _)  = freeVars e
          freeVarsField (RecordFieldImplicit vn t _) = ident $ Ident vn t noLoc

  ArrayLit es _ _      -> foldMap freeVars es
  Range e me incl _ _  -> freeVars e <> foldMap freeVars me <>
                          foldMap freeVars incl
  Empty t _ _          -> names $ foldMap dimName $ nestedDims $ unInfo $ expandedType t
  Var qn (Info t) _    -> NameSet $ M.singleton (qualLeaf qn) $ uniqueness t
  Ascript e t _        -> freeVars e <> names (foldMap dimName $ nestedDims $ unInfo $ expandedType t)
  LetPat _ pat e1 e2 _ -> freeVars e1 <> ((names (patternDimNames pat) <> freeVars e2)
                                          `without` patternVars pat)

  LetFun vn (_, pats, _, _, e1) e2 _ ->
    ((freeVars e1 <> names (foldMap patternDimNames pats))
      `without` foldMap patternVars pats) <>
    (freeVars e2 `without` oneName vn)

  If e1 e2 e3 _ _           -> freeVars e1 <> freeVars e2 <> freeVars e3
  Apply e1 e2 _ _ _         -> freeVars e1 <> freeVars e2
  Negate e _                -> freeVars e
  Lambda tps pats e0 _ _ _  -> (names (foldMap patternDimNames pats) <> freeVars e0)
                               `without` (foldMap patternVars pats <>
                                          mconcat (map (oneName . typeParamName) tps))
  OpSection{}                 -> mempty
  OpSectionLeft _  _ e _ _ _  -> freeVars e
  OpSectionRight _ _ e _ _ _  -> freeVars e

  DoLoop _ pat e1 form e3 _ -> let (e2fv, e2ident) = formVars form
                               in freeVars e1 <> e2fv <>
                               (freeVars e3 `without` (patternVars pat <> e2ident))
    where formVars (For v e2) = (freeVars e2, ident v)
          formVars (ForIn p e2)   = (freeVars e2, patternVars p)
          formVars (While e2)     = (freeVars e2, mempty)

  BinOp qn _ (e1, _) (e2, _) _ _ -> oneName (qualLeaf qn) <>
                                    freeVars e1 <> freeVars e2
  Project _ e _ _                -> freeVars e

  LetWith id1 id2 idxs e1 e2 _ ->
    ident id2 <> foldMap freeDimIndex idxs <> freeVars e1 <>
    (freeVars e2 `without` ident id1)

  Index e idxs _ _    -> freeVars e  <> foldMap freeDimIndex idxs
  Update e1 idxs e2 _ -> freeVars e1 <> foldMap freeDimIndex idxs <> freeVars e2
  Concat _ e1 e2 _    -> freeVars e1 <> freeVars e2
  Reshape e1 e2 _ _   -> freeVars e1 <> freeVars e2
  Rearrange _ e _     -> freeVars e
  Rotate _ e1 e2 _    -> freeVars e1 <> freeVars e2

  Map e1 e2 _ _       -> freeVars e1 <> freeVars e2
  Reduce _ e1 e2 e3 _ -> freeVars e1 <> freeVars e2 <> freeVars e3
  Scan e1 e2 e3 _     -> freeVars e1 <> freeVars e2 <> freeVars e3
  Filter e1 e2 _      -> freeVars e1 <> freeVars e2
  Partition _ e1 e2 _ -> freeVars e1 <> freeVars e2
  Stream form e1 e2 _ -> freeInForm form <> freeVars e1 <> freeVars e2
    where freeInForm (RedLike _ _ e) = freeVars e
          freeInForm _ = mempty

  Zip _ e es _ _      -> freeVars e <> foldMap freeVars es
  Unzip e _ _         -> freeVars e
  Unsafe e _          -> freeVars e

freeDimIndex :: DimIndexBase Info VName -> NameSet
freeDimIndex (DimFix e) = freeVars e
freeDimIndex (DimSlice me1 me2 me3) =
  foldMap (foldMap freeVars) [me1, me2, me3]

-- | Extract all the variable names bound in a pattern.
patternVars :: Pattern -> NameSet
patternVars = mconcat . map ident . S.toList . patIdentSet

-- | Combine the shape information of types as much as possible. The first
-- argument is the orignal type and the second is the type of the transformed
-- expression. This is necessary since the original type may contain additional
-- information (e.g., shape restrictions) from the user given annotation.
combineTypeShapes :: ArrayDim dim =>
                     TypeBase dim as -> TypeBase dim as -> TypeBase dim as
combineTypeShapes (Record ts1) (Record ts2)
  | M.keys ts1 == M.keys ts2 =
  Record $ M.map (uncurry combineTypeShapes) (M.intersectionWith (,) ts1 ts2)
combineTypeShapes (Array et1 shape1 u1) (Array et2 shape2 _u2)
  | Just new_shape <- unifyShapes shape1 shape2 =
      Array (combineElemTypeInfo et1 et2) new_shape u1
combineTypeShapes _ new_tp = new_tp

combineElemTypeInfo :: ArrayDim dim =>
                       ArrayElemTypeBase dim as
                    -> ArrayElemTypeBase dim as -> ArrayElemTypeBase dim as
combineElemTypeInfo (ArrayRecordElem et1) (ArrayRecordElem et2) =
  ArrayRecordElem $ M.map (uncurry combineRecordArrayTypeInfo)
                          (M.intersectionWith (,) et1 et2)
combineElemTypeInfo _ new_tp = new_tp

combineRecordArrayTypeInfo :: ArrayDim dim =>
                              RecordArrayElemTypeBase dim as
                           -> RecordArrayElemTypeBase dim as
                           -> RecordArrayElemTypeBase dim as
combineRecordArrayTypeInfo (RecordArrayElem et1) (RecordArrayElem et2) =
  RecordArrayElem $ combineElemTypeInfo et1 et2
combineRecordArrayTypeInfo (RecordArrayArrayElem et1 shape1 u1)
                           (RecordArrayArrayElem et2 shape2 u2)
  | Just new_shape <- unifyShapes shape1 shape2 =
      RecordArrayArrayElem (combineElemTypeInfo et1 et2) new_shape (u1 <> u2)
combineRecordArrayTypeInfo _ new_tp = new_tp

-- | Extract all the shape names that occur in a given pattern.
patternDimNames :: Pattern -> Names
patternDimNames (TuplePattern ps _)    = foldMap patternDimNames ps
patternDimNames (RecordPattern fs _)   = foldMap (patternDimNames . snd) fs
patternDimNames (PatternParens p _)    = patternDimNames p
patternDimNames (Id _ (Info tp) _)     = foldMap dimName $ nestedDims tp
patternDimNames (Wildcard (Info tp) _) = foldMap dimName $ nestedDims tp
patternDimNames (PatternAscription p (TypeDecl _ (Info t)) _) =
  patternDimNames p <> foldMap dimName (nestedDims t)

dimName :: DimDecl VName -> Names
dimName (NamedDim qn) = S.singleton $ qualLeaf qn
dimName _             = mempty

-- | Defunctionalize a top-level value binding. Returns the transformed result
-- as well as an environment that binds the name of the value binding to the
-- static value of the transformed body.
defuncValBind :: ValBind -> DefM (ValBind, Env)

-- Eta-expand entry points with a functional return type.
defuncValBind (ValBind True name _ (Info rettype) tparams params body _ loc)
  | (rettype_ps, rettype') <- unfoldFunType rettype,
    not $ null rettype_ps = do
      (body_pats, body', _) <- etaExpand body
      defuncValBind $ ValBind True name Nothing (Info rettype')
        tparams (params <> body_pats) body' Nothing loc

defuncValBind valbind@(ValBind _ name _ rettype tparams params body _ _) = do
  let env = envFromShapeParams tparams
  (params', body', sv) <- localEnv env $ defuncLet tparams params body rettype
  -- Remove any shape parameters that no longer occur in the value parameters.
  let dim_names = foldMap patternDimNames params'
      tparams' = filter ((`S.member` dim_names) . typeParamName) tparams

  let rettype' = vacuousShapeAnnotations . toStruct $ typeOf body'
  return ( valbind { valBindRetDecl    = Nothing
                   , valBindRetType    = Info $ combineTypeShapes
                                         (unInfo rettype) rettype'
                   , valBindTypeParams = tparams'
                   , valBindParams     = params'
                   , valBindBody       = body'
                   }
         , M.singleton name sv)

-- | Defunctionalize a list of top-level declarations.
defuncVals :: [ValBind] -> DefM (Seq.Seq ValBind)
defuncVals [] = return mempty
defuncVals (valbind : ds) = do
  ((valbind', env), defs) <- collectFuns $ defuncValBind valbind
  ds' <- localEnv env $ defuncVals ds
  return $ defs <> Seq.singleton valbind' <> ds'

-- | Transform a list of top-level value bindings. May produce new
-- lifted function definitions, which are placed in front of the
-- resulting list of declarations.
transformProg :: MonadFreshNames m => [ValBind] -> m [ValBind]
transformProg decs = modifyNameSource $ \namesrc ->
  let (decs', namesrc', liftedDecs) = runDefM namesrc $ defuncVals decs
  in (toList $ liftedDecs <> decs', namesrc')
