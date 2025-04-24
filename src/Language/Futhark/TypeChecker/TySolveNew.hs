module Language.Futhark.TypeChecker.TySolveNew
  ( Type,
    Solution,
    UnconTyVar,
    solve,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.ST
import Data.Bifunctor
import Data.List qualified as L
import Data.Loc
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Futhark.Util.Pretty
import Language.Futhark
import Language.Futhark.TypeChecker.Constraints
import Language.Futhark.TypeChecker.Error
import Language.Futhark.TypeChecker.Monad (Notes, TypeError (..), aNote)
import Language.Futhark.TypeChecker.Types (substTyVars)
import Language.Futhark.TypeChecker.UnionFind

-- | The type representation used by the constraint solver. Agnostic
-- to sizes and uniqueness.
type Type = CtType ()

type UF s = M.Map TyVar (TyVarNode s)

type Unifier s = (UF s, [CtTy ()])

newtype SolverState s = SolverState { solverTyVars :: UF s }

newtype SolveM s a = SolveM { runSolveM :: StateT (SolverState s) (ExceptT TypeError (ST s)) a }
  deriving (Functor, Applicative, Monad, MonadError TypeError, MonadState (SolverState s))

-- | A solution maps a type variable to its substitution. This
-- substitution is complete, in the sense there are no right-hand
-- sides that contain a type variable.
type Solution = M.Map TyVar (Either [PrimType] (TypeBase () NoUniqueness))

-- | An unconstrained type variable comprises a name and (ironically)
-- a constraint on how it can be instantiated.
type UnconTyVar = (VName, Liftedness)


liftST :: ST s a -> SolveM s a
liftST = SolveM . lift . lift

initialState :: TyParams -> TyVars () -> SolveM s ()
initialState typarams tyvars = do
  tyvars' <- M.traverseWithKey f tyvars
  typarams' <- M.traverseWithKey g typarams
  put $ SolverState $ typarams' <> tyvars'
  where
    f tv (lvl, info) = liftST $ makeTyVarNode tv lvl info
    g tv (lvl, lft, loc) = liftST $ makeTyParamNode tv lvl lft loc

typeError :: Loc -> Notes -> Doc () -> SolveM s ()
typeError loc notes msg =
  throwError $ TypeError loc notes msg

typeVar :: (Monoid u) => VName -> TypeBase dim u
typeVar v = Scalar $ TypeVar mempty (qualName v) []

enrichType :: Type -> SolveM s Type
enrichType t = do
  s <- get
  uf <- convertUF (solverTyVars s)
  pure $ substTyVars (substTyVar uf) t

cannotUnify ::
  Reason Type ->
  Notes ->
  BreadCrumbs ->
  Type ->
  Type ->
  SolveM s ()
cannotUnify reason notes bcs t1 t2 = do
  t1' <- enrichType t1
  t2' <- enrichType t2
  case reason of
    Reason loc ->
      typeError loc notes . stack $
        [ "Cannot unify",
          indent 2 (pretty t1'),
          "with",
          indent 2 (pretty t2')
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonPatMatch loc pat value_t ->
      typeError loc notes . stack $
        [ "Pattern",
          indent 2 $ align $ pretty pat,
          "cannot match value of type",
          indent 2 $ align $ pretty value_t
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonAscription loc expected actual ->
      typeError loc notes . stack $
        [ "Expression does not have expected type from type ascription.",
          "Expected:" <+> align (pretty expected),
          "Actual:  " <+> align (pretty actual)
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonRetType loc expected actual -> do
      expected' <- enrichType expected
      actual' <- enrichType actual
      typeError loc notes . stack $
        [ "Function body does not have expected type.",
          "Expected:" <+> align (pretty expected'),
          "Actual:  " <+> align (pretty actual')
        ]
          <> [pretty bcs | not $ hasNoBreadCrumbs bcs]
    ReasonApply loc f e expected actual -> do
      expected' <- enrichType expected
      actual' <- enrichType actual
      typeError loc notes . stack $
        [ header,
          "Expected:" <+> align (pretty expected'),
          "Actual:  " <+> align (pretty actual')
        ]
      where
        header =
          case f of
            (Nothing, _) ->
              "Cannot apply function to"
                <+> dquotes (shorten $ group $ pretty e)
                <> " (invalid type)."
            (Just fname, _) ->
              "Cannot apply"
                <+> dquotes (pretty fname)
                <+> "to"
                <+> dquotes (align $ shorten $ group $ pretty e)
                <> " (invalid type)."
    ReasonApplySplit loc (fname, 0) _ ftype ->
      typeError loc notes $
        stack
          [ "Cannot apply"
              <+> fname'
              <+> "as function, as it has non-function type:"
              </> indent 2 (align $ pretty ftype)
          ]
      where
        fname' = maybe "expression" (dquotes . pretty) fname
    ReasonApplySplit loc (fname, i) e _ ->
      typeError loc notes $
        stack
          [ "Cannot apply"
              <+> fname'
              <+> "to"
              <+> dquotes (align $ shorten $ group $ pretty e)
              <> ".",
            "Function accepts only" <+> pretty i <+> "arguments."
          ]
      where
        fname' = maybe "expression" (dquotes . pretty) fname
    ReasonBranches loc former latter -> do
      former' <- enrichType former
      latter' <- enrichType latter
      typeError loc notes . stack $
        [ "Branches differ in type.",
          "Former:" <+> pretty former',
          "Latter:" <+> pretty latter'
        ]

unsharedConstructorsMsg :: M.Map Name t -> M.Map Name t -> Doc a
unsharedConstructorsMsg cs1 cs2 =
  "Unshared constructors:" <+> commasep (map (("#" <>) . pretty) missing) <> "."
  where
    missing =
      filter (`notElem` M.keys cs1) (M.keys cs2)
        ++ filter (`notElem` M.keys cs2) (M.keys cs1)

convertUF :: UF s -> SolveM s (M.Map TyVar TyVarSol)
convertUF uf = do
  mappings <- mapM maybeLookupSol (M.toList uf)
  pure $ M.fromList $ catMaybes mappings
  where
    maybeLookupSol :: (TyVar, TyVarNode s) -> SolveM s (Maybe (TyVar, TyVarSol))
    maybeLookupSol (tv, node) = do
      root <- liftST $ find node
      descr <- liftST $ getDescr root
      pure $ case descr of
        t@(Solved _) -> Just (tv, t)
        _ -> Nothing

substTyVar :: (Monoid u) => M.Map TyVar TyVarSol -> VName -> Maybe (TypeBase () u)
substTyVar m v =
  case M.lookup v m of
    Just (Solved t') -> Just $ second (const mempty) $ substTyVars (substTyVar m) t'
    _ -> Nothing

occursCheck :: Reason Type -> VName -> Type -> SolveM s ()
occursCheck reason v tp = do
  vars <- gets solverTyVars
  vars' <- convertUF vars
  let tp' = substTyVars (substTyVar vars') tp
  when (v `S.member` typeVars tp') . typeError (locOf reason) mempty $
    "Occurs check: cannot instantiate"
      <+> prettyName v
      <+> "with"
      <+> pretty tp
      <> "."

bindTyVar :: Reason Type -> BreadCrumbs -> VName -> Type -> SolveM s ()
bindTyVar reason bcs v t = do
  occursCheck reason v t
  v_node <- lookupUF v

  setInfo v_node (Solved t)

  v_info <- liftST $ getDescr v_node

  case (v_info, t) of
    ( Unsolved TyVarFree {}, _ ) -> pure ()
    ( Unsolved (TyVarPrim _ v_pts), _ ) ->
        if t `elem` map (Scalar . Prim) v_pts
          then pure ()
          else cannotUnify reason notes bcs (typeVar v) t
        where
          notes =
            aNote $
              "Cannot instantiate type that must be one of"
                </> indent 2 (pretty v_pts)
                </> "with"
                </> indent 2 (pretty t)
    ( Unsolved (TyVarSum _ cs1), Scalar (Sum cs2) ) ->
        if all (`elem` M.keys cs2) (M.keys cs1)
          then unifySharedConstructors reason bcs cs1 cs2
          else cannotUnify reason notes bcs (typeVar v) t
        where
          notes =
            aNote $
              "Cannot match type with constructors"
                </> indent 2 (stack (map (("#" <>) . pretty) (M.keys cs1)))
                </> "with type with constructors"
                </> indent 2 (stack (map (("#" <>) . pretty) (M.keys cs2)))
                </> unsharedConstructorsMsg cs1 cs2
    ( Unsolved (TyVarSum _ cs1), _ ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty t)
    ( Unsolved (TyVarRecord _ fs1), Scalar (Record fs2) ) ->
        if all (`elem` M.keys fs2) (M.keys fs1)
          then unifySharedFields reason bcs fs1 fs2
          else
            typeError (locOf reason) mempty $
              "Cannot unify record type with fields"
                </> indent 2 (pretty (Record fs1))
                </> "with record type"
                </> indent 2 (pretty (Record fs2))
    ( Unsolved (TyVarRecord _ fs1), _ ) ->
        typeError (locOf reason) mempty $
          "Cannot unify record type with fields"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty t)
    --
    -- Internal error cases
    (Solved {}, _) ->
      error $ "Type variable already solved: " <> prettyNameString v
    (Param {}, _) ->
      error $ "Cannot substitute type parameter: " <> prettyNameString v
    -- ({}, _) ->
    --   error $ "Type variable already linked: " <> prettyNameString v
    -- (Nothing, _) ->
    --   error $ "subTyVar: Nothing v: " <> prettyNameString v
  
  

solution :: SolverState s -> ([UnconTyVar], Solution)
solution _s = undefined

solveCt :: CtTy () -> SolveM s ()
solveCt (CtEq reason t1 t2) = solveEq reason mempty t1 t2

solveEq :: Reason Type -> BreadCrumbs -> Type -> Type -> SolveM s ()
solveEq _reason _obcs _orig_t1 _orig_t2 = undefined

maybeLookupTyVar :: TyVar -> SolveM s (Maybe TyVarSol)
maybeLookupTyVar tv = do
  tyvars <- gets solverTyVars
  case M.lookup tv tyvars of
    Nothing -> pure Nothing
    Just node -> do
      sol <- liftST $ getDescr node
      pure $ Just sol

lookupTyVar :: TyVar -> SolveM s (Either (TyVarInfo ()) Type)
lookupTyVar tv =
  maybe bad unpack <$> maybeLookupTyVar tv
  where
    bad = error $ "Unknown tyvar: " <> prettyNameString tv
    unpack (Param {}) = error $ "Is a type param: " <> prettyNameString tv
    unpack (Solved t) = Right t
    unpack (Unsolved info) = Left info    

lookupTyVarInfo :: TyVar -> SolveM s (TyVarInfo ())
lookupTyVarInfo v = do
  r <- lookupTyVar v
  case r of
    Left info -> pure info
    Right _ -> error $ "Tyvar is nonflexible: " <> prettyNameString v

lookupUF :: TyVar -> SolveM s (TyVarNode s)
lookupUF tv = do
  uf <- gets solverTyVars
  case M.lookup tv uf of
    Nothing -> error $ "Unknown tyvar: " <> prettyNameString tv
    Just node -> pure node

unifySharedFields ::
  Reason Type ->
  BreadCrumbs ->
  M.Map Name Type ->
  M.Map Name Type ->
  SolveM s ()
unifySharedFields reason bcs fs1 fs2 =
  forM_ (M.toList $ M.intersectionWith (,) fs1 fs2) $ \(f, (ts1, ts2)) ->
    solveEq reason (matchingField f <> bcs) ts1 ts2

unifySharedConstructors ::
  Reason Type ->
  BreadCrumbs ->
  M.Map Name [Type] ->
  M.Map Name [Type] ->
  SolveM s ()
unifySharedConstructors reason bcs cs1 cs2 =
  forM_ (M.toList $ M.intersectionWith (,) cs1 cs2) $ \(c, (ts1, ts2)) ->
    if length ts1 == length ts2
      then zipWithM_ (solveEq reason $ matchingConstructor c <> bcs) ts1 ts2
      else
        typeError (locOf reason) mempty $
          "Cannot unify type with constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts1)))
            </> "with type of constructor"
            </> indent 2 (pretty (Sum (M.singleton c ts2)))
            </> "because they differ in arity."

setInfo :: TyVarNode s -> TyVarSol -> SolveM s ()
setInfo node sol = liftST $ assignNewSol node sol

unionTyVars :: Reason Type -> BreadCrumbs -> VName -> VName -> SolveM s ()
unionTyVars reason bcs v t = do
  v_node <- lookupUF v
  t_node <- lookupUF t

  -- Unify the equivalence classes of v and t.
  liftST $ union v_node t_node

  v_info <- liftST $ getDescr v_node
  t_info <- lookupTyVarInfo t
  case (v_info, t_info) of
    (Unsolved (TyVarFree _ v_l), TyVarFree t_loc t_l) 
      | v_l /= t_l -> 
        setInfo t_node $ Unsolved $ TyVarFree t_loc (min v_l t_l)
    (Unsolved TyVarFree {}, _) -> pure ()
    (Unsolved info, TyVarFree {}) -> do
      setInfo t_node $ Unsolved info
    --
    -- TyVarPrim cases
    ( Unsolved (TyVarPrim _ v_pts),
      TyVarPrim t_loc t_pts
      ) ->
        let pts = L.intersect v_pts t_pts
         in if null pts
              then
                typeError (locOf reason) mempty $
                  "Cannot unify type that must be one of"
                    </> indent 2 (pretty v_pts)
                    </> "with type that must be one of"
                    </> indent 2 (pretty t_pts)
              else setInfo t_node (Unsolved (TyVarPrim t_loc pts))
    ( Unsolved (TyVarPrim _ v_pts),
      TyVarRecord {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be a record."
    ( Unsolved (TyVarPrim _ v_pts),
      TyVarSum {}
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type that must be one of"
            </> indent 2 (pretty v_pts)
            </> "with type that must be sum."
    --
    -- TyVarSum cases
    ( Unsolved (TyVarSum _ cs1),
      TyVarSum loc cs2
      ) -> do
        unifySharedConstructors reason bcs cs1 cs2
        let cs3 = cs1 <> cs2
        setInfo t_node $ Unsolved $ TyVarSum loc cs3
    ( Unsolved TyVarSum {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A sum type cannot be one of"
            </> indent 2 (pretty pts)
    ( Unsolved (TyVarSum _ cs1),
      TyVarRecord _ fs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify type with constructors"
            </> indent 2 (pretty (Sum cs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Record fs)))
    --
    -- TyVarRecord cases
    ( Unsolved (TyVarRecord _ fs1),
      TyVarRecord loc fs2
      ) -> do
        unifySharedFields reason bcs fs1 fs2
        let fs3 = fs1 <> fs2
        setInfo t_node (Unsolved (TyVarRecord loc fs3))
    ( Unsolved TyVarRecord {},
      TyVarPrim _ pts
      ) ->
        typeError (locOf reason) mempty $
          "A record type cannot be one of"
            </> indent 2 (pretty pts)
    ( Unsolved (TyVarRecord _ fs1),
      TyVarSum _ cs
      ) ->
        typeError (locOf reason) mempty $
          "Cannot unify record type"
            </> indent 2 (pretty (Record fs1))
            </> "with type"
            </> indent 2 (pretty (Scalar (Sum cs)))
    --
    -- Internal error cases
    (Solved {}, _) -> alreadySolved
    (Param {}, _) -> isParam

  where
    alreadySolved = error $ "Type variable already solved: " <> prettyNameString v
    isParam = error $ "Type name is a type parameter: " <> prettyNameString v    

solveTyVar :: (VName, (Level, TyVarInfo ())) -> SolveM s (UF s)
solveTyVar _ = gets solverTyVars -- Just a test to see if things typecheck.

-- | Solve type constraints, producing either an error or a solution,
-- alongside a list of unconstrained type variables.
solve ::
  [CtTy ()] ->
  TyParams ->
  TyVars () ->
  Either TypeError ([UnconTyVar], Solution)
solve _constraints _typarams _tyvars = undefined
{-# NOINLINE solve #-}