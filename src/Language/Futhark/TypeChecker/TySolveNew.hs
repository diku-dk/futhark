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

typeError :: Loc -> Notes -> Doc () -> SolveM s ()
typeError loc notes msg =
  throwError $ TypeError loc notes msg

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