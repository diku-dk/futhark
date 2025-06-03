{-# OPTIONS_GHC -funbox-strict-fields #-}
module Language.Futhark.TypeChecker.UnionFind
  ( TyVarNode,
    TyVarSol(..),
    makeTyVarNode,
    makeTyParamNode,
    find,
    getSol,
    getKey,
    getLvl,
    assignNewSol,
    union,
  )
where

import Control.Monad ( when )
import Control.Monad.ST ( ST )
import Data.STRef
    ( STRef, modifySTRef, newSTRef, readSTRef, writeSTRef )
import Language.Futhark ( Loc, Liftedness )
import Language.Futhark.TypeChecker.Constraints
    ( CtType, Level, TyVar, TyVarInfo )

type Type = CtType ()

-- | A (partial) solution for a type variable.
data TyVarSol
  = Solved Type
    -- ^ Has been assigned this type.
  | Param Level Liftedness Loc
    -- ^ Is an explicit (rigid) type parameter in the source program.
  | Unsolved (TyVarInfo ())
    -- ^ Is unsolved but has this constraint.
  deriving (Show, Eq)

newtype TyVarNode s = Node (STRef s (Link s)) deriving Eq

-- TODO: Determine if this is a more suitable representation.
-- data TyVarKind
--   = Flexible (TyVarInfo ())
--     -- ^ A flexible type variable.
--   | Rigid RigidTyVar
--     -- ^ A rigid type variable: either already assigned a type or is 
--     -- an explicit type parameter in the source program.

-- data RigidTyVar
--   = TyVarSol Type
--   | TyVarParam Level Liftedness Loc

data Link s
  = Repr {-# UNPACK #-} !(STRef s ReprInfo)
    -- ^ The representative of an equivalence class.
  | Link {-# UNPACK #-} !(TyVarNode s)
    -- ^ Pointer to some other element of the equivalence class.
    deriving Eq

-- | Information about an equivalence class.
data ReprInfo = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'. 
  , solution  :: TyVarSol
    -- ^ The "type" of the equivalence class.
  , key :: TyVar
    -- ^ The name of the type variable representing the equivalence class.

  , level :: {-# UNPACK #-} !Level
  --   -- ^ The level of the representative type variable.
  } deriving Eq

-- | Create a fresh node of a type variable and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyVarNode :: TyVar -> Level -> TyVarInfo () -> ST s (TyVarNode s)
makeTyVarNode tv lvl constraint = do
  info <- newSTRef (MkInfo {
      weight = 1
    , solution = Unsolved constraint
    , key = tv
    , level = lvl
  })
  l <- newSTRef $ Repr info
  pure $ Node l

-- | Create a fresh node of a type parameter and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyParamNode :: TyVar -> Level -> Liftedness -> Loc -> ST s (TyVarNode s)
makeTyParamNode tv lvl lft loc = do
  info <- newSTRef (MkInfo {
      weight = 1
    , solution = Param lvl lft loc
    , key = tv
    , level = lvl
  })
  l <- newSTRef $ Repr info
  pure $ Node l

-- | @find node@ returns the representative of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: TyVarNode s -> ST s (TyVarNode s)
find node@(Node link_ref) = do
  link <- readSTRef link_ref
  case link of
    Repr _ -> pure node
    Link parent -> do
      repr <- find parent
      writeSTRef link_ref $ Link repr
      pure repr

-- | Return the reference to the descriptor of the node's
-- equivalence class.
descrRef :: TyVarNode s -> ST s (STRef s ReprInfo)
descrRef node@(Node link_ref) = do
  link <- readSTRef link_ref
  case link of
    Repr info -> pure info
    Link (Node link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Repr info -> pure info
        _ -> descrRef =<< find node

-- | Return the solution associated with the argument node's
-- equivalence class.
getSol :: TyVarNode s -> ST s TyVarSol
getSol node = do
  solution <$> (readSTRef =<< descrRef node)

-- | Return the name of the representative type variable.
getKey :: TyVarNode s -> ST s TyVar
getKey node = do
  key <$> (readSTRef =<< descrRef node)

getLvl :: TyVarNode s -> ST s Level
getLvl node = do
  level <$> (readSTRef =<< descrRef node)

-- | Assign a new solution/type to the node's equivalence class.
--
-- Precondition: The node is in an equivalence class representing an
-- unsolved/flexible type variable.
assignNewSol :: TyVarNode s -> TyVarSol -> ST s ()
assignNewSol node new_sol = do
  ref <- descrRef node
  modifySTRef ref $ \i -> i { solution = new_sol }  

-- | Join the equivalence classes of the nodes. The resulting equivalence
-- class has the same solution and key as the second argument.
union :: TyVarNode s -> TyVarNode s -> ST s ()
union n1 n2 = do
  root1@(Node link_ref1) <- find n1
  root2@(Node link_ref2) <- find n2

  -- Ensure that nodes aren't in the same equivalence class. 
  when (root1 /= root2) $ do
    link1 <- readSTRef link_ref1
    link2 <- readSTRef link_ref2
    case (link1, link2) of
      (Repr info_ref1, Repr info_ref2) -> do
        (MkInfo w1 _   _  l1) <- readSTRef info_ref1
        (MkInfo w2 sol k2 l2) <- readSTRef info_ref2
        let min_lvl = min l1 l2
            w' = w1 + w2
        if w1 >= w2 
          then do
            writeSTRef link_ref2 $ Link root1
            writeSTRef info_ref1 $ MkInfo w' sol k2 min_lvl
          else do
            writeSTRef link_ref1 $ Link root2
            writeSTRef info_ref2 $ MkInfo w' sol k2 min_lvl

      -- This shouldn't be possible.       
      _ -> error "'find' somehow didn't return a Repr"
