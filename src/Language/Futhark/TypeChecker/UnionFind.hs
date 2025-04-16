{-# OPTIONS_GHC -funbox-strict-fields #-}
module Language.Futhark.TypeChecker.UnionFind
  ( TyVarNode,
    TyVarSol(..),
    makeTyVarNode,
    makeTyParamNode,
    find,
    getDescr,
    getKey,
    assignType,
    union
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
--   | Rigid RigidTyVar

-- data RigidTyVar
--   = Solved Type
--   | Param Level Liftedness Loc

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
  , descr  :: {-# UNPACK #-} !TyVarSol
    -- ^ The "type" of the equivalence class.
  , key :: {-# UNPACK #-} !TyVar
    -- ^ The name of the type variable representing the equivalence class.

  --   -- TODO: Should we have this "permanent" level field?
  -- , level :: {-# UNPACK #-} !Level
  --   -- ^ The level of the representative type variable.
  } deriving Eq

-- | Create a fresh node of a type variable and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyVarNode :: TyVar -> TyVarInfo () -> ST s (TyVarNode s)
makeTyVarNode tv constraint = do
  info <- newSTRef (MkInfo {
      weight = 1
    , descr = Unsolved constraint
    , key = tv
  })
  l <- newSTRef $ Repr info
  pure $ Node l

-- | Create a fresh node of a type parameter and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyParamNode :: TyVar -> Level -> Liftedness -> Loc -> ST s (TyVarNode s)
makeTyParamNode tv lvl lft loc = do
  info <- newSTRef (MkInfo {
      weight = 1
    , descr = Param lvl lft loc
    , key = tv
  })
  l <- newSTRef $ Repr info
  pure $ Node l

-- | @find node@ returns the representative of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: TyVarNode s -> ST s (TyVarNode s)
find node@(Node l) = do
  link <- readSTRef l
  case link of
    -- Input node is representative.
    Repr _ -> pure node

    -- Input node's parent is another node.
    Link node'@(Node l') -> do
      node'' <- find node'
      when (node' /= node'') $ do
        -- Input node's parent isn't representative;
        -- performing path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      pure node''

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

-- | Return the descriptor associated with the argument node's
-- equivalence class.
getDescr :: TyVarNode s -> ST s TyVarSol
getDescr node = do
  descr <$> (readSTRef =<< descrRef node)

getKey :: TyVarNode s -> ST s TyVar
getKey node = do
  key <$> (readSTRef =<< descrRef node)

-- TODO: Determine if it should be a precondition that the input
-- TODO: node is in an "unsolved" equivalence class.
-- | Replace the type of the node's equivalence class
-- with the second argument.
assignType :: TyVarNode s -> Type -> ST s ()
assignType node t = do
  ref <- descrRef node
  info <- readSTRef ref
  case descr info of
    Unsolved _ -> modifySTRef ref $ \i -> i { descr = Solved t }
    _          -> pure () -- This would be an error.

-- TODO: Make sure we correctly handle level, liftedness, and if 
-- TODO: type parameters are involved.
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
        (MkInfo w1 _   _) <- readSTRef info_ref1
        (MkInfo w2 sol k) <- readSTRef info_ref2
        if w1 >= w2 then do
          writeSTRef link_ref2 (Link root1)
          writeSTRef info_ref1 (MkInfo (w1 + w2) sol k)
        else do
          writeSTRef link_ref1 (Link root2)
          writeSTRef info_ref2 (MkInfo (w1 + w2) sol k)

      -- This shouldn't be possible.       
      _ -> error "'find' somehow didn't return a Repr"

-- | Return @True@ if both nodes belong to the same
-- equivalence class.
-- equivalent :: TyVarNode s -> TyVarNode s -> ST s Bool
-- equivalent n1 n2 = (==) <$> find n1 <*> find n2