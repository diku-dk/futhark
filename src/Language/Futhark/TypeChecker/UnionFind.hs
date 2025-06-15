module Language.Futhark.TypeChecker.UnionFind
  ( TyVarNode,
    TyVarSol(..),
    makeTyVarNode,
    makeTyParamNode,
    find,
    getSol,
    getKey,
    assignNewSol,
    union,
  )
where

import Control.Monad ( when )
import Control.Monad.ST ( ST )
import Data.STRef
    ( STRef, modifySTRef', newSTRef, readSTRef, writeSTRef )
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

newtype TyVarNode s = Node (STRef s (LinkInfo s)) deriving Eq

data LinkInfo s
  = Link !(TyVarNode s)
  | Repr
      { solution :: {-# UNPACK #-} !TyVarSol
      , key      :: {-# UNPACK #-} !TyVar
      }

-- | Create a fresh node of a type variable and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyVarNode :: TyVar -> TyVarInfo () -> ST s (TyVarNode s)
makeTyVarNode tv constraint = do
  let r = Repr {
      solution = Unsolved constraint
    , key = tv
  }
  Node <$> newSTRef r

-- | Create a fresh node of a type parameter and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyParamNode :: TyVar -> Level -> Liftedness -> Loc -> ST s (TyVarNode s)
makeTyParamNode tv lvl lft loc = do
  let r = Repr {
      solution = Param lvl lft loc
    , key = tv
  }
  Node <$> newSTRef r
  
-- | @find node@ returns the representative of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: TyVarNode s -> ST s (TyVarNode s)
find node@(Node link_ref) = do
  link <- readSTRef link_ref
  case link of
    -- Input node is representative.
    Repr {} -> pure node

    -- Input node's parent is another node.
    Link parent -> do
      repr <- find parent
      -- Performing path compression.
      when (repr /= parent) $ do
        writeSTRef link_ref $ Link repr
      pure repr

-- | Return the solution associated with the argument node's
-- equivalence class.
getSol :: TyVarNode s -> ST s TyVarSol
getSol node = do
  Node ref <- find node
  solution <$> readSTRef ref

-- | Return the name of the representative type variable.
getKey :: TyVarNode s -> ST s TyVar
getKey node = do
  Node ref <- find node
  key <$> readSTRef ref

-- | Assign a new solution/type to the node's equivalence class.
--
-- Precondition: The node is in an equivalence class representing an
-- unsolved/flexible type variable.
assignNewSol :: TyVarNode s -> TyVarSol -> ST s ()
assignNewSol node new_sol = do
  Node ref <- find node
  modifySTRef' ref $ \l ->
    case l of
      Repr {} ->
        l { solution = new_sol }
      _ -> error ""

-- | Join the equivalence classes of the nodes. The resulting equivalence
-- class has the same solution and key as the second argument.
union :: TyVarNode s -> TyVarNode s -> ST s ()
union n1 n2 = do
  Node link_ref1 <- find n1
  root2 <- find n2

  writeSTRef link_ref1 $ Link root2