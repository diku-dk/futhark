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

newtype TyVarNode s = Node (STRef s (NodeInfo s)) deriving Eq

data NodeInfo s
  = Link !(TyVarNode s)
  | Repr !ReprInfo

data ReprInfo = ReprInfo 
  { solution :: !TyVarSol
  , key      :: !TyVar
  }

-- | Create a fresh node of a type variable and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyVarNode :: TyVar -> TyVarInfo () -> ST s (TyVarNode s)
makeTyVarNode tv constraint = do
  let r = ReprInfo {
      solution = Unsolved constraint
    , key = tv
  }
  ref <- newSTRef $ Repr r
  pure $ Node ref

-- | Create a fresh node of a type parameter and return it. A fresh node
-- is in the equivalence class that contains only itself.
makeTyParamNode :: TyVar -> Level -> Liftedness -> Loc -> ST s (TyVarNode s)
makeTyParamNode tv lvl lft loc = do
  let r = ReprInfo {
      solution = Param lvl lft loc
    , key = tv
  }
  ref <- newSTRef $ Repr r
  pure $ Node ref
  
-- | @find node@ returns the representative of @node@'s
-- equivalence class and the information associated with
-- this equivalence class.
--
-- This method performs the path compresssion.
find :: TyVarNode s -> ST s (TyVarNode s, ReprInfo)
find node@(Node info_ref) = do
  info <- readSTRef info_ref
  case info of
    -- Input node is representative.
    Repr repr_info -> pure (node, repr_info)

    -- Input node's parent is another node.
    Link parent -> do
      a@(repr, _) <- find parent
      when (repr /= parent) $
        -- Performing path compression.
        writeSTRef info_ref $ Link repr
      pure a

-- | Return the solution associated with the argument node's
-- equivalence class.
getSol :: TyVarNode s -> ST s TyVarSol
getSol node = solution . snd <$> find node

-- | Return the name of the representative type variable.
getKey :: TyVarNode s -> ST s TyVar
getKey node = key . snd <$> find node

-- | Assign a new solution/type to the node's equivalence class.
--
-- Precondition: The node is in an equivalence class representing an
-- unsolved/flexible type variable.
assignNewSol :: TyVarNode s -> TyVarSol -> ST s ()
assignNewSol node new_sol = do
  (Node ref, info) <- find node
  modifySTRef' ref $ const $ Repr $ info { solution = new_sol }

-- | Join the equivalence classes of the nodes. The resulting equivalence
-- class has the same solution and key as the second argument.
union :: TyVarNode s -> TyVarNode s -> ST s ()
union n1 n2 = do
  Node info_ref <- fst <$> find n1
  root2 <- fst <$> find n2

  writeSTRef info_ref $ Link root2