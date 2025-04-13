{-# OPTIONS_GHC -funbox-strict-fields #-}
module Language.Futhark.TypeChecker.UnionFind 
  ( VarNode(..),
    makeSet,
    find,
    getType,
    getKey,
    assignType,
    union
  ) 
where

import Language.Futhark.TypeChecker.Constraints

import Control.Monad ( when )
import Control.Monad.ST
import Data.STRef

type Type = CtType ()

newtype VarNode s = Node (STRef s (Link s)) deriving Eq

data Link s
    = Repr {-# UNPACK #-} !(STRef s Info)
      -- ^ This is the representative of the equivalence class.
    | Link {-# UNPACK #-} !(VarNode s)
      -- ^ Pointer to some other element of the equivalence class.
     deriving Eq

data Info = MkInfo
  { weight :: {-# UNPACK #-} !Int
    -- ^ The size of the equivalence class, used by 'union'.
  , descr  :: Maybe Type
  , key :: TyVar
  } deriving Eq

-- | Create a fresh node and return it.  A fresh node is in
-- the equivalence class that contains only itself.
makeSet :: TyVar -> ST s (VarNode s)
makeSet tv = do
  info <- newSTRef (MkInfo { weight = 1, descr = Nothing, key = tv})
  l <- newSTRef (Repr info)
  pure (Node l)

-- | @find node@ returns the representative node of
-- @node@'s equivalence class.
--
-- This method performs the path compresssion.
find :: VarNode s -> ST s (VarNode s)
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

-- | Return the reference to the node's equivalence class's
-- descriptor.
descrRef :: VarNode s -> ST s (STRef s Info) 
descrRef node@(Node link_ref) = do
  link <- readSTRef link_ref
  case link of
    Repr info -> pure info
    Link (Node link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Repr info -> pure info
        _ -> descrRef =<< find node

-- | Return the type associated with argument node's
-- equivalence class.
getType :: VarNode s -> ST s (Maybe Type)
getType node = do
  descr <$> (readSTRef =<< descrRef node)

getKey :: VarNode s -> ST s TyVar
getKey node = do
  key <$> (readSTRef =<< descrRef node)

-- | Replace the type of the node's equivalence class
-- with the second argument.
assignType :: VarNode s -> Type -> ST s ()
assignType node t = do
  r <- descrRef node
  modifySTRef r $ \i -> i { descr = Just t }

-- modifyDescriptor :: VarNode s -> (Type -> Type) -> ST s ()
-- modifyDescriptor node f = do
--   r <- descrRef node
--   modifySTRef r $ \i -> i { descr = f (descr i) }

-- | Join the equivalence classes of the nodes.
union :: VarNode s -> VarNode s -> ST s ()
union n1 n2 = do
  node1@(Node link_ref1) <- find n1
  node2@(Node link_ref2) <- find n2

  -- Ensure that nodes aren't in the same equivalence class. 
  when (node1 /= node2) $ do
    link1 <- readSTRef link_ref1
    link2 <- readSTRef link_ref2
    case (link1, link2) of
      (Repr info_ref1, Repr info_ref2) -> do
        (MkInfo w1 _ tv1) <- readSTRef info_ref1
        (MkInfo w2 mt2 tv2) <- readSTRef info_ref2
        if w1 >= w2 then do
          writeSTRef link_ref2 (Link n1)
          writeSTRef info_ref1 (MkInfo (w1 + w2) mt2 tv1)
        else do
          writeSTRef link_ref1 (Link node2)
          writeSTRef info_ref2 (MkInfo (w1 + w2) mt2 tv2)

      -- This shouldn't be possible.       
      _ -> error "'find' somehow didn't return a Repr" 

-- | /O(1)/. Return @True@ if both nodes belong to the same
-- | equivalence class.
-- equivalent :: VarNode s -> VarNode s -> ST s Bool
-- equivalent n1 n2 = (==) <$> find n1 <*> find n2