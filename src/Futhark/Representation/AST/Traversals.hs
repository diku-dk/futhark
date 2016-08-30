-----------------------------------------------------------------------------
-- |
--
-- Functions for generic traversals across Futhark syntax trees.  The
-- motivation for this module came from dissatisfaction with rewriting
-- the same trivial tree recursions for every module.  A possible
-- alternative would be to use normal \"Scrap your
-- boilerplate\"-techniques, but these are rejected for two reasons:
--
--    * They are too slow.
--
--    * More importantly, they do not tell you whether you have missed
--      some cases.
--
-- Instead, this module defines various traversals of the Futhark syntax
-- tree.  The implementation is rather tedious, but the interface is
-- easy to use.
--
-- A traversal of the Futhark syntax tree is expressed as a tuple of
-- functions expressing the operations to be performed on the various
-- types of nodes.
--
-- The "Futhark.Transform.Rename" is a simple example of how to use
-- this facility.
--
-----------------------------------------------------------------------------
module Futhark.Representation.AST.Traversals
  (
  -- * Mapping
    Mapper(..)
  , identityMapper
  , mapBody
  , mapExpM
  , mapExp
  , mapOnType

  -- * Folding
  , Folder(..)
  , foldExpM
  , foldExp
  , identityFolder

  -- * Walking
  , Walker(..)
  , identityWalker
  , walkExpM
  , walkExp
  -- * Simple wrappers
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Traversable

import Prelude

import Futhark.Representation.AST.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper flore tlore m = Mapper {
    mapOnSubExp :: SubExp -> m SubExp
  , mapOnBody :: Body flore -> m (Body tlore)
  , mapOnVName :: VName -> m VName
  , mapOnCertificates :: Certificates -> m Certificates
  , mapOnRetType :: RetType flore -> m (RetType tlore)
  , mapOnFParam :: FParam flore -> m (FParam tlore)
  , mapOnOp :: Op flore -> m (Op tlore)
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper lore lore m
identityMapper = Mapper {
                   mapOnSubExp = return
                 , mapOnBody = return
                 , mapOnVName = return
                 , mapOnCertificates = return
                 , mapOnRetType = return
                 , mapOnFParam = return
                 , mapOnOp = return
                 }

-- | Map across the bindings of a 'Body'.
mapBody :: (Binding lore -> Binding lore) -> Body lore -> Body lore
mapBody f (Body attr bnds res) = Body attr (map f bnds) res

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) =>
           Mapper flore tlore m -> Exp flore -> m (Exp tlore)
mapExpM tv (BasicOp (SubExp se)) =
  BasicOp <$> (SubExp <$> mapOnSubExp tv se)
mapExpM tv (BasicOp (ArrayLit els rowt)) =
  BasicOp <$> (pure ArrayLit <*> mapM (mapOnSubExp tv) els <*>
              mapOnType (mapOnSubExp tv) rowt)
mapExpM tv (BasicOp (BinOp bop x y)) =
  BasicOp <$> (BinOp bop <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (CmpOp op x y)) =
  BasicOp <$> (CmpOp op <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (ConvOp conv x)) =
  BasicOp <$> (ConvOp conv <$> mapOnSubExp tv x)
mapExpM tv (BasicOp (UnOp op x)) =
  BasicOp <$> (UnOp op <$> mapOnSubExp tv x)
mapExpM tv (If c texp fexp ts) =
  pure If <*> mapOnSubExp tv c <*> mapOnBody tv texp <*> mapOnBody tv fexp <*>
       mapM (mapOnExtType tv) ts
mapExpM tv (Apply fname args ret) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*>
    mapOnRetType tv ret
mapExpM tv (BasicOp (Index cs arr slice)) =
  BasicOp <$> (pure Index <*> mapOnCertificates tv cs <*>
              mapOnVName tv arr <*>
              mapM (Data.Traversable.traverse (mapOnSubExp tv)) slice)
mapExpM tv (BasicOp (Iota n x s)) =
  BasicOp <$> (pure Iota <*> mapOnSubExp tv n <*> mapOnSubExp tv x <*> mapOnSubExp tv s)
mapExpM tv (BasicOp (Replicate shape vexp)) =
  BasicOp <$> (Replicate
               <$> (Shape <$> mapM (mapOnSubExp tv) (shapeDims shape))
               <*> mapOnSubExp tv vexp)
mapExpM tv (BasicOp (Scratch t shape)) =
  BasicOp <$> (Scratch t <$> mapM (mapOnSubExp tv) shape)
mapExpM tv (BasicOp (Reshape cs shape arrexp)) =
  BasicOp <$> (pure Reshape <*> mapOnCertificates tv cs <*>
                 mapM (Data.Traversable.traverse (mapOnSubExp tv)) shape <*>
                 mapOnVName tv arrexp)
mapExpM tv (BasicOp (Rearrange cs perm e)) =
  BasicOp <$> (pure Rearrange <*> mapOnCertificates tv cs <*>
                 pure perm <*> mapOnVName tv e)
mapExpM tv (BasicOp (Rotate cs es e)) =
  BasicOp <$> (pure Rotate <*> mapOnCertificates tv cs <*>
              mapM (mapOnSubExp tv) es <*> mapOnVName tv e)
mapExpM tv (BasicOp (Split cs i sizeexps arrexp)) =
  BasicOp <$> (pure Split <*> mapOnCertificates tv cs <*> pure i <*>
              mapM (mapOnSubExp tv) sizeexps <*> mapOnVName tv arrexp)
mapExpM tv (BasicOp (Concat cs i x ys size)) =
  BasicOp <$> (pure Concat <*> mapOnCertificates tv cs <*> pure i <*>
              mapOnVName tv x <*> mapM (mapOnVName tv) ys <*>
              mapOnSubExp tv size)
mapExpM tv (BasicOp (Copy e)) =
  BasicOp <$> (pure Copy <*> mapOnVName tv e)
mapExpM tv (BasicOp (Assert e loc)) =
  BasicOp <$> (pure Assert <*> mapOnSubExp tv e <*> pure loc)
mapExpM tv (BasicOp (Partition cs n flags arr)) =
  BasicOp <$> (pure Partition <*> mapOnCertificates tv cs <*>
              pure n <*>
              mapOnVName tv flags <*>
              mapM (mapOnVName tv) arr)
mapExpM tv (DoLoop ctxmerge valmerge form loopbody) =
  DoLoop <$>
  (zip <$> mapM (mapOnFParam tv) ctxparams <*> mapM (mapOnSubExp tv) ctxinits) <*>
  (zip <$> mapM (mapOnFParam tv) valparams <*> mapM (mapOnSubExp tv) valinits) <*>
  mapOnLoopForm tv form <*>
  mapOnBody tv loopbody
  where (ctxparams,ctxinits) = unzip ctxmerge
        (valparams,valinits) = unzip valmerge
mapExpM tv (Op op) =
  Op <$> mapOnOp tv op

mapOnExtType :: (Monad m, Applicative m) =>
                Mapper flore tlore m -> TypeBase ExtShape u -> m (TypeBase ExtShape u)
mapOnExtType tv (Array bt (ExtShape shape) u) =
  Array bt <$> (ExtShape <$> mapM mapOnExtSize shape) <*>
  return u
  where mapOnExtSize (Ext x)   = return $ Ext x
        mapOnExtSize (Free se) = Free <$> mapOnSubExp tv se
mapOnExtType _ (Prim bt) = return $ Prim bt
mapOnExtType tv (Mem size space) = Mem <$> mapOnSubExp tv size <*> pure space

mapOnLoopForm :: (Monad m, Applicative m) =>
                 Mapper flore tlore m -> LoopForm -> m LoopForm
mapOnLoopForm tv (ForLoop i bound) =
  ForLoop <$> mapOnVName tv i <*> mapOnSubExp tv bound
mapOnLoopForm tv (WhileLoop cond) =
  WhileLoop <$> mapOnVName tv cond

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper flore tlore Identity -> Exp flore -> Exp tlore
mapExp m = runIdentity . mapExpM m

mapOnType :: (Applicative m, Monad m) =>
             (SubExp -> m SubExp) -> Type -> m Type
mapOnType _ (Prim bt) = return $ Prim bt
mapOnType f (Mem size space) = Mem <$> f size <*> pure space
mapOnType f (Array bt shape u) =
  Array bt <$> (Shape <$> mapM f (shapeDims shape)) <*> pure u

-- | Reification of a left-reduction across a syntax tree.
data Folder a lore m = Folder {
    foldOnSubExp :: a -> SubExp -> m a
  , foldOnBody :: a -> Body lore -> m a
  , foldOnBinding :: a -> Binding lore -> m a
  , foldOnVName :: a -> VName -> m a
  , foldOnCertificates :: a -> Certificates -> m a
  , foldOnRetType :: a -> RetType lore -> m a
  , foldOnFParam :: a -> FParam lore -> m a
  , foldOnLParam :: a -> LParam lore -> m a
  , foldOnOp :: a -> Op lore -> m a
  }

-- | A folding operation where the accumulator is returned verbatim.
identityFolder :: Monad m => Folder a lore m
identityFolder = Folder {
                   foldOnSubExp = const . return
                 , foldOnBody = const . return
                 , foldOnBinding = const . return
                 , foldOnVName = const . return
                 , foldOnCertificates = const . return
                 , foldOnRetType = const . return
                 , foldOnFParam = const . return
                 , foldOnLParam = const . return
                 , foldOnOp = const . return
                 }

foldMapper :: Monad m => Folder a lore m -> Mapper lore lore (StateT a m)
foldMapper f = Mapper {
                 mapOnSubExp = wrap foldOnSubExp
               , mapOnBody = wrap foldOnBody
               , mapOnVName = wrap foldOnVName
               , mapOnCertificates = wrap foldOnCertificates
               , mapOnRetType = wrap foldOnRetType
               , mapOnFParam = wrap foldOnFParam
               , mapOnOp = wrap foldOnOp
               }
  where wrap op k = do
          v <- get
          put =<< lift (op f v k)
          return k

-- | Perform a left-reduction across the immediate children of a body.
-- The reduction does not descend recursively into subterms and is
-- done left-to-right.
foldExpM :: (Monad m, Functor m) => Folder a lore m -> a -> Exp lore -> m a
foldExpM f x e = execStateT (mapExpM (foldMapper f) e) x

-- | As 'foldExpM', but in the 'Identity' monad.
foldExp :: Folder a lore Identity -> a -> Exp lore -> a
foldExp m x = runIdentity . foldExpM m x

-- | Express a monad expression on a syntax node.  Each element of
-- this structure expresses the action to be performed on a given
-- child.
data Walker lore m = Walker {
    walkOnSubExp :: SubExp -> m ()
  , walkOnBody :: Body lore -> m ()
  , walkOnBinding :: Binding lore -> m ()
  , walkOnVName :: VName -> m ()
  , walkOnCertificates :: Certificates -> m ()
  , walkOnRetType :: RetType lore -> m ()
  , walkOnFParam :: FParam lore -> m ()
  , walkOnLParam :: LParam lore -> m ()
  , walkOnOp :: Op lore -> m ()
  }

-- | A no-op traversal.
identityWalker :: Monad m => Walker lore m
identityWalker = Walker {
                   walkOnSubExp = const $ return ()
                 , walkOnBody = const $ return ()
                 , walkOnBinding = const $ return ()
                 , walkOnVName = const $ return ()
                 , walkOnCertificates = const $ return ()
                 , walkOnRetType = const $ return ()
                 , walkOnFParam = const $ return ()
                 , walkOnLParam = const $ return ()
                 , walkOnOp = const $ return ()
                 }

walkMapper :: Monad m => Walker lore m -> Mapper lore lore m
walkMapper f = Mapper {
                 mapOnSubExp = wrap walkOnSubExp
               , mapOnBody = wrap walkOnBody
               , mapOnVName = wrap walkOnVName
               , mapOnCertificates = wrap walkOnCertificates
               , mapOnRetType = wrap walkOnRetType
               , mapOnFParam = wrap walkOnFParam
               , mapOnOp = wrap walkOnOp
               }
  where wrap op k = op f k >> return k

-- | As 'walkBodyM', but for expressions.
walkExpM :: (Monad m, Applicative m) => Walker lore m -> Exp lore -> m ()
walkExpM f = void . mapExpM m
  where m = walkMapper f

-- | As 'walkExp', but runs in the 'Identity' monad..
walkExp :: Walker lore Identity -> Exp lore -> ()
walkExp f = runIdentity . walkExpM f
