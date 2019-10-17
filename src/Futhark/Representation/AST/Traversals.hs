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
-- A traversal of the Futhark syntax tree is expressed as a record of
-- functions expressing the operations to be performed on the various
-- types of nodes.
--
-- The "Futhark.Transform.Rename" module is a simple example of how to
-- use this facility.
module Futhark.Representation.AST.Traversals
  (
  -- * Mapping
    Mapper(..)
  , identityMapper
  , mapExpM
  , mapExp
  , mapOnType
  , mapOnLoopForm
  , mapOnExtType

  -- * Walking
  , Walker(..)
  , identityWalker
  , walkExpM
  )
  where

import Control.Monad
import Control.Monad.Identity
import qualified Data.Traversable
import Data.Foldable (traverse_)

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Scope

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper flore tlore m = Mapper {
    mapOnSubExp :: SubExp -> m SubExp
  , mapOnBody :: Scope tlore -> Body flore -> m (Body tlore)
    -- ^ Most bodies are enclosed in a scope, which is passed along
    -- for convenience.
  , mapOnVName :: VName -> m VName
  , mapOnRetType :: RetType flore -> m (RetType tlore)
  , mapOnBranchType :: BranchType flore -> m (BranchType tlore)
  , mapOnFParam :: FParam flore -> m (FParam tlore)
  , mapOnLParam :: LParam flore -> m (LParam tlore)
  , mapOnOp :: Op flore -> m (Op tlore)
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper lore lore m
identityMapper = Mapper {
                   mapOnSubExp = return
                 , mapOnBody = const return
                 , mapOnVName = return
                 , mapOnRetType = return
                 , mapOnBranchType = return
                 , mapOnFParam = return
                 , mapOnLParam = return
                 , mapOnOp = return
                 }

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) =>
           Mapper flore tlore m -> Exp flore -> m (Exp tlore)
mapExpM tv (BasicOp (SubExp se)) =
  BasicOp <$> (SubExp <$> mapOnSubExp tv se)
mapExpM tv (BasicOp (ArrayLit els rowt)) =
  BasicOp <$> (ArrayLit <$> mapM (mapOnSubExp tv) els <*>
              mapOnType (mapOnSubExp tv) rowt)
mapExpM tv (BasicOp (BinOp bop x y)) =
  BasicOp <$> (BinOp bop <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (CmpOp op x y)) =
  BasicOp <$> (CmpOp op <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (ConvOp conv x)) =
  BasicOp <$> (ConvOp conv <$> mapOnSubExp tv x)
mapExpM tv (BasicOp (UnOp op x)) =
  BasicOp <$> (UnOp op <$> mapOnSubExp tv x)
mapExpM tv (If c texp fexp (IfAttr ts s)) =
  If <$> mapOnSubExp tv c <*> mapOnBody tv mempty texp <*> mapOnBody tv mempty fexp <*>
        (IfAttr <$> mapM (mapOnBranchType tv) ts <*> pure s)
mapExpM tv (Apply fname args ret loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  Apply fname <$> pure args' <*> mapM (mapOnRetType tv) ret <*> pure loc
mapExpM tv (BasicOp (Index arr slice)) =
  BasicOp <$> (Index <$> mapOnVName tv arr <*>
               mapM (traverse (mapOnSubExp tv)) slice)
mapExpM tv (BasicOp (Update arr slice se)) =
  BasicOp <$> (Update <$> mapOnVName tv arr <*>
               mapM (traverse (mapOnSubExp tv)) slice <*> mapOnSubExp tv se)
mapExpM tv (BasicOp (Iota n x s et)) =
  BasicOp <$> (Iota <$> mapOnSubExp tv n <*> mapOnSubExp tv x <*> mapOnSubExp tv s <*> pure et)
mapExpM tv (BasicOp (Replicate shape vexp)) =
  BasicOp <$> (Replicate <$> mapOnShape tv shape <*> mapOnSubExp tv vexp)
mapExpM tv (BasicOp (Repeat shapes innershape v)) =
  BasicOp <$> (Repeat <$> mapM (mapOnShape tv) shapes <*>
               mapOnShape tv innershape <*> mapOnVName tv v)
mapExpM tv (BasicOp (Scratch t shape)) =
  BasicOp <$> (Scratch t <$> mapM (mapOnSubExp tv) shape)
mapExpM tv (BasicOp (Reshape shape arrexp)) =
  BasicOp <$> (Reshape <$>
               mapM (Data.Traversable.traverse (mapOnSubExp tv)) shape <*>
               mapOnVName tv arrexp)
mapExpM tv (BasicOp (Rearrange perm e)) =
  BasicOp <$> (Rearrange <$> pure perm <*> mapOnVName tv e)
mapExpM tv (BasicOp (Rotate es e)) =
  BasicOp <$> (Rotate <$> mapM (mapOnSubExp tv) es <*> mapOnVName tv e)
mapExpM tv (BasicOp (Concat i x ys size)) =
  BasicOp <$> (Concat <$> pure i <*>
              mapOnVName tv x <*> mapM (mapOnVName tv) ys <*>
              mapOnSubExp tv size)
mapExpM tv (BasicOp (Copy e)) =
  BasicOp <$> (Copy <$> mapOnVName tv e)
mapExpM tv (BasicOp (Manifest perm e)) =
  BasicOp <$> (Manifest perm <$> mapOnVName tv e)
mapExpM tv (BasicOp (Assert e msg loc)) =
  BasicOp <$> (Assert <$> mapOnSubExp tv e <*> traverse (mapOnSubExp tv) msg <*> pure loc)
mapExpM tv (BasicOp (Opaque e)) =
  BasicOp <$> (Opaque <$> mapOnSubExp tv e)
mapExpM tv (DoLoop ctxmerge valmerge form loopbody) = do
  ctxparams' <- mapM (mapOnFParam tv) ctxparams
  valparams' <- mapM (mapOnFParam tv) valparams
  form' <- mapOnLoopForm tv form
  let scope = scopeOf form' <> scopeOfFParams (ctxparams'++valparams')
  DoLoop <$>
    (zip ctxparams' <$> mapM (mapOnSubExp tv) ctxinits) <*>
    (zip valparams' <$> mapM (mapOnSubExp tv) valinits) <*>
    pure form' <*> mapOnBody tv scope loopbody
  where (ctxparams,ctxinits) = unzip ctxmerge
        (valparams,valinits) = unzip valmerge
mapExpM tv (Op op) =
  Op <$> mapOnOp tv op

mapOnShape :: Monad m => Mapper flore tlore m -> Shape -> m Shape
mapOnShape tv (Shape ds) = Shape <$> mapM (mapOnSubExp tv) ds

mapOnExtType :: Monad m =>
                Mapper flore tlore m -> TypeBase ExtShape u -> m (TypeBase ExtShape u)
mapOnExtType tv (Array bt (Shape shape) u) =
  Array bt <$> (Shape <$> mapM mapOnExtSize shape) <*>
  return u
  where mapOnExtSize (Ext x)   = return $ Ext x
        mapOnExtSize (Free se) = Free <$> mapOnSubExp tv se
mapOnExtType _ (Prim bt) = return $ Prim bt
mapOnExtType _ (Mem space) = pure $ Mem space

mapOnLoopForm :: Monad m =>
                 Mapper flore tlore m -> LoopForm flore -> m (LoopForm tlore)
mapOnLoopForm tv (ForLoop i it bound loop_vars) =
  ForLoop <$> mapOnVName tv i <*> pure it <*> mapOnSubExp tv bound <*>
  (zip <$> mapM (mapOnLParam tv) loop_lparams <*> mapM (mapOnVName tv) loop_arrs)
  where (loop_lparams,loop_arrs) = unzip loop_vars
mapOnLoopForm tv (WhileLoop cond) =
  WhileLoop <$> mapOnVName tv cond

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper flore tlore Identity -> Exp flore -> Exp tlore
mapExp m = runIdentity . mapExpM m

mapOnType :: Monad m =>
             (SubExp -> m SubExp) -> Type -> m Type
mapOnType _ (Prim bt) = return $ Prim bt
mapOnType _ (Mem space) = pure $ Mem space
mapOnType f (Array bt shape u) =
  Array bt <$> (Shape <$> mapM f (shapeDims shape)) <*> pure u

-- | Express a monad expression on a syntax node.  Each element of
-- this structure expresses the action to be performed on a given
-- child.
data Walker lore m = Walker {
    walkOnSubExp :: SubExp -> m ()
  , walkOnBody :: Body lore -> m ()
  , walkOnVName :: VName -> m ()
  , walkOnRetType :: RetType lore -> m ()
  , walkOnBranchType :: BranchType lore -> m ()
  , walkOnFParam :: FParam lore -> m ()
  , walkOnLParam :: LParam lore -> m ()
  , walkOnOp :: Op lore -> m ()
  }

-- | A no-op traversal.
identityWalker :: Monad m => Walker lore m
identityWalker = Walker {
                   walkOnSubExp = const $ return ()
                 , walkOnBody = const $ return ()
                 , walkOnVName = const $ return ()
                 , walkOnRetType = const $ return ()
                 , walkOnBranchType = const $ return ()
                 , walkOnFParam = const $ return ()
                 , walkOnLParam = const $ return ()
                 , walkOnOp = const $ return ()
                 }

walkOnShape :: Monad m => Walker lore m -> Shape -> m ()
walkOnShape tv (Shape ds) = mapM_ (walkOnSubExp tv) ds

walkOnType :: Monad m =>
             (SubExp -> m ()) -> Type -> m ()
walkOnType _ Prim{} = return ()
walkOnType _ Mem{} = return ()
walkOnType f (Array _ shape _) = mapM_ f $ shapeDims shape

walkOnLoopForm :: Monad m => Walker lore m -> LoopForm lore -> m ()
walkOnLoopForm tv (ForLoop i _ bound loop_vars) =
  walkOnVName tv i >> walkOnSubExp tv bound >>
  mapM_ (walkOnLParam tv) loop_lparams >> mapM_ (walkOnVName tv) loop_arrs
  where (loop_lparams,loop_arrs) = unzip loop_vars
walkOnLoopForm tv (WhileLoop cond) =
  walkOnVName tv cond

-- | As 'mapExpM', but do not construct a result AST.
walkExpM :: Monad m => Walker lore m -> Exp lore -> m ()
walkExpM tv (BasicOp (SubExp se)) =
  walkOnSubExp tv se
walkExpM tv (BasicOp (ArrayLit els rowt)) =
  mapM_ (walkOnSubExp tv) els >> walkOnType (walkOnSubExp tv) rowt
walkExpM tv (BasicOp (BinOp _ x y)) =
  walkOnSubExp tv x >> walkOnSubExp tv y
walkExpM tv (BasicOp (CmpOp _ x y)) =
  walkOnSubExp tv x >> walkOnSubExp tv y
walkExpM tv (BasicOp (ConvOp _ x)) =
  walkOnSubExp tv x
walkExpM tv (BasicOp (UnOp _ x)) =
  walkOnSubExp tv x
walkExpM tv (If c texp fexp (IfAttr ts _)) =
  walkOnSubExp tv c >> walkOnBody tv texp >>
  walkOnBody tv fexp >> mapM_ (walkOnBranchType tv) ts
walkExpM tv (Apply _ args ret _) =
  mapM_ (walkOnSubExp tv . fst) args >> mapM_ (walkOnRetType tv) ret
walkExpM tv (BasicOp (Index arr slice)) =
  walkOnVName tv arr >> mapM_ (traverse_ (walkOnSubExp tv)) slice
walkExpM tv (BasicOp (Update arr slice se)) =
  walkOnVName tv arr >>
  mapM_ (traverse_ (walkOnSubExp tv)) slice >>
  walkOnSubExp tv se
walkExpM tv (BasicOp (Iota n x s _)) =
  walkOnSubExp tv n >> walkOnSubExp tv x >> walkOnSubExp tv s
walkExpM tv (BasicOp (Replicate shape vexp)) =
  walkOnShape tv shape >> walkOnSubExp tv vexp
walkExpM tv (BasicOp (Repeat shapes innershape v)) =
  mapM_ (walkOnShape tv) shapes >> walkOnShape tv innershape >> walkOnVName tv v
walkExpM tv (BasicOp (Scratch _ shape)) =
  mapM_ (walkOnSubExp tv) shape
walkExpM tv (BasicOp (Reshape shape arrexp)) =
  mapM_ (traverse_ (walkOnSubExp tv)) shape >> walkOnVName tv arrexp
walkExpM tv (BasicOp (Rearrange _ e)) =
  walkOnVName tv e
walkExpM tv (BasicOp (Rotate es e)) =
  mapM_ (walkOnSubExp tv) es >> walkOnVName tv e
walkExpM tv (BasicOp (Concat _ x ys size)) =
  walkOnVName tv x >> mapM_ (walkOnVName tv) ys >> walkOnSubExp tv size
walkExpM tv (BasicOp (Copy e)) =
  walkOnVName tv e
walkExpM tv (BasicOp (Manifest _ e)) =
  walkOnVName tv e
walkExpM tv (BasicOp (Assert e msg _)) =
  walkOnSubExp tv e >> traverse_ (walkOnSubExp tv) msg
walkExpM tv (BasicOp (Opaque e)) =
  walkOnSubExp tv e
walkExpM tv (DoLoop ctxmerge valmerge form loopbody) = do
  mapM_ (walkOnFParam tv) ctxparams
  mapM_ (walkOnFParam tv) valparams
  walkOnLoopForm tv form
  mapM_ (walkOnSubExp tv) ctxinits
  mapM_ (walkOnSubExp tv) valinits
  walkOnBody tv loopbody
  where (ctxparams,ctxinits) = unzip ctxmerge
        (valparams,valinits) = unzip valmerge
walkExpM tv (Op op) =
  walkOnOp tv op
