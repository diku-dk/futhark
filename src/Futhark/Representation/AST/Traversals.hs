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
-- The "Futhark.Renamer" and "Futhark.Untrace" modules are simple examples of
-- how to use this facility.
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
  , foldlPattern
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

import Prelude

import Futhark.Representation.AST.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper flore tlore m = Mapper {
    mapOnSubExp :: SubExp -> m SubExp
  , mapOnBody :: Body flore -> m (Body tlore)
  , mapOnLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnExtLambda :: ExtLambda flore -> m (ExtLambda tlore)
  , mapOnVName :: VName -> m VName
  , mapOnCertificates :: Certificates -> m Certificates
  , mapOnRetType :: RetType flore -> m (RetType tlore)
  , mapOnFParam :: FParam flore -> m (FParam tlore)
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper lore lore m
identityMapper = Mapper {
                   mapOnSubExp = return
                 , mapOnBody = return
                 , mapOnLambda = return
                 , mapOnExtLambda = return
                 , mapOnVName = return
                 , mapOnCertificates = return
                 , mapOnRetType = return
                 , mapOnFParam = return
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
mapExpM tv (PrimOp (SubExp se)) =
  PrimOp <$> (SubExp <$> mapOnSubExp tv se)
mapExpM tv (PrimOp (ArrayLit els rowt)) =
  PrimOp <$> (pure ArrayLit <*> mapM (mapOnSubExp tv) els <*> mapOnType tv rowt)
mapExpM tv (PrimOp (BinOp bop x y t)) =
  PrimOp <$> (pure (BinOp bop) <*>
                 mapOnSubExp tv x <*> mapOnSubExp tv y <*>
                 pure t)
mapExpM tv (PrimOp (Not x)) =
  PrimOp <$> (Not <$> mapOnSubExp tv x)
mapExpM tv (PrimOp (Negate x)) =
  PrimOp <$> (Negate <$> mapOnSubExp tv x)
mapExpM tv (If c texp fexp ts) =
  pure If <*> mapOnSubExp tv c <*> mapOnBody tv texp <*> mapOnBody tv fexp <*>
       mapM (mapOnExtType tv) ts
mapExpM tv (Apply fname args ret) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*>
    mapOnRetType tv ret
mapExpM tv (PrimOp (Index cs arr idxexps)) =
  PrimOp <$> (pure Index <*> mapOnCertificates tv cs <*>
                 mapOnVName tv arr <*>
                 mapM (mapOnSubExp tv) idxexps)
mapExpM tv (PrimOp (Iota nexp)) =
  PrimOp <$> (pure Iota <*> mapOnSubExp tv nexp)
mapExpM tv (PrimOp (Replicate nexp vexp)) =
  PrimOp <$> (pure Replicate <*> mapOnSubExp tv nexp <*> mapOnSubExp tv vexp)
mapExpM tv (PrimOp (Scratch t shape)) =
  PrimOp <$> (Scratch t <$> mapM (mapOnSubExp tv) shape)
mapExpM tv (PrimOp (Reshape cs shape arrexp)) =
  PrimOp <$> (pure Reshape <*> mapOnCertificates tv cs <*>
                 mapM (mapOnSubExp tv) shape <*>
                 mapOnVName tv arrexp)
mapExpM tv (PrimOp (Rearrange cs perm e)) =
  PrimOp <$> (pure Rearrange <*> mapOnCertificates tv cs <*>
                 pure perm <*> mapOnVName tv e)
mapExpM tv (PrimOp (Split cs sizeexps arrexp)) =
  PrimOp <$> (pure Split <*> mapOnCertificates tv cs <*>
              mapM (mapOnSubExp tv) sizeexps <*> mapOnVName tv arrexp)
mapExpM tv (PrimOp (Concat cs x ys size)) =
  PrimOp <$> (pure Concat <*> mapOnCertificates tv cs <*>
                 mapOnVName tv x <*> mapM (mapOnVName tv) ys <*>
                 mapOnSubExp tv size)
mapExpM tv (PrimOp (Copy e)) =
  PrimOp <$> (pure Copy <*> mapOnSubExp tv e)
mapExpM tv (PrimOp (Alloc e)) =
  PrimOp <$> (pure Alloc <*> mapOnSubExp tv e)
mapExpM tv (PrimOp (Assert e loc)) =
  PrimOp <$> (pure Assert <*> mapOnSubExp tv e <*> pure loc)
mapExpM tv (PrimOp (Partition cs n flags arr)) =
  PrimOp <$> (pure Partition <*> mapOnCertificates tv cs <*>
              pure n <*>
              mapOnVName tv flags <*>
              mapOnVName tv arr)
mapExpM tv (LoopOp (DoLoop res mergepat form loopbody)) =
  LoopOp <$> (DoLoop <$> mapM (mapOnVName tv) res <*>
              (zip <$> mapM (mapOnFParam tv) vs <*> mapM (mapOnSubExp tv) es) <*>
              mapOnLoopForm tv form <*>
              mapOnBody tv loopbody)
  where (vs,es) = unzip mergepat
mapExpM tv (LoopOp (Map cs fun arrexps)) =
  LoopOp <$> (pure Map <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*> mapM (mapOnVName tv) arrexps)
mapExpM tv (LoopOp (ConcatMap cs fun arrexps)) =
  LoopOp <$> (pure ConcatMap <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*> mapM (mapM (mapOnVName tv)) arrexps)
mapExpM tv (LoopOp (Reduce cs fun inputs)) =
  LoopOp <$> (pure Reduce <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                       mapM (mapOnVName tv) arrexps))
  where (startexps, arrexps) = unzip inputs
mapExpM tv (LoopOp (Scan cs fun inputs)) =
  LoopOp <$> (pure Scan <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                       mapM (mapOnVName tv) arrexps))
  where (startexps, arrexps) = unzip inputs
mapExpM tv (LoopOp (Redomap cs redfun mapfun accexps arrexps)) =
  LoopOp <$> (pure Redomap <*> mapOnCertificates tv cs <*>
              mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
              mapM (mapOnSubExp tv) accexps <*> mapM (mapOnVName tv) arrexps)
mapExpM tv (LoopOp (Stream cs accs arrs lam)) =
  LoopOp <$> (pure Stream <*> mapOnCertificates tv cs <*>
              mapM (mapOnSubExp tv) accs <*>
              mapM (mapOnVName  tv) arrs <*> mapOnExtLambda tv lam)
mapExpM tv (SegOp (SegReduce cs fun inputs descp_exp)) =
  SegOp <$> (pure SegReduce <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                        mapM (mapOnVName tv) flatarr_exps) <*>
              mapOnVName tv descp_exp)
    where (startexps, flatarr_exps) = unzip inputs
mapExpM tv (SegOp (SegScan cs fun inputs descp_exp)) =
  SegOp <$> (pure SegScan <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                        mapM (mapOnVName tv) flatarr_exps) <*>
              mapOnVName tv descp_exp)
    where (startexps, flatarr_exps) = unzip inputs

mapOnExtType :: (Monad m, Applicative m) =>
                Mapper flore tlore m -> ExtType -> m ExtType
mapOnExtType tv (Array bt (ExtShape shape) u) =
  Array bt <$> (ExtShape <$> mapM mapOnExtSize shape) <*>
  return u
  where mapOnExtSize (Ext x)   = return $ Ext x
        mapOnExtSize (Free se) = Free <$> mapOnSubExp tv se
mapOnExtType _ (Basic bt) = return $ Basic bt
mapOnExtType tv (Mem size) = Mem <$> mapOnSubExp tv size

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
             Mapper flore tlore m -> Type -> m Type
mapOnType _ (Basic bt) = return $ Basic bt
mapOnType tv (Mem size) = Mem <$> mapOnSubExp tv size
mapOnType tv (Array bt shape u) =
  Array bt <$> (Shape <$> mapM (mapOnSubExp tv) (shapeDims shape)) <*> pure u

-- | Reification of a left-reduction across a syntax tree.
data Folder a lore m = Folder {
    foldOnSubExp :: a -> SubExp -> m a
  , foldOnBody :: a -> Body lore -> m a
  , foldOnBinding :: a -> Binding lore -> m a
  , foldOnLambda :: a -> Lambda lore -> m a
  , foldOnExtLambda :: a -> ExtLambda lore -> m a
  , foldOnVName :: a -> VName -> m a
  , foldOnCertificates :: a -> Certificates -> m a
  , foldOnRetType :: a -> RetType lore -> m a
  , foldOnFParam :: a -> FParam lore -> m a
  }

-- | A folding operation where the accumulator is returned verbatim.
identityFolder :: Monad m => Folder a lore m
identityFolder = Folder {
                   foldOnSubExp = const . return
                 , foldOnBody = const . return
                 , foldOnBinding = const . return
                 , foldOnLambda = const . return
                 , foldOnExtLambda = const . return
                 , foldOnVName = const . return
                 , foldOnCertificates = const . return
                 , foldOnRetType = const . return
                 , foldOnFParam = const . return
                 }

foldMapper :: Monad m => Folder a lore m -> Mapper lore lore (StateT a m)
foldMapper f = Mapper {
                 mapOnSubExp = wrap foldOnSubExp
               , mapOnBody = wrap foldOnBody
               , mapOnLambda = wrap foldOnLambda
               , mapOnExtLambda = wrap foldOnExtLambda
               , mapOnVName = wrap foldOnVName
               , mapOnCertificates = wrap foldOnCertificates
               , mapOnRetType = wrap foldOnRetType
               , mapOnFParam = wrap foldOnFParam
               }
  where wrap op k = do
          v <- get
          put =<< lift (op f v k)
          return k

-- | Perform a left-reduction across the bindings of a
-- body.
foldBody :: (a -> Binding lore -> a) -> a -> Body lore -> a
foldBody f a = foldl f a . bodyBindings

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
  , walkOnLambda :: Lambda lore -> m ()
  , walkOnExtLambda :: ExtLambda lore -> m ()
  , walkOnVName :: VName -> m ()
  , walkOnCertificates :: Certificates -> m ()
  , walkOnRetType :: RetType lore -> m ()
  , walkOnFParam :: FParam lore -> m ()
  }

-- | A no-op traversal.
identityWalker :: Monad m => Walker lore m
identityWalker = Walker {
                   walkOnSubExp = const $ return ()
                 , walkOnBody = const $ return ()
                 , walkOnBinding = const $ return ()
                 , walkOnLambda = const $ return ()
                 , walkOnExtLambda = const $ return ()
                 , walkOnVName = const $ return ()
                 , walkOnCertificates = const $ return ()
                 , walkOnRetType = const $ return ()
                 , walkOnFParam = const $ return ()
                 }

walkMapper :: Monad m => Walker lore m -> Mapper lore lore m
walkMapper f = Mapper {
                 mapOnSubExp = wrap walkOnSubExp
               , mapOnBody = wrap walkOnBody
               , mapOnLambda = wrap walkOnLambda
               , mapOnExtLambda = wrap walkOnExtLambda
               , mapOnVName = wrap walkOnVName
               , mapOnCertificates = wrap walkOnCertificates
               , mapOnRetType = wrap walkOnRetType
               , mapOnFParam = wrap walkOnFParam
               }
  where wrap op k = op f k >> return k

-- | As 'walkBodyM', but for expressions.
walkExpM :: (Monad m, Applicative m) => Walker lore m -> Exp lore -> m ()
walkExpM f = void . mapExpM m
  where m = walkMapper f

-- | As 'walkExp', but runs in the 'Identity' monad..
walkExp :: Walker lore Identity -> Exp lore -> ()
walkExp f = runIdentity . walkExpM f

-- | Common case of 'foldExp', where only 'Exp's are taken into
-- account.
foldlPattern :: (a -> Exp lore -> a) ->
                a -> Exp lore -> a
foldlPattern expf = foldExp m
  where m = identityFolder {
              foldOnBinding = \x (Let _ _ e) -> return $ expf x e
            , foldOnBody = \x -> return . foldBody onBinding x
            , foldOnLambda =
              \x (Lambda _ body _) ->
                return $ foldBody onBinding x body
            }
        onBinding x (Let _ _ e) = expf x e
