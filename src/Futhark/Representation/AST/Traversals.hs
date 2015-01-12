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
  , mapBodyM
  , mapBody
  , mapExpM
  , mapExp

  -- * Folding
  , Folder(..)
  , foldBodyM
  , foldBody
  , foldExpM
  , foldExp
  , identityFolder

  -- * Walking
  , Walker(..)
  , identityWalker
  , walkExpM
  , walkExp
  , walkBodyM

  -- * Simple wrappers
  , foldlPattern
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

import Futhark.Representation.AST.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper flore tlore m = Mapper {
    mapOnSubExp :: SubExp -> m SubExp
  , mapOnBody :: Body flore -> m (Body tlore)
  , mapOnBinding :: Binding flore -> m (Binding tlore)
  , mapOnType :: Type -> m Type
  , mapOnLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnIdent :: Ident -> m Ident
  , mapOnValue :: Value -> m Value
  , mapOnCertificates :: Certificates -> m Certificates
  , mapOnRetType :: RetType flore -> m (RetType tlore)
  , mapOnFParam :: FParam flore -> m (FParam tlore)
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper lore lore m
identityMapper = Mapper {
                   mapOnSubExp = return
                 , mapOnBinding = return
                 , mapOnBody = return
                 , mapOnType = return
                 , mapOnLambda = return
                 , mapOnIdent = return
                 , mapOnValue = return
                 , mapOnCertificates = return
                 , mapOnRetType = return
                 , mapOnFParam = return
                 }

-- | Map a monadic action across the immediate children of a body.
-- Importantly, the 'mapOnBody' action is not invoked for the body
-- itself.  The mapping is done left-to-right.
mapBodyM :: (Applicative m, Monad m) =>
            Mapper lore lore m -> Body lore -> m (Body lore)
mapBodyM tv (Body lore [] (Result cs ses loc)) =
  Body lore [] <$> (Result <$> mapOnCertificates tv cs <*>
                    mapM (mapOnSubExp tv) ses <*> pure loc)
mapBodyM tv (Body lore (bnd:bnds) res) = do
  bnd' <- mapOnBinding tv bnd
  Body lore' bnds' res' <- mapOnBody tv $ Body lore bnds res
  return $ Body lore' (bnd':bnds') res'

-- | Like 'mapBodyM', but in the 'Identity' monad.
mapBody :: Mapper lore lore Identity -> Body lore -> Body lore
mapBody m = runIdentity . mapBodyM m

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) =>
           Mapper flore tlore m -> Exp flore -> m (Exp tlore)
mapExpM tv (PrimOp (SubExp se)) =
  PrimOp <$> (SubExp <$> mapOnSubExp tv se)
mapExpM tv (PrimOp (ArrayLit els rowt loc)) =
  PrimOp <$> (pure ArrayLit <*> mapM (mapOnSubExp tv) els <*> mapOnType tv rowt <*> pure loc)
mapExpM tv (PrimOp (BinOp bop x y t loc)) =
  PrimOp <$> (pure (BinOp bop) <*>
                 mapOnSubExp tv x <*> mapOnSubExp tv y <*>
                 mapOnType tv t <*> pure loc)
mapExpM tv (PrimOp (Not x loc)) =
  PrimOp <$> (pure Not <*> mapOnSubExp tv x <*> pure loc)
mapExpM tv (PrimOp (Negate x loc)) =
  PrimOp <$> (pure Negate <*> mapOnSubExp tv x <*> pure loc)
mapExpM tv (If c texp fexp ts loc) =
  pure If <*> mapOnSubExp tv c <*> mapOnBody tv texp <*> mapOnBody tv fexp <*>
       mapM (mapOnExtType tv) ts <*> pure loc
mapExpM tv (Apply fname args ret loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*>
    mapOnRetType tv ret <*> pure loc
mapExpM tv (PrimOp (Index cs arr idxexps loc)) =
  PrimOp <$> (pure Index <*> mapOnCertificates tv cs <*>
                 mapOnIdent tv arr <*>
                 mapM (mapOnSubExp tv) idxexps <*>
                 pure loc)
mapExpM tv (PrimOp (Update cs src idxexps vexp loc)) =
  PrimOp <$> (pure Update <*> mapOnCertificates tv cs <*>
                 mapOnIdent tv src <*>
                 mapM (mapOnSubExp tv) idxexps <*> mapOnSubExp tv vexp <*>
                 pure loc)
mapExpM tv (PrimOp (Iota nexp loc)) =
  PrimOp <$> (pure Iota <*> mapOnSubExp tv nexp <*> pure loc)
mapExpM tv (PrimOp (Replicate nexp vexp loc)) =
  PrimOp <$> (pure Replicate <*> mapOnSubExp tv nexp <*> mapOnSubExp tv vexp <*> pure loc)
mapExpM tv (PrimOp (Reshape cs shape arrexp loc)) =
  PrimOp <$> (pure Reshape <*> mapOnCertificates tv cs <*>
                 mapM (mapOnSubExp tv) shape <*>
                 mapOnIdent tv arrexp <*> pure loc)
mapExpM tv (PrimOp (Rearrange cs perm e loc)) =
  PrimOp <$> (pure Rearrange <*> mapOnCertificates tv cs <*>
                 pure perm <*> mapOnIdent tv e <*> pure loc)
mapExpM tv (PrimOp (Rotate cs n e loc)) =
  PrimOp <$> (pure Rotate <*> mapOnCertificates tv cs <*>
                 pure n <*> mapOnIdent tv e <*> pure loc)
mapExpM tv (PrimOp (Split cs nexp arrexp size loc)) =
  PrimOp <$> (pure Split <*> mapOnCertificates tv cs <*>
                 mapOnSubExp tv nexp <*> mapOnIdent tv arrexp <*>
                 mapOnSubExp tv size <*> pure loc)
mapExpM tv (PrimOp (Concat cs x y size loc)) =
  PrimOp <$> (pure Concat <*> mapOnCertificates tv cs <*>
                 mapOnIdent tv x <*> mapOnIdent tv y <*>
                 mapOnSubExp tv size <*> pure loc)
mapExpM tv (PrimOp (Copy e loc)) =
  PrimOp <$> (pure Copy <*> mapOnSubExp tv e <*> pure loc)
mapExpM tv (PrimOp (Alloc e loc)) =
  PrimOp <$> (pure Alloc <*> mapOnSubExp tv e <*> pure loc)
mapExpM tv (PrimOp (Assert e loc)) =
  PrimOp <$> (pure Assert <*> mapOnSubExp tv e <*> pure loc)
mapExpM tv (PrimOp (Conjoin es loc)) =
  PrimOp <$> (pure Conjoin <*> mapM (mapOnSubExp tv) es <*> pure loc)
mapExpM tv (LoopOp (DoLoop res mergepat loopvar boundexp loopbody loc)) =
  LoopOp <$> (DoLoop <$> mapM (mapOnIdent tv) res <*>
              (zip <$> mapM (mapOnFParam tv) vs <*> mapM (mapOnSubExp tv) es) <*>
              mapOnIdent tv loopvar <*> mapOnSubExp tv boundexp <*>
              mapOnBody tv loopbody <*> pure loc)
  where (vs,es) = unzip mergepat
mapExpM tv (LoopOp (Map cs fun arrexps loc)) =
  LoopOp <$> (pure Map <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*> mapM (mapOnIdent tv) arrexps <*>
              pure loc)
mapExpM tv (LoopOp (Reduce cs fun inputs loc)) =
  LoopOp <$> (pure Reduce <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                       mapM (mapOnIdent tv) arrexps) <*>
              pure loc)
  where (startexps, arrexps) = unzip inputs
mapExpM tv (LoopOp (Scan cs fun inputs loc)) =
  LoopOp <$> (pure Scan <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              (zip <$> mapM (mapOnSubExp tv) startexps <*>
                       mapM (mapOnIdent tv) arrexps) <*>
              pure loc)
  where (startexps, arrexps) = unzip inputs
mapExpM tv (LoopOp (Filter cs fun arrexps loc)) =
  LoopOp <$> (pure Filter <*> mapOnCertificates tv cs <*>
              mapOnLambda tv fun <*>
              mapM (mapOnIdent tv) arrexps <*>
              pure loc)
mapExpM tv (LoopOp (Redomap cs redfun mapfun accexps arrexps loc)) =
  LoopOp <$> (pure Redomap <*> mapOnCertificates tv cs <*>
              mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
              mapM (mapOnSubExp tv) accexps <*> mapM (mapOnIdent tv) arrexps <*>
              pure loc)

mapOnExtType :: (Monad m, Applicative m) =>
                Mapper flore tlore m -> ExtType -> m ExtType
mapOnExtType tv (Array bt (ExtShape shape) u) =
  Array bt <$> (ExtShape <$> mapM mapOnExtSize shape) <*>
  return u
  where mapOnExtSize (Ext x)   = return $ Ext x
        mapOnExtSize (Free se) = Free <$> mapOnSubExp tv se
mapOnExtType _ (Basic bt) = return $ Basic bt
mapOnExtType tv (Mem size) = Mem <$> mapOnSubExp tv size

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper flore tlore Identity -> Exp flore -> Exp tlore
mapExp m = runIdentity . mapExpM m

-- | Reification of a left-reduction across a syntax tree.
data Folder a lore m = Folder {
    foldOnSubExp :: a -> SubExp -> m a
  , foldOnBody :: a -> Body lore -> m a
  , foldOnBinding :: a -> Binding lore -> m a
  , foldOnType :: a -> Type -> m a
  , foldOnLambda :: a -> Lambda lore -> m a
  , foldOnIdent :: a -> Ident -> m a
  , foldOnValue :: a -> Value -> m a
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
                 , foldOnType = const . return
                 , foldOnLambda = const . return
                 , foldOnIdent = const . return
                 , foldOnValue = const . return
                 , foldOnCertificates = const . return
                 , foldOnRetType = const . return
                 , foldOnFParam = const . return
                 }

foldMapper :: Monad m => Folder a lore m -> Mapper lore lore (StateT a m)
foldMapper f = Mapper {
                 mapOnSubExp = wrap foldOnSubExp
               , mapOnBody = wrap foldOnBody
               , mapOnBinding = wrap foldOnBinding
               , mapOnType = wrap foldOnType
               , mapOnLambda = wrap foldOnLambda
               , mapOnIdent = wrap foldOnIdent
               , mapOnValue = wrap foldOnValue
               , mapOnCertificates = wrap foldOnCertificates
               , mapOnRetType = wrap foldOnRetType
               , mapOnFParam = wrap foldOnFParam
               }
  where wrap op k = do
          v <- get
          put =<< lift (op f v k)
          return k

-- | Perform a left-reduction across the immediate children of a
-- body.  Importantly, the 'foldOnExp' action is not invoked for
-- the body itself, and the reduction does not descend recursively
-- into subterms.  The reduction is done left-to-right.
foldBodyM :: (Monad m, Functor m) => Folder a lore m -> a -> Body lore -> m a
foldBodyM f x e = execStateT (mapBodyM (foldMapper f) e) x

-- | As 'foldBodyM', but in the 'Identity' monad.
foldBody :: Folder a lore Identity -> a -> Body lore -> a
foldBody m x = runIdentity . foldBodyM m x

-- | As 'foldBodyM', but for expressions.
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
  , walkOnType :: Type -> m ()
  , walkOnLambda :: Lambda lore -> m ()
  , walkOnIdent :: Ident -> m ()
  , walkOnValue :: Value -> m ()
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
                 , walkOnType = const $ return ()
                 , walkOnLambda = const $ return ()
                 , walkOnIdent = const $ return ()
                 , walkOnValue = const $ return ()
                 , walkOnCertificates = const $ return ()
                 , walkOnRetType = const $ return ()
                 , walkOnFParam = const $ return ()
                 }

walkMapper :: Monad m => Walker lore m -> Mapper lore lore m
walkMapper f = Mapper {
                 mapOnSubExp = wrap walkOnSubExp
               , mapOnBody = wrap walkOnBody
               , mapOnBinding = wrap walkOnBinding
               , mapOnType = wrap walkOnType
               , mapOnLambda = wrap walkOnLambda
               , mapOnIdent = wrap walkOnIdent
               , mapOnValue = wrap walkOnValue
               , mapOnCertificates = wrap walkOnCertificates
               , mapOnRetType = wrap walkOnRetType
               , mapOnFParam = wrap walkOnFParam
               }
  where wrap op k = op f k >> return k

-- | Perform a monadic action on each of the immediate children of an
-- expression.  Importantly, the 'walkOnExp' action is not invoked for
-- the expression itself, and the traversal does not descend
-- recursively into subexpressions.  The traversal is done
-- left-to-right.
walkBodyM :: (Monad m, Applicative m) => Walker lore m -> Body lore -> m ()
walkBodyM f = void . mapBodyM m
  where m = walkMapper f

-- | As 'walkBodyM', but for expressions.
walkExpM :: (Monad m, Applicative m) => Walker lore m -> Exp lore -> m ()
walkExpM f = void . mapExpM m
  where m = walkMapper f

-- | As 'walkExp', but runs in the 'Identity' monad..
walkExp :: Walker lore Identity -> Exp lore -> ()
walkExp f = runIdentity . walkExpM f

-- | Common case of 'foldExp', where only 'Exp's and 'Lambda's are
-- taken into account.
foldlPattern :: (a -> Exp lore -> a) ->
                (a -> Lambda lore -> a) ->
                a -> Exp lore -> a
foldlPattern expf lamf = foldExp m
  where m = identityFolder {
              foldOnBinding = \x (Let _ _ e) -> return $ expf x e
            , foldOnBody = \x -> return . foldBody m x
            , foldOnLambda =
              \x lam@(Lambda _ body _ _) ->
                return $ foldBody m (lamf x lam) body
            }
