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
    mapOnSubExp :: SubExp flore -> m (SubExp tlore)
  , mapOnBody :: Body flore -> m (Body tlore)
  , mapOnBinding :: Binding flore -> m (Binding tlore)
  , mapOnType :: Type flore -> m (Type tlore)
  , mapOnLambda :: Lambda flore -> m (Lambda tlore)
  , mapOnIdent :: Ident flore -> m (Ident tlore)
  , mapOnValue :: Value -> m Value
  , mapOnCertificates :: Certificates flore -> m (Certificates tlore)
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
                 }

-- | Map a monadic action across the immediate children of a body.
-- Importantly, the 'mapOnBody' action is not invoked for the body
-- itself.  The mapping is done left-to-right.
mapBodyM :: (Applicative m, Monad m) => Mapper flore tlore m -> Body flore -> m (Body tlore)
mapBodyM tv (Body [] (Result cs ses loc)) =
  Body [] <$> (Result <$> mapOnCertificates tv cs <*>
               mapM (mapOnSubExp tv) ses <*> pure loc)
mapBodyM tv (Body (bnd:bnds) res) = do
  bnd' <- mapOnBinding tv bnd
  Body bnds' res' <- mapOnBody tv $ Body bnds res
  return $ Body (bnd':bnds') res'

-- | Like 'mapBodyM', but in the 'Identity' monad.
mapBody :: Mapper flore tlore Identity -> Body flore -> Body tlore
mapBody m = runIdentity . mapBodyM m

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) =>
           Mapper flore tlore m -> Exp flore -> m (Exp tlore)
mapExpM tv (SubExp se) =
  SubExp <$> mapOnSubExp tv se
mapExpM tv (ArrayLit els rowt loc) =
  pure ArrayLit <*> mapM (mapOnSubExp tv) els <*> mapOnType tv rowt <*> pure loc
mapExpM tv (BinOp bop x y t loc) =
  pure (BinOp bop) <*>
         mapOnSubExp tv x <*> mapOnSubExp tv y <*>
         mapOnType tv t <*> pure loc
mapExpM tv (Not x loc) =
  pure Not <*> mapOnSubExp tv x <*> pure loc
mapExpM tv (Negate x loc) =
  pure Negate <*> mapOnSubExp tv x <*> pure loc
mapExpM tv (If c texp fexp t loc) =
  pure If <*> mapOnSubExp tv c <*> mapOnBody tv texp <*> mapOnBody tv fexp <*>
       mapOnResType tv t <*> pure loc
mapExpM tv (Apply fname args ret loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*>
    mapOnResType tv ret <*> pure loc
mapExpM tv (Index cs arr idxexps loc) =
  pure Index <*> mapOnCertificates tv cs <*>
       mapOnIdent tv arr <*>
       mapM (mapOnSubExp tv) idxexps <*>
       pure loc
mapExpM tv (Update cs src idxexps vexp loc) =
  Update <$> mapOnCertificates tv cs <*>
         mapOnIdent tv src <*>
         mapM (mapOnSubExp tv) idxexps <*> mapOnSubExp tv vexp <*>
         pure loc
mapExpM tv (Iota nexp loc) =
  pure Iota <*> mapOnSubExp tv nexp <*> pure loc
mapExpM tv (Replicate nexp vexp loc) =
  pure Replicate <*> mapOnSubExp tv nexp <*> mapOnSubExp tv vexp <*> pure loc
mapExpM tv (Reshape cs shape arrexp loc) =
  pure Reshape <*> mapOnCertificates tv cs <*>
       mapM (mapOnSubExp tv) shape <*>
       mapOnSubExp tv arrexp <*> pure loc
mapExpM tv (Rearrange cs perm e loc) =
  pure Rearrange <*> mapOnCertificates tv cs <*>
       pure perm <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Rotate cs n e loc) =
  pure Rotate <*> mapOnCertificates tv cs <*>
       pure n <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Split cs nexp arrexp size loc) =
  pure Split <*> mapOnCertificates tv cs <*>
       mapOnSubExp tv nexp <*> mapOnSubExp tv arrexp <*>
       mapOnSubExp tv size <*> pure loc
mapExpM tv (Concat cs x y size loc) =
  pure Concat <*> mapOnCertificates tv cs <*>
       mapOnSubExp tv x <*> mapOnSubExp tv y <*>
       mapOnSubExp tv size <*> pure loc
mapExpM tv (Copy e loc) =
  pure Copy <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Assert e loc) =
  pure Assert <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Conjoin es loc) =
  pure Conjoin <*> mapM (mapOnSubExp tv) es <*> pure loc
mapExpM tv (DoLoop respat mergepat loopvar boundexp loopbody loc) =
  DoLoop <$> mapM (mapOnIdent tv) respat <*>
             (zip <$> mapM (mapOnIdent tv) vs <*> mapM (mapOnSubExp tv) es) <*>
             mapOnIdent tv loopvar <*> mapOnSubExp tv boundexp <*>
             mapOnBody tv loopbody <*> pure loc
  where (vs,es) = unzip mergepat
mapExpM tv (Map cs fun arrexps loc) =
  pure Map <*> mapOnCertificates tv cs <*>
       mapOnLambda tv fun <*> mapM (mapOnSubExp tv) arrexps <*>
       pure loc
mapExpM tv (Reduce cs fun inputs loc) =
  pure Reduce <*> mapOnCertificates tv cs <*>
       mapOnLambda tv fun <*>
       (zip <$> mapM (mapOnSubExp tv) startexps <*> mapM (mapOnSubExp tv) arrexps) <*>
       pure loc
  where (startexps, arrexps) = unzip inputs
mapExpM tv (Scan cs fun inputs loc) =
  pure Scan <*> mapOnCertificates tv cs <*>
       mapOnLambda tv fun <*>
       (zip <$> mapM (mapOnSubExp tv) startexps <*> mapM (mapOnSubExp tv) arrexps) <*>
       pure loc
  where (startexps, arrexps) = unzip inputs
mapExpM tv (Filter cs fun arrexps loc) =
  pure Filter <*> mapOnCertificates tv cs <*>
       mapOnLambda tv fun <*>
       mapM (mapOnSubExp tv) arrexps <*>
       pure loc
mapExpM tv (Redomap cs redfun mapfun accexps arrexps loc) =
  pure Redomap <*> mapOnCertificates tv cs <*>
       mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
       mapM (mapOnSubExp tv) accexps <*> mapM (mapOnSubExp tv) arrexps <*>
       pure loc

mapOnResType :: (Monad m, Applicative m) =>
                Mapper flore tlore m -> ResType flore -> m (ResType tlore)
mapOnResType tv = mapM mapOnResType'
  where mapOnResType' (Array bt (ExtShape shape) u als) =
          Array bt <$> (ExtShape <$> mapM mapOnExtSize shape) <*>
          return u <*> return als
        mapOnResType' (Basic bt) = return $ Basic bt
        mapOnExtSize (Ext x)   = return $ Ext x
        mapOnExtSize (Free se) = Free <$> mapOnSubExp tv se

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper flore tlore Identity -> Exp flore -> Exp tlore
mapExp m = runIdentity . mapExpM m

-- | Reification of a left-reduction across a syntax tree.
data Folder a lore m = Folder {
    foldOnSubExp :: a -> SubExp lore -> m a
  , foldOnBody :: a -> Body lore -> m a
  , foldOnBinding :: a -> Binding lore -> m a
  , foldOnType :: a -> Type lore -> m a
  , foldOnLambda :: a -> Lambda lore -> m a
  , foldOnIdent :: a -> Ident lore -> m a
  , foldOnValue :: a -> Value -> m a
  , foldOnCertificates :: a -> Certificates lore -> m a
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
    walkOnSubExp :: SubExp lore -> m ()
  , walkOnBody :: Body lore -> m ()
  , walkOnBinding :: Binding lore -> m ()
  , walkOnType :: Type lore -> m ()
  , walkOnLambda :: Lambda lore -> m ()
  , walkOnIdent :: Ident lore -> m ()
  , walkOnValue :: Value -> m ()
  , walkOnCertificates :: Certificates lore -> m ()
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
