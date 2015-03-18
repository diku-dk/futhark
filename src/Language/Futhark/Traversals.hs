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
module Language.Futhark.Traversals
  (
  -- * Mapping
    MapperBase(..)
  , Mapper
  , identityMapper
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
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

import Language.Futhark.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data MapperBase tyf tyt vnf vnt m = Mapper {
    mapOnExp :: ExpBase tyf vnf -> m (ExpBase tyt vnt)
  , mapOnType :: tyf vnf -> m (tyt vnt)
  , mapOnLambda :: LambdaBase tyf vnf -> m (LambdaBase tyt vnt)
  , mapOnPattern :: TupIdentBase tyf vnf -> m (TupIdentBase tyt vnt)
  , mapOnIdent :: IdentBase tyf vnf -> m (IdentBase tyt vnt)
  , mapOnValue :: Value -> m Value
  }

-- | A special case of 'MapperBase' when the name- and type
-- representation does not change.
type Mapper ty vn m = MapperBase ty ty vn vn m

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper ty vn m
identityMapper = Mapper {
                   mapOnExp = return
                 , mapOnType = return
                 , mapOnLambda = return
                 , mapOnPattern = return
                 , mapOnIdent = return
                 , mapOnValue = return
                 }

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) => MapperBase tyf tyt vnf vnt m -> ExpBase tyf vnf -> m (ExpBase tyt vnt)
mapExpM tv (Var ident) =
  pure Var <*> mapOnIdent tv ident
mapExpM tv (Literal val loc) =
  pure Literal <*> mapOnValue tv val <*> pure loc
mapExpM tv (TupLit els loc) =
  pure TupLit <*> mapM (mapOnExp tv) els <*> pure loc
mapExpM tv (ArrayLit els elt loc) =
  pure ArrayLit <*> mapM (mapOnExp tv) els <*> mapOnType tv elt <*> pure loc
mapExpM tv (BinOp bop x y t loc) =
  pure (BinOp bop) <*>
         mapOnExp tv x <*> mapOnExp tv y <*>
         mapOnType tv t <*> pure loc
mapExpM tv (UnOp unop x loc) =
  pure (UnOp unop) <*> mapOnExp tv x <*> pure loc
mapExpM tv (If c texp fexp t loc) =
  pure If <*> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*>
       mapOnType tv t <*> pure loc
mapExpM tv (Apply fname args t loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*> mapOnType tv t <*> pure loc
mapExpM tv (LetPat pat e body loc) =
  pure LetPat <*> mapOnPattern tv pat <*> mapOnExp tv e <*>
         mapOnExp tv body <*> pure loc
mapExpM tv (LetWith dest src idxexps vexp body loc) =
  pure LetWith <*>
       mapOnIdent tv dest <*> mapOnIdent tv src <*>
       mapM (mapOnExp tv) idxexps <*> mapOnExp tv vexp <*>
       mapOnExp tv body <*> pure loc
mapExpM tv (Index arr idxexps loc) =
  pure Index <*>
       mapOnIdent tv arr <*>
       mapM (mapOnExp tv) idxexps <*>
       pure loc
mapExpM tv (Iota nexp loc) =
  pure Iota <*> mapOnExp tv nexp <*> pure loc
mapExpM tv (Size i e loc) =
  pure Size <*> pure i <*> mapOnExp tv e <*> pure loc
mapExpM tv (Replicate nexp vexp loc) =
  pure Replicate <*> mapOnExp tv nexp <*> mapOnExp tv vexp <*> pure loc
mapExpM tv (Reshape shape arrexp loc) =
  pure Reshape <*> mapM (mapOnExp tv) shape <*>
                   mapOnExp tv arrexp <*> pure loc
mapExpM tv (Transpose k n e3 loc) =
  pure Transpose <*>
       pure k <*> pure n <*>
       mapOnExp tv e3 <*> pure loc
mapExpM tv (Rearrange perm e loc) =
  pure Rearrange <*> pure perm <*> mapOnExp tv e <*> pure loc
mapExpM tv (Map fun e loc) =
  pure Map <*> mapOnLambda tv fun <*> mapOnExp tv e <*> pure loc
mapExpM tv (ConcatMap fun e es loc) =
  pure ConcatMap <*> mapOnLambda tv fun <*>
  mapOnExp tv e <*> mapM (mapOnExp tv) es <*> pure loc
mapExpM tv (Reduce fun startexp arrexp loc) =
  pure Reduce <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Zip args loc) = do
  args' <- forM args $ \(argexp, argt) -> do
                              argexp' <- mapOnExp tv argexp
                              argt' <- mapOnType tv argt
                              pure (argexp', argt')
  pure $ Zip args' loc
mapExpM tv (Unzip e ts loc) =
  pure Unzip <*> mapOnExp tv e <*> mapM (mapOnType tv) ts <*> pure loc
mapExpM tv (Scan fun startexp arrexp loc) =
  pure Scan <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
       pure loc
mapExpM tv (Filter fun arrexp loc) =
  pure Filter <*> mapOnLambda tv fun <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Partition funs arrexp loc) =
  pure Partition <*> mapM (mapOnLambda tv) funs <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Redomap redfun mapfun accexp arrexp loc) =
  pure Redomap <*> mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
       mapOnExp tv accexp <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Split splitexps arrexp loc) =
  pure Split <*>
       mapM (mapOnExp tv) splitexps <*> mapOnExp tv arrexp <*>
       pure loc
mapExpM tv (Concat x ys loc) =
  pure Concat <*>
       mapOnExp tv x <*> mapM (mapOnExp tv) ys <*> pure loc
mapExpM tv (Copy e loc) =
  pure Copy <*> mapOnExp tv e <*> pure loc
mapExpM tv (DoLoop mergepat mergeexp (ForLoop loopvar boundexp) loopbody letbody loc) =
  pure DoLoop <*> mapOnPattern tv mergepat <*> mapOnExp tv mergeexp <*>
       (ForLoop <$> mapOnIdent tv loopvar <*> mapOnExp tv boundexp) <*>
       mapOnExp tv loopbody <*> mapOnExp tv letbody <*> pure loc
mapExpM tv (DoLoop mergepat mergeexp (WhileLoop cond) loopbody letbody loc) =
  pure DoLoop <*> mapOnPattern tv mergepat <*> mapOnExp tv mergeexp <*>
       (WhileLoop <$> mapOnExp tv cond) <*>
       mapOnExp tv loopbody <*> mapOnExp tv letbody <*> pure loc

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper ty vn Identity -> ExpBase ty vn -> ExpBase ty vn
mapExp m = runIdentity . mapExpM m

-- | Reification of a left-reduction across a syntax tree.
data Folder ty vn a m = Folder {
    foldOnExp :: a -> ExpBase ty vn -> m a
  , foldOnType :: a -> ty vn -> m a
  , foldOnLambda :: a -> LambdaBase ty vn -> m a
  , foldOnPattern :: a -> TupIdentBase ty vn -> m a
  , foldOnIdent :: a -> IdentBase ty vn -> m a
  , foldOnValue :: a -> Value -> m a
  }

-- | A folding operation where the accumulator is returned verbatim.
identityFolder :: Monad m => Folder ty vn a m
identityFolder = Folder {
                   foldOnExp = const . return
                 , foldOnType = const . return
                 , foldOnLambda = const . return
                 , foldOnPattern = const . return
                 , foldOnIdent = const . return
                 , foldOnValue = const . return
                 }

-- | Perform a left-reduction across the immediate children of an
-- expression.  Importantly, the 'foldOnExp' action is not invoked for
-- the expression itself, and the reduction does not descend recursively
-- into subexpressions.  The reduction is done left-to-right.
foldExpM :: (Monad m, Functor m) => Folder ty vn a m -> a -> ExpBase ty vn -> m a
foldExpM f x e = execStateT (mapExpM m e) x
  where m = Mapper {
              mapOnExp = wrap foldOnExp
            , mapOnType = wrap foldOnType
            , mapOnLambda = wrap foldOnLambda
            , mapOnPattern = wrap foldOnPattern
            , mapOnIdent = wrap foldOnIdent
            , mapOnValue = wrap foldOnValue
            }
        wrap op k = do
          v <- get
          put =<< lift (op f v k)
          return k

-- | As 'foldExpM', but in the 'Identity' monad.
foldExp :: Folder ty vn a Identity -> a -> ExpBase ty vn -> a
foldExp m x = runIdentity . foldExpM m x

-- | Express a monad expression on a syntax node.  Each element of
-- this structure expresses the action to be performed on a given
-- child.
data Walker ty vn m = Walker {
    walkOnExp :: ExpBase ty vn -> m ()
  , walkOnType :: ty vn -> m ()
  , walkOnLambda :: LambdaBase ty vn -> m ()
  , walkOnPattern :: TupIdentBase ty vn -> m ()
  , walkOnIdent :: IdentBase ty vn -> m ()
  , walkOnValue :: Value -> m ()
  }

-- | A no-op traversal.
identityWalker :: Monad m => Walker ty vn m
identityWalker = Walker {
                   walkOnExp = const $ return ()
                 , walkOnType = const $ return ()
                 , walkOnLambda = const $ return ()
                 , walkOnPattern = const $ return ()
                 , walkOnIdent = const $ return ()
                 , walkOnValue = const $ return ()
                 }

-- | Perform a monadic action on each of the immediate children of an
-- expression.  Importantly, the 'walkOnExp' action is not invoked for
-- the expression itself, and the traversal does not descend
-- recursively into subexpressions.  The traversal is done
-- left-to-right.
walkExpM :: (Monad m, Applicative m) => Walker ty vn m -> ExpBase ty vn -> m ()
walkExpM f = void . mapExpM m
  where m = Mapper {
              mapOnExp = wrap walkOnExp
            , mapOnType = wrap walkOnType
            , mapOnLambda = wrap walkOnLambda
            , mapOnPattern = wrap walkOnPattern
            , mapOnIdent = wrap walkOnIdent
            , mapOnValue = wrap walkOnValue
            }
        wrap op k = op f k >> return k
