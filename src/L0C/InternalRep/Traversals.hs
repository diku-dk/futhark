-----------------------------------------------------------------------------
-- |
--
-- Functions for generic traversals across L0 syntax trees.  The
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
-- Instead, this module defines various traversals of the L0 syntax
-- tree.  The implementation is rather tedious, but the interface is
-- easy to use.
--
-- A traversal of the L0 syntax tree is expressed as a tuple of
-- functions expressing the operations to be performed on the various
-- types of nodes.
--
-- The "L0C.Renamer" and "L0C.Untrace" modules are simple examples of
-- how to use this facility.
--
-----------------------------------------------------------------------------
module L0C.InternalRep.Traversals
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
  , buildExpPattern
  , foldlPattern
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

import L0C.InternalRep.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper m = Mapper {
    mapOnSubExp :: SubExp -> m SubExp
  , mapOnBody :: Body -> m Body
  , mapOnExp :: Exp -> m Exp
  , mapOnType :: Type -> m Type
  , mapOnLambda :: Lambda -> m Lambda
  , mapOnIdent :: Ident -> m Ident
  , mapOnValue :: Value -> m Value
  , mapOnCertificates :: Certificates -> m Certificates
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: Monad m => Mapper m
identityMapper = Mapper {
                   mapOnSubExp = return
                 , mapOnExp = return
                 , mapOnBody = return
                 , mapOnType = return
                 , mapOnLambda = return
                 , mapOnIdent = return
                 , mapOnValue = return
                 , mapOnCertificates = return
                 }

mapBodyM :: (Applicative m, Monad m) => Mapper m -> Body -> m Body
mapBodyM tv (LetPat pat e body loc) =
  pure LetPat <*> mapM (mapOnIdent tv) pat <*> mapOnExp tv e <*>
       mapOnBody tv body <*> pure loc
mapBodyM tv (LetWith cs dest src idxcs idxexps vexp body loc) =
  pure LetWith <*> mapOnCertificates tv cs <*>
       mapOnIdent tv dest <*> mapOnIdent tv src <*>
       (case idxcs of
          Nothing -> return Nothing
          Just idxcs' -> Just <$> mapOnCertificates tv idxcs') <*>
       mapM (mapOnSubExp tv) idxexps <*> mapOnSubExp tv vexp <*>
       mapOnBody tv body <*> pure loc
mapBodyM tv (DoLoop mergepat loopvar boundexp loopbody letbody loc) =
  pure DoLoop <*>
       (zip <$> mapM (mapOnIdent tv) vs <*> mapM (mapOnSubExp tv) es) <*>
       mapOnIdent tv loopvar <*> mapOnSubExp tv boundexp <*>
       mapOnBody tv loopbody <*> mapOnBody tv letbody <*> pure loc
  where (vs,es) = unzip mergepat
mapBodyM tv (Result ses loc) =
  Result <$> mapM (mapOnSubExp tv) ses <*> pure loc

-- | Like 'mapBodyM', but in the 'Identity' monad.
mapBody :: Mapper Identity -> Body -> Body
mapBody m = runIdentity . mapBodyM m

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) => Mapper m -> Exp -> m Exp
mapExpM tv (SubExp e) =
  pure SubExp <*> mapOnSubExp tv e
mapExpM tv (TupLit els loc) =
  pure TupLit <*> mapM (mapOnSubExp tv) els <*> pure loc
mapExpM tv (ArrayLit els elt loc) =
  pure ArrayLit <*> mapM (mapOnSubExp tv) els <*> mapOnType tv elt <*> pure loc
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
       mapM (mapOnType tv) t <*> pure loc
mapExpM tv (Apply fname args t loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnSubExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*> mapM (mapOnType tv) t <*> pure loc
mapExpM tv (Index cs arr idxcs idxexps loc) =
  pure Index <*> mapOnCertificates tv cs <*>
       mapOnIdent tv arr <*>
       (case idxcs of
          Nothing -> return Nothing
          Just idxcs' -> Just <$> mapOnCertificates tv idxcs') <*>
       mapM (mapOnSubExp tv) idxexps <*>
       pure loc
mapExpM tv (Iota nexp loc) =
  pure Iota <*> mapOnSubExp tv nexp <*> pure loc
mapExpM tv (Size cs i e loc) =
  pure Size <*> mapOnCertificates tv cs <*>
       pure i <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Replicate nexp vexp loc) =
  pure Replicate <*> mapOnSubExp tv nexp <*> mapOnSubExp tv vexp <*> pure loc
mapExpM tv (Reshape cs shape arrexp loc) =
  pure Reshape <*> mapOnCertificates tv cs <*>
       mapM (mapOnSubExp tv) shape <*>
       mapOnSubExp tv arrexp <*> pure loc
mapExpM tv (Transpose cs k n e3 loc) =
  pure Transpose <*> mapOnCertificates tv cs <*>
       pure k <*> pure n <*>
       mapOnSubExp tv e3 <*> pure loc
mapExpM tv (Split cs nexp arrexp loc) =
  pure Split <*> mapOnCertificates tv cs <*>
       mapOnSubExp tv nexp <*> mapOnSubExp tv arrexp <*>
       pure loc
mapExpM tv (Concat cs x y loc) =
  pure Concat <*> mapOnCertificates tv cs <*>
       mapOnSubExp tv x <*> mapOnSubExp tv y <*> pure loc
mapExpM tv (Copy e loc) =
  pure Copy <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Assert e loc) =
  pure Assert <*> mapOnSubExp tv e <*> pure loc
mapExpM tv (Conjoin es loc) =
  pure Conjoin <*> mapM (mapOnSubExp tv) es <*> pure loc
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
       mapM (mapOnSubExp tv) arrexps <*> pure loc
mapExpM tv (Redomap cs redfun mapfun accexps arrexps loc) =
  pure Redomap <*> mapOnCertificates tv cs <*>
       mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
       mapM (mapOnSubExp tv) accexps <*> mapM (mapOnSubExp tv) arrexps <*>
       pure loc

-- | Like 'mapExp', but in the 'Identity' monad.
mapExp :: Mapper Identity -> Exp -> Exp
mapExp m = runIdentity . mapExpM m

-- | Reification of a left-reduction across a syntax tree.
data Folder a m = Folder {
    foldOnSubExp :: a -> SubExp -> m a
  , foldOnBody :: a -> Body -> m a
  , foldOnExp :: a -> Exp -> m a
  , foldOnType :: a -> Type -> m a
  , foldOnLambda :: a -> Lambda -> m a
  , foldOnIdent :: a -> Ident -> m a
  , foldOnValue :: a -> Value -> m a
  , foldOnCertificates :: a -> Certificates -> m a
  }

-- | A folding operation where the accumulator is returned verbatim.
identityFolder :: Monad m => Folder a m
identityFolder = Folder {
                   foldOnSubExp = const . return
                 , foldOnBody = const . return
                 , foldOnExp = const . return
                 , foldOnType = const . return
                 , foldOnLambda = const . return
                 , foldOnIdent = const . return
                 , foldOnValue = const . return
                 , foldOnCertificates = const . return
                 }

foldMapper :: Monad m => Folder a m -> Mapper (StateT a m)
foldMapper f = Mapper {
                 mapOnSubExp = wrap foldOnSubExp
               , mapOnBody = wrap foldOnBody
               , mapOnExp = wrap foldOnExp
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
foldBodyM :: (Monad m, Functor m) => Folder a m -> a -> Body -> m a
foldBodyM f x e = execStateT (mapBodyM (foldMapper f) e) x

-- | As 'foldBodyM', but in the 'Identity' monad.
foldBody :: Folder a Identity -> a -> Body -> a
foldBody m x = runIdentity . foldBodyM m x

-- | As 'foldBodyM', but for expressions.
foldExpM :: (Monad m, Functor m) => Folder a m -> a -> Exp -> m a
foldExpM f x e = execStateT (mapExpM (foldMapper f) e) x

-- | As 'foldExpM', but in the 'Identity' monad.
foldExp :: Folder a Identity -> a -> Exp -> a
foldExp m x = runIdentity . foldExpM m x

-- | Express a monad expression on a syntax node.  Each element of
-- this structure expresses the action to be performed on a given
-- child.
data Walker m = Walker {
    walkOnSubExp :: SubExp -> m ()
  , walkOnBody :: Body -> m ()
  , walkOnExp :: Exp -> m ()
  , walkOnType :: Type -> m ()
  , walkOnLambda :: Lambda -> m ()
  , walkOnTupleLambda :: Lambda -> m ()
  , walkOnIdent :: Ident -> m ()
  , walkOnValue :: Value -> m ()
  , walkOnCertificates :: Certificates -> m ()
  }

-- | A no-op traversal.
identityWalker :: Monad m => Walker m
identityWalker = Walker {
                   walkOnSubExp = const $ return ()
                 , walkOnBody = const $ return ()
                 , walkOnExp = const $ return ()
                 , walkOnType = const $ return ()
                 , walkOnLambda = const $ return ()
                 , walkOnTupleLambda = const $ return ()
                 , walkOnIdent = const $ return ()
                 , walkOnValue = const $ return ()
                 , walkOnCertificates = const $ return ()
                 }

walkMapper :: Monad m => Walker m -> Mapper m
walkMapper f = Mapper {
                 mapOnSubExp = wrap walkOnSubExp
               , mapOnBody = wrap walkOnBody
               , mapOnExp = wrap walkOnExp
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
walkBodyM :: (Monad m, Applicative m) => Walker m -> Body -> m ()
walkBodyM f = void . mapBodyM m
  where m = walkMapper f

-- | As 'walkBodyM', but for expressions.
walkExpM :: (Monad m, Applicative m) => Walker m -> Exp -> m ()
walkExpM f = void . mapExpM m
  where m = walkMapper f

-- | Common case of 'mapExp', where only 'Exp's are taken into
-- account.
buildExpPattern :: (Exp -> Exp) -> Exp -> Exp
buildExpPattern f = mapExp f'
  where f' = identityMapper {
               mapOnExp = return . f
             , mapOnBody = return . mapBody f'
             , mapOnLambda = return . buildLambda
             }

        buildLambda (Lambda tps body tp loc) =
          Lambda tps (mapBody f' body) tp loc

-- | Common case of 'foldExp', where only 'Exp's and 'Lambda's are
-- taken into account.
foldlPattern :: (a -> Exp    -> a) ->
                (a -> Lambda -> a) ->
                a -> Exp -> a
foldlPattern expf lamf = foldExp m
  where m = identityFolder {
              foldOnExp = \x -> return . expf x
            , foldOnBody = \x -> return . foldBody m x
            , foldOnLambda =
              \x lam@(Lambda _ body _ _) ->
                return $ foldBody m (lamf x lam) body
            }
