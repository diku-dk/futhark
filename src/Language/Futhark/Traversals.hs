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
module Language.Futhark.Traversals
  (
  -- * Mapping
    MapperBase(..)
  , mapExpM
  )
  where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (mapM, forM)

import Prelude

import Language.Futhark.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data MapperBase vnf vnt m = Mapper {
    mapOnExp :: ExpBase NoInfo vnf -> m (ExpBase NoInfo vnt)
  , mapOnLambda :: LambdaBase NoInfo vnf -> m (LambdaBase NoInfo vnt)
  , mapOnType :: CompTypeBase vnf -> m (CompTypeBase vnt)
  , mapOnPattern :: PatternBase NoInfo vnf -> m (PatternBase NoInfo vnt)
  , mapOnIdent :: IdentBase NoInfo vnf -> m (IdentBase NoInfo vnt)
  , mapOnName :: vnf -> m vnt
  , mapOnValue :: Value -> m Value
  }

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the 'mapOnExp' action is not invoked for
-- the expression itself, and the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM :: (Applicative m, Monad m) =>
           MapperBase vnf vnt m -> ExpBase NoInfo vnf -> m (ExpBase NoInfo vnt)
mapExpM tv (Var ident) =
  pure Var <*> mapOnIdent tv ident
mapExpM tv (Literal val loc) =
  pure Literal <*> mapOnValue tv val <*> pure loc
mapExpM tv (TupLit els loc) =
  pure TupLit <*> mapM (mapOnExp tv) els <*> pure loc
mapExpM tv (ArrayLit els elt loc) =
  pure ArrayLit <*> mapM (mapOnExp tv) els <*> mapTypeM tv elt <*> pure loc
mapExpM tv (Empty (TypeDecl ut NoInfo) loc) =
  pure Empty <*> (TypeDecl <$> mapUserTypeM tv ut <*> pure NoInfo) <*> pure loc
mapExpM tv (BinOp bop x y t loc) =
  pure (BinOp bop) <*>
         mapOnExp tv x <*> mapOnExp tv y <*>
         mapTypeM tv t <*> pure loc
mapExpM tv (UnOp unop x loc) =
  pure (UnOp unop) <*> mapOnExp tv x <*> pure loc
mapExpM tv (If c texp fexp t loc) =
  pure If <*> mapOnExp tv c <*> mapOnExp tv texp <*> mapOnExp tv fexp <*>
       mapTypeM tv t <*> pure loc
mapExpM tv (Apply fname args t loc) = do
  args' <- forM args $ \(arg, d) ->
             (,) <$> mapOnExp tv arg <*> pure d
  pure (Apply fname) <*> pure args' <*> mapTypeM tv t <*> pure loc
mapExpM tv (LetPat pat e body loc) =
  pure LetPat <*> mapOnPattern tv pat <*> mapOnExp tv e <*>
         mapOnExp tv body <*> pure loc
mapExpM tv (LetWith dest src idxexps vexp body loc) =
  pure LetWith <*>
       mapOnIdent tv dest <*> mapOnIdent tv src <*>
       mapM (mapDimIndexM tv) idxexps <*> mapOnExp tv vexp <*>
       mapOnExp tv body <*> pure loc
mapExpM tv (Update v idxexps vexp loc) =
  pure Update <*> mapOnIdent tv v <*>
       mapM (mapDimIndexM tv) idxexps <*> mapOnExp tv vexp <*> pure loc
mapExpM tv (Index arr idxexps loc) =
  pure Index <*>
       mapOnExp tv arr <*>
       mapM (mapDimIndexM tv) idxexps <*>
       pure loc
mapExpM tv (TupleIndex e i NoInfo loc) =
  TupleIndex <$> mapOnExp tv e <*> pure i <*> pure NoInfo <*> pure loc
mapExpM tv (Iota nexp loc) =
  pure Iota <*> mapOnExp tv nexp <*> pure loc
mapExpM tv (Shape e loc) =
  pure Shape <*> mapOnExp tv e <*> pure loc
mapExpM tv (Replicate nexp vexp loc) =
  pure Replicate <*> mapOnExp tv nexp <*> mapOnExp tv vexp <*> pure loc
mapExpM tv (Reshape shape arrexp loc) =
  pure Reshape <*> mapM (mapOnExp tv) shape <*>
                   mapOnExp tv arrexp <*> pure loc
mapExpM tv (Transpose e loc) =
  Transpose <$> mapOnExp tv e <*> pure loc
mapExpM tv (Rotate d e a loc) =
  Rotate d <$> mapOnExp tv e <*> mapOnExp tv a <*> pure loc
mapExpM tv (Rearrange perm e loc) =
  pure Rearrange <*> pure perm <*> mapOnExp tv e <*> pure loc
mapExpM tv (Map fun e loc) =
  pure Map <*> mapOnLambda tv fun <*> mapOnExp tv e <*> pure loc
mapExpM tv (Reduce comm fun startexp arrexp loc) =
  Reduce comm <$> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Zip i e es loc) =
  Zip i <$> mapOnExp tv e <*> mapM (mapOnExp tv) es <*> pure loc
mapExpM tv (Unzip e ts loc) =
  pure Unzip <*> mapOnExp tv e <*> mapM (mapTypeM tv) ts <*> pure loc
mapExpM tv (Unsafe e loc) =
  pure Unsafe <*> mapOnExp tv e <*> pure loc
mapExpM tv (Scan fun startexp arrexp loc) =
  pure Scan <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
       pure loc
mapExpM tv (Filter fun arrexp loc) =
  pure Filter <*> mapOnLambda tv fun <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Partition funs arrexp loc) =
  pure Partition <*> mapM (mapOnLambda tv) funs <*> mapOnExp tv arrexp <*> pure loc
mapExpM tv (Stream form fun arr loc) =
  pure Stream <*> mapOnStreamForm form <*> mapOnLambda tv fun <*>
       mapOnExp tv arr <*> pure loc
  where mapOnStreamForm (MapLike o) = pure $ MapLike o
        mapOnStreamForm (RedLike o comm lam acc) =
            RedLike o comm <$>
            mapOnLambda tv lam <*>
            mapOnExp tv acc
        mapOnStreamForm (Sequential acc) =
            pure Sequential <*> mapOnExp tv acc
mapExpM tv (Split i splitexps arrexp loc) =
  Split i <$>
  mapM (mapOnExp tv) splitexps <*> mapOnExp tv arrexp <*>
  pure loc
mapExpM tv (Concat i x ys loc) =
  Concat i <$> mapOnExp tv x <*> mapM (mapOnExp tv) ys <*> pure loc
mapExpM tv (Copy e loc) =
  pure Copy <*> mapOnExp tv e <*> pure loc
mapExpM tv (DoLoop mergepat mergeexp form loopbody letbody loc) =
  pure DoLoop <*> mapOnPattern tv mergepat <*> mapOnExp tv mergeexp <*>
       mapLoopFormM tv form <*>
       mapOnExp tv loopbody <*> mapOnExp tv letbody <*> pure loc
mapExpM tv (Write i v as loc) =
  Write <$> mapOnExp tv i <*> mapOnExp tv v <*> mapM (mapOnExp tv) as <*> pure loc

mapLoopFormM :: (Applicative m, Monad m) =>
                MapperBase vnf vnt m
             -> LoopFormBase NoInfo vnf
             -> m (LoopFormBase NoInfo vnt)
mapLoopFormM tv (For FromUpTo lbound i ubound) =
  For FromUpTo <$> mapOnExp tv lbound <*> mapOnIdent tv i <*> mapOnExp tv ubound
mapLoopFormM tv (For FromDownTo lbound i ubound) =
  For FromDownTo <$> mapOnExp tv lbound <*> mapOnIdent tv i <*> mapOnExp tv ubound
mapLoopFormM tv (While e) =
  While <$> mapOnExp tv e

mapUserTypeM :: (Applicative m, Monad m) =>
                MapperBase vnf vnt m
             -> UserType vnf
             -> m (UserType vnt)
mapUserTypeM _ (UserPrim bt loc) = pure $ UserPrim bt loc
mapUserTypeM tv (UserUnique t loc) = UserUnique <$> mapUserTypeM tv t <*> pure loc
mapUserTypeM tv (UserArray t d loc) =
  UserArray <$> mapUserTypeM tv t <*> mapDimDecl tv d <*> pure loc
mapUserTypeM tv (UserTuple ts loc) =
  UserTuple <$> mapM (mapUserTypeM tv) ts <*> pure loc
mapUserTypeM _ (UserTypeAlias name loc) =
  pure $ UserTypeAlias name loc

mapDimDecl :: (Applicative m, Monad m) =>
              MapperBase vnf vnt m
           -> DimDecl vnf
           -> m (DimDecl vnt)
mapDimDecl tv (NamedDim vn) = NamedDim <$> mapOnName tv vn
mapDimDecl _ (ConstDim k) = pure $ ConstDim k
mapDimDecl _ AnyDim = pure AnyDim

mapDimIndexM :: (Applicative m, Monad m) =>
                MapperBase vnf vnt m
             -> DimIndexBase NoInfo vnf
             -> m (DimIndexBase NoInfo vnt)
mapDimIndexM tv (DimFix j) = DimFix <$> mapExpM tv j
mapDimIndexM tv (DimSlice i j) = DimSlice <$> mapExpM tv i <*> mapExpM tv j

mapTypeM :: (Applicative m, Monad m, Traversable f) =>
            MapperBase vnf vnt m
         -> f (CompTypeBase vnf) -> m (f (CompTypeBase vnt))
mapTypeM = traverse . mapOnType
