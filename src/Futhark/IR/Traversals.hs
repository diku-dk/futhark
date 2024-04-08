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
module Futhark.IR.Traversals
  ( -- * Mapping
    Mapper (..),
    identityMapper,
    mapExpM,
    mapExp,

    -- * Walking
    Walker (..),
    identityWalker,
    walkExpM,

    -- * Ops
    TraverseOpStms (..),
    OpStmsTraverser,
    traverseLambdaStms,
  )
where

import Control.Monad
import Control.Monad.Identity
import Data.Bitraversable
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Futhark.IR.Prop.Scope
import Futhark.IR.Prop.Types (mapOnType)
import Futhark.IR.Syntax

-- | Express a monad mapping operation on a syntax node.  Each element
-- of this structure expresses the operation to be performed on a
-- given child.
data Mapper frep trep m = Mapper
  { mapOnSubExp :: SubExp -> m SubExp,
    -- | Most bodies are enclosed in a scope, which is passed along
    -- for convenience.
    mapOnBody :: Scope trep -> Body frep -> m (Body trep),
    mapOnVName :: VName -> m VName,
    mapOnRetType :: RetType frep -> m (RetType trep),
    mapOnBranchType :: BranchType frep -> m (BranchType trep),
    mapOnFParam :: FParam frep -> m (FParam trep),
    mapOnLParam :: LParam frep -> m (LParam trep),
    mapOnOp :: Op frep -> m (Op trep)
  }

-- | A mapper that simply returns the tree verbatim.
identityMapper :: forall rep m. (Monad m) => Mapper rep rep m
identityMapper =
  Mapper
    { mapOnSubExp = pure,
      mapOnBody = const pure,
      mapOnVName = pure,
      mapOnRetType = pure,
      mapOnBranchType = pure,
      mapOnFParam = pure,
      mapOnLParam = pure,
      mapOnOp = pure
    }

-- | Map a monadic action across the immediate children of an
-- expression.  Importantly, the mapping does not descend recursively
-- into subexpressions.  The mapping is done left-to-right.
mapExpM ::
  (Monad m) =>
  Mapper frep trep m ->
  Exp frep ->
  m (Exp trep)
mapExpM tv (BasicOp (SubExp se)) =
  BasicOp <$> (SubExp <$> mapOnSubExp tv se)
mapExpM tv (BasicOp (ArrayLit els rowt)) =
  BasicOp
    <$> ( ArrayLit
            <$> mapM (mapOnSubExp tv) els
            <*> mapOnType (mapOnSubExp tv) rowt
        )
mapExpM tv (BasicOp (BinOp bop x y)) =
  BasicOp <$> (BinOp bop <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (CmpOp op x y)) =
  BasicOp <$> (CmpOp op <$> mapOnSubExp tv x <*> mapOnSubExp tv y)
mapExpM tv (BasicOp (ConvOp conv x)) =
  BasicOp <$> (ConvOp conv <$> mapOnSubExp tv x)
mapExpM tv (BasicOp (UnOp op x)) =
  BasicOp <$> (UnOp op <$> mapOnSubExp tv x)
mapExpM tv (Match ses cases defbody (MatchDec ts s)) =
  Match
    <$> mapM (mapOnSubExp tv) ses
    <*> mapM mapOnCase cases
    <*> mapOnBody tv mempty defbody
    <*> (MatchDec <$> mapM (mapOnBranchType tv) ts <*> pure s)
  where
    mapOnCase (Case vs body) = Case vs <$> mapOnBody tv mempty body
mapExpM tv (Apply fname args ret loc) = do
  args' <- forM args $ \(arg, d) ->
    (,) <$> mapOnSubExp tv arg <*> pure d
  Apply fname args' <$> mapM (bitraverse (mapOnRetType tv) pure) ret <*> pure loc
mapExpM tv (BasicOp (Index arr slice)) =
  BasicOp
    <$> ( Index
            <$> mapOnVName tv arr
            <*> traverse (mapOnSubExp tv) slice
        )
mapExpM tv (BasicOp (Update safety arr slice se)) =
  BasicOp
    <$> ( Update safety
            <$> mapOnVName tv arr
            <*> traverse (mapOnSubExp tv) slice
            <*> mapOnSubExp tv se
        )
mapExpM tv (BasicOp (FlatIndex arr slice)) =
  BasicOp
    <$> ( FlatIndex
            <$> mapOnVName tv arr
            <*> traverse (mapOnSubExp tv) slice
        )
mapExpM tv (BasicOp (FlatUpdate arr slice se)) =
  BasicOp
    <$> ( FlatUpdate
            <$> mapOnVName tv arr
            <*> traverse (mapOnSubExp tv) slice
            <*> mapOnVName tv se
        )
mapExpM tv (BasicOp (Iota n x s et)) =
  BasicOp <$> (Iota <$> mapOnSubExp tv n <*> mapOnSubExp tv x <*> mapOnSubExp tv s <*> pure et)
mapExpM tv (BasicOp (Replicate shape vexp)) =
  BasicOp <$> (Replicate <$> mapOnShape tv shape <*> mapOnSubExp tv vexp)
mapExpM tv (BasicOp (Scratch t shape)) =
  BasicOp <$> (Scratch t <$> mapM (mapOnSubExp tv) shape)
mapExpM tv (BasicOp (Reshape kind shape arrexp)) =
  BasicOp
    <$> ( Reshape kind
            <$> mapM (mapOnSubExp tv) shape
            <*> mapOnVName tv arrexp
        )
mapExpM tv (BasicOp (Rearrange perm e)) =
  BasicOp <$> (Rearrange perm <$> mapOnVName tv e)
mapExpM tv (BasicOp (Concat i (x :| ys) size)) = do
  x' <- mapOnVName tv x
  ys' <- mapM (mapOnVName tv) ys
  size' <- mapOnSubExp tv size
  pure $ BasicOp $ Concat i (x' :| ys') size'
mapExpM tv (BasicOp (Manifest perm e)) =
  BasicOp <$> (Manifest perm <$> mapOnVName tv e)
mapExpM tv (BasicOp (Assert e msg loc)) =
  BasicOp <$> (Assert <$> mapOnSubExp tv e <*> traverse (mapOnSubExp tv) msg <*> pure loc)
mapExpM tv (BasicOp (Opaque op e)) =
  BasicOp <$> (Opaque op <$> mapOnSubExp tv e)
mapExpM tv (BasicOp (UpdateAcc safety v is ses)) =
  BasicOp
    <$> ( UpdateAcc safety
            <$> mapOnVName tv v
            <*> mapM (mapOnSubExp tv) is
            <*> mapM (mapOnSubExp tv) ses
        )
mapExpM tv (WithAcc inputs lam) =
  WithAcc <$> mapM onInput inputs <*> mapOnLambda tv lam
  where
    onInput (shape, vs, op) =
      (,,)
        <$> mapOnShape tv shape
        <*> mapM (mapOnVName tv) vs
        <*> traverse (bitraverse (mapOnLambda tv) (mapM (mapOnSubExp tv))) op
mapExpM tv (Loop merge form loopbody) = do
  params' <- mapM (mapOnFParam tv) params
  form' <- mapOnLoopForm tv form
  let scope = scopeOfLoopForm form' <> scopeOfFParams params'
  Loop
    <$> (zip params' <$> mapM (mapOnSubExp tv) args)
    <*> pure form'
    <*> mapOnBody tv scope loopbody
  where
    (params, args) = unzip merge
mapExpM tv (Op op) =
  Op <$> mapOnOp tv op

mapOnShape :: (Monad m) => Mapper frep trep m -> Shape -> m Shape
mapOnShape tv (Shape ds) = Shape <$> mapM (mapOnSubExp tv) ds

mapOnLoopForm ::
  (Monad m) =>
  Mapper frep trep m ->
  LoopForm ->
  m LoopForm
mapOnLoopForm tv (ForLoop i it bound) =
  ForLoop <$> mapOnVName tv i <*> pure it <*> mapOnSubExp tv bound
mapOnLoopForm tv (WhileLoop cond) =
  WhileLoop <$> mapOnVName tv cond

mapOnLambda ::
  (Monad m) =>
  Mapper frep trep m ->
  Lambda frep ->
  m (Lambda trep)
mapOnLambda tv (Lambda params ret body) = do
  params' <- mapM (mapOnLParam tv) params
  Lambda params'
    <$> mapM (mapOnType (mapOnSubExp tv)) ret
    <*> mapOnBody tv (scopeOfLParams params') body

-- | Like 'mapExpM', but in the 'Identity' monad.
mapExp :: Mapper frep trep Identity -> Exp frep -> Exp trep
mapExp m = runIdentity . mapExpM m

-- | Express a monad expression on a syntax node.  Each element of
-- this structure expresses the action to be performed on a given
-- child.
data Walker rep m = Walker
  { walkOnSubExp :: SubExp -> m (),
    walkOnBody :: Scope rep -> Body rep -> m (),
    walkOnVName :: VName -> m (),
    walkOnRetType :: RetType rep -> m (),
    walkOnBranchType :: BranchType rep -> m (),
    walkOnFParam :: FParam rep -> m (),
    walkOnLParam :: LParam rep -> m (),
    walkOnOp :: Op rep -> m ()
  }

-- | A no-op traversal.
identityWalker :: forall rep m. (Monad m) => Walker rep m
identityWalker =
  Walker
    { walkOnSubExp = const $ pure (),
      walkOnBody = const $ const $ pure (),
      walkOnVName = const $ pure (),
      walkOnRetType = const $ pure (),
      walkOnBranchType = const $ pure (),
      walkOnFParam = const $ pure (),
      walkOnLParam = const $ pure (),
      walkOnOp = const $ pure ()
    }

walkOnShape :: (Monad m) => Walker rep m -> Shape -> m ()
walkOnShape tv (Shape ds) = mapM_ (walkOnSubExp tv) ds

walkOnType :: (Monad m) => Walker rep m -> Type -> m ()
walkOnType _ Prim {} = pure ()
walkOnType tv (Acc acc ispace ts _) = do
  walkOnVName tv acc
  traverse_ (walkOnSubExp tv) ispace
  mapM_ (walkOnType tv) ts
walkOnType _ Mem {} = pure ()
walkOnType tv (Array _ shape _) = walkOnShape tv shape

walkOnLoopForm :: (Monad m) => Walker rep m -> LoopForm -> m ()
walkOnLoopForm tv (ForLoop i _ bound) = do
  walkOnVName tv i
  walkOnSubExp tv bound
walkOnLoopForm tv (WhileLoop cond) =
  walkOnVName tv cond

walkOnLambda :: (Monad m) => Walker rep m -> Lambda rep -> m ()
walkOnLambda tv (Lambda params ret body) = do
  mapM_ (walkOnLParam tv) params
  walkOnBody tv (scopeOfLParams params) body
  mapM_ (walkOnType tv) ret

-- | As 'mapExpM', but do not construct a result AST.
walkExpM :: (Monad m) => Walker rep m -> Exp rep -> m ()
walkExpM tv (BasicOp (SubExp se)) =
  walkOnSubExp tv se
walkExpM tv (BasicOp (ArrayLit els rowt)) =
  mapM_ (walkOnSubExp tv) els >> walkOnType tv rowt
walkExpM tv (BasicOp (BinOp _ x y)) =
  walkOnSubExp tv x >> walkOnSubExp tv y
walkExpM tv (BasicOp (CmpOp _ x y)) =
  walkOnSubExp tv x >> walkOnSubExp tv y
walkExpM tv (BasicOp (ConvOp _ x)) =
  walkOnSubExp tv x
walkExpM tv (BasicOp (UnOp _ x)) =
  walkOnSubExp tv x
walkExpM tv (Match ses cases defbody (MatchDec ts _)) = do
  mapM_ (walkOnSubExp tv) ses
  mapM_ (walkOnBody tv mempty . caseBody) cases
  walkOnBody tv mempty defbody
  mapM_ (walkOnBranchType tv) ts
walkExpM tv (Apply _ args ret _) = do
  mapM_ (walkOnSubExp tv . fst) args
  mapM_ (walkOnRetType tv . fst) ret
walkExpM tv (BasicOp (Index arr slice)) =
  walkOnVName tv arr >> traverse_ (walkOnSubExp tv) slice
walkExpM tv (BasicOp (Update _ arr slice se)) =
  walkOnVName tv arr
    >> traverse_ (walkOnSubExp tv) slice
    >> walkOnSubExp tv se
walkExpM tv (BasicOp (FlatIndex arr slice)) =
  walkOnVName tv arr >> traverse_ (walkOnSubExp tv) slice
walkExpM tv (BasicOp (FlatUpdate arr slice se)) =
  walkOnVName tv arr
    >> traverse_ (walkOnSubExp tv) slice
    >> walkOnVName tv se
walkExpM tv (BasicOp (Iota n x s _)) =
  walkOnSubExp tv n >> walkOnSubExp tv x >> walkOnSubExp tv s
walkExpM tv (BasicOp (Replicate shape vexp)) =
  walkOnShape tv shape >> walkOnSubExp tv vexp
walkExpM tv (BasicOp (Scratch _ shape)) =
  mapM_ (walkOnSubExp tv) shape
walkExpM tv (BasicOp (Reshape _ shape arrexp)) =
  mapM_ (walkOnSubExp tv) shape >> walkOnVName tv arrexp
walkExpM tv (BasicOp (Rearrange _ e)) =
  walkOnVName tv e
walkExpM tv (BasicOp (Concat _ (x :| ys) size)) =
  walkOnVName tv x >> mapM_ (walkOnVName tv) ys >> walkOnSubExp tv size
walkExpM tv (BasicOp (Manifest _ e)) =
  walkOnVName tv e
walkExpM tv (BasicOp (Assert e msg _)) =
  walkOnSubExp tv e >> traverse_ (walkOnSubExp tv) msg
walkExpM tv (BasicOp (Opaque _ e)) =
  walkOnSubExp tv e
walkExpM tv (BasicOp (UpdateAcc _ v is ses)) = do
  walkOnVName tv v
  mapM_ (walkOnSubExp tv) is
  mapM_ (walkOnSubExp tv) ses
walkExpM tv (WithAcc inputs lam) = do
  forM_ inputs $ \(shape, vs, op) -> do
    walkOnShape tv shape
    mapM_ (walkOnVName tv) vs
    traverse_ (bitraverse (walkOnLambda tv) (mapM (walkOnSubExp tv))) op
  walkOnLambda tv lam
walkExpM tv (Loop merge form loopbody) = do
  mapM_ (walkOnFParam tv) params
  walkOnLoopForm tv form
  mapM_ (walkOnSubExp tv) args
  let scope = scopeOfFParams params <> scopeOfLoopForm form
  walkOnBody tv scope loopbody
  where
    (params, args) = unzip merge
walkExpM tv (Op op) =
  walkOnOp tv op

-- | A function for monadically traversing any sub-statements of the
-- given op for some representation.
type OpStmsTraverser m op rep = (Scope rep -> Stms rep -> m (Stms rep)) -> op -> m op

-- | This representation supports an 'OpStmsTraverser' for its t'Op'.
-- This is used for some simplification rules.
class TraverseOpStms rep where
  -- | Transform every sub-'Stms' of this op.
  traverseOpStms :: (Monad m) => OpStmsTraverser m (Op rep) rep

-- | A helper for defining 'traverseOpStms'.
traverseLambdaStms :: (Monad m) => OpStmsTraverser m (Lambda rep) rep
traverseLambdaStms f (Lambda ps ret (Body dec stms res)) =
  Lambda ps ret <$> (Body dec <$> f (scopeOfLParams ps) stms <*> pure res)
