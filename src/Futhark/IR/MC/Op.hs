{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Definitions for multicore operations.
--
-- Most of the interesting stuff is in "Futhark.IR.SegOp", which is
-- also re-exported from here.
module Futhark.IR.MC.Op
  ( MCOp (..),
    traverseMCOpStms,
    typeCheckMCOp,
    simplifyMCOp,
    module Futhark.IR.SegOp,
  )
where

import Data.Bifunctor (first)
import Futhark.Analysis.Metrics
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.IR
import Futhark.IR.Aliases (Aliases, CanBeAliased (..))
import Futhark.IR.Mem (OpReturns (..))
import Futhark.IR.Prop.Aliases
import Futhark.IR.SegOp
import Futhark.IR.TypeCheck qualified as TC
import Futhark.Optimise.Simplify qualified as Simplify
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty
  ( nestedBlock,
    pretty,
    stack,
    (<+>),
    (</>),
  )
import Prelude hiding (id, (.))

-- | An operation for the multicore representation.  Feel free to
-- extend this on an ad hoc basis as needed.  Parameterised with some
-- other operation.
data MCOp op rep
  = -- | The first 'SegOp' (if it exists) contains nested parallelism,
    -- while the second one has a fully sequential body.  They are
    -- semantically fully equivalent.
    ParOp
      (Maybe (SegOp () rep))
      (SegOp () rep)
  | -- | Something else (in practice often a SOAC).
    OtherOp (op rep)
  deriving (Eq, Ord, Show)

traverseMCOpStms ::
  (Monad m) =>
  OpStmsTraverser m (op rep) rep ->
  OpStmsTraverser m (MCOp op rep) rep
traverseMCOpStms _ f (ParOp par_op op) =
  ParOp <$> traverse (traverseSegOpStms f) par_op <*> traverseSegOpStms f op
traverseMCOpStms onInner f (OtherOp op) = OtherOp <$> onInner f op

instance (ASTRep rep, Substitute (op rep)) => Substitute (MCOp op rep) where
  substituteNames substs (ParOp par_op op) =
    ParOp (substituteNames substs <$> par_op) (substituteNames substs op)
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op

instance (ASTRep rep, Rename (op rep)) => Rename (MCOp op rep) where
  rename (ParOp par_op op) = ParOp <$> rename par_op <*> rename op
  rename (OtherOp op) = OtherOp <$> rename op

instance (ASTRep rep, FreeIn (op rep)) => FreeIn (MCOp op rep) where
  freeIn' (ParOp par_op op) = freeIn' par_op <> freeIn' op
  freeIn' (OtherOp op) = freeIn' op

instance (IsOp op) => IsOp (MCOp op) where
  safeOp (ParOp _ op) = safeOp op
  safeOp (OtherOp op) = safeOp op

  cheapOp (ParOp _ op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op

  opDependencies (ParOp _ op) = opDependencies op
  opDependencies (OtherOp op) = opDependencies op

instance (TypedOp op) => TypedOp (MCOp op) where
  opType (ParOp _ op) = opType op
  opType (OtherOp op) = opType op

instance (AliasedOp op) => AliasedOp (MCOp op) where
  opAliases (ParOp _ op) = opAliases op
  opAliases (OtherOp op) = opAliases op

  consumedInOp (ParOp _ op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op

instance (CanBeAliased op) => CanBeAliased (MCOp op) where
  addOpAliases aliases (ParOp par_op op) =
    ParOp (addOpAliases aliases <$> par_op) (addOpAliases aliases op)
  addOpAliases aliases (OtherOp op) =
    OtherOp $ addOpAliases aliases op

instance (CanBeWise op) => CanBeWise (MCOp op) where
  addOpWisdom (ParOp par_op op) =
    ParOp (addOpWisdom <$> par_op) (addOpWisdom op)
  addOpWisdom (OtherOp op) =
    OtherOp $ addOpWisdom op

instance (ASTRep rep, ST.IndexOp (op rep)) => ST.IndexOp (MCOp op rep) where
  indexOp vtable k (ParOp _ op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is

instance OpReturns (MCOp NoOp) where
  opReturns (ParOp _ op) = segOpReturns op
  opReturns (OtherOp NoOp) = pure []

instance (PrettyRep rep, Pretty (op rep)) => Pretty (MCOp op rep) where
  pretty (ParOp Nothing op) = pretty op
  pretty (ParOp (Just par_op) op) =
    stack
      [ "par" <+> nestedBlock (pretty par_op),
        "seq" <+> nestedBlock (pretty op)
      ]
  pretty (OtherOp op) = pretty op

instance (OpMetrics (Op rep), OpMetrics (op rep)) => OpMetrics (MCOp op rep) where
  opMetrics (ParOp par_op op) = opMetrics par_op >> opMetrics op
  opMetrics (OtherOp op) = opMetrics op

instance (RephraseOp op) => RephraseOp (MCOp op) where
  rephraseInOp r (ParOp par_op op) =
    ParOp <$> traverse (rephraseInOp r) par_op <*> rephraseInOp r op
  rephraseInOp r (OtherOp op) = OtherOp <$> rephraseInOp r op

typeCheckMCOp ::
  (TC.Checkable rep) =>
  (op (Aliases rep) -> TC.TypeM rep ()) ->
  MCOp op (Aliases rep) ->
  TC.TypeM rep ()
typeCheckMCOp _ (ParOp (Just par_op) op) = do
  -- It is valid for the same array to be consumed in both par_op and op.
  _ <- typeCheckSegOp pure par_op `TC.alternative` typeCheckSegOp pure op
  pure ()
typeCheckMCOp _ (ParOp Nothing op) =
  typeCheckSegOp pure op
typeCheckMCOp f (OtherOp op) = f op

simplifyMCOp ::
  ( Engine.SimplifiableRep rep,
    BodyDec rep ~ ()
  ) =>
  Simplify.SimplifyOp rep (op (Wise rep)) ->
  MCOp op (Wise rep) ->
  Engine.SimpleM rep (MCOp op (Wise rep), Stms (Wise rep))
simplifyMCOp f (OtherOp op) = do
  (op', stms) <- f op
  pure (OtherOp op', stms)
simplifyMCOp _ (ParOp par_op op) = do
  (par_op', par_op_hoisted) <-
    case par_op of
      Nothing -> pure (Nothing, mempty)
      Just x -> first Just <$> simplifySegOp x

  (op', op_hoisted) <- simplifySegOp op

  pure (ParOp par_op' op', par_op_hoisted <> op_hoisted)
