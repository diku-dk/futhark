{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.IR
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SegOp
import qualified Futhark.IR.TypeCheck as TC
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import Futhark.Util.Pretty
  ( Pretty,
    nestedBlock,
    ppr,
    (<+>),
    (</>),
  )
import Prelude hiding (id, (.))

-- | An operation for the multicore representation.  Feel free to
-- extend this on an ad hoc basis as needed.  Parameterised with some
-- other operation.
data MCOp rep op
  = -- | The first 'SegOp' (if it exists) contains nested parallelism,
    -- while the second one has a fully sequential body.  They are
    -- semantically fully equivalent.
    ParOp
      (Maybe (SegOp () rep))
      (SegOp () rep)
  | -- | Something else (in practice often a SOAC).
    OtherOp op
  deriving (Eq, Ord, Show)

traverseMCOpStms :: Monad m => OpStmsTraverser m op rep -> OpStmsTraverser m (MCOp rep op) rep
traverseMCOpStms _ f (ParOp par_op op) =
  ParOp <$> traverse (traverseSegOpStms f) par_op <*> traverseSegOpStms f op
traverseMCOpStms onInner f (OtherOp op) = OtherOp <$> onInner f op

instance (ASTRep rep, Substitute op) => Substitute (MCOp rep op) where
  substituteNames substs (ParOp par_op op) =
    ParOp (substituteNames substs <$> par_op) (substituteNames substs op)
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op

instance (ASTRep rep, Rename op) => Rename (MCOp rep op) where
  rename (ParOp par_op op) = ParOp <$> rename par_op <*> rename op
  rename (OtherOp op) = OtherOp <$> rename op

instance (ASTRep rep, FreeIn op) => FreeIn (MCOp rep op) where
  freeIn' (ParOp par_op op) = freeIn' par_op <> freeIn' op
  freeIn' (OtherOp op) = freeIn' op

instance (ASTRep rep, IsOp op) => IsOp (MCOp rep op) where
  safeOp (ParOp _ op) = safeOp op
  safeOp (OtherOp op) = safeOp op

  cheapOp (ParOp _ op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op

instance TypedOp op => TypedOp (MCOp rep op) where
  opType (ParOp _ op) = opType op
  opType (OtherOp op) = opType op

instance
  (Aliased rep, AliasedOp op, ASTRep rep) =>
  AliasedOp (MCOp rep op)
  where
  opAliases (ParOp _ op) = opAliases op
  opAliases (OtherOp op) = opAliases op

  consumedInOp (ParOp _ op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op

instance
  (CanBeAliased (Op rep), CanBeAliased op, ASTRep rep) =>
  CanBeAliased (MCOp rep op)
  where
  type OpWithAliases (MCOp rep op) = MCOp (Aliases rep) (OpWithAliases op)

  addOpAliases aliases (ParOp par_op op) =
    ParOp (addOpAliases aliases <$> par_op) (addOpAliases aliases op)
  addOpAliases aliases (OtherOp op) =
    OtherOp $ addOpAliases aliases op

  removeOpAliases (ParOp par_op op) =
    ParOp (removeOpAliases <$> par_op) (removeOpAliases op)
  removeOpAliases (OtherOp op) =
    OtherOp $ removeOpAliases op

instance
  (CanBeWise (Op rep), CanBeWise op, ASTRep rep) =>
  CanBeWise (MCOp rep op)
  where
  type OpWithWisdom (MCOp rep op) = MCOp (Wise rep) (OpWithWisdom op)

  removeOpWisdom (ParOp par_op op) =
    ParOp (removeOpWisdom <$> par_op) (removeOpWisdom op)
  removeOpWisdom (OtherOp op) =
    OtherOp $ removeOpWisdom op

  addOpWisdom (ParOp par_op op) =
    ParOp (addOpWisdom <$> par_op) (addOpWisdom op)
  addOpWisdom (OtherOp op) =
    OtherOp $ addOpWisdom op

instance (ASTRep rep, ST.IndexOp op) => ST.IndexOp (MCOp rep op) where
  indexOp vtable k (ParOp _ op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is

instance (PrettyRep rep, Pretty op) => Pretty (MCOp rep op) where
  ppr (ParOp Nothing op) = ppr op
  ppr (ParOp (Just par_op) op) =
    "par" <+> nestedBlock "{" "}" (ppr par_op)
      </> "seq" <+> nestedBlock "{" "}" (ppr op)
  ppr (OtherOp op) = ppr op

instance (OpMetrics (Op rep), OpMetrics op) => OpMetrics (MCOp rep op) where
  opMetrics (ParOp par_op op) = opMetrics par_op >> opMetrics op
  opMetrics (OtherOp op) = opMetrics op

typeCheckMCOp ::
  TC.Checkable rep =>
  (op -> TC.TypeM rep ()) ->
  MCOp (Aliases rep) op ->
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
  Simplify.SimplifyOp rep op ->
  MCOp (Wise rep) op ->
  Engine.SimpleM rep (MCOp (Wise rep) op, Stms (Wise rep))
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
