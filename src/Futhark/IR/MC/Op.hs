{-# LANGUAGE DeriveGeneric #-}
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
    typeCheckMCOp,
    simplifyMCOp,
    module Futhark.IR.SegOp,
  )
where

import Control.Category
import Data.Bifunctor (first)
import Futhark.Analysis.Metrics
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.IR
import Futhark.IR.Aliases (Aliases)
import Futhark.IR.Prop.Aliases
import Futhark.IR.SegOp
import qualified Futhark.Optimise.Simplify as Simplify
import qualified Futhark.Optimise.Simplify.Engine as Engine
import Futhark.Optimise.Simplify.Lore
import Futhark.Transform.Rename
import Futhark.Transform.Substitute
import qualified Futhark.TypeCheck as TC
import Futhark.Util.Pretty
  ( Pretty,
    nestedBlock,
    ppr,
    (<+>),
    (</>),
  )
import GHC.Generics (Generic)
import Language.SexpGrammar as Sexp
import Language.SexpGrammar.Generic
import Prelude hiding (id, (.))

-- | An operation for the multicore representation.  Feel free to
-- extend this on an ad hoc basis as needed.  Parameterised with some
-- other operation.
data MCOp lore op
  = -- | The first 'SegOp' (if it exists) contains nested parallelism,
    -- while the second one has a fully sequential body.  They are
    -- semantically fully equivalent.
    ParOp
      (Maybe (SegOp () lore))
      (SegOp () lore)
  | -- | Something else (in practice often a SOAC).
    OtherOp op
  deriving (Eq, Ord, Show, Generic)

instance (Decorations lore, SexpIso op) => SexpIso (MCOp lore op) where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el sexpIso >>> Sexp.el sexpIso)) $
        With
          (. Sexp.list (Sexp.el sexpIso))
          End

instance (ASTLore lore, Substitute op) => Substitute (MCOp lore op) where
  substituteNames substs (ParOp par_op op) =
    ParOp (substituteNames substs <$> par_op) (substituteNames substs op)
  substituteNames substs (OtherOp op) =
    OtherOp $ substituteNames substs op

instance (ASTLore lore, Rename op) => Rename (MCOp lore op) where
  rename (ParOp par_op op) = ParOp <$> rename par_op <*> rename op
  rename (OtherOp op) = OtherOp <$> rename op

instance (ASTLore lore, FreeIn op) => FreeIn (MCOp lore op) where
  freeIn' (ParOp par_op op) = freeIn' par_op <> freeIn' op
  freeIn' (OtherOp op) = freeIn' op

instance (ASTLore lore, IsOp op) => IsOp (MCOp lore op) where
  safeOp (ParOp _ op) = safeOp op
  safeOp (OtherOp op) = safeOp op

  cheapOp (ParOp _ op) = cheapOp op
  cheapOp (OtherOp op) = cheapOp op

instance TypedOp op => TypedOp (MCOp lore op) where
  opType (ParOp _ op) = opType op
  opType (OtherOp op) = opType op

instance
  (Aliased lore, AliasedOp op, ASTLore lore) =>
  AliasedOp (MCOp lore op)
  where
  opAliases (ParOp _ op) = opAliases op
  opAliases (OtherOp op) = opAliases op

  consumedInOp (ParOp _ op) = consumedInOp op
  consumedInOp (OtherOp op) = consumedInOp op

instance
  (CanBeAliased (Op lore), CanBeAliased op, ASTLore lore) =>
  CanBeAliased (MCOp lore op)
  where
  type OpWithAliases (MCOp lore op) = MCOp (Aliases lore) (OpWithAliases op)

  addOpAliases (ParOp par_op op) =
    ParOp (addOpAliases <$> par_op) (addOpAliases op)
  addOpAliases (OtherOp op) =
    OtherOp $ addOpAliases op

  removeOpAliases (ParOp par_op op) =
    ParOp (removeOpAliases <$> par_op) (removeOpAliases op)
  removeOpAliases (OtherOp op) =
    OtherOp $ removeOpAliases op

instance
  (CanBeWise (Op lore), CanBeWise op, ASTLore lore) =>
  CanBeWise (MCOp lore op)
  where
  type OpWithWisdom (MCOp lore op) = MCOp (Wise lore) (OpWithWisdom op)

  removeOpWisdom (ParOp par_op op) =
    ParOp (removeOpWisdom <$> par_op) (removeOpWisdom op)
  removeOpWisdom (OtherOp op) =
    OtherOp $ removeOpWisdom op

instance (ASTLore lore, ST.IndexOp op) => ST.IndexOp (MCOp lore op) where
  indexOp vtable k (ParOp _ op) is = ST.indexOp vtable k op is
  indexOp vtable k (OtherOp op) is = ST.indexOp vtable k op is

instance (PrettyLore lore, Pretty op) => Pretty (MCOp lore op) where
  ppr (ParOp Nothing op) = ppr op
  ppr (ParOp (Just par_op) op) =
    "par" <+> nestedBlock "{" "}" (ppr par_op)
      </> "seq" <+> nestedBlock "{" "}" (ppr op)
  ppr (OtherOp op) = ppr op

instance (OpMetrics (Op lore), OpMetrics op) => OpMetrics (MCOp lore op) where
  opMetrics (ParOp par_op op) = opMetrics par_op >> opMetrics op
  opMetrics (OtherOp op) = opMetrics op

typeCheckMCOp ::
  TC.Checkable lore =>
  (op -> TC.TypeM lore ()) ->
  MCOp (Aliases lore) op ->
  TC.TypeM lore ()
typeCheckMCOp _ (ParOp (Just par_op) op) = do
  -- It is valid for the same array to be consumed in both par_op and op.
  _ <- typeCheckSegOp return par_op `TC.alternative` typeCheckSegOp return op
  return ()
typeCheckMCOp _ (ParOp Nothing op) =
  typeCheckSegOp return op
typeCheckMCOp f (OtherOp op) = f op

simplifyMCOp ::
  ( Engine.SimplifiableLore lore,
    BodyDec lore ~ ()
  ) =>
  Simplify.SimplifyOp lore op ->
  MCOp lore op ->
  Engine.SimpleM lore (MCOp (Wise lore) (OpWithWisdom op), Stms (Wise lore))
simplifyMCOp f (OtherOp op) = do
  (op', stms) <- f op
  return (OtherOp op', stms)
simplifyMCOp _ (ParOp par_op op) = do
  (par_op', par_op_hoisted) <-
    case par_op of
      Nothing -> return (Nothing, mempty)
      Just x -> first Just <$> simplifySegOp x

  (op', op_hoisted) <- simplifySegOp op

  return (ParOp par_op' op', par_op_hoisted <> op_hoisted)
