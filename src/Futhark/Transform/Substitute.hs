{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- This module contains facilities for replacing variable names in
-- syntactic constructs.
module Futhark.Transform.Substitute
  ( Substitutions,
    Substitute (..),
    Substitutable,
  )
where

import Control.Monad.Identity
import Data.Map.Strict qualified as M
import Futhark.Analysis.PrimExp
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Scope
import Futhark.IR.Syntax
import Futhark.IR.Traversals

-- | The substitutions to be made are given by a mapping from names to
-- names.
type Substitutions = M.Map VName VName

-- | A type that is an instance of this class supports substitution of
-- any names contained within.
class Substitute a where
  -- | @substituteNames m e@ replaces the variable names in @e@ with
  -- new names, based on the mapping in @m@.  It is assumed that all
  -- names in @e@ are unique, i.e. there is no shadowing.
  substituteNames :: M.Map VName VName -> a -> a

instance (Substitute a) => Substitute [a] where
  substituteNames substs = map $ substituteNames substs

instance (Substitute (Stm rep)) => Substitute (Stms rep) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute a, Substitute b) => Substitute (a, b) where
  substituteNames substs (x, y) =
    (substituteNames substs x, substituteNames substs y)

instance (Substitute a, Substitute b, Substitute c) => Substitute (a, b, c) where
  substituteNames substs (x, y, z) =
    ( substituteNames substs x,
      substituteNames substs y,
      substituteNames substs z
    )

instance (Substitute a, Substitute b, Substitute c, Substitute d) => Substitute (a, b, c, d) where
  substituteNames substs (x, y, z, u) =
    ( substituteNames substs x,
      substituteNames substs y,
      substituteNames substs z,
      substituteNames substs u
    )

instance (Substitute a) => Substitute (Maybe a) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute Bool where
  substituteNames = const id

instance Substitute VName where
  substituteNames substs k = M.findWithDefault k k substs

instance Substitute SubExp where
  substituteNames substs (Var v) = Var $ substituteNames substs v
  substituteNames _ (Constant v) = Constant v

instance (Substitutable rep) => Substitute (Exp rep) where
  substituteNames substs = mapExp $ replace substs

instance (Substitute dec) => Substitute (PatElem dec) where
  substituteNames substs (PatElem ident dec) =
    PatElem (substituteNames substs ident) (substituteNames substs dec)

instance Substitute Attrs where
  substituteNames _ attrs = attrs

instance (Substitute dec) => Substitute (StmAux dec) where
  substituteNames substs (StmAux cs attrs dec) =
    StmAux
      (substituteNames substs cs)
      (substituteNames substs attrs)
      (substituteNames substs dec)

instance (Substitute dec) => Substitute (Param dec) where
  substituteNames substs (Param attrs name dec) =
    Param
      (substituteNames substs attrs)
      (substituteNames substs name)
      (substituteNames substs dec)

instance Substitute SubExpRes where
  substituteNames substs (SubExpRes cs se) =
    SubExpRes (substituteNames substs cs) (substituteNames substs se)

instance (Substitute dec) => Substitute (Pat dec) where
  substituteNames substs (Pat xs) =
    Pat (substituteNames substs xs)

instance Substitute Certs where
  substituteNames substs (Certs cs) =
    Certs $ substituteNames substs cs

instance (Substitutable rep) => Substitute (Stm rep) where
  substituteNames substs (Let pat annot e) =
    Let
      (substituteNames substs pat)
      (substituteNames substs annot)
      (substituteNames substs e)

instance (Substitutable rep) => Substitute (Body rep) where
  substituteNames substs (Body dec stms res) =
    Body
      (substituteNames substs dec)
      (substituteNames substs stms)
      (substituteNames substs res)

replace :: (Substitutable rep) => M.Map VName VName -> Mapper rep rep Identity
replace substs =
  Mapper
    { mapOnVName = pure . substituteNames substs,
      mapOnSubExp = pure . substituteNames substs,
      mapOnBody = const $ pure . substituteNames substs,
      mapOnRetType = pure . substituteNames substs,
      mapOnBranchType = pure . substituteNames substs,
      mapOnFParam = pure . substituteNames substs,
      mapOnLParam = pure . substituteNames substs,
      mapOnOp = pure . substituteNames substs
    }

instance Substitute Rank where
  substituteNames _ = id

instance Substitute () where
  substituteNames _ = id

instance Substitute (NoOp rep) where
  substituteNames _ = id

instance (Substitute d) => Substitute (ShapeBase d) where
  substituteNames substs (Shape es) =
    Shape $ map (substituteNames substs) es

instance (Substitute d) => Substitute (NewShape d) where
  substituteNames substs = fmap (substituteNames substs)

instance (Substitute d) => Substitute (Ext d) where
  substituteNames substs (Free x) = Free $ substituteNames substs x
  substituteNames _ (Ext x) = Ext x

instance Substitute Names where
  substituteNames = mapNames . substituteNames

instance Substitute PrimType where
  substituteNames _ t = t

instance (Substitute shape) => Substitute (TypeBase shape u) where
  substituteNames _ (Prim et) =
    Prim et
  substituteNames substs (Acc acc ispace ts u) =
    Acc
      (substituteNames substs acc)
      (substituteNames substs ispace)
      (substituteNames substs ts)
      u
  substituteNames substs (Array et sz u) =
    Array (substituteNames substs et) (substituteNames substs sz) u
  substituteNames _ (Mem space) =
    Mem space

instance (Substitutable rep) => Substitute (Lambda rep) where
  substituteNames substs (Lambda params rettype body) =
    Lambda
      (substituteNames substs params)
      (map (substituteNames substs) rettype)
      (substituteNames substs body)

instance Substitute Ident where
  substituteNames substs v =
    v
      { identName = substituteNames substs $ identName v,
        identType = substituteNames substs $ identType v
      }

instance (Substitute d) => Substitute (DimIndex d) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute d) => Substitute (Slice d) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute d) => Substitute (FlatDimIndex d) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute d) => Substitute (FlatSlice d) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute v) => Substitute (PrimExp v) where
  substituteNames substs = fmap $ substituteNames substs

instance (Substitute v) => Substitute (TPrimExp t v) where
  substituteNames substs =
    TPrimExp . fmap (substituteNames substs) . untyped

instance (Substitutable rep) => Substitute (NameInfo rep) where
  substituteNames subst (LetName dec) =
    LetName $ substituteNames subst dec
  substituteNames subst (FParamName dec) =
    FParamName $ substituteNames subst dec
  substituteNames subst (LParamName dec) =
    LParamName $ substituteNames subst dec
  substituteNames _ (IndexName it) =
    IndexName it

instance Substitute FV where
  substituteNames subst = fvNames . substituteNames subst . freeIn

-- | Representations in which all annotations support name
-- substitution.
type Substitutable rep =
  ( RepTypes rep,
    Substitute (ExpDec rep),
    Substitute (BodyDec rep),
    Substitute (LetDec rep),
    Substitute (FParamInfo rep),
    Substitute (LParamInfo rep),
    Substitute (RetType rep),
    Substitute (BranchType rep),
    Substitute (Op rep)
  )
