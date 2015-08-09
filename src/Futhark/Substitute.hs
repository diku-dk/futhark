{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- |
--
-- This module contains exports a single function, 'substituteNames',
-- for performing name substitution in an Futhark expression.
module Futhark.Substitute
  (Substitutions,
   Substitute(..),
   Substitutable)
  where

import Control.Monad.Identity
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.RetType

-- | The substitutions to be made are given by a mapping from names to
-- names.
type Substitutions = HM.HashMap VName VName

-- | A type that is an instance of this class supports substitution of
-- any names contained within.
class Substitute a where
  -- | @substituteNames m e@ replaces the variable names in @e@ with
  -- new names, based on the mapping in @m@.  It is assumed that all
  -- names in @e@ are unique, i.e. there is no shadowing.  Aliasing
  -- information is also updated, although the resulting information
  -- may be erroneous if any if the substitute names in @m@ were
  -- already in use in @e@.
  substituteNames :: HM.HashMap VName VName -> a -> a

instance Substitute a => Substitute [a] where
  substituteNames substs = map $ substituteNames substs

instance (Substitute a, Substitute b) => Substitute (a,b) where
  substituteNames substs (x,y) =
    (substituteNames substs x, substituteNames substs y)

instance (Substitute a, Substitute b, Substitute c) => Substitute (a,b,c) where
  substituteNames substs (x,y,z) =
    (substituteNames substs x,
     substituteNames substs y,
     substituteNames substs z)

instance Substitute a => Substitute (Maybe a) where
  substituteNames substs = fmap $ substituteNames substs

instance Substitute Bool where
  substituteNames = flip const

instance Substitute VName where
  substituteNames substs k = HM.lookupDefault k k substs

instance Substitute SubExp where
  substituteNames substs (Var v) = Var $ substituteNames substs v
  substituteNames _ (Constant v) = Constant v

instance Substitutable lore => Substitute (Exp lore) where
  substituteNames substs = mapExp $ replace substs

instance Substitute attr => Substitute (PatElemT attr) where
  substituteNames substs (PatElem ident bindage attr) =
    PatElem
    (substituteNames substs ident)
    (substituteNames substs bindage)
    (substituteNames substs attr)

instance Substitute Bindage where
  substituteNames _ BindVar =
    BindVar
  substituteNames substs (BindInPlace cs src is) =
    BindInPlace
    (map (substituteNames substs) cs)
    (substituteNames substs src)
    (map (substituteNames substs) is)

instance Substitute attr => Substitute (ParamT attr) where
  substituteNames substs (Param ident attr) =
    Param (substituteNames substs ident) (substituteNames substs attr)

instance Substitutable lore => Substitute (Pattern lore) where
  substituteNames substs (Pattern context values) =
    Pattern (substituteNames substs context) (substituteNames substs values)

instance Substitutable lore => Substitute (Binding lore) where
  substituteNames substs (Let pat annot e) =
    Let
    (substituteNames substs pat)
    (substituteNames substs annot)
    (substituteNames substs e)

instance Substitutable lore => Substitute (Body lore) where
  substituteNames substs (Body attr bnds res) =
    Body
    (substituteNames substs attr)
    (substituteNames substs bnds)
    (substituteNames substs res)

replace :: (Substitutable lore) => HM.HashMap VName VName -> Mapper lore lore Identity
replace substs = Mapper {
                   mapOnVName = return . substituteNames substs
                 , mapOnSubExp = return . substituteNames substs
                 , mapOnBody = return . substituteNames substs
                 , mapOnLambda = return . substituteNames substs
                 , mapOnExtLambda = return . substituteNames substs
                 , mapOnCertificates = return . map (substituteNames substs)
                 , mapOnRetType = return . substituteNames substs
                 , mapOnFParam = return . substituteNames substs
                 }

instance Substitute Rank where
  substituteNames _ = id

instance Substitute () where
  substituteNames _ = id

instance Substitute Shape where
  substituteNames substs (Shape es) =
    Shape $ map (substituteNames substs) es

instance Substitute ExtShape where
  substituteNames substs (ExtShape es) =
    ExtShape $ map (substituteNames substs) es

instance Substitute ExtDimSize where
  substituteNames substs (Free se) = Free $ substituteNames substs se
  substituteNames _      (Ext x)   = Ext x

instance Substitute Names where
  substituteNames = HS.map . substituteNames

instance (Substitute shape) => Substitute (TypeBase shape) where
  substituteNames _ (Basic et) = Basic et
  substituteNames substs (Array et sz u) =
    Array et (substituteNames substs sz) u
  substituteNames substs (Mem sz) =
    Mem $ substituteNames substs sz

instance Substitutable lore => Substitute (Lambda lore) where
  substituteNames substs (Lambda index params body rettype) =
    Lambda
    (substituteNames substs index)
    (substituteNames substs params)
    (substituteNames substs body)
    (map (substituteNames substs) rettype)

instance Substitutable lore => Substitute (ExtLambda lore) where
  substituteNames substs (ExtLambda index params body rettype) =
    ExtLambda
    (substituteNames substs index)
    (substituteNames substs params)
    (substituteNames substs body)
    (map (substituteNames substs) rettype)

instance Substitute Ident where
  substituteNames substs v =
    v { identName = substituteNames substs $ identName v
      , identType = substituteNames substs $ identType v
      }

instance Substitute ExtRetType where
  substituteNames substs (ExtRetType ts) =
    ExtRetType $ map (substituteNames substs) ts

instance Substitute d => Substitute (DimChange d) where
  substituteNames substs =
    fmap $ substituteNames substs

-- | The class of lores in which all annotations support name
-- substitution.
class (Substitute (Annotations.Exp lore),
       Substitute (Annotations.Body lore),
       Substitute (Annotations.LetBound lore),
       Substitute (Annotations.FParam lore),
       Substitute (Annotations.LParam lore),
       Substitute (Annotations.RetType lore)) =>
      Substitutable lore where
