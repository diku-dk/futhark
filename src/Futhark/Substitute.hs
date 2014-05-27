{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
--
-- This module contains exports a single function, 'substituteNames',
-- for performing name substitution in an Futhark expression.
module Futhark.Substitute
  (Substitute(..))
  where

import Control.Monad.Identity
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Futhark.InternalRep

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

instance Substitute VName where
  substituteNames substs k = fromMaybe k $ HM.lookup k substs

instance Substitute SubExp where
  substituteNames substs (Var v)     = Var $ substituteNames substs v
  substituteNames _ (Constant v loc) = Constant v loc

instance Substitute Exp where
  substituteNames substs = mapExp $ replace substs

instance Substitute Body where
  substituteNames substs = mapBody $ replace substs

replace :: HM.HashMap VName VName -> Mapper Identity
replace substs = Mapper {
                   mapOnIdent = return . substituteNames substs
                 , mapOnSubExp = return . substituteNames substs
                 , mapOnBody = return . substituteNames substs
                 , mapOnExp = return . substituteNames substs
                 , mapOnType = return . substituteNames substs
                 , mapOnValue = return
                 , mapOnLambda = return . substituteNames substs
                 , mapOnCertificates = return . map (substituteNames substs)
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

instance (Substitute als, Substitute shape) => Substitute (TypeBase als shape) where
  substituteNames _ (Basic et) = Basic et
  substituteNames substs (Array et sz u als) =
    Array et (substituteNames substs sz) u (substituteNames substs als)

instance Substitute Lambda where
  substituteNames substs (Lambda params body rettype loc) =
    Lambda params (substituteNames substs body)
           (map (substituteNames substs) rettype) loc

instance Substitute Ident where
  substituteNames substs v =
    v { identName = substituteNames substs $ identName v
      , identType = substituteNames substs $ identType v
      }
