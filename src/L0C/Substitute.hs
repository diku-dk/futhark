{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
--
-- This module contains exports a single function, 'substituteNames',
-- for performing name substitution in an L0 expression.
module L0C.Substitute
  (Substitute(..))
  where

import Control.Monad
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import L0C.InternalRep

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
  substituteNames substs = mapExp replace
    where replace = Mapper {
                      mapOnIdent = return . substituteNames substs
                    , mapOnSubExp = return . substituteNames substs
                    , mapOnExp = return . substituteNames substs
                    , mapOnType = return . substituteNames substs
                    , mapOnValue = return
                    , mapOnLambda = return . substituteNames substs
                    , mapOnCertificates = return . map (substituteNames substs)
                    }

instance Substitute Type where
  substituteNames _ (Basic et) = Basic et
  substituteNames substs (Array et sz u als) =
    Array et (map (liftM $ substituteNames substs) sz)
             u (HS.map (substituteNames substs) als)

instance Substitute Lambda where
  substituteNames substs (Lambda params body rettype loc) =
    Lambda params (substituteNames substs body)
           (map (toDecl . substituteNames substs . fromDecl) rettype) loc

instance Substitute Ident where
  substituteNames substs v = v { identName = substituteNames substs $ identName v }
