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

import L0C.L0

class Substitute a where
  -- | @substituteNames m e@ replaces the variable names in @e@ with
  -- new names, based on the mapping in @m@.  It is assumed that all
  -- names in @e@ are unique, i.e. there is no shadowing.  Aliasing
  -- information is also updated, although the resulting information
  -- may be erroneous if any if the substitute names in @m@ were
  -- already in use in @e@.
  substituteNames :: HM.HashMap VName VName -> a -> a

instance Substitute VName where
  substituteNames substs k = fromMaybe k $ HM.lookup k substs

instance Substitute Exp where
  substituteNames substs = mapExp replace
    where replace = Mapper {
                      mapOnIdent = return . substituteNames substs
                    , mapOnExp = return . substituteNames substs
                    , mapOnType = return . substituteNames substs
                    , mapOnPattern = return . substituteNames substs
                    , mapOnValue = return
                    , mapOnLambda = return . substituteNames substs
                    , mapOnTupleLambda = return . substituteNames substs
                    , mapOnCertificates = return . map (substituteNames substs)
                    }

instance Substitute TupIdent where
  substituteNames substs (Id v) =
    Id $ substituteNames substs v
  substituteNames substs (TupId pats loc) =
    TupId (map (substituteNames substs) pats) loc
  substituteNames _ (Wildcard t loc) =
    Wildcard t loc

instance Substitute ElemType where
  substituteNames substs (Tuple ts) = Tuple $ map (substituteNames substs) ts
  substituteNames _     et = et

instance Substitute Type where
  substituteNames substs (Elem et) = Elem $ substituteNames substs et
  substituteNames substs (Array et sz u als) =
    Array (toElemDecl $ substituteNames substs $ fromElemDecl et)
            (map (liftM $ substituteNames substs) sz)
            u (HS.map (substituteNames substs) als)

instance Substitute Lambda where
  substituteNames substs (AnonymFun params body rettype loc) =
    AnonymFun params (substituteNames substs body)
    (toDecl $ substituteNames substs $ fromDecl rettype) loc
  substituteNames substs (CurryFun fname curryargs rettype loc) =
    CurryFun fname (map (substituteNames substs) curryargs) (substituteNames substs rettype) loc

instance Substitute TupleLambda where
  substituteNames substs (TupleLambda params body rettype loc) =
    TupleLambda params (substituteNames substs body)
                (map (toDecl . substituteNames substs . fromDecl) rettype) loc

instance Substitute Ident where
  substituteNames substs v = v { identName = substituteNames substs $ identName v }
