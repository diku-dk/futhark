-- |
--
-- This module contains exports a single function, 'substituteNames',
-- for performing name substitution in an L0 expression.
module L0C.Substitute
  (substituteNames)
  where

import Control.Monad
import Data.Maybe
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import L0C.L0

-- | @substituteNames m e@ replaces the variable names in @e@ with new
-- names, based on the mapping in @m@.  It is assumed that all names
-- in @e@ are unique, i.e. there is no shadowing.  Aliasing
-- information is also updated, although the resulting information may
-- be erroneous if any if the substitute names in @m@ were already in
-- use in @e@.
substituteNames :: HM.HashMap VName VName -> Exp -> Exp
substituteNames substs = substInExp
  where replaceName k = fromMaybe k $ HM.lookup k substs

        replaceIdent v = v { identName = replaceName $ identName v }

        substInExp = mapExp replace

        substInElemType (Tuple ts) = Tuple $ map substInType ts
        substInElemType et = et

        substInType (Elem et) = Elem $ substInElemType et
        substInType (Array et sz u als) =
          Array (toElemDecl $ substInElemType $ fromElemDecl et)
                  (map (liftM substInArrayDim) sz)
                  u (HS.map replaceName als)

        substInArrayDim = mapExp replace
        substInPattern (Id v) = Id $ replaceIdent v
        substInPattern (TupId pats loc) = TupId (map substInPattern pats) loc
        substInPattern (Wildcard t loc) = Wildcard t loc

        substInLambda (AnonymFun params body rettype loc) =
          AnonymFun params (substInExp body)
                      (toDecl $ substInType $ fromDecl rettype) loc
        substInLambda (CurryFun fname curryargs rettype loc) =
          CurryFun fname (map substInExp curryargs) (substInType rettype) loc

        substInTupleLambda (TupleLambda params body rettype loc) =
          TupleLambda params (substInExp body)
                      (map (toDecl . substInType . fromDecl) rettype) loc

        replace = Mapper {
                    mapOnIdent = return . replaceIdent
                  , mapOnExp = return . substInExp
                  , mapOnType = return . substInType
                  , mapOnPattern = return . substInPattern
                  , mapOnValue = return
                  , mapOnLambda = return . substInLambda
                  , mapOnTupleLambda = return . substInTupleLambda
                  , mapOnCertificates = return . map replaceIdent
                  }
