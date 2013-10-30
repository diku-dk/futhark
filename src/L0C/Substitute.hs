module L0C.Substitute
  (substituteNames)
  where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import L0C.L0

substituteNames :: M.Map VName VName -> Exp -> Exp
substituteNames substs = substInExp
  where replaceName k = fromMaybe k $ M.lookup k substs

        replaceIdent v = v { identName = replaceName $ identName v }

        substInExp = mapExp replace

        substInElemType (Tuple ts) = Tuple $ map substInType ts
        substInElemType et = et

        substInType (Elem et) = Elem $ substInElemType et
        substInType (Array et sz u als) =
          Array (toElemDecl $ substInElemType $ fromElemDecl et)
                  (map (liftM substInArrayDim) sz)
                  u (S.map replaceName als)

        substInArrayDim = mapExp replace
        substInPattern (Id v) = Id $ replaceIdent v
        substInPattern (TupId pats loc) = TupId (map substInPattern pats) loc
        substInPattern (Wildcard t loc) = Wildcard t loc

        substInLambda (AnonymFun params body rettype loc) =
          AnonymFun params (substInExp body)
                      (toDecl $ substInType $ fromDecl rettype) loc
        substInLambda (CurryFun fname curryargs rettype loc) =
          CurryFun fname (map substInExp curryargs) (substInType rettype) loc

        replace = Mapper {
                    mapOnIdent = return . replaceIdent
                  , mapOnExp = return . substInExp
                  , mapOnType = return . substInType
                  , mapOnPattern = return . substInPattern
                  , mapOnValue = return
                  , mapOnLambda = return . substInLambda
                  }
