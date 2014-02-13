module Language.L0.Externalise
  ( externaliseProg
  , externaliseExp
  , externaliseTypes
  , externaliseType
  )
  where

import Language.L0.Misc
import L0C.ExternalRep as E
import L0C.InternalRep as I
import L0C.MonadFreshNames

externaliseProg :: I.Prog -> E.Prog
externaliseProg = undefined

externaliseExp :: I.Exp -> E.Exp
externaliseExp (SubExp e) = externaliseSubExp e

externaliseTypes :: [I.GenType als] -> E.GenType als
externaliseTypes ts =
  case map externaliseType ts of
    [t] -> t
    ts  -> E.Elem $ E.Tuple ts

externaliseType :: I.GenType als -> E.GenType als
externaliseType = undefined

externaliseSubExp :: I.SubExp -> E.Exp
externaliseSubExp (I.Var v) = E.Var $ externaliseIdent v

externaliseIdent :: I.IdentBase (als VName) -> E.GenIdent als
externaliseIdent (I.Ident name t loc) =
  E.Ident name (externaliseType t) loc
