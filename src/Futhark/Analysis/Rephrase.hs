-- | Facilities for changing the lore of some fragment, with no context.
module Futhark.Analysis.Rephrase
       ( rephraseProg
       , rephraseFunDec
       , rephraseExp
       , rephraseBody
       , rephraseBinding
       , rephraseLambda
       , rephrasePattern
       , Rephraser (..)
       )
where

import Control.Monad.Identity
import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore

data Rephraser from to
  = Rephraser { rephraseExpLore :: Lore.Exp from -> Lore.Exp to
              , rephraseLetBoundLore :: Lore.LetBound from -> Lore.LetBound to
              , rephraseFParamLore :: Lore.FParam from -> Lore.FParam to
              , rephraseBodyLore :: Lore.Body from -> Lore.Body to
              , rephraseRetType :: RetType from -> RetType to
              }

rephraseProg :: Rephraser from to -> Prog from -> Prog to
rephraseProg rephraser = Prog . map (rephraseFunDec rephraser) . progFunctions

rephraseFunDec :: Rephraser from to -> FunDec from -> FunDec to
rephraseFunDec rephraser fundec =
  fundec { funDecBody = rephraseBody rephraser $ funDecBody fundec
         , funDecParams = map (rephraseFParam $ rephraseFParamLore rephraser) $
                          funDecParams fundec
         , funDecRetType = rephraseRetType rephraser $ funDecRetType fundec
         }

rephraseExp :: Rephraser from to -> Exp from -> Exp to
rephraseExp = mapExp . mapper

rephraseBinding :: Rephraser from to -> Binding from -> Binding to
rephraseBinding rephraser (Let pat lore e) =
  Let
  (rephrasePattern rephraser pat)
  (rephraseExpLore rephraser lore)
  (rephraseExp rephraser e)

rephrasePattern :: Rephraser from to -> Pattern from -> Pattern to
rephrasePattern rephraser =
  Pattern .
  map (rephrasePatElem $ rephraseLetBoundLore rephraser) .
  patternElements

rephrasePatElem :: (from -> to) -> PatElemT from -> PatElemT to
rephrasePatElem rephraser (PatElem ident BindVar from) =
  PatElem ident BindVar $ rephraser from
rephrasePatElem rephraser (PatElem ident (BindInPlace cs src is) from) =
  PatElem ident (BindInPlace cs src is) $ rephraser from

rephraseFParam :: (from -> to) -> FParamT from -> FParamT to
rephraseFParam rephraser (FParam ident from) =
  FParam ident $ rephraser from

rephraseBody :: Rephraser from to -> Body from -> Body to
rephraseBody rephraser (Body lore bnds res) =
  Body
  (rephraseBodyLore rephraser lore)
  (map (rephraseBinding rephraser) bnds)
  res

rephraseLambda :: Rephraser from to -> Lambda from -> Lambda to
rephraseLambda rephraser lam =
  lam { lambdaBody = rephraseBody rephraser $ lambdaBody lam }

mapper :: Rephraser from to -> Mapper from to Identity
mapper rephraser = identityMapper {
    mapOnBinding = return . rephraseBinding rephraser
  , mapOnBody = return . rephraseBody rephraser
  , mapOnLambda = return . rephraseLambda rephraser
  , mapOnRetType = return . rephraseRetType rephraser
  , mapOnFParam = return . rephraseFParam (rephraseFParamLore rephraser)
  }
