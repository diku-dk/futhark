-- | Facilities for changing the lore of some fragment, with no context.
module Futhark.Analysis.Rephrase
       ( rephraseProg
       , rephraseFunDec
       , rephraseExp
       , rephraseBody
       , rephraseBinding
       , rephraseLambda
       , rephraseExtLambda
       , rephrasePattern
       , Rephraser (..)
       )
where

import Control.Monad.Identity
import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Annotations as Annotations

data Rephraser from to
  = Rephraser { rephraseExpLore :: Annotations.Exp from -> Annotations.Exp to
              , rephraseLetBoundLore :: Annotations.LetBound from -> Annotations.LetBound to
              , rephraseFParamLore :: Annotations.FParam from -> Annotations.FParam to
              , rephraseLParamLore :: Annotations.LParam from -> Annotations.LParam to
              , rephraseBodyLore :: Annotations.Body from -> Annotations.Body to
              , rephraseRetType :: RetType from -> RetType to
              , rephraseOp :: Op from -> Op to
              }

rephraseProg :: Rephraser from to -> Prog from -> Prog to
rephraseProg rephraser = Prog . map (rephraseFunDec rephraser) . progFunctions

rephraseFunDec :: Rephraser from to -> FunDec from -> FunDec to
rephraseFunDec rephraser fundec =
  fundec { funDecBody = rephraseBody rephraser $ funDecBody fundec
         , funDecParams = map (rephraseParam $ rephraseFParamLore rephraser) $
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
rephrasePattern rephraser (Pattern context values) =
  Pattern (rephrase context) (rephrase values)
  where rephrase = map (rephrasePatElem $ rephraseLetBoundLore rephraser)

rephrasePatElem :: (from -> to) -> PatElemT from -> PatElemT to
rephrasePatElem rephraser (PatElem ident BindVar from) =
  PatElem ident BindVar $ rephraser from
rephrasePatElem rephraser (PatElem ident (BindInPlace cs src is) from) =
  PatElem ident (BindInPlace cs src is) $ rephraser from

rephraseParam :: (from -> to) -> ParamT from -> ParamT to
rephraseParam rephraser (Param name from) =
  Param name $ rephraser from

rephraseBody :: Rephraser from to -> Body from -> Body to
rephraseBody rephraser (Body lore bnds res) =
  Body
  (rephraseBodyLore rephraser lore)
  (map (rephraseBinding rephraser) bnds)
  res

rephraseLambda :: Rephraser from to -> Lambda from -> Lambda to
rephraseLambda rephraser lam =
  lam { lambdaBody = rephraseBody rephraser $ lambdaBody lam
      , lambdaParams = map (rephraseParam $ rephraseLParamLore rephraser) $
                       lambdaParams lam
      }

rephraseExtLambda :: Rephraser from to -> ExtLambda from -> ExtLambda to
rephraseExtLambda rephraser lam =
  lam { extLambdaBody = rephraseBody rephraser $ extLambdaBody lam
      , extLambdaParams = map (rephraseParam $ rephraseLParamLore rephraser) $
                          extLambdaParams lam
      }

mapper :: Rephraser from to -> Mapper from to Identity
mapper rephraser = identityMapper {
    mapOnBody = return . rephraseBody rephraser
  , mapOnLambda = return . rephraseLambda rephraser
  , mapOnExtLambda = return . rephraseExtLambda rephraser
  , mapOnRetType = return . rephraseRetType rephraser
  , mapOnFParam = return . rephraseParam (rephraseFParamLore rephraser)
  , mapOnLParam = return . rephraseParam (rephraseLParamLore rephraser)
  , mapOnOp = return . rephraseOp rephraser
  }
