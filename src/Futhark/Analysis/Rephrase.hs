-- | Facilities for changing the lore of some fragment, with no context.
module Futhark.Analysis.Rephrase
       ( rephraseProg
       , rephraseFunDef
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

data Rephraser from to
  = Rephraser { rephraseExpLore :: ExpAttr from -> ExpAttr to
              , rephraseLetBoundLore :: LetAttr from -> LetAttr to
              , rephraseFParamLore :: FParamAttr from -> FParamAttr to
              , rephraseLParamLore :: LParamAttr from -> LParamAttr to
              , rephraseBodyLore :: BodyAttr from -> BodyAttr to
              , rephraseRetType :: RetType from -> RetType to
              , rephraseOp :: Op from -> Op to
              }

rephraseProg :: Rephraser from to -> Prog from -> Prog to
rephraseProg rephraser = Prog . map (rephraseFunDef rephraser) . progFunctions

rephraseFunDef :: Rephraser from to -> FunDef from -> FunDef to
rephraseFunDef rephraser fundec =
  fundec { funDefBody = rephraseBody rephraser $ funDefBody fundec
         , funDefParams = map (rephraseParam $ rephraseFParamLore rephraser) $
                          funDefParams fundec
         , funDefRetType = rephraseRetType rephraser $ funDefRetType fundec
         }

rephraseExp :: Rephraser from to -> Exp from -> Exp to
rephraseExp = mapExp . mapper

rephraseBinding :: Rephraser from to -> Binding from -> Binding to
rephraseBinding rephraser (Let pat lore e) =
  Let
  (rephrasePattern (rephraseLetBoundLore rephraser) pat)
  (rephraseExpLore rephraser lore)
  (rephraseExp rephraser e)

rephrasePattern :: (from -> to)
                -> PatternT from
                -> PatternT to
rephrasePattern f (Pattern context values) =
  Pattern (rephrase context) (rephrase values)
  where rephrase = map (rephrasePatElem f)

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
  , mapOnRetType = return . rephraseRetType rephraser
  , mapOnFParam = return . rephraseParam (rephraseFParamLore rephraser)
  , mapOnOp = return . rephraseOp rephraser
  }
