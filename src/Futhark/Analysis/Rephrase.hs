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
              , rephraseBindeeLore :: Lore.LetBound from -> Lore.LetBound to
              , rephraseFParamLore :: Lore.FParam from -> Lore.FParam to
              , rephraseBodyLore :: Lore.Body from -> Lore.Body to
              , rephraseResTypeLore :: Lore.ResTypeElem from -> Lore.ResTypeElem to
              }

rephraseProg :: Rephraser from to -> Prog from -> Prog to
rephraseProg rephraser = Prog . map (rephraseFunDec rephraser) . progFunctions

rephraseFunDec :: Rephraser from to -> FunDec from -> FunDec to
rephraseFunDec rephraser fundec =
  fundec { funDecBody = rephraseBody rephraser $ funDecBody fundec
         , funDecParams = map (rephraseBindee $ rephraseFParamLore rephraser) $
                          funDecParams fundec
         , funDecRetType = rephraseResType rephraser $ funDecRetType fundec
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
  map (rephraseBindee $ rephraseBindeeLore rephraser) .
  patternBindees

rephraseBindee :: (from -> to) -> Bindee from -> Bindee to
rephraseBindee rephraser bindee =
  bindee { bindeeLore = rephraser $ bindeeLore bindee
         }

rephraseBody :: Rephraser from to -> Body from -> Body to
rephraseBody rephraser (Body lore bnds res) =
  Body
  (rephraseBodyLore rephraser lore)
  (map (rephraseBinding rephraser) bnds)
  res

rephraseLambda :: Rephraser from to -> Lambda from -> Lambda to
rephraseLambda rephraser lam =
  lam { lambdaBody = rephraseBody rephraser $ lambdaBody lam }

rephraseResType :: Rephraser from to -> ResType from -> ResType to
rephraseResType rephraser (ResType ts) =
  ResType $ map rephrase ts
  where rephrase (t,attr) = (t, rephraseResTypeLore rephraser attr)

mapper :: Rephraser from to -> Mapper from to Identity
mapper rephraser = identityMapper { mapOnBinding = return . rephraseBinding rephraser
                                  , mapOnBody = return . rephraseBody rephraser
                                  , mapOnLambda = return . rephraseLambda rephraser
                                  , mapOnResType = return . rephraseResType rephraser
                                  }
