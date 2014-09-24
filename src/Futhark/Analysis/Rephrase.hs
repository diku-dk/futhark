-- | Facilities for changing the lore of some fragment, with no context.
module Futhark.Analysis.Rephrase
       ( rephraseProg
       , rephraseFunDec
       , rephraseExp
       , rephraseBody
       , rephraseBinding
       , rephraseLambda
       , Rephraser (..)
       )
where

import Control.Monad.Identity
import Futhark.Representation.AST
import qualified Futhark.Representation.AST.Lore as Lore

data Rephraser from to
  = Rephraser { rephraseExpLore :: Lore.Exp from -> Lore.Exp to
              , rephraseBindeeLore :: Lore.Binding from -> Lore.Binding to
              , rephraseBodyLore :: Lore.Body from -> Lore.Body to
              }

rephraseProg :: Rephraser from to -> Prog from -> Prog to
rephraseProg rephraser = Prog . map (rephraseFunDec rephraser) . progFunctions

rephraseFunDec :: Rephraser from to -> FunDec from -> FunDec to
rephraseFunDec rephraser (fname,rettype,params,body,loc) =
  (fname,
   rettype,
   params,
   rephraseBody rephraser body,
   loc)

rephraseExp :: Rephraser from to -> Exp from -> Exp to
rephraseExp = mapExp . mapper

rephraseBinding :: Rephraser from to -> Binding from -> Binding to
rephraseBinding rephraser (Let pat lore e) =
  Let
  (rephrasePattern rephraser pat)
  (rephraseExpLore rephraser lore)
  (rephraseExp rephraser e)

rephrasePattern :: Rephraser from to -> Pattern from -> Pattern to
rephrasePattern rephraser = Pattern . map rephraseBindee . patternBindees
  where rephraseBindee bindee =
          bindee { bindeeLore =
                      rephraseBindeeLore rephraser $ bindeeLore bindee
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

mapper :: Rephraser from to -> Mapper from to Identity
mapper rephraser = identityMapper { mapOnBinding = return . rephraseBinding rephraser
                                  , mapOnBody = return . rephraseBody rephraser
                                  , mapOnLambda = return . rephraseLambda rephraser
                                  }
