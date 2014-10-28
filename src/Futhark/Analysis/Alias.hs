module Futhark.Analysis.Alias
       ( aliasAnalysis
       )
       where

import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Syntax as In
import qualified Futhark.Representation.Aliases as Out

aliasAnalysis :: Lore lore => In.Prog lore -> Out.Prog lore
aliasAnalysis = Out.Prog . map analyseFun . In.progFunctions

analyseFun :: Lore lore => In.FunDec lore -> Out.FunDec lore
analyseFun (In.FunDec fname restype params body loc) =
  Out.FunDec fname restype params body' loc
  where body' = analyseBody body

analyseBody :: Lore lore => In.Body lore -> Out.Body lore
analyseBody (In.Body lore origbnds result) =
  let bnds' = map analyseBinding origbnds
  in Out.mkAliasedBody lore bnds' result

analyseBinding :: Lore lore => In.Binding lore -> Out.Binding lore
analyseBinding (In.Let pat lore e) =
  let e' = analyseExp e
      pat' = Out.addAliasesToPattern pat e'
      lore' = (Out.Names' $ Out.consumedInExp e', lore)
  in Out.Let pat' lore' e'

analyseExp :: Lore lore => In.Exp lore -> Out.Exp lore
analyseExp (Out.LoopOp (In.Map cs lam args loc)) =
  Out.LoopOp $
  Out.Map cs (analyseLambda lam) args loc
analyseExp (Out.LoopOp (In.Filter cs lam args loc)) =
  Out.LoopOp $
  Out.Filter cs (analyseLambda lam) args loc
analyseExp (Out.LoopOp (In.Reduce cs lam input loc)) =
  Out.LoopOp $
  Out.Reduce cs (analyseLambda lam) input loc
analyseExp (Out.LoopOp (In.Scan cs lam input loc)) =
  Out.LoopOp $
  Out.Scan cs (analyseLambda lam) input loc
analyseExp (Out.LoopOp (In.Redomap cs outerlam innerlam acc arr loc)) =
  Out.LoopOp $
  Out.Redomap cs
   (analyseLambda outerlam)
   (analyseLambda innerlam)
   acc arr loc
analyseExp e = Out.mapExp traverse e
  where traverse =
          Out.Mapper { Out.mapOnSubExp = return
                     , Out.mapOnValue = return
                     , Out.mapOnCertificates = return
                     , Out.mapOnIdent = return
                     , Out.mapOnType = return
                     , Out.mapOnBody = return . analyseBody
                     , Out.mapOnBinding = return . analyseBinding
                     , Out.mapOnLambda = error "Improperly handled lambda in alias analysis"
                     , Out.mapOnResType = return
                     }

analyseLambda :: Lore lore => In.Lambda lore -> Out.Lambda lore
analyseLambda lam =
  let body = analyseBody $ In.lambdaBody lam
  in lam { Out.lambdaBody = body }
