-- | Alias analysis of a full Futhark program.  Takes as input a
-- program with an arbitrary lore and produces one with aliases.  This
-- module does not implement the aliasing logic itself, and derives
-- its information from definitions in
-- "Futhark.Representation.AST.Attributes.Aliases" and
-- "Futhark.Representation.Aliases".
module Futhark.Analysis.Alias
       ( aliasAnalysis
       )
       where

import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Syntax as In
import qualified Futhark.Representation.Aliases as Out

-- | Perform alias analysis on a Futhark program.
aliasAnalysis :: Lore lore => In.Prog lore -> Out.Prog lore
aliasAnalysis = Out.Prog . map analyseFun . In.progFunctions

analyseFun :: Lore lore => In.FunDec lore -> Out.FunDec lore
analyseFun (In.FunDec fname restype params body) =
  Out.FunDec fname restype params body'
  where body' = analyseBody body

analyseBody :: Lore lore => In.Body lore -> Out.Body lore
analyseBody (In.Body lore origbnds result) =
  let bnds' = map analyseBinding origbnds
  in Out.mkAliasedBody lore bnds' result

analyseBinding :: Lore lore => In.Binding lore -> Out.Binding lore
analyseBinding (In.Let pat lore e) =
  let e' = analyseExp e
      pat' = Out.addAliasesToPattern pat e'
      lore' = (Out.Names' $ Out.consumedInExp pat' e',
               lore)
  in Out.Let pat' lore' e'

analyseExp :: Lore lore => In.Exp lore -> Out.Exp lore
analyseExp (Out.LoopOp (In.Map cs size lam args)) =
  Out.LoopOp $
  Out.Map cs size (analyseLambda lam) args
analyseExp (Out.LoopOp (In.ConcatMap cs size lam args)) =
  Out.LoopOp $
  Out.ConcatMap cs size (analyseLambda lam) args
analyseExp (Out.LoopOp (In.Reduce cs size lam input)) =
  Out.LoopOp $
  Out.Reduce cs size (analyseLambda lam) input
analyseExp (Out.LoopOp (In.Scan cs size lam input)) =
  Out.LoopOp $
  Out.Scan cs size (analyseLambda lam) input
analyseExp (Out.LoopOp (In.Redomap cs size outerlam innerlam acc arr)) =
  Out.LoopOp $
  Out.Redomap cs size
   (analyseLambda outerlam)
   (analyseLambda innerlam)
   acc arr
analyseExp (Out.LoopOp (In.Stream cs size form lam arr ii)) =
  Out.LoopOp $
  Out.Stream cs size (analyseStreamForm form)
                     (analyseExtLambda lam) arr ii
  where analyseStreamForm (In.RedLike o lam0 acc) =
            Out.RedLike o (analyseLambda lam0) acc
        analyseStreamForm (In.Sequential acc) = Out.Sequential acc
        analyseStreamForm (In.MapLike    o  ) = Out.MapLike    o

analyseExp (Out.SegOp (In.SegReduce cs size lam input descp)) =
  Out.SegOp $
  Out.SegReduce cs size (analyseLambda lam) input descp
analyseExp (Out.SegOp (In.SegScan cs size st lam input descp)) =
  Out.SegOp $
  Out.SegScan cs size st (analyseLambda lam) input descp
analyseExp e = Out.mapExp analyse e
  where analyse =
          Out.Mapper { Out.mapOnSubExp = return
                     , Out.mapOnCertificates = return
                     , Out.mapOnVName = return
                     , Out.mapOnBody = return . analyseBody
                     , Out.mapOnLambda = error "Improperly handled lambda in alias analysis"
                     , Out.mapOnExtLambda = error "Improperly handled existential lambda in alias analysis"
                     , Out.mapOnRetType = return
                     , Out.mapOnFParam = return
                     }

analyseLambda :: Lore lore => In.Lambda lore -> Out.Lambda lore
analyseLambda lam =
  let body = analyseBody $ In.lambdaBody lam
  in lam { Out.lambdaBody = body
         , Out.lambdaParams = In.lambdaParams lam
         }
analyseExtLambda :: Lore lore => In.ExtLambda lore -> Out.ExtLambda lore
analyseExtLambda lam =
  let body = analyseBody $ In.extLambdaBody lam
  in lam { Out.extLambdaBody = body
         , Out.extLambdaParams = In.extLambdaParams lam
         }
