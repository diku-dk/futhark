module L0C.EnablingOpts.RangeProp (
    rangeProp
    , dummyUsingRangeProp
)
where

import L0C.L0
import L0C.EnablingOpts.Range

import L0C.EnablingOpts.EnablingOptErrors

import qualified Data.Map as Map

import Control.Monad.Writer

import Debug.Trace

import Text.PrettyPrint.Mainland
import L0C.EscapeColor

--------------------------------


dummyUsingRangeProp :: Prog -> RangeDict -> Either EnablingOptError Prog
dummyUsingRangeProp prog _ = return prog

rangeProp :: Prog -> Either EnablingOptError RangeDict
rangeProp prog = do
    let asList = filter (\(_,t,_) -> isElemInt t) $ allIdentsAsList prog
        hahaBadName = foldl keepAddingToRangeDict Map.empty asList
    trace ( foldr ((++) . (++ "\n") . ppDictElem) "" (Map.toList hahaBadName) ++ "\n" ++
           prettyPrint prog ++ "\n"
           ) return hahaBadName
    where isElemInt (Elem Int) = True
          isElemInt _ = False
          keepAddingToRangeDict acc (i,_,e) =
            acc `Map.union` Map.singleton i (createRangeAndSign acc e)



ppDictElem :: (VName, (Range, Sign)) -> String
ppDictElem (vname, (range, sign)) =
  escapeColorize Green (pretty 80 $ ppr vname) ++ " " ++
  escapeColorize Blue (show range) ++ " " ++
  escapeColorize Yellow (show sign)


-- stolen from Traversals.progNames
allIdentsAsList :: Prog -> [ (VName, DeclType, Maybe Exp) ]
allIdentsAsList = execWriter . mapM funNames . progFunctions
  where
    tellParam :: Parameter -> Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellParam (Ident name tp _) =
      tell [(name, tp, Nothing)]

    tellLet :: Ident -> Exp ->  Writer [ (VName, DeclType, Maybe Exp) ] ()
    tellLet (Ident name tp _) toExp =
      tell [(name, toDecl tp, Just toExp)]

    names = identityWalker {
                  walkOnExp = expNames
                , walkOnLambda = lambdaNames
                }

    funNames (_, _, params, body, _) =
      mapM_ tellParam params >> expNames body

    expNames e@(LetPat (Id ident) toExp _ _) =
      tellLet ident toExp >> walkExpM names e
    --expNames e@(LetWith dest _ _ _ _ _) =
      --tellParam dest >> walkExpM names e
    --expNames e@(DoLoop _ _ i _ _ _ _) =
      --tellParam i >> walkExpM names e
    expNames e = walkExpM names e

    lambdaNames (AnonymFun params body _ _) =
      mapM_ tellParam params >> expNames body
    lambdaNames (CurryFun _ exps _ _) =
          mapM_ expNames exps
