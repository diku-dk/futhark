module L0C.SplitAssertions
  (
   splitAssertions
  )
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Data.Loc
import Data.Maybe

import L0C.InternalRep
import L0C.InternalRep.Renamer
import L0C.MonadFreshNames

splitAssertions :: Prog -> Prog
splitAssertions prog =
  let (funs, _) = runSplitM (mapM splitFunAssertions $ progFunctions prog) src
  in Prog funs
  where src = newNameSourceForProg prog

type SplitM = State VNameSource

runSplitM :: SplitM a -> VNameSource -> (a, VNameSource)
runSplitM = runState

splitFunAssertions :: FunDec -> SplitM FunDec
splitFunAssertions (name, rettype, params, body, loc) = do
  body' <- splitBodyAssertions body
  return (name, rettype, params, body', loc)

splitBodyAssertions :: Body -> SplitM Body
splitBodyAssertions body = do
  bnds' <- concat <$> mapM splitBndAssertions (bodyBindings body)
  return $ bnds' `setBodyBindings` body

splitBndAssertions :: Binding -> SplitM [Binding]

splitBndAssertions (Let out (Map cs fun arrs loc)) = do
  -- We need to pick out any boolean bindings in 'fun' that are passed
  -- to an 'assert' in 'fun'.
  let (certs, predbody, valbody) = getAssertions $ lambdaBody fun
      outersize = arraysSize 0 (map subExpType arrs)
  predbody' <- renameBody predbody
  split_preds <- newIdents "split_predicate"
                 [arrayOf t (Shape [outersize]) Nonunique
                  | t <- bodyType predbody']
                 loc
  andfun <- makeAndFun (length split_preds) loc
  anded_preds <- newIdents "anded_predicate" (bodyType $ lambdaBody andfun) loc
  let predfun = fun { lambdaBody = predbody'
                    , lambdaReturnType = map toConstType $ bodyType predbody'
                    }
      split_pred_comp =
        Let split_preds $ Map cs predfun arrs loc
      and_preds_comp =
        Let anded_preds $
          Reduce [] andfun [(Constant (BasicVal $ LogVal True) loc,
                             Var split_pred)
                              | split_pred <- split_preds ]
          loc
      assertions =
        [ Let [cert] (Assert (Var predicate) loc)
            | (cert, predicate) <- zip certs anded_preds ]
      val_comp =
        Let out $ Map cs fun { lambdaBody = valbody } arrs loc
  return $ [split_pred_comp, and_preds_comp] ++ assertions ++ [val_comp]
splitBndAssertions bnd = return [bnd]

getAssertions :: Body -> ([Ident], Body, Body)
getAssertions (Body bnds res) =
  let (bnds', asserts) = runWriter $ catMaybes <$> mapM getAssertion bnds
      (certs, preds)   = unzip asserts
  in (certs,
      Body bnds $ Result [] preds $ srclocOf res,
      Body bnds' res)
  where getAssertion (Let [v] (Assert e _)) = do tell [(v,e)]
                                                 return Nothing
        getAssertion bnd                    = return $ Just bnd

-- | 'makeAndFun n' loc creates a reduce function that applies @&&@ to @n@
-- array inputs.
makeAndFun :: Int -> SrcLoc -> SplitM Lambda
makeAndFun n loc = do
  xs <- replicateM n $ newIdent "x" (Basic Bool) loc
  ys <- replicateM n $ newIdent "y" (Basic Bool) loc
  zs <- replicateM n $ newIdent "z" (Basic Bool) loc
  let bnds = [ Let [z] (BinOp LogAnd (Var x) (Var y) (Basic Bool) loc)
                 | (x,y,z) <- zip3 xs ys zs ]
  return Lambda {
           lambdaParams     = map toParam $ xs ++ ys
         , lambdaReturnType = map (toConstType . identType) zs
         , lambdaSrcLoc     = loc
         , lambdaBody = Body bnds $ Result [] (map Var zs) loc
         }
