module Futhark.SplitAssertions
  (
   splitAssertions
  )
where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import Futhark.InternalRep
import Futhark.InternalRep.Renamer
import Futhark.MonadFreshNames
import Futhark.Tools

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

splitBndAssertions (Let pat (DoLoop respat merge i bound body loc)) = do
  -- XXX slightly gross hack
  DoLoop _ merge' i' _ body' _ <-
    renameExp $ DoLoop respat merge i bound body loc
  (certbody, allBoundsChecks) <- splitLoopBody body'
  allBoundsChecksRes  <-
    newIdent "loop_bounds_cert_res" (Basic Bool) loc
  allBoundsChecksCert  <-
    newIdent "loop_bounds_cert" (Basic Cert) loc
  let certmerge = (allBoundsChecks, constant True loc) : merge'
      certloop = Let [allBoundsChecksRes] $
                 DoLoop [allBoundsChecks] certmerge i' bound certbody loc
      valbody = replaceBoundsCerts allBoundsChecksCert body
      valloop = Let pat $ DoLoop respat merge i bound valbody loc
  Body certbnds _ <- runBinder $ copyConsumed $ Body [certloop] nullRes
  return $ certbnds ++
           [Let [allBoundsChecksCert] (Assert (Var allBoundsChecksRes) loc),
            valloop]
  where nullRes = Result [] [] $ srclocOf body

splitBndAssertions (Let pat (Map cs fun args loc)) = do
  fun' <- splitMapFun fun
  allBoundsChecks     <- newIdent "map_bounds_checks" boolarray loc
  andedBoundsChecks   <- newIdent "map_comb_bounds_check" (Basic Bool) loc
  allBoundsChecksCert <- newIdent "loop_bounds_cert"  (Basic Cert) loc
  andfun <- binOpLambda LogAnd (Basic Bool) loc
  let certmap = Let [allBoundsChecks] $ Map cs fun' args loc
      valfun = fun { lambdaBody = replaceBoundsCerts allBoundsChecksCert $
                                  lambdaBody fun
                   }
      valmap = Let pat $ Map cs valfun args loc
      andbnd = Let [andedBoundsChecks] $
               Reduce [] andfun [(constant True loc,Var allBoundsChecks)] loc
      assertbnd = Let [allBoundsChecksCert] (Assert (Var andedBoundsChecks) loc)
  Body certbnds _ <- runBinder $ copyConsumed $ Body [certmap] nullRes
  return $ certbnds ++ [andbnd, assertbnd, valmap]
  where nullRes = Result [] [] loc
        boolarray = arrayOf (Basic Bool)
                    (Shape [arraysSize 0 $ map subExpType args]) Unique

splitBndAssertions bnd = return [bnd]

splitLoopBody :: Body -> SplitM (Body, Ident)
splitLoopBody body = do
  allBoundsChecks  <- newIdent "loop_bounds_checks" (Basic Bool) $ srclocOf body
  certbody <- returnChecksInBody (Var allBoundsChecks) body
  return (certbody, allBoundsChecks)

splitMapFun :: Lambda -> SplitM Lambda
splitMapFun lam = do
  Body checkbnds (Result cs es loc) <-
    returnChecksInBody (constant True $ srclocOf lam) $ lambdaBody lam
  return $ lam { lambdaBody = Body checkbnds $ Result cs (take 1 es) loc
               , lambdaReturnType = [Basic Bool]
               }

returnChecksInBody :: SubExp -> Body -> SplitM Body
returnChecksInBody startcert (Body bnds (Result cs es loc)) = do
  (bnds', bndchecks) <- unzip <$> mapM returnChecksInBinding bnds
  c <- newIdent "body_bounds_checks" (Basic Bool) loc
  (check, checkbnds) <-
    runBinder'' $ foldBinOp LogAnd startcert (concat bndchecks) (Basic Bool)
  return $ Body (bnds'++checkbnds++[Let [c] check]) $
           Result cs (Var c:es) loc

-- XXX? We assume that all assertions are bound checks.
returnChecksInBinding :: Binding -> SplitM (Binding, [SubExp])
returnChecksInBinding (Let pat (If cond tbranch fbranch t loc)) = do
  tbranch' <- returnChecksInBody (constant True loc) tbranch
  fbranch' <- returnChecksInBody (constant True loc) fbranch
  cert <- newIdent "if_bounds_check" (Basic Bool) loc
  return (Let (cert:pat) $ If cond tbranch' fbranch' (Basic Bool:t) loc,
          [Var cert])
returnChecksInBinding (Let pat e@(Assert prop _)) =
  return (Let pat e, [prop])
returnChecksInBinding (Let pat (DoLoop respat merge i bound body loc)) = do
  (certbody, allBoundsChecks) <- splitLoopBody body
  let certmerge = (allBoundsChecks, constant True loc) : merge
      certloop = DoLoop (allBoundsChecks:respat) certmerge i bound certbody loc
  return (Let (allBoundsChecks:pat) certloop, [Var allBoundsChecks])
returnChecksInBinding (Let pat e) =
  return (Let pat e, []) -- XXX what if 'e' is a SOAC or something?

replaceBoundsCerts :: Ident -> Body -> Body
replaceBoundsCerts c = mapBody replace
  where replace = identityMapper {
                    mapOnBody = return . replaceBoundsCerts c
                  , mapOnExp = return . replaceInExp
                  , mapOnCertificates = const $ return [c]
                  }
        replaceInExp (Map cs fun arrs loc)     = Map cs fun' arrs loc
          where fun' = fun { lambdaBody =
                               replaceBoundsCerts c $ lambdaBody fun }
        replaceInExp (Index _ v idxs loc)      = Index [c] v idxs loc
        replaceInExp (Update _ v idxs val loc) = Update [c] v idxs val loc
        replaceInExp e                         = mapExp replace e
