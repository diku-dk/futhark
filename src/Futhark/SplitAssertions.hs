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

splitBndAssertions (Let pat (DoLoop merge i bound body loc)) = do
  -- XXX slightly gross hack
  DoLoop merge' i' _ body' _ <-
    renameExp $ DoLoop merge i bound body loc
  (certmergepat, certbody, allBoundsChecks) <-
    splitLoopBody merge' body'
  allBoundsChecksCert  <-
    newIdent "loop_bounds_cert" (Basic Cert) loc
  pat' <- mapM (newIdent' id) certmergepat
  let certmerge = zip (allBoundsChecks:certmergepat)
                      (constant True loc:map snd merge)
      certloop = Let (allBoundsChecks:pat') $
                 DoLoop certmerge i' bound certbody loc
      valbody = replaceBoundsCerts allBoundsChecksCert body
      valloop = Let pat $ DoLoop merge i bound valbody loc
  Body certbnds _ <- runBinder $ copyConsumed $ Body [certloop] nullRes
  return $ certbnds ++
           [Let [allBoundsChecksCert] (Assert (Var allBoundsChecks) loc),
            valloop]
  where nullRes = Result [] [] $ srclocOf body
splitBndAssertions bnd = return [bnd]

splitLoopBody :: [(Ident,SubExp)] -> Body -> SplitM ([Ident], Body, Ident)
splitLoopBody merge body = do
  allBoundsChecks  <- newIdent "loop_bounds_checks" (Basic Bool) $ srclocOf body
  certbody <- returnChecksInBody (Var allBoundsChecks) body
  return (map fst merge, certbody, allBoundsChecks)

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
returnChecksInBinding (Let pat (DoLoop merge i bound body loc)) = do
  (certmergepat, certbody, allBoundsChecks) <-
    splitLoopBody merge body
  let certmerge = zip (allBoundsChecks:certmergepat)
                  (constant True loc:map snd merge)
      certloop = DoLoop certmerge i bound certbody loc
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
        replaceInExp (Index _ v idxs loc)      = Index [c] v idxs loc
        replaceInExp (Update _ v idxs val loc) = Update [c] v idxs val loc
        replaceInExp e                         = mapExp replace e
