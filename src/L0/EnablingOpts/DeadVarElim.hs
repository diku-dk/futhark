{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.DeadVarElim ( 
                                deadCodeElim
                            )
  where
 
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Data
import Data.Generics

import qualified Data.Set as S
--import qualified Data.Map as M

import L0.AbSyn
 
--import L0.Traversals
--import L0.EnablingOpts.InliningDeadFun
import L0.EnablingOpts.EnablingOptErrors


-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Dead-Variable Elimination          ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data DCElimEnv tf = DCElimEnv {   
                        envVtable  :: S.Set Name
                  }

-----------------------------------------------------
-- might want to add `has_io' part of the result  
-- to keep complexity to O(N)
-----------------------------------------------------
data DCElimRes tf = DCElimRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: S.Set Name
  -- ^ The hashtable recording the uses
  --, has_io     :: Bool
  }


instance Monoid (DCElimRes tf) where
  DCElimRes s1 m1 `mappend` DCElimRes s2 m2 =
    DCElimRes (s1 || s2) (m1 `S.union` m2) --(io1 || io2)
  mempty = DCElimRes False S.empty -- False

newtype DCElimM tf a = DCElimM (WriterT (DCElimRes tf) (ReaderT (DCElimEnv tf) (Either EnablingOptError)) a)
    deriving (MonadWriter (DCElimRes tf),
              MonadReader (DCElimEnv tf), Monad, Applicative, Functor)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runDCElimM :: DCElimM tf a -> DCElimEnv tf -> Either EnablingOptError (a, DCElimRes tf)
runDCElimM  (DCElimM a) = runReaderT (runWriterT a)

badDCElimM :: EnablingOptError -> DCElimM tf a
badDCElimM = DCElimM . lift . lift . Left



collectRes :: [Name] -> DCElimM tf a -> DCElimM tf (a, Bool)
collectRes mvars m = pass collect
  where wasNotUsed hashtab vnm =
          return $ not (S.member vnm hashtab)

        collect = do
          (x,res) <- listen m
          tmps    <- mapM (wasNotUsed (resMap res)) mvars
          return ( (x, and tmps), const res )

changed :: DCElimM tf a -> DCElimM tf a
changed m = pass collect
    where
     collect = do
        (x, res) <- listen m
        return (x, const $ res { resSuccess = True })

-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: DCElimEnv tf -> Name -> DCElimEnv tf
bindVar env name =
  env { envVtable = S.insert name $ envVtable env }

bindVars :: DCElimEnv tf -> [Name] -> DCElimEnv tf
bindVars = foldl bindVar

binding :: [Name] -> DCElimM tf a -> DCElimM tf a
binding bnds = local (`bindVars` bnds)


-- | Applies Dead-Code Elimination to an Entire Program.
-- TypeBox tf => 
deadCodeElim :: TypeBox tf => Prog tf -> Either EnablingOptError (Bool, Prog tf)
deadCodeElim prog = do
    let env = DCElimEnv { envVtable = S.empty }
    (rs, res) <- runDCElimM (mapM deadCodeElimFun prog) env
    return (resSuccess res, rs)

deadCodeElimFun :: TypeBox tf => FunDec tf -> DCElimM tf (FunDec tf)
deadCodeElimFun (fname, rettype, args, body, pos) = do
    let ids = map identName args
    body' <- binding ids $ deadCodeElimExp body
    return (fname, rettype, args, body', pos)

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimExp :: TypeBox tf => Exp tf -> DCElimM tf (Exp tf)

-----------------------------------------------------------------------------
-- 'trace' and 'assertZip' exhibit side effects and should not be removed!
deadCodeElimExp (LetPat pat (Apply fname args tp p) body pos)
  | "trace" <- nameToString fname = do
    let ids = getBnds pat
    args' <- mapM deadCodeElimExp args
    body' <- binding ids $ deadCodeElimExp body
    return $ LetPat pat (Apply fname args' tp p) body' pos
deadCodeElimExp (LetPat pat (Apply fname args tp p) body pos)
  | "assertZip" <- nameToString fname = do
    let ids = getBnds pat
    args' <- mapM deadCodeElimExp args
    body' <- binding ids $ deadCodeElimExp body
    return $ LetPat pat (Apply fname args' tp p) body' pos
-----------------------------------------------------------------------------
 
deadCodeElimExp (LetPat pat e body pos) = do
    let ids = getBnds pat
    (body', noref) <- collectRes ids $ binding ids $ deadCodeElimExp body

    if noref 
    then changed $ return body'
    else do
            e' <- deadCodeElimExp e
            return $ LetPat pat e' body' pos


deadCodeElimExp (LetWith nm src inds el body pos) = do
    (body', noref) <- collectRes [identName nm] $ binding [identName nm] $ deadCodeElimExp body
    
    if noref 
    then changed $ return body'
    else do
            let srcnm = identName src
            in_vtab <- asks $ S.member srcnm . envVtable
            if not in_vtab
            then badDCElimM $ VarNotInFtab pos srcnm
            else do
                    _ <- tell $ DCElimRes False (S.insert srcnm S.empty)
                    inds' <- mapM deadCodeElimExp inds
                    el'   <- deadCodeElimExp el
                    return $ LetWith nm src inds' el' body' pos


deadCodeElimExp e@(Var (Ident vnm _ pos)) = do 
    in_vtab <- asks $ S.member vnm . envVtable
    if not in_vtab
    then badDCElimM $ VarNotInFtab pos vnm
    else do
            _ <- tell $ DCElimRes False (S.insert vnm S.empty)
            return e


deadCodeElimExp (Index s idxs t1 t2 pos) = do
    let vnm = identName s
    in_vtab <- asks $ S.member vnm . envVtable
    if not in_vtab
    then badDCElimM $ VarNotInFtab pos vnm
    else do
            _ <- tell $ DCElimRes False (S.insert vnm S.empty)
            idxs' <- mapM deadCodeElimExp idxs
            return $ Index s idxs' t1 t2 pos

deadCodeElimExp (DoLoop mergepat mergeexp idd n loopbdy letbdy pos) = do
    let idnms = getBnds mergepat
    (letbdy',noref) <- collectRes idnms $ binding idnms $ deadCodeElimExp letbdy
    if noref 
    then changed $ return letbdy'
    else do mergeexp'   <- deadCodeElimExp mergeexp
            n'      <- deadCodeElimExp n
            loopbdy'<- binding ( identName idd : idnms) $ deadCodeElimExp loopbdy
            return $ DoLoop mergepat mergeexp' idd n' loopbdy' letbdy' pos



-- The above case may have to be extended if syntax nodes ever contain
-- anything but lambdas, expressions, lists of expressions or lists of
-- pairs of expression-types.  Pay particular attention to the fact
-- that the latter has to be specially handled.
deadCodeElimExp e = gmapM (mkM deadCodeElimExp
                            `extM` deadCodeElimLambda
                            `extM` mapM deadCodeElimExp
                            `extM` mapM deadCodeElimExpPair) e

deadCodeElimExpPair :: TypeBox tf => (Exp tf, tf) -> DCElimM tf (Exp tf, tf)
deadCodeElimExpPair (e,t) = do e' <- deadCodeElimExp e
                               return (e',t)

deadCodeElimLambda :: TypeBox tf => Lambda tf -> DCElimM tf (Lambda tf)
deadCodeElimLambda (AnonymFun params body ret pos) = do
    let ids  = map identName params
    body' <- binding ids $ deadCodeElimExp body
    return $ AnonymFun params body' ret pos

deadCodeElimLambda (CurryFun fname curryargexps rettype pos) = do
    curryargexps' <- mapM deadCodeElimExp curryargexps
    return (CurryFun fname curryargexps' rettype pos)

--deadCodeElimExp e = do
--    return e


getBnds :: TypeBox tf => TupIdent tf -> [Name]
getBnds ( Id (Ident var _ _) ) = [var]
getBnds ( TupId ids _ ) = concatMap getBnds ids
