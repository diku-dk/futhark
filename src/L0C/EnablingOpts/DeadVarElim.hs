{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.DeadVarElim (
                                deadCodeElim
                            )
  where
 
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Set as S

import L0C.L0

--import L0.EnablingOpts.InliningDeadFun
import L0C.EnablingOpts.EnablingOptErrors


-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Dead-Variable Elimination          ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data DCElimEnv = DCElimEnv { envVtable :: S.Set VName }

-----------------------------------------------------
-- might want to add `has_io' part of the result  
-- to keep complexity to O(N)
-----------------------------------------------------
data DCElimRes = DCElimRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: S.Set VName
  -- ^ The hashtable recording the uses
  --, has_io     :: Bool
  }


instance Monoid DCElimRes where
  DCElimRes s1 m1 `mappend` DCElimRes s2 m2 =
    DCElimRes (s1 || s2) (m1 `S.union` m2) --(io1 || io2)
  mempty = DCElimRes False S.empty -- False

newtype DCElimM a = DCElimM (WriterT DCElimRes (ReaderT DCElimEnv (Either EnablingOptError)) a)
    deriving (MonadWriter DCElimRes,
              MonadReader DCElimEnv, Monad, Applicative, Functor)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runDCElimM :: DCElimM a -> DCElimEnv -> Either EnablingOptError (a, DCElimRes)
runDCElimM  (DCElimM a) = runReaderT (runWriterT a)

badDCElimM :: EnablingOptError -> DCElimM a
badDCElimM = DCElimM . lift . lift . Left



collectRes :: [VName] -> DCElimM a -> DCElimM (a, Bool)
collectRes mvars m = pass collect
  where wasNotUsed hashtab vnm =
          return $ not (S.member vnm hashtab)

        collect = do
          (x,res) <- listen m
          tmps    <- mapM (wasNotUsed (resMap res)) mvars
          return ( (x, and tmps), const res )

changed :: DCElimM a -> DCElimM a
changed m = pass collect
    where
     collect = do
        (x, res) <- listen m
        return (x, const $ res { resSuccess = True })

-- | Bind a name as a common (non-merge) variable.
bindVar :: DCElimEnv -> VName -> DCElimEnv
bindVar env name =
  env { envVtable = S.insert name $ envVtable env }

bindVars :: DCElimEnv -> [VName] -> DCElimEnv
bindVars = foldl bindVar

binding :: [VName] -> DCElimM a -> DCElimM a
binding bnds = local (`bindVars` bnds)


-- | Applies Dead-Code Elimination to an Entire Program.
deadCodeElim :: Prog -> Either EnablingOptError (Bool, Prog)
deadCodeElim prog = do
    let env = DCElimEnv { envVtable = S.empty }
    (rs, res) <- runDCElimM (mapM deadCodeElimFun $ progFunctions prog) env
    return (resSuccess res, Prog rs)

deadCodeElimFun :: FunDec -> DCElimM FunDec
deadCodeElimFun (fname, rettype, args, body, pos) = do
    let ids = map identName args
    body' <- binding ids $ deadCodeElimExp body
    return (fname, rettype, args, body', pos)

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimExp :: Exp -> DCElimM Exp

-----------------------------------------------------------------------------
-- 'trace' exhibits side effects and should not be removed!
deadCodeElimExp (LetPat pat (Apply fname args tp p) body pos)
  | "trace" <- nameToString fname = do
    let ids = getBnds pat
    args' <- mapM (deadCodeElimExp . fst) args
    body' <- binding ids $ deadCodeElimExp body
    return $ LetPat pat (Apply fname (zip args' $ map snd args) tp p) body' pos
-----------------------------------------------------------------------------
 
deadCodeElimExp (LetPat pat e body pos) = do
    let ids = getBnds pat
    (body', noref) <- collectRes ids $ binding ids $ deadCodeElimExp body

    if noref 
    then changed $ return body'
    else do
            e' <- deadCodeElimExp e
            return $ LetPat pat e' body' pos


deadCodeElimExp (LetWith cs nm src idxcs idxs el body pos) = do
    cs' <- mapM deadCodeElimIdent cs
    (body', noref) <- collectRes [identName nm] $ binding [identName nm] $ deadCodeElimExp body
    idxcs' <- case idxcs of
                Nothing     -> return Nothing
                Just idxcs' -> Just <$> mapM deadCodeElimIdent idxcs'
    if noref 
    then changed $ return body'
    else do
            let srcnm = identName src
            in_vtab <- asks $ S.member srcnm . envVtable
            if not in_vtab
            then badDCElimM $ VarNotInFtab pos srcnm
            else do
                    _ <- tell $ DCElimRes False (S.insert srcnm S.empty)
                    idxs' <- mapM deadCodeElimExp idxs
                    el'   <- deadCodeElimExp el
                    return $ LetWith cs' nm src idxcs' idxs' el' body' pos

deadCodeElimExp (DoLoop mergepat mergeexp idd n loopbdy letbdy pos) = do
    let idnms = getBnds mergepat
    (letbdy',noref) <- collectRes idnms $ binding idnms $ deadCodeElimExp letbdy
    if noref 
    then changed $ return letbdy'
    else do mergeexp'   <- deadCodeElimExp mergeexp
            n'      <- deadCodeElimExp n
            loopbdy'<- binding ( identName idd : idnms) $ deadCodeElimExp loopbdy
            return $ DoLoop mergepat mergeexp' idd n' loopbdy' letbdy' pos



deadCodeElimExp e = mapExpM mapper e
  where mapper = identityMapper {
                   mapOnExp = deadCodeElimExp
                 , mapOnLambda = deadCodeElimLambda
                 , mapOnTupleLambda = deadCodeElimTupleLambda
                 , mapOnIdent = deadCodeElimIdent
                 , mapOnCertificates = mapM deadCodeElimIdent
                 }

deadCodeElimIdent :: Ident -> DCElimM Ident
deadCodeElimIdent ident@(Ident vnm _ pos) = do
  in_vtab <- asks $ S.member vnm . envVtable
  if not in_vtab
  then badDCElimM $ VarNotInFtab pos vnm
  else do tell $ DCElimRes False (S.insert vnm S.empty)
          return ident

deadCodeElimLambda :: Lambda -> DCElimM Lambda
deadCodeElimLambda (AnonymFun params body ret pos) = do
    let ids  = map identName params
    body' <- binding ids $ deadCodeElimExp body
    return $ AnonymFun params body' ret pos

deadCodeElimLambda (CurryFun fname curryargexps rettype pos) = do
    curryargexps' <- mapM deadCodeElimExp curryargexps
    return (CurryFun fname curryargexps' rettype pos)

deadCodeElimTupleLambda :: TupleLambda -> DCElimM TupleLambda
deadCodeElimTupleLambda (TupleLambda params body ret pos) = do
  let ids  = map identName params
  body' <- binding ids $ deadCodeElimExp body
  return $ TupleLambda params body' ret pos

--deadCodeElimExp e = do
--    return e


getBnds :: TupIdent -> [VName]
getBnds ( Id (Ident var _ _) ) = [var]
getBnds ( TupId ids _ ) = concatMap getBnds ids
getBnds ( Wildcard _ _ ) = []
