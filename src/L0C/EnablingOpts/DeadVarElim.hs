{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module implements a compiler pass that removes unused
-- @let@-bindings.
module L0C.EnablingOpts.DeadVarElim (
                                deadCodeElim
                            )
  where
 
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Set as S

import L0C.InternalRep

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
    body' <- binding ids $ deadCodeElimBody body
    return (fname, rettype, args, body', pos)

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimSubExp :: SubExp -> DCElimM SubExp
deadCodeElimSubExp (Var ident)      = Var <$> deadCodeElimIdent ident
deadCodeElimSubExp (Constant v loc) = return $ Constant v loc

deadCodeElimBody :: Body -> DCElimM Body

deadCodeElimBody (LetPat pat e body pos) = do
  let idds = map identName pat
  (body', noref) <- collectRes idds $ binding idds $ deadCodeElimBody body

  if noref
  then changed $ return body'
  else do e' <- deadCodeElimExp e
          pat' <- deadCodeElimPat pat
          return $ LetPat pat' e' body' pos

deadCodeElimBody (LetWith cs dest src idxs el body pos) = do
  cs' <- mapM deadCodeElimIdent cs
  (body', noref) <- collectRes [identName dest] $
                    binding [identName dest] $
                    deadCodeElimBody body
  if noref
  then changed $ return body'
  else do
          let srcnm = identName src
          in_vtab <- asks $ S.member srcnm . envVtable
          if not in_vtab
          then badDCElimM $ VarNotInFtab pos srcnm
          else do _ <- tell $ DCElimRes False (S.insert srcnm S.empty)
                  idxs' <- mapM deadCodeElimSubExp idxs
                  el'   <- deadCodeElimSubExp el
                  src'  <- deadCodeElimIdent src
                  dest' <- deadCodeElimBnd dest
                  return $ LetWith cs' dest' src' idxs' el' body' pos

deadCodeElimBody (DoLoop merge idd n loopbdy letbdy pos) = do
  let (mergepat, mergeexp) = unzip merge
      idds = map identName mergepat
  (letbdy',noref) <- collectRes idds $ binding idds $ deadCodeElimBody letbdy
  if noref
  then changed $ return letbdy'
  else do
    mergeexp' <- mapM deadCodeElimSubExp mergeexp
    n'        <- deadCodeElimSubExp n
    loopbdy'  <- binding ( identName idd : idds) $ deadCodeElimBody loopbdy
    mergepat' <- deadCodeElimPat mergepat
    return $ DoLoop (zip mergepat' mergeexp') idd n' loopbdy' letbdy' pos

deadCodeElimBody (Result cs es loc) =
  Result <$> mapM deadCodeElimIdent cs <*> mapM deadCodeElimSubExp es <*> pure loc

deadCodeElimExp :: Exp -> DCElimM Exp
deadCodeElimExp = mapExpM mapper
  where mapper = identityMapper {
                   mapOnExp = deadCodeElimExp
                 , mapOnBody = deadCodeElimBody
                 , mapOnSubExp = deadCodeElimSubExp
                 , mapOnLambda = deadCodeElimLambda
                 , mapOnIdent = deadCodeElimIdent
                 , mapOnCertificates = mapM deadCodeElimIdent
                 }

deadCodeElimIdent :: Ident -> DCElimM Ident
deadCodeElimIdent ident@(Ident vnm t pos) = do
  in_vtab <- asks $ S.member vnm . envVtable
  if not in_vtab
  then badDCElimM $ VarNotInFtab pos vnm
  else do tell $ DCElimRes False (S.insert vnm S.empty)
          dims <- mapM deadCodeElimSubExp $ arrayDims t
          return ident { identType = t `setArrayShape` Shape dims }

deadCodeElimPat :: [IdentBase als Shape] -> DCElimM [IdentBase als Shape]
deadCodeElimPat = mapM deadCodeElimBnd

deadCodeElimBnd :: IdentBase als Shape -> DCElimM (IdentBase als Shape)
deadCodeElimBnd ident = do
  let dims = arrayDims $ identType ident
  dims' <- mapM deadCodeElimSubExp dims
  return $ ident { identType = identType ident
                               `setArrayShape`
                               Shape dims' }

deadCodeElimLambda :: Lambda -> DCElimM Lambda
deadCodeElimLambda (Lambda params body ret pos) = do
  let ids  = map identName params
  body' <- binding ids $ deadCodeElimBody body
  params' <- deadCodeElimPat params
  return $ Lambda params' body' ret pos
