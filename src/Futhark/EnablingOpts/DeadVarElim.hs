{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module implements a compiler pass that removes unused
-- @let@-bindings.
module Futhark.EnablingOpts.DeadVarElim (
                                deadCodeElim
                            )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.Set as S

import Futhark.InternalRep

import Futhark.EnablingOpts.EnablingOptErrors

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

deadCodeElimBody (Body (Let pat e:bnds) res) = do
  let idds = map identName pat
  ((pat',Body bnds' res'), noref) <-
    collectRes idds $ binding idds $ do
      (pat', _) <- collectRes idds $ deadCodeElimPat pat
      body <- deadCodeElimBody $ Body bnds res
      return (pat', body)

  if noref
  then changed $ return $ Body bnds' res'
  else do e' <- deadCodeElimExp e
          return $ Body (Let pat' e':bnds') res'

deadCodeElimBody (Body (DoLoop merge idd n loopbdy:bnds) res) = do
  let (mergepat, mergeexp) = unzip merge
      idds = map identName mergepat
  ((mergepat', Body bnds' res'),noref) <-
    collectRes idds $ binding idds $ do
      (mergepat', _) <- collectRes idds $ deadCodeElimPat mergepat
      body' <- deadCodeElimBody $ Body bnds res
      return (mergepat', body')
  if noref
  then changed $ return $ Body bnds' res'
  else do
    mergeexp' <- mapM deadCodeElimSubExp mergeexp
    n'        <- deadCodeElimSubExp n
    loopbdy'  <- binding ( identName idd : idds) $ deadCodeElimBody loopbdy
    return $ Body (DoLoop (zip mergepat' mergeexp') idd n' loopbdy':bnds') res'

deadCodeElimBody (Body [] (Result cs es loc)) =
  resultBody <$> mapM deadCodeElimIdent cs <*>
                 mapM deadCodeElimSubExp es <*> pure loc

deadCodeElimExp :: Exp -> DCElimM Exp
deadCodeElimExp = mapExpM mapper
  where mapper = Mapper {
                   mapOnExp = deadCodeElimExp
                 , mapOnBody = deadCodeElimBody
                 , mapOnSubExp = deadCodeElimSubExp
                 , mapOnLambda = deadCodeElimLambda
                 , mapOnIdent = deadCodeElimIdent
                 , mapOnCertificates = mapM deadCodeElimIdent
                 , mapOnType = deadCodeElimType
                 , mapOnValue = return
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
  t <- deadCodeElimType $ identType ident
  return $ ident { identType = t }

deadCodeElimType :: TypeBase als Shape -> DCElimM (TypeBase als Shape)
deadCodeElimType t = do
  dims <- mapM deadCodeElimSubExp $ arrayDims t
  return $ t `setArrayDims` dims

deadCodeElimLambda :: Lambda -> DCElimM Lambda
deadCodeElimLambda (Lambda params body rettype pos) = do
  let ids  = map identName params
  body' <- binding ids $ deadCodeElimBody body
  params' <- deadCodeElimPat params
  rettype' <- mapM deadCodeElimType rettype
  return $ Lambda params' body' rettype' pos
