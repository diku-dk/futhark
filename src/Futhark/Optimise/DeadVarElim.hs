-- | This module implements a compiler pass that removes unused
-- @let@-bindings.
module Futhark.Optimise.DeadVarElim
       ( deadCodeElim
       , deadCodeElimFun
       , deadCodeElimBody
       )
  where

import Control.Applicative
import Control.Monad.Writer

import qualified Data.Set as S

import Futhark.Representation.Basic

-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Dead-Variable Elimination          ----
-----------------------------------------------------------------
-----------------------------------------------------------------

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

type DCElimM = Writer DCElimRes

runDCElimM :: DCElimM a -> (a, DCElimRes)
runDCElimM = runWriter

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

-- | Applies Dead-Code Elimination to an entire program.
deadCodeElim :: Prog -> Prog
deadCodeElim = Prog . map deadCodeElimFun . progFunctions

-- | Applies Dead-Code Elimination to just a single function.
deadCodeElimFun :: FunDec -> FunDec
deadCodeElimFun (fname, rettype, args, body, loc) =
  let body' = deadCodeElimBody body
  in (fname, rettype, args, body', loc)

-- | Applies Dead-Code Elimination to just a single body.
deadCodeElimBody :: Body -> Body
deadCodeElimBody = fst . runDCElimM . deadCodeElimBodyM

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimSubExp :: SubExp -> DCElimM SubExp
deadCodeElimSubExp (Var ident)      = Var <$> deadCodeElimIdent ident
deadCodeElimSubExp (Constant v loc) = return $ Constant v loc

deadCodeElimBodyM :: Body -> DCElimM Body

deadCodeElimBodyM (Body (Let pat () e:bnds) res) = do
  let idds = map identName pat
  ((pat',Body bnds' res'), noref) <-
    collectRes idds $ do
      (pat', _) <- collectRes idds $ deadCodeElimPat pat
      body <- deadCodeElimBodyM $ Body bnds res
      return (pat', body)

  if noref
  then changed $ return $ Body bnds' res'
  else do e' <- deadCodeElimExp e
          return $ Body (Let pat' () e':bnds') res'

deadCodeElimBodyM (Body [] (Result cs es loc)) =
  resultBody <$> mapM deadCodeElimIdent cs <*>
                 mapM deadCodeElimSubExp es <*> pure loc

deadCodeElimExp :: Exp -> DCElimM Exp
deadCodeElimExp (DoLoop respat merge i bound body loc) = do
  let (mergepat, mergeexp) = unzip merge
  mergepat' <- mapM deadCodeElimBnd mergepat
  mergeexp' <- mapM deadCodeElimSubExp mergeexp
  bound' <- deadCodeElimSubExp bound
  body' <- deadCodeElimBodyM body
  return $ DoLoop respat (zip mergepat' mergeexp') i bound' body' loc
deadCodeElimExp e = mapExpM mapper e
  where mapper = Mapper {
                   mapOnBinding = return -- Handled in case for Body.
                 , mapOnBody = deadCodeElimBodyM
                 , mapOnSubExp = deadCodeElimSubExp
                 , mapOnLambda = deadCodeElimLambda
                 , mapOnIdent = deadCodeElimIdent
                 , mapOnCertificates = mapM deadCodeElimIdent
                 , mapOnType = deadCodeElimType
                 , mapOnValue = return
                 }

deadCodeElimIdent :: Ident -> DCElimM Ident
deadCodeElimIdent ident@(Ident vnm t _) = do
  tell $ DCElimRes False (S.insert vnm S.empty)
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
  body' <- deadCodeElimBodyM body
  params' <- deadCodeElimPat params
  rettype' <- mapM deadCodeElimType rettype
  return $ Lambda params' body' rettype' pos
