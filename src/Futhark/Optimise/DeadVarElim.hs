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

import qualified Data.HashSet as HS

import Futhark.Representation.AST
import Futhark.Binder

-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Dead-Variable Elimination          ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data DCElimRes = DCElimRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: Names
  -- ^ The hashtable recording the uses
  }

instance Monoid DCElimRes where
  DCElimRes s1 m1 `mappend` DCElimRes s2 m2 =
    DCElimRes (s1 || s2) (m1 `HS.union` m2) --(io1 || io2)
  mempty = DCElimRes False HS.empty -- False

type DCElimM = Writer DCElimRes

runDCElimM :: DCElimM a -> (a, DCElimRes)
runDCElimM = runWriter

collectRes :: [VName] -> DCElimM a -> DCElimM (a, Bool)
collectRes mvars m = pass collect
  where wasNotUsed hashtab vnm =
          return $ not (HS.member vnm hashtab)

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
deadCodeElim :: Proper lore => Prog lore -> Prog lore
deadCodeElim = Prog . map deadCodeElimFun . progFunctions

-- | Applies Dead-Code Elimination to just a single function.
deadCodeElimFun :: Proper lore => FunDec lore -> FunDec lore
deadCodeElimFun (FunDec fname rettype args body loc) =
  let body' = deadCodeElimBody body
  in FunDec fname rettype args body' loc

-- | Applies Dead-Code Elimination to just a single body.
deadCodeElimBody :: Proper lore => Body lore -> Body lore
deadCodeElimBody = fst . runDCElimM . deadCodeElimBodyM

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimSubExp :: SubExp -> DCElimM SubExp
deadCodeElimSubExp (Var ident)      = Var <$> deadCodeElimIdent ident
deadCodeElimSubExp (Constant v loc) = return $ Constant v loc

deadCodeElimBodyM :: Proper lore => Body lore -> DCElimM (Body lore)

deadCodeElimBodyM (Body bodylore (Let pat explore e:bnds) res) = do
  let idds = patternNames pat
  seen $ freeNamesIn explore
  (Body _ bnds' res', noref) <-
    collectRes idds $ do
      _ <- collectRes idds $ deadCodeElimPat pat
      deadCodeElimBodyM $ Body bodylore bnds res
  if noref
  then changed $ return $ Body bodylore bnds' res'
  else do e' <- deadCodeElimExp e
          return $ Body bodylore
                   (Let pat explore e':bnds') res'

deadCodeElimBodyM (Body bodylore [] (Result cs es loc)) = do
  seen $ freeNamesIn bodylore
  Body bodylore [] <$>
    (Result <$> mapM deadCodeElimIdent cs <*>
     mapM deadCodeElimSubExp es <*> pure loc)

deadCodeElimExp :: Proper lore => Exp lore -> DCElimM (Exp lore)
deadCodeElimExp (LoopOp (DoLoop respat merge i bound body loc)) = do
  let (mergepat, mergeexp) = unzip merge
  mapM_ (deadCodeElimBnd . bindeeIdent) mergepat
  mapM_ deadCodeElimSubExp mergeexp
  bound' <- deadCodeElimSubExp bound
  body' <- deadCodeElimBodyM body
  return $ LoopOp $ DoLoop respat merge i bound' body' loc
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
                 , mapOnResType = \rt -> do
                   seen $ freeNamesIn rt
                   return rt
                 , mapOnFParam = \fparam -> do
                   seen $ freeNamesIn fparam
                   return fparam
                 }

deadCodeElimIdent :: Ident -> DCElimM Ident
deadCodeElimIdent ident@(Ident vnm t _) = do
  tell $ DCElimRes False $ HS.singleton vnm
  dims <- mapM deadCodeElimSubExp $ arrayDims t
  return ident { identType = t `setArrayShape` Shape dims }

deadCodeElimPat :: Pattern lore -> DCElimM ()
deadCodeElimPat = mapM_ deadCodeElimBnd . patternIdents

deadCodeElimBnd :: Ident -> DCElimM ()
deadCodeElimBnd = void . deadCodeElimType . identType

deadCodeElimType :: Type -> DCElimM Type
deadCodeElimType t = do
  dims <- mapM deadCodeElimSubExp $ arrayDims t
  return $ t `setArrayDims` dims

deadCodeElimLambda :: Proper lore =>
                      Lambda lore -> DCElimM (Lambda lore)
deadCodeElimLambda (Lambda params body rettype pos) = do
  body' <- deadCodeElimBodyM body
  mapM_ deadCodeElimBnd params
  mapM_ deadCodeElimType rettype
  return $ Lambda params body' rettype pos

seen :: Names -> DCElimM ()
seen = tell . DCElimRes False
