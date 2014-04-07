{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.EnablingOpts.CopyCtPropFold
  ( copyCtProp
  , copyCtPropOneLambda
  )
  where

import Control.Applicative
import Control.Monad.RWS

import Data.List
import Data.Maybe

import Data.Loc

import qualified Data.HashSet      as HS

import Futhark.InternalRep
import Futhark.EnablingOpts.EnablingOptErrors
import qualified Futhark.EnablingOpts.SymbolTable as ST
import Futhark.EnablingOpts.Simplification
import qualified Futhark.Interpreter as Interp
import Futhark.MonadFreshNames

-----------------------------------------------------------------
-----------------------------------------------------------------
---- Copy and Constant Propagation + Constant Folding        ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data CPropEnv = CopyPropEnv {
    envVtable  :: ST.SymbolTable
  , program    :: Prog
  }


data CPropRes = CPropRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  }


instance Monoid CPropRes where
  CPropRes c1 `mappend` CPropRes c2 =
    CPropRes (c1 || c2)
  mempty = CPropRes False

newtype CPropM a = CPropM (RWST CPropEnv
                                CPropRes
                                VNameSource
                                (Either EnablingOptError)
                                a)
  deriving (MonadReader CPropEnv,
            MonadWriter CPropRes,
            MonadState VNameSource,
            Monad, Applicative, Functor)

instance MonadFreshNames CPropM where
  getNameSource = get
  putNameSource = put

-- | We changed part of the AST, and this is the result.  For
-- convenience, use this instead of 'return'.
changed :: a -> CPropM a
changed x = do
  tell $ CPropRes True
  return x

-- | The identifier was consumed, and should not be used within the
-- body, so mark it and any aliases as nonremovable (with
-- 'nonRemovable') and delete any expressions using it or an alias
-- from the symbol table.
consuming :: Ident -> CPropM a -> CPropM a
consuming idd m = do
  vtable <- ST.filter ok <$> asks envVtable
  local (\e -> e { envVtable = vtable }) m
  where als = identName idd `HS.insert` aliases (identType idd)
        ok entry  = HS.null $ als `HS.intersection`
                    maybe HS.empty freeNamesInExp (ST.asExp entry)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runCPropM :: CPropM a -> CPropEnv -> VNameSource -> Either EnablingOptError (a, VNameSource, CPropRes)
runCPropM  (CPropM a) = runRWST a

badCPropM :: EnablingOptError -> CPropM a
badCPropM = CPropM . lift . Left

localVtable :: (ST.SymbolTable -> ST.SymbolTable) -> CPropM a -> CPropM a
localVtable f = local $ \env -> env { envVtable = f $ envVtable env }

binding :: [(VName, Exp)] -> CPropM a -> CPropM a
binding = localVtable . flip (foldr $ uncurry ST.insert)

bindParams :: [Param] -> CPropM a -> CPropM a
bindParams params =
  localVtable $ \vtable ->
    let vtable' = foldr (ST.insert' . identName) vtable params
    in foldr (`ST.isAtLeast` 0) vtable' sizevars
  where sizevars = mapMaybe isVar $ concatMap (arrayDims . identType) params
        isVar (Var v) = Just $ identName v
        isVar _       = Nothing

bindLoopVar :: Ident -> SubExp -> CPropM a -> CPropM a
bindLoopVar var upper =
  localVtable $ clampUpper . clampVar
  where -- If we enter the loop, then 'var' is at least zero, and at
        -- most 'upper'-1 (so this is not completely tight - FIXME).
        clampVar = ST.insertBounded (identName var) (Just zero, Just upper)
        -- If we enter the loop, then 'upper' is at least one.
        clampUpper = case upper of Var v -> ST.isAtLeast (identName v) 1
                                   _     -> id
        zero = Constant (BasicVal $ IntVal 0) $ srclocOf var

-- | Applies Copy/Constant Propagation and Folding to an Entire Program.
copyCtProp :: Prog -> Either EnablingOptError (Bool, Prog)
copyCtProp prog = do
  let env = CopyPropEnv { envVtable = ST.empty, program = prog }
      src = newNameSourceForProg prog
  -- res   <- runCPropM (mapM copyCtPropFun prog) env
  -- let (bs, rs) = unzip res
  (rs, _, res) <- runCPropM (mapM copyCtPropFun $ progFunctions prog) env src
  return (resSuccess res, Prog rs)

copyCtPropFun :: FunDec -> CPropM FunDec
copyCtPropFun (fname, rettype, params, body, pos) = do
  body' <- bindParams params $ copyCtPropBody body
  return (fname, rettype, params, body', pos)

-----------------------------------------------------------------
---- Run on Lambda Only!
-----------------------------------------------------------------

copyCtPropOneLambda :: Prog -> Lambda -> Either EnablingOptError Lambda
copyCtPropOneLambda prog lam = do
  let env = CopyPropEnv { envVtable = ST.empty, program = prog }
      src = newNameSourceForProg prog
  (res, _, _) <- runCPropM (copyCtPropLambda lam) env src
  return res

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main functions: Copy/Ct propagation and folding for exps   ----
--------------------------------------------------------------------
--------------------------------------------------------------------

copyCtPropBody :: Body -> CPropM Body

copyCtPropBody (Body (LetWith cs dest src inds el:bnds) res) = do
  src' <- copyCtPropIdent src
  consuming src' $ do
    cs'             <- copyCtPropCerts cs
    el'             <- copyCtPropSubExp el
    inds'           <- mapM copyCtPropSubExp inds
    dest'           <- copyCtPropBnd dest
    Body bnds' res' <- copyCtPropBody $ Body bnds res
    return $ Body (LetWith cs' dest' src' inds' el' :bnds') res'

copyCtPropBody (Body (Let pat e:bnds) res) = do
  pat' <- copyCtPropPat pat
  e' <- copyCtPropExp e
  vtable <- asks envVtable
  simplified <- simplifyBinding vtable (Let pat e')
  let body = Body bnds res
  case simplified of
    Just newbnds ->
      copyCtPropBody $ insertBindings newbnds body
    Nothing   -> do
      let patbnds = getPropBnds pat' e'
      Body bnds' res' <- binding patbnds $ copyCtPropBody body
      return $ Body (Let pat' e':bnds') res'

copyCtPropBody (Body (DoLoop merge idd n loopbody:bnds) res) = do
  let (mergepat, mergeexp) = unzip merge
  mergepat' <- copyCtPropPat mergepat
  mergeexp' <- mapM copyCtPropSubExp mergeexp
  n'        <- copyCtPropSubExp n
  loopbody' <- bindLoopVar idd n $ copyCtPropBody loopbody
  let merge' = zip mergepat' mergeexp'
      letbody = Body bnds res
  vtable <- asks envVtable
  simplified <- simplifyBinding vtable $ DoLoop merge' idd n' loopbody'
  case simplified of
    Nothing -> do Body bnds' res' <- copyCtPropBody letbody
                  return $ Body (DoLoop merge' idd n' loopbody':bnds') res'
    Just newbnds -> copyCtPropBody $ insertBindings newbnds letbody

copyCtPropBody (Body [] (Result cs es loc)) =
  resultBody <$> copyCtPropCerts cs <*> mapM copyCtPropSubExp es <*> pure loc

copyCtPropSubExp :: SubExp -> CPropM SubExp
copyCtPropSubExp (Var ident@(Ident vnm _ pos)) = do
  bnd <- asks $ ST.lookup vnm . envVtable
  case bnd of
    Just (ST.Value v)
      | isBasicTypeVal v  -> changed $ Constant v pos
    Just (ST.VarId  id' tp1) -> changed $ Var (Ident id' tp1 pos) -- or tp
    Just (ST.SymExp (SubExps [se] _)) -> changed se
    _                                 -> Var <$> copyCtPropBnd ident
copyCtPropSubExp (Constant v loc) = return $ Constant v loc

copyCtPropExp :: Exp -> CPropM Exp

copyCtPropExp (If cond tbranch fbranch t loc) = do
  -- Here, we have to check whether 'cond' puts a bound on some free
  -- variable, and if so, chomp it.
  cond' <- copyCtPropSubExp cond
  tbranch' <- localVtable (ST.updateBounds True cond) $
              copyCtPropBody tbranch
  fbranch' <- localVtable (ST.updateBounds False cond) $
              copyCtPropBody fbranch
  t' <- mapM copyCtPropType t
  return $ If cond' tbranch' fbranch' t' loc

-- The simplification engine cannot handle Apply, because it requires
-- access to the full program.
copyCtPropExp (Apply fname args tp pos) = do
    args' <- mapM (copyCtPropSubExp . fst) args
    (all_are_vals, vals) <- allArgsAreValues args'
    if all_are_vals
    then do prg <- asks program
            let vv = Interp.runFunNoTrace fname vals  prg
            case vv of
              Right [v] -> changed $ subExp $ Constant v pos
              Right vs  -> changed $ SubExps (map (`Constant` pos) vs) pos
              Left e    -> badCPropM $ EnablingOptError
                           pos (" Interpreting fun " ++ nameToString fname ++
                                " yields error:\n" ++ show e)
    else return $ Apply fname (zip args' $ map snd args) tp pos

    where
        allArgsAreValues :: [SubExp] -> CPropM (Bool, [Value])
        allArgsAreValues []     = return (True, [])
        allArgsAreValues (a:as) =
            case a of
                Constant v _ -> do (res, vals) <- allArgsAreValues as
                                   if res then return (True,  v:vals)
                                          else return (False, []    )
                Var idd   -> do vv <- asks $ ST.lookup (identName idd) . envVtable
                                case vv of
                                  Just (ST.Value v) -> do
                                    (res, vals) <- allArgsAreValues as
                                    if res then return (True,  v:vals)
                                           else return (False, []    )
                                  _ -> return (False, [])

copyCtPropExp e = mapExpM mapper e
  where mapper = Mapper {
                   mapOnExp = copyCtPropExp
                 , mapOnBody = copyCtPropBody
                 , mapOnSubExp = copyCtPropSubExp
                 , mapOnLambda = copyCtPropLambda
                 , mapOnIdent = copyCtPropIdent
                 , mapOnCertificates = copyCtPropCerts
                 , mapOnType = copyCtPropType
                 , mapOnValue = return
                 }

copyCtPropPat :: [IdentBase als Shape] -> CPropM [IdentBase als Shape]
copyCtPropPat = mapM copyCtPropBnd

copyCtPropBnd :: IdentBase als Shape -> CPropM (IdentBase als Shape)
copyCtPropBnd (Ident vnm t loc) = do
  t' <- copyCtPropType t
  return $ Ident vnm t' loc

copyCtPropType :: TypeBase als Shape -> CPropM (TypeBase als Shape)
copyCtPropType t = do
  dims <- mapM copyCtPropSubExp $ arrayDims t
  return $ t `setArrayDims` dims

copyCtPropIdent :: Ident -> CPropM Ident
copyCtPropIdent ident@(Ident vnm _ loc) = do
    bnd <- asks $ ST.lookup vnm . envVtable
    case bnd of
      Just (ST.VarId  id' tp1) -> changed $ Ident id' tp1 loc
      Nothing                  -> copyCtPropBnd ident
      _                        -> copyCtPropBnd ident

copyCtPropCerts :: Certificates -> CPropM Certificates
copyCtPropCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- asks $ ST.lookup (identName idd) . envVtable
          case vv of
            Just (ST.Value (BasicVal Checked)) -> changed []
            Just (ST.VarId  id' tp1)           -> changed [Ident id' tp1 loc]
            _ -> return [idd]
          where loc = srclocOf idd

copyCtPropLambda :: Lambda -> CPropM Lambda
copyCtPropLambda (Lambda params body rettype loc) = do
  params' <- copyCtPropPat params
  body' <- bindParams params' $ copyCtPropBody body
  rettype' <- mapM copyCtPropType rettype
  return $ Lambda params' body' rettype' loc

isBasicTypeVal :: Value -> Bool
isBasicTypeVal = basicType . valueType

getPropBnds :: [Ident] -> Exp -> [(VName, Exp)]
getPropBnds [Ident var _ _] e = [(var, e)]
getPropBnds ids (SubExps ts _)
  | length ids == length ts =
    concatMap (\(x,y)-> getPropBnds [x] (subExp y)) $ zip ids ts
getPropBnds _ _ = []
