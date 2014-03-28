{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.CopyCtPropFold
  ( copyCtProp
  , copyCtPropOneLambda
  )
  where

import Control.Applicative
import Control.Monad.RWS

import Data.List

import Data.Loc

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import L0C.InternalRep
import L0C.EnablingOpts.EnablingOptErrors
import L0C.EnablingOpts.Simplification
import qualified L0C.Interpreter as Interp
import L0C.MonadFreshNames

-----------------------------------------------------------------
-----------------------------------------------------------------
---- Copy and Constant Propagation + Constant Folding        ----
-----------------------------------------------------------------
-----------------------------------------------------------------

-----------------------------------------------
-- The data to be stored in vtable           --
--   the third param (Bool) indicates if the --
--   binding is to be removed from program   --
-----------------------------------------------

data CtOrId  = Value Value
             -- ^ value for constant propagation

             | VarId VName Type
             -- ^ Variable id for copy propagation

             | SymArr Exp
             -- ^ Various other opportunities for copy propagation.
               deriving (Show)

data CPropEnv = CopyPropEnv {
    envVtable  :: HM.HashMap VName CtOrId,
    program    :: Prog
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
  (vtable, _) <- spartition ok <$> asks envVtable
  local (\e -> e { envVtable = vtable }) m
  where als = identName idd `HS.insert` aliases (identType idd)
        spartition f s = let s' = HM.filter f s
                         in (s', s `HM.difference` s')
        ok (Value {})  = True
        ok (VarId k _) = not $ k `HS.member` als
        ok (SymArr e)  = HS.null $ als `HS.intersection` freeNamesInExp e

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runCPropM :: CPropM a -> CPropEnv -> VNameSource -> Either EnablingOptError (a, VNameSource, CPropRes)
runCPropM  (CPropM a) = runRWST a

badCPropM :: EnablingOptError -> CPropM a
badCPropM = CPropM . lift . Left

-- | Bind a name as a common (non-merge) variable.
bindVar :: CPropEnv -> (VName, CtOrId) -> CPropEnv
bindVar env (name,val) =
  env { envVtable = HM.insert name val $ envVtable env }

bindVars :: CPropEnv -> [(VName, CtOrId)] -> CPropEnv
bindVars = foldl bindVar

binding :: [(VName, CtOrId)] -> CPropM a -> CPropM a
binding bnds = local (`bindVars` bnds)

varLookup :: CPropM VarLookup
varLookup = do
  env <- ask
  return $ \k -> asExp <$> HM.lookup k (envVtable env)
  where asExp (SymArr e)      = e
        asExp (VarId vname t) = subExp $ Var $ Ident vname t noLoc
        asExp (Value val)     = subExp (Constant val noLoc)

-- | Applies Copy/Constant Propagation and Folding to an Entire Program.
copyCtProp :: Prog -> Either EnablingOptError (Bool, Prog)
copyCtProp prog = do
  let env = CopyPropEnv { envVtable = HM.empty, program = prog }
      src = newNameSourceForProg prog
  -- res   <- runCPropM (mapM copyCtPropFun prog) env
  -- let (bs, rs) = unzip res
  (rs, _, res) <- runCPropM (mapM copyCtPropFun $ progFunctions prog) env src
  return (resSuccess res, Prog rs)

copyCtPropFun :: FunDec -> CPropM FunDec
copyCtPropFun (fname, rettype, args, body, pos) = do
  body' <- copyCtPropBody body
  return (fname, rettype, args, body', pos)

-----------------------------------------------------------------
---- Run on Lambda Only!
-----------------------------------------------------------------

copyCtPropOneLambda :: Prog -> Lambda -> Either EnablingOptError Lambda
copyCtPropOneLambda prog lam = do
  let env = CopyPropEnv { envVtable = HM.empty, program = prog }
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
  look <- varLookup
  simplified <- simplifyBinding look (Let pat e')
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
  loopbody' <- copyCtPropBody loopbody
  look      <- varLookup
  let merge' = zip mergepat' mergeexp'
      letbody = Body bnds res
  simplified <- simplifyBinding look (DoLoop merge' idd n' loopbody')
  case simplified of
    Nothing -> do Body bnds' res' <- copyCtPropBody letbody
                  return $ Body (DoLoop merge' idd n' loopbody':bnds') res'
    Just newbnds -> copyCtPropBody $ insertBindings newbnds letbody

copyCtPropBody (Body [] (Result cs es loc)) =
  resultBody <$> copyCtPropCerts cs <*> mapM copyCtPropSubExp es <*> pure loc

copyCtPropSubExp :: SubExp -> CPropM SubExp
copyCtPropSubExp (Var ident@(Ident vnm _ pos)) = do
  bnd <- asks $ HM.lookup vnm . envVtable
  case bnd of
    Just (Value v)
      | isBasicTypeVal v  -> changed $ Constant v pos
    Just (VarId  id' tp1) -> changed $ Var (Ident id' tp1 pos) -- or tp
    Just (SymArr (SubExps [se] _)) -> changed se
    _                              -> Var <$> copyCtPropBnd ident
copyCtPropSubExp (Constant v loc) = return $ Constant v loc

copyCtPropExp :: Exp -> CPropM Exp

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
                Var idd   -> do vv <- asks $ HM.lookup (identName idd) . envVtable
                                case vv of
                                  Just (Value v) -> do
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
    bnd <- asks $ HM.lookup vnm . envVtable
    case bnd of
      Just (VarId  id' tp1) -> changed $ Ident id' tp1 loc
      Nothing               -> copyCtPropBnd ident
      _                     -> copyCtPropBnd ident

copyCtPropCerts :: Certificates -> CPropM Certificates
copyCtPropCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- asks $ HM.lookup (identName idd) . envVtable
          case vv of
            Just (Value (BasicVal Checked)) -> changed []
            Just (VarId  id' tp1)           -> changed [Ident id' tp1 loc]
            _ -> return [idd]
          where loc = srclocOf idd

copyCtPropLambda :: Lambda -> CPropM Lambda
copyCtPropLambda (Lambda params body rettype loc) = do
  params' <- copyCtPropPat params
  body' <- copyCtPropBody body
  rettype' <- mapM copyCtPropType rettype
  return $ Lambda params' body' rettype' loc

isBasicTypeVal :: Value -> Bool
isBasicTypeVal = basicType . valueType

getPropBnds :: [Ident] -> Exp -> [(VName, CtOrId)]
getPropBnds [Ident var _ _] e =
  case e of
    SubExps [Constant v _] _ -> [(var, Value v)]
    SubExps [Var v]        _ -> [(var, VarId (identName v) (identType v))]
    _                        -> [(var, SymArr e)]
getPropBnds ids (SubExps ts _)
  | length ids == length ts =
    concatMap (\(x,y)-> getPropBnds [x] (subExp y)) $ zip ids ts
getPropBnds _ _ = []
