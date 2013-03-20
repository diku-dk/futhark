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

import qualified Data.Map as M

import L0.AbSyn
 
import L0.EnablingOpts.GenPatterns
import L0.EnablingOpts.InliningDeadFun
import L0.EnablingOpts.EnablingOptErrors



-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Dead-Variable Elimination          ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data DCElimEnv tf = DCElimEnv {   
                        envVtable  :: M.Map String Int
                      , callGraph  :: CallGraph
                  }

data DCElimRes tf = DCElimRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: M.Map String Int
  -- ^ The hashtable recording the uses
  }


instance Monoid (DCElimRes tf) where
  DCElimRes s1 m1 `mappend` DCElimRes s2 m2 =
    DCElimRes (s1 || s2) (M.union m1 m2)   
  mempty = DCElimRes False M.empty

newtype DCElimM tf a = DCElimM (WriterT (DCElimRes tf) (ReaderT (DCElimEnv tf) (Either EnablingOptError)) a)
    deriving (MonadWriter (DCElimRes tf),
              MonadReader (DCElimEnv tf), Monad, Applicative, Functor)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runDCElimM :: DCElimM tf a -> DCElimEnv tf -> Either EnablingOptError (a, DCElimRes tf)
runDCElimM  (DCElimM a) env = runReaderT (runWriterT a) env

badCPropM :: EnablingOptError -> DCElimM tf a
badCPropM = DCElimM . lift . lift . Left



collectRes :: [String] -> DCElimM tf a -> DCElimM tf (a, Bool)
collectRes mvars m = pass collect
  where wasNotUsed hashtab vnm = do
          let tmp = M.lookup vnm hashtab
          case tmp of
            Nothing -> return True
            Just _  -> return False
        collect = do
          (x,res) <- listen m
          tmps    <- mapM (wasNotUsed (resMap res)) mvars
          let unused = foldl (&&) True tmps
          return ( (x, unused), const $ res )
                  -- const $ res) { resSuccess = resSuccess res || unused })

changed :: DCElimM tf a -> DCElimM tf a
changed m = pass collect
    where
     collect = do
        (x, res) <- listen m
        return (x, const $ res { resSuccess = True })

-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: DCElimEnv tf -> (String, Int) -> DCElimEnv tf
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: DCElimEnv tf -> [(String, Int)] -> DCElimEnv tf
bindVars = foldl bindVar

binding :: [(String, Int)] -> DCElimM tf a -> DCElimM tf a
binding bnds = local (`bindVars` bnds)

-- | Remove the binding for a name.
-- TypeBox tf => 
--remVar :: DCElimEnv tf -> String -> DCElimEnv tf
--remVar env name = env { envVtable = M.delete name $ envVtable env }
--
--remVars :: DCElimEnv tf -> [String] -> DCElimEnv tf
--remVars = foldl remVar
--
--remBindings :: [String] -> DCElimM tf a -> DCElimM tf a
--remBindings keys = local (`remVars` keys)

-- | Applies Copy/Constant Propagation and Folding to an Entire Program.
-- TypeBox tf => 
deadCodeElim :: TypeBox tf => Prog tf -> Either EnablingOptError (Bool, Prog tf)
deadCodeElim prog = do
    cg <- buildCG prog
    let env = DCElimEnv { envVtable = M.empty, callGraph = cg }
    (rs, res) <- runDCElimM (mapM deadCodeElimFun prog) env
    return (resSuccess res, rs)

deadCodeElimFun :: TypeBox tf => FunDec tf -> DCElimM tf (FunDec tf)
deadCodeElimFun (fname, rettype, args, body, pos) = do
    let ids = map identName args
    let bnds= zip ids (replicate (length ids) 0)
    body' <- binding bnds $ deadCodeElimExp body
    return (fname, rettype, args, body', pos)
    --return (fname, rettype, args, body, pos)

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Dead-Code Elimination for exps              ----
--------------------------------------------------------------------
--------------------------------------------------------------------

deadCodeElimExp :: TypeBox tf => Exp tf -> DCElimM tf (Exp tf)


deadCodeElimExp (LetPat pat e body pos) = do
    let ids = getBnds pat
    let bnds= zip ids (replicate (length ids) 0)
    (body', noref) <- collectRes ids $ binding bnds $ deadCodeElimExp body

    cg  <- asks $ callGraph
    let torem = noref && not (hasIO cg e)
    if torem 
    then changed $ return body'
    else do
            e' <- deadCodeElimExp e
            return $ LetPat pat e' body' pos


deadCodeElimExp (LetWith nm src inds el body pos) = do
    (body', noref) <- collectRes [identName nm] $ binding [(identName nm, 0)] $ deadCodeElimExp body

    cg  <- asks $ callGraph
    
    let torem = noref && not ( foldl (||) False ( map (hasIO cg) (el:inds) ) ) 
    if torem 
    then changed $ return body'
    else do
            let srcnm = identName src
            bnd   <- asks $ M.lookup srcnm . envVtable
            case bnd of
                Nothing -> badCPropM $ TypeError pos  ("Var "++srcnm++" not in vtable!")
                Just _  -> do
                    _ <- tell $ DCElimRes False (M.fromList [(srcnm, 1)])
                    inds' <- mapM deadCodeElimExp inds
                    el'   <- deadCodeElimExp el
                    return $ LetWith nm src inds' el' body' pos


deadCodeElimExp e@(Var (Ident vnm _ pos)) = do 
    bnd <- asks $ M.lookup vnm . envVtable
    case bnd of
        Nothing -> badCPropM $ TypeError pos  ("Var "++vnm ++" not in vtable!")
        Just _  -> do
            _ <- tell $ DCElimRes False (M.fromList [(vnm, 1)])
            return e

deadCodeElimExp (Index s idxs t1 t2 pos) = do
    let vnm = identName s
    bnd <- asks $ M.lookup vnm . envVtable
    case bnd of
        Nothing -> badCPropM $ TypeError pos  ("Var "++vnm ++" not in vtable!")
        Just _  -> do
            _ <- tell $ DCElimRes False (M.fromList [(vnm, 1)])
            idxs' <- mapM deadCodeElimExp idxs
            return $ Index s idxs' t1 t2 pos

deadCodeElimExp (DoLoop ind n body mergevars pos) = do
    n'    <- deadCodeElimExp n
    body' <- binding [(identName ind, 0)] $ deadCodeElimExp body
    let mergenms = map identName mergevars
    let mergebnds= zip mergenms (replicate (length mergenms) 1)
    _ <- tell $ DCElimRes False (M.fromList mergebnds)
    return $ DoLoop ind n' body' mergevars pos



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
    let bnds = zip ids (replicate (length ids) 0)
    body' <- binding bnds $ deadCodeElimExp body
    return $ AnonymFun params body' ret pos

deadCodeElimLambda (CurryFun fname curryargexps rettype pos) = do
    curryargexps' <- mapM deadCodeElimExp curryargexps
    return (CurryFun fname curryargexps' rettype pos)

--deadCodeElimExp e = do
--    return e


getBnds :: TypeBox tf => TupIdent tf -> [String]
getBnds ( Id (Ident var _ _) ) = [var]
getBnds ( TupId ids _ ) = foldl (++) [] (map getBnds ids)

hasIO :: TypeBox tf => CallGraph -> Exp tf -> Bool
hasIO _  (Read  _ _    ) = True
hasIO _  (Write _ _ _  ) = True
hasIO cg (Apply f _ _ _) = 
    case M.lookup f cg of
        Just (_,_,has_io) -> has_io
        Nothing           -> error $ "In hasIO, DeadVarElims: Function "++f++" not in call graph!"
    
hasIO call_graph cur_exp =
    let (_, has_io) = foldlPattern combine doLam (call_graph, False) cur_exp
    in  has_io

    where
        combine :: TypeBox tf => (CallGraph, Bool) -> Exp tf -> (CallGraph, Bool)
        combine (cg, has_io) e = (cg, has_io || hasIO cg e)

        doLam :: TypeBox tf => (CallGraph, Bool) -> Lambda tf -> (CallGraph, Bool)
        doLam (cg, hio) (AnonymFun _    _ _ _) = (cg, hio)
        doLam (cg, hio) (CurryFun fname _ _ _) = 
            case M.lookup fname cg of
                Just (_,_,has_io') -> (cg, hio || has_io')
                Nothing            -> error $ "In hasIO, DeadVarElims: Function "++fname++" not in call graph!" 
                                      --(cg, True)


--foldlPattern :: TypeBox tf =>   (a -> Exp tf    -> a) -> 
--                                (a -> Lambda tf -> a) -> 
--                                a -> Exp tf -> a
