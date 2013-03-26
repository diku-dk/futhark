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
                        envVtable  :: S.Set String
                      , callGraph  :: CallGraph
                  }

data DCElimRes tf = DCElimRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: S.Set String
  -- ^ The hashtable recording the uses
  }


instance Monoid (DCElimRes tf) where
  DCElimRes s1 m1 `mappend` DCElimRes s2 m2 =
    DCElimRes (s1 || s2) (S.union m1 m2)   
  mempty = DCElimRes False S.empty

newtype DCElimM tf a = DCElimM (WriterT (DCElimRes tf) (ReaderT (DCElimEnv tf) (Either EnablingOptError)) a)
    deriving (MonadWriter (DCElimRes tf),
              MonadReader (DCElimEnv tf), Monad, Applicative, Functor)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runDCElimM :: DCElimM tf a -> DCElimEnv tf -> Either EnablingOptError (a, DCElimRes tf)
runDCElimM  (DCElimM a) env = runReaderT (runWriterT a) env

badDCElimM :: EnablingOptError -> DCElimM tf a
badDCElimM = DCElimM . lift . lift . Left



collectRes :: [String] -> DCElimM tf a -> DCElimM tf (a, Bool)
collectRes mvars m = pass collect
  where wasNotUsed hashtab vnm = do
          return $ not (S.member vnm hashtab)

        collect = do
          (x,res) <- listen m
          tmps    <- mapM (wasNotUsed (resMap res)) mvars
          let unused = foldl (&&) True tmps
          return ( (x, unused), const $ res )

changed :: DCElimM tf a -> DCElimM tf a
changed m = pass collect
    where
     collect = do
        (x, res) <- listen m
        return (x, const $ res { resSuccess = True })

-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: DCElimEnv tf -> String -> DCElimEnv tf
bindVar env name =
  env { envVtable = S.insert name $ envVtable env }

bindVars :: DCElimEnv tf -> [String] -> DCElimEnv tf
bindVars = foldl bindVar

binding :: [String] -> DCElimM tf a -> DCElimM tf a
binding bnds = local (`bindVars` bnds)


-- | Applies Dead-Code Elimination to an Entire Program.
-- TypeBox tf => 
deadCodeElim :: TypeBox tf => Prog tf -> Either EnablingOptError (Bool, Prog tf)
deadCodeElim prog = do
    cg <- buildCG prog
    let env = DCElimEnv { envVtable = S.empty, callGraph = cg }
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


deadCodeElimExp (LetPat pat e body pos) = do
    let ids = getBnds pat
    (body', noref) <- collectRes ids $ binding ids $ deadCodeElimExp body

    cg  <- asks $ callGraph
    let torem = noref && not (hasIO cg e)
    if torem 
    then changed $ return body'
    else do
            e' <- deadCodeElimExp e
            return $ LetPat pat e' body' pos


deadCodeElimExp (LetWith nm src inds el body pos) = do
    (body', noref) <- collectRes [identName nm] $ binding [identName nm] $ deadCodeElimExp body

    cg  <- asks $ callGraph
    
    let torem = noref && not ( foldl (||) False ( map (hasIO cg) (el:inds) ) ) 
    if torem 
    then changed $ return body'
    else do
            let srcnm = identName src
            in_vtab <- asks $ S.member srcnm . envVtable
            if not in_vtab
            then badDCElimM $ TypeError pos  ("Var "++srcnm++" not in vtable!")
            else do
                    _ <- tell $ DCElimRes False (S.insert srcnm S.empty)
                    inds' <- mapM deadCodeElimExp inds
                    el'   <- deadCodeElimExp el
                    return $ LetWith nm src inds' el' body' pos


deadCodeElimExp e@(Var (Ident vnm _ pos)) = do 
    in_vtab <- asks $ S.member vnm . envVtable
    if not in_vtab
    then badDCElimM $ TypeError pos  ("Var "++vnm ++" not in vtable!")
    else do
            _ <- tell $ DCElimRes False (S.insert vnm S.empty)
            return e


deadCodeElimExp (Index s idxs t1 t2 pos) = do
    let vnm = identName s
    in_vtab <- asks $ S.member vnm . envVtable
    if not in_vtab
    then badDCElimM $ TypeError pos  ("Var "++vnm ++" not in vtable!")
    else do
            _ <- tell $ DCElimRes False (S.insert vnm S.empty)
            idxs' <- mapM deadCodeElimExp idxs
            return $ Index s idxs' t1 t2 pos


deadCodeElimExp (DoLoop ind n body mergevars pos) = do
    n'    <- deadCodeElimExp n
    body' <- binding [identName ind] $ deadCodeElimExp body
    let mergenms = map identName mergevars
    _ <- tell $ DCElimRes False (S.fromList mergenms)
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
    body' <- binding ids $ deadCodeElimExp body
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
