{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0C.EnablingOpts.InliningDeadFun  (
                        CallGraph
                      , buildCG
                      , aggInlineDriver
                      , deadFunElim
                      , mkUnnamedLamPrg
                    )
  where 

import Control.Arrow
import Control.Applicative
import Control.Monad.Reader
 
--import Data.Either

--import Control.Monad.State
import Data.List
import Data.Loc

import qualified Data.Map as M

import L0C.L0

import L0C.EnablingOpts.EnablingOptErrors


------------------------------------------------------------------------------
------------------------------------------------------------------------------
----  Simple Transformation to replace soac's curried with unnamed lambda 
------------------------------------------------------------------------------
------------------------------------------------------------------------------

mkUnnamedLamPrg :: Prog Type -> Prog Type
mkUnnamedLamPrg prog = 
    let fnms = map (\(nm,_,_,_,_)->nm) prog
        ftab = M.fromList $ zip fnms prog
    in  map (mkUnnamedLamFun ftab) prog

mkUnnamedLamFun :: M.Map Name (FunDec Type) -> FunDec Type -> FunDec Type
mkUnnamedLamFun ftab (fnm,rtp,idds,body,pos) = 
    let body' = mkUnnamedLamExp ftab body
    in  (fnm,rtp,idds,body',pos)

mkUnnamedLamExp :: M.Map Name (FunDec Type) -> Exp Type -> Exp Type
mkUnnamedLamExp ftab (Mapall lam arrr pos) = 
    Mapall (mkUnnamedLamLam ftab lam) (mkUnnamedLamExp ftab arrr) pos
mkUnnamedLamExp ftab (Map lam arrr eltp pos) = 
    Map    (mkUnnamedLamLam ftab lam) (mkUnnamedLamExp ftab arrr) eltp pos
mkUnnamedLamExp ftab (Filter lam arrr eltp pos) = 
    Filter (mkUnnamedLamLam ftab lam) (mkUnnamedLamExp ftab arrr) eltp pos
mkUnnamedLamExp ftab (Reduce lam ne arrr eltp pos) = 
    Reduce (mkUnnamedLamLam ftab lam) (mkUnnamedLamExp ftab ne) (mkUnnamedLamExp ftab arrr) eltp pos
mkUnnamedLamExp ftab (Scan lam ne arrr eltp pos) = 
    Scan   (mkUnnamedLamLam ftab lam) (mkUnnamedLamExp ftab ne) (mkUnnamedLamExp ftab arrr) eltp pos
mkUnnamedLamExp ftab (Redomap lam1 lam2 ne arrr eltp pos) = 
    Redomap (mkUnnamedLamLam ftab lam1) (mkUnnamedLamLam ftab lam2) 
            (mkUnnamedLamExp ftab ne  ) (mkUnnamedLamExp ftab arrr) eltp pos

mkUnnamedLamExp ftab e = buildExpPattern (mkUnnamedLamExp ftab) e

mkUnnamedLamLam ::  M.Map Name (FunDec Type) -> Lambda Type -> Lambda Type
mkUnnamedLamLam ftab (AnonymFun ids body  tp pos) = 
    AnonymFun ids (mkUnnamedLamExp ftab body) tp pos
mkUnnamedLamLam ftab (CurryFun  nm params tp pos) = 
    case M.lookup nm ftab of
        Nothing                      -> 
            CurryFun nm (map (mkUnnamedLamExp ftab) params) tp pos
        Just (fnm,_,idds,_,_) -> 
            let idds' = drop (length params) idds  
                args  = params ++ (map (\x->Var x) idds')
            in  AnonymFun idds' (Apply fnm args tp pos) tp pos

------------------------------------------------------------------------------
------------------------------------------------------------------------------
----  Call Graph And Inlining
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | The Call Graph is just a Map from a function name, i.e., the caller,
-- to a three-element tuple: The first element is a list that contains the 
-- (unique) function names that may be called directly from the current function, 
-- i.e., ``apply'' callees. The second element is a list that contains the (unique) 
-- function names that may be called via SOACs. The third element is a boolean
-- that specifies whether the current function, i.e., the caller, contains IO ops.
type CallGraph = M.Map Name ([Name],[Name])

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: M.Map Name FunDec }

newtype CGM a = CGM (ReaderT CGEnv (Either EnablingOptError) a)
    deriving (MonadReader CGEnv, Monad, Applicative, Functor)

-- | Building the call grah runs in this monad.  There is no
-- mutable state.
runCGM :: CGM a -> CGEnv -> Either EnablingOptError a
runCGM  (CGM a) = runReaderT a

badCGM :: EnablingOptError -> CGM a
badCGM = CGM . lift . Left


-- | @buildCG prog@ build the program's Call Graph. The representation
-- is a hashtable that maps function names to a list of callee names.
buildCG :: Prog -> Either EnablingOptError CallGraph
buildCG prog = do
    env <- foldM expand env0 (progFunctions prog)
    runCGM (buildCGfun M.empty defaultEntryPoint) env

    where
        env0 = CGEnv { envFtable = M.empty }
        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise =
                Right CGEnv { envFtable = M.insert name f (envFtable ftab) }


-- | @buildCG cg fname@ updates Call Graph @cg@ with the contributions of function 
-- @fname@, and recursively, with the contributions of the callees of @fname@.
-- In particular, @buildCGfun M.empty defaultEntryPoint@ should construct the Call Graph
-- of the whole program.
buildCGfun :: CallGraph -> Name -> CGM CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> badCGM $ FunctionNotInFtab fname
    Just (caller,_,_,body,pos) -> 
      if caller == fname
      then
        case M.lookup caller cg of
          Just _  -> return cg
          Nothing -> do let callees@(fs, soacs) = buildCGexp ([],[]) body

                        -- at this point caller is only known whether it has `local' IO. 
                        let cg' = M.insert caller callees cg

                        -- recursively build the callees
                        let fs_soacs = fs `union` soacs
                        foldM buildCGfun cg' fs_soacs
                                            
      else  badCGM $ TypeError pos  (" in buildCGfun lookup for fundec of " ++ 
                                     nameToString fname ++ " resulted in " ++
                                     nameToString caller)
  where
    
             
    
buildCGexp :: ([Name],[Name]) -> Exp -> ([Name],[Name])

buildCGexp callees@(fs, soacfs) (Apply fname args _ _)  =
    if isBuiltInFun fname || elem fname fs
    then foldl buildCGexp callees args
    else foldl buildCGexp (fname:fs, soacfs) args   


buildCGexp callees e = 
    foldlPattern buildCGexp addLamFun callees e

-- Promoted to AbSyn
--isBuiltInFun :: String -> Bool
--isBuiltInFun fn = elem fn ["toReal", "trunc", "sqrt", "log", "exp"]

addLamFun :: ([Name],[Name]) -> Lambda -> ([Name],[Name])
addLamFun callees (AnonymFun {}) = callees
addLamFun (fs,soacs) (CurryFun nm _ _ _) =
    if isBuiltInFun nm || elem nm fs
    then (fs,soacs) else (fs,nm:soacs)

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
-- | @aggInlineDriver prog@ performs aggressive inlining for all functions
-- in @prog@ by repeatedly inlining the functions with empty-apply-callee set into
-- other callers.   The functions called (indirectly) via SOACs are disregarded.
aggInlineDriver :: Prog -> Either EnablingOptError Prog
aggInlineDriver prog = do
    env <- foldM expand env0 $ progFunctions prog
    cg  <- runCGM (buildCGfun M.empty defaultEntryPoint) env
    runCGM (aggInlining cg) env
    where
        env0 = CGEnv { envFtable = M.empty }
        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise = 
                Right CGEnv { envFtable = M.insert name f (envFtable ftab) }


-- | Bind a name as a common (non-merge) variable.
bindVarFtab :: CGEnv -> (Name, FunDec) -> CGEnv
bindVarFtab env (name,val) =
  env { envFtable = M.insert name val $ envFtable env }

bindVarsFtab :: CGEnv -> [(Name, FunDec)] -> CGEnv
bindVarsFtab = foldl bindVarFtab

bindingFtab :: [(Name, FunDec)] -> CGM a -> CGM a
bindingFtab bnds = local (`bindVarsFtab` bnds)

-- | Remove the binding for a name.
remVarFtab :: CGEnv -> Name -> CGEnv
remVarFtab env name = env { envFtable = M.delete name $ envFtable env }

remVarsFtab :: CGEnv -> [Name] -> CGEnv
remVarsFtab = foldl remVarFtab

remBindingsFtab :: [Name] -> CGM a -> CGM a
remBindingsFtab keys = local (`remVarsFtab` keys)



aggInlining :: CallGraph -> CGM Prog
aggInlining cg = do
    let inlcand    = M.keys (M.filter funHasNoCalls cg)
    let inlinf     = getInlineOps inlcand cg
--    let inlinf     = (  M.filter (\l -> not (null l)) . 
--                        M.map (\callees -> intersect inlcand callees)
--                     ) cg
    let work       = M.toList inlinf

    if null work 
         -- a fix point has been reached, hence gather the program
         -- from the hashtable and return it.
    then Prog <$> asks (M.elems . envFtable)

         -- Remove the to-be-inlined functions from the Call Graph,
         -- and then, for each caller that exhibits to-be-inlined callees
         -- perform the inlining. Finally, update the `ftable' and
         -- iterate to a fix point. At this point it is guaranteed
         -- that `work' is not empty, hence there are functions to
         -- be inlined in each caller function (from `work').
    else do let cg'  = M.map ( \(callees,l) -> ( (\\) callees inlcand, l ) ) cg
            let work'= map ( \(x,(y,_)) -> (x,y) ) work
            newfuns  <- mapM processfun work'
            let newfunnms  = fst (unzip work')
            remBindingsFtab  newfunnms $ 
                bindingFtab (zip newfunnms newfuns) $  
                aggInlining cg'
    where
        processfun :: (Name, [Name]) -> CGM FunDec
        processfun (fname, inlcallees) = do
            f  <- asks $ M.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> do
                    tobeinl <- mapM getFromFtable inlcallees  
                    return $ doInlineInCaller ff tobeinl 

        getFromFtable :: Name -> CGM FunDec
        getFromFtable fname = do 
            f <- asks $ M.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> return ff
            
        getInlineOps :: [Name] -> CallGraph -> CallGraph
        getInlineOps inlcand =
            M.filter (\(callees,_) -> not (null callees)) .
            M.map (first $ intersect inlcand)

        funHasNoCalls :: ([Name],[Name]) -> Bool
        funHasNoCalls (callees,_) = null callees 


-- | @doInlineInCaller caller inlcallees@ inlines in @calleer@ the functions
-- in @inlcallees@. At this point the preconditions are that if @inlcallees@ 
-- is not empty, and, more importantly, the functions in @inlcallees@ do 
-- not call any other functions. Further extensions that transform a
-- tail-recursive function to a do or while loop, should do the transformation
-- first and then do the inlining.
doInlineInCaller :: FunDec ->  [FunDec] -> FunDec
doInlineInCaller (name,rtp,args,body,pos) inlcallees = 
    let body' = inlineInExp inlcallees body 
    in (name, rtp, args, body',pos)

inlineInExp :: [FunDec] -> Exp -> Exp
inlineInExp inlcallees (Apply fname aargs rtp pos) = 
    let aargs' = map (inlineInExp inlcallees) aargs
    in  case filter (\(nm,_,_,_,_)->fname==nm) inlcallees of
            [] -> Apply fname aargs' rtp pos
            (_,_,fargs,body,_):_ ->
              let revbnds = reverse (zip fargs aargs)
              in  foldl (addArgBnd pos) body revbnds
    where
        addArgBnd :: SrcLoc -> Exp -> (Ident, Exp) -> Exp
        addArgBnd ppos body (farg, aarg) = 
            let fargutp = boxType (identType farg)
                fargnm  = identName   farg
                fargpos = identSrcLoc farg
                farg' = Ident fargnm fargutp fargpos
            in  LetPat (Id farg') aarg body ppos


inlineInExp inlcallees e = 
    buildExpPattern (inlineInExp inlcallees) e


 
------------------------------------------------------------------
------------------  Dead Function Elimination --------------------
------------------------------------------------------------------
-- | @deadFunElim prog@ removes the functions that are unreachable from
-- the main function from the program.
-- The functions called (indirectly) via SOACs are obviously considered.
deadFunElim :: Prog -> Either EnablingOptError Prog
deadFunElim prog = do
    env  <- foldM expand env0 $ progFunctions prog
    cg   <- runCGM (buildCGfun M.empty defaultEntryPoint) env
    let env'  = (M.filter (isFunInCallGraph cg) . envFtable) env
    let prog' = M.elems env'
    return $ Prog prog'
    where
        env0 = CGEnv { envFtable = M.empty }

        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise = 
                Right CGEnv { envFtable = M.insert name f (envFtable ftab) }

        isFunInCallGraph :: CallGraph -> FunDec -> Bool
        isFunInCallGraph cg (fnm,_,_,_,_) = 
            case M.lookup fnm cg of
                Nothing -> False
                Just  _ -> True
