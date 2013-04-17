{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.InliningDeadFun  ( 
                        CallGraph
                      , buildCG
                      , aggInlineDriver
                      , deadFunElim
                    )
  where 
  
import Control.Applicative
import Control.Monad.Reader
 
--import Data.Either

--import Control.Monad.State
import Data.List
import Data.Loc

import qualified Data.Map as M

import L0.AbSyn

import L0.EnablingOpts.GenPatterns
import L0.EnablingOpts.EnablingOptErrors


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
type CallGraph = M.Map String ([String],[String],Bool)

-- | The symbol table for functions
data CGEnv tf = CGEnv {   
                    envFtable  :: M.Map String (FunDec tf)
               }

newtype CGM tf a = CGM (ReaderT (CGEnv tf) (Either EnablingOptError) a)
    deriving (MonadReader (CGEnv tf), Monad, Applicative, Functor)

-- | Building the call grah runs in this monad.  There is no
-- mutable state.
runCGM :: CGM tf a -> CGEnv tf -> Either EnablingOptError a
runCGM  (CGM a) env = runReaderT a env

badCGM :: EnablingOptError -> CGM tf a
badCGM = CGM . lift . Left


-- | @buildCG prog@ build the program's Call Graph. The representation
-- is a hashtable that maps function names to a list of callee names.
buildCG :: TypeBox tf => Prog tf -> Either EnablingOptError CallGraph
buildCG prog = do
    env <- foldM expand env0 prog
    runCGM (buildCGfun M.empty "main") env

    where
        env0 = CGEnv { envFtable = M.empty }
        expand :: TypeBox tf => CGEnv tf -> FunDec tf -> Either EnablingOptError (CGEnv tf)
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise = 
                Right $ CGEnv { envFtable = M.insert name f (envFtable ftab) }
                

-- | @buildCG cg fname@ updates Call Graph @cg@ with the contributions of function 
-- @fname@, and recursively, with the contributions of the callees of @fname@.
-- In particular, @buildCGfun M.empty "main"@ should construct the Call Graph
-- of the whole program.
buildCGfun :: TypeBox tf => CallGraph -> String -> CGM tf CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> badCGM $ FunctionNotInFtab fname
    Just (caller,_,_,body,pos) -> 
      if(caller == fname) 
      then
        case M.lookup caller cg of
          Just _  -> return cg
          Nothing -> do let callees@(fs, soacs, has_io_loc) = buildCGexp ([],[], False) body

                        -- at this point caller is only known whether it has `local' IO. 
                        let cg' = M.insert caller callees cg

                        -- recursively build the callees
                        let fs_soacs = union fs soacs
                        cg'' <- foldM buildCGfun cg' fs_soacs

                        -- if any of the callees have IO then the caller has IO
                        callees_io  <-  mapM (\ fnm -> 
                                                let res = M.lookup fnm cg''
                                                in  case res of 
                                                      Just (_,_,fnm_has_io) -> return fnm_has_io
                                                      Nothing -> badCGM $ TypeError pos  (" in buildCGfun lookup for function " ++ 
                                                                                          fnm ++ " not in call graph! Caller: " ++ caller)
                                             ) fs_soacs
                        let has_io_glob = foldl (||) False callees_io
                        if( has_io_glob == has_io_loc ) 
                        then return cg''
                        else do return $ M.update (\ (x,y,_)->Just (x,y,has_io_glob) ) caller cg'' 
                                            
      else  badCGM $ TypeError pos  (" in buildCGfun lookup for fundec of " ++ 
                                     fname ++ " resulted in " ++ caller)
  where
    
             
    
buildCGexp :: TypeBox tf => ([String],[String],Bool) -> Exp tf -> ([String],[String],Bool) 

buildCGexp callees@(fs, soacfs, has_io) (Apply fname args _ _)  =
    if isBuiltInFun fname || elem fname fs
    then foldl buildCGexp callees args
    else foldl buildCGexp (fname:fs, soacfs, has_io) args   

buildCGexp(fs, soacfs, _) (Read  _ _  ) = (fs, soacfs, True)
buildCGexp callees        (Write e _ _) = 
    let (fs', soacfs', _) = buildCGexp callees e
    in  (fs', soacfs', True)


buildCGexp callees e = 
    foldlPattern buildCGexp addLamFun callees e

-- Promoted to AbSyn
--isBuiltInFun :: String -> Bool
--isBuiltInFun fn = elem fn ["toReal", "trunc", "sqrt", "log", "exp"]

addLamFun :: TypeBox tf => ([String],[String],Bool) -> Lambda tf -> ([String],[String],Bool)
addLamFun callees           (AnonymFun _ _ _ _) = callees
addLamFun (fs,soacs,has_io) (CurryFun nm _ _ _) =
    if isBuiltInFun nm || elem nm fs
    then (fs,soacs,has_io) else (fs,nm:soacs,has_io)

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
-- | @aggInlineDriver prog@ performs aggressive inlining for all functions
-- in @prog@ by repeatedly inlining the functions with empty-apply-callee set into
-- other callers.   The functions called (indirectly) via SOACs are disregarded.
aggInlineDriver :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
aggInlineDriver prog = do
    env <- foldM expand env0 prog
    cg  <- runCGM (buildCGfun M.empty "main") env
    runCGM (aggInlining cg) env
    where
        env0 = CGEnv { envFtable = M.empty }
        expand :: TypeBox tf => CGEnv tf -> FunDec tf -> Either EnablingOptError (CGEnv tf)
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise = 
                Right $ CGEnv { envFtable = M.insert name f (envFtable ftab) }


-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVarFtab :: CGEnv tf -> (String, FunDec tf) -> CGEnv tf
bindVarFtab env (name,val) =
  env { envFtable = M.insert name val $ envFtable env }

bindVarsFtab :: CGEnv tf -> [(String, FunDec tf)] -> CGEnv tf
bindVarsFtab = foldl bindVarFtab

bindingFtab :: [(String, FunDec tf)] -> CGM tf a -> CGM tf a
bindingFtab bnds = local (`bindVarsFtab` bnds)

-- | Remove the binding for a name.
-- TypeBox tf => 
remVarFtab :: CGEnv tf -> String -> CGEnv tf
remVarFtab env name = env { envFtable = M.delete name $ envFtable env }

remVarsFtab :: CGEnv tf -> [String] -> CGEnv tf
remVarsFtab = foldl remVarFtab

remBindingsFtab :: [String] -> CGM tf a -> CGM tf a
remBindingsFtab keys = local (`remVarsFtab` keys)



aggInlining :: TypeBox tf => CallGraph -> CGM tf (Prog tf)
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
    then do funs <- asks $ M.elems . envFtable
            return funs

         -- Remove the to-be-inlined functions from the Call Graph,
         -- and then, for each caller that exhibits to-be-inlined callees
         -- perform the inlining. Finally, update the `ftable' and
         -- iterate to a fix point. At this point it is guaranteed
         -- that `work' is not empty, hence there are functions to
         -- be inlined in each caller function (from `work').
    else do let cg'  = M.map ( \(callees,l,has_io) -> ( (\\) callees inlcand, l,has_io ) ) cg
            let work'= map ( \(x,(y,_,_)) -> (x,y) ) work
            newfuns  <- mapM processfun work'
            let newfunnms  = fst (unzip work')
            remBindingsFtab  newfunnms $ 
                bindingFtab (zip newfunnms newfuns) $  
                aggInlining cg'
    where
        processfun :: TypeBox tf => (String, [String]) -> CGM tf (FunDec tf)
        processfun (fname, inlcallees) = do
            f  <- asks $ M.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> do
                    tobeinl <- mapM getFromFtable inlcallees  
                    return $ doInlineInCaller ff tobeinl 

        getFromFtable :: TypeBox tf => String -> CGM tf (FunDec tf)
        getFromFtable fname = do 
            f <- asks $ M.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> return ff
            
        getInlineOps :: [String] -> CallGraph -> CallGraph
        getInlineOps inlcand callgr = 
            (  M.filter (\(callees,_,_) -> not (null callees))   . 
               M.map ( \(callees,l,hio) -> (intersect inlcand callees,l,hio) )
            )  callgr

        funHasNoCalls :: ([String],[String],Bool) -> Bool
        funHasNoCalls (callees,_,_) = null callees 


-- | @doInlineInCaller caller inlcallees@ inlines in @calleer@ the functions
-- in @inlcallees@. At this point the preconditions are that if @inlcallees@ 
-- is not empty, and, more importantly, the functions in @inlcallees@ do 
-- not call any other functions. Further extensions that transform a
-- tail-recursive function to a do or while loop, should do the transformation
-- first and then do the inlining.
doInlineInCaller :: TypeBox tf => FunDec tf ->  [FunDec tf] -> FunDec tf
doInlineInCaller (name,rtp,args,body,pos) inlcallees = 
    let body' = inlineInExp inlcallees body 
    in (name, rtp, args, body',pos)

inlineInExp :: TypeBox tf => [FunDec tf] -> Exp tf -> Exp tf
inlineInExp inlcallees (Apply fname aargs rtp pos) = 
    let aargs' = map (inlineInExp inlcallees) aargs
    in  case filter (\(nm,_,_,_,_)->fname==nm) inlcallees of
            [] -> Apply fname aargs' rtp pos
            cs -> let (_,_,fargs,body,_) = head cs
                      revbnds = reverse (zip fargs aargs)
                  in  foldl (addArgBnd pos) body revbnds
    where
        addArgBnd :: TypeBox tf => SrcLoc -> Exp tf -> (Ident Type, Exp tf) -> Exp tf
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
-- | @aggInlineDriver prog@ performs aggressive inlining for all functions
-- in @prog@ by repeatedly inlining the functions with empty-apply-callee set into
-- other callers.   The functions called (indirectly) via SOACs are disregarded.
deadFunElim :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
deadFunElim prog = do
    env  <- foldM expand env0 prog
    cg   <- runCGM (buildCGfun M.empty "main") env
    let env'  = (M.filter (isFunInCallGraph cg) . envFtable) env
    let prog' = M.elems env'
    return prog'
    where
        env0 = CGEnv { envFtable = M.empty }

        expand :: TypeBox tf => CGEnv tf -> FunDec tf -> Either EnablingOptError (CGEnv tf)
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- M.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise = 
                Right $ CGEnv { envFtable = M.insert name f (envFtable ftab) }

        isFunInCallGraph :: TypeBox tf => CallGraph -> FunDec tf -> Bool
        isFunInCallGraph cg (fnm,_,_,_,_) = 
            case M.lookup fnm cg of
                Nothing -> False
                Just  _ -> True
