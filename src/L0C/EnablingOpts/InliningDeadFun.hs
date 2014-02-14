{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

import Data.List
import Data.Loc

import qualified Data.HashMap.Lazy as HM

import L0C.InternalRep

import L0C.EnablingOpts.EnablingOptErrors


------------------------------------------------------------------------------
------------------------------------------------------------------------------
----  Simple Transformation to replace soac's curried with unnamed lambda
------------------------------------------------------------------------------
------------------------------------------------------------------------------

mkUnnamedLamPrg :: Prog -> Prog
mkUnnamedLamPrg prog =
    let fnms = map (\(nm,_,_,_,_)->nm) $ progFunctions prog
        ftab = HM.fromList $ zip fnms $ progFunctions prog
    in  Prog $ map (mkUnnamedLamFun ftab) $ progFunctions prog

mkUnnamedLamFun :: HM.HashMap Name FunDec -> FunDec -> FunDec
mkUnnamedLamFun ftab (fnm,rtp,idds,body,pos) =
    let body' = mkUnnamedLamExp ftab body
    in  (fnm,rtp,idds,body',pos)

mkUnnamedLamExp :: HM.HashMap Name FunDec -> Exp -> Exp
mkUnnamedLamExp ftab (Map cs lam arrs pos) =
    Map    cs (mkUnnamedTupleLamLam ftab lam) arrs pos
mkUnnamedLamExp ftab (Filter cs lam arrs pos) =
    Filter cs (mkUnnamedTupleLamLam ftab lam) arrs pos
mkUnnamedLamExp ftab (Reduce cs lam inputs pos) =
    Reduce cs (mkUnnamedTupleLamLam ftab lam) inputs pos
mkUnnamedLamExp ftab (Scan cs lam inputs pos) =
    Scan cs (mkUnnamedTupleLamLam ftab lam) inputs pos
mkUnnamedLamExp ftab (Redomap cs lam1 lam2 nes arrs pos) =
    Redomap cs (mkUnnamedTupleLamLam ftab lam1) (mkUnnamedTupleLamLam ftab lam2)
               nes arrs pos

mkUnnamedLamExp ftab e = buildExpPattern (mkUnnamedLamExp ftab) e

mkUnnamedTupleLamLam :: HM.HashMap Name FunDec -> Lambda -> Lambda
mkUnnamedTupleLamLam ftab (Lambda ids body tp loc) =
  Lambda ids (mkUnnamedLamExp ftab body) tp loc

------------------------------------------------------------------------------
------------------------------------------------------------------------------
----  Call Graph And Inlining
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- | The Call Graph is just a Map from a function name, i.e., the caller,
-- to a three-element tuple: The first element is a list that contains the
-- (unique) function names that may be called directly from the current function,
-- i.e., ``apply'' callees. The second element is a list that contains the (unique)
-- function names that may be called via SOACs.
type CallGraph = HM.HashMap Name ([Name],[Name])

-- | The symbol table for functions
data CGEnv = CGEnv { envFtable  :: HM.HashMap Name FunDec }

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
    runCGM (buildCGfun HM.empty defaultEntryPoint) env

    where
        env0 = CGEnv { envFtable = HM.empty }
        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- HM.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise =
                Right CGEnv { envFtable = HM.insert name f (envFtable ftab) }


-- | @buildCG cg fname@ updates Call Graph @cg@ with the contributions of function
-- @fname@, and recursively, with the contributions of the callees of @fname@.
-- In particular, @buildCGfun HM.empty defaultEntryPoint@ should construct the Call Graph
-- of the whole program.
buildCGfun :: CallGraph -> Name -> CGM CallGraph
buildCGfun cg fname  = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> badCGM $ FunctionNotInFtab fname
    Just (caller,_,_,body,pos) ->
      if caller == fname
      then
        case HM.lookup caller cg of
          Just _  -> return cg
          Nothing -> do let callees@(fs, soacs) = buildCGexp ([],[]) body

                        let cg' = HM.insert caller callees cg

                        -- recursively build the callees
                        let fs_soacs = fs `union` soacs
                        foldM buildCGfun cg' fs_soacs

      else  badCGM $ TypeError pos  (" in buildCGfun lookup for fundec of " ++
                                     nameToString fname ++ " resulted in " ++
                                     nameToString caller)
  where



buildCGexp :: ([Name],[Name]) -> Exp -> ([Name],[Name])

buildCGexp callees@(fs, soacfs) (Apply fname _ _ _)  =
    if isBuiltInFun fname || elem fname fs
    then callees
    else (fname:fs, soacfs)

buildCGexp callees e =
    foldlPattern buildCGexp addLamFun callees e

addLamFun :: ([Name],[Name]) -> Lambda -> ([Name],[Name])
addLamFun callees _ = callees

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
    cg  <- runCGM (buildCGfun HM.empty defaultEntryPoint) env
    runCGM (aggInlining cg) env
    where
        env0 = CGEnv { envFtable = HM.empty }
        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- HM.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise =
                Right CGEnv { envFtable = HM.insert name f (envFtable ftab) }


-- | Bind a name as a common (non-merge) variable.
bindVarFtab :: CGEnv -> (Name, FunDec) -> CGEnv
bindVarFtab env (name,val) =
  env { envFtable = HM.insert name val $ envFtable env }

bindVarsFtab :: CGEnv -> [(Name, FunDec)] -> CGEnv
bindVarsFtab = foldl bindVarFtab

bindingFtab :: [(Name, FunDec)] -> CGM a -> CGM a
bindingFtab bnds = local (`bindVarsFtab` bnds)

-- | Remove the binding for a name.
remVarFtab :: CGEnv -> Name -> CGEnv
remVarFtab env name = env { envFtable = HM.delete name $ envFtable env }

remVarsFtab :: CGEnv -> [Name] -> CGEnv
remVarsFtab = foldl remVarFtab

remBindingsFtab :: [Name] -> CGM a -> CGM a
remBindingsFtab keys = local (`remVarsFtab` keys)



aggInlining :: CallGraph -> CGM Prog
aggInlining cg = do
    let inlcand    = HM.keys (HM.filter funHasNoCalls cg)
    let inlinf     = getInlineOps inlcand cg
    let work       = HM.toList inlinf

    if null work
         -- a fix point has been reached, hence gather the program
         -- from the hashtable and return it.
    then Prog <$> asks (HM.elems . envFtable)

         -- Remove the to-be-inlined functions from the Call Graph,
         -- and then, for each caller that exhibits to-be-inlined callees
         -- perform the inlining. Finally, update the `ftable' and
         -- iterate to a fix point. At this point it is guaranteed
         -- that `work' is not empty, hence there are functions to
         -- be inlined in each caller function (from `work').
    else do let cg'  = HM.map ( \(callees,l) -> ( (\\) callees inlcand, l ) ) cg
            let work'= map ( \(x,(y,_)) -> (x,y) ) work
            newfuns  <- mapM processfun work'
            let newfunnms  = fst (unzip work')
            remBindingsFtab  newfunnms $
                bindingFtab (zip newfunnms newfuns) $
                aggInlining cg'
    where
        processfun :: (Name, [Name]) -> CGM FunDec
        processfun (fname, inlcallees) = do
            f  <- asks $ HM.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> do
                    tobeinl <- mapM getFromFtable inlcallees
                    return $ doInlineInCaller ff tobeinl

        getFromFtable :: Name -> CGM FunDec
        getFromFtable fname = do
            f <- asks $ HM.lookup fname . envFtable
            case f of
                Nothing -> badCGM $ FunctionNotInFtab fname
                Just ff -> return ff

        getInlineOps :: [Name] -> CallGraph -> CallGraph
        getInlineOps inlcand =
            HM.filter (\(callees,_) -> not (null callees)) .
            HM.map (first $ intersect inlcand)

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
inlineInExp inlcallees (Apply fname args rtp pos) =
    let aargs = map fst args
    in  case filter (\(nm,_,_,_,_)->fname==nm) inlcallees of
            [] -> Apply fname args rtp pos
            (_,_,fargs,body,_):_ ->
              let revbnds = reverse (zip (map fromParam fargs) aargs)
              in  foldl (addArgBnd pos) body revbnds
    where
        addArgBnd :: SrcLoc -> Exp -> (Ident, SubExp) -> Exp
        addArgBnd ppos body (farg, aarg) =
            let fargutp = identType farg
                fargnm  = identName   farg
                fargpos = identSrcLoc farg
                farg' = Ident fargnm fargutp fargpos
            in  LetPat [farg'] (SubExp aarg) body ppos


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
    cg   <- runCGM (buildCGfun HM.empty defaultEntryPoint) env
    let env'  = (HM.filter (isFunInCallGraph cg) . envFtable) env
    let prog' = HM.elems env'
    return $ Prog prog'
    where
        env0 = CGEnv { envFtable = HM.empty }

        expand :: CGEnv -> FunDec -> Either EnablingOptError CGEnv
        expand ftab f@(name,_,_,_,pos)
            | Just (_,_,_,_,pos2) <- HM.lookup name (envFtable ftab) =
              Left $ DupDefinitionError name pos pos2
            | otherwise =
                Right CGEnv { envFtable = HM.insert name f (envFtable ftab) }

        isFunInCallGraph :: CallGraph -> FunDec -> Bool
        isFunInCallGraph cg (fnm,_,_,_,_) =
            case HM.lookup fnm cg of
                Nothing -> False
                Just  _ -> True
