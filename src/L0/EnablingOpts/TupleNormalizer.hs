{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.TupleNormalizer ( tupleNormProg )
  where
 
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
--import Control.Monad.Writer


--import Data.Data
--import Data.Generics

--import qualified Data.Set as S
import qualified Data.Map as M

import L0.AbSyn
import Data.Loc
 
import L0.FreshNames

--import L0.EnablingOpts.GenPatterns
import L0.EnablingOpts.EnablingOptErrors


-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Program Normalization:             ----
----    1. x : (tp1 * (tp2*tp3)) replaced with (x1,(x2,x3))  ----
----    2. array and tuple literal normalization, i.e.,      ----
----          literals contains a series of variables only   ----
----    3. let y = (let x = exp1 in exp2) in body ->         ----
----       let x = exp1 in let y = exp2 in body              ----
----    4. function calls separated from expressions,i.e.,   ----
----             let y = f(x) + g(z) + exp in body           ----
----         is replaced by:                                 ----
----             let t1 = f(x) in let t2 = g(z) in           ----
----             let y = t1 + t2 + exp in body               ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data NormEnv tf = NormEnv {   
                        envVtable  :: M.Map String (TupIdent tf)
                  }


newtype NormM tf a = NormM (StateT NameSource (ReaderT (NormEnv tf) (Either EnablingOptError)) a)
    deriving (  MonadState NameSource, 
                MonadReader (NormEnv tf),
                Monad, Applicative, Functor )


-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: NormEnv tf -> (String, TupIdent tf) -> NormEnv tf
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: NormEnv tf -> [(String, TupIdent tf)] -> NormEnv tf
bindVars = foldl bindVar

binding :: [(String, TupIdent tf)] -> NormM tf a -> NormM tf a
binding bnds = local (`bindVars` bnds)


-- | The program normalizer runs in this monad.  The mutable
-- state refers to the fresh-names engine. The reader hides
-- the vtable that associates variable names with/to-be-substituted-for tuples pattern.
-- The 'Either' monad is used for error handling.
runNormM :: TypeBox tf => Prog tf -> NormM tf a -> NormEnv tf -> Either EnablingOptError a
runNormM prog (NormM a) env = 
    runReaderT (evalStateT a (newNameSourceForProg prog)) env

badNormM :: EnablingOptError -> NormM tf a
badNormM = NormM . lift . lift . Left

-- | Return a fresh, unique name.  The @String@ is prepended to the
-- name.
new :: TypeBox tf => String -> NormM tf String
new = state . newName


-----------------------------------------------------------------
--- Tuple Normalizer Entry Point: normalizes tuples@pgm level ---
-----------------------------------------------------------------

tupleNormProg :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
tupleNormProg prog = do
    let env = NormEnv { envVtable = M.empty }
    prog' <- runNormM prog (mapM tupleNormFun prog) env
    return prog'


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing a function: for every tuple param, e.g.,    ----
----            (int*(real*int)) x                           ----
----     pattern match it with a tuple at the beginning      ----
----            let (x1,(x2,x3)) = x in body'                ----
----     where body' is the normalized-body of the function  ---- 
-----------------------------------------------------------------
-----------------------------------------------------------------

tupleNormFun :: TypeBox tf => FunDec tf -> NormM tf (FunDec tf)
tupleNormFun (fname, rettype, args, body, pos) = do
    let tups    = filter isTuple args
    let nms     = map identName tups
    (ok, bnds) <- procTups tups body []

    if ok
    then do -- required pattern match already done
            -- add the vtable bindings and normalize body
            body' <- binding (zip nms bnds) $ tupleNormExp body 
            return (fname, rettype, args, body', pos)
    else do -- add the vtable bindings, normalize body
            tups' <- mapM makeTupId tups
            let bnds' = zip nms tups'
            body' <- binding bnds' $ tupleNormExp body
            -- Finally, add the required pattern matching 
            -- on top of the normalized body
            let pats = zip tups tups' 
            let body'' = addPatterns pos pats body'
            return (fname, rettype, args, body'', pos)

    where
        addPatterns :: TypeBox ty => SrcLoc -> [(Ident Type,TupIdent ty)] -> Exp ty -> Exp ty
        addPatterns _  []         bdy = bdy
        addPatterns p1 (pat:pats) bdy = 
            let (idd, tupid) = pat
                idd' = Ident{ identName = identName idd, 
                              identType = boxType (identType idd), 
                              identSrcLoc = identSrcLoc idd
                            }
            in  LetPat tupid (Var idd') (addPatterns p1 pats bdy) p1


        makeTupId :: TypeBox ty => Ident Type -> NormM ty (TupIdent ty)
        makeTupId ident = do
            case tp of
                Tuple tps _ -> do
                    tupids <- mapM makeTupIdFromType tps
                    let res = TupId tupids pp
                    return res
                _ ->badNormM $ TypeError pp 
                                 ( "Id: " ++ nm ++ 
                                   " not a tuple: " ++ ppType tp )
             
            where
                (nm, tp, pp) = (identName ident, identType ident, identSrcLoc ident)

                makeTupIdFromType :: TypeBox ty => Type -> NormM ty (TupIdent ty)
                makeTupIdFromType ttpp = 
                    case ttpp of
                        Tuple tps _ ->  do
                            tups <- mapM makeTupIdFromType tps
                            return $ TupId tups pos
                        _            ->  do
                            new_nm <- new nm 
                            return $ Id (Ident { identName = new_nm, identType = boxType ttpp, identSrcLoc = pp })
                

        isTuple :: Ident Type -> Bool
        isTuple x = case identType x of 
                        Tuple _ _ -> True
                        _         -> False

        completeTup :: TypeBox ty => (Type, TupIdent ty) -> NormM ty Bool
        completeTup (tp, tup) = do  -- whether is a complete tuple pattern.
            case tup of
                Id _ -> 
                    case tp of
                        Tuple _ _ -> return False -- id abstracts a tuple
                        _         -> return True  -- ok, id is not a tuple

                TupId tupids _ -> 
                    case tp of
                        Tuple tps _ -> 
                            if (length tupids) == (length tps)
                            then do bools <- mapM completeTup (zip tps tupids)
                                    return $ foldl (&&) True bools                                  
                            else badNormM $ TypeError pos ("TupIdent: " ++ ppTupId tup ++
                                                           " length doesn't match type: " ++ ppType tp)
                        _     -> badNormM $ TypeError pos ("TupIdent: " ++ ppTupId tup ++
                                                           " does not match type: "++ppType tp)
 
        procTups :: TypeBox ty => [Ident Type] -> Exp ty -> [TupIdent ty] -> NormM ty (Bool,[TupIdent ty])
        procTups []     _   lst = return (True, reverse lst) 
        procTups (i:is) bdy lst = do
            case bdy of
                LetPat tupids (Var idd) bdy' _ -> do
                    is_complete <- completeTup ((identType i), tupids)
                    if  identName idd == identName i && is_complete
                    then procTups is bdy' (tupids:lst)
                    else return (False, lst)
                _ ->     return (False, lst)


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing an expression                               ----
-----------------------------------------------------------------
-----------------------------------------------------------------

tupleNormExp :: TypeBox tf => Exp tf -> NormM tf (Exp tf)
tupleNormExp e = do
    return e
