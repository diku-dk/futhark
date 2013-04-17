{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.LetNormalizer ( letNormProg )
  where
 
import Control.Monad.State
import Control.Applicative
import Control.Monad.Writer


--import Data.Data
--import Data.Generics

import qualified Data.List as L

import L0.AbSyn
import Data.Loc
 
import L0.FreshNames

--import L0.EnablingOpts.GenPatterns
import L0.EnablingOpts.EnablingOptErrors

--import Debug.Trace

-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Program Normalization:             ----
----    1. array and tuple literal normalization, i.e.,      ----
----          literals contains a series of variables only   ----
----    2. let y = (let x = exp1 in exp2) in body ->         ----
----       let x = exp1 in let y = exp2 in body              ----
----    3. function calls separated from expressions,i.e.,   ----
----             let y = f(x) + g(z) + exp in body           ----
----         is replaced by:                                 ----
----             let t1 = f(x) in let t2 = g(z) in           ----
----             let y = t1 + t2 + exp in body               ----
----    4. Same for array constructors and combinators       ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data LetNormRes tf = LetNormRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resMap     :: [(String, Exp tf)]
  -- ^ The hashtable recording the uses
  }

instance Monoid (LetNormRes tf) where
  LetNormRes s1 m1 `mappend` LetNormRes s2 m2 =
    LetNormRes (s1 || s2) (m1 ++ m2) 
  mempty = LetNormRes False []


newtype LetNormM tf a = LetNormM (StateT NameSource (WriterT (LetNormRes tf) (Either EnablingOptError)) a)
    deriving (  MonadState NameSource, 
                MonadWriter (LetNormRes tf),
                Monad, Applicative, Functor )


-----------------------------
--- Collecting the result ---
-----------------------------

--collectRes :: Exp tf -> LetNormM tf a -> LetNormM tf (a, [(String, Exp tf)])
collectRes :: LetNormM tf (Exp tf) -> LetNormM tf (Exp tf, [(String, Exp tf)])
collectRes m = pass collect
  where 
    collect = do
      (x,res) <- listen m
      let (suc,res_map) = (resSuccess res, resMap res)

      -- trim out the result
      let (x', res_map') = case x of
            LetPat pat (Var idd) body pos -> 
                let nm = identName idd
                in  case L.lookup nm res_map of
                      Just mexp -> (LetPat pat mexp body pos, L.deleteBy (\ (x1,_) (x2,_) -> x1==x2) (nm,mexp) res_map)
                      Nothing   -> (x, res_map)
            _ -> (x, res_map)

      return ( (x', res_map'), const $ LetNormRes{ resSuccess = suc, resMap = [] } )
{-
changed :: a -> LetNormM tf a
changed x = do
  tell $ LetNormRes True []
  return x
-}
-----------------------------
-----------------------------


-- | The program normalizer runs in this monad.  The mutable
-- state refers to the fresh-names engine. The reader hides
-- the vtable that associates variable names with/to-be-substituted-for tuples pattern.
-- The 'Either' monad is used for error handling.
runLetNormM :: TypeBox tf => Prog tf -> LetNormM tf a -> Either EnablingOptError (a, LetNormRes tf)
runLetNormM prog (LetNormM a) = 
    runWriterT (evalStateT a (newNameSourceForProg prog))

{-
badLetNormM :: EnablingOptError -> LetNormM tf a
badLetNormM = LetNormM . lift . lift . Left
-}

-- | Return a fresh, unique name.  The @String@ is prepended to the
-- name.
new :: TypeBox tf => String -> LetNormM tf String
new = state . newName


letNormProg :: TypeBox tf => Prog tf -> Either EnablingOptError (Bool, Prog tf)
letNormProg prog = do
    (prog', res) <- runLetNormM prog (mapM letNormFun prog)
    return (resSuccess res, prog')


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing a function: for every tuple param, e.g.,    ----
----            (int*(real*int)) x                           ----
----     pattern match it with a tuple at the beginning      ----
----            let (x1,(x2,x3)) = x in body'                ----
----     where body' is the normalized-body of the function  ---- 
-----------------------------------------------------------------
-----------------------------------------------------------------

letNormFun :: TypeBox tf => FunDec tf -> LetNormM tf (FunDec tf)
letNormFun (fname, rettype, args, body, pos) = do
    (body', newbnds) <- collectRes $ letNormExp body
    let body'' = addPatterns pos newbnds body'
    return (fname, rettype, args, body'', pos)


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing an expression                               ----
-----------------------------------------------------------------
-----------------------------------------------------------------

letNormExp :: TypeBox tf => Exp tf -> LetNormM tf (Exp tf)

-----------------------------
---- LetPat/With/Do-Loop ----
-----------------------------

letNormExp (LetPat pat e body pos) = do
    (body', bodyres') <- collectRes $ letNormExp body
    let body'' = makeLetExp pos bodyres' body'

    (e', eres')    <- collectRes $ letNormExp e 
    let res = combinePats (ReguPat pat pos) e' body'' 
    return $ makeLetExp pos eres' res


letNormExp (LetWith nm src inds el body pos) = do
    (body', bodyres') <- collectRes $ letNormExp body
    let body'' = makeLetExp pos bodyres' body'

    (el', eres')    <- collectRes $ letNormExp el 
    let res = combinePats (WithPat nm src inds pos) el' body'' 
    return $ makeLetExp pos eres' res


--letNormLambda (AnonymFun params body ret pos) = do
--    (body', newbnds) <- collectRes $ letNormExp body
--    let body'' = addPatterns pos newbnds body'
--    return $ AnonymFun params body'' ret pos

--letNormExp (DoLoop ind n body mergevars pos) = do
letNormExp (DoLoop idexps idd n loopbdy letbdy pos) = do
    -- the potential bindings from the loop-count 
    -- expression are handled in the outer-loop scope
    n'  <-  subLetoNormExp "tmp_ub" n

    let (ids, exps) = unzip idexps
    exps' <- mapM (subLetoNormExp "tmp_ini") exps
    
    -- a do-loop creates a scope, hence we need to treat the
    -- the binding at this level, similar to a let-construct
    (loopbdy', loopres') <- collectRes $ letNormExp loopbdy
    let loopbdy'' = makeLetExp pos loopres' loopbdy'
    --(body', bodyres') <- collectRes $ letNormExp body
    --let body'' = makeLetExp pos bodyres' body'

    (letbdy', letres') <- collectRes $ letNormExp letbdy
    let letbdy'' = makeLetExp pos letres' letbdy'

    -- finally return the new loop
    return $ DoLoop (zip ids exps') idd n' loopbdy'' letbdy'' pos
    --makeVarExpSubst "tmp_loop" pos (DoLoop ind n' body'' mergevars pos)
    
------------------------------------
---- expression-free constructs ----
------------------------------------

letNormExp e@(Literal _) = do return e
letNormExp e@(Read  _ _) = do return e
letNormExp e@(Var     _) = do return e

-------------------------------------
---- expression-list constructs: ----
----     literals & indexed var  ----
-------------------------------------

letNormExp (TupLit exps pos) = do
    exps' <- mapM (subLetoNormExp "tmp_lit") exps
    -- exps' <- mapM (subsNormExp pos "tmp_lit") exps
    return $ TupLit exps' pos

letNormExp (ArrayLit exps tp pos) = do
    exps' <- mapM (subLetoNormExp "tmp_lit") exps
    -- exps' <- mapM (subsNormExp pos "tmp_lit") exps
    return $ ArrayLit exps' tp pos

letNormExp (Index s idx t1 t2 pos) = do
    idx' <- mapM (subLetoNormExp "tmp_ind") idx
    return $ Index s idx' t1 t2 pos

-----------------------
--- unary operators ---
-----------------------

letNormExp (Write e tp pos) = do
    e' <- subLetoNormExp "tmp_write" e
    return $ Write e' tp pos

letNormExp (Negate e tp pos) = do
    e' <- subLetoNormExp "tmp_neg" e
    return $ Negate e' tp pos

letNormExp (Not e pos) = do
    e' <- subLetoNormExp "tmp_not" e
    return $ Not e' pos

------------------------
--- binary operators ---
------------------------

letNormExp (BinOp bop e1 e2 tp pos) = do
    e1' <- subLetoNormExp "tmp_bop" e1
    e2' <- subLetoNormExp "tmp_bop" e2
    return $ (BinOp bop e1' e2' tp pos)

letNormExp (And e1 e2 pos) = do
    e1' <- subLetoNormExp "tmp_and" e1
    e2' <- subLetoNormExp "tmp_and" e2
    return $ (And e1' e2' pos)

letNormExp (Or e1 e2 pos) = do
    e1' <- subLetoNormExp "tmp_and" e1
    e2' <- subLetoNormExp "tmp_and" e2
    return $ (Or e1' e2' pos)

---------------------------
---- If construct      ----
---------------------------

letNormExp (If e1 e2 e3 tp pos) = do
    -- transfer the bindings to the outer let
    e1' <- subLetoNormExp "tmp_if" e1
    -- collect bindings for the each branches
    (e2', bnds2) <- collectRes $ letNormExp e2
    (e3', bnds3) <- collectRes $ letNormExp e3
    -- merge bindings with the exp result for each branch
    let e2'' = addPatterns pos bnds2 e2'
    let e3'' = addPatterns pos bnds3 e3'
    -- finally, build the normalized if
    return $ If e1' e2'' e3'' tp pos

---------------------------
---- Function Call     ----
---------------------------

letNormExp (Apply fnm args tp pos) = do
    -- transfer the bindings of arg exps to the outer let
    args'  <- mapM (subLetoNormExp "tmp_arg") args
    -- substitute the call with a fresh variable
    makeVarExpSubst "tmp_call" pos (Apply fnm args' tp pos)
    

-----------------------------------------------
---- Iota/Size/Replicate/Reshape/Transpose ----
-----------------------------------------------

letNormExp (Iota e pos) = do
    e' <- subLetoNormExp "tmp_arg" e
    makeVarExpSubst "tmp_iota" pos (Iota e' pos)

letNormExp (Size arr pos) = do
    arr' <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_size" pos (Size arr' pos)

letNormExp (Replicate n arr pos) = do
    n'    <- subLetoNormExp "tmp_arg" n
    -- normalized arr & get it outside replicate
    arr'  <- (letNormExp arr >>= makeVarExpSubst "tmp_arr" pos)
    makeVarExpSubst "tmp_repl" pos (Replicate n' arr' pos)

letNormExp (Reshape dims arr tp1 tp2 pos) = do
    dims' <- mapM (subLetoNormExp "tmp_dim") dims
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_resh" pos (Reshape dims' arr' tp1 tp2 pos)

letNormExp (Transpose arr tp1 tp2 pos) = do
    -- normalized arr param & get it outside replicate
    arr' <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_tran" pos (Transpose arr' tp1 tp2 pos)


-------------------------------------
---- Zip/Unzip/Split/Concat/Copy ----
-------------------------------------

letNormExp (Zip arrtps pos) = do
    let (arrs, tps) = unzip arrtps
    arrs' <- mapM letNormExp arrs >>= mapM (makeVarExpSubst "tmp_arr" pos)
    makeVarExpSubst "tmp_zip" pos (Zip (zip arrs' tps) pos)

letNormExp (Unzip arr tps pos) = do
    arr' <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_unzip" pos (Unzip arr' tps pos)

letNormExp (Split n arr tp pos) = do
    n'    <- subLetoNormExp "tmp_arg" n
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_split" pos (Split n' arr' tp pos)

letNormExp (Concat arr1 arr2 tp pos) = do
    arr1' <- letNormExp arr1 >>= makeVarExpSubst "tmp_arr" pos
    arr2' <- letNormExp arr2 >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_conc" pos (Concat arr1' arr2' tp pos)

letNormExp (Copy arr pos) = do
    arr' <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_copy" pos (Size arr' pos)


-----------------------------------------------
---- Map/Filter/Mapall/Reduce/Scan/Redomap ----
-----------------------------------------------

letNormExp (Map lam arr tp1 tp2 pos) = do
    lam'  <- letNormLambda lam
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_map" pos (Map lam' arr' tp1 tp2 pos)

letNormExp (Mapall lam arr tp1 tp2 pos) = do
    lam'  <- letNormLambda lam
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_map" pos (Mapall lam' arr' tp1 tp2 pos)

letNormExp (Filter lam arr tp pos) = do
    lam'  <- letNormLambda lam
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_filt" pos (Filter lam' arr' tp pos)

letNormExp (Reduce lam ne arr tp pos) = do
    lam'  <- letNormLambda lam
    ne'   <- subLetoNormExp "tmp_arg" ne
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_red" pos (Reduce lam' ne' arr' tp pos)

letNormExp (Scan lam ne arr tp pos) = do
    lam'  <- letNormLambda lam
    ne'   <- subLetoNormExp "tmp_arg" ne
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_scan" pos (Scan lam' ne' arr' tp pos)

letNormExp (Redomap lam1 lam2 ne arr tp1 tp2 pos) = do
    lam1' <- letNormLambda lam1
    lam2' <- letNormLambda lam2
    ne'   <- subLetoNormExp "tmp_arg" ne
    arr'  <- letNormExp arr >>= makeVarExpSubst "tmp_arr" pos
    makeVarExpSubst "tmp_red" pos (Redomap lam1' lam2' ne' arr' tp1 tp2 pos)

-------------------------------------------------------
-------------------------------------------------------
---- Pattern Match The Rest of the Implementation! ----
----          NOT USED !!!!!                       ----
-------------------------------------------------------        
-------------------------------------------------------

--------------------------------------------------------------------------------------
--letNormExp e = gmapM ( mkM (subLetoNormExp "tmp")
--                        `extM` letNormLambda
--                        `extM` mapM (subLetoNormExp     "tmp")
--                        `extM` mapM (subLetoNormExpPair "tmp") ) e
--
--
--subLetoNormExpPair :: TypeBox tf => String -> (Exp tf, tf) -> LetNormM tf (Exp tf, tf)
--subLetoNormExpPair str (e,t) = do e' <- subLetoNormExp str e
--                                  return (e',t)
--
--
----letNormExp e = do
----    return e
--------------------------------------------------------------------------------------

letNormLambda :: TypeBox tf => Lambda tf -> LetNormM tf (Lambda tf)
letNormLambda (AnonymFun params body ret pos) = do
    (body', newbnds) <- collectRes $ letNormExp body
    let body'' = addPatterns pos newbnds body'
    return $ AnonymFun params body'' ret pos

letNormLambda (CurryFun fname exps rettype pos) = do
    exps'  <- mapM (subLetoNormExp "tmp_arg") exps
    return $ CurryFun fname exps' rettype pos 
 
---------------------------
---------------------------
---- Helper Functions -----
---------------------------
---------------------------
{-
subsNormExp :: TypeBox tf => SrcLoc -> String -> Exp tf -> LetNormM tf (Exp tf)
subsNormExp pos str ee = letNormExp ee >>= makeVarExpSubst str pos
-}

subLetoNormExp :: TypeBox tf => String -> Exp tf -> LetNormM tf (Exp tf)
subLetoNormExp str ee = letNormExp ee >>= subsLetExp str 
    where
        subsLetExp :: TypeBox ty => String -> Exp ty -> LetNormM ty (Exp ty)
        subsLetExp s e = 
            case e of
                (LetPat      _ _ _ pos) -> makeVarExpSubst s pos e
                (LetWith _ _ _ _ _ pos) -> makeVarExpSubst s pos e
                (If        _ _ _ _ pos) -> makeVarExpSubst s pos e
                _                       -> do return e

makeVarExpSubst :: TypeBox ty => String -> SrcLoc -> Exp ty -> LetNormM ty (Exp ty)
-- Precondition: e is normalized!
makeVarExpSubst str pos e = case e of
    -- do not substitute a var or an indexed var
    Var   _         -> do return e
    Index _ _ _ _ _ -> do return e
    -- perform substitution for all other expression
    _               -> do
        tmp_nm <- new str
        let idd = Ident { identName = tmp_nm, 
                          identType = getExpType e, 
                          identSrcLoc = pos
                        }
        _ <- tell $ LetNormRes True [(tmp_nm, e)]
        return $ Var idd
    


-------------------------------------------------------------------
---- makeLetExp SEMANTICALLY EQUIVALENT with addPatterns ????? ----
-------------------------------------------------------------------

makeLetExp :: TypeBox tf => SrcLoc -> [(String, Exp tf)] -> Exp tf -> Exp tf -- LetNormM tf (Exp tf)
makeLetExp _   []           body = body

-- Preconditions: lst contains normalized bindings, 
--                   i.e., the exp in lst are normalized
--                and body is normalized as well.
makeLetExp pos l@((vnm,ee):lll) body =
    case body of
        (LetPat pat1 (Var id1) b1 p1) -> 
            if vnm == identName id1 && not (isLetPatWith ee) 
            then let b1' = makeLetExp pos lll b1
                 in  combinePats (ReguPat pat1 p1) ee b1'
                    -- return $ LetPat pat1 ee b1' p1
            else commonCase l body
        _ -> commonCase l body
    where
        isLetPatWith e = case e of
            LetPat  _ _ _ _     -> True
            LetWith _ _ _ _ _ _ -> True
            _                   -> False

        commonCase :: TypeBox ty => [(String, Exp ty)] -> Exp ty -> Exp ty -- LetNormM ty (Exp ty)
        commonCase []          bdy = bdy
        commonCase ((nm,e):ll) bdy = 
            let bdy' = makeLetExp pos ll bdy
                idd = Ident { identName   = nm, 
                              identType   = getExpType e, 
                              identSrcLoc = pos
                            }
            in combinePats (ReguPat (Id idd) pos) e bdy'
            -- letNormExp (LetPat (Id idd) e bdy' pos)


data PatAbstr tf = ReguPat (TupIdent tf) SrcLoc
                 | WithPat (Ident tf) (Ident tf) [Exp tf] SrcLoc 

combinePats :: TypeBox tf => PatAbstr tf -> Exp tf -> Exp tf -> Exp tf
combinePats rp@(ReguPat y pos) e body =
    case e of
        -- let y = (let x = def_x in e_x) in body
        LetPat x def_x e_x pos_x ->
            LetPat x def_x (combinePats rp e_x body) pos_x

        -- let y = (let x1 = x0 with [inds] <- el in e_x) in body
        LetWith x1 x0 inds el e_x pos_x ->
            LetWith x1 x0 inds el (combinePats rp e_x body) pos_x

        -- let y = (loop (...) for i < N do loopbody in letbody) in body
        DoLoop idexps idd n loopbdy letbdy pos_x ->
            DoLoop idexps idd n loopbdy (combinePats rp letbdy body) pos_x

        -- not a let bindings
        _ -> LetPat y e body pos

combinePats wp@(WithPat y1 y0 inds pos) el body = 
    case el of
        -- let y1 = y0 with [inds] <- (let x = def_x in e_x) in body
        LetPat x def_x e_x pos_x ->
            LetPat x def_x (combinePats wp e_x body) pos_x

        -- let y1 = y0 with [inds] <- (let x1 = x0 with [indsx] <- el_x in e_x) in body
        LetWith x1 x0 inds_x el_x e_x pos_x ->
            LetWith x1 x0 inds_x el_x (combinePats wp e_x body) pos_x

        -- let y1 = y0 with [inds] <- (loop (...) = for i < N do loopbdy in letbdy) in body
        DoLoop idexps idd n loopbdy letbdy pos_x ->
            DoLoop idexps idd n loopbdy (combinePats wp letbdy body) pos_x
        
        -- not a let bindings
        _ -> LetWith y1 y0 inds el body pos    

addPatterns :: TypeBox tf => SrcLoc -> [(String, Exp tf)] -> Exp tf -> Exp tf
addPatterns = makeLetExp
--addPatterns :: TypeBox tf => SrcLoc -> [(String, Exp tf)] -> Exp tf -> Exp tf
--addPatterns _   []         bdy = bdy
--addPatterns pos (pat:pats) bdy = 
--    let (nm, e) = pat
--        idd = Ident { identName = nm, 
--                      identType = getExpType e, 
--                      identSrcLoc = pos
--                    }
--        rpat = ReguPat (Id idd) pos
--    in  combinePats rpat e (addPatterns pos pats bdy)
-------    in  LetPat (Id idd) e (addPatterns pos pats bdy) pos

