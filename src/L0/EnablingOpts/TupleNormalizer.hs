{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpts.TupleNormalizer ( tupleNormProg )
  where
 
import Control.Monad.State
import Control.Applicative
import Control.Monad.Reader
--import Control.Monad.Writer

 
--import qualified Data.Set as S
import qualified Data.Map as M

import L0.AbSyn
import L0.Traversals
import Data.Loc
 
import L0.FreshNames

--import L0.Traversals
import L0.EnablingOpts.EnablingOptErrors

--import Debug.Trace

-----------------------------------------------------------------
-----------------------------------------------------------------
---- This file implements Tuple Normalization:               ----
----    1. x : (tp1 * (tp2*tp3)) replaced with (x1,(x2,x3))  ----
-----------------------------------------------------------------
-----------------------------------------------------------------

data TupNormEnv tf = TupNormEnv {   
                        envVtable  :: M.Map Name (TupIdent tf)
                     }


newtype TupNormM tf a = TupNormM (StateT NameSource (ReaderT (TupNormEnv tf) (Either EnablingOptError)) a)
    deriving (  MonadState NameSource, 
                MonadReader (TupNormEnv tf),
                Monad, Applicative, Functor )


-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: TupNormEnv tf -> (Name, TupIdent tf) -> TupNormEnv tf
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: TupNormEnv tf -> [(Name, TupIdent tf)] -> TupNormEnv tf
bindVars = foldl bindVar

binding :: [(Name, TupIdent tf)] -> TupNormM tf a -> TupNormM tf a
binding bnds = local (`bindVars` bnds)


-- | The program normalizer runs in this monad.  The mutable
-- state refers to the fresh-names engine. The reader hides
-- the vtable that associates variable names with/to-be-substituted-for tuples pattern.
-- The 'Either' monad is used for error handling.
runNormM :: TypeBox tf => Prog tf -> TupNormM tf a -> TupNormEnv tf -> Either EnablingOptError a
runNormM prog (TupNormM a) =
    runReaderT (evalStateT a (newNameSourceForProg prog))

badTupNormM :: EnablingOptError -> TupNormM tf a
badTupNormM = TupNormM . lift . lift . Left

-- | Return a fresh, unique name.  The @Name@ is prepended to the
-- name.
new :: TypeBox tf => Name -> TupNormM tf Name
new = state . newName


-----------------------------------------------------------------
--- Tuple Normalizer Entry Point: normalizes tuples@pgm level ---
-----------------------------------------------------------------

--tupleNormProg :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
tupleNormProg :: Prog Type -> Either EnablingOptError (Prog Type)
tupleNormProg prog = do
    let env = TupNormEnv { envVtable = M.empty }
    runNormM prog (mapM tupleNormFun prog) env


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing a function: for every tuple param, e.g.,    ----
----            (int*(real*int)) x                           ----
----     pattern match it with a tuple at the beginning      ----
----            let (x1,(x2,x3)) = x in body'                ----
----     where body' is the normalized-body of the function  ---- 
-----------------------------------------------------------------
-----------------------------------------------------------------

--tupleNormFun :: TypeBox tf => FunDec tf -> TupNormM tf (FunDec tf)
tupleNormFun :: FunDec Type -> TupNormM Type (FunDec Type)
tupleNormFun (fname, rettype, args, body, pos) = do
    --body' <- trace ("in function: "++fname++"\n") (tupleNormAbstrFun args body pos)
    body' <- tupleNormAbstrFun args body pos
    return (fname, rettype, args, body', pos)

-----------------------------------------------------------------
-----------------------------------------------------------------
---- Normalizing an expression                               ----
-----------------------------------------------------------------
-----------------------------------------------------------------

tupleNormExp :: Exp Type -> TupNormM Type (Exp Type)
--tupleNormExp :: TypeBox tf => Exp tf -> TupNormM tf (Exp tf)

-----------------------------
---- LetPat/With/Do-Loop ----
-----------------------------

tupleNormExp (LetPat pat e body _) = do
    e'    <- tupleNormExp  e
    (pat', bnds) <- mkFullPattern pat
    body' <- binding bnds $ tupleNormExp  body 
    distribPatExp pat' e' body'



tupleNormExp (DoLoop mergepat mergeexp idd n loopbdy letbdy pos) = do
    (mergepat', bnds) <- mkFullPattern mergepat
    mergeexp' <- tupleNormExp mergeexp
    n'    <- tupleNormExp n    

    loopbdy' <- binding bnds $ tupleNormExp loopbdy
    letbdy'  <- binding bnds $ tupleNormExp letbdy

    return $ DoLoop mergepat' mergeexp' idd n' loopbdy' letbdy' pos
       
---------------------------------------------------------------
-- OLD AND UGLY VERSION OF DO-LOOP
---------------------------------------------------------------
--tupleNormExp (DoLoop ind n body mergevars pos) = do
    --n'    <- tupleNormExp n
    --body' <- tupleNormExp body
    --bools <- mapM checkNotSubst mergevars
    --if foldl (&&) True bools
    --then return $ DoLoop ind n' body' mergevars pos
    --else badTupNormM $ EnablingOptError pos "Error in tupleNormExp for DoLoop see above!"
    --
    --where
    --    checkNotSubst :: Ident Type -> TupNormM Type Bool
    --    checkNotSubst idd = do
    --        bnd <- asks $ M.lookup (identName idd) . envVtable
    --        case bnd of
    --            Nothing-> return True 
    --            Just _ -> badTupNormM $ EnablingOptError pos ("tupleNormExp Implementation Shortcoming: "
    --                                                          ++" a merged var cannot have a tuple type")

tupleNormExp (LetWith nm src inds el body pos) = do
    bnd <- asks $ M.lookup (identName src) . envVtable
    case bnd of
        Nothing  -> do  inds' <- mapM tupleNormExp inds
                        el'   <- tupleNormExp el
                        body' <- tupleNormExp body
                        return $ LetWith nm src inds' el' body' pos
        Just _   -> badTupNormM $ EnablingOptError pos ("In tupleNormExp of LetWith, broken invariant: "
                                                        ++" source array var has a TupId binding! ")

-------------------------------------------------------
--- Var/Index ...
-------------------------------------------------------

tupleNormExp e@(Var (Ident vnm _ pos)) = do 
    bnd <- asks $ M.lookup vnm . envVtable
    case bnd of
        Nothing  -> return e
        Just pat -> mkTuplitFromPat pat
    where
        mkTuplitFromPat :: TupIdent Type -> TupNormM Type (Exp Type)
        mkTuplitFromPat (Id idd) = do
            let idd'= Ident { identName = identName idd, 
                              identType = identType idd, 
                              identSrcLoc = pos
                            }
            return $ Var idd' 
        mkTuplitFromPat (TupId tupids _) = do 
            exps <- mapM mkTuplitFromPat tupids
            return $ TupLit exps pos 

tupleNormExp (Index idd inds tp2 pos) = do
    bnd <- asks $ M.lookup (identName idd) . envVtable
    case bnd of
        Nothing  -> do  inds' <- mapM tupleNormExp inds
                        return $ Index idd inds' tp2 pos
        Just _   -> badTupNormM $ EnablingOptError pos ("In tupleNormExp of Index, broken invariant: "
                                                        ++" indexed var has a TupId binding! ")
-------------------------------------------------------
-------------------------------------------------------
---- Pattern Match The Rest of the Implementation! ----
----          NOT USED !!!!!                       ----
-------------------------------------------------------        
-------------------------------------------------------


--
--tupleNormExp (Apply fnm params tp pos) = do
--    params' <- mapM tupleNormExp params
--    return $ Apply fnm params' tp pos


tupleNormExp e = mapExpM tupleNorm e
  where tupleNorm = identityMapper {
                      mapOnExp = tupleNormExp
                    , mapOnLambda = tupleNormLambda
                    }

tupleNormLambda :: Lambda Type -> TupNormM Type (Lambda Type)
tupleNormLambda (AnonymFun params body ret pos) = do  
    body' <- tupleNormAbstrFun params body pos
    return $ AnonymFun params body' ret pos

tupleNormLambda (CurryFun fname exps rettype pos) = do
    exps'  <- mapM tupleNormExp exps
    return $ CurryFun fname exps' rettype pos 


--tupleNormExp e = do
--    return e



-----------------------------------------------------------------
-----------------------------------------------------------------
---- HELPER FUNCTIONS                                        ----
-----------------------------------------------------------------
-----------------------------------------------------------------
distribPatExp :: TupIdent Type -> Exp Type -> Exp Type -> TupNormM Type (Exp Type)

distribPatExp pat@(Id idd) e body =
    return $ LetPat pat e body (identSrcLoc idd)

distribPatExp pat@(TupId idlst pos) e body =
    case e of
        TupLit (e':es) epos
            -- sanity check!
          | length idlst /= length (e':es) ->
            badTupNormM $ EnablingOptError pos ("In ArrTup2TupArr, distribPatExp, broken invariant: "
                                                ++" the lengths of TupleLit and TupId differ! exp: "
                                                    ++ppExp 0 e++" tupid: "++ppTupId pat )
          | [ident] <- idlst ->
            distribPatExp ident e' body
          | ident:idlst' <- idlst -> do
             body' <- distribPatExp (TupId idlst' pos) (TupLit es epos) body
             distribPatExp ident e' body'
        _ -> return $ LetPat pat e body pos


--------------------
--- from a potentially partially instantiated tuple id, it creates
---    a fully instantiated tuple id, and adds the corresponding
---    (new) bindings to the symbol table.
--------------------
mkFullPattern :: TupIdent Type -> TupNormM Type (TupIdent Type, [(Name, TupIdent Type)])

mkFullPattern pat@(Id ident) = do
    let (nm, tp) = (identName ident, identType ident)
    case tp of
        Elem (Tuple {}) ->
            do  pat' <- mkPatFromType (identSrcLoc ident) nm tp
                return (pat', [(nm, pat')])
        _           -> return (pat, [])

mkFullPattern (TupId idlst pos) = do
    reslst <- mapM mkFullPattern idlst
    let (tupids, bndlsts) = unzip reslst
    return (TupId tupids pos, concat bndlsts)

-----------------------
-- given a (tuple) type, creates a fully instantiated TupIdent 
-----------------------
mkPatFromType :: SrcLoc -> Name -> Type -> TupNormM Type (TupIdent Type)

mkPatFromType pos nm (Elem (Tuple tps)) = do
  tupids <- mapM (mkPatFromType pos nm) tps
  return $ TupId tupids pos
mkPatFromType pos nm tp = do
  tmp_nm <- new nm
  return $ Id Ident { identName = tmp_nm
                    , identType = tp
                    , identSrcLoc = pos  }


--------------------------------------------------
---- Helper for function declaration / lambda ----
--------------------------------------------------
tupleNormAbstrFun :: [Ident Type] -> Exp Type -> SrcLoc -> TupNormM Type (Exp Type)
tupleNormAbstrFun args body pos = do
    let tups = filter isTuple args
    let vars = map Var tups
    resms <- mapM (mkFullPattern . Id) tups
    let (pats, bndlst)  = unzip resms 
    let bnds = concat bndlst
    body'  <- binding bnds $ tupleNormExp body
    mergePatterns (reverse pats) (reverse vars) body'

    where    
        mergePatterns :: [TupIdent Type] -> [Exp Type] -> Exp Type -> TupNormM Type (Exp Type)
        mergePatterns [] [] bdy = return bdy
        mergePatterns [] _  _   = 
            badTupNormM $ EnablingOptError pos
                                           ("in TupleNormalizer.hs, mergePatterns: "
                                            ++" lengths of tups and exps don't agree!")
        mergePatterns _  [] _   = 
            badTupNormM $ EnablingOptError pos 
                                           ("in TupleNormalizer.hs, mergePatterns: "
                                            ++" lengths of tups and exps don't agree!")
        mergePatterns (pat:pats) (e:es) bdy =
            mergePatterns pats es (LetPat pat e bdy pos)


isTuple :: Ident Type -> Bool
isTuple x = case identType x of
              Elem (Tuple {}) -> True
              _               -> False

  
------------------
---- NOT USED ----
------------------


------------------------
--- whether the pattern is fully instanciated w.r.t. its type
------------------------
{-
isFullTuple :: TupIdent Type -> Bool

isFullTuple (Id ident) =
    case identType ident of
        Tuple _ _ -> False
        _         -> True

isFullTuple (TupId idlst _) = 
    foldl (&&) True (map isFullTuple idlst)
-}


-------------------
--- old version ---
-------------------
{-
tupleNormAbstrFunOld :: [Ident Type] -> Exp Type -> SrcLoc -> TupNormM Type (Exp Type)
tupleNormAbstrFunOld args body pos = do
    let tups    = filter isTuple args
    let nms     = map identName tups
    (ok, bnds) <- procTups tups body []

    if ok
    then do -- required pattern match already done
            -- add the vtable bindings and normalize body
            body' <- binding (zip nms bnds) $ tupleNormExp body 
            return body'
    else do -- add the vtable bindings, normalize body
            tups' <- mapM makeTupId tups
            let bnds' = zip nms tups'
            body' <- binding bnds' $ tupleNormExp body
            -- Finally, add the required pattern matching 
            -- on top of the normalized body
            let pats = zip tups tups' 
            let body'' = addPatterns pos pats body'
            return body''

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


        makeTupId :: TypeBox ty => Ident Type -> TupNormM ty (TupIdent ty)
        makeTupId ident = do
            case tp of
                Tuple tps _ -> do
                    tupids <- mapM makeTupIdFromType tps
                    let res = TupId tupids pp
                    return res
                _ ->badTupNormM $ TypeError pp 
                                    ( "Id: " ++ nm ++ 
                                      " not a tuple: " ++ ppType tp )
             
            where
                (nm, tp, pp) = (identName ident, identType ident, identSrcLoc ident)

                makeTupIdFromType :: TypeBox ty => Type -> TupNormM ty (TupIdent ty)
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

 
        procTups :: TypeBox ty => [Ident Type] -> Exp ty -> [TupIdent ty] -> TupNormM ty (Bool,[TupIdent ty])
        procTups []     _   lst = return (True, reverse lst) 
        procTups (i:is) bdy lst = do
            case bdy of
                LetPat tupids (Var idd) bdy' _ -> do
                    is_complete <- matchesTupleType ((identType i), tupids)
                    if  identName idd == identName i && is_complete
                    then procTups is bdy' (tupids:lst)
                    else return (False, lst)
                _ ->     return (False, lst)

        matchesTupleType :: TypeBox ty => (Type, TupIdent ty) -> TupNormM ty Bool
        matchesTupleType (tp, tup) = do  -- whether is a complete tuple pattern.
            case tup of
                Id _ -> 
                    case tp of
                        Tuple _ _ -> return False -- id abstracts a tuple
                        _         -> return True  -- ok, id is not a tuple
        
                TupId tupids _ -> 
                    case tp of
                        Tuple tps _ -> 
                            if (length tupids) == (length tps)
                            then do bools <- mapM matchesTupleType (zip tps tupids)
                                    return $ foldl (&&) True bools                                  
                            else badTupNormM $ TypeError pos ("TupIdent: " ++ ppTupId tup ++
                                                              " length doesn't match type: " ++ ppType tp)
                        _     -> badTupNormM $ TypeError pos ("TupIdent: " ++ ppTupId tup ++
                                                              " does not match type: "++ppType tp)
-}

