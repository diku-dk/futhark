{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.CopyCtPropFold (
                                copyCtProp
                              , copyCtPropOneTupleLambda
                            )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Array
import Data.List
import Data.Maybe

import Data.Bits
import Data.Loc

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

import L0C.L0

import L0C.EnablingOpts.EnablingOptErrors
import qualified L0C.Interpreter as Interp

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

data CtOrId  = Constant Value Type Bool
             -- value for constant propagation

             | VarId VName Type Bool
             -- Variable id for copy propagation

             | SymArr Exp Type Bool
             -- various other opportunities for copy
             -- propagation, for the moment: (i) an indexed variable,
             -- (ii) a iota array, (iii) a replicated array, (iv) a TupLit,
             -- and (v) an ArrayLit.   I leave this one open, i.e., Exp,
             -- as I do not know exactly what we need here
             -- To Cosmin: Clean it up in the end, i.e., get rid of Exp.

data CPropEnv = CopyPropEnv {
    envVtable  :: HM.HashMap VName CtOrId,
    program    :: Prog
  }


data CPropRes = CPropRes {
    resSuccess :: Bool
  -- ^ Whether we have changed something.
  , resNonRemovable :: [VName]
  -- ^ The set of variables used as merge variables.
  }


instance Monoid CPropRes where
  CPropRes c1 m1 `mappend` CPropRes c2 m2 =
    CPropRes (c1 || c2) (m1 `union` m2)
  mempty = CPropRes False []

newtype CPropM a = CPropM (WriterT CPropRes (ReaderT CPropEnv (Either EnablingOptError)) a)
    deriving (MonadWriter CPropRes,
              MonadReader CPropEnv,
              Monad, Applicative, Functor)

-- | We changed part of the AST, and this is the result.  For
-- convenience, use this instead of 'return'.
changed :: a -> CPropM a
changed x = do
  tell $ CPropRes True []
  return x


-- | This name was used as a merge variable.
nonRemovable :: VName -> CPropM ()
nonRemovable name =
  tell $ CPropRes False [name]

-- | The identifier was consumed, and should not be used within the
-- body, so mark it and any aliases as nonremovable (with
-- 'nonRemovable') and delete any expressions using it or an alias
-- from the symbol table.
consuming :: Ident -> CPropM a -> CPropM a
consuming idd m = do
  (vtable, removed) <- spartition ok <$> asks envVtable
  mapM_ nonRemovable $ identName idd : HM.keys removed
  local (\e -> e { envVtable = vtable }) m
  where als = identName idd `HS.insert` aliases (identType idd)
        spartition f s = let s' = HM.filter f s
                         in (s', s `HM.difference` s')
        ok (Constant {})  = True
        ok (VarId k _ _)  = not $ k `HS.member` als
        ok (SymArr e _ _) = HS.null $ als `HS.intersection` freeNamesInExp e

-- | @collectNonRemovable mvars m@ executes the action @m@.  The
-- intersection of @mvars@ and any variables used as merge variables
-- while executing @m@ will also be returned, and removed from the
-- writer result.  The latter property is only important if names are
-- not unique.
collectNonRemovable :: [VName] -> CPropM a -> CPropM (a, [VName])
collectNonRemovable mvars m = pass collect
  where collect = do
          (x,res) <- listen m
          return ((x, mvars `intersect` resNonRemovable res),
                  const $ res { resNonRemovable = resNonRemovable res \\ mvars})


-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runCPropM :: CPropM a -> CPropEnv -> Either EnablingOptError (a, CPropRes)
runCPropM  (CPropM a) = runReaderT (runWriterT a)

badCPropM :: EnablingOptError -> CPropM a
badCPropM = CPropM . lift . lift . Left


-- | Bind a name as a common (non-merge) variable.
bindVar :: CPropEnv -> (VName, CtOrId) -> CPropEnv
bindVar env (name,val) =
  env { envVtable = HM.insert name val $ envVtable env }

bindVars :: CPropEnv -> [(VName, CtOrId)] -> CPropEnv
bindVars = foldl bindVar

binding :: [(VName, CtOrId)] -> CPropM a -> CPropM a
binding bnds = local (`bindVars` bnds)

-- | Applies Copy/Constant Propagation and Folding to an Entire Program.
copyCtProp :: Prog -> Either EnablingOptError (Bool, Prog)
copyCtProp prog = do
    let env = CopyPropEnv { envVtable = HM.empty, program = prog }
    -- res   <- runCPropM (mapM copyCtPropFun prog) env
    -- let (bs, rs) = unzip res
    (rs, res) <- runCPropM (mapM copyCtPropFun $ progFunctions prog) env
    return (resSuccess res, Prog rs)

copyCtPropFun :: FunDec -> CPropM FunDec
copyCtPropFun (fname, rettype, args, body, pos) = do
    body' <- copyCtPropExp body
    return (fname, rettype, args, body', pos)


-----------------------------------------------------------------
---- Run on Lambda Only!
-----------------------------------------------------------------

copyCtPropOneTupleLambda :: Prog -> TupleLambda -> Either EnablingOptError TupleLambda
copyCtPropOneTupleLambda prog lam = do
    let env = CopyPropEnv { envVtable = HM.empty, program = prog }
    (res, _) <- runCPropM (copyCtPropTupleLambda lam) env
    return res

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Copy/Ct propagation and folding for exps    ----
--------------------------------------------------------------------
--------------------------------------------------------------------

copyCtPropExp :: Exp -> CPropM Exp

copyCtPropExp (LetWith cs nm src inds el body pos) =
  consuming src $ do
    cs'       <- copyCtPropCerts cs
    el'       <- copyCtPropExp el
    inds'     <- mapM copyCtPropExp inds
    body'     <- copyCtPropExp body
    return $ LetWith cs' nm src inds' el' body' pos

copyCtPropExp (LetPat pat e body pos) = do
    e'    <- copyCtPropExp e
    remv  <- isRemovablePat pat e'
    bnds  <- getPropBnds pat e' remv

    (body', mvars) <-  collectNonRemovable (map fst bnds) $
                       if null bnds then copyCtPropExp body
                       else binding bnds $ copyCtPropExp body
    if remv && null mvars then changed body'
    else return $ LetPat pat e' body' pos


copyCtPropExp (DoLoop mergepat mergeexp idd n loopbdy letbdy pos) = do
    mergeexp'    <- copyCtPropExp mergeexp
    n'       <- copyCtPropExp n
    loopbdy' <- copyCtPropExp loopbdy
    letbdy'  <- copyCtPropExp letbdy
    return $ DoLoop mergepat mergeexp' idd n' loopbdy' letbdy' pos


copyCtPropExp e@(Var (Ident vnm _ pos)) = do
  bnd <- asks $ HM.lookup vnm . envVtable
  case bnd of
    Nothing                 -> return e
    Just (Constant v   _ _) -> if isBasicTypeVal v
                               then changed $ Literal v pos
                               else return e
    Just (VarId  id' tp1 _) -> changed $ Var (Ident id' tp1 pos) -- or tp
    Just (SymArr e'    _ _) ->
      case e' of
        Replicate {}      -> return e
        TupLit    _ _     -> if isCtOrCopy e then changed e' else return e
        ArrayLit  {}      -> return e
        Index {}          -> changed e'
        Transpose {}      -> changed e'
        -- DO NOT INLINE IOTA!
        Iota  _ _         -> changed e'
        --Iota _ _          -> return e
        _                 -> return e

copyCtPropExp eee@(Index cs idd@(Ident vnm tp p) csidx inds tp2 pos) = do
  inds' <- mapM copyCtPropExp inds
  bnd   <- asks $ HM.lookup vnm . envVtable
  cs'   <- copyCtPropCerts cs
  csidx' <- maybe (return Nothing) (liftM Just) $ liftM copyCtPropCerts csidx
  case bnd of
    Nothing               -> return  $ Index cs' idd csidx' inds' tp2 pos
    Just (VarId  id' _ _) -> changed $ Index cs' (Ident id' tp p) csidx' inds' tp2 pos
    Just (Constant v _ _) ->
      case v of
        ArrayVal _ _ ->
          let sh = arrayShape v
          in case ctIndex inds' of
               Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
               Just iis->
                 if length iis == length sh
                 then case getArrValInd v iis of
                        Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                        Just el -> changed $ Literal el pos
                 else return $ Index cs' idd csidx' inds' tp2 pos
        _ -> badCPropM $ TypeError pos  " indexing into a non-array value "
    Just (SymArr e' _ _) ->
      case (e', inds') of
        (Iota _ _, [ii]) -> changed ii
        (Iota _ _, _)    -> badCPropM $ TypeError pos  " bad indexing in iota "

        (Index cs2 aa csidx2 ais _ _,_) -> do
            -- the array element type is the same as the one of the big array, i.e., t1
            -- the result type is the same as eee's, i.e., tp2
            inner <- copyCtPropExp(Index (cs'++cs2) aa
                                   (liftM2 (++) csidx' csidx2)
                                   (ais ++ inds') tp2 pos)
            changed inner

        (ArrayLit {}   , _) ->
            case ctIndex inds' of
                Nothing  -> return $ Index cs' idd csidx' inds' tp2 pos
                Just iis -> case getArrLitInd e' iis of
                                Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                                Just el -> changed el

        (TupLit   _ _, _       ) -> badCPropM $ TypeError pos  " indexing into a tuple (found tuplit) "


        (Replicate _ vvv@(Var vv) _, _:is') -> do
            inner <- if null is'
                     then copyCtPropExp vvv
                     else copyCtPropExp (Index cs' vv csidx' is' tp2 pos)
            changed inner
        (Replicate _ (Index cs2 a csidx2 ais _ _) _, _:is') -> do
            inner <- copyCtPropExp (Index (cs'++cs2) a
                                    (liftM2 (++) csidx' csidx2)
                                    (ais ++ is') tp2 pos)
            changed inner
        (Replicate _ (Literal arr@(ArrayVal _ _) _) _, _:is') ->
            case ctIndex is' of
                Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                Just iis-> case getArrValInd arr iis of
                               Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                               Just el -> changed $ Literal el pos
        (Replicate _ val@(Literal _ _) _, _:is') ->
            if null is' then changed val
            else badCPropM $ TypeError pos  " indexing into a basic type "

        (Replicate _ arr@(ArrayLit {}) _, _:is') ->
            case ctIndex is' of
                Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                Just iis-> case getArrLitInd arr iis of
                               Nothing -> return $ Index cs' idd csidx' inds' tp2 pos
                               Just el -> changed el
        (Replicate _ tup@(TupLit _ _) _, _:is') ->
            if null is' && isCtOrCopy tup then changed tup
            else  badCPropM $ TypeError pos  " indexing into a tuple (expected replicate) "
        (Replicate _ (Iota n _) _, _:is')
          | [] <- is'  -> changed $ Iota n pos
          | [x] <- is' -> changed x
          | otherwise -> badCPropM $ TypeError pos  (" illegal indexing: " ++ ppExp eee)
        (Replicate {}, _) ->
            return $ Index cs' idd csidx' inds' tp2 pos

        (Transpose cs2 k n (Var src) _, _)
          | k+n < length inds' ->
            changed $ Index (cs'++cs2) src csidx' (transposeIndex k n inds') tp2 pos
        (Transpose {}, _) -> do
          nonRemovable vnm
          return $ Index cs' idd csidx' inds' tp2 pos

        _ -> badCPropM $ CopyCtPropError pos (" Unreachable case in copyCtPropExp of Index exp: " ++
                                              ppExp eee++" is bound to "++ppExp e' )

copyCtPropExp (BinOp bop e1 e2 tp pos) = do
    e1'   <- copyCtPropExp e1
    e2'   <- copyCtPropExp e2
    ctFoldBinOp (BinOp bop e1' e2' tp pos)

copyCtPropExp (And e1 e2 pos) = do
    e1'   <- copyCtPropExp e1
    e2'   <- copyCtPropExp e2
    ctFoldBinOp (And e1' e2' pos)

copyCtPropExp (Or e1 e2 pos) = do
    e1'   <- copyCtPropExp e1
    e2'   <- copyCtPropExp e2
    ctFoldBinOp $ Or e1' e2' pos

copyCtPropExp (Negate e tp pos) = do
    e'   <- copyCtPropExp e
    if isValue e'
    then case e' of
            Literal (IntVal  v) _ -> changed $ Literal (IntVal  (-v)) pos
            Literal (RealVal v) _ -> changed $ Literal (RealVal (0.0-v)) pos
            _ -> badCPropM $ TypeError pos  " ~ operands not of (the same) numeral type! "
    else return $ Negate e' tp pos

copyCtPropExp (Not e pos) = do
    e'   <- copyCtPropExp e
    if isValue e'
    then case e' of
            Literal (LogVal  v) _ -> changed $ Literal (LogVal (not v)) pos
            _ -> badCPropM $ TypeError pos  " not operands not of (the same) numeral type! "
    else return $ Not e' pos

copyCtPropExp (If e1 e2 e3 t pos) = do
    e1' <- copyCtPropExp e1
    e2' <- copyCtPropExp e2
    e3' <- copyCtPropExp e3
    if      isCt1 e1' then changed e2'
    else if isCt0 e1' then changed e3'
    else return $ If e1' e2' e3' t pos

-----------------------------------------------------------
--- If expression is an array literal than replace it   ---
---    with the array's size
-----------------------------------------------------------
copyCtPropExp (Size cs i e pos) = do
    e' <- copyCtPropExp e
    cs' <- copyCtPropCerts cs
    case e' of
        Var idd -> do vv <- asks $ HM.lookup (identName idd) . envVtable
                      case vv of Just (Constant a _ _) -> literal a
                                 Just (SymArr (Iota ne _) _ _)
                                   | i == 0 -> return ne
                                 Just (SymArr (Replicate ne _ _) _ _)
                                   | i == 0 -> return ne
                                 _ -> return $ Size cs' i e' pos
        Literal a _ -> literal a
        Iota ne _        | i == 0 -> return ne
        Replicate ne _ _ | i == 0 -> return ne
        _ ->  return $ Size cs i e' pos
    where literal a =
            case drop i $ arrayShape a of
              []  -> badCPropM $ TypeError pos " array literal has too few dimensions! "
              n:_ -> changed $ Literal (IntVal n) pos

-----------------------------------------------------------
--- If expression is true then just replace assertion   ---
-----------------------------------------------------------

copyCtPropExp (Assert e loc) = do
  e' <- copyCtPropExp e
  case e' of
    Literal (LogVal True) _ -> return $ Literal Checked loc
    Var idd -> do
      vv <- asks $ HM.lookup (identName idd) . envVtable
      case vv of
        Just (Constant (LogVal True) _ _) -> return $ Literal Checked loc
        _                                 -> return $ Assert e' loc
    _ -> return $ Assert e' loc

copyCtPropExp (Conjoin es loc) = do
  es' <- mapM copyCtPropExp es
  -- Remove trivial certificates.
  let check (Literal Checked _) = return Nothing
      check (Var idd) = do
        vv <- asks $ HM.lookup (identName idd) . envVtable
        case vv of
          Just (Constant Checked _ _) -> changed Nothing
          _                           -> return $ Just $ Var idd
      check e = return $ Just e
  es'' <- liftM catMaybes $ mapM check es'
  case es'' of []  -> changed $ Literal Checked loc
               [c] -> changed c
               _   -> return $ Conjoin es'' loc

copyCtPropExp (Apply fname args tp pos) = do
    args' <- mapM (copyCtPropExp . fst) args
    (all_are_vals, vals) <- allArgsAreValues args'
    if all_are_vals
    then do prg <- asks program
            let vv = Interp.runFunNoTrace fname vals  prg
            case vv of
              (Right v) -> changed $ Literal v pos
              _ -> badCPropM $ EnablingOptError pos (" Interpreting fun " ++
                                                     nameToString fname ++ " yields error!")
    else return $ Apply fname (zip args' $ map snd args) tp pos

    where
        allArgsAreValues :: [Exp] -> CPropM (Bool, [Value])
        allArgsAreValues []     = return (True, [])
        allArgsAreValues (a:as) =
            case a of
                Literal v _ -> do (res, vals) <- allArgsAreValues as
                                  if res then return (True,  v:vals)
                                         else return (False, []    )
                Var idd   -> do vv <- asks $ HM.lookup (identName idd) . envVtable
                                case vv of
                                  Just (Constant v _ _) -> do
                                    (res, vals) <- allArgsAreValues as
                                    if res then return (True,  v:vals)
                                           else return (False, []    )
                                  _ -> return (False, [])
                _         -> return (False, [])

------------------------------
--- Pattern Match the Rest ---
------------------------------

copyCtPropExp e = mapExpM mapper e
  where mapper = identityMapper {
                   mapOnExp = copyCtPropExp
                 , mapOnLambda = copyCtPropLambda
                 , mapOnTupleLambda = copyCtPropTupleLambda
                 , mapOnIdent = copyCtPropIdent
                 , mapOnCertificates = copyCtPropCerts
                 }

copyCtPropIdent :: Ident -> CPropM Ident
copyCtPropIdent ident@(Ident vnm _ loc) = do
    bnd <- asks $ HM.lookup vnm . envVtable
    case bnd of
      Nothing                 -> return ident
      Just (VarId  id' tp1 _) -> changed $ Ident id' tp1 loc
      _                       -> do nonRemovable vnm
                                    return ident

copyCtPropCerts :: Certificates -> CPropM Certificates
copyCtPropCerts = liftM concat . mapM check
  where check idd = do
          vv <- asks $ HM.lookup (identName idd) . envVtable
          case vv of
            Just (Constant Checked _ _) -> changed []
            Just (VarId  id' tp1 _)     -> changed [Ident id' tp1 loc]
            _                           -> do
              nonRemovable $ identName idd
              return [idd]
          where loc = srclocOf idd

copyCtPropLambda :: Lambda -> CPropM Lambda
copyCtPropLambda (AnonymFun ids body tp pos) = do
    body' <- copyCtPropExp body
    return $ AnonymFun ids body' tp pos
copyCtPropLambda (CurryFun fname params tp pos) = do
    params' <- copyCtPropExpList params
    return $ CurryFun fname params' tp pos

copyCtPropTupleLambda :: TupleLambda -> CPropM TupleLambda
copyCtPropTupleLambda (TupleLambda ids body tp loc) = do
    body' <- copyCtPropExp body
    return $ TupleLambda ids body' tp loc


copyCtPropExpList :: [Exp] -> CPropM [Exp]
copyCtPropExpList = mapM copyCtPropExp

------------------------------------------------
---- Constant Folding                       ----
------------------------------------------------

ctFoldBinOp :: Exp -> CPropM Exp
ctFoldBinOp e@(BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = changed e2
  | isCt0 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (IntVal  (v1+v2)) pos
      (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (RealVal (v1+v2)) pos
      _ -> badCPropM $ TypeError pos  " + operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (IntVal  (v1-v2)) pos
      (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (RealVal (v1-v2)) pos
      _ -> badCPropM $ TypeError pos  " - operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Times e1 e2 _ pos)
  | isCt0 e1 = changed e1
  | isCt0 e2 = changed e2
  | isCt1 e1 = changed e2
  | isCt1 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (IntVal  (v1*v2)) pos
      (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (RealVal (v1*v2)) pos
      _ -> badCPropM $ TypeError pos  " * operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = changed e1
  | isCt0 e2 = badCPropM $ Div0Error pos
  | isCt1 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (IntVal  (div v1 v2)) pos
      (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (RealVal (v1 / v2)) pos
      _ -> badCPropM $ TypeError pos  " / operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Mod e1 e2 _ pos)
  | isCt0 e2 = badCPropM $ Div0Error pos
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (IntVal  (v1 `mod` v2)) pos
      _ -> badCPropM $ TypeError pos  " % operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Pow e1 e2 _ pos)
  | isCt0 e2 =
    case typeOf e1 of
      Elem Int  -> changed $ Literal (IntVal  1) pos
      Elem Real -> changed $ Literal (RealVal 1.0) pos
      _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = changed e1
  |  isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (v1 ^v2)) pos
      (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (RealVal (v1**v2)) pos
      _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (shiftL v1 v2)) pos
      _ -> badCPropM $ TypeError pos  " << operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = changed e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Literal (IntVal v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (shiftR v1 v2)) pos
      _ -> badCPropM $ TypeError pos  " >> operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Band e1 e2 _ pos)
  | isCt0 e1 = changed e1
  | isCt0 e2 = changed e2
  | isCt1 e1 = changed e2
  | isCt1 e2 = changed e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Literal (IntVal  v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (v1 .&. v2)) pos
      _ -> badCPropM $ TypeError pos  " & operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Bor e1 e2 _ pos)
  | isCt0 e1 = changed e2
  | isCt0 e2 = changed e1
  | isCt1 e1 = changed e1
  | isCt1 e2 = changed e2
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Literal (IntVal v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (v1 .|. v2)) pos
      _ -> badCPropM $ TypeError pos  " | operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = changed e2
  | isCt0 e2 = return e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Literal (IntVal v1) _, Literal (IntVal v2) _) -> changed $ Literal (IntVal  (xor v1 v2)) pos
      _ -> badCPropM $ TypeError pos  " ^ operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(And e1 e2 pos)
  | isCt0 e1 = changed e1
  | isCt0 e2 = changed e2
  | isCt1 e1 = changed e2
  | isCt1 e2 = changed e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Literal (LogVal  v1) _, Literal (LogVal  v2) _) -> changed $ Literal (LogVal  (v1 && v2)) pos
      _ -> badCPropM $ TypeError pos  " && operands not of boolean type! "
  | otherwise = return e
ctFoldBinOp e@(Or e1 e2 pos)
  | isCt0 e1 = changed e2
  | isCt0 e2 = changed e1
  | isCt1 e1 = changed e1
  | isCt1 e2 = changed e2
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Literal (LogVal  v1) _, Literal (LogVal  v2) _) -> changed $ Literal (LogVal  (v1 || v2)) pos
      _ -> badCPropM $ TypeError pos  " || operands not of boolean type! "
  | otherwise = return e

ctFoldBinOp e@(BinOp Equal e1 e2 _ pos)
  | isValue e1 && isValue e2 =
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
        (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (LogVal (v1==v2)) pos
        (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (LogVal (v1==v2)) pos
        (Literal (LogVal  v1) _, Literal (LogVal  v2) _) -> changed $ Literal (LogVal (v1==v2)) pos
        (Literal (CharVal v1) _, Literal (CharVal v2) _) -> changed $ Literal (LogVal (v1==v2)) pos
        --(Literal (TupVal  v1 _), Literal (TupVal  v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        _ -> badCPropM $ TypeError pos  " equal operands not of (the same) basic type! "
  | e1 == e2 = changed $ Literal (LogVal True) pos
  | otherwise = return e
ctFoldBinOp e@(BinOp Less e1 e2 _ pos) =
    if isValue e1 && isValue e2 then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Literal (IntVal  v1) _, Literal (IntVal  v2) _) -> changed $ Literal (LogVal (v1<v2)) pos
        (Literal (RealVal v1) _, Literal (RealVal v2) _) -> changed $ Literal (LogVal (v1<v2)) pos
        (Literal (LogVal  v1) _, Literal (LogVal  v2) _) -> changed $ Literal (LogVal (v1<v2)) pos
        (Literal (CharVal v1) _, Literal (CharVal v2) _) -> changed $ Literal (LogVal (v1<v2)) pos
        --(Literal (TupVal  v1 _), Literal (TupVal  v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        _ -> badCPropM $ TypeError pos  " less-than operands not of (the same) basic type! "
    else return e
ctFoldBinOp e@(BinOp Leq e1 e2 _ pos) =
    if isValue e1 && isValue e2 then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Literal (IntVal  v1 ) _, Literal (IntVal  v2 ) _) -> changed $ Literal (LogVal (v1<=v2)) pos
        (Literal (RealVal v1 ) _, Literal (RealVal v2 ) _) -> changed $ Literal (LogVal (v1<=v2)) pos
        (Literal (LogVal  v1 ) _, Literal (LogVal  v2 ) _) -> changed $ Literal (LogVal (v1<=v2)) pos
        (Literal (CharVal v1 ) _, Literal (CharVal v2 ) _) -> changed $ Literal (LogVal (v1<=v2)) pos
        --(Literal (TupVal  v1 ) _, Literal (TupVal  v2 ) _) -> return (True, Literal (LogVal (v1<=v2)) pos)
        _ -> badCPropM $ TypeError pos  " less-than-or-equal operands not of (the same) basic type! "
    else return e
ctFoldBinOp e = return e



----------------------------------------------------
---- Helpers for Constant Folding                ---
----------------------------------------------------


isValue :: Exp -> Bool
isValue e = case e of
              Literal _ _ -> True
              _           -> False

isCt1 :: Exp -> Bool
isCt1 e = case e of
            Literal (IntVal  one) _ -> one == 1
            Literal (RealVal one) _ -> one == 1.0
            Literal (LogVal True) _ -> True
            _                       -> False
isCt0 :: Exp -> Bool
isCt0 e = case e of
            Literal (IntVal  zr) _   -> zr == 0
            Literal (RealVal zr) _   -> zr == 0.0
            Literal (LogVal False) _ -> True
            _                        -> False

----------------------------------------------------
---- Helpers for Constant/Copy Propagation       ---
----------------------------------------------------

isBasicTypeVal :: Value -> Bool
isBasicTypeVal = basicType . valueType

isCtOrCopy :: Exp -> Bool
isCtOrCopy (Literal  val _ ) = isBasicTypeVal val
isCtOrCopy (TupLit   ts _  ) = all isCtOrCopy ts
isCtOrCopy (Var           _) = True
isCtOrCopy (Iota        _ _) = True
isCtOrCopy (Transpose {}   ) = True
isCtOrCopy (Index {}       ) = True
isCtOrCopy _                 = False

isRemovablePat  :: TupIdent -> Exp -> CPropM Bool

isRemovablePat (TupId tups _) e =
  case e of
    Var (Ident vnm _ _) -> do
     bnd <- asks $ HM.lookup vnm . envVtable
     case bnd of
       Just (Constant val@(TupVal ts) _ _) ->
         return ( isBasicTypeVal val && length ts == length tups )
       Just (SymArr   tup@(TupLit ts _  ) _ _) ->
         return ( isCtOrCopy tup && length ts == length tups )
       _ ->  return False
    TupLit {}                -> return $ isCtOrCopy e
    Literal val@(TupVal _) _ -> return $ isBasicTypeVal val
    _                        -> return False

-- Covers Wildcard and Id.
isRemovablePat _ (Var _)        = return True
isRemovablePat _ (Index {})     = return True
isRemovablePat _ (Literal v _)  = return $ isBasicTypeVal v
isRemovablePat _ e@(TupLit {})  = return $ isCtOrCopy e
isRemovablePat _ (Transpose {}) = return True
isRemovablePat _ (Iota {})      = return True
isRemovablePat _ _              = return False

getPropBnds :: TupIdent -> Exp -> Bool -> CPropM [(VName, CtOrId)]
getPropBnds ( Wildcard _ _ ) _ _ = return []
getPropBnds ( Id (Ident var tp _) ) e to_rem =
  let r = case e of
            Literal v _          -> [(var, Constant v (fromDecl $ valueType v) to_rem)]
            Var (Ident id1 tp1 _)-> [(var, VarId  id1 tp1 to_rem)]
            Index   {}           -> [(var, SymArr e   tp  to_rem)]
            TupLit  {}           -> [(var, SymArr e   tp  to_rem)]
            Transpose   {}       -> [(var, SymArr e   tp  to_rem)]

            Iota {}              -> let newtp = Array Int [Nothing] Nonunique mempty
                                    in  [(var, SymArr e newtp to_rem)]
            Replicate {}      -> [(var, SymArr e tp to_rem)]
            ArrayLit  {}      -> [(var, SymArr e tp to_rem)]
            _ -> []
  in return r
getPropBnds pat@(TupId ids _) e to_rem =
    case e of
        TupLit  ts _          ->
            if length ids == length ts
            then concat <$> mapM (\(x,y)->getPropBnds x y to_rem) (zip ids ts)
            else return []
        Literal (TupVal ts) loc ->
            if length ids == length ts
            then concat <$> mapM (\(x,y)->getPropBnds x (Literal y loc) to_rem) (zip ids ts)
            else return []
        Var (Ident vnm _ loc)   -> do
            bnd <- asks $ HM.lookup vnm . envVtable
            case bnd of
                Just (SymArr tup@(TupLit   _ _) _ _) -> getPropBnds pat tup               to_rem
                Just (Constant tup@(TupVal _) _ _)   -> getPropBnds pat (Literal tup loc) to_rem
                _                                    -> return []
        _ -> return []

ctIndex :: [Exp] -> Maybe [Int]
ctIndex []     = Just []
ctIndex (i:is) =
  case i of
    Literal (IntVal ii) _ ->
      let x = ctIndex is in
      case x of
        Nothing -> Nothing
        Just y -> Just (ii:y)
    _ -> Nothing

getArrValInd :: Value -> [Int] -> Maybe Value
getArrValInd v [] = if isBasicTypeVal v then Just v else Nothing
getArrValInd (ArrayVal arr _) (i:is) = getArrValInd (arr ! i) is
getArrValInd _ _ = Nothing

getArrLitInd :: Exp -> [Int] -> Maybe Exp
getArrLitInd e [] = if isCtOrCopy e then Just e else Nothing
getArrLitInd (ArrayLit els _ _) (i:is) = getArrLitInd (els !! i) is
getArrLitInd (Literal arr@(ArrayVal _ _) loc) (i:is) =
    case getArrValInd arr (i:is) of
        Nothing -> Nothing
        Just v  -> Just (Literal v loc)
getArrLitInd _ _ = Nothing
