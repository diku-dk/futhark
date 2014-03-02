{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.CopyCtPropFold
  ( copyCtProp
  , copyCtPropOneLambda
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Array
import Data.List

import Data.Bits
import Data.Loc

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Set          as S

import L0C.InternalRep
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

data CtOrId  = Value Value Type
             -- ^ value for constant propagation

             | VarId VName Type
             -- ^ Variable id for copy propagation

             | SymArr Exp Type
             -- ^ Various other opportunities for copy propagation,
             -- for the moment: (i) an indexed variable, (ii) a iota
             -- array, (iii) a replicated array, (iv) a TupLit, and
             -- (v) an ArrayLit.  I leave this one open, i.e., Exp, as
             -- I do not know exactly what we need here To Cosmin:
             -- Clean it up in the end, i.e., get rid of Exp.
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

newtype CPropM a = CPropM (WriterT CPropRes (ReaderT CPropEnv (Either EnablingOptError)) a)
    deriving (MonadWriter CPropRes,
              MonadReader CPropEnv,
              Monad, Applicative, Functor)

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
        ok (Value {})   = True
        ok (VarId k _)  = not $ k `HS.member` als
        ok (SymArr e _) = HS.null $ als `HS.intersection` freeNamesInExp e

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
  body' <- copyCtPropBody body
  return (fname, rettype, args, body', pos)

-----------------------------------------------------------------
---- Run on Lambda Only!
-----------------------------------------------------------------

copyCtPropOneLambda :: Prog -> Lambda -> Either EnablingOptError Lambda
copyCtPropOneLambda prog lam = do
  let env = CopyPropEnv { envVtable = HM.empty, program = prog }
  (res, _) <- runCPropM (copyCtPropLambda lam) env
  return res

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main functions: Copy/Ct propagation and folding for exps   ----
--------------------------------------------------------------------
--------------------------------------------------------------------

copyCtPropBody :: Body -> CPropM Body

copyCtPropBody (LetWith cs nm src indcs inds el body pos) =
  consuming src $ do
    cs'    <- copyCtPropCerts cs
    el'    <- copyCtPropSubExp el
    indcs' <- copyCtPropCerts indcs
    inds'  <- mapM copyCtPropSubExp inds
    body'  <- copyCtPropBody body
    return $ LetWith cs' nm src indcs' inds' el' body' pos

copyCtPropBody (LetPat pat e body loc) = do
  let continue e' = do
        let bnds = getPropBnds pat e'
        body' <- binding bnds $ copyCtPropBody body
        return $ LetPat pat e' body' loc
      continue' es = continue $ TupLit es loc
  e' <- copyCtPropExp e
  case e' of
    If e1 tb fb _ _
      | isCt1 e1 -> mapTailM continue' tb
      | isCt0 e1 -> mapTailM continue' fb
    _ -> continue e'


copyCtPropBody (DoLoop merge idd n loopbdy letbdy loc) = do
  let (mergepat, mergeexp) = unzip merge
  mergeexp'    <- mapM copyCtPropSubExp mergeexp
  n'       <- copyCtPropSubExp n
  loopbdy' <- copyCtPropBody loopbdy
  letbdy'  <- copyCtPropBody letbdy
  return $ DoLoop (zip mergepat mergeexp') idd n' loopbdy' letbdy' loc

copyCtPropBody (Result es loc) =
  Result <$> mapM copyCtPropSubExp es <*> pure loc

copyCtPropSubExp :: SubExp -> CPropM SubExp
copyCtPropSubExp e@(Var (Ident vnm _ pos)) = do
  bnd <- asks $ HM.lookup vnm . envVtable
  case bnd of
    Just (Value v   _)
      | isBasicTypeVal v  -> changed $ Constant v pos
    Just (VarId  id' tp1) -> changed $ Var (Ident id' tp1 pos) -- or tp
    Just (SymArr (SubExp se) _) -> changed se
    _                       -> return e
copyCtPropSubExp (Constant v loc) = return $ Constant v loc

copyCtPropExp :: Exp -> CPropM Exp

copyCtPropExp (TupLit es loc) =
  TupLit <$> mapM copyCtPropSubExp es <*> pure loc

copyCtPropExp (Index cs idd@(Ident vnm tp p) csidx inds pos) = do
  inds' <- mapM copyCtPropSubExp inds
  bnd   <- asks $ HM.lookup vnm . envVtable
  cs'   <- copyCtPropCerts cs
  csidx' <- copyCtPropCerts csidx
  case bnd of
    Nothing             -> return $ Index cs' idd csidx' inds' pos
    Just (VarId  id' _) -> changed $ Index cs' (Ident id' tp p) csidx' inds' pos
    Just (Value v@(ArrayVal _ _) _)
      | Just iis <- ctIndex inds',
        length iis == length (arrayShape v),
        Just el <- getArrValInd v iis -> changed $ SubExp $ Constant el pos

    Just (SymArr e' _) ->
      case (e', inds') of
        (Iota _ _, [ii]) -> changed $ SubExp ii

        (Index cs2 aa csidx2 ais _,_) -> do
            inner <- copyCtPropExp (Index (cs'++cs2) aa
                                    (csidx' ++ csidx2)
                                    (ais ++ inds') pos)
            changed inner

        (ArrayLit {}   , _)
          | Just iis <- ctIndex inds',
            Just el <- getArrLitInd e' iis -> changed el

        (Replicate _ (Var vv) _, _:is') -> do
            inner <- if null is'
                     then SubExp <$> copyCtPropSubExp (Var vv)
                     else copyCtPropExp (Index cs' vv csidx' is' pos)
            changed inner

        (Replicate _ (Constant arr@(ArrayVal _ _) _) _, _:is')
          | Just iis <- ctIndex is',
            Just el <- getArrValInd arr iis ->
              changed $ SubExp $ Constant el pos

        (Replicate _ val@(Constant _ _) _, [_]) ->
          changed $ SubExp val

        (Rearrange cs2 perm (Var src) _, _)
          | permuteReach perm < length inds' ->
            let inds'' = permuteDims (take (length inds') perm) inds'
            in changed $ Index (cs'++cs2) src csidx' inds'' pos

        _ -> return $ Index cs' idd csidx' inds' pos

    _ -> return $ Index cs' idd csidx' inds' pos

copyCtPropExp (BinOp bop e1 e2 tp pos) = do
    e1'   <- copyCtPropSubExp e1
    e2'   <- copyCtPropSubExp e2
    ctFoldBinOp (BinOp bop e1' e2' tp pos)

copyCtPropExp (Negate e pos) = do
    e'   <- copyCtPropSubExp e
    if isValue e'
    then case e' of
            Constant (BasicVal (IntVal  v)) _ ->
              changed $ SubExp $ Constant (BasicVal $ IntVal  (-v)) pos
            Constant (BasicVal (RealVal v)) _ ->
              changed $ SubExp $ Constant (BasicVal $ RealVal (0.0-v)) pos
            _ -> badCPropM $ TypeError pos  " ~ operands not of (the same) numeral type! "
    else return $ Negate e' pos

copyCtPropExp (Not e pos) = do
    e'   <- copyCtPropSubExp e
    if isValue e'
    then case e' of
            Constant (BasicVal (LogVal  v)) _ ->
              changed $ SubExp $ Constant (BasicVal $ LogVal (not v)) pos
            _ -> badCPropM $ TypeError pos  " not operands not of (the same) numeral type! "
    else return $ Not e' pos

-----------------------------------------------------------
--- If expression is an array literal than replace it   ---
---    with the array's size
-----------------------------------------------------------
copyCtPropExp (Size cs i e pos) = do
    e' <- copyCtPropSubExp e
    cs' <- copyCtPropCerts cs
    case e' of
      Var idd -> do vv <- asks $ HM.lookup (identName idd) . envVtable
                    case vv of Just (Value a _) -> literal a
                               Just (SymArr (Iota ne _) _)
                                 | i == 0 -> return $ SubExp ne
                               Just (SymArr (Replicate ne _ _) _)
                                 | i == 0 -> return $ SubExp ne
                               _ -> return $ Size cs' i e' pos
      Constant a _ -> literal a
    where literal a =
            case drop i $ arrayShape a of
              []  -> badCPropM $ TypeError pos " array literal has too few dimensions! "
              n:_ -> changed $ SubExp $ Constant (BasicVal $ IntVal n) pos

-----------------------------------------------------------
--- If expression is true then just replace assertion   ---
-----------------------------------------------------------

copyCtPropExp (Assert e loc) = do
  e' <- copyCtPropSubExp e
  case e' of
    Constant (BasicVal (LogVal True)) _ ->
      return $ SubExp $ Constant (BasicVal Checked) loc
    Var idd -> do
      vv <- asks $ HM.lookup (identName idd) . envVtable
      case vv of
        Just (Value (BasicVal (LogVal True)) _) ->
          return $ SubExp $ Constant (BasicVal Checked) loc
        _                                         ->
          return $ Assert e' loc
    _ -> return $ Assert e' loc

copyCtPropExp (Conjoin es loc) = do
  es' <- mapM copyCtPropSubExp es
  -- Remove trivial certificates.
  let check seen (Constant (BasicVal Checked) _) = return seen
      check seen (Var idd) = do
        vv <- asks $ HM.lookup (identName idd) . envVtable
        case vv of
          Just (Value (BasicVal Checked) _) -> changed seen
          Just (SymArr (Conjoin es2 _) _)   -> changed $ seen `S.union` S.fromList es2
          _                                 -> return  $ Var idd `S.insert` seen
      check seen e = return $ e `S.insert` seen
  es'' <- S.toList <$> foldM check S.empty es'
  case es'' of []  -> changed $ SubExp $ Constant (BasicVal Checked) loc
               [c] -> changed $ SubExp c
               _   -> return $ Conjoin es'' loc

copyCtPropExp (Apply fname args tp pos) = do
    args' <- mapM (copyCtPropSubExp . fst) args
    (all_are_vals, vals) <- allArgsAreValues args'
    if all_are_vals
    then do prg <- asks program
            let vv = Interp.runFunNoTrace fname vals  prg
            case vv of
              (Right [v]) -> changed $ SubExp $ Constant v pos
              (Right vs) -> changed $ TupLit (map (`Constant` pos) vs) pos
              _ -> badCPropM $ EnablingOptError pos (" Interpreting fun " ++
                                                     nameToString fname ++ " yields error!")
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
                                  Just (Value v _) -> do
                                    (res, vals) <- allArgsAreValues as
                                    if res then return (True,  v:vals)
                                           else return (False, []    )
                                  _ -> return (False, [])

------------------------------
--- Pattern Match the Rest ---
------------------------------

copyCtPropExp e = mapExpM mapper e
  where mapper = identityMapper {
                   mapOnExp = copyCtPropExp
                 , mapOnBody = copyCtPropBody
                 , mapOnSubExp = copyCtPropSubExp
                 , mapOnLambda = copyCtPropLambda
                 , mapOnIdent = copyCtPropIdent
                 , mapOnCertificates = copyCtPropCerts
                 }

copyCtPropIdent :: Ident -> CPropM Ident
copyCtPropIdent ident@(Ident vnm _ loc) = do
    bnd <- asks $ HM.lookup vnm . envVtable
    case bnd of
      Nothing               -> return ident
      Just (VarId  id' tp1) -> changed $ Ident id' tp1 loc
      _                     -> return ident

copyCtPropCerts :: Certificates -> CPropM Certificates
copyCtPropCerts = liftM (nub . concat) . mapM check
  where check idd = do
          vv <- asks $ HM.lookup (identName idd) . envVtable
          case vv of
            Just (Value (BasicVal Checked) _) -> changed []
            Just (VarId  id' tp1)             -> changed [Ident id' tp1 loc]
            _ -> return [idd]
          where loc = srclocOf idd

copyCtPropLambda :: Lambda -> CPropM Lambda
copyCtPropLambda (Lambda ids body tp loc) = do
    body' <- copyCtPropBody body
    return $ Lambda ids body' tp loc

------------------------------------------------
---- Constant Folding                       ----
------------------------------------------------

binOpRes :: SrcLoc -> BasicValue -> CPropM Exp
binOpRes loc v = changed $ SubExp $ Constant (BasicVal v) loc

ctFoldBinOp :: Exp -> CPropM Exp
ctFoldBinOp e@(BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e2
  | isCt0 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _,  Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1+v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1+v2
      _ -> badCPropM $ TypeError pos  " + operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1-v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1-v2
      _ -> badCPropM $ TypeError pos  " - operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Times e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e1
  | isCt0 e2 = changed $ SubExp e2
  | isCt1 e1 = changed $ SubExp e2
  | isCt1 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1*v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1*v2
      _ -> badCPropM $ TypeError pos  " * operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e1
  | isCt0 e2 = return e -- Division by zero
  | isCt1 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `div` v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1 / v2
      _ -> badCPropM $ TypeError pos  " / operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Mod e1 e2 _ pos)
  | isCt0 e2 = return e -- Division by zero
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `mod` v2
      _ -> badCPropM $ TypeError pos  " % operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Pow e1 e2 _ pos)
  | isCt0 e2 =
    case subExpType e1 of
      Basic Int  -> binOpRes pos $ IntVal 1
      Basic Real -> binOpRes pos $ RealVal 1.0
      _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = changed $ SubExp e1
  |  isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 ^ v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1**v2
      _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftL` v2
      _ -> badCPropM $ TypeError pos  " << operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = changed $ SubExp e1
  | isValue e1, isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftR` v2
      _ -> badCPropM $ TypeError pos  " >> operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Band e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e1
  | isCt0 e2 = changed $ SubExp e2
  | isCt1 e1 = changed $ SubExp e2
  | isCt1 e2 = changed $ SubExp e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .&. v2
      _ -> badCPropM $ TypeError pos  " & operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Bor e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e2
  | isCt0 e2 = changed $ SubExp e1
  | isCt1 e1 = changed $ SubExp e1
  | isCt1 e2 = changed $ SubExp e2
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .|. v2
      _ -> badCPropM $ TypeError pos  " | operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e2
  | isCt0 e2 = changed $ SubExp e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `xor` v2
      _ -> badCPropM $ TypeError pos  " ^ operands not of integer type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp LogAnd e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e1
  | isCt0 e2 = changed $ SubExp e2
  | isCt1 e1 = changed $ SubExp e2
  | isCt1 e2 = changed $ SubExp e1
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 && v2
      _ -> badCPropM $ TypeError pos  " && operands not of boolean type! "
  | otherwise = return e
ctFoldBinOp e@(BinOp LogOr e1 e2 _ pos)
  | isCt0 e1 = changed $ SubExp e2
  | isCt0 e2 = changed $ SubExp e1
  | isCt1 e1 = changed $ SubExp e1
  | isCt1 e2 = changed $ SubExp e2
  | isValue e1 && isValue e2 =
    case (e1, e2) of
      (Constant (BasicVal (LogVal v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 || v2
      _ -> badCPropM $ TypeError pos  " || operands not of boolean type! "
  | otherwise = return e

ctFoldBinOp e@(BinOp Equal e1 e2 _ pos)
  | isValue e1 && isValue e2 =
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
        (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
          binOpRes pos $ LogVal $ v1==v2
        (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
          binOpRes pos $ LogVal $ v1==v2
        (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
          binOpRes pos $ LogVal $ v1==v2
        (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
          binOpRes pos $ LogVal $ v1==v2
        _ -> badCPropM $ TypeError pos  " equal operands not of (the same) basic type! "
  | e1 == e2 = binOpRes pos $ LogVal True
  | otherwise = return e
ctFoldBinOp e@(BinOp Less e1 e2 _ pos) =
    if isValue e1 && isValue e2 then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
          binOpRes pos $ LogVal $ v1<v2
        (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
          binOpRes pos $ LogVal $ v1<v2
        (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
          binOpRes pos $ LogVal $ v1<v2
        (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
          binOpRes pos $ LogVal $ v1<v2
        _ -> badCPropM $ TypeError pos  " less-than operands not of (the same) basic type! "
    else return e
ctFoldBinOp e@(BinOp Leq e1 e2 _ pos) =
    if isValue e1 && isValue e2 then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Constant (BasicVal (IntVal  v1)) _, Constant (BasicVal (IntVal  v2)) _) ->
          binOpRes pos $ LogVal $ v1<=v2
        (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
          binOpRes pos $ LogVal $ v1<=v2
        (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal  v2)) _) ->
          binOpRes pos $ LogVal $ v1<=v2
        (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2 )) _) ->
          binOpRes pos $ LogVal $ v1<=v2
        _ -> badCPropM $ TypeError pos  " less-than-or-equal operands not of (the same) basic type! "
    else return e
ctFoldBinOp e = return e



----------------------------------------------------
---- Helpers for Constant Folding                ---
----------------------------------------------------


isValue :: SubExp -> Bool
isValue e = case e of
              Constant _ _ -> True
              _            -> False

isCt1 :: SubExp -> Bool
isCt1 (Constant (BasicVal (IntVal x))  _) = x == 1
isCt1 (Constant (BasicVal (RealVal x)) _) = x == 1
isCt1 (Constant (BasicVal (LogVal x))  _) = x
isCt1 _                                   = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (BasicVal (IntVal x))  _) = x == 0
isCt0 (Constant (BasicVal (RealVal x)) _) = x == 0
isCt0 (Constant (BasicVal (LogVal x))  _) = not x
isCt0 _                                   = False

----------------------------------------------------
---- Helpers for Constant/Copy Propagation       ---
----------------------------------------------------

isBasicTypeVal :: Value -> Bool
isBasicTypeVal = basicType . valueType

getPropBnds :: [Ident] -> Exp -> [(VName, CtOrId)]
getPropBnds [ident@(Ident var tp _)] e =
  case e of
    SubExp (Constant v _) -> [(var, Value v (fromDecl $ valueType v))]
    SubExp (Var v)        -> [(var, VarId  (identName v) (identType v))]
    Index   {}            -> [(var, SymArr e   tp)]
    TupLit  [e'] _        -> getPropBnds [ident] $ SubExp e'
    Rearrange   {}        -> [(var, SymArr e   tp)]
    Reshape   {}          -> [(var, SymArr e   tp)]
    Conjoin {}            -> [(var, SymArr e   tp)]

    Iota {}               -> let newtp = Array Int [Nothing] Nonunique mempty
                             in  [(var, SymArr e newtp)]
    Replicate {}          -> [(var, SymArr e tp)]
    ArrayLit  {}          -> [(var, SymArr e tp)]
    _                     -> []
getPropBnds ids (TupLit ts _)
  | length ids == length ts =
    concatMap (\(x,y)-> getPropBnds [x] (SubExp y)) $ zip ids ts
getPropBnds _ _ = []

ctIndex :: [SubExp] -> Maybe [Int]
ctIndex [] = Just []
ctIndex (Constant (BasicVal (IntVal ii)) _:is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

getArrValInd :: Value -> [Int] -> Maybe Value
getArrValInd v [] = if isBasicTypeVal v then Just v else Nothing
getArrValInd (ArrayVal arr _) (i:is) = getArrValInd (arr ! i) is
getArrValInd _ _ = Nothing

getArrLitInd :: Exp -> [Int] -> Maybe Exp
getArrLitInd e [] = Just e
getArrLitInd (ArrayLit els _ _) (i:is) = getArrLitInd (SubExp $ els !! i) is
getArrLitInd (SubExp (Constant arr@(ArrayVal _ _) loc)) (i:is) =
  case getArrValInd arr (i:is) of
    Nothing -> Nothing
    Just v  -> Just $ SubExp $ Constant v loc
getArrLitInd _ _ = Nothing
