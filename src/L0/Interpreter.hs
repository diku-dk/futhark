{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module L0.Interpreter
  ( runProg
  , runProgIO
  , runFun
  , InterpreterError(..) )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error

import Data.Array
import Data.Bits
import Data.List
import Data.Loc
import qualified Data.Map as M

import System.IO (hFlush, stdout)

import L0.AbSyn
import L0.Parser

data InterpreterError = MissingMainFunction
                      | IndexOutOfBounds SrcLoc Int Int
                      -- ^ First @Int@ is array size, second is attempted index.
                      | NegativeIota SrcLoc Int
                      | NegativeReplicate SrcLoc Int
                      | ReadError SrcLoc Type String
                      | InvalidArrayShape SrcLoc [Int] [Int]
                      -- ^ First @Int@ is old shape, second is attempted new shape.
                      | TypeError SrcLoc String

instance Show InterpreterError where
  show MissingMainFunction =
    "No main function defined."
  show (IndexOutOfBounds pos arrsz i) =
    "Array index " ++ show i ++ " out of bounds of array size " ++ show arrsz ++ " at " ++ locStr pos ++ "."
  show (NegativeIota pos n) =
    "Argument " ++ show n ++ " to iota at " ++ locStr pos ++ " is negative."
  show (NegativeReplicate pos n) =
    "Argument " ++ show n ++ " to replicate at " ++ locStr pos ++ " is negative."
  show (TypeError pos s) =
    "Type error at " ++ locStr pos ++ " in " ++ s ++ " during interpretation.  This implies a bug in the type checker."
  show (ReadError pos t s) =
    "Read error while trying to read " ++ ppType t ++ " at " ++ locStr pos ++ ".  Input line was: " ++ s
  show (InvalidArrayShape pos shape newshape) =
    "Invalid array reshaping at " ++ locStr pos ++ ", from " ++ show shape ++ " to " ++ show newshape

instance Error InterpreterError where
  strMsg = TypeError noLoc

data L0Env m = L0Env { envVtable  :: M.Map String Value
                     , envFtable  :: M.Map String ([Value] -> L0M m Value)
                     , envWriteOp :: String -> L0M m ()
                     , envReadOp  :: L0M m String }

newtype L0M m a = L0M (ReaderT (L0Env m) (ErrorT InterpreterError m) a)
  deriving (MonadReader (L0Env m), Monad, Applicative, Functor)

runL0M :: L0M m a -> L0Env m -> m (Either InterpreterError a)
runL0M (L0M m) env = runErrorT (runReaderT m env)

bad :: Monad m => InterpreterError -> L0M m a
bad = L0M . throwError

bindVar :: L0Env m -> (Ident Type, Value) -> L0Env m
bindVar env (Ident name _ _,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: L0Env m -> [(Ident Type, Value)] -> L0Env m
bindVars = foldl bindVar

binding :: Monad m => [(Ident Type, Value)] -> L0M m a -> L0M m a
binding bnds = local (`bindVars` bnds)

lookupVar :: Monad m => String -> L0M m Value
lookupVar vname = do val <- asks $ M.lookup vname . envVtable
                     case val of Just val' -> return val'
                                 Nothing   -> bad $ TypeError noLoc $ "lookupVar " ++ vname

lookupFun :: Monad m => String -> L0M m ([Value] -> L0M m Value)
lookupFun fname = do fun <- asks $ M.lookup fname . envFtable
                     case fun of Just fun' -> return fun'
                                 Nothing   -> bad $ TypeError noLoc $ "lookupFun " ++ fname

arrToList :: Monad m => Value -> L0M m [Value]
arrToList (ArrayVal l _ _) = return $ elems l
arrToList v = bad $ TypeError (srclocOf v) "arrToList"

tupToList :: Monad m => Value -> L0M m [Value]
tupToList (TupVal l _) = return l
tupToList v = bad $ TypeError (srclocOf v) "tupToList"    


runProgIO :: Prog Type -> IO (Either InterpreterError Value)
runProgIO = runProg putStr (hFlush stdout >> getLine)

runProg :: (Applicative m, Monad m) => (String -> m ()) -> m String
        -> Prog Type -> m (Either InterpreterError Value)
runProg wop rop prog = do
  let ftable = foldl expand builtins prog
      l0env = L0Env { envVtable = M.empty
                    , envFtable = ftable
                    , envWriteOp = L0M . lift . lift . wop
                    , envReadOp = L0M $ lift $ lift rop }
      runmain = case M.lookup "main" ftable of
                  Nothing -> bad MissingMainFunction
                  Just mainfun -> mainfun []
  runL0M runmain l0env
  where
    -- We assume that the program already passed the type checker, so
    -- we don't check for duplicate definitions or anything.
    expand ftable (name,_,params,body,_) =
        let fun args = binding (zip params args) $ evalExp body
        in M.insert name fun ftable


--------------------------------------------------
------- Interpreting an arbitrary function -------
--------------------------------------------------

runFun :: String -> [Value] -> Prog Type -> Maybe (Either InterpreterError Value)
runFun fname aargs prog = do
    let wop = const Nothing
    let rop = Nothing
    let ftable = foldl expand builtins prog
        l0env = L0Env { envVtable  = M.empty
                      , envFtable  = ftable
                      , envWriteOp = L0M . lift . lift . wop
                      , envReadOp  = L0M $ lift $ lift rop }
        runfun  = case M.lookup fname ftable of
                    Nothing -> bad MissingMainFunction
                    Just curfun -> curfun aargs
    runL0M runfun l0env
  where
    expand ftable (name,_,params,body,_) =
        let fun args = binding (zip params args) $ evalExp body
        in M.insert name fun ftable


--------------------------------------------
--------------------------------------------
------------- BUILTIN FUNCTIONS ------------
--------------------------------------------
--------------------------------------------

builtins :: (Applicative m, Monad m) => M.Map String ([Value] -> L0M m Value)
builtins = M.fromList [("toReal", builtin "toReal")
                      ,("trunc", builtin "trunc")
                      ,("sqrt", builtin "sqrt")
                      ,("log", builtin "log")
                      ,("exp", builtin "exp")
                      ,("op not", builtin "op not")
                      ,("op ~", builtin "op ~")]

builtin :: (Applicative m, Monad m) => String -> [Value] -> L0M m Value
builtin "toReal" [IntVal x pos] = return $ RealVal (fromIntegral x) pos
builtin "trunc" [RealVal x pos] = return $ IntVal (truncate x) pos
builtin "sqrt" [RealVal x pos] = return $ RealVal (sqrt x) pos
builtin "log" [RealVal x pos] = return $ RealVal (log x) pos
builtin "exp" [RealVal x pos] = return $ RealVal (exp x) pos
builtin "op not" [LogVal b pos] = return $ LogVal (not b) pos
builtin "op ~" [RealVal b pos] = return $ RealVal (-b) pos
builtin fname _ = bad $ TypeError noLoc $ "Builtin " ++ fname

--------------------------------------------
--------------------------------------------
--------------------------------------------
--------------------------------------------


evalExp :: (Applicative m, Monad m) => Exp Type -> L0M m Value
evalExp (Literal val) = return val
evalExp (TupLit es pos) =
  TupVal <$> mapM evalExp es <*> pure pos
evalExp (ArrayLit es t pos) =
  arrayVal <$> mapM evalExp es <*> pure t <*> pure pos
evalExp (BinOp Plus e1 e2 (Int _) pos) = evalIntBinOp (+) e1 e2 pos
evalExp (BinOp Plus e1 e2 (Real _) pos) = evalRealBinOp (+) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Int _) pos) = evalIntBinOp (-) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Real _) pos) = evalRealBinOp (-) e1 e2 pos
evalExp (BinOp Pow e1 e2 (Int _) pos) = evalIntBinOp (^) e1 e2 pos
evalExp (BinOp Pow e1 e2 (Real _) pos) = evalRealBinOp (**) e1 e2 pos
evalExp (BinOp Times e1 e2 (Int _) pos) = evalIntBinOp (*) e1 e2 pos
evalExp (BinOp Times e1 e2 (Real _) pos) = evalRealBinOp (*) e1 e2 pos
evalExp (BinOp Divide e1 e2 (Int _) pos) = evalIntBinOp div e1 e2 pos
evalExp (BinOp Divide e1 e2 (Real _) pos) = evalRealBinOp (/) e1 e2 pos
evalExp (BinOp ShiftR e1 e2 _ pos) = evalIntBinOp shiftR e1 e2 pos
evalExp (BinOp ShiftL e1 e2 _ pos) = evalIntBinOp shiftL e1 e2 pos
evalExp (BinOp Band e1 e2 _ pos) = evalIntBinOp (.&.) e1 e2 pos
evalExp (BinOp Xor e1 e2 _ pos) = evalIntBinOp xor e1 e2 pos
evalExp (BinOp Bor e1 e2 _ pos) = evalIntBinOp (.|.) e1 e2 pos
evalExp (BinOp LogAnd e1 e2 _ pos) = evalBoolBinOp (&&) e1 e2 pos
evalExp (BinOp LogOr e1 e2 _ pos) = evalBoolBinOp (||) e1 e2 pos
evalExp (BinOp Equal e1 e2 _ pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1==v2) pos
evalExp (BinOp Less e1 e2 _ pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<v2) pos
evalExp (BinOp Leq e1 e2 _ pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<=v2) pos
evalExp (BinOp _ _ _ _ pos) = bad $ TypeError pos "evalExp Binop"
evalExp (And e1 e2 pos) = do
  v1 <- evalExp e1
  case v1 of LogVal True  _ -> evalExp e2
             LogVal False _ -> return v1
             _              -> bad $ TypeError pos "evalExp And"
evalExp (Or e1 e2 pos) = do
  v1 <- evalExp e1
  case v1 of LogVal True  _ -> return v1
             LogVal False _ -> evalExp e2
             _              -> bad $ TypeError pos "evalExp Or"
evalExp (Not e pos) = do
  v <- evalExp e
  case v of LogVal b _   -> return $ LogVal (not b) pos
            _            -> bad $ TypeError pos "evalExp Not"
evalExp (Negate e _ pos) = do
  v <- evalExp e
  case v of IntVal x _    -> return $ IntVal (-x) pos
            RealVal x _   -> return $ RealVal (-x) pos
            _             -> bad $ TypeError pos "evalExp Negate"
evalExp (If e1 e2 e3 _ pos) = do
  v <- evalExp e1
  case v of LogVal True _  -> evalExp e2
            LogVal False _ -> evalExp e3
            _              -> bad $ TypeError pos "evalExp If"
evalExp (Var (Ident name _ _)) =
  lookupVar name
evalExp (Apply fname args _ _) = do
  fun <- lookupFun fname
  args' <- mapM evalExp args
  fun args'
evalExp (LetPat pat e body pos) = do
  v <- evalExp e
  case evalPattern pat v of
    Nothing   -> bad $ TypeError pos "evalExp Let pat"
    Just bnds -> local (`bindVars` bnds) $ evalExp body
evalExp (LetWith name src idxs ve body pos) = do
  v <- lookupVar $ identName src
  idxs' <- mapM evalExp idxs
  vev <- evalExp ve
  v' <- change v idxs' vev
  binding [(name, v')] $ evalExp body
  where change _ [] to = return to
        change (ArrayVal arr t _) (IntVal i _:rest) to
          | i >= 0 && i <= upper = do
            let x = arr ! i
            x' <- change x rest to
            return $ ArrayVal (arr // [(i, x')]) t pos
          | otherwise = bad $ IndexOutOfBounds pos (upper+1) i
          where upper = snd $ bounds arr
        change _ _ _ = bad $ TypeError pos "evalExp Let Id"
evalExp (Index (Ident name _ _) idxs _ _ pos) = do
  v <- lookupVar name
  idxs' <- mapM evalExp idxs
  foldM idx v idxs'
  where idx (ArrayVal arr _ _) (IntVal i _)
          | i >= 0 && i <= upper = return $ arr ! i
          | otherwise             = bad $ IndexOutOfBounds pos (upper+1) i
          where upper = snd $ bounds arr
        idx _ _ = bad $ TypeError pos "evalExp Index"
evalExp (Iota e pos) = do
  v <- evalExp e
  case v of
    IntVal x _
      | x >= 0    -> return $ arrayVal (map (`IntVal` pos) [0..x-1]) (Int pos) pos
      | otherwise -> bad $ NegativeIota pos x
    _ -> bad $ TypeError pos "evalExp Iota"
evalExp (Size e pos) = do
  v <- evalExp e
  case v of
    ArrayVal arr _ _ -> let (lower, upper) = bounds arr
                        in return $ IntVal (upper - lower + 1) pos
    _ -> bad $ TypeError pos "evalExp Size"
evalExp (Replicate e1 e2 pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v1 of
    IntVal x _
      | x >= 0    -> return $ ArrayVal (listArray (0,x-1) $ repeat v2) (valueType v2) pos
      | otherwise -> bad $ NegativeReplicate pos x
    _   -> bad $ TypeError pos "evalExp Replicate"
evalExp (Reshape shapeexp arrexp _ outtype pos) = do
  shape <- mapM (asInt <=< evalExp) shapeexp
  arr <- evalExp arrexp
  let reshape (Array t _ _) (n:rest) vs
        | length vs `mod` n == 0 =
          arrayVal <$> mapM (reshape t rest) (chunk (length vs `div` n) vs)
                   <*> pure t <*> pure pos
        | otherwise = bad $ InvalidArrayShape pos (arrayShape arr) shape
      reshape _ [] [v] = return v
      reshape _ _ _ = bad $ TypeError pos "evalExp Reshape reshape"
  reshape outtype shape $ flatten arr
  where flatten (ArrayVal arr _ _) = concatMap flatten $ elems arr
        flatten t = [t]
        chunk _ [] = []
        chunk i l = let (a,b) = splitAt i l
                    in a : chunk i b
        asInt (IntVal x _) = return x
        asInt _ = bad $ TypeError pos "evalExp Reshape int"
evalExp (Transpose arrexp _ _ pos) = do
  v <- evalExp arrexp
  case v of
    ArrayVal inarr (Array et _ _) _ -> do
      let arr el = arrayVal el et pos
      els' <- map arr <$> transpose <$> mapM arrToList (elems inarr)
      return $ arrayVal els' (Array et Nothing pos) pos
    _ -> bad $ TypeError pos "evalExp Transpose"
evalExp (Map fun e _ outtype pos) = do
  vs <- arrToList =<< evalExp e
  vs' <- mapM (applyLambda fun . (:[])) vs
  return $ arrayVal vs' outtype pos
evalExp (Reduce fun accexp arrexp _ _) = do
  startacc <- evalExp accexp
  vs <- arrToList =<< evalExp arrexp
  let foldfun acc x = applyLambda fun [acc, x]
  foldM foldfun startacc vs
evalExp (Zip arrexps pos) = do
  arrs <- mapM ((arrToList <=< evalExp) . fst) arrexps
  arrayVal <$> zipit arrs <*> pure (Tuple (map snd arrexps) pos) <*> pure pos
  where split []     = Nothing
        split (x:xs) = Just (x, xs)
        zipit ls = case unzip <$> mapM split ls of
                     Just (hds, tls) -> do
                       let el = TupVal hds pos
                       ls' <- zipit tls
                       return $ el : ls'
                     Nothing -> return []
evalExp (Unzip e ts pos) = do
  arr <- mapM tupToList =<< arrToList =<< evalExp e
  return $ TupVal (zipWith (\vs t -> arrayVal vs t pos) (transpose arr) ts) pos
-- scan * e {x1,..,xn} = {e*x1, e*x1*x2, ..., e*x1*x2*...*xn}
-- we can change this definition of scan if deemed not suitable
evalExp (Scan fun startexp arrexp _ pos) = do
  startval <- evalExp startexp
  vals <- arrToList =<< evalExp arrexp
  (acc, vals') <- foldM scanfun (startval, []) vals
  return $ arrayVal (reverse vals') (valueType acc) pos
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun [acc, x]
            return (acc', acc' : l)
evalExp (Filter fun arrexp outtype pos) = do
  vs <- filterM filt =<< arrToList =<< evalExp arrexp
  return $ arrayVal vs outtype pos
  where filt x = do res <- applyLambda fun [x]
                    case res of (LogVal True _) -> return True
                                _               -> return False
evalExp (Mapall fun arrexp _ outtype pos) =
  mapall outtype =<< evalExp arrexp
    where mapall t@(Array et _ _) (ArrayVal arr _ _) = do
            els' <- mapM (mapall et) $ elems arr
            return $ arrayVal els' t pos
          mapall _ v = applyLambda fun [v]
evalExp (Redomap redfun mapfun accexp arrexp _ _ _) = do
  startacc <- evalExp accexp
  vs <- arrToList =<< evalExp arrexp
  vs' <- mapM (applyLambda mapfun . (:[])) vs
  let foldfun acc x = applyLambda redfun [acc, x]
  foldM foldfun startacc vs'
evalExp (Split splitexp arrexp intype pos) = do
  split <- evalExp splitexp
  vs <- arrToList =<< evalExp arrexp
  case split of
    IntVal i _
      | i <= length vs ->
        let (bef,aft) = splitAt i vs
        in return $ TupVal [arrayVal bef intype pos, arrayVal aft intype pos] pos
      | otherwise        -> bad $ IndexOutOfBounds pos (length vs) i
    _ -> bad $ TypeError pos "evalExp Split"
evalExp (Concat arr1exp arr2exp intype pos) = do
  elems1 <- arrToList =<< evalExp arr1exp
  elems2 <- arrToList =<< evalExp arr2exp
  return $ arrayVal (elems1 ++ elems2) intype pos
evalExp (Copy e _) = evalExp e
evalExp (Read t pos) = do
  s <- join $ asks envReadOp
  case liftM check $ parsefun "input" s of
    Right (Just v) -> return v
    _ -> bad $ ReadError pos t s
  where check v
          | valueType v == t = Just v
          | otherwise        = Nothing
        parsefun = case t of Int  _             -> parseInt
                             Real _             -> parseReal
                             Bool _             -> parseBool
                             Char _             -> parseChar
                             Array (Char _) _ _ -> parseString
                             Array {}           -> parseArray
                             Tuple {}           -> parseTuple
evalExp (Write e _ _) = do
  v <- evalExp e
  join $ asks envWriteOp <*> pure (write v)
  return v
  where write (CharVal c _) = [c]
        write (ArrayVal arr _ _)
          | Just s <- mapM char $ elems arr = s
          where char (CharVal c _) = Just c
                char _             = Nothing
        write v = ppValue v
evalExp (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody pos) = do
  bound <- evalExp boundexp
  mergestart <- evalExp mergeexp
  case bound of
    IntVal n _ -> do loopresult <- foldM iteration mergestart [0..n-1]
                     evalExp $ LetPat mergepat (Literal loopresult) letbody pos
    _ -> bad $ TypeError pos "evalExp DoLoop"
  where iteration mergeval i =
          binding [(loopvar, IntVal i pos)] $
            evalExp $ LetPat mergepat (Literal mergeval) loopbody pos

evalIntBinOp :: (Applicative m, Monad m) =>
                (Int -> Int -> Int) -> Exp Type -> Exp Type -> SrcLoc -> L0M m Value
evalIntBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (IntVal x _, IntVal y _) -> return $ IntVal (op x y) pos
    _                        -> bad $ TypeError pos "evalIntBinOp"

evalRealBinOp :: (Applicative m, Monad m) =>
                 (Double -> Double -> Double) -> Exp Type -> Exp Type -> SrcLoc -> L0M m Value
evalRealBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (RealVal x _, RealVal y _) -> return $ RealVal (op x y) pos
    _                          -> bad $ TypeError pos $ "evalRealBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalBoolBinOp :: (Applicative m, Monad m) =>
                 (Bool -> Bool -> Bool) -> Exp Type -> Exp Type -> SrcLoc -> L0M m Value
evalBoolBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (LogVal x _, LogVal y _) -> return $ LogVal (op x y) pos
    _                        -> bad $ TypeError pos $ "evalBoolBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalPattern :: TupIdent Type -> Value -> Maybe [(Ident Type, Value)]
evalPattern (Id ident) v = Just [(ident, v)]
evalPattern (TupId pats _) (TupVal vs _)
  | length pats == length vs =
    concat <$> zipWithM evalPattern pats vs
evalPattern _ _ = Nothing

applyLambda :: (Applicative m, Monad m) => Lambda Type -> [Value] -> L0M m Value
applyLambda (AnonymFun params body _ _) args =
  binding (zip params args) $ evalExp body
applyLambda (CurryFun name curryargs _ _) args = do
  curryargs' <- mapM evalExp curryargs
  fun <- lookupFun name
  fun $ curryargs' ++ args
