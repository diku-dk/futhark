{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module L0.Interpreter
  ( runProg
  , runProgIO
  , InterpreterError(..) )
where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Either

import Data.Bits
import Data.List
import qualified Data.Map as M

import System.IO (hFlush, stdout)

import L0.AbSyn
import L0.Parser

data InterpreterError = MissingMainFunction
                      | IndexOutOfBounds Pos Int Int
                      -- ^ First @Int@ is array size, second is attempted index.
                      | NegativeIota Pos Int
                      | NegativeReplicate Pos Int
                      | ReadError Pos Type String
                      | InvalidArrayShape Pos [Int] [Int]
                      -- ^ First @Int@ is old shape, second is attempted new shape.
                      | TypeError Pos String

instance Show InterpreterError where
  show MissingMainFunction =
    "No main function defined."
  show (IndexOutOfBounds pos arrsz i) =
    "Array index " ++ show i ++ " out of bounds of array size " ++ show arrsz ++ " at " ++ posStr pos ++ "."
  show (NegativeIota pos n) =
    "Argument " ++ show n ++ " to iota at " ++ posStr pos ++ " is negative."
  show (NegativeReplicate pos n) =
    "Argument " ++ show n ++ " to replicate at " ++ posStr pos ++ " is negative."
  show (TypeError pos s) =
    "Type error at " ++ posStr pos ++ " in " ++ s ++ " during interpretation.  This implies a bug in the type checker."
  show (ReadError pos t s) =
    "Read error while trying to read " ++ ppType t ++ " at " ++ posStr pos ++ ".  Input line was: " ++ s
  show (InvalidArrayShape pos shape newshape) =
    "Invalid array reshaping at " ++ posStr pos ++ ", from " ++ show shape ++ " to " ++ show newshape


data L0Env m = L0Env { envVtable  :: M.Map String Value
                     , envFtable  :: M.Map String ([Value] -> L0M m Value)
                     , envWriteOp :: String -> L0M m ()
                     , envReadOp  :: L0M m String }

newtype L0M m a = L0M (ReaderT (L0Env m) (EitherT InterpreterError m) a)
  deriving (MonadReader (L0Env m), Monad, Applicative, Functor)

runL0M :: L0M m a -> L0Env m -> m (Either InterpreterError a)
runL0M (L0M m) env = runEitherT (runReaderT m env)

bad :: Monad m => InterpreterError -> L0M m a
bad = L0M . lift . left

bindVar :: L0Env m -> (String, Value) -> L0Env m
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: L0Env m -> [(String, Value)] -> L0Env m
bindVars = foldl bindVar

binding :: Monad m => [(String, Value)] -> L0M m a -> L0M m a
binding bnds = local (`bindVars` bnds)

lookupVar :: Monad m => String -> L0M m Value
lookupVar fname = do val <- asks $ M.lookup fname . envVtable
                     case val of Just val' -> return val'
                                 Nothing   -> error "wtf"  -- bad $ TypeError (0,0)

lookupFun :: Monad m => String -> L0M m ([Value] -> L0M m Value)
lookupFun fname = do fun <- asks $ M.lookup fname . envFtable
                     case fun of Just fun' -> return fun'
                                 Nothing   -> error $ "fun " ++ fname -- bad $ TypeError (0,0)

arrToList :: Monad m => Value -> L0M m [Value]
arrToList (ArrayVal l _ _) = return l
arrToList _ = bad $ TypeError (0,0) "arrToList"

runProgIO :: Prog Identity -> IO (Either InterpreterError Value)
runProgIO = runProg putStr (hFlush stdout >> getLine)

runProg :: (Applicative m, Monad m) => (String -> m ()) -> m String
        -> Prog Identity -> m (Either InterpreterError Value)
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
        let fun args = binding (zip (map fst params) args) $ evalExp body
        in M.insert name fun ftable
    builtins = M.fromList [("toReal", builtin "toReal")
                          ,("sqrt", builtin "sqrt")
                          ,("log", builtin "log")
                          ,("exp", builtin "exp")
                          ,("op not", builtin "op not")
                          ,("op ~", builtin "op ~")]
    builtin "toReal" [IntVal x pos] = return $ RealVal (fromIntegral x) pos
    builtin "sqrt" [RealVal x pos] = return $ RealVal (sqrt x) pos
    builtin "log" [RealVal x pos] = return $ RealVal (log x) pos
    builtin "exp" [RealVal x pos] = return $ RealVal (exp x) pos
    builtin "op not" [LogVal b pos] = return $ LogVal (not b) pos
    builtin "op ~" [RealVal b pos] = return $ RealVal (-b) pos
    builtin fname _ = bad $ TypeError (0,0) $ "Builtin " ++ fname

evalExp :: (Applicative m, Monad m) => Exp Identity -> L0M m Value
evalExp (Literal val) = return val
evalExp (TupLit es _ pos) =
  TupVal <$> mapM evalExp es <*> pure pos
evalExp (ArrayLit es (Identity t) pos) =
  ArrayVal <$> mapM evalExp es <*> pure t <*> pure pos
evalExp (BinOp Plus e1 e2 (Identity (Int _)) pos) = evalIntBinOp (+) e1 e2 pos
evalExp (BinOp Plus e1 e2 (Identity (Real _)) pos) = evalRealBinOp (+) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Identity (Int _)) pos) = evalIntBinOp (-) e1 e2 pos
evalExp (BinOp Minus e1 e2 (Identity (Real _)) pos) = evalRealBinOp (-) e1 e2 pos
evalExp (BinOp Pow e1 e2 (Identity (Int _)) pos) = evalIntBinOp (^) e1 e2 pos
evalExp (BinOp Pow e1 e2 (Identity (Real _)) pos) = evalRealBinOp (**) e1 e2 pos
evalExp (BinOp Times e1 e2 (Identity (Int _)) pos) = evalIntBinOp (*) e1 e2 pos
evalExp (BinOp Times e1 e2 (Identity (Real _)) pos) = evalRealBinOp (*) e1 e2 pos
evalExp (BinOp Divide e1 e2 (Identity (Int _)) pos) = evalIntBinOp div e1 e2 pos
evalExp (BinOp Divide e1 e2 (Identity (Real _)) pos) = evalRealBinOp (/) e1 e2 pos
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
evalExp (Var name _ _) =
  lookupVar name
evalExp (Apply fname args _ _) = do
  fun <- lookupFun fname
  args' <- mapM evalExp args
  fun args'
evalExp (Let pat e Nothing Nothing body pos) = do
  v <- evalExp e
  case evalPattern pat v of
    Nothing   -> bad $ TypeError pos "evalExp Let pat"
    Just bnds -> local (`bindVars` bnds) $ evalExp body
evalExp (Let (Id name _) e (Just idxs) (Just ve) body pos) = do
  v <- evalExp e
  idxs' <- mapM evalExp idxs
  vev <- evalExp ve
  v' <- change v idxs' vev
  binding [(name, v')] $ evalExp body
  where change _ [] to = return to
        change (ArrayVal vs t _) (IntVal i _:rest) to = 
          case splitAt i vs of
            (_, []) -> bad $ IndexOutOfBounds pos (length vs) i
            (bef, x:aft) -> do
              x' <- change x rest to
              return $ ArrayVal (bef++x':aft) t pos
        change _g _ _ = bad $ TypeError pos "evalExp Let Id"
evalExp (Let _ _ _ _ _ pos) = bad $ TypeError pos "evalExp _"
evalExp (Index name idxs _ _ pos) = do
  v <- lookupVar name
  idxs' <- mapM evalExp idxs
  foldM index v idxs'
  where index (ArrayVal vs _ _) (IntVal i _)
          | i < length vs = return $ vs !! i
          | otherwise     = bad $ IndexOutOfBounds pos (length vs) i
        index _ _ = bad $ TypeError pos "evalExp Index"
evalExp (Iota e pos) = do
  v <- evalExp e
  case v of
    IntVal x _
      | x >= 0    -> return $ ArrayVal (map (`IntVal` pos) [0..x-1]) (Int pos) pos
      | otherwise -> bad $ NegativeIota pos x
    _ -> bad $ TypeError pos "evalExp Iota"
evalExp (Replicate e1 e2 _ pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v1 of
    IntVal x _
      | x >= 0    -> return $ ArrayVal (replicate x v2) (Int pos) pos
      | otherwise -> bad $ NegativeReplicate pos x
    _ -> bad $ TypeError pos "evalExp Replicate"
evalExp (Reshape shapeexp arrexp _ (Identity outtype) pos) = do
  shape <- mapM (asInt <=< evalExp) shapeexp
  arr <- evalExp arrexp
  let reshape (Array t _ _) (n:rest) vs
        | length vs `mod` n == 0 =
          ArrayVal <$> mapM (reshape t rest) (chunk (length vs `div` n) vs)
                   <*> pure t <*> pure pos
        | otherwise = bad $ InvalidArrayShape pos (arrayShape arr) shape
      reshape _ [] [v] = return v
      reshape _ _ _ = bad $ TypeError pos "evalExp Reshape reshape"
  reshape outtype shape $ flatten arr
  where flatten (ArrayVal vs _ _) = concatMap flatten vs
        flatten t = [t]
        chunk _ [] = []
        chunk i l = let (a,b) = splitAt i l
                    in a : chunk i b
        asInt (IntVal x _) = return x
        asInt _ = bad $ TypeError pos "evalExp Reshape int"
evalExp (Transpose arrexp _ _ pos) = do
  v <- evalExp arrexp
  case v of
    ArrayVal els (Array et _ _) _ -> do
      let arr el = ArrayVal el et pos
      els' <- map arr <$> transpose <$> mapM elems els
      return $ ArrayVal els' (Array et Nothing pos) pos
    _ -> bad $ TypeError pos "evalExp Transpose"
  where elems (ArrayVal els _ _) = return els
        elems _ = bad $ TypeError pos "evalExp Transpose"
evalExp (Map fun e _ (Identity outtype) pos) = do
  elems <- arrToList =<< evalExp e
  case outtype of
    (Array t _ _) -> do
      elems' <- mapM (applyLambda fun . (:[])) elems
      return $ ArrayVal elems' t pos
    _ -> bad $ TypeError pos "evalExp Map"
evalExp (Reduce fun accexp arrexp _ _) = do
  startacc <- evalExp accexp
  elems <- arrToList =<< evalExp arrexp
  let foldfun acc x = applyLambda fun [acc, x]
  foldM foldfun startacc elems
evalExp (ZipWith fun arrexps _ (Identity outtype) pos) = do
  arrs <- mapM (arrToList <=< evalExp) arrexps
  ArrayVal <$> zipit arrs <*> pure outtype <*> pure pos
  where split []     = Nothing
        split (x:xs) = Just (x, xs)
        zipit ls = case unzip <$> mapM split ls of
                     Just (hds, tls) -> do
                       el <- applyLambda fun hds
                       ls' <- zipit tls
                       return $ el : ls'
                     Nothing -> return []

-- scan * e {x1,..,xn} = {e*x1, e*x1*x2, ..., e*x1*x2*...*xn}
-- we can change this definition of scan
evalExp (Scan fun startexp arrexp _ pos) = do
  startval <- evalExp startexp
  vals <- arrToList =<< evalExp arrexp
  (acc, vals') <- foldM scanfun (startval, []) vals
  return $ ArrayVal (reverse vals') (valueType acc) pos
    where scanfun (acc, l) x = do
            acc' <- applyLambda fun [acc, x]
            return (acc', acc' : l)
evalExp (Filter fun arrexp (Identity outtype) pos) = do
  elems <- filterM filt =<< arrToList =<< evalExp arrexp
  return $ ArrayVal elems outtype pos
  where filt x = do res <- applyLambda fun [x]
                    case res of (LogVal True _) -> return True
                                _               -> return False
evalExp (Mapall fun arrexp _ (Identity outtype) pos) =
  mapall outtype =<< evalExp arrexp
    where mapall t@(Array et _ _) (ArrayVal els _ _) = do
            els' <- mapM (mapall et) els
            return $ ArrayVal els' t pos
          mapall _ v = applyLambda fun [v]
evalExp (Redomap redfun mapfun accexp arrexp _ _ _) = do
  startacc <- evalExp accexp
  elems <- arrToList =<< evalExp arrexp
  elems' <- mapM (applyLambda mapfun . (:[])) elems
  let foldfun acc x = applyLambda redfun [acc, x]
  foldM foldfun startacc elems'
evalExp (Split splitexp arrexp (Identity intype) pos) = do
  split <- evalExp splitexp
  elems <- arrToList =<< evalExp arrexp
  case split of
    IntVal i _
      | i < length elems ->
        let (bef,aft) = splitAt i elems
        in return $ ArrayVal [ArrayVal bef intype pos,
                              ArrayVal aft intype pos] outtype pos
      | otherwise        -> bad $ IndexOutOfBounds pos (length elems) i
    _ -> bad $ TypeError pos "evalExp Split"
  where outtype = Array intype Nothing pos
evalExp (Concat arr1exp arr2exp (Identity intype) pos) = do
  elems1 <- arrToList =<< evalExp arr1exp
  elems2 <- arrToList =<< evalExp arr2exp
  return $ ArrayVal (elems1 ++ elems2) intype pos
evalExp (Read t pos) = do
  s <- join $ asks envReadOp
  case check $ parsefun s of
    Nothing -> bad $ ReadError pos t s
    Just v -> return v
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
        write (ArrayVal vs _ _)
          | Just s <- mapM char vs = s
          where char (CharVal c _) = Just c
                char _             = Nothing
        write v = ppValue v
evalExp (DoLoop loopvar boundexp body [mergevar] pos) = do
  bound <- evalExp boundexp
  mergeval <- lookupVar mergevar
  case bound of
    IntVal n _ -> foldM iteration mergeval [0..n-1]
    _ -> bad $ TypeError pos "evalExp DoLoop"
  where iteration val i =
          binding [(mergevar, val), (loopvar, IntVal i pos)] $ evalExp body
evalExp (DoLoop {}) = fail "Sorry, that loop's just too crazy for now."

evalIntBinOp :: (Applicative m, Monad m) =>
                (Int -> Int -> Int) -> Exp Identity -> Exp Identity -> Pos -> L0M m Value
evalIntBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (IntVal x _, IntVal y _) -> return $ IntVal (op x y) pos
    _                        -> bad $ TypeError pos "evalIntBinOp"

evalRealBinOp :: (Applicative m, Monad m) =>
                 (Double -> Double -> Double) -> Exp Identity -> Exp Identity -> Pos -> L0M m Value
evalRealBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (RealVal x _, RealVal y _) -> return $ RealVal (op x y) pos
    _                          -> bad $ TypeError pos $ "evalRealBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalBoolBinOp :: (Applicative m, Monad m) =>
                 (Bool -> Bool -> Bool) -> Exp Identity -> Exp Identity -> Pos -> L0M m Value
evalBoolBinOp op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (LogVal x _, LogVal y _) -> return $ LogVal (op x y) pos
    _                        -> bad $ TypeError pos $ "evalBoolBinOp " ++ ppValue v1 ++ " " ++ ppValue v2

evalPattern :: TupIdent -> Value -> Maybe [(String, Value)]
evalPattern (Id name _) v = Just [(name, v)]
evalPattern (TupId pats _) (TupVal vs _)
  | length pats == length vs =
    concat <$> zipWithM evalPattern pats vs
evalPattern _ _ = Nothing

applyLambda :: (Applicative m, Monad m) => Lambda Identity -> [Value] -> L0M m Value
applyLambda (AnonymFun params body _ _) args =
  binding (zip (map fst params) args) $ evalExp body
applyLambda (CurryFun name curryargs _ _ _) args = do
  curryargs' <- mapM evalExp curryargs
  fun <- lookupFun name
  fun $ curryargs' ++ args
