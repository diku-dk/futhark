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

import L0.AbSyn
import L0.Parser

data InterpreterError = MissingMainFunction
                      | IndexOutOfBounds Pos Int Int
                      -- ^ First @Int@ is array size, second is attempted index.
                      | NegativeIota Pos Int
                      | NegativeReplicate Pos Int
                      | ReadError Pos Type String
                      | InvalidArrayShape Pos
                      | TypeError

instance Show InterpreterError where
  show MissingMainFunction =
    "No main function defined."
  show (IndexOutOfBounds pos arrsz i) =
    "Array index " ++ show i ++ " out of bounds of array size " ++ show arrsz ++ " at " ++ posStr pos ++ "."
  show (NegativeIota pos n) =
    "Argument " ++ show n ++ " to iota at " ++ posStr pos ++ " is negative."
  show (NegativeReplicate pos n) =
    "Argument " ++ show n ++ " to replicate at " ++ posStr pos ++ " is negative."
  show TypeError =
    "Type error during interpretation.  This implies a bug in the type checker."
  show (ReadError pos t s) =
    "Read error while trying to read " ++ ppType t ++ " at " ++ posStr pos ++ ".  Input line was: " ++ s
  show (InvalidArrayShape pos) =
    "Invalid array reshaping at " ++ posStr pos ++ "."


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
                                 Nothing   -> bad TypeError

lookupFun :: Monad m => String -> L0M m ([Value] -> L0M m Value)
lookupFun fname = do fun <- asks $ M.lookup fname . envFtable
                     case fun of Just fun' -> return fun'
                                 Nothing   -> bad TypeError

arrToList :: Monad m => Value -> L0M m [Value]
arrToList (ArrayVal l _ _) = return l
arrToList _ = bad TypeError

runProgIO :: Prog Identity -> IO (Either InterpreterError Value)
runProgIO = runProg putStr readLn

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
    builtins = M.empty

evalExp :: (Applicative m, Monad m) => Exp Identity -> L0M m Value
evalExp (Literal val) = return val
evalExp (TupLit es _ pos) =
  TupVal <$> mapM evalExp es <*> pure pos
evalExp (ArrayLit es (Identity t) pos) =
  ArrayVal <$> mapM evalExp es <*> pure t <*> pure pos
evalExp (Plus e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (+) e1 e2 pos
evalExp (Plus e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (+) e1 e2 pos
evalExp (Plus {}) = bad TypeError
evalExp (Minus e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (-) e1 e2 pos
evalExp (Minus e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (-) e1 e2 pos
evalExp (Minus {}) = bad TypeError
evalExp (Pow e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (^) e1 e2 pos
evalExp (Pow e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (**) e1 e2 pos
evalExp (Pow {}) = bad TypeError
evalExp (Times e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (*) e1 e2 pos
evalExp (Times e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (*) e1 e2 pos
evalExp (Times {}) = bad TypeError
evalExp (Divide e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal div e1 e2 pos
evalExp (Divide e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (/) e1 e2 pos
evalExp (Divide {}) = bad TypeError
evalExp (ShiftR e1 e2 pos) = evalIntBinOp IntVal shiftR e1 e2 pos
evalExp (ShiftL e1 e2 pos) = evalIntBinOp IntVal shiftL e1 e2 pos
evalExp (Band e1 e2 pos) = evalIntBinOp IntVal (.&.) e1 e2 pos
evalExp (Xor e1 e2 pos) = evalIntBinOp IntVal xor e1 e2 pos
evalExp (Bor e1 e2 pos) = evalIntBinOp IntVal (.|.) e1 e2 pos
evalExp (And e1 e2 _) = do
  v1 <- evalExp e1
  case v1 of LogVal True  _ -> evalExp e2
             LogVal False _ -> return v1
             _              -> bad TypeError
evalExp (Or e1 e2 _) = do
  v1 <- evalExp e1
  case v1 of LogVal True  _ -> return v1
             LogVal False _ -> evalExp e2
             _              -> bad TypeError
evalExp (Equal e1 e2 pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1==v2) pos
evalExp (Less e1 e2 pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<v2) pos
evalExp (Leq e1 e2 pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return $ LogVal (v1<=v2) pos
evalExp (Not e _) = do
  v <- evalExp e
  case v of LogVal b pos -> return $ LogVal (not b) pos
            _            -> bad TypeError
evalExp (Negate e _ _) = do
  v <- evalExp e
  case v of IntVal x pos  -> return $ IntVal (-x) pos
            RealVal x pos -> return $ RealVal (-x) pos
            _             -> bad TypeError
evalExp (If e1 e2 e3 _ _) = do
  v <- evalExp e1
  case v of LogVal True _  -> evalExp e2
            LogVal False _ -> evalExp e3
            _              -> bad TypeError
evalExp (Var name _ _) =
  lookupVar name
evalExp (Apply fname args _ _) = do
  fun <- lookupFun fname
  args' <- mapM evalExp args
  fun args'
evalExp (Let pat e Nothing Nothing body _) = do
  v <- evalExp e
  case evalPattern pat v of
    Nothing   -> bad TypeError
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
        change _g _ _ = bad TypeError
evalExp (Let {}) = bad TypeError
evalExp (Index name idxs _ _ pos) = do
  v <- lookupVar name
  idxs' <- mapM evalExp idxs
  foldM index v idxs'
  where index (ArrayVal vs _ _) (IntVal i _)
          | i < length vs = return $ vs !! i
          | otherwise     = bad $ IndexOutOfBounds pos (length vs) i
        index _ _ = bad TypeError
evalExp (Iota e pos) = do
  v <- evalExp e
  case v of
    IntVal x _
      | x >= 0    -> return $ ArrayVal (map (`IntVal` pos) [0..x]) (Int pos) pos
      | otherwise -> bad $ NegativeIota pos x
    _ -> bad TypeError
evalExp (Replicate e1 e2 _ pos) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case v1 of
    IntVal x _
      | x >= 0    -> return $ ArrayVal (replicate x v2) (Int pos) pos
      | otherwise -> bad $ NegativeReplicate pos x
    _ -> bad TypeError
evalExp (Reshape shapeexp arrexp _ (Identity outtype) pos) = do
  shape <- mapM evalExp shapeexp
  arr <- evalExp arrexp
  reshape outtype shape $ flatten arr
    where flatten (ArrayVal vs _ _) = concatMap flatten vs
          flatten t = [t]
          reshape (Array t _ _) (IntVal n _:rest) vs
            | length vs `mod` n == 0 =
              ArrayVal <$> mapM (reshape t rest) (chunk n rest)
                       <*> pure t <*> pure pos
            | otherwise = bad $ InvalidArrayShape pos
          reshape t [] vs = return $ ArrayVal vs t pos
          reshape _ _ _ = bad TypeError
          chunk _ [] = []
          chunk i l = let (a,b) = splitAt i l
                      in a : chunk i b
evalExp (Transpose arrexp _ _ _) = do
  v <- evalExp arrexp
  case v of
    ArrayVal els (Array et _ _) pos -> do
      let arr el = ArrayVal el et pos
      els' <- map arr <$> transpose <$> mapM elems els
      return $ ArrayVal els' (Array et Nothing pos) pos
    _ -> bad TypeError
  where elems (ArrayVal els _ _) = return els
        elems _ = bad TypeError
evalExp (Map fun e _ (Identity outtype) pos) = do
  elems <- arrToList =<< evalExp e
  case outtype of
    (Array t _ _) -> do
      elems' <- mapM (applyLambda fun . (:[])) elems
      return $ ArrayVal elems' t pos
    _ -> bad TypeError
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
evalExp (Scan fun startexp arrexp _ pos) = do
  startval <- evalExp startexp
  vals <- arrToList =<< evalExp arrexp
  (acc, vals') <- foldM scanfun (startval, [startval]) vals
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
    _ -> bad TypeError
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
  join $ asks envWriteOp <*> pure (ppValue 0 v)
  return v

evalIntBinOp :: (Applicative m, Monad m) =>
                (a -> Pos -> Value) -> (Int -> Int -> a)
             -> Exp Identity -> Exp Identity -> Pos -> L0M m Value
evalIntBinOp con op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (IntVal x _, IntVal y _) -> return $ con (op x y) pos
    _                        -> bad TypeError

evalRealBinOp :: (Applicative m, Monad m) =>
                 (a -> Pos -> Value) -> (Double -> Double -> a)
             -> Exp Identity -> Exp Identity -> Pos -> L0M m Value
evalRealBinOp con op e1 e2 pos = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  case (v1, v2) of
    (RealVal x _, RealVal y _) -> return $ con (op x y) pos
    _                          -> bad TypeError

evalPattern :: TupIdent -> Value -> Maybe [(String, Value)]
evalPattern (Id name _) v = Just [(name, v)]
evalPattern (TupId pats _) (TupVal vs _)
  | length pats == length vs =
    concat <$> zipWithM evalPattern pats vs
evalPattern _ _ = Nothing

applyLambda :: Monad m => Lambda Identity -> [Value] -> L0M m Value
applyLambda = undefined
