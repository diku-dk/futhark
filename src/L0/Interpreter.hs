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
import qualified Data.Map as M

import L0.AbSyn

data InterpreterError = MissingMainFunction
                      | IndexOutOfBounds Pos Int Int
                      -- ^ First @Int@ is array size, second is attempted index.
                      | NegativeIota Pos Int
                      | NegativeReplicate Pos Int
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

runProgIO :: Prog Identity -> IO (Either InterpreterError Value)
runProgIO = runProg putStr readLn

runProg :: (Applicative m, Monad m) => (String -> m ()) -> (m String)
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
evalExp (TupLit es (Identity t) pos) =
  TupVal <$> mapM evalExp es <*> pure t <*> pure pos
evalExp (ArrayLit es (Identity t) pos) =
  ArrayVal <$> mapM evalExp es <*> pure t <*> pure pos
evalExp (Plus e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (+) e1 e2 pos
evalExp (Plus e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (+) e1 e2 pos
evalExp (Plus _ _ _ _) = bad TypeError
evalExp (Minus e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (-) e1 e2 pos
evalExp (Minus e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (-) e1 e2 pos
evalExp (Minus _ _ _ _) = bad TypeError
evalExp (Pow e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (^) e1 e2 pos
evalExp (Pow e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (**) e1 e2 pos
evalExp (Pow _ _ _ _) = bad TypeError
evalExp (Times e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal (*) e1 e2 pos
evalExp (Times e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (*) e1 e2 pos
evalExp (Times _ _ _ _) = bad TypeError
evalExp (Divide e1 e2 (Identity (Int _)) pos) = evalIntBinOp IntVal div e1 e2 pos
evalExp (Divide e1 e2 (Identity (Real _)) pos) = evalRealBinOp RealVal (/) e1 e2 pos
evalExp (Divide _ _ _ _) = bad TypeError
evalExp (ShiftR e1 e2 pos) = evalIntBinOp IntVal shiftR e1 e2 pos
evalExp (ShiftL e1 e2 pos) = evalIntBinOp IntVal shiftL e1 e2 pos
evalExp (Band e1 e2 pos) = evalIntBinOp IntVal (.&.) e1 e2 pos
evalExp (Xor e1 e2 pos) = evalIntBinOp IntVal xor e1 e2 pos
evalExp (Bor e1 e2 pos) = evalIntBinOp IntVal (.|.) e1 e2 pos
evalExp (And e1 e2 _) = do
  v1 <- evalExp e1
  case v1 of LogVal True _  -> evalExp e2
             LogVal False _ -> return v1
             _              -> bad TypeError
evalExp (Or e1 e2 _) = do
  v1 <- evalExp e1
  case v1 of LogVal True _  -> return v1
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
evalExp (Var name _ _) = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of Nothing -> bad TypeError
              Just v  -> return v
evalExp (Apply fname args _ _) = do
  fun <- asks $ M.lookup fname . envFtable
  args' <- mapM evalExp args
  case fun of Nothing   -> bad TypeError
              Just fun' -> fun' args'
evalExp (Let pat e Nothing Nothing body _) = do
  v <- evalExp e
  case evalPattern pat v of
    Nothing   -> bad TypeError
    Just bnds -> local (`bindVars` bnds) $ evalExp body
evalExp (Let (Id name _) e (Just idxs) (Just ve) body _) = do
  v <- evalExp e
  error "Later."
evalExp (Let _ _ _ _ _ _) = bad TypeError
evalExp (Index name idxs _ _ pos) = do
  bnd <- asks $ M.lookup name . envVtable
  idxs' <- mapM evalExp idxs
  case bnd of
    Nothing -> bad TypeError
    Just v  -> foldM index v idxs'
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
evalPattern (TupId pats _) (TupVal vs _ _)
  | length pats == length vs =
    concat <$> zipWithM evalPattern pats vs
evalPattern _ _ = Nothing
