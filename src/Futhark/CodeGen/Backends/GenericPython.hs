{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.Backends.GenericPython
  ( compileProg,
    compileExp,
    compileCode,
    CompilerEnv(..),
    CompilerState(..)
    ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (BinOp (..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.Util.Pretty(pretty)

data CompilerEnv = CompilerEnv {
  envFtable     :: HM.HashMap Name [Imp.Type]
}

newCompilerEnv :: Imp.Program () -> CompilerEnv
newCompilerEnv (Imp.Program funs) =
  CompilerEnv { envFtable = ftable <> builtinFtable }
  where ftable = HM.fromList $ map funReturn funs
        funReturn (name, Imp.Function outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = HM.map (map Imp.Scalar . snd) builtInFunctions

data CompilerState = CompilerState {
  compNameSrc :: VNameSource
}

newCompilerState :: VNameSource -> CompilerState
newCompilerState src = CompilerState { compNameSrc = src }

newtype CompilerM a = CompilerM (RWS
                                     CompilerEnv
                                     [PyStmt]
                                     CompilerState a)
  deriving (Functor, Applicative, Monad,
            MonadState CompilerState,
            MonadReader CompilerEnv,
            MonadWriter [PyStmt])

collect :: CompilerM () -> CompilerM [PyStmt]
collect m = pass $ do
  ((), w) <- listen m
  return (w, const mempty)

collect' :: CompilerM a -> CompilerM (a, [PyStmt])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, w), const mempty)

stm :: PyStmt -> CompilerM ()
stm x = tell [x]

stms :: [PyStmt] -> CompilerM ()
stms = mapM_ stm

paramsTypes :: [Imp.Param] -> [Imp.Type]
paramsTypes = map paramType
  where paramType (Imp.MemParam _ size space) = Imp.Mem size space
        paramType (Imp.ScalarParam _ t) = Imp.Scalar t

runCompilerM :: Imp.Program () -> VNameSource
             -> CompilerM a
             -> a
runCompilerM prog src (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv prog ) (newCompilerState src)


compileProg :: Imp.Program () -> String
compileProg prog@(Imp.Program funs) =
  pretty $ PyProg $ runCompilerM prog blankNameSource (mapM compileFunc funs)

compileFunc :: (Name, Imp.Function ()) -> CompilerM PyFunc
compileFunc (fname, Imp.Function outputs inputs body _ _) = do
  body' <- collect $ compileCode body
  let inputs' = map (pretty . Imp.paramName) inputs
  --let pyrettype = typeToPyType $ paramsTypes outputs
  --let retval = PyStmt
  return $ PyFunc (nameToString fname) inputs' body'

--data UnOp = Not -- ^ Boolean negation.
--          | Complement -- ^ Bitwise complement.
--          | Negate -- ^ Numerical negation.
--          | Abs -- ^ Absolute/numerical value.
--          | Signum -- ^ Sign function.
--            deriving (Eq, Show)

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Imp.Not -> "!"
    Imp.Complement -> "~"
    Imp.Negate -> "-"
    Imp.Abs -> "abs"
    Imp.Signum -> undefined

--data BinOp = Plus -- Binary Ops for Numbers
--           | Minus
--           | Pow
--           | Times
--           | FloatDiv
--           | Div -- ^ Rounds towards negative infinity.
--           | Mod
--           | Quot -- ^ Rounds towards zero.
--           | Rem
--           | ShiftR
--           | ShiftL
--           | Band
--           | Xor
--           | Bor
--           | LogAnd
--           | LogOr
--           -- Relational Ops for all basic types at least
--           | Equal
--           | Less
--           | Leq
--           deriving (Eq, Ord, Enum, Bounded, Show)

compileBinOp :: BinOp -> String
compileBinOp op =
  case op of
    Plus -> "+"
    Minus -> "-"
    Div -> "/"
    Times -> "*"
    Equal -> "=="
    _ -> undefined

compileExp :: Imp.Exp -> PyExp
compileExp (Imp.Constant v) = Constant v
compileExp (Imp.ScalarVar vname) = ScalarVar $ pretty vname
compileExp (Imp.BinOp op exp1 exp2) = BinaryOp (compileBinOp op) (compileExp exp1) (compileExp exp2)
compileExp (Imp.UnOp op exp) = UnOp (compileUnOp op) (compileExp exp)
compileExp (Imp.Cond exp1 exp2 exp3) = Cond (compileExp exp1) (compileExp exp2) (compileExp exp3)
compileExp _ = undefined

-- get everything to work except memblocks and function calls
compileCode :: Imp.Code () -> CompilerM ()
compileCode (Imp.If cond tb fb) = do
  let cond' = compileExp cond
  tb' <- collect $ compileCode tb
  fb' <- collect $ compileCode fb
  stm $ If cond' tb' fb'

compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2

compileCode (Imp.While cond body) = do
  let cond' = compileExp cond
  body' <- collect $ compileCode body
  stm $ While cond' body'

compileCode (Imp.For i bound body) = do
  let bound' = compileExp bound
  body' <- collect $ compileCode body
  stm $ For (pretty i) bound' body'

compileCode (Imp.SetScalar vname exp1) =
  stm $ AssignVar (pretty vname) (compileExp exp1)

compileCode (Imp.DeclareScalar _ _) = return ()

compileCode c = fail $ "This is not handled yet: " ++ pretty c
