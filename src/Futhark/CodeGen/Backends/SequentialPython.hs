-- | Code generation for sequential Python.
module Futhark.CodeGen.Backends.SequentialPython
  ( compileProg,
  )
where

import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython qualified as Py
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.ImpCode.Sequential qualified as Imp
import Futhark.CodeGen.ImpGen.Sequential qualified as ImpGen
import Futhark.IR.SeqMem (Prog, SeqMem)
import Futhark.MonadFreshNames

-- | Compile the program to Python.
compileProg ::
  (MonadFreshNames m) =>
  Py.CompilerMode ->
  String ->
  Prog SeqMem ->
  m (ImpGen.Warnings, T.Text)
compileProg mode class_name prog = do
  (ws, defs) <- ImpGen.compileProg prog
  (ws,)
    <$> Py.compileProg
      mode
      class_name
      constructor
      imports
      defines
      operations
      ()
      []
      []
      defs
  where
    imports =
      [ Import "sys" Nothing,
        Import "numpy" $ Just "np",
        Import "ctypes" $ Just "ct",
        Import "time" Nothing
      ]
    constructor =
      Py.Constructor
        ["self", "user_sizes=user_sizes"]
        [Exp $ Py.simpleCall "set_user_params" [Var "self.sizes", Var "user_sizes"]]
    defines = []
    operations :: Py.Operations Imp.Sequential ()
    operations =
      Py.defaultOperations
        { Py.opsCompiler = const $ pure ()
        }
