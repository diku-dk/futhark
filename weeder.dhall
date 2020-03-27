-- Configuration rules for weeder; a tool for finding dead code:
-- https://github.com/ocharles/weeder

{ roots = [
  -- The entry points for the main CLI program and the unit tests.
  "^Main.main$"

  -- Modules intended as externally visible for library code.
  , "^Language.Futhark.Query"
  , "^Language.Futhark.Parser"

  -- Generated code that we cannot do anything about.
  , "^Paths_futhark"

  -- Code that might technically be dead right now, but is kept around
  -- for consistency of the internal API.
  , "^Futhark.Analysis.AlgSimplify.ppRangesRep"
  , "^Futhark.Analysis.PrimExp"
  , "^Futhark.Binder"
  , "^Futhark.Construct.eConvOp"
  , "^Futhark.Pass.ExtractKernels.Distribution.ppKernelNest"
  , "^Futhark.Representation.AST.Attributes.Types.int16"
  , "^Futhark.Representation.AST.Attributes.Types.float32"
  , "^Futhark.Representation.AST.Attributes.Types.float64"
  , "^Futhark.Representation.PrimitiveTests"
  ],

  type-class-roots = True
}
