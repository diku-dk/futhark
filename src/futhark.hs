-- | Futhark Compiler Driver
module Main (main) where

import System.Console.GetOpt

import Futhark.Passes
import Futhark.Actions
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Pipeline

type FutharkOption = FunOptDescr FutharkConfig

passoption :: String -> Pass -> String -> [String] -> FutharkOption
passoption desc pass short long =
  Option short long
  (NoArg $ Right $ \opts -> opts { futharkpipeline = pass : futharkpipeline opts })
  desc

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file -> Right $ \opts -> opts { futharkverbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["inhibit-uniqueness-checking"]
    (NoArg $ Right $ \opts -> opts { futharkcheckAliases = False })
    "Don't check that uniqueness constraints are being upheld."
  , Option [] ["compile-sequential"]
    (NoArg $ Right $ \opts -> opts { futharkaction = seqCodegenAction })
    "Translate program into sequential C and write it on standard output."
  , Option [] ["compile-imperative"]
    (NoArg $ Right $ \opts -> opts { futharkaction = impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option [] ["compile-imperative-kernels"]
    (NoArg $ Right $ \opts -> opts { futharkaction = kernelImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkaction = printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ Right $ \opts -> opts { futharkaction = interpretAction' $
                                                     futharkRealConfiguration opts })
    "Run the program via an interpreter."
  , Option [] ["externalise"]
    (NoArg $ Right $ \opts -> opts { futharkaction = externaliseAction})
    "Prettyprint the resulting external representation on standard output."
  , Option [] ["range-analysis"]
    (NoArg $ Right $ \opts -> opts { futharkaction = rangeAction })
    "Print the program with range annotations added."
  , Option [] ["no-bounds-checking"]
    (NoArg $ Right $ \opts -> opts { futharkboundsCheck = False })
    "Do not perform bounds checking in the generated program."
  , Option [] ["real-as-single"]
    (NoArg $ Right $ \config -> config { futharkRealConfiguration = RealAsFloat32 } )
    "Map 'real' to 32-bit floating point."
  , Option [] ["real-as-double"]
    (NoArg $ Right $ \config -> config { futharkRealConfiguration = RealAsFloat64 } )
    "Map 'real' to 64-bit floating point (the default)."
  , passoption "Remove debugging annotations from program." uttransform
    "u" ["untrace"]
  , passoption "Transform all second-order array combinators to for-loops." fotransform
    "f" ["first-order-transform"]
  , passoption "Transform program to explicit memory representation" explicitMemory
    "a" ["explicit-allocations"]
  , passoption "Perform simple enabling optimisations." eotransform
    "e" ["enabling-optimisations"]
  , passoption "Perform higher-order optimisation, i.e., fusion." hotransform
    "o" ["higher-order-optimizations"]
  , passoption "Aggressively inline and remove dead functions." inlinetransform
    [] ["inline-functions"]
  , passoption "Remove dead functions." removeDeadFunctions
    [] ["remove-dead-functions"]
  , passoption "Optimise predicates" optimisePredicates
    [] ["optimise-predicates"]
  , passoption "Optimise shape computation" optimiseShapes
    [] ["optimise-shapes"]
  , passoption "Lower in-place updates" inPlaceLowering
    [] ["in-place-lowering"]
  , passoption "Common subexpression elimination" commonSubexpressionElimination
    [] ["cse"]
  , passoption "Flattening" flattening
    [] ["flattening"]
  , passoption "Double-buffering" doubleBuffer
    [] ["double-buffer"]
  , passoption "Kernel sequentialisation" sequentialiseKernels
    [] ["sequentialise-kernels"]
  , Option "s" ["standard"]
    (NoArg $ Right $ \opts -> opts { futharkpipeline = standardPipeline ++ futharkpipeline opts })
    "Use the recommended optimised pipeline."
  ]

standardPipeline :: [Pass]
standardPipeline =
  [ uttransform
  , eotransform
  , inlinetransform
  , commonSubexpressionElimination
  , eotransform
  , hotransform
  , commonSubexpressionElimination
  , eotransform
  , removeDeadFunctions
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = mainWithOptions newFutharkConfig commandLineOptions compile
  where compile [file] config = Just $ runCompilerOnProgram config file
        compile _      _      = Nothing
