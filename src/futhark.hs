-- | Futhark Compiler Driver
module Main (main) where

import System.Console.GetOpt

import Futhark.Passes
import Futhark.Actions
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Pipeline

data Config = Config { futharkConfig :: FutharkConfig
                     , futharkAction :: Action
                     }

newConfig :: Config
newConfig = Config newFutharkConfig printAction

changeFutharkConfig :: (FutharkConfig -> FutharkConfig)
                    -> Config -> Config
changeFutharkConfig f cfg = cfg { futharkConfig = f $ futharkConfig cfg }

type FutharkOption = FunOptDescr Config

passoption :: String -> Pass -> String -> [String] -> FutharkOption
passoption desc pass short long =
  Option short long
  (NoArg $ Right $ changeFutharkConfig $
   \opts -> opts { futharkPipeline = pass : futharkPipeline opts })
  desc

commandLineOptions :: [FutharkOption]
commandLineOptions =
  [ Option "V" ["verbose"]
    (OptArg (\file -> Right $ changeFutharkConfig $
                      \opts -> opts { futharkVerbose = Just file }) "FILE")
    "Print verbose output on standard error; wrong program to FILE."
  , Option [] ["inhibit-uniqueness-checking"]
    (NoArg $ Right $ changeFutharkConfig $
     \opts -> opts { futharkCheckAliases = False })
    "Don't check that uniqueness constraints are being upheld."
  , Option [] ["no-bounds-checking"]
    (NoArg $ Right $ changeFutharkConfig $
     \opts -> opts { futharkBoundsCheck = False })
    "Do not perform bounds checking in the generated program."

  , Option [] ["compile-sequential"]
    (NoArg $ Right $ \opts -> opts { futharkAction = seqCodegenAction })
    "Translate program into sequential C and write it on standard output."
  , Option [] ["compile-imperative"]
    (NoArg $ Right $ \opts -> opts { futharkAction = impCodeGenAction })
    "Translate program into the imperative IL and write it on standard output."
  , Option [] ["compile-imperative-kernels"]
    (NoArg $ Right $ \opts -> opts { futharkAction = kernelImpCodeGenAction })
    "Translate program into the imperative IL with kernels and write it on standard output."
  , Option "p" ["print"]
    (NoArg $ Right $ \opts -> opts { futharkAction = printAction })
    "Prettyprint the resulting internal representation on standard output (default action)."
  , Option "i" ["interpret"]
    (NoArg $ Right $ \opts -> opts { futharkAction = interpretAction' $
                                                     futharkRealConfiguration $
                                                     futharkConfig opts })
    "Run the program via an interpreter."
  , Option [] ["externalise"]
    (NoArg $ Right $ \opts -> opts { futharkAction = externaliseAction})
    "Prettyprint the resulting external representation on standard output."
  , Option [] ["range-analysis"]
    (NoArg $ Right $ \opts -> opts { futharkAction = rangeAction })
    "Print the program with range annotations added."

  , Option [] ["real-as-single"]
    (NoArg $ Right $ changeFutharkConfig $
     \config -> config { futharkRealConfiguration = RealAsFloat32 } )
    "Map 'real' to 32-bit floating point."
  , Option [] ["real-as-double"]
    (NoArg $ Right $ changeFutharkConfig $
     \config -> config { futharkRealConfiguration = RealAsFloat64 } )
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
  , passoption "Kernel babysitting" babysitKernels
    [] ["babysit-kernels"]
  , passoption "Expand allocations" expandAllocations
    [] ["expand-allocations"]
  , passoption "Expand arrays" expandArrays
    [] ["expand-arrays"]
  , passoption "Kernel extraction" extractKernels
    [] ["extract-kernels"]

  , Option "s" ["standard"]
    (NoArg $ Right $ changeFutharkConfig $
     \opts -> opts { futharkPipeline = standardPipeline ++ futharkPipeline opts })
    "Use the recommended optimised pipeline."
  ]

-- | Entry point.  Non-interactive, except when reading interpreter
-- input from standard input.
main :: IO ()
main = mainWithOptions newConfig commandLineOptions compile
  where compile [file] config =
          Just $ runCompilerOnProgram (futharkConfig config) (futharkAction config) file
        compile _      _      =
          Nothing
