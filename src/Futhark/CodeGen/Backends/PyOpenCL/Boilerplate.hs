{-# LANGUAGE QuasiQuotes #-}

-- | Various boilerplate definitions for the PyOpenCL backend.
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClInit,
  )
where

import Control.Monad.Identity
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython qualified as Py
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.ImpCode.OpenCL
  ( ErrorMsg (..),
    ErrorMsgPart (..),
    FailureMsg (..),
    KernelConst (..),
    KernelConstExp,
    ParamMap,
    PrimType (..),
    errorMsgArgTypes,
    sizeDefault,
    untyped,
  )
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.Util.Pretty (prettyString, prettyText)
import NeatInterpolation (text)

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

getParamByKey :: Name -> PyExp
getParamByKey key = Index (Var "self.sizes") (IdxExp $ String $ prettyText key)

kernelConstToExp :: KernelConst -> PyExp
kernelConstToExp (SizeConst key _) =
  getParamByKey key
kernelConstToExp (SizeMaxConst size_class) =
  Var $ "self.max_" <> prettyString size_class

compileConstExp :: KernelConstExp -> PyExp
compileConstExp e = runIdentity $ Py.compilePrimExp (pure . kernelConstToExp) e

-- | Python code (as a string) that calls the
-- @initiatialize_opencl_object@ procedure.  Should be put in the
-- class constructor.
openClInit :: [(Name, KernelConstExp)] -> [PrimType] -> String -> ParamMap -> [FailureMsg] -> T.Text
openClInit constants types assign sizes failures =
  [text|
size_heuristics=$size_heuristics
self.global_failure_args_max = $max_num_args
self.failure_msgs=$failure_msgs
constants = $constants'
program = initialise_opencl_object(self,
                                   program_src=fut_opencl_src,
                                   build_options=build_options,
                                   command_queue=command_queue,
                                   interactive=interactive,
                                   platform_pref=platform_pref,
                                   device_pref=device_pref,
                                   default_group_size=default_group_size,
                                   default_num_groups=default_num_groups,
                                   default_tile_size=default_tile_size,
                                   default_reg_tile_size=default_reg_tile_size,
                                   default_threshold=default_threshold,
                                   size_heuristics=size_heuristics,
                                   required_types=$types',
                                   user_sizes=sizes,
                                   all_sizes=$sizes',
                                   constants=constants)
$assign'
|]
  where
    assign' = T.pack assign
    size_heuristics = prettyText $ sizeHeuristicsToPython sizeHeuristicsTable
    types' = prettyText $ map (show . prettyString) types -- Looks enough like Python.
    sizes' = prettyText $ sizeClassesToPython sizes
    max_num_args = prettyText $ foldl max 0 $ map (errorMsgNumArgs . failureError) failures
    failure_msgs = prettyText $ List $ map formatFailure failures
    onConstant (name, e) =
      Tuple
        [ String (nameToText name),
          Lambda "" (compileConstExp e)
        ]
    constants' = prettyText $ List $ map onConstant constants

formatFailure :: FailureMsg -> PyExp
formatFailure (FailureMsg (ErrorMsg parts) backtrace) =
  String $ mconcat (map onPart parts) <> "\n" <> formatEscape backtrace
  where
    formatEscape =
      let escapeChar '{' = "{{"
          escapeChar '}' = "}}"
          escapeChar c = T.singleton c
       in mconcat . map escapeChar

    onPart (ErrorString s) = formatEscape $ T.unpack s
    onPart ErrorVal {} = "{}"

sizeClassesToPython :: ParamMap -> PyExp
sizeClassesToPython = Dict . map f . M.toList
  where
    f (size_name, (size_class, _)) =
      ( String $ prettyText size_name,
        Dict
          [ (String "class", String $ prettyText size_class),
            ( String "value",
              maybe None (Integer . fromIntegral) $
                sizeDefault size_class
            )
          ]
      )

sizeHeuristicsToPython :: [SizeHeuristic] -> PyExp
sizeHeuristicsToPython = List . map f
  where
    f (SizeHeuristic platform_name device_type which what) =
      Tuple
        [ String (T.pack platform_name),
          clDeviceType device_type,
          which',
          what'
        ]
      where
        clDeviceType DeviceGPU = Var "cl.device_type.GPU"
        clDeviceType DeviceCPU = Var "cl.device_type.CPU"

        which' = case which of
          LockstepWidth -> String "lockstep_width"
          NumBlocks -> String "num_groups"
          BlockSize -> String "group_size"
          TileSize -> String "tile_size"
          RegTileSize -> String "reg_tile_size"
          Threshold -> String "threshold"

        what' =
          Lambda "device" $
            runIdentity $
              Py.compilePrimExp onLeaf $
                untyped what

        onLeaf (DeviceInfo s) =
          pure $
            Py.simpleCall
              "device.get_info"
              [Py.simpleCall "getattr" [Var "cl.device_info", String (T.pack s)]]
