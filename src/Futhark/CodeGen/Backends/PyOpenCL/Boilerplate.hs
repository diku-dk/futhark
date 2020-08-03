{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Various boilerplate definitions for the PyOpenCL backend.
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClInit
  , openClPrelude
  ) where

import Control.Monad.Identity
import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Map as M
import NeatInterpolation (text)

import Futhark.CodeGen.ImpCode.OpenCL
  (PrimType(..), SizeClass(..), sizeDefault,
   FailureMsg(..), ErrorMsg(..), ErrorMsgPart(..), errorMsgArgTypes)
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.Backends.GenericPython.AST
import qualified Futhark.CodeGen.Backends.GenericPython as Py
import Futhark.Util.Pretty (prettyText)

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

-- | @rts/python/opencl.py@ embedded as a string.
openClPrelude :: String
openClPrelude = $(embedStringFile "rts/python/opencl.py")

-- | Python code (as a string) that calls the
-- @initiatialize_opencl_object@ procedure.  Should be put in the
-- class constructor.
openClInit :: [PrimType] -> String -> M.Map Name SizeClass -> [FailureMsg] -> String
openClInit types assign sizes failures = T.unpack [text|
size_heuristics=$size_heuristics
self.global_failure_args_max = $max_num_args
self.failure_msgs=$failure_msgs
program = initialise_opencl_object(self,
                                   program_src=fut_opencl_src,
                                   command_queue=command_queue,
                                   interactive=interactive,
                                   platform_pref=platform_pref,
                                   device_pref=device_pref,
                                   default_group_size=default_group_size,
                                   default_num_groups=default_num_groups,
                                   default_tile_size=default_tile_size,
                                   default_threshold=default_threshold,
                                   size_heuristics=size_heuristics,
                                   required_types=$types',
                                   user_sizes=sizes,
                                   all_sizes=$sizes')
$assign'
|]
  where assign' = T.pack assign
        size_heuristics = prettyText $ sizeHeuristicsToPython sizeHeuristicsTable
        types' = prettyText $ map (show . pretty) types -- Looks enough like Python.
        sizes' = prettyText $ sizeClassesToPython sizes
        max_num_args = prettyText $ foldl max 0 $ map (errorMsgNumArgs . failureError) failures
        failure_msgs = prettyText $ List $ map formatFailure failures

formatFailure :: FailureMsg -> PyExp
formatFailure (FailureMsg (ErrorMsg parts) backtrace) =
  String $ concatMap onPart parts ++ "\n" ++ formatEscape backtrace
  where formatEscape = let escapeChar '{' = "{{"
                           escapeChar '}' = "}}"
                           escapeChar c = [c]
                       in concatMap escapeChar

        onPart (ErrorString s) = formatEscape s
        onPart ErrorInt32{} = "{}"

sizeClassesToPython :: M.Map Name SizeClass -> PyExp
sizeClassesToPython = Dict . map f . M.toList
  where f (size_name, size_class) =
          (String $ pretty size_name,
           Dict [(String "class", String $ pretty size_class),
                 (String "value", maybe None (Integer . fromIntegral) $
                                  sizeDefault size_class)])

sizeHeuristicsToPython :: [SizeHeuristic] -> PyExp
sizeHeuristicsToPython = List . map f
  where f (SizeHeuristic platform_name device_type which what) =
          Tuple [String platform_name,
                 clDeviceType device_type,
                 which',
                 what']

          where clDeviceType DeviceGPU = Var "cl.device_type.GPU"
                clDeviceType DeviceCPU = Var "cl.device_type.CPU"

                which' = case which of LockstepWidth -> String "lockstep_width"
                                       NumGroups     -> String "num_groups"
                                       GroupSize     -> String "group_size"
                                       TileSize      -> String "tile_size"
                                       Threshold     -> String "threshold"

                what' = Lambda "device" $ runIdentity $ Py.compilePrimExp onLeaf what

                onLeaf (DeviceInfo s) =
                  pure $ Py.simpleCall "device.get_info"
                  [Py.simpleCall "getattr" [Var "cl.device_info", String s]]
