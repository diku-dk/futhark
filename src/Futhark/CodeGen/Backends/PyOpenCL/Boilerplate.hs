{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClInit
  , openClPrelude
  ) where

import Data.FileEmbed
import qualified Data.Map as M
import qualified Data.Text as T
import NeatInterpolation (text)

import Futhark.CodeGen.ImpCode.OpenCL (PrimType(..), SizeClass(..))
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.Util.Pretty (prettyText)

-- | @rts/python/opencl.py@ embedded as a string.
openClPrelude :: String
openClPrelude = $(embedStringFile "rts/python/opencl.py")

-- | Python code (as a string) that calls the
-- @initiatialize_opencl_object@ procedure.  Should be put in the
-- class constructor.
openClInit :: [PrimType] -> String -> M.Map Name SizeClass -> String
openClInit types assign sizes = T.unpack [text|
size_heuristics=$size_heuristics
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

sizeClassesToPython :: M.Map Name SizeClass -> PyExp
sizeClassesToPython = Dict . map f . M.toList
  where f (size_name, size_class) =
          (String $ pretty size_name,
           Dict [(String "class", String $ pretty size_class),
                 (String "value", None)])

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

                what' = case what of
                          HeuristicConst x -> Integer $ toInteger x
                          HeuristicDeviceInfo s -> String s
