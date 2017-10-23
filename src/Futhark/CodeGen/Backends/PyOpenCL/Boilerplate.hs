{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClInit
  , openClPrelude
  ) where

import Data.FileEmbed
import qualified Data.Text as T
import NeatInterpolation (text)

import Futhark.CodeGen.OpenCL.Kernels
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.Util.Pretty (pretty)

openClPrelude :: String
openClPrelude = $(embedStringFile "rts/python/opencl.py")

openClInit :: String -> String
openClInit assign = T.unpack [text|
self.ctx = get_prefered_context(interactive, platform_pref, device_pref)
self.queue = cl.CommandQueue(self.ctx)
self.device = self.ctx.get_info(cl.context_info.DEVICES)[0]
 # XXX: Assuming just a single device here.
platform_name = self.ctx.get_info(cl.context_info.DEVICES)[0].platform.name
device_type = self.device.type
lockstep_width = None
$set_sizes
max_tile_size = int(np.sqrt(self.device.max_work_group_size))
if (tile_size * tile_size > self.device.max_work_group_size):
  sys.stderr.write('Warning: Device limits tile size to {} (setting was {})\n'.format(max_tile_size, tile_size))
  tile_size = max_tile_size
self.group_size = group_size
self.num_groups = num_groups
self.tile_size = tile_size
if (len(fut_opencl_src) >= 0):
  program = cl.Program(self.ctx, fut_opencl_src).build(["-DFUT_BLOCK_DIM={}".format(FUT_BLOCK_DIM),
                                                        "-DLOCKSTEP_WIDTH={}".format(lockstep_width),
                                                        "-DDEFAULT_GROUP_SIZE={}".format(group_size),
                                                        "-DDEFAULT_NUM_GROUPS={}".format(num_groups),
                                                        "-DDEFAULT_TILE_SIZE={}".format(tile_size)])

$assign'
|]
  where assign' = T.pack assign
        set_sizes = T.pack $ unlines $
                    map (pretty . sizeHeuristicsCode) sizeHeuristicsTable


sizeHeuristicsCode :: SizeHeuristic -> PyStmt
sizeHeuristicsCode (SizeHeuristic platform_name device_type which what) =
  If (BinOp "and"
      (BinOp "==" which' (Var "None"))
      (BinOp "and"
        (BinOp "!="
         (Call (Field (Var "platform_name") "find") [Arg (String platform_name)])
         (Var "None"))
        (BinOp "==" (Var "device_type") (clDeviceType device_type))))
  [Assign which' what'] []
  where clDeviceType DeviceGPU = Var "cl.device_type.GPU"
        clDeviceType DeviceCPU = Var "cl.device_type.CPU"
        which' = case which of LockstepWidth -> Var "lockstep_width"
                               NumGroups     -> Var "num_groups"
                               GroupSize     -> Var "group_size"
        what' = case what of
                  HeuristicConst x -> Integer $ toInteger x
                  HeuristicDeviceInfo s ->
                    Call (Field (Var "self.device") "get_info")
                    [Arg $ Var $ "cl.device_info." ++ s]
