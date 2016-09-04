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

import Futhark.Representation.AST.Attributes.Constants (value)
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
lockstep_width = 1
$set_lockstep_width
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
        set_lockstep_width =
          T.pack $ unlines $
          map (pretty . lockstepWidthHeuristicsCode) lockstepWidthHeuristicsTable


lockstepWidthHeuristicsCode :: LockstepWidthHeuristic -> PyStmt
lockstepWidthHeuristicsCode
  (LockstepWidthHeuristic platform_name device_type width) =
  If (BinOp "and"
      (BinOp "==" (Var "platform_name") (StringLiteral platform_name))
      (BinOp "==" (Var "device_type") (clDeviceType device_type)))
  [Assign (Var "lockstep_width") (Constant (value (fromIntegral width::Int32)))]
  []
  where clDeviceType DeviceGPU = Var "cl.device_type.GPU"
        clDeviceType DeviceCPU = Var "cl.device_type.CPU"
