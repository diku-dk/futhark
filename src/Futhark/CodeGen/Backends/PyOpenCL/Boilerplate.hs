{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClInit
  ) where

import NeatInterpolation

openClInit :: String -> String
openClInit assign = [string|
self.ctx = cl.create_some_context(interactive=False)
self.queue = cl.CommandQueue(self.ctx)
if (len(fut_opencl_src) >= 0):
  program = cl.Program(self.ctx, fut_opencl_src).build(["-DFUT_BLOCK_DIM={}".format(FUT_BLOCK_DIM), "-DWAVE_SIZE=32"])

$assign
|]
