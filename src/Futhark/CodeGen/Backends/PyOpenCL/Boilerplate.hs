{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
  ( openClDecls
  , openClInit
--  , openClReport
  ) where

import NeatInterpolation

openClInit :: String
openClInit = [string|
c = cl.create_some_context(interactive=False)
q = cl.CommandQueue(c)
setup_opencl(c, q)
|]

openClDecls ::String -> String -> String -> String
openClDecls pyopencl_code assign declare =
    kernelDeclarations ++ openclBoilerplate assign declare
    where kernelDeclarations =
            [string|fut_opencl_src = """
            ${pyopencl_code}"""|]
          openclBoilerplate assignBlock declareBlock = [string|

cl_group_size = 512


def setup_opencl(context_set, queue_set):
  global ctx
  global queue
  global program
  $declareBlock

  ctx = context_set
  queue = queue_set

  # Some drivers complain if we compile empty programs, so bail out early if so.
  if (len(fut_opencl_src) == 0):
    assert True

  program = cl.Program(ctx, fut_opencl_src).build(["-DFUT_BLOCK_DIM={}".format(FUT_BLOCK_DIM), "-DWAVE_SIZE=32"])

  $assignBlock
|]
