{-# LANGUAGE FlexibleContexts #-}
module Futhark.CodeGen.Backends.PyOpenCL
  ( compileProg
  ) where

import Control.Monad

import Futhark.Error
import Futhark.Representation.ExplicitMemory (Prog, ExplicitMemory)
import Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
import qualified Futhark.CodeGen.Backends.GenericPython as Py
import qualified Futhark.CodeGen.ImpCode.OpenCL as Imp
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.Backends.GenericPython.Definitions
import Futhark.MonadFreshNames


--maybe pass the config file rather than multiple arguments
compileProg :: MonadFreshNames m =>
               Maybe String -> Prog ExplicitMemory ->  m (Either InternalError String)
compileProg module_name prog = do
  res <- ImpGen.compileProg prog
  --could probably be a better why do to this..
  case res of
    Left err -> return $ Left err
    Right (Imp.Program opencl_code opencl_prelude kernel_names types sizes prog')  -> do
      --prepare the strings for assigning the kernels and set them as global
      let assign = unlines $ map (\x -> pretty $ Assign (Var ("self."++x++"_var")) (Var $ "program."++x)) kernel_names

      let defines =
            [Assign (Var "synchronous") $ Bool False,
             Assign (Var "preferred_platform") None,
             Assign (Var "preferred_device") None,
             Assign (Var "default_threshold") None,
             Assign (Var "default_group_size") None,
             Assign (Var "default_num_groups") None,
             Assign (Var "default_tile_size") None,
             Assign (Var "fut_opencl_src") $ RawStringLiteral $ opencl_prelude ++ opencl_code,
             Escape pyValues,
             Escape pyFunctions,
             Escape pyPanic,
             Escape pyTuning]
      let imports = [Import "sys" Nothing,
                     Import "numpy" $ Just "np",
                     Import "ctypes" $ Just "ct",
                     Escape openClPrelude,
                     Import "pyopencl.array" Nothing,
                     Import "time" Nothing]

      let constructor = Py.Constructor [ "self"
                                       , "command_queue=None"
                                       , "interactive=False"
                                       , "platform_pref=preferred_platform"
                                       , "device_pref=preferred_device"
                                       , "default_group_size=default_group_size"
                                       , "default_num_groups=default_num_groups"
                                       , "default_tile_size=default_tile_size"
                                       , "default_threshold=default_threshold"
                                       , "sizes=sizes"]
                        [Escape $ openClInit types assign sizes]
          options = [ Option { optionLongName = "platform"
                             , optionShortName = Just 'p'
                             , optionArgument = RequiredArgument "str"
                             , optionAction =
                               [ Assign (Var "preferred_platform") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "device"
                             , optionShortName = Just 'd'
                             , optionArgument = RequiredArgument "str"
                             , optionAction =
                               [ Assign (Var "preferred_device") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "default-threshold"
                             , optionShortName = Nothing
                             , optionArgument = RequiredArgument "int"
                             , optionAction =
                               [ Assign (Var "default_threshold") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "default-group-size"
                             , optionShortName = Nothing
                             , optionArgument = RequiredArgument "int"
                             , optionAction =
                               [ Assign (Var "default_group_size") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "default-num-groups"
                             , optionShortName = Nothing
                             , optionArgument = RequiredArgument "int"
                             , optionAction =
                               [ Assign (Var "default_num_groups") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "default-tile-size"
                             , optionShortName = Nothing
                             , optionArgument = RequiredArgument "int"
                             , optionAction =
                               [ Assign (Var "default_tile_size") $ Var "optarg" ]
                             }
                    , Option { optionLongName = "size"
                             , optionShortName = Nothing
                             , optionArgument = RequiredArgument "size_assignment"
                             , optionAction =
                                 [Assign (Index (Var "sizes")
                                          (IdxExp (Index (Var "optarg")
                                                   (IdxExp (Integer 0)))))
                                   (Index (Var "optarg") (IdxExp (Integer 1)))
                                 ]
                             }
                    ]

      Right <$> Py.compileProg module_name constructor imports defines operations ()
        [Exp $ Py.simpleCall "self.queue.finish" []] options prog'
  where operations :: Py.Operations Imp.OpenCL ()
        operations = Py.Operations
                     { Py.opsCompiler = callKernel
                     , Py.opsWriteScalar = writeOpenCLScalar
                     , Py.opsReadScalar = readOpenCLScalar
                     , Py.opsAllocate = allocateOpenCLBuffer
                     , Py.opsCopy = copyOpenCLMemory
                     , Py.opsStaticArray = staticOpenCLArray
                     , Py.opsEntryOutput = packArrayOutput
                     , Py.opsEntryInput = unpackArrayInput
                     }

-- We have many casts to 'long', because PyOpenCL may get confused at
-- the 32-bit numbers that ImpCode uses for offsets and the like.
asLong :: PyExp -> PyExp
asLong x = Py.simpleCall "np.long" [x]

callKernel :: Py.OpCompiler Imp.OpenCL ()
callKernel (Imp.GetSize v key) =
  Py.stm $ Assign (Var (Py.compileName v)) $
  Index (Var "self.sizes") (IdxExp $ String $ pretty key)
callKernel (Imp.CmpSizeLe v key x) = do
  x' <- Py.compileExp x
  Py.stm $ Assign (Var (Py.compileName v)) $
    BinOp "<=" (Index (Var "self.sizes") (IdxExp $ String $ pretty key)) x'
callKernel (Imp.GetSizeMax v size_class) =
  Py.stm $ Assign (Var (Py.compileName v)) $
  Var $ "self.max_" ++ pretty size_class
callKernel (Imp.HostCode c) =
  Py.compileCode c

callKernel (Imp.LaunchKernel name args num_workgroups workgroup_size) = do
  num_workgroups' <- mapM (fmap asLong . Py.compileExp) num_workgroups
  workgroup_size' <- mapM (fmap asLong . Py.compileExp) workgroup_size
  let kernel_size = zipWith mult_exp num_workgroups' workgroup_size'
      total_elements = foldl mult_exp (Integer 1) kernel_size
      cond = BinOp "!=" total_elements (Integer 0)
  body <- Py.collect $ launchKernel name kernel_size workgroup_size' args
  Py.stm $ If cond body []
  where mult_exp = BinOp "*"

launchKernel :: String -> [PyExp] -> [PyExp] -> [Imp.KernelArg]
             -> Py.CompilerM op s ()
launchKernel kernel_name kernel_dims workgroup_dims args = do
  let kernel_dims' = Tuple kernel_dims
      workgroup_dims' = Tuple workgroup_dims
      kernel_name' = "self." ++ kernel_name ++ "_var"
  args' <- mapM processKernelArg args
  Py.stm $ Exp $ Py.simpleCall (kernel_name' ++ ".set_args") args'
  Py.stm $ Exp $ Py.simpleCall "cl.enqueue_nd_range_kernel"
    [Var "self.queue", Var kernel_name', kernel_dims', workgroup_dims']
  finishIfSynchronous
  where processKernelArg :: Imp.KernelArg -> Py.CompilerM op s PyExp
        processKernelArg (Imp.ValueKArg e bt) = do
          e' <- Py.compileExp e
          return $ Py.simpleCall (Py.compilePrimToNp bt) [e']
        processKernelArg (Imp.MemKArg v) = return $ Var $ Py.compileName v
        processKernelArg (Imp.SharedMemoryKArg (Imp.Count num_bytes)) = do
          num_bytes' <- Py.compileExp num_bytes
          return $ Py.simpleCall "cl.LocalMemory" [asLong num_bytes']

writeOpenCLScalar :: Py.WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  let mem' = Var $ Py.compileName mem
  let nparr = Call (Var "np.array")
              [Arg val, ArgKeyword "dtype" $ Var $ Py.compilePrimType bt]
  Py.stm $ Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg mem', Arg nparr,
     ArgKeyword "device_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
     ArgKeyword "is_blocking" $ Var "synchronous"]

writeOpenCLScalar _ _ _ space _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: Py.ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName "read_res"
  let val' = Var $ pretty val
  let mem' = Var $ Py.compileName mem
  let nparr = Call (Var "np.empty")
              [Arg $ Integer 1,
               ArgKeyword "dtype" (Var $ Py.compilePrimType bt)]
  Py.stm $ Assign val' nparr
  Py.stm $ Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg val', Arg mem',
     ArgKeyword "device_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
     ArgKeyword "is_blocking" $ Bool True]
  return $ Index val' $ IdxExp $ Integer 0

readOpenCLScalar _ _ _ space =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: Py.Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" =
  Py.stm $ Assign (Var $ Py.compileName mem) $
  Py.simpleCall "opencl_alloc" [Var "self", size, String $ pretty mem]

allocateOpenCLBuffer _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space"

copyOpenCLMemory :: Py.Copy Imp.OpenCL ()
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx (Imp.Space "device") nbytes bt = do
  let srcmem'  = Var $ Py.compileName srcmem
  let destmem' = Var $ Py.compileName destmem
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
  let end = BinOp "+" destidx divide
  let dest = Index destmem' (IdxRange destidx end)
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg dest, Arg srcmem',
     ArgKeyword "device_offset" $ asLong srcidx,
     ArgKeyword "is_blocking" $ Var "synchronous"]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx Imp.DefaultSpace nbytes bt = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
  let end = BinOp "+" srcidx divide
  let src = Index srcmem' (IdxRange srcidx end)
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg destmem', Arg src,
     ArgKeyword "device_offset" $ asLong destidx,
     ArgKeyword "is_blocking" $ Var "synchronous"]

copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx (Imp.Space "device") nbytes _ = do
  let destmem' = Var $ Py.compileName destmem
  let srcmem'  = Var $ Py.compileName srcmem
  Py.stm $ ifNotZeroSize nbytes $
    Exp $ Call (Var "cl.enqueue_copy")
    [Arg $ Var "self.queue", Arg destmem', Arg srcmem',
     ArgKeyword "dest_offset" $ asLong destidx,
     ArgKeyword "src_offset" $ asLong srcidx,
     ArgKeyword "byte_count" $ asLong nbytes]
  finishIfSynchronous

copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  Py.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes

copyOpenCLMemory _ _ destspace _ _ srcspace _ _=
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: Py.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ Py.atInit <=< Py.collect $ do
    -- Create host-side Numpy array with intended values.
    Py.stm $ Assign (Var name') $ case vs of
      Imp.ArrayValues vs' ->
        Call (Var "np.array")
        [Arg $ List $ map Py.compilePrimValue vs',
         ArgKeyword "dtype" $ Var $ Py.compilePrimToNp t]
      Imp.ArrayZeros n ->
        Call (Var "np.zeros")
        [Arg $ Integer $ fromIntegral n,
         ArgKeyword "dtype" $ Var $ Py.compilePrimToNp t]

    let num_elems = case vs of Imp.ArrayValues vs' -> length vs'
                               Imp.ArrayZeros n -> n

    -- Create memory block on the device.
    static_mem <- newVName "static_mem"
    let size = Integer $ toInteger num_elems * Imp.primByteSize t
    allocateOpenCLBuffer static_mem size "device"

    -- Copy Numpy array to the device memory block.
    Py.stm $ ifNotZeroSize size $
      Exp $ Call (Var "cl.enqueue_copy")
      [Arg $ Var "self.queue",
       Arg $ Var $ Py.compileName static_mem,
       Arg $ Call (Var "normaliseArray") [Arg (Var name')],
       ArgKeyword "is_blocking" $ Var "synchronous"]

    -- Store the memory block for later reference.
    Py.stm $ Assign (Field (Var "self") name') $
      Var $ Py.compileName static_mem

  Py.stm $ Assign (Var name') (Field (Var "self") name')
  where name' = Py.compileName name
staticOpenCLArray _ space _ _ =
  error $ "PyOpenCL backend cannot create static array in memory space '" ++ space ++ "'"

packArrayOutput :: Py.EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims =
  return $ Call (Var "cl.array.Array")
  [Arg $ Var "self.queue",
   Arg $ Tuple $ map Py.compileDim dims,
   Arg $ Var $ Py.compilePrimTypeExt bt ept,
   ArgKeyword "data" $ Var $ Py.compileName mem]
packArrayOutput _ sid _ _ _ =
  error $ "Cannot return array from " ++ sid ++ " space."

unpackArrayInput :: Py.EntryInput Imp.OpenCL ()
unpackArrayInput mem "device" t s dims e = do
  let type_is_ok =
        BinOp "and"
        (BinOp "in" (Py.simpleCall "type" [e]) (List [Var "np.ndarray", Var "cl.array.Array"]))
        (BinOp "==" (Field e "dtype") (Var (Py.compilePrimToExtNp t s)))
  Py.stm $ Assert type_is_ok $ String "Parameter has unexpected type"

  zipWithM_ (Py.unpackDim e) dims [0..]

  let memsize' = Py.simpleCall "np.int64" [Field e "nbytes"]
      pyOpenCLArrayCase =
        [Assign mem_dest $ Field e "data"]
  numpyArrayCase <- Py.collect $ do
    allocateOpenCLBuffer mem memsize' "device"
    Py.stm $ ifNotZeroSize memsize' $
      Exp $ Call (Var "cl.enqueue_copy")
      [Arg $ Var "self.queue",
       Arg $ Var $ Py.compileName mem,
       Arg $ Call (Var "normaliseArray") [Arg e],
       ArgKeyword "is_blocking" $ Var "synchronous"]

  Py.stm $ If (BinOp "==" (Py.simpleCall "type" [e]) (Var "cl.array.Array"))
    pyOpenCLArrayCase
    numpyArrayCase
  where mem_dest = Var $ Py.compileName mem
unpackArrayInput _ sid _ _ _ _ =
  error $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: PyExp -> PyStmt -> PyStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: Py.CompilerM op s ()
finishIfSynchronous =
  Py.stm $ If (Var "synchronous") [Exp $ Py.simpleCall "self.queue.finish" []] []
