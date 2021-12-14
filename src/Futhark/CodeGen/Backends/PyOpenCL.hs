{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- | Code generation for Python with OpenCL.
module Futhark.CodeGen.Backends.PyOpenCL
  ( compileProg,
  )
where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Futhark.CodeGen.Backends.GenericPython as Py
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
import qualified Futhark.CodeGen.ImpCode.OpenCL as Imp
import qualified Futhark.CodeGen.ImpGen.OpenCL as ImpGen
import Futhark.CodeGen.RTS.Python (openclPy)
import Futhark.IR.GPUMem (GPUMem, Prog)
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)
import Futhark.Util.Pretty (pretty)

-- | Compile the program to Python with calls to OpenCL.
compileProg ::
  MonadFreshNames m =>
  Py.CompilerMode ->
  String ->
  Prog GPUMem ->
  m (ImpGen.Warnings, T.Text)
compileProg mode class_name prog = do
  ( ws,
    Imp.Program
      opencl_code
      opencl_prelude
      kernels
      types
      sizes
      failures
      prog'
    ) <-
    ImpGen.compileProg prog
  --prepare the strings for assigning the kernels and set them as global
  let assign =
        unlines $
          map
            ( \x ->
                pretty $
                  Assign
                    (Var ("self." ++ zEncodeString (nameToString x) ++ "_var"))
                    (Var $ "program." ++ zEncodeString (nameToString x))
            )
            $ M.keys kernels

  let defines =
        [ Assign (Var "synchronous") $ Bool False,
          Assign (Var "preferred_platform") None,
          Assign (Var "preferred_device") None,
          Assign (Var "default_threshold") None,
          Assign (Var "default_group_size") None,
          Assign (Var "default_num_groups") None,
          Assign (Var "default_tile_size") None,
          Assign (Var "default_reg_tile_size") None,
          Assign (Var "fut_opencl_src") $ RawStringLiteral $ opencl_prelude <> opencl_code
        ]

  let imports =
        [ Import "sys" Nothing,
          Import "numpy" $ Just "np",
          Import "ctypes" $ Just "ct",
          Escape openclPy,
          Import "pyopencl.array" Nothing,
          Import "time" Nothing
        ]

  let constructor =
        Py.Constructor
          [ "self",
            "command_queue=None",
            "interactive=False",
            "platform_pref=preferred_platform",
            "device_pref=preferred_device",
            "default_group_size=default_group_size",
            "default_num_groups=default_num_groups",
            "default_tile_size=default_tile_size",
            "default_reg_tile_size=default_reg_tile_size",
            "default_threshold=default_threshold",
            "sizes=sizes"
          ]
          [Escape $ openClInit types assign sizes failures]
      options =
        [ Option
            { optionLongName = "platform",
              optionShortName = Just 'p',
              optionArgument = RequiredArgument "str",
              optionAction =
                [Assign (Var "preferred_platform") $ Var "optarg"]
            },
          Option
            { optionLongName = "device",
              optionShortName = Just 'd',
              optionArgument = RequiredArgument "str",
              optionAction =
                [Assign (Var "preferred_device") $ Var "optarg"]
            },
          Option
            { optionLongName = "default-threshold",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "int",
              optionAction =
                [Assign (Var "default_threshold") $ Var "optarg"]
            },
          Option
            { optionLongName = "default-group-size",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "int",
              optionAction =
                [Assign (Var "default_group_size") $ Var "optarg"]
            },
          Option
            { optionLongName = "default-num-groups",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "int",
              optionAction =
                [Assign (Var "default_num_groups") $ Var "optarg"]
            },
          Option
            { optionLongName = "default-tile-size",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "int",
              optionAction =
                [Assign (Var "default_tile_size") $ Var "optarg"]
            },
          Option
            { optionLongName = "default-reg-tile-size",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "int",
              optionAction =
                [Assign (Var "default_reg_tile_size") $ Var "optarg"]
            },
          Option
            { optionLongName = "param",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "param_assignment",
              optionAction =
                [ Assign
                    ( Index
                        (Var "params")
                        ( IdxExp
                            ( Index
                                (Var "optarg")
                                (IdxExp (Integer 0))
                            )
                        )
                    )
                    (Index (Var "optarg") (IdxExp (Integer 1)))
                ]
            }
        ]

  (ws,)
    <$> Py.compileProg
      mode
      class_name
      constructor
      imports
      defines
      operations
      ()
      [Exp $ Py.simpleCall "sync" [Var "self"]]
      options
      prog'
  where
    operations :: Py.Operations Imp.OpenCL ()
    operations =
      Py.Operations
        { Py.opsCompiler = callKernel,
          Py.opsWriteScalar = writeOpenCLScalar,
          Py.opsReadScalar = readOpenCLScalar,
          Py.opsAllocate = allocateOpenCLBuffer,
          Py.opsCopy = copyOpenCLMemory,
          Py.opsStaticArray = staticOpenCLArray,
          Py.opsEntryOutput = packArrayOutput,
          Py.opsEntryInput = unpackArrayInput
        }

-- We have many casts to 'long', because PyOpenCL may get confused at
-- the 32-bit numbers that ImpCode uses for offsets and the like.
asLong :: PyExp -> PyExp
asLong x = Py.simpleCall "np.int64" [x]

callKernel :: Py.OpCompiler Imp.OpenCL ()
callKernel (Imp.GetSize v key) = do
  v' <- Py.compileVar v
  Py.stm $
    Assign v' $
      Index (Var "self.sizes") (IdxExp $ String $ pretty key)
callKernel (Imp.CmpSizeLe v key x) = do
  v' <- Py.compileVar v
  x' <- Py.compileExp x
  Py.stm $
    Assign v' $
      BinOp "<=" (Index (Var "self.sizes") (IdxExp $ String $ pretty key)) x'
callKernel (Imp.GetSizeMax v size_class) = do
  v' <- Py.compileVar v
  Py.stm $
    Assign v' $
      Var $ "self.max_" ++ pretty size_class
callKernel (Imp.LaunchKernel safety name args num_workgroups workgroup_size) = do
  num_workgroups' <- mapM (fmap asLong . Py.compileExp) num_workgroups
  workgroup_size' <- mapM (fmap asLong . Py.compileExp) workgroup_size
  let kernel_size = zipWith mult_exp num_workgroups' workgroup_size'
      total_elements = foldl mult_exp (Integer 1) kernel_size
      cond = BinOp "!=" total_elements (Integer 0)

  body <- Py.collect $ launchKernel name safety kernel_size workgroup_size' args
  Py.stm $ If cond body []

  when (safety >= Imp.SafetyFull) $
    Py.stm $
      Assign (Var "self.failure_is_an_option") $
        Py.compilePrimValue (Imp.IntValue (Imp.Int32Value 1))
  where
    mult_exp = BinOp "*"

launchKernel ::
  Imp.KernelName ->
  Imp.KernelSafety ->
  [PyExp] ->
  [PyExp] ->
  [Imp.KernelArg] ->
  Py.CompilerM op s ()
launchKernel kernel_name safety kernel_dims workgroup_dims args = do
  let kernel_dims' = Tuple kernel_dims
      workgroup_dims' = Tuple workgroup_dims
      kernel_name' = "self." ++ zEncodeString (nameToString kernel_name) ++ "_var"
  args' <- mapM processKernelArg args
  let failure_args =
        take
          (Imp.numFailureParams safety)
          [ Var "self.global_failure",
            Var "self.failure_is_an_option",
            Var "self.global_failure_args"
          ]
  Py.stm $
    Exp $
      Py.simpleCall (kernel_name' ++ ".set_args") $
        failure_args ++ args'
  Py.stm $
    Exp $
      Py.simpleCall
        "cl.enqueue_nd_range_kernel"
        [Var "self.queue", Var kernel_name', kernel_dims', workgroup_dims']
  finishIfSynchronous
  where
    processKernelArg :: Imp.KernelArg -> Py.CompilerM op s PyExp
    processKernelArg (Imp.ValueKArg e bt) =
      Py.toStorage bt <$> Py.compileExp e
    processKernelArg (Imp.MemKArg v) = Py.compileVar v
    processKernelArg (Imp.SharedMemoryKArg (Imp.Count num_bytes)) = do
      num_bytes' <- Py.compileExp num_bytes
      return $ Py.simpleCall "cl.LocalMemory" [asLong num_bytes']

writeOpenCLScalar :: Py.WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  let nparr =
        Call
          (Var "np.array")
          [Arg val, ArgKeyword "dtype" $ Var $ Py.compilePrimType bt]
  Py.stm $
    Exp $
      Call
        (Var "cl.enqueue_copy")
        [ Arg $ Var "self.queue",
          Arg mem,
          Arg nparr,
          ArgKeyword "device_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
          ArgKeyword "is_blocking" $ Var "synchronous"
        ]
writeOpenCLScalar _ _ _ space _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: Py.ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName "read_res"
  let val' = Var $ pretty val
  let nparr =
        Call
          (Var "np.empty")
          [ Arg $ Integer 1,
            ArgKeyword "dtype" (Var $ Py.compilePrimType bt)
          ]
  Py.stm $ Assign val' nparr
  Py.stm $
    Exp $
      Call
        (Var "cl.enqueue_copy")
        [ Arg $ Var "self.queue",
          Arg val',
          Arg mem,
          ArgKeyword "device_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
          ArgKeyword "is_blocking" $ Var "synchronous"
        ]
  Py.stm $ Exp $ Py.simpleCall "sync" [Var "self"]
  return $ Index val' $ IdxExp $ Integer 0
readOpenCLScalar _ _ _ space =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: Py.Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" =
  Py.stm $
    Assign mem $
      Py.simpleCall "opencl_alloc" [Var "self", size, String $ pretty mem]
allocateOpenCLBuffer _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space"

copyOpenCLMemory :: Py.Copy Imp.OpenCL ()
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx (Imp.Space "device") nbytes bt = do
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
      end = BinOp "+" destidx divide
      dest = Index destmem (IdxRange destidx end)
  Py.stm $
    ifNotZeroSize nbytes $
      Exp $
        Call
          (Var "cl.enqueue_copy")
          [ Arg $ Var "self.queue",
            Arg dest,
            Arg srcmem,
            ArgKeyword "device_offset" $ asLong srcidx,
            ArgKeyword "is_blocking" $ Var "synchronous"
          ]
copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx Imp.DefaultSpace nbytes bt = do
  let divide = BinOp "//" nbytes (Integer $ Imp.primByteSize bt)
      end = BinOp "+" srcidx divide
      src = Index srcmem (IdxRange srcidx end)
  Py.stm $
    ifNotZeroSize nbytes $
      Exp $
        Call
          (Var "cl.enqueue_copy")
          [ Arg $ Var "self.queue",
            Arg destmem,
            Arg src,
            ArgKeyword "device_offset" $ asLong destidx,
            ArgKeyword "is_blocking" $ Var "synchronous"
          ]
copyOpenCLMemory destmem destidx (Imp.Space "device") srcmem srcidx (Imp.Space "device") nbytes _ = do
  Py.stm $
    ifNotZeroSize nbytes $
      Exp $
        Call
          (Var "cl.enqueue_copy")
          [ Arg $ Var "self.queue",
            Arg destmem,
            Arg srcmem,
            ArgKeyword "dest_offset" $ asLong destidx,
            ArgKeyword "src_offset" $ asLong srcidx,
            ArgKeyword "byte_count" $ asLong nbytes
          ]
  finishIfSynchronous
copyOpenCLMemory destmem destidx Imp.DefaultSpace srcmem srcidx Imp.DefaultSpace nbytes _ =
  Py.copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes
copyOpenCLMemory _ _ destspace _ _ srcspace _ _ =
  error $ "Cannot copy to " ++ show destspace ++ " from " ++ show srcspace

staticOpenCLArray :: Py.StaticArray Imp.OpenCL ()
staticOpenCLArray name "device" t vs = do
  mapM_ Py.atInit <=< Py.collect $ do
    -- Create host-side Numpy array with intended values.
    Py.stm $
      Assign (Var name') $ case vs of
        Imp.ArrayValues vs' ->
          Call
            (Var "np.array")
            [ Arg $ List $ map Py.compilePrimValue vs',
              ArgKeyword "dtype" $ Var $ Py.compilePrimToNp t
            ]
        Imp.ArrayZeros n ->
          Call
            (Var "np.zeros")
            [ Arg $ Integer $ fromIntegral n,
              ArgKeyword "dtype" $ Var $ Py.compilePrimToNp t
            ]

    let num_elems = case vs of
          Imp.ArrayValues vs' -> length vs'
          Imp.ArrayZeros n -> n

    -- Create memory block on the device.
    static_mem <- newVName "static_mem"
    let size = Integer $ toInteger num_elems * Imp.primByteSize t
    allocateOpenCLBuffer (Var (Py.compileName static_mem)) size "device"

    -- Copy Numpy array to the device memory block.
    Py.stm $
      ifNotZeroSize size $
        Exp $
          Call
            (Var "cl.enqueue_copy")
            [ Arg $ Var "self.queue",
              Arg $ Var $ Py.compileName static_mem,
              Arg $ Call (Var "normaliseArray") [Arg (Var name')],
              ArgKeyword "is_blocking" $ Var "synchronous"
            ]

    -- Store the memory block for later reference.
    Py.stm $
      Assign (Field (Var "self") name') $
        Var $ Py.compileName static_mem

  Py.stm $ Assign (Var name') (Field (Var "self") name')
  where
    name' = Py.compileName name
staticOpenCLArray _ space _ _ =
  error $ "PyOpenCL backend cannot create static array in memory space '" ++ space ++ "'"

packArrayOutput :: Py.EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims = do
  mem' <- Py.compileVar mem
  dims' <- mapM Py.compileDim dims
  return $
    Call
      (Var "cl.array.Array")
      [ Arg $ Var "self.queue",
        Arg $ Tuple dims',
        Arg $ Var $ Py.compilePrimToExtNp bt ept,
        ArgKeyword "data" mem'
      ]
packArrayOutput _ sid _ _ _ =
  error $ "Cannot return array from " ++ sid ++ " space."

unpackArrayInput :: Py.EntryInput Imp.OpenCL ()
unpackArrayInput mem "device" t s dims e = do
  let type_is_ok =
        BinOp
          "and"
          (BinOp "in" (Py.simpleCall "type" [e]) (List [Var "np.ndarray", Var "cl.array.Array"]))
          (BinOp "==" (Field e "dtype") (Var (Py.compilePrimToExtNp t s)))
  Py.stm $ Assert type_is_ok $ String "Parameter has unexpected type"

  zipWithM_ (Py.unpackDim e) dims [0 ..]

  let memsize' = Py.simpleCall "np.int64" [Field e "nbytes"]
      pyOpenCLArrayCase =
        [Assign mem $ Field e "data"]
  numpyArrayCase <- Py.collect $ do
    allocateOpenCLBuffer mem memsize' "device"
    Py.stm $
      ifNotZeroSize memsize' $
        Exp $
          Call
            (Var "cl.enqueue_copy")
            [ Arg $ Var "self.queue",
              Arg mem,
              Arg $ Call (Var "normaliseArray") [Arg e],
              ArgKeyword "is_blocking" $ Var "synchronous"
            ]

  Py.stm $
    If
      (BinOp "==" (Py.simpleCall "type" [e]) (Var "cl.array.Array"))
      pyOpenCLArrayCase
      numpyArrayCase
unpackArrayInput _ sid _ _ _ _ =
  error $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: PyExp -> PyStmt -> PyStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: Py.CompilerM op s ()
finishIfSynchronous =
  Py.stm $ If (Var "synchronous") [Exp $ Py.simpleCall "sync" [Var "self"]] []
