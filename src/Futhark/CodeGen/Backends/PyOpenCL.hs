-- | Code generation for Python with OpenCL.
module Futhark.CodeGen.Backends.PyOpenCL
  ( compileProg,
  )
where

import Control.Monad
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericPython hiding (compileProg)
import Futhark.CodeGen.Backends.GenericPython qualified as GP
import Futhark.CodeGen.Backends.GenericPython.AST
import Futhark.CodeGen.Backends.GenericPython.Options
import Futhark.CodeGen.Backends.PyOpenCL.Boilerplate
import Futhark.CodeGen.ImpCode (Count (..))
import Futhark.CodeGen.ImpCode.OpenCL qualified as Imp
import Futhark.CodeGen.ImpGen.OpenCL qualified as ImpGen
import Futhark.CodeGen.RTS.Python (openclPy)
import Futhark.IR.GPUMem (GPUMem, Prog)
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeText)
import Futhark.Util.Pretty (prettyString, prettyText)

-- | Compile the program to Python with calls to OpenCL.
compileProg ::
  (MonadFreshNames m) =>
  CompilerMode ->
  String ->
  Prog GPUMem ->
  m (ImpGen.Warnings, T.Text)
compileProg mode class_name prog = do
  ( ws,
    Imp.Program
      opencl_code
      opencl_prelude
      macros
      kernels
      types
      sizes
      failures
      prog'
    ) <-
    ImpGen.compileProg prog
  -- prepare the strings for assigning the kernels and set them as global
  let assign =
        unlines
          $ map
            ( \x ->
                prettyString $
                  Assign
                    (Var (T.unpack ("self." <> zEncodeText (nameToText x) <> "_var")))
                    (Var $ T.unpack $ "program." <> zEncodeText (nameToText x))
            )
          $ M.keys kernels

  let defines =
        [ Assign (Var "synchronous") $ Bool False,
          Assign (Var "preferred_platform") None,
          Assign (Var "build_options") $ List [],
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
        Constructor
          [ "self",
            "build_options=build_options",
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
          [Escape $ openClInit macros types assign sizes failures]
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
            { optionLongName = "build-option",
              optionShortName = Nothing,
              optionArgument = RequiredArgument "str",
              optionAction =
                [ Assign (Var "build_options") $
                    BinOp "+" (Var "build_options") $
                      List [Var "optarg"]
                ]
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
    <$> GP.compileProg
      mode
      class_name
      constructor
      imports
      defines
      operations
      ()
      [Exp $ simpleCall "sync" [Var "self"]]
      options
      prog'
  where
    operations :: Operations Imp.OpenCL ()
    operations =
      Operations
        { opsCompiler = callKernel,
          opsWriteScalar = writeOpenCLScalar,
          opsReadScalar = readOpenCLScalar,
          opsAllocate = allocateOpenCLBuffer,
          opsCopies =
            M.insert (Imp.Space "device", Imp.Space "device") copygpu2gpu $
              opsCopies defaultOperations,
          opsEntryOutput = packArrayOutput,
          opsEntryInput = unpackArrayInput
        }

-- We have many casts to 'long', because PyOpenCL may get confused at
-- the 32-bit numbers that ImpCode uses for offsets and the like.
asLong :: PyExp -> PyExp
asLong x = simpleCall "np.int64" [x]

getParamByKey :: Name -> PyExp
getParamByKey key = Index (Var "self.sizes") (IdxExp $ String $ prettyText key)

kernelConstToExp :: Imp.KernelConst -> PyExp
kernelConstToExp (Imp.SizeConst key _) =
  getParamByKey key
kernelConstToExp (Imp.SizeMaxConst size_class) =
  Var $ "self.max_" <> prettyString size_class

compileBlockDim :: Imp.BlockDim -> CompilerM op s PyExp
compileBlockDim (Left e) = asLong <$> compileExp e
compileBlockDim (Right kc) = pure $ kernelConstToExp kc

callKernel :: OpCompiler Imp.OpenCL ()
callKernel (Imp.GetSize v key) = do
  v' <- compileVar v
  stm $ Assign v' $ getParamByKey key
callKernel (Imp.CmpSizeLe v key x) = do
  v' <- compileVar v
  x' <- compileExp x
  stm $ Assign v' $ BinOp "<=" (getParamByKey key) x'
callKernel (Imp.GetSizeMax v size_class) = do
  v' <- compileVar v
  stm $ Assign v' $ kernelConstToExp $ Imp.SizeMaxConst size_class
callKernel (Imp.LaunchKernel safety name shared_memory args num_threadblocks workgroup_size) = do
  num_threadblocks' <- mapM (fmap asLong . compileExp) num_threadblocks
  workgroup_size' <- mapM compileBlockDim workgroup_size
  let kernel_size = zipWith mult_exp num_threadblocks' workgroup_size'
      total_elements = foldl mult_exp (Integer 1) kernel_size
      cond = BinOp "!=" total_elements (Integer 0)
  shared_memory' <- compileExp $ Imp.untyped $ Imp.unCount shared_memory
  body <- collect $ launchKernel name safety kernel_size workgroup_size' shared_memory' args
  stm $ If cond body []

  when (safety >= Imp.SafetyFull) $
    stm $
      Assign (Var "self.failure_is_an_option") $
        compilePrimValue (Imp.IntValue (Imp.Int32Value 1))
  where
    mult_exp = BinOp "*"

launchKernel ::
  Imp.KernelName ->
  Imp.KernelSafety ->
  [PyExp] ->
  [PyExp] ->
  PyExp ->
  [Imp.KernelArg] ->
  CompilerM op s ()
launchKernel kernel_name safety kernel_dims threadblock_dims shared_memory args = do
  let kernel_dims' = Tuple kernel_dims
      threadblock_dims' = Tuple threadblock_dims
      kernel_name' = "self." <> zEncodeText (nameToText kernel_name) <> "_var"
  args' <- mapM processKernelArg args
  let failure_args =
        take
          (Imp.numFailureParams safety)
          [ Var "self.global_failure",
            Var "self.failure_is_an_option",
            Var "self.global_failure_args"
          ]
  stm . Exp $
    simpleCall (T.unpack $ kernel_name' <> ".set_args") $
      [simpleCall "cl.LocalMemory" [simpleCall "max" [shared_memory, Integer 1]]]
        ++ failure_args
        ++ args'
  stm . Exp $
    simpleCall
      "cl.enqueue_nd_range_kernel"
      [Var "self.queue", Var (T.unpack kernel_name'), kernel_dims', threadblock_dims']
  finishIfSynchronous
  where
    processKernelArg :: Imp.KernelArg -> CompilerM op s PyExp
    processKernelArg (Imp.ValueKArg e bt) = toStorage bt <$> compileExp e
    processKernelArg (Imp.MemKArg v) = compileVar v

writeOpenCLScalar :: WriteScalar Imp.OpenCL ()
writeOpenCLScalar mem i bt "device" val = do
  let nparr =
        Call
          (Var "np.array")
          [Arg val, ArgKeyword "dtype" $ Var $ compilePrimType bt]
  stm $
    Exp $
      Call
        (Var "cl.enqueue_copy")
        [ Arg $ Var "self.queue",
          Arg mem,
          Arg nparr,
          ArgKeyword "dst_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
          ArgKeyword "is_blocking" $ Var "synchronous"
        ]
writeOpenCLScalar _ _ _ space _ =
  error $ "Cannot write to '" ++ space ++ "' memory space."

readOpenCLScalar :: ReadScalar Imp.OpenCL ()
readOpenCLScalar mem i bt "device" = do
  val <- newVName "read_res"
  let val' = Var $ prettyString val
  let nparr =
        Call
          (Var "np.empty")
          [ Arg $ Integer 1,
            ArgKeyword "dtype" (Var $ compilePrimType bt)
          ]
  stm $ Assign val' nparr
  stm $
    Exp $
      Call
        (Var "cl.enqueue_copy")
        [ Arg $ Var "self.queue",
          Arg val',
          Arg mem,
          ArgKeyword "src_offset" $ BinOp "*" (asLong i) (Integer $ Imp.primByteSize bt),
          ArgKeyword "is_blocking" $ Var "synchronous"
        ]
  stm $ Exp $ simpleCall "sync" [Var "self"]
  pure $ Index val' $ IdxExp $ Integer 0
readOpenCLScalar _ _ _ space =
  error $ "Cannot read from '" ++ space ++ "' memory space."

allocateOpenCLBuffer :: Allocate Imp.OpenCL ()
allocateOpenCLBuffer mem size "device" =
  stm $
    Assign mem $
      simpleCall "opencl_alloc" [Var "self", size, String $ prettyText mem]
allocateOpenCLBuffer _ _ space =
  error $ "Cannot allocate in '" ++ space ++ "' space"

packArrayOutput :: EntryOutput Imp.OpenCL ()
packArrayOutput mem "device" bt ept dims = do
  mem' <- compileVar mem
  dims' <- mapM compileDim dims
  pure $
    Call
      (Var "cl.array.Array")
      [ Arg $ Var "self.queue",
        Arg $ Tuple $ dims' <> [Integer 0 | bt == Imp.Unit],
        Arg $ Var $ compilePrimToExtNp bt ept,
        ArgKeyword "data" mem'
      ]
packArrayOutput _ sid _ _ _ =
  error $ "Cannot return array from " ++ sid ++ " space."

unpackArrayInput :: EntryInput Imp.OpenCL ()
unpackArrayInput mem "device" t s dims e = do
  let type_is_ok =
        BinOp
          "and"
          (BinOp "in" (simpleCall "type" [e]) (List [Var "np.ndarray", Var "cl.array.Array"]))
          (BinOp "==" (Field e "dtype") (Var (compilePrimToExtNp t s)))
  stm $ Assert type_is_ok $ String "Parameter has unexpected type"

  zipWithM_ (unpackDim e) dims [0 ..]

  let memsize' = simpleCall "np.int64" [Field e "nbytes"]
      pyOpenCLArrayCase =
        [Assign mem $ Field e "data"]
  numpyArrayCase <- collect $ do
    allocateOpenCLBuffer mem memsize' "device"
    stm $
      ifNotZeroSize memsize' $
        Exp $
          Call
            (Var "cl.enqueue_copy")
            [ Arg $ Var "self.queue",
              Arg mem,
              Arg $ Call (Var "normaliseArray") [Arg e],
              ArgKeyword "is_blocking" $ Var "synchronous"
            ]

  stm $
    If
      (BinOp "==" (simpleCall "type" [e]) (Var "cl.array.Array"))
      pyOpenCLArrayCase
      numpyArrayCase
unpackArrayInput _ sid _ _ _ _ =
  error $ "Cannot accept array from " ++ sid ++ " space."

ifNotZeroSize :: PyExp -> PyStmt -> PyStmt
ifNotZeroSize e s =
  If (BinOp "!=" e (Integer 0)) [s] []

finishIfSynchronous :: CompilerM op s ()
finishIfSynchronous =
  stm $ If (Var "synchronous") [Exp $ simpleCall "sync" [Var "self"]] []

copygpu2gpu :: DoCopy op s
copygpu2gpu t shape dst (dstoffset, dststride) src (srcoffset, srcstride) = do
  stm . Exp . simpleCall "lmad_copy_gpu2gpu" $
    [ Var "self",
      Var (compilePrimType t),
      dst,
      unCount dstoffset,
      List (map unCount dststride),
      src,
      unCount srcoffset,
      List (map unCount srcstride),
      List (map unCount shape)
    ]
