module Futhark.CodeGen.Backends.CSOpenCL.Boilerplate
  ( generateBoilerplate

  , kernelRuntime
  , kernelRuns
  ) where

import qualified Data.Map as M

import Futhark.CodeGen.ImpCode.OpenCL hiding (Index, If)
import Futhark.CodeGen.Backends.GenericCSharp as CS
import Futhark.CodeGen.Backends.GenericCSharp.AST as AST
import Futhark.CodeGen.OpenCL.Kernels


intT, longT, stringT, intArrayT, stringArrayT :: CSType
intT = Primitive $ CSInt Int32T
longT = Primitive $ CSInt Int64T
stringT = Primitive StringT
intArrayT = Composite $ ArrayT intT
stringArrayT = Composite $ ArrayT stringT

generateBoilerplate :: String -> String -> [String] -> [PrimType]
                    -> M.Map VName (SizeClass, Name)
                    -> CS.CompilerM OpenCL () ()
generateBoilerplate opencl_code opencl_prelude kernel_names types sizes = do
  final_inits <- CS.contextFinalInits

  let (opencl_fields, opencl_inits, top_decls, later_top_decls) =
        openClDecls kernel_names opencl_code opencl_prelude

  CS.stm top_decls

  CS.stm $ AssignTyped stringArrayT (Var "size_names")
    (Just $ Collection "string[]" (map (String . pretty) $ M.keys sizes))

  CS.stm $ AssignTyped stringArrayT (Var "size_classes")
    (Just $ Collection "string[]" (map (String . pretty . fst) $ M.elems sizes))

  CS.stm $ AssignTyped stringArrayT (Var "size_entry_points")
    (Just $ Collection "string[]" (map (String . pretty . snd) $ M.elems sizes))


  let get_num_sizes = CS.publicName "get_num_sizes"
  let get_size_name = CS.publicName "get_size_name"
  let get_size_class = CS.publicName "get_size_class"
  let get_size_entry = CS.publicName "get_size_entry"


  CS.stm $ CS.funDef get_num_sizes intT []
    [ Return $ (Integer . toInteger) $ M.size sizes ]
  CS.stm $ CS.funDef get_size_name (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "size_names") (IdxExp $ Var "i") ]
  CS.stm $ CS.funDef get_size_class (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "size_classes") (IdxExp $ Var "i") ]
  CS.stm $ CS.funDef get_size_entry (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "size_entry_points") (IdxExp $ Var "i") ]

  let cfg = CS.publicName "context_config"
  let new_cfg = CS.publicName "context_config_new"
  let cfg_set_debugging = CS.publicName "context_config_set_debugging"
  let cfg_set_device = CS.publicName "context_config_set_device"
  let cfg_set_platform = CS.publicName "context_config_set_platform"
  let cfg_dump_program_to = CS.publicName "context_config_dump_program_to"
  let cfg_load_program_from = CS.publicName "context_config_load_program_from"
  let cfg_set_default_group_size = CS.publicName "context_config_set_default_group_size"
  let cfg_set_default_num_groups = CS.publicName "context_config_set_default_num_groups"
  let cfg_set_default_tile_size = CS.publicName "context_config_set_default_tile_size"
  let cfg_set_default_threshold = CS.publicName "context_config_set_default_threshold"
  let cfg_set_size = CS.publicName "context_config_set_size"

  CS.stm $ StructDef "sizes" (map (\k -> (intT, pretty k)) $ M.keys sizes)
  CS.stm $ StructDef cfg [ (CustomT "opencl_config", "opencl")
                         , (intArrayT, "sizes")]

  let tmp_cfg = Var "tmp_cfg"
  CS.stm $ CS.funDef new_cfg (CustomT cfg) []
    [ Assign tmp_cfg $ CS.simpleInitClass cfg []
    , Reassign (Field tmp_cfg "sizes") (Collection "int[]" (replicate (M.size sizes) (Integer 0)))
    , Exp $ CS.simpleCall "opencl_config_init" [ Out $ Field tmp_cfg "opencl", (Integer . toInteger) $ M.size sizes
                                               , Var "size_names", Field tmp_cfg "sizes", Var "size_classes" ]
    , Reassign (Field tmp_cfg "opencl.transpose_block_dim") (Integer transposeBlockDim)
    , Return tmp_cfg
    ]

  CS.stm $ CS.funDef cfg_set_debugging VoidT [(RefT $ CustomT cfg, "_cfg"),(Primitive BoolT, "flag")]
    [Reassign (Var "_cfg.opencl.debugging") (Var "flag")]

  CS.stm $ CS.funDef cfg_set_device VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "set_preferred_device" [Ref $ Var "_cfg.opencl", Var "s"]]

  CS.stm $ CS.funDef cfg_set_platform VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "set_preferred_platform" [Ref $ Var "_cfg.opencl", Var "s"]]

  CS.stm $ CS.funDef cfg_dump_program_to VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "path")]
    [Reassign (Var "_cfg.opencl.dump_program_to") (Var "path")]

  CS.stm $ CS.funDef cfg_load_program_from VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "path")]
    [Reassign (Var "_cfg.opencl.load_program_from") (Var "path")]

  CS.stm $ CS.funDef cfg_set_default_group_size VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.opencl.default_group_size") (Var "size")]

  CS.stm $ CS.funDef cfg_set_default_num_groups VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "num")]
    [Reassign (Var "_cfg.opencl.default_num_groups") (Var "num")]


  CS.stm $ CS.funDef cfg_set_default_tile_size VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.opencl.default_tile_size") (Var "size")]

  CS.stm $ CS.funDef cfg_set_default_threshold VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.opencl.default_threshold") (Var "size")]

  CS.stm $ CS.funDef cfg_set_size (Primitive BoolT) [(RefT $ CustomT cfg, "_cfg")
                                                    , (stringT, "size_name")
                                                    , (intT, "size_value")]
    [ AST.For "i" ((Integer . toInteger) $ M.size sizes)
      [ If (BinOp "==" (Var "size_name") (Index (Var "size_names") (IdxExp (Var "i"))))
          [ Reassign (Index (Var "_cfg.sizes") (IdxExp (Var "i"))) (Var "size_value")
          , Return (AST.Bool True)] []
      ]
    , Return $ AST.Bool False ]


  let ctx_ = CS.publicName "context"
  let new_ctx = CS.publicName "context_new"
  let sync_ctx = CS.publicName "context_sync"
--  (fields, init_fields) <- GC.contextContents

  CS.stm $ StructDef ctx_ $
    [ (Primitive IntPtrT, "NULL")
    , (CustomT "CLMemoryHandle", "EMPTY_MEM_HANDLE")
    , (CustomT "opencl_memblock", "EMPTY_MEMBLOCK")
    , (CustomT "opencl_free_list", "free_list")
    , (Primitive $ CSInt Int64T, "cur_mem_usage_device")
    , (Primitive $ CSInt Int64T, "peak_mem_usage_device")
    , (Primitive BoolT, "detail_memory")
    , (Primitive BoolT, "debugging")
    , (CustomT "opencl_context", "opencl")
    , (CustomT "sizes", "sizes") ]
    ++ opencl_fields

  mapM_ CS.stm later_top_decls

  CS.addMemberDecl $ AssignTyped (CustomT cfg) (Var "cfg") Nothing
  CS.addMemberDecl $ AssignTyped (CustomT ctx_) (Var "ctx") Nothing

  CS.beforeParse $ Reassign (Var "cfg") $ CS.simpleCall new_cfg []
  CS.atInit $ Exp $ CS.simpleCall new_ctx [Var "cfg"]
  CS.atInit $ Reassign (Var "ctx.EMPTY_MEM_HANDLE") $ CS.simpleCall "empty_mem_handle" [Var "ctx.opencl.context"]
  CS.atInit $ Reassign (Var "ctx.EMPTY_MEMBLOCK") $ CS.simpleCall "empty_memblock" [Var "ctx.EMPTY_MEM_HANDLE"]
  CS.atInit $ Reassign (Var "ctx.free_list") $ CS.simpleCall "opencl_free_list_init" []

  CS.addMemberDecl $ AssignTyped (Primitive BoolT) (Var "synchronous") (Just $ AST.Bool False)

  let set_required_types = [Reassign (Var "required_types") (AST.Bool True)
                           | FloatType Float64 `elem` types]

      set_sizes = zipWith (\i k -> Reassign (Field (Var "ctx.sizes") (pretty k))
                                            (Index (Var "cfg.sizes") (IdxExp $ (Integer . toInteger) i)))
                          [(0::Int)..] $ M.keys sizes


  CS.stm $ CS.funDef new_ctx VoidT [(CustomT cfg, "cfg")] $
    [ AssignTyped (CustomT "ComputeErrorCode") (Var "error") Nothing
    , Reassign (Var "ctx.detail_memory") (Var "cfg.opencl.debugging")
    , Reassign (Var "ctx.debugging") (Var "cfg.opencl.debugging")
    , Reassign (Var "ctx.opencl.cfg") (Var "cfg.opencl")]
    ++ opencl_inits ++
    [ Assign (Var "required_types") (AST.Bool False) ]
    ++ set_required_types ++
    [ AssignTyped (CustomT "CLProgramHandle") (Var "prog")
        (Just $ CS.simpleCall "setup_opencl" [ Ref $ Var "ctx"
                                             , Var "opencl_program"
                                             , Var "required_types"])]
    ++ concatMap loadKernelByName kernel_names
    ++ final_inits
    ++ set_sizes

  CS.stm $ CS.funDef sync_ctx intT []
    [ Exp $ CS.simpleCall "OPENCL_SUCCEED" [CS.simpleCall "CL10.Finish" [Var "ctx.opencl.queue"]]
    , Return $ Integer 0 ]

  CS.debugReport $ openClReport kernel_names


openClDecls :: [String] -> String -> String
            -> ([(CSType, String)], [CSStmt], CSStmt, [CSStmt])
openClDecls kernel_names opencl_program opencl_prelude =
  (ctx_fields, ctx_inits, openCL_boilerplate, openCL_load)
  where ctx_fields =
          [ (intT, "total_runs")
          , (Primitive $ CSInt Int64T, "total_runtime")]
          ++ concatMap (\name -> [(CustomT "CLKernelHandle", name)
                                 ,(longT, kernelRuntime name)
                                 ,(intT, kernelRuns name)]) kernel_names

        ctx_inits =
          [ Reassign (Var $ ctx "total_runs") (Integer 0)
          , Reassign (Var $ ctx "total_runtime") (Integer 0) ]
          ++ concatMap (\name -> [ Reassign (Var $ (ctx . kernelRuntime) name) (Integer 0)
                                 , Reassign (Var $ (ctx . kernelRuns) name) (Integer 0)]
                  ) kernel_names


        futhark_context = CS.publicName "context"

        openCL_load = [CS.funDef "post_opencl_setup" VoidT
            [ (RefT $ CustomT futhark_context, "ctx")
            , (RefT $ CustomT "opencl_device_option", "option")] $ map sizeHeuristicsCode sizeHeuristicsTable]

        openCL_boilerplate =
          AssignTyped stringArrayT (Var "opencl_program")
              (Just $ Collection "string[]" [String $ opencl_prelude ++ opencl_program])

loadKernelByName :: String -> [CSStmt]
loadKernelByName name =
  [ Reassign (Var $ ctx name)
      (CS.simpleCall "CL10.CreateKernel" [Var "prog", String name, Out $ Var "error"])
  , AST.Assert (BinOp "==" (Var "error") (Integer 0)) []
  , If (Var "ctx.debugging")
      [Exp $ consoleErrorWriteLine "Created kernel {0}" [Var $ ctx name]]
      []
  ]

kernelRuntime :: String -> String
kernelRuntime = (++"_total_runtime")

kernelRuns :: String -> String
kernelRuns = (++"_runs")

openClReport :: [String] -> CSStmt
openClReport names =
  If (Var "ctx.debugging") (report_kernels ++ [report_total]) []
  where longest_name = foldl max 0 $ map length names
        report_kernels = map reportKernel names
        format_string name =
          let padding = replicate (longest_name - length name) ' '
          in unwords ["Kernel",
                      name ++ padding,
                      "executed {0} times, with average runtime: {1}\tand total runtime: {2}"]
        reportKernel name =
          let runs = ctx $ kernelRuns name
              total_runtime = ctx $ kernelRuntime name
          in If (BinOp "!=" (Var runs) (Integer 0))
             [Exp $ consoleErrorWriteLine (format_string name)
               [ Var runs
               , Ternary (BinOp "!="
                           (BinOp "/"
                             (Cast (Primitive $ CSInt Int64T) (Var total_runtime))
                             (Var runs))
                           (Integer 0))
                 (Var runs) (Integer 1)
               , Cast (Primitive $ CSInt Int64T) $ Var total_runtime]
             , AssignOp "+" (Var $ ctx "total_runtime") (Var total_runtime)
             , AssignOp "+" (Var $ ctx "total_runs") (Var runs)
             ] []

        ran_text = "Ran {0} kernels with cumulative runtime: {1}"
        report_total = Exp $ consoleErrorWriteLine ran_text [ Var $ ctx "total_runs"
                                                            , Var $ ctx "total_runtime"]

sizeHeuristicsCode :: SizeHeuristic -> CSStmt
sizeHeuristicsCode (SizeHeuristic platform_name device_type which what) =
  let which'' = BinOp "==" which' (Integer 0)
      option_contains_platform_name = CS.simpleCall "option.platform_name.Contains" [String platform_name]
      option_contains_device_type = BinOp "==" (Var "option.device_type") (Var $ clDeviceType device_type)
  in If (BinOp "&&" which''
          (BinOp "&&" option_contains_platform_name
                      option_contains_device_type))
          [ get_size ] []

  where clDeviceType DeviceGPU = "ComputeDeviceTypes.Gpu"
        clDeviceType DeviceCPU = "ComputeDeviceTypes.Cpu"

        which' = case which of
                   LockstepWidth -> Var "ctx.opencl.lockstep_width"
                   NumGroups ->     Var "ctx.opencl.cfg.default_num_groups"
                   GroupSize ->     Var "ctx.opencl.cfg.default_group_size"
                   TileSize ->      Var "ctx.opencl.cfg.default_tile_size"

        get_size = case what of
                     HeuristicConst x ->
                       Reassign which' (Integer $ toInteger x)

                     HeuristicDeviceInfo _ ->
                       -- This only works for device info that fits in the variable.
                       Unsafe
                       [
                         Fixed (CS.assignScalarPointer which' (Var "ptr"))
                         [
                           Exp $ CS.simpleCall "CL10.GetDeviceInfo"
                             [ Var "ctx.opencl.device", Var "ComputeDeviceInfo.MaxComputeUnits"
                             , CS.simpleCall "new IntPtr" [CS.simpleCall "Marshal.SizeOf" [which']]
                             , CS.toIntPtr $ Var "ptr", Out ctxNULL ]
                         ]
                       ]

ctx :: String -> String
ctx = (++) "ctx."

ctxNULL :: CSExp
ctxNULL = Var "ctx.NULL"
