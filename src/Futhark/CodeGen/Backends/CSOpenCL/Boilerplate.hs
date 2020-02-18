module Futhark.CodeGen.Backends.CSOpenCL.Boilerplate
  ( generateBoilerplate

  , kernelRuntime
  , kernelRuns
  ) where

import qualified Data.Map as M

import Futhark.CodeGen.ImpCode.OpenCL hiding (Index, If)
import Futhark.CodeGen.Backends.GenericCSharp as CS
import Futhark.CodeGen.Backends.GenericCSharp.AST as AST
import Futhark.CodeGen.OpenCL.Heuristics
import Futhark.Util (zEncodeString)

intT, longT, stringT, intArrayT, stringArrayT :: CSType
intT = Primitive $ CSInt Int32T
longT = Primitive $ CSInt Int64T
stringT = Primitive StringT
intArrayT = Composite $ ArrayT intT
stringArrayT = Composite $ ArrayT stringT

errorMsgNumArgs :: ErrorMsg a -> Int
errorMsgNumArgs = length . errorMsgArgTypes

formatEscape :: String -> String
formatEscape = concatMap escapeChar
  where escapeChar '{' = "{{"
        escapeChar '}' = "}}"
        escapeChar c = [c]

failureCase :: Integer -> FailureMsg -> CSStmt
failureCase i (FailureMsg (ErrorMsg parts) backtrace) =
  If (BinOp "==" (Var "failure_idx") (Integer i))
  [ let (formatstr, formatargs) = onParts 0 parts
    in AST.Assert (AST.Bool False) $
       String (formatstr ++ "\nBacktrace:\n" ++ formatEscape backtrace) :
       formatargs
  ]
  []
  where onParts _ [] = ("", [])
        onParts j (ErrorString s : parts') =
          let (formatstr, formatargs) = onParts j parts'
          in (s ++ formatstr, formatargs)
        onParts j (ErrorInt32 _ : parts') =
          let (formatstr, formatargs) = onParts (j+1) parts'
          in ("{" ++ show j ++ "}" ++ formatstr, Index (Var "args") (IdxExp $ Integer j) : formatargs)

generateBoilerplate :: String -> String -> M.Map KernelName Safety -> [PrimType]
                    -> M.Map Name SizeClass
                    -> [FailureMsg]
                    -> CS.CompilerM OpenCL () ()
generateBoilerplate opencl_code opencl_prelude kernels types sizes failures = do
  final_inits <- CS.contextFinalInits

  let (opencl_fields, opencl_inits, top_decls, later_top_decls) =
        openClDecls kernels opencl_code opencl_prelude

  CS.stm top_decls

  CS.stm $ AssignTyped stringArrayT (Var "SizeNames")
    (Just $ Collection "string[]" (map (String . pretty) $ M.keys sizes))

  CS.stm $ AssignTyped stringArrayT (Var "SizeVars")
    (Just $ Collection "string[]" (map (String . zEncodeString . pretty) $ M.keys sizes))

  CS.stm $ AssignTyped stringArrayT (Var "SizeClasses")
    (Just $ Collection "string[]" (map (String . pretty) $ M.elems sizes))


  let get_num_sizes = CS.publicName  "GetNumSizes"
  let get_size_name = CS.publicName  "GetSizeName"
  let get_size_class = CS.publicName "GetSizeClass"


  CS.stm $ CS.privateFunDef get_num_sizes intT []
    [ Return $ (Integer . toInteger) $ M.size sizes ]
  CS.stm $ CS.privateFunDef get_size_name (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "SizeNames") (IdxExp $ Var "i") ]
  CS.stm $ CS.privateFunDef get_size_class (Primitive StringT) [(intT, "i")]
    [ Return $ Index (Var "SizeClasses") (IdxExp $ Var "i") ]

  let cfg = CS.publicName "ContextConfig"
  let new_cfg = CS.publicName "ContextConfigNew"
  let cfg_set_debugging = CS.publicName "ContextConfigSetDebugging"
  let cfg_set_device = CS.publicName "ContextConfigSetDevice"
  let cfg_set_platform = CS.publicName "ContextConfigSetPlatform"
  let cfg_dump_program_to = CS.publicName "ContextConfigDumpProgramTo"
  let cfg_load_program_from = CS.publicName "ContextConfigLoadProgramFrom"
  let cfg_set_default_group_size = CS.publicName "ContextConfigSetDefaultGroupSize"
  let cfg_set_default_num_groups = CS.publicName "ContextConfigSetDefaultNumGroups"
  let cfg_set_default_tile_size = CS.publicName "ContextConfigSetDefaultTileSize"
  let cfg_set_default_threshold = CS.publicName "ContextConfigSetDefaultThreshold"
  let cfg_set_size = CS.publicName "ContextConfigSetSize"

  CS.stm $ StructDef "Sizes" (map (\k -> (intT, zEncodeString $ pretty k)) $ M.keys sizes)
  CS.stm $ StructDef cfg [ (CustomT "OpenCLConfig", "OpenCL")
                         , (intArrayT, "Sizes")]

  let tmp_cfg = Var "tmp_cfg"
      sizeInit (SizeBespoke _ x) = Integer $ toInteger x
      sizeInit _ = Integer 0

  CS.stm $ CS.privateFunDef new_cfg (CustomT cfg) []
    [ Assign tmp_cfg $ CS.simpleInitClass cfg []
    , Reassign (Field tmp_cfg "Sizes") (Collection "int[]" (map sizeInit (M.elems sizes)))
    , Exp $ CS.simpleCall "OpenCLConfigInit" [ Out $ Field tmp_cfg "OpenCL", (Integer . toInteger) $ M.size sizes
                                             , Var "SizeNames", Var "SizeVars", Field tmp_cfg "Sizes", Var "SizeClasses" ]
    , Return tmp_cfg
    ]

  CS.stm $ CS.privateFunDef cfg_set_debugging VoidT [(RefT $ CustomT cfg, "_cfg"),(Primitive BoolT, "flag")]
    [Reassign (Var "_cfg.OpenCL.Debugging") (Var "flag")]

  CS.stm $ CS.privateFunDef cfg_set_device VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "SetPreferredDevice" [Ref $ Var "_cfg.OpenCL", Var "s"]]

  CS.stm $ CS.privateFunDef cfg_set_platform VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "s")]
    [Exp $ CS.simpleCall "SetPreferredPlatform" [Ref $ Var "_cfg.OpenCL", Var "s"]]

  CS.stm $ CS.privateFunDef cfg_dump_program_to VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "path")]
    [Reassign (Var "_cfg.OpenCL.DumpProgramTo") (Var "path")]

  CS.stm $ CS.privateFunDef cfg_load_program_from VoidT [(RefT $ CustomT cfg, "_cfg"),(stringT, "path")]
    [Reassign (Var "_cfg.OpenCL.LoadProgramFrom") (Var "path")]

  CS.stm $ CS.privateFunDef cfg_set_default_group_size VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.OpenCL.DefaultGroupSize") (Var "size")]

  CS.stm $ CS.privateFunDef cfg_set_default_num_groups VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "num")]
    [Reassign (Var "_cfg.OpenCL.DefaultNumGroups") (Var "num")]


  CS.stm $ CS.privateFunDef cfg_set_default_tile_size VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.OpenCL.DefaultTileSize") (Var "size")]

  CS.stm $ CS.privateFunDef cfg_set_default_threshold VoidT [(RefT $ CustomT cfg, "_cfg"),(intT, "size")]
    [Reassign (Var "_cfg.OpenCL.DefaultThreshold") (Var "size")]

  CS.stm $ CS.privateFunDef cfg_set_size (Primitive BoolT) [(RefT $ CustomT cfg, "_cfg")
                                                    , (stringT, "SizeName")
                                                    , (intT, "SizeValue")]
    [ AST.For "i" ((Integer . toInteger) $ M.size sizes)
      [ If (BinOp "==" (Var "SizeName") (Index (Var "SizeNames") (IdxExp (Var "i"))))
          [ Reassign (Index (Var "_cfg.Sizes") (IdxExp (Var "i"))) (Var "SizeValue")
          , Return (AST.Bool True)] []
      ]
    , Return $ AST.Bool False ]


  let ctx_ = CS.publicName "Context"
  let new_ctx = CS.publicName "ContextNew"
  let sync_ctx = CS.publicName "ContextSync"

  CS.stm $ StructDef ctx_ $
    [ (Primitive IntPtrT, "NULL")
    , (CustomT "CLMemoryHandle", "EMPTY_MEM_HANDLE")
    , (CustomT "OpenCLFreeList", "FreeList")
    , (Primitive $ CSInt Int64T, "CurrentMemUsageDevice")
    , (Primitive $ CSInt Int64T, "PeakMemUsageDevice")
    , (Primitive BoolT, "DetailMemory")
    , (Primitive BoolT, "Debugging")
    , (CustomT "CLMemoryHandle", "GlobalFailure")
    , (CustomT "CLMemoryHandle", "GlobalFailureArgs")
    , (CustomT "OpenCLContext", "OpenCL")
    , (CustomT "Sizes", "Sizes") ]
    ++ opencl_fields

  mapM_ CS.stm later_top_decls

  CS.addMemberDecl $ AssignTyped (CustomT cfg) (Var "Cfg") Nothing
  CS.addMemberDecl $ AssignTyped (CustomT ctx_) (Var "Ctx") Nothing

  CS.beforeParse $ Reassign (Var "Cfg") $ CS.simpleCall new_cfg []
  CS.atInit $ Exp $ CS.simpleCall new_ctx [Var "Cfg"]
  CS.atInit $ Reassign (Var "Ctx.EMPTY_MEM_HANDLE") $ CS.simpleCall "EmptyMemHandle" [Var "Ctx.OpenCL.Context"]
  CS.atInit $ Reassign (Var "Ctx.FreeList") $ CS.simpleCall "OpenCLFreeListInit" []

  CS.addMemberDecl $ AssignTyped (Primitive BoolT) (Var "Synchronous") (Just $ AST.Bool False)

  let set_required_types = [Reassign (Var "RequiredTypes") (AST.Bool True)
                           | FloatType Float64 `elem` types]

      set_sizes = zipWith (\i k -> Reassign (Field (Var "Ctx.Sizes") (zEncodeString $ pretty k))
                                            (Index (Var "Cfg.Sizes") (IdxExp $ (Integer . toInteger) i)))
                          [(0::Int)..] $ M.keys sizes
      max_failure_args =
        foldl max 0 $ map (errorMsgNumArgs . failureError) failures

  CS.stm $ CS.privateFunDef new_ctx VoidT [(CustomT cfg, "Cfg")] $
    [ AssignTyped (CustomT "ComputeErrorCode") (Var "error") Nothing
    , Reassign (Var "Ctx.DetailMemory") (Var "Cfg.OpenCL.Debugging")
    , Reassign (Var "Ctx.Debugging") (Var "Cfg.OpenCL.Debugging")
    , Reassign (Var "Ctx.OpenCL.Cfg") (Var "Cfg.OpenCL")]
    ++ opencl_inits ++
    [ Assign (Var "RequiredTypes") (AST.Bool False) ]
    ++ set_required_types ++
    [ AssignTyped (CustomT "CLProgramHandle") (Var "prog")
        (Just $ CS.simpleCall "SetupOpenCL" [ Ref $ Var "Ctx"
                                            , Var "OpenCLProgram"
                                            , Var "RequiredTypes"])] ++
    [Unsafe
     [ Exp $ CS.simpleCall "OPENCL_SUCCEED"
       [CS.simpleCall "OpenCLAllocActual" [ Ref $ Var "Ctx"
                                          , Integer 4
                                          , Ref $ Var "Ctx.GlobalFailure"]]
     , AssignTyped intT (Var "no_failure") (Just (Integer (-1)))
     , Exp $ CS.simpleCall "OPENCL_SUCCEED"
       [CS.simpleCall "CL10.EnqueueWriteBuffer"
        [ Var "Ctx.OpenCL.Queue", Var "Ctx.GlobalFailure", AST.Bool True
        , CS.toIntPtr $ Integer 0
        , CS.toIntPtr $ Integer 4
        , CS.toIntPtr $ Addr (Var "no_failure")
        , Integer 0, Null, Null]]

     , Exp $ CS.simpleCall "OPENCL_SUCCEED"
       [CS.simpleCall "OpenCLAllocActual" [ Ref $ Var "Ctx"
                                          , Integer $ toInteger $ 4 * max_failure_args + 1
                                          , Ref $ Var "Ctx.GlobalFailureArgs"]]]]
    ++ concatMap loadKernel (M.toList kernels)
    ++ final_inits
    ++ set_sizes

  CS.stm $ CS.privateFunDef sync_ctx VoidT []
    [ AssignTyped intT (Var "failure_idx") (Just $ CS.simpleInitClass (pretty intT) [])
    , Unsafe [CS.assignScalarPointer (Var "failure_idx") (Var "ptr")
             , Exp $ CS.simpleCall "OPENCL_SUCCEED" [
                 CS.simpleCall "CL10.EnqueueReadBuffer"
                 [ Var "Ctx.OpenCL.Queue", Var "Ctx.GlobalFailure", AST.Bool True
                 , CS.toIntPtr $ Integer 0
                 , CS.toIntPtr $ Integer 4
                 , CS.toIntPtr $ Var "ptr"
                , Integer 0, Null, Null]
                ]
            ]

    , If (BinOp "!=" (Var "failure_idx") (Integer (-1)))
      ([ AssignTyped intArrayT (Var "args") $ Just $ CreateArray intT $ Left max_failure_args
       , Unsafe [
           Fixed (Var "ptr") (Addr (Index (Var "args") $ IdxExp $ Integer 0))
           [Exp $ CS.simpleCall "CL10.EnqueueReadBuffer"
            [ Var "Ctx.OpenCL.Queue", Var "Ctx.GlobalFailureArgs", AST.Bool True
            , CS.toIntPtr $ Integer 0
            , CS.toIntPtr $ Integer $ toInteger $ 4 * max_failure_args
            , CS.toIntPtr $ Var "ptr"
            , Integer 0, Null, Null]]]] ++
        zipWith failureCase [0..] failures)
      []
    ]

  CS.debugReport $ openClReport $ M.keys kernels


openClDecls :: M.Map KernelName Safety -> String -> String
            -> ([(CSType, String)], [CSStmt], CSStmt, [CSStmt])
openClDecls kernels opencl_program opencl_prelude =
  (ctx_fields, ctx_inits, openCL_boilerplate, openCL_load)
  where ctx_fields =
          [ (intT, "TotalRuns")
          , (Primitive $ CSInt Int64T, "TotalRuntime")]
          ++ concatMap (\name -> [(CustomT "CLKernelHandle", name)
                                 ,(longT, kernelRuntime name)
                                 ,(intT, kernelRuns name)])
          (M.keys kernels)

        ctx_inits =
          [ Reassign (Var $ ctx "TotalRuns") (Integer 0)
          , Reassign (Var $ ctx "TotalRuntime") (Integer 0) ]
          ++ concatMap (\name -> [ Reassign (Var $ (ctx . kernelRuntime) name) (Integer 0)
                                 , Reassign (Var $ (ctx . kernelRuns) name) (Integer 0)])
          (M.keys kernels)


        futhark_context = CS.publicName "Context"

        openCL_load = [CS.privateFunDef "PostOpenCLSetup" VoidT
            [ (RefT $ CustomT futhark_context, "Ctx")
            , (RefT $ CustomT "OpenCLDeviceOption", "Option")] $ map sizeHeuristicsCode sizeHeuristicsTable]

        openCL_boilerplate =
          AssignTyped stringArrayT (Var "OpenCLProgram")
              (Just $ Collection "string[]" [String $ opencl_prelude ++ opencl_program])

loadKernel :: (String, Safety) -> [CSStmt]
loadKernel (name, _) =
  [ Reassign (Var $ ctx name)
      (CS.simpleCall "CL10.CreateKernel" [Var "prog", String name, Out $ Var "error"])
  , AST.Assert (BinOp "==" (Var "error") (Integer 0)) []
  , If (Var "Ctx.Debugging")
      [Exp $ consoleErrorWriteLine "Created kernel {0}" [Var $ ctx name]]
      []
  ]

kernelRuntime :: String -> String
kernelRuntime = (++"_TotalRuntime")

kernelRuns :: String -> String
kernelRuns = (++"_Runs")

openClReport :: [String] -> CSStmt
openClReport names =
  If (Var "Ctx.Debugging") (report_kernels ++ [report_total]) []
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
             , AssignOp "+" (Var $ ctx "TotalRuntime") (Var total_runtime)
             , AssignOp "+" (Var $ ctx "TotalRuns") (Var runs)
             ] []

        ran_text = "Ran {0} kernels with cumulative runtime: {1}"
        report_total = Exp $ consoleErrorWriteLine ran_text [ Var $ ctx "TotalRuns"
                                                            , Var $ ctx "TotalRuntime"]

sizeHeuristicsCode :: SizeHeuristic -> CSStmt
sizeHeuristicsCode (SizeHeuristic platform_name device_type which what) =
  let which'' = BinOp "==" which' (Integer 0)
      option_contains_platform_name = CS.simpleCall "Option.PlatformName.Contains" [String platform_name]
      option_contains_device_type = BinOp "==" (Var "Option.DeviceType") (Var $ clDeviceType device_type)
  in If (BinOp "&&" which''
          (BinOp "&&" option_contains_platform_name
                      option_contains_device_type))
          [ get_size ] []

  where clDeviceType DeviceGPU = "ComputeDeviceTypes.Gpu"
        clDeviceType DeviceCPU = "ComputeDeviceTypes.Cpu"

        which' = case which of
                   LockstepWidth -> Var "Ctx.OpenCL.LockstepWidth"
                   NumGroups ->     Var "Ctx.OpenCL.Cfg.DefaultNumGroups"
                   GroupSize ->     Var "Ctx.OpenCL.Cfg.DefaultGroupSize"
                   TileSize ->      Var "Ctx.OpenCL.Cfg.DefaultTileSize"
                   Threshold ->     Var "Ctx.OpenCL.Cfg.DefaultThreshold"

        get_size = case what of
                     HeuristicConst x ->
                       Reassign which' (Integer $ toInteger x)

                     HeuristicDeviceInfo _ ->
                       -- This only works for device info that fits in the variable.
                       Unsafe
                       [
                         Fixed (Var "ptr") (Addr which')
                         [
                           Exp $ CS.simpleCall "CL10.GetDeviceInfo"
                             [ Var "Ctx.OpenCL.Device", Var "ComputeDeviceInfo.MaxComputeUnits"
                             , CS.simpleCall "new IntPtr" [CS.simpleCall "Marshal.SizeOf" [which']]
                             , CS.toIntPtr $ Var "ptr", Out ctxNULL ]
                         ]
                       ]

ctx :: String -> String
ctx = (++) "Ctx."

ctxNULL :: CSExp
ctxNULL = Var "Ctx.NULL"
