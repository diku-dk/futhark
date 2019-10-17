{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
-- | A generic C# code generator which is polymorphic in the type
-- of the operations.  Concretely, we use this to handle both
-- sequential and OpenCL C# code.
module Futhark.CodeGen.Backends.GenericCSharp
  ( compileProg
  , Constructor (..)
  , emptyConstructor

  , assignScalarPointer
  , toIntPtr
  , compileName
  , compileDim
  , compileExp
  , compileCode
  , compilePrimValue
  , compilePrimType
  , compilePrimTypeExt
  , compilePrimTypeToAST
  , compilePrimTypeToASText
  , contextFinalInits
  , debugReport

  , Operations (..)
  , defaultOperations

  , unpackDim

  , CompilerM (..)
  , OpCompiler
  , WriteScalar
  , ReadScalar
  , Allocate
  , Copy
  , StaticArray
  , EntryOutput
  , EntryInput

  , CompilerEnv(..)
  , CompilerState(..)
  , CompilerAcc
  , stm
  , stms
  , atInit
  , staticMemDecl
  , staticMemAlloc
  , addMemberDecl
  , beforeParse
  , collect'
  , collect
  , simpleCall
  , callMethod
  , simpleInitClass
  , parametrizedCall

  , copyMemoryDefaultSpace
  , consoleErrorWrite
  , consoleErrorWriteLine
  , consoleWrite
  , consoleWriteLine

  , publicName
  , sizeOf
  , privateFunDef
  , publicFunDef
  , getDefaultDecl
  ) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Arrow((&&&))
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import Futhark.Representation.Primitive hiding (Bool)
import Futhark.MonadFreshNames
import Futhark.Representation.AST.Syntax (Space(..))
import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.CodeGen.Backends.GenericCSharp.AST
import Futhark.CodeGen.Backends.GenericCSharp.Options
import Futhark.CodeGen.Backends.GenericCSharp.Definitions
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions)
import Text.Printf (printf)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId -> CSExp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = VName -> CSExp -> PrimType -> Imp.SpaceId
                       -> CompilerM op s CSExp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = VName -> CSExp -> Imp.SpaceId
                     -> CompilerM op s ()

-- | Copy from one memory block to another.
type Copy op s = VName -> CSExp -> Imp.Space ->
                 VName -> CSExp -> Imp.Space ->
                 CSExp -> PrimType ->
                 CompilerM op s ()

-- | Create a static array of values - initialised at load time.
type StaticArray op s = VName -> Imp.SpaceId -> PrimType -> Imp.ArrayContents -> CompilerM op s ()

-- | Construct the C# array being returned from an entry point.
type EntryOutput op s = VName -> Imp.SpaceId ->
                        PrimType -> Imp.Signedness ->
                        [Imp.DimSize] ->
                        CompilerM op s CSExp

-- | Unpack the array being passed to an entry point.
type EntryInput op s = VName -> Imp.SpaceId ->
                       PrimType -> Imp.Signedness ->
                       [Imp.DimSize] ->
                       CSExp ->
                       CompilerM op s ()

data Operations op s = Operations { opsWriteScalar :: WriteScalar op s
                                  , opsReadScalar :: ReadScalar op s
                                  , opsAllocate :: Allocate op s
                                  , opsCopy :: Copy op s
                                  , opsStaticArray :: StaticArray op s
                                  , opsCompiler :: OpCompiler op s
                                  , opsEntryOutput :: EntryOutput op s
                                  , opsEntryInput :: EntryInput op s
                                  , opsSyncRun :: CSStmt
                                  }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations = Operations { opsWriteScalar = defWriteScalar
                               , opsReadScalar = defReadScalar
                               , opsAllocate  = defAllocate
                               , opsCopy = defCopy
                               , opsStaticArray = defStaticArray
                               , opsCompiler = defCompiler
                               , opsEntryOutput = defEntryOutput
                               , opsEntryInput = defEntryInput
                               , opsSyncRun = defSyncRun
                               }
  where defWriteScalar _ _ _ _ _ =
          error "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          error "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          error "Cannot allocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ _ =
          error "Cannot copy to or from non-default memory space"
        defStaticArray _ _ _ _ =
          error "Cannot create static array in non-default memory space"
        defCompiler _ =
          error "The default compiler cannot compile extended operations"
        defEntryOutput _ _ _ _ =
          error "Cannot return array not in default memory space"
        defEntryInput _ _ _ _ =
          error "Cannot accept array not in default memory space"
        defSyncRun =
          Pass

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: M.Map Name [Imp.Type]
}

data CompilerAcc op s = CompilerAcc {
    accItems :: [CSStmt]
  , accFreedMem :: [VName]
  }

instance Semigroup (CompilerAcc op s) where
  CompilerAcc items1 freed1 <> CompilerAcc items2 freed2 =
    CompilerAcc (items1<>items2) (freed1<>freed2)

instance Monoid (CompilerAcc op s) where
  mempty = CompilerAcc mempty mempty

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

envStaticArray :: CompilerEnv op s -> StaticArray op s
envStaticArray = opsStaticArray . envOperations

envEntryOutput :: CompilerEnv op s -> EntryOutput op s
envEntryOutput = opsEntryOutput . envOperations

envEntryInput :: CompilerEnv op s -> EntryInput op s
envEntryInput = opsEntryInput . envOperations

envSyncFun :: CompilerEnv op s -> CSStmt
envSyncFun = opsSyncRun . envOperations

newCompilerEnv :: Imp.Functions op -> Operations op s -> CompilerEnv op s
newCompilerEnv (Imp.Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = M.fromList $ map funReturn funs
        funReturn (name, Imp.Function _ outparams _ _ _ _) = (name, paramsTypes outparams)
        builtinFtable = M.map (map Imp.Scalar . snd) builtInFunctions

data CompilerState s = CompilerState {
    compNameSrc :: VNameSource
  , compBeforeParse :: [CSStmt]
  , compInit :: [CSStmt]
  , compStaticMemDecls :: [CSStmt]
  , compStaticMemAllocs :: [CSStmt]
  , compDebugItems :: [CSStmt]
  , compUserState :: s
  , compMemberDecls :: [CSStmt]
  , compAssignedVars :: [VName]
  , compDeclaredMem :: [(VName, Space)]
}

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compNameSrc = src
                                       , compBeforeParse = []
                                       , compInit = []
                                       , compStaticMemDecls = []
                                       , compStaticMemAllocs = []
                                       , compDebugItems = []
                                       , compMemberDecls = []
                                       , compUserState = s
                                       , compAssignedVars = []
                                       , compDeclaredMem = []
                                       }

newtype CompilerM op s a = CompilerM (RWS (CompilerEnv op s) (CompilerAcc op s) (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter (CompilerAcc op s))

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

collect :: CompilerM op s () -> CompilerM op s [CSStmt]
collect m = pass $ do
  ((), w) <- listen m
  return (accItems w,
          const w { accItems = mempty} )

collect' :: CompilerM op s a -> CompilerM op s (a, [CSStmt])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, accItems w),
          const w { accItems = mempty})

beforeParse :: CSStmt -> CompilerM op s ()
beforeParse x = modify $ \s ->
  s { compBeforeParse = compBeforeParse s ++ [x] }

atInit :: CSStmt -> CompilerM op s ()
atInit x = modify $ \s ->
  s { compInit = compInit s ++ [x] }

staticMemDecl :: CSStmt -> CompilerM op s ()
staticMemDecl x = modify $ \s ->
  s { compStaticMemDecls = compStaticMemDecls s ++ [x] }

staticMemAlloc :: CSStmt -> CompilerM op s ()
staticMemAlloc x = modify $ \s ->
  s { compStaticMemAllocs = compStaticMemAllocs s ++ [x] }

addMemberDecl :: CSStmt -> CompilerM op s ()
addMemberDecl x = modify $ \s ->
  s { compMemberDecls = compMemberDecls s ++ [x] }

contextFinalInits :: CompilerM op s [CSStmt]
contextFinalInits = gets compInit

item :: CSStmt -> CompilerM op s ()
item x = tell $ mempty { accItems = [x] }

stm :: CSStmt -> CompilerM op s ()
stm = item

stms :: [CSStmt] -> CompilerM op s ()
stms = mapM_ stm

debugReport :: CSStmt -> CompilerM op s ()
debugReport x = modify $ \s ->
  s { compDebugItems = compDebugItems s ++ [x] }

getVarAssigned :: VName -> CompilerM op s Bool
getVarAssigned vname =
  elem vname <$> gets compAssignedVars

setVarAssigned :: VName -> CompilerM op s ()
setVarAssigned vname = modify $ \s ->
  s { compAssignedVars = vname : compAssignedVars s}

futharkFun :: String -> String
futharkFun s = "futhark_" ++ zEncodeString s

paramsTypes :: [Imp.Param] -> [Imp.Type]
paramsTypes = map paramType

paramType :: Imp.Param -> Imp.Type
paramType (Imp.MemParam _ space) = Imp.Mem (Imp.ConstSize 0) space
paramType (Imp.ScalarParam _ t) = Imp.Scalar t

compileOutput :: Imp.Param -> (CSExp, CSType)
compileOutput = nameFun &&& typeFun
  where nameFun = Var . compileName . Imp.paramName
        typeFun = compileType . paramType

getDefaultDecl :: Imp.Param -> CSStmt
getDefaultDecl (Imp.MemParam v DefaultSpace) =
  Assign (Var $ compileName v) $ simpleCall "allocateMem" [Integer 0]
getDefaultDecl (Imp.MemParam v _) =
  AssignTyped (CustomT "OpenCLMemblock") (Var $ compileName v) (Just $ simpleCall "EmptyMemblock" [Var "Ctx.EMPTY_MEM_HANDLE"])
getDefaultDecl (Imp.ScalarParam v Cert) =
  Assign (Var $ compileName v) $ Bool True
getDefaultDecl (Imp.ScalarParam v t) =
  Assign (Var $ compileName v) $ simpleInitClass (compilePrimType t) []


runCompilerM :: Imp.Functions op -> Operations op s
             -> VNameSource
             -> s
             -> CompilerM op s a
             -> a
runCompilerM prog ops src userstate (CompilerM m) =
  fst $ evalRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)

standardOptions :: [Option]
standardOptions = [
  Option { optionLongName = "write-runtime-to"
         , optionShortName = Just 't'
         , optionArgument = RequiredArgument
         , optionAction =
           [
             If (BinOp "!=" (Var "RuntimeFile") Null)
             [Exp $ simpleCall "RuntimeFile.Close" []] []
           , Reassign (Var "RuntimeFile") $
             simpleInitClass "FileStream" [Var "optarg", Var "FileMode.Create"]
           , Reassign (Var "RuntimeFileWriter") $
             simpleInitClass "StreamWriter" [Var "RuntimeFile"]
           ]
         },
  Option { optionLongName = "runs"
         , optionShortName = Just 'r'
         , optionArgument = RequiredArgument
         , optionAction =
           [ Reassign (Var "NumRuns") $ simpleCall "Convert.ToInt32" [Var "optarg"]
           , Reassign (Var "DoWarmupRun") $ Bool True
           ]
         },
  Option { optionLongName = "entry-point"
         , optionShortName = Just 'e'
         , optionArgument = RequiredArgument
         , optionAction =
             [ Reassign (Var "EntryPoint") $ Var "optarg" ]
         }
  ]

-- | The class generated by the code generator must have a
-- constructor, although it can be vacuous.
data Constructor = Constructor [CSFunDefArg] [CSStmt]

-- | A constructor that takes no arguments and does nothing.
emptyConstructor :: Constructor
emptyConstructor = Constructor [(Composite $ ArrayT $ Primitive StringT, "args")] []

constructorToConstructorDef :: Constructor -> String -> [CSStmt] -> CSStmt
constructorToConstructorDef (Constructor params body) name at_init =
  ConstructorDef $ ClassConstructor name params $ body <> at_init


compileProg :: MonadFreshNames m =>
               Maybe String
            -> Constructor
            -> [CSStmt]
            -> [CSStmt]
            -> Operations op s
            -> s
            -> CompilerM op s ()
            -> [CSStmt]
            -> [Space]
            -> [Option]
            -> Imp.Functions op
            -> m String
compileProg module_name constructor imports defines ops userstate boilerplate pre_timing _ options prog@(Imp.Functions funs) = do
  src <- getNameSource
  let prog' = runCompilerM prog ops src userstate compileProg'
  let imports' = [ Using Nothing "System"
                 , Using Nothing "System.Diagnostics"
                 , Using Nothing "System.Collections"
                 , Using Nothing "System.Collections.Generic"
                 , Using Nothing "System.IO"
                 , Using Nothing "System.Linq"
                 , Using Nothing "System.Runtime.InteropServices"
                 , Using Nothing "static System.ValueTuple"
                 , Using Nothing "static System.Convert"
                 , Using Nothing "static System.Math"
                 , Using Nothing "System.Numerics"
                 , Using Nothing "Mono.Options" ] ++ imports

  return $ pretty (CSProg $ imports' ++ prog')
  where compileProg' = do
          definitions <- mapM compileFunc funs
          opencl_boilerplate <- collect boilerplate
          compBeforeParses <- gets compBeforeParse
          compInits <- gets compInit
          staticDecls <- gets compStaticMemDecls
          staticAllocs <- gets compStaticMemAllocs
          extraMemberDecls <- gets compMemberDecls
          let member_decls' = member_decls ++ extraMemberDecls ++ staticDecls
          let at_inits' = at_inits ++ compBeforeParses ++ parse_options ++ compInits ++ staticAllocs


          case module_name of
            Just name -> do
              entry_points <- mapM (compileEntryFun pre_timing) $ filter (Imp.functionEntry . snd) funs
              let constructor' = constructorToConstructorDef constructor name at_inits'
              return [ Namespace name [ClassDef $ PublicClass name $ member_decls' ++
                       constructor' : defines' ++ opencl_boilerplate ++
                       map PrivateFunDef definitions ++
                       map PublicFunDef entry_points ]]


            Nothing -> do
              let name = "FutharkInternal"
              let constructor' = constructorToConstructorDef constructor name at_inits'
              (entry_point_defs, entry_point_names, entry_points) <-
                unzip3 <$> mapM (callEntryFun pre_timing)
                (filter (Imp.functionEntry . snd) funs)

              debug_ending <- gets compDebugItems
              return [Namespace name (ClassDef
                       (PublicClass name $
                         member_decls' ++
                         constructor' : defines' ++
                         opencl_boilerplate ++
                         map PrivateFunDef (definitions ++ entry_point_defs) ++
                         [PublicFunDef $ Def "InternalEntry" VoidT [] $ selectEntryPoint entry_point_names entry_points ++ debug_ending
                         ]
                      ) :
                     [ClassDef $ PublicClass "Program"
                       [StaticFunDef $ Def "Main" VoidT [(string_arrayT,"args")] main_entry]])
                     ]



        string_arrayT = Composite $ ArrayT $ Primitive StringT
        main_entry :: [CSStmt]
        main_entry = [ Assign (Var "internalInstance") (simpleInitClass "FutharkInternal" [Var "args"])
                     , Exp $ simpleCall "internalInstance.InternalEntry" []
                     ]

        member_decls =
          [ AssignTyped (CustomT "FileStream") (Var "RuntimeFile") Nothing
          , AssignTyped (CustomT "StreamWriter") (Var "RuntimeFileWriter") Nothing
          , AssignTyped (Primitive BoolT) (Var "DoWarmupRun") Nothing
          , AssignTyped (Primitive $ CSInt Int32T) (Var "NumRuns") Nothing
          , AssignTyped (Primitive StringT) (Var "EntryPoint") Nothing
          ]

        at_inits = [ Reassign (Var "DoWarmupRun") (Bool False)
                   , Reassign (Var "NumRuns") (Integer 1)
                   , Reassign (Var "EntryPoint") (String "main")
                   , Exp $ simpleCall "ValueReader" []
                   ]

        defines' = [ Escape csScalar
                   , Escape csMemory
                   , Escape csPanic
                   , Escape csExceptions
                   , Escape csReader] ++ defines

        parse_options =
          generateOptionParser (standardOptions ++ options)

        selectEntryPoint entry_point_names entry_points =
          [ Assign (Var "EntryPoints") $
              Collection "Dictionary<string, Action>" $ zipWith Pair (map String entry_point_names) entry_points,
            If (simpleCall "!EntryPoints.ContainsKey" [Var "EntryPoint"])
              [ Exp $ simpleCall "Console.Error.WriteLine"
                  [simpleCall "string.Format"
                    [ String "No entry point '{0}'.  Select another with --entry point.  Options are:\n{1}"
                    , Var "EntryPoint"
                    , simpleCall "string.Join"
                        [ String "\n"
                        , Field (Var "EntryPoints") "Keys" ]]]
              , Exp $ simpleCall "Environment.Exit" [Integer 1]]
              [ Assign (Var "entryPointFun") $
                  Index (Var "EntryPoints") (IdxExp $ Var "EntryPoint")
              , Exp $ simpleCall "entryPointFun.Invoke" []]
          ]


compileFunc :: (Name, Imp.Function op) -> CompilerM op s CSFunDef
compileFunc (fname, Imp.Function _ outputs inputs body _ _) = do
  body' <- blockScope $ compileCode body
  let inputs' = map compileTypedInput inputs
  let outputs' = map compileOutput outputs
  let outputDecls = map getDefaultDecl outputs
  let (ret, retType) = unzip outputs'
  let retType' = tupleOrSingleT retType
  let ret' = [Return $ tupleOrSingle ret]

  case outputs of
    [] -> return $ Def (futharkFun . nameToString $ fname) VoidT inputs' (outputDecls++body')
    _ -> return $ Def (futharkFun . nameToString $ fname) retType' inputs' (outputDecls++body'++ret')


compileTypedInput :: Imp.Param -> (CSType, String)
compileTypedInput input = (typeFun input, nameFun input)
  where nameFun = compileName . Imp.paramName
        typeFun = compileType . paramType

tupleOrSingleEntryT :: [CSType] -> CSType
tupleOrSingleEntryT [e] = e
tupleOrSingleEntryT es = Composite $ SystemTupleT es

tupleOrSingleEntry :: [CSExp] -> CSExp
tupleOrSingleEntry [e] = e
tupleOrSingleEntry es = CreateSystemTuple es

tupleOrSingleT :: [CSType] -> CSType
tupleOrSingleT [e] = e
tupleOrSingleT es = Composite $ TupleT es

tupleOrSingle :: [CSExp] -> CSExp
tupleOrSingle [e] = e
tupleOrSingle es = Tuple es

assignScalarPointer :: CSExp -> CSExp -> CSStmt
assignScalarPointer e ptr =
  AssignTyped (PointerT VoidT) ptr (Just $ Addr e)

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
simpleCall :: String -> [CSExp] -> CSExp
simpleCall fname = Call (Var fname) . map simpleArg

-- | A 'Call' where the function is a variable and every argument is a
-- simple 'Arg'.
parametrizedCall :: String -> String -> [CSExp] -> CSExp
parametrizedCall fname primtype = Call (Var fname') . map simpleArg
  where fname' = concat [fname, "<", primtype, ">"]

simpleArg :: CSExp -> CSArg
simpleArg = Arg Nothing

-- | A CallMethod
callMethod :: CSExp -> String -> [CSExp] -> CSExp
callMethod object method = CallMethod object (Var method) . map simpleArg

simpleInitClass :: String -> [CSExp] -> CSExp
simpleInitClass fname =CreateObject (Var fname) . map simpleArg

compileName :: VName -> String
compileName = zEncodeString . pretty

compileType :: Imp.Type -> CSType
compileType (Imp.Scalar p) = compilePrimTypeToAST p
compileType (Imp.Mem _ space) = rawMemCSType space

compilePrimTypeToAST :: PrimType -> CSType
compilePrimTypeToAST (IntType Int8) = Primitive $ CSInt Int8T
compilePrimTypeToAST (IntType Int16) = Primitive $ CSInt Int16T
compilePrimTypeToAST (IntType Int32) = Primitive $ CSInt Int32T
compilePrimTypeToAST (IntType Int64) = Primitive $ CSInt Int64T
compilePrimTypeToAST (FloatType Float32) = Primitive $ CSFloat FloatT
compilePrimTypeToAST (FloatType Float64) = Primitive $ CSFloat DoubleT
compilePrimTypeToAST Imp.Bool = Primitive BoolT
compilePrimTypeToAST Imp.Cert = Primitive BoolT

compilePrimTypeToASText :: PrimType -> Imp.Signedness -> CSType
compilePrimTypeToASText (IntType Int8) Imp.TypeUnsigned = Primitive  $ CSUInt UInt8T
compilePrimTypeToASText (IntType Int16) Imp.TypeUnsigned = Primitive $ CSUInt UInt16T
compilePrimTypeToASText (IntType Int32) Imp.TypeUnsigned = Primitive $ CSUInt UInt32T
compilePrimTypeToASText (IntType Int64) Imp.TypeUnsigned = Primitive $ CSUInt UInt64T
compilePrimTypeToASText (IntType Int8) _ = Primitive $ CSInt Int8T
compilePrimTypeToASText (IntType Int16) _ = Primitive $ CSInt Int16T
compilePrimTypeToASText (IntType Int32) _ = Primitive $ CSInt Int32T
compilePrimTypeToASText (IntType Int64) _ = Primitive $ CSInt Int64T
compilePrimTypeToASText (FloatType Float32) _ = Primitive $ CSFloat FloatT
compilePrimTypeToASText (FloatType Float64) _ = Primitive $ CSFloat DoubleT
compilePrimTypeToASText Imp.Bool _ = Primitive BoolT
compilePrimTypeToASText Imp.Cert _ = Primitive BoolT

compileDim :: Imp.DimSize -> CSExp
compileDim (Imp.ConstSize i) = Integer $ toInteger i
compileDim (Imp.VarSize v) = Var $ compileName v

unpackDim :: CSExp -> Imp.DimSize -> Int32 -> CompilerM op s ()
unpackDim arr_name (Imp.ConstSize c) i = do
  let shape_name = Field arr_name "Item2" -- array tuples are currently (data array * dimension array) currently
  let constant_c = Integer $ toInteger c
  let constant_i = Integer $ toInteger i
  stm $ Assert (BinOp "==" constant_c (Index shape_name $ IdxExp constant_i)) [String "constant dimension wrong"]

unpackDim arr_name (Imp.VarSize var) i = do
  let shape_name = Field arr_name "Item2"
  let src = Index shape_name $ IdxExp $ Integer $ toInteger i
  let dest = Var $ compileName var
  isAssigned <- getVarAssigned var
  if isAssigned
    then
      stm $ Reassign dest $ Cast (Primitive $ CSInt Int32T) src
    else do
      stm $ Assign dest $ Cast (Primitive $ CSInt Int32T) src
      setVarAssigned var

entryPointOutput :: Imp.ExternalValue -> CompilerM op s CSExp
entryPointOutput (Imp.OpaqueValue _ vs) =
  CreateSystemTuple <$> mapM (entryPointOutput . Imp.TransparentValue) vs

entryPointOutput (Imp.TransparentValue (Imp.ScalarValue bt ept name)) =
  return $ cast $ Var $ compileName name
  where cast = compileTypecastExt bt ept

entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem Imp.DefaultSpace bt ept dims)) = do
  let src = Var $ compileName mem
  let createTuple = "createTuple_" ++ compilePrimTypeExt bt ept
  return $ simpleCall createTuple [src, CreateArray (Primitive $ CSInt Int64T) $ Right $ map compileDim dims]

entryPointOutput (Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims)) = do
  unRefMem mem (Imp.Space sid)
  pack_output <- asks envEntryOutput
  pack_output mem sid bt ept dims

entryPointInput :: (Int, Imp.ExternalValue, CSExp) -> CompilerM op s ()
entryPointInput (i, Imp.OpaqueValue _ vs, e) =
  mapM_ entryPointInput $ zip3 (repeat i) (map Imp.TransparentValue vs) $
    map (\idx -> Field e $ "Item" ++ show (idx :: Int)) [1..]

entryPointInput (_, Imp.TransparentValue (Imp.ScalarValue bt _ name), e) = do
  let vname' = Var $ compileName name
      cast = compileTypecast bt
  stm $ Assign vname' (cast e)

entryPointInput (_, Imp.TransparentValue (Imp.ArrayValue mem Imp.DefaultSpace bt _ dims), e) = do
  zipWithM_ (unpackDim e) dims [0..]
  let arrayData = Field e "Item1"
  let dest = Var $ compileName mem
      unwrap_call = simpleCall "unwrapArray" [arrayData, sizeOf $ compilePrimTypeToAST bt]
  stm $ Assign dest unwrap_call

entryPointInput (_, Imp.TransparentValue (Imp.ArrayValue mem (Imp.Space sid) bt ept dims), e) = do
  unpack_input <- asks envEntryInput
  unpack <- collect $ unpack_input mem sid bt ept dims e
  stms unpack

extValueDescName :: Imp.ExternalValue -> String
extValueDescName (Imp.TransparentValue v) = extName $ valueDescName v
extValueDescName (Imp.OpaqueValue desc []) = extName $ zEncodeString desc
extValueDescName (Imp.OpaqueValue desc (v:_)) =
  extName $ zEncodeString desc ++ "_" ++ pretty (baseTag (valueDescVName v))

extName :: String -> String
extName = (++"_ext")

sizeOf :: CSType -> CSExp
sizeOf t = simpleCall "sizeof" [(Var . pretty) t]

publicFunDef :: String -> CSType -> [(CSType, String)] -> [CSStmt] -> CSStmt
publicFunDef s t args stmts = PublicFunDef $ Def s t args stmts

privateFunDef :: String -> CSType -> [(CSType, String)] -> [CSStmt] -> CSStmt
privateFunDef s t args stmts = PrivateFunDef $ Def s t args stmts

valueDescName :: Imp.ValueDesc -> String
valueDescName = compileName . valueDescVName

valueDescVName :: Imp.ValueDesc -> VName
valueDescVName (Imp.ScalarValue _ _ vname) = vname
valueDescVName (Imp.ArrayValue vname _ _ _ _) = vname

consoleWrite :: String -> [CSExp] -> CSExp
consoleWrite str exps = simpleCall "Console.Write" $ String str:exps

consoleWriteLine :: String -> [CSExp] -> CSExp
consoleWriteLine str exps = simpleCall "Console.WriteLine" $ String str:exps

consoleErrorWrite :: String -> [CSExp] -> CSExp
consoleErrorWrite str exps = simpleCall "Console.Error.Write" $ String str:exps

consoleErrorWriteLine :: String -> [CSExp] -> CSExp
consoleErrorWriteLine str exps = simpleCall "Console.Error.WriteLine" $ String str:exps

readFun :: PrimType -> Imp.Signedness -> String
readFun (FloatType Float32) _ = "ReadF32"
readFun (FloatType Float64) _ = "ReadF64"
readFun (IntType Int8)  Imp.TypeUnsigned = "ReadU8"
readFun (IntType Int16) Imp.TypeUnsigned = "ReadU16"
readFun (IntType Int32) Imp.TypeUnsigned = "ReadU32"
readFun (IntType Int64) Imp.TypeUnsigned = "ReadU64"
readFun (IntType Int8)  Imp.TypeDirect   = "ReadI8"
readFun (IntType Int16) Imp.TypeDirect   = "ReadI16"
readFun (IntType Int32) Imp.TypeDirect   = "ReadI32"
readFun (IntType Int64) Imp.TypeDirect   = "ReadI64"
readFun Imp.Bool _      = "ReadBool"
readFun Cert _          = error "readFun: cert"

readBinFun :: PrimType -> Imp.Signedness -> String
readBinFun (FloatType Float32) _bin_ = "ReadBinF32"
readBinFun (FloatType Float64) _bin_ = "ReadBinF64"
readBinFun (IntType Int8)  Imp.TypeUnsigned = "ReadBinU8"
readBinFun (IntType Int16) Imp.TypeUnsigned = "ReadBinU16"
readBinFun (IntType Int32) Imp.TypeUnsigned = "ReadBinU32"
readBinFun (IntType Int64) Imp.TypeUnsigned = "ReadBinU64"
readBinFun (IntType Int8)  Imp.TypeDirect   = "ReadBinI8"
readBinFun (IntType Int16) Imp.TypeDirect   = "ReadBinI16"
readBinFun (IntType Int32) Imp.TypeDirect   = "ReadBinI32"
readBinFun (IntType Int64) Imp.TypeDirect   = "ReadBinI64"
readBinFun Imp.Bool _      = "ReadBinBool"
readBinFun Cert _          = error "readFun: cert"

-- The value returned will be used when reading binary arrays, to indicate what
-- the expected type is
-- Key into the FUTHARK_PRIMTYPES dict.
readTypeEnum :: PrimType -> Imp.Signedness -> String
readTypeEnum (IntType Int8)  Imp.TypeUnsigned = "u8"
readTypeEnum (IntType Int16) Imp.TypeUnsigned = "u16"
readTypeEnum (IntType Int32) Imp.TypeUnsigned = "u32"
readTypeEnum (IntType Int64) Imp.TypeUnsigned = "u64"
readTypeEnum (IntType Int8)  Imp.TypeDirect   = "i8"
readTypeEnum (IntType Int16) Imp.TypeDirect   = "i16"
readTypeEnum (IntType Int32) Imp.TypeDirect   = "i32"
readTypeEnum (IntType Int64) Imp.TypeDirect   = "i64"
readTypeEnum (FloatType Float32) _ = "f32"
readTypeEnum (FloatType Float64) _ = "f64"
readTypeEnum Imp.Bool _ = "bool"
readTypeEnum Cert _ = error "readTypeEnum: cert"

readInput :: Imp.ExternalValue -> CSStmt
readInput (Imp.OpaqueValue desc _) =
  Throw $ simpleInitClass "Exception" [String $ "Cannot read argument of type " ++ desc ++ "."]

readInput decl@(Imp.TransparentValue (Imp.ScalarValue bt ept _)) =
  let read_func =  Var $ readFun bt ept
      read_bin_func =  Var $ readBinFun bt ept
      type_enum = String $ readTypeEnum bt ept
      bt' =  compilePrimTypeExt bt ept
      readScalar = initializeGenericFunction "ReadScalar" bt'
  in Assign (Var $ extValueDescName decl) $ simpleCall readScalar [type_enum, read_func, read_bin_func]

-- TODO: If the type identifier of 'Float32' is changed, currently the error
-- messages for reading binary input will not use this new name. This is also a
-- problem for the C runtime system.
readInput decl@(Imp.TransparentValue (Imp.ArrayValue _ _ bt ept dims)) =
  let rank' = Var $ show $ length dims
      type_enum = String $ readTypeEnum bt ept
      bt' =  compilePrimTypeExt bt ept
      read_func =  Var $ readFun bt ept
      readArray = initializeGenericFunction "ReadArray" bt'
  in Assign (Var $ extValueDescName decl) $ simpleCall readArray [rank', type_enum, read_func]

initializeGenericFunction :: String -> String -> String
initializeGenericFunction fun tp = fun ++ "<" ++ tp ++ ">"


printPrimStm :: CSExp -> CSStmt
printPrimStm val = Exp $ simpleCall "WriteValue" [val]

formatString :: String -> [CSExp] -> CSExp
formatString fmt contents =
  simpleCall "String.Format" $ String fmt : contents

printStm :: Imp.ValueDesc -> CSExp -> CSExp -> CompilerM op s CSStmt
printStm Imp.ScalarValue{} _ e =
  return $ printPrimStm e
printStm (Imp.ArrayValue _ _ _ _ []) ind e = do
  let e' = Index e (IdxExp (PostUnOp "++" ind))
  return $ printPrimStm e'

printStm (Imp.ArrayValue mem space bt ept (outer:shape)) ind e = do
  ptr <- newVName "shapePtr"
  first <- newVName "printFirst"
  let size = callMethod (CreateArray (Primitive $ CSInt Int32T) $ Right $ map compileDim $ outer:shape)
                 "Aggregate" [ Integer 1
                             , Lambda (Tuple [Var "acc", Var "val"])
                                      [Exp $ BinOp "*" (Var "acc") (Var "val")]
                             ]
      emptystr = "empty(" ++ ppArrayType bt (length shape) ++ ")"

  printelem <- printStm (Imp.ArrayValue mem space bt ept shape) ind e
  return $
    If (BinOp "==" size (Integer 0))
      [puts emptystr]
    [ Assign (Var $ pretty first) $ Var "true"
    , puts "["
    , For (pretty ptr) (compileDim outer)
      [ If (simpleCall "!" [Var $ pretty first]) [puts ", "] []
      , printelem
      , Reassign (Var $ pretty first) $ Var "false"
      ]
    , puts "]"
    ]

    where ppArrayType :: PrimType -> Int -> String
          ppArrayType t 0 = prettyPrimType ept t
          ppArrayType t n = "[]" ++ ppArrayType t (n-1)

          prettyPrimType Imp.TypeUnsigned (IntType Int8) = "u8"
          prettyPrimType Imp.TypeUnsigned (IntType Int16) = "u16"
          prettyPrimType Imp.TypeUnsigned (IntType Int32) = "u32"
          prettyPrimType Imp.TypeUnsigned (IntType Int64) = "u64"
          prettyPrimType _ t = pretty t

          puts s = Exp $ simpleCall "Console.Write" [String s]

printValue :: [(Imp.ExternalValue, CSExp)] -> CompilerM op s [CSStmt]
printValue = fmap concat . mapM (uncurry printValue')
  -- We copy non-host arrays to the host before printing.  This is
  -- done in a hacky way - we assume the value has a .get()-method
  -- that returns an equivalent Numpy array.  This works for CSOpenCL,
  -- but we will probably need yet another plugin mechanism here in
  -- the future.
  where printValue' (Imp.OpaqueValue desc _) _ =
          return [Exp $ simpleCall "Console.Write"
                  [String $ "#<opaque " ++ desc ++ ">"]]
        printValue' (Imp.TransparentValue r@Imp.ScalarValue{}) e = do
          p <- printStm r (Integer 0) e
          return [p, Exp $ simpleCall "Console.Write" [String "\n"]]
        printValue' (Imp.TransparentValue r@Imp.ArrayValue{}) e = do
          tuple <- newVName "resultArr"
          i <- newVName "arrInd"
          let i' = Var $ compileName i
          p <- printStm r i' (Var $ compileName tuple)
          let e' = Var $ pretty e
          return [ Assign (Var $ compileName tuple) (Field e' "Item1")
                 , Assign i' (Integer 0)
                 , p
                 , Exp $ simpleCall "Console.Write" [String "\n"]]

prepareEntry :: (Name, Imp.Function op) -> CompilerM op s
                (String, [(CSType, String)], CSType, [CSStmt], [CSStmt], [CSStmt], [CSStmt],
                 [(Imp.ExternalValue, CSExp)], [CSStmt])
prepareEntry (fname, Imp.Function _ outputs inputs _ results args) = do
  let (output_types, output_paramNames) = unzip $ map compileTypedInput outputs
      funTuple = tupleOrSingle $ fmap Var output_paramNames


  (_, sizeDecls) <- collect' $ forM args declsfunction

  (argexps_mem_copies, prepare_run) <- collect' $ forM inputs $ \case
    Imp.MemParam name space -> do
      -- A program might write to its input parameters, so create a new memory
      -- block and copy the source there.  This way the program can be run more
      -- than once.
      name' <- newVName $ baseString name <> "_copy"
      copy <- asks envCopy
      allocate <- asks envAllocate

      let size = Var (compileName name ++ "_nbytes")
          dest = name'
          src = name
          offset = Integer 0
      case space of
        DefaultSpace ->
          stm $ Reassign (Var (compileName name'))
                       (simpleCall "allocateMem" [size]) -- FIXME
        Space sid ->
          allocate name' size sid
      copy dest offset space src offset space size (IntType Int64) -- FIXME
      return $ Just (compileName name')
    _ -> return Nothing

  prepareIn <- collect $ mapM_ entryPointInput $ zip3 [0..] args $
               map (Var . extValueDescName) args
  (res, prepareOut) <- collect' $ mapM entryPointOutput results

  let mem_copies = mapMaybe liftMaybe $ zip argexps_mem_copies inputs
      mem_copy_inits = map initCopy mem_copies

      argexps_lib = map (compileName . Imp.paramName) inputs
      argexps_bin = zipWith fromMaybe argexps_lib argexps_mem_copies
      fname' = futharkFun (nameToString fname)
      arg_types = map (fst . compileTypedInput) inputs
      inputs' = zip arg_types (map extValueDescName args)
      output_type = tupleOrSingleEntryT output_types
      call_lib = [Reassign funTuple $ simpleCall fname' (fmap Var argexps_lib)]
      call_bin = [Reassign funTuple $ simpleCall fname' (fmap Var argexps_bin)]
      prepareIn' = prepareIn ++ mem_copy_inits ++ sizeDecls

  return (nameToString fname, inputs', output_type,
          prepareIn', call_lib, call_bin, prepareOut,
          zip results res, prepare_run)

  where liftMaybe (Just a, b) = Just (a,b)
        liftMaybe _ = Nothing

        initCopy (varName, Imp.MemParam _ space) = declMem' varName space
        initCopy _ = Pass

        valueDescFun (Imp.ArrayValue mem Imp.DefaultSpace _ _ _) =
            stm $ Assign (Var $ compileName mem ++ "_nbytes") (Var $ compileName mem ++ ".Length")
        valueDescFun (Imp.ArrayValue mem (Imp.Space _) bt _ dims) =
            stm $ Assign (Var $ compileName mem ++ "_nbytes") $ foldr (BinOp "*" . compileDim) (sizeOf $ compilePrimTypeToAST bt) dims
        valueDescFun _ = stm Pass

        declsfunction (Imp.TransparentValue v) = valueDescFun v
        declsfunction (Imp.OpaqueValue _ vs) = mapM_ valueDescFun vs

copyMemoryDefaultSpace :: VName -> CSExp -> VName -> CSExp -> CSExp ->
                          CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes =
  stm $ Exp $ simpleCall "Buffer.BlockCopy" [ Var (compileName srcmem), srcidx
                                            , Var (compileName destmem), destidx,
                                              nbytes]

compileEntryFun :: [CSStmt] -> (Name, Imp.Function op)
                -> CompilerM op s CSFunDef
compileEntryFun pre_timing entry@(_,Imp.Function _ outputs _ _ results args) = do
  let params = map (getType &&& extValueDescName) args
  let outputType = tupleOrSingleEntryT $ map getType results

  (fname', _, _, prepareIn, body_lib, _, prepareOut, res, _) <- prepareEntry entry
  let ret = Return $ tupleOrSingleEntry $ map snd res
  let outputDecls = map getDefaultDecl outputs
      do_run = body_lib ++ pre_timing
  (do_run_with_timing, close_runtime_file) <- addTiming do_run

  let do_warmup_run = If (Var "DoWarmupRun") do_run []
      do_num_runs = For "i" (Var "NumRuns") do_run_with_timing

  return $ Def fname' outputType params $
    prepareIn ++ outputDecls ++ [do_warmup_run, do_num_runs, close_runtime_file] ++ prepareOut ++ [ret]

  where getType :: Imp.ExternalValue -> CSType
        getType (Imp.OpaqueValue _ valueDescs) =
          let valueDescs' = map getType' valueDescs
          in Composite $ SystemTupleT valueDescs'
        getType (Imp.TransparentValue valueDesc) =
          getType' valueDesc

        getType' :: Imp.ValueDesc -> CSType
        getType' (Imp.ScalarValue primtype signedness _) =
          compilePrimTypeToASText primtype signedness
        getType' (Imp.ArrayValue _ _ primtype signedness _) =
          let t = compilePrimTypeToASText primtype signedness
          in Composite $ SystemTupleT [Composite $ ArrayT t, Composite $ ArrayT $ Primitive $ CSInt Int64T]


callEntryFun :: [CSStmt] -> (Name, Imp.Function op)
             -> CompilerM op s (CSFunDef, String, CSExp)
callEntryFun pre_timing entry@(fname, Imp.Function _ outputs _ _ _ decl_args) =
  if any isOpaque decl_args then
    return (Def fname' VoidT [] [exitException], nameToString fname, Var fname')
  else do
    (_, _, _, prepareIn, _, body_bin, prepare_out, res, prepare_run) <- prepareEntry entry
    let str_input = map readInput decl_args

    let outputDecls = map getDefaultDecl outputs
        exitcall = [
            Exp $ simpleCall "Console.Error.WriteLine" [formatString "Assertion.{0} failed" [Var "e"]]
          , Exp $ simpleCall "Environment.Exit" [Integer 1]
          ]
        except' = Catch (Var "Exception") exitcall
        do_run = body_bin ++ pre_timing
    (do_run_with_timing, close_runtime_file) <- addTiming do_run

        -- We ignore overflow errors and the like for executable entry
        -- points.  These are (somewhat) well-defined in Futhark.

    let maybe_free =
          [If (BinOp "<" (Var "i") (BinOp "-" (Var "NumRuns") (Integer 1)))
              prepare_out []]

        do_warmup_run =
          If (Var "DoWarmupRun") (prepare_run ++ do_run ++ prepare_out) []

        do_num_runs =
          For "i" (Var "NumRuns") (prepare_run ++ do_run_with_timing ++ maybe_free)

    str_output <- printValue res

    return (Def fname' VoidT [] $
             str_input ++ prepareIn ++ outputDecls ++
             [Try [do_warmup_run, do_num_runs] [except']] ++
             [close_runtime_file] ++
             str_output,

            nameToString fname,

            Var fname')

    where fname' = "entry_" ++ nameToString fname
          isOpaque Imp.TransparentValue{} = False
          isOpaque _ = True

          exitException = Throw $ simpleInitClass "Exception" [String $ "The function " ++ nameToString fname ++ " is not available as an entry function."]

addTiming :: [CSStmt] -> CompilerM s op ([CSStmt], CSStmt)
addTiming statements = do
  syncFun <- asks envSyncFun

  return ([ Assign (Var "StopWatch") $ simpleInitClass "Stopwatch" []
   , syncFun
   , Exp $ simpleCall "StopWatch.Start" [] ] ++
   statements ++
   [ syncFun
   , Exp $ simpleCall "StopWatch.Stop" []
   , Assign (Var "timeElapsed") $ asMicroseconds (Var "StopWatch")
   , If (not_null (Var "RuntimeFile")) [print_runtime] []
   ]
   , If (not_null (Var "RuntimeFile")) [
       Exp $ simpleCall "RuntimeFileWriter.Close" [] ,
       Exp $ simpleCall "RuntimeFile.Close" []
       ] []
    )

  where print_runtime = Exp $ simpleCall "RuntimeFileWriter.WriteLine" [ callMethod (Var "timeElapsed") "ToString" [] ]
        not_null var = BinOp "!=" var Null
        asMicroseconds watch =
          BinOp "/" (Field watch "ElapsedTicks")
         (BinOp "/" (Field (Var "TimeSpan") "TicksPerMillisecond") (Integer 1000))

compileUnOp :: Imp.UnOp -> String
compileUnOp op =
  case op of
    Not -> "!"
    Complement{} -> "~"
    Abs{} -> "Math.Abs" -- actually write these helpers
    FAbs{} -> "Math.Abs"
    SSignum{} -> "ssignum"
    USignum{} -> "usignum"

compileBinOpLike :: Monad m =>
                    Imp.Exp -> Imp.Exp
                 -> CompilerM op s (CSExp, CSExp, String -> m CSExp)
compileBinOpLike x y = do
  x' <- compileExp x
  y' <- compileExp y
  let simple s = return $ BinOp s x' y'
  return (x', y', simple)

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimType :: PrimType -> String
compilePrimType t =
  case t of
    IntType Int8 -> "sbyte"
    IntType Int16 -> "short"
    IntType Int32 -> "int"
    IntType Int64 -> "long"
    FloatType Float32 -> "float"
    FloatType Float64 -> "double"
    Imp.Bool -> "bool"
    Cert -> "bool"

-- | The ctypes type corresponding to a 'PrimType', taking sign into account.
compilePrimTypeExt :: PrimType -> Imp.Signedness -> String
compilePrimTypeExt t ept =
  case (t, ept) of
    (IntType Int8, Imp.TypeUnsigned) -> "byte"
    (IntType Int16, Imp.TypeUnsigned) -> "ushort"
    (IntType Int32, Imp.TypeUnsigned) -> "uint"
    (IntType Int64, Imp.TypeUnsigned) -> "ulong"
    (IntType Int8, _) -> "sbyte"
    (IntType Int16, _) -> "short"
    (IntType Int32, _) -> "int"
    (IntType Int64, _) -> "long"
    (FloatType Float32, _) -> "float"
    (FloatType Float64, _) -> "double"
    (Imp.Bool, _) -> "bool"
    (Cert, _) -> "byte"

-- | Select function to retrieve bytes from byte array as specific data type
-- | The ctypes type corresponding to a 'PrimType'.
compileTypecastExt :: PrimType -> Imp.Signedness -> (CSExp -> CSExp)
compileTypecastExt t ept =
  let t' = case (t, ept) of
       (IntType Int8     , Imp.TypeUnsigned)-> Primitive $ CSUInt UInt8T
       (IntType Int16    , Imp.TypeUnsigned)-> Primitive $ CSUInt UInt16T
       (IntType Int32    , Imp.TypeUnsigned)-> Primitive $ CSUInt UInt32T
       (IntType Int64    , Imp.TypeUnsigned)-> Primitive $ CSUInt UInt64T
       (IntType Int8     , _)-> Primitive $ CSInt Int8T
       (IntType Int16    , _)-> Primitive $ CSInt Int16T
       (IntType Int32    , _)-> Primitive $ CSInt Int32T
       (IntType Int64    , _)-> Primitive $ CSInt Int64T
       (FloatType Float32, _)-> Primitive $ CSFloat FloatT
       (FloatType Float64, _)-> Primitive $ CSFloat DoubleT
       (Imp.Bool         , _)-> Primitive BoolT
       (Cert, _)-> Primitive $ CSInt Int8T
  in Cast t'

-- | The ctypes type corresponding to a 'PrimType'.
compileTypecast :: PrimType -> (CSExp -> CSExp)
compileTypecast t =
  let t' = case t of
        IntType Int8 -> Primitive $ CSInt Int8T
        IntType Int16 -> Primitive $ CSInt Int16T
        IntType Int32 -> Primitive $ CSInt Int32T
        IntType Int64 -> Primitive $ CSInt Int64T
        FloatType Float32 -> Primitive $ CSFloat FloatT
        FloatType Float64 -> Primitive $ CSFloat DoubleT
        Imp.Bool -> Primitive BoolT
        Cert -> Primitive $ CSInt Int8T
  in Cast t'

-- | The ctypes type corresponding to a 'PrimType'.
compilePrimValue :: Imp.PrimValue -> CSExp
compilePrimValue (IntValue (Int8Value v)) =
  Cast (Primitive $ CSInt Int8T) $ Integer $ toInteger v
compilePrimValue (IntValue (Int16Value v)) =
  Cast (Primitive $ CSInt Int16T) $ Integer $ toInteger v
compilePrimValue (IntValue (Int32Value v)) =
  Cast (Primitive $ CSInt Int32T) $ Integer $ toInteger v
compilePrimValue (IntValue (Int64Value v)) =
  Cast (Primitive $ CSInt Int64T) $ Integer $ toInteger v
compilePrimValue (FloatValue (Float32Value v))
  | isInfinite v =
      if v > 0 then Var "Single.PositiveInfinity" else Var "Single.NegativeInfinity"
  | isNaN v =
      Var "Single.NaN"
  | otherwise = Cast (Primitive $ CSFloat FloatT) (Float $ fromRational $ toRational v)
compilePrimValue (FloatValue (Float64Value v))
  | isInfinite v =
      if v > 0 then Var "Double.PositiveInfinity" else Var "Double.NegativeInfinity"
  | isNaN v =
      Var "Double.NaN"
  | otherwise = Cast (Primitive $ CSFloat DoubleT) (Float $ fromRational $ toRational v)
compilePrimValue (BoolValue v) = Bool v
compilePrimValue Checked = Bool True

compileExp :: Imp.Exp -> CompilerM op s CSExp

compileExp (Imp.ValueExp v) = return $ compilePrimValue v

compileExp (Imp.LeafExp (Imp.ScalarVar vname) _) =
  return $ Var $ compileName vname

compileExp (Imp.LeafExp (Imp.SizeOf t) _) =
  return $ (compileTypecast $ IntType Int32) (Integer $ primByteSize t)

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) (IntType Int8) DefaultSpace _) _) = do
  let src' = compileName src
  iexp' <- compileExp iexp
  return $ Cast (Primitive $ CSInt Int8T) (Index (Var src') (IdxExp iexp'))

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) bt DefaultSpace _) _) = do
  iexp' <- compileExp iexp
  let bt' = compilePrimType bt
  return $ simpleCall ("indexArray_" ++ bt') [Var $ compileName src, iexp']

compileExp (Imp.LeafExp (Imp.Index src (Imp.Count iexp) restype (Imp.Space space) _) _) =
  join $ asks envReadScalar
    <*> pure src <*> compileExp iexp
    <*> pure restype <*> pure space

compileExp (Imp.BinOpExp op x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case op of
    FAdd{} -> simple "+"
    FSub{} -> simple "-"
    FMul{} -> simple "*"
    FDiv{} -> simple "/"
    FMod{} -> simple "%"
    LogAnd{} -> simple "&&"
    LogOr{} -> simple "||"
    _ -> return $ simpleCall (pretty op) [x', y']

compileExp (Imp.ConvOpExp conv x) = do
  x' <- compileExp x
  return $ simpleCall (pretty conv) [x']

compileExp (Imp.CmpOpExp cmp x y) = do
  (x', y', simple) <- compileBinOpLike x y
  case cmp of
    CmpEq{} -> simple "=="
    FCmpLt{} -> simple "<"
    FCmpLe{} -> simple "<="
    _ -> return $ simpleCall (pretty cmp) [x', y']

compileExp (Imp.UnOpExp op exp1) =
  PreUnOp (compileUnOp op) <$> compileExp exp1

compileExp (Imp.FunExp h args _) =
  simpleCall (futharkFun (pretty h)) <$> mapM compileExp args

compileCode :: Imp.Code op -> CompilerM op s ()

compileCode Imp.DebugPrint{} =
  return ()

compileCode (Imp.Op op) =
  join $ asks envOpCompiler <*> pure op

compileCode (Imp.If cond tb fb) = do
  cond' <- compileExp cond
  tb' <- blockScope $ compileCode tb
  fb' <- blockScope $ compileCode fb
  stm $ If cond' tb' fb'

compileCode (c1 Imp.:>>: c2) = do
  compileCode c1
  compileCode c2

compileCode (Imp.While cond body) = do
  cond' <- compileExp cond
  body' <- blockScope $ compileCode body
  stm $ While cond' body'

compileCode (Imp.For i it bound body) = do
  bound' <- compileExp bound
  let i' = compileName i
  body' <- blockScope $ compileCode body
  counter <- pretty <$> newVName "counter"
  one <- pretty <$> newVName "one"
  stm $ Assign (Var i') $ compileTypecast (IntType it) (Integer 0)
  stm $ Assign (Var one) $ compileTypecast (IntType it) (Integer 1)
  stm $ For counter bound' $ body' ++
    [AssignOp "+" (Var i') (Var one)]


compileCode (Imp.SetScalar vname exp1) = do
  let name' = Var $ compileName vname
  exp1' <- compileExp exp1
  stm $ Reassign name' exp1'

compileCode (Imp.DeclareMem v space) = declMem v space

compileCode (Imp.DeclareScalar v Cert) =
  stm $ Assign (Var $ compileName v) $ Bool True
compileCode (Imp.DeclareScalar v t) =
  stm $ AssignTyped t' (Var $ compileName v) Nothing
  where t' = compilePrimTypeToAST t

compileCode (Imp.DeclareArray name DefaultSpace t vs) =
  stms [Assign (Var $ "init_"++name') $
        simpleCall "unwrapArray"
         [
           case vs of Imp.ArrayValues vs' ->
                        CreateArray (compilePrimTypeToAST t) $ Right $ map compilePrimValue vs'
                      Imp.ArrayZeros n ->
                        CreateArray (compilePrimTypeToAST t) $ Left n
         , simpleCall "sizeof" [Var $ compilePrimType t]
         ]
       , Assign (Var name') $ Var ("init_"++name')
       ]
  where name' = compileName name

compileCode (Imp.DeclareArray name (Space space) t vs) =
  join $ asks envStaticArray <*>
  pure name <*> pure space <*> pure t <*> pure vs

compileCode (Imp.Comment s code) = do
  code' <- blockScope $ compileCode code
  stm $ Comment s code'

compileCode (Imp.Assert e (Imp.ErrorMsg parts) (loc,locs)) = do
  e' <- compileExp e
  let onPart (i, Imp.ErrorString s) = return (printFormatArg i, String s)
      onPart (i, Imp.ErrorInt32 x) = (printFormatArg i,) <$> compileExp x
  (formatstrs, formatargs) <- unzip <$> mapM onPart (zip ([1..] :: [Integer]) parts)
  stm $ Assert e' $ String ("Error at {0}:\n" <> concat formatstrs) : (String stacktrace : formatargs)
  where stacktrace = intercalate " -> " (reverse $ map locStr $ loc:locs)
        printFormatArg = printf "{%d}"

compileCode (Imp.Call dests fname args) = do
  args' <- mapM compileArg args
  let dests' = tupleOrSingle $ fmap Var (map compileName dests)
      fname' = futharkFun (pretty fname)
      call' = simpleCall fname' args'
  -- If the function returns nothing (is called only for side
  -- effects), take care not to assign to an empty tuple.
  stm $ if null dests
        then Exp call'
        else Reassign dests' call'
  where compileArg (Imp.MemArg m) = return $ Var $ compileName m
        compileArg (Imp.ExpArg e) = compileExp e

compileCode (Imp.SetMem dest src DefaultSpace) = do
  let src' = Var (compileName src)
  let dest' = Var (compileName dest)
  stm $ Reassign dest' src'

compileCode (Imp.SetMem dest src _) = do
  let src' = Var (compileName src)
  let dest' = Var (compileName dest)
  stm $ Exp $ simpleCall "MemblockSetDevice" [Ref $ Var "Ctx", Ref dest', Ref src', String (compileName src)]

compileCode (Imp.Allocate name (Imp.Count e) DefaultSpace) = do
  e' <- compileExp e
  let allocate' = simpleCall "allocateMem" [e']
  let name' = Var (compileName name)
  stm $ Reassign name' allocate'

compileCode (Imp.Allocate name (Imp.Count e) (Imp.Space space)) =
  join $ asks envAllocate
    <*> pure name
    <*> compileExp e
    <*> pure space

compileCode (Imp.Free name space) = do
  unRefMem name space
  tell $ mempty { accFreedMem = [name] }

compileCode (Imp.Copy dest (Imp.Count destoffset) DefaultSpace src (Imp.Count srcoffset) DefaultSpace (Imp.Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  let dest' = Var (compileName dest)
  let src' = Var (compileName src)
  size' <- compileExp size
  stm $ Exp $ simpleCall "Buffer.BlockCopy" [src', srcoffset', dest', destoffset', size']

compileCode (Imp.Copy dest (Imp.Count destoffset) destspace src (Imp.Count srcoffset) srcspace (Imp.Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> pure dest <*> compileExp destoffset <*> pure destspace
    <*> pure src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size <*> pure (IntType Int64) -- FIXME

compileCode (Imp.Write dest (Imp.Count idx) elemtype DefaultSpace _ elemexp) = do
  idx' <- compileExp idx
  elemexp' <- compileExp elemexp
  let dest' = Var $ compileName dest
  let elemtype' = compileTypecast elemtype
  let ctype = elemtype' elemexp'
  stm $ Exp $ simpleCall "writeScalarArray" [dest', idx', ctype]

compileCode (Imp.Write dest (Imp.Count idx) elemtype (Imp.Space space) _ elemexp) =
  join $ asks envWriteScalar
    <*> pure dest
    <*> compileExp idx
    <*> pure elemtype
    <*> pure space
    <*> compileExp elemexp

compileCode Imp.Skip = return ()

blockScope :: CompilerM op s () -> CompilerM op s [CSStmt]
blockScope = fmap snd . blockScope'

blockScope' :: CompilerM op s a -> CompilerM op s (a, [CSStmt])
blockScope' m = do
  old_allocs <- gets compDeclaredMem
  (x, items) <- pass $ do
    (x, w) <- listen m
    let items = accItems w
    return ((x, items), const mempty)
  new_allocs <- gets $ filter (`notElem` old_allocs) . compDeclaredMem
  modify $ \s -> s { compDeclaredMem = old_allocs }
  releases <- collect $ mapM_ (uncurry unRefMem) new_allocs
  return (x, items <> releases)

unRefMem :: VName -> Space -> CompilerM op s ()
unRefMem mem (Space "device") =
  (stm . Exp) $ simpleCall "MemblockUnrefDevice" [ Ref $ Var "Ctx"
                                                 , (Ref . Var . compileName) mem
                                                 , (String . compileName) mem]
unRefMem _ DefaultSpace = stm Pass
unRefMem _ (Space "local") = stm Pass
unRefMem _ (Space _) = error "The default compiler cannot compile unRefMem for other spaces"


-- | Public names must have a consistent prefix.
publicName :: String -> String
publicName s = "Futhark" ++ s

declMem :: VName -> Space -> CompilerM op s ()
declMem name space = do
  modify $ \s -> s { compDeclaredMem = (name, space) : compDeclaredMem s}
  stm $ declMem' (compileName name) space

declMem' :: String -> Space -> CSStmt
declMem' name DefaultSpace =
  AssignTyped (Composite $ ArrayT $ Primitive ByteT) (Var name) Nothing
declMem' name (Space _) =
  AssignTyped (CustomT "OpenCLMemblock") (Var name) (Just $ simpleCall "EmptyMemblock" [Var "Ctx.EMPTY_MEM_HANDLE"])

rawMemCSType :: Space -> CSType
rawMemCSType DefaultSpace = Composite $ ArrayT $ Primitive ByteT
rawMemCSType (Space _) = CustomT "OpenCLMemblock"

toIntPtr :: CSExp -> CSExp
toIntPtr e = simpleInitClass "IntPtr" [e]
