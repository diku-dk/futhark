{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}

-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg,
    compileProg',
    CParts (..),
    asLibrary,
    asExecutable,
    asServer,

    -- * Pluggable compiler
    Operations (..),
    defaultOperations,
    OpCompiler,
    ErrorCompiler,
    CallCompiler,
    PointerQuals,
    MemoryType,
    WriteScalar,
    writeScalarPointerWithQuals,
    ReadScalar,
    readScalarPointerWithQuals,
    Allocate,
    Deallocate,
    CopyBarrier (..),
    Copy,
    StaticArray,

    -- * Monadic compiler interface
    CompilerM,
    CompilerState (compUserState, compNameSrc, compDeclaredMem),
    CompilerEnv (envCachedMem),
    getUserState,
    modifyUserState,
    contextContents,
    contextFinalInits,
    runCompilerM,
    inNewFunction,
    cachingMemory,
    compileFun,
    compileCode,
    compileExp,
    compilePrimExp,
    compileExpToName,
    rawMem,
    item,
    items,
    stm,
    stms,
    decl,
    atInit,
    headerDecl,
    publicDef,
    publicDef_,
    profileReport,
    onClear,
    HeaderSection (..),
    libDecl,
    earlyDecl,
    publicName,
    contextField,
    contextFieldDyn,
    memToCType,
    cacheMem,
    fatMemory,
    rawMemCType,
    cproduct,
    fatMemType,
    declAllocatedMem,
    freeAllocatedMem,
    collect,

    -- * Building Blocks
    primTypeToCType,
    intTypeToCType,
    copyMemoryDefaultSpace,
    linearCode,
    derefPointer,
    fatMemAlloc,
    fatMemSet,
    fatMemUnRef,
    errorMsgString,
  )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.DList as DL
import Data.List (unzip4)
import Data.Loc
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Futhark.CodeGen.Backends.GenericC.CLI (cliDefs)
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.GenericC.Server (serverDefs)
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.RTS.C (cacheH, errorsH, halfH, lockH, timingH, utilH)
import Futhark.IR.Prop (isBuiltInFunction)
import qualified Futhark.Manifest as Manifest
import Futhark.MonadFreshNames
import Futhark.Util (chunks, mapAccumLM, zEncodeString)
import Futhark.Util.Pretty (prettyText)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C
import NeatInterpolation (untrimming)

-- How public an array type definition sould be.  Public types show up
-- in the generated API, while private types are used only to
-- implement the members of opaques.
data Publicness = Private | Public
  deriving (Eq, Ord, Show)

type ArrayType = (Signedness, PrimType, Int)

data CompilerState s = CompilerState
  { compArrayTypes :: M.Map ArrayType Publicness,
    compEarlyDecls :: DL.DList C.Definition,
    compInit :: [C.Stm],
    compNameSrc :: VNameSource,
    compUserState :: s,
    compHeaderDecls :: M.Map HeaderSection (DL.DList C.Definition),
    compLibDecls :: DL.DList C.Definition,
    compCtxFields :: DL.DList (C.Id, C.Type, Maybe C.Exp, Maybe C.Stm),
    compProfileItems :: DL.DList C.BlockItem,
    compClearItems :: DL.DList C.BlockItem,
    compDeclaredMem :: [(VName, Space)],
    compItems :: DL.DList C.BlockItem
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s =
  CompilerState
    { compArrayTypes = mempty,
      compEarlyDecls = mempty,
      compInit = [],
      compNameSrc = src,
      compUserState = s,
      compHeaderDecls = mempty,
      compLibDecls = mempty,
      compCtxFields = mempty,
      compProfileItems = mempty,
      compClearItems = mempty,
      compDeclaredMem = mempty,
      compItems = mempty
    }

-- | In which part of the header file we put the declaration.  This is
-- to ensure that the header file remains structured and readable.
data HeaderSection
  = ArrayDecl String
  | OpaqueTypeDecl String
  | OpaqueDecl String
  | EntryDecl
  | MiscDecl
  | InitDecl
  deriving (Eq, Ord)

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

type ErrorCompiler op s = ErrorMsg Exp -> String -> CompilerM op s ()

-- | The address space qualifiers for a pointer of the given type with
-- the given annotation.
type PointerQuals op s = String -> CompilerM op s [C.TypeQual]

-- | The type of a memory block in the given memory space.
type MemoryType op s = SpaceId -> CompilerM op s C.Type

-- | Write a scalar to the given memory block with the given element
-- index and in the given memory space.
type WriteScalar op s =
  C.Exp -> C.Exp -> C.Type -> SpaceId -> Volatility -> C.Exp -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given element
-- index and in the given memory space.
type ReadScalar op s =
  C.Exp -> C.Exp -> C.Type -> SpaceId -> Volatility -> CompilerM op s C.Exp

-- | Allocate a memory block of the given size and with the given tag
-- in the given memory space, saving a reference in the given variable
-- name.
type Allocate op s =
  C.Exp ->
  C.Exp ->
  C.Exp ->
  SpaceId ->
  CompilerM op s ()

-- | De-allocate the given memory block with the given tag, which is
-- in the given memory space.
type Deallocate op s = C.Exp -> C.Exp -> SpaceId -> CompilerM op s ()

-- | Create a static array of values - initialised at load time.
type StaticArray op s = VName -> SpaceId -> PrimType -> ArrayContents -> CompilerM op s ()

-- | Whether a copying operation should implicitly function as a
-- barrier regarding further operations on the source.  This is a
-- rather subtle detail and is mostly useful for letting some
-- device/GPU copies be asynchronous (#1664).
data CopyBarrier
  = CopyBarrier
  | -- | Explicit context synchronisation should be done
    -- before the source or target is used.
    CopyNoBarrier
  deriving (Eq, Show)

-- | Copy from one memory block to another.
type Copy op s =
  CopyBarrier ->
  C.Exp ->
  C.Exp ->
  Space ->
  C.Exp ->
  C.Exp ->
  Space ->
  C.Exp ->
  CompilerM op s ()

-- | Call a function.
type CallCompiler op s = [VName] -> Name -> [C.Exp] -> CompilerM op s ()

data Operations op s = Operations
  { opsWriteScalar :: WriteScalar op s,
    opsReadScalar :: ReadScalar op s,
    opsAllocate :: Allocate op s,
    opsDeallocate :: Deallocate op s,
    opsCopy :: Copy op s,
    opsStaticArray :: StaticArray op s,
    opsMemoryType :: MemoryType op s,
    opsCompiler :: OpCompiler op s,
    opsError :: ErrorCompiler op s,
    opsCall :: CallCompiler op s,
    -- | If true, use reference counting.  Otherwise, bare
    -- pointers.
    opsFatMemory :: Bool,
    -- | Code to bracket critical sections.
    opsCritical :: ([C.BlockItem], [C.BlockItem])
  }

errorMsgString :: ErrorMsg Exp -> CompilerM op s (String, [C.Exp])
errorMsgString (ErrorMsg parts) = do
  let boolStr e = [C.cexp|($exp:e) ? "true" : "false"|]
      asLongLong e = [C.cexp|(long long int)$exp:e|]
      asDouble e = [C.cexp|(double)$exp:e|]
      onPart (ErrorString s) = pure ("%s", [C.cexp|$string:s|])
      onPart (ErrorVal Bool x) = ("%s",) . boolStr <$> compileExp x
      onPart (ErrorVal Unit _) = pure ("%s", [C.cexp|"()"|])
      onPart (ErrorVal (IntType Int8) x) = ("%hhd",) <$> compileExp x
      onPart (ErrorVal (IntType Int16) x) = ("%hd",) <$> compileExp x
      onPart (ErrorVal (IntType Int32) x) = ("%d",) <$> compileExp x
      onPart (ErrorVal (IntType Int64) x) = ("%lld",) . asLongLong <$> compileExp x
      onPart (ErrorVal (FloatType Float16) x) = ("%f",) . asDouble <$> compileExp x
      onPart (ErrorVal (FloatType Float32) x) = ("%f",) . asDouble <$> compileExp x
      onPart (ErrorVal (FloatType Float64) x) = ("%f",) <$> compileExp x
  (formatstrs, formatargs) <- unzip <$> mapM onPart parts
  pure (mconcat formatstrs, formatargs)

freeAllocatedMem :: CompilerM op s [C.BlockItem]
freeAllocatedMem = collect $ mapM_ (uncurry unRefMem) =<< gets compDeclaredMem

declAllocatedMem :: CompilerM op s [C.BlockItem]
declAllocatedMem = collect $ mapM_ f =<< gets compDeclaredMem
  where
    f (name, space) = do
      ty <- memToCType name space
      decl [C.cdecl|$ty:ty $id:name;|]
      resetMem name space

defError :: ErrorCompiler op s
defError msg stacktrace = do
  (formatstr, formatargs) <- errorMsgString msg
  let formatstr' = "Error: " <> formatstr <> "\n\nBacktrace:\n%s"
  items
    [C.citems|if (ctx->error == NULL)
                ctx->error = msgprintf($string:formatstr', $args:formatargs, $string:stacktrace);
              err = FUTHARK_PROGRAM_ERROR;
              goto cleanup;|]

defCall :: CallCompiler op s
defCall dests fname args = do
  let out_args = [[C.cexp|&$id:d|] | d <- dests]
      args'
        | isBuiltInFunction fname = args
        | otherwise = [C.cexp|ctx|] : out_args ++ args
  case dests of
    [dest]
      | isBuiltInFunction fname ->
          stm [C.cstm|$id:dest = $id:(funName fname)($args:args');|]
    _ ->
      item [C.citem|if ($id:(funName fname)($args:args') != 0) { err = 1; goto cleanup; }|]

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations =
  Operations
    { opsWriteScalar = defWriteScalar,
      opsReadScalar = defReadScalar,
      opsAllocate = defAllocate,
      opsDeallocate = defDeallocate,
      opsCopy = defCopy,
      opsStaticArray = defStaticArray,
      opsMemoryType = defMemoryType,
      opsCompiler = defCompiler,
      opsFatMemory = True,
      opsError = defError,
      opsCall = defCall,
      opsCritical = mempty
    }
  where
    defWriteScalar _ _ _ _ _ =
      error "Cannot write to non-default memory space because I am dumb"
    defReadScalar _ _ _ _ =
      error "Cannot read from non-default memory space"
    defAllocate _ _ _ =
      error "Cannot allocate in non-default memory space"
    defDeallocate _ _ =
      error "Cannot deallocate in non-default memory space"
    defCopy _ destmem destoffset DefaultSpace srcmem srcoffset DefaultSpace size =
      copyMemoryDefaultSpace destmem destoffset srcmem srcoffset size
    defCopy _ _ _ _ _ _ _ _ =
      error "Cannot copy to or from non-default memory space"
    defStaticArray _ _ _ _ =
      error "Cannot create static array in non-default memory space"
    defMemoryType _ =
      error "Has no type for non-default memory space"
    defCompiler _ =
      error "The default compiler cannot compile extended operations"

data CompilerEnv op s = CompilerEnv
  { envOperations :: Operations op s,
    -- | Mapping memory blocks to sizes.  These memory blocks are CPU
    -- memory that we know are used in particularly simple ways (no
    -- reference counting necessary).  To cut down on allocator
    -- pressure, we keep these allocations around for a long time, and
    -- record their sizes so we can reuse them if possible (and
    -- realloc() when needed).
    envCachedMem :: M.Map C.Exp VName
  }

envOpCompiler :: CompilerEnv op s -> OpCompiler op s
envOpCompiler = opsCompiler . envOperations

envMemoryType :: CompilerEnv op s -> MemoryType op s
envMemoryType = opsMemoryType . envOperations

envReadScalar :: CompilerEnv op s -> ReadScalar op s
envReadScalar = opsReadScalar . envOperations

envWriteScalar :: CompilerEnv op s -> WriteScalar op s
envWriteScalar = opsWriteScalar . envOperations

envAllocate :: CompilerEnv op s -> Allocate op s
envAllocate = opsAllocate . envOperations

envDeallocate :: CompilerEnv op s -> Deallocate op s
envDeallocate = opsDeallocate . envOperations

envCopy :: CompilerEnv op s -> Copy op s
envCopy = opsCopy . envOperations

envStaticArray :: CompilerEnv op s -> StaticArray op s
envStaticArray = opsStaticArray . envOperations

envFatMemory :: CompilerEnv op s -> Bool
envFatMemory = opsFatMemory . envOperations

declsCode :: (HeaderSection -> Bool) -> CompilerState s -> T.Text
declsCode p =
  T.unlines
    . map prettyText
    . concatMap (DL.toList . snd)
    . filter (p . fst)
    . M.toList
    . compHeaderDecls

initDecls, arrayDecls, opaqueDecls, opaqueTypeDecls, entryDecls, miscDecls :: CompilerState s -> T.Text
initDecls = declsCode (== InitDecl)
arrayDecls = declsCode isArrayDecl
  where
    isArrayDecl ArrayDecl {} = True
    isArrayDecl _ = False
opaqueTypeDecls = declsCode isOpaqueTypeDecl
  where
    isOpaqueTypeDecl OpaqueTypeDecl {} = True
    isOpaqueTypeDecl _ = False
opaqueDecls = declsCode isOpaqueDecl
  where
    isOpaqueDecl OpaqueDecl {} = True
    isOpaqueDecl _ = False
entryDecls = declsCode (== EntryDecl)
miscDecls = declsCode (== MiscDecl)

contextContents :: CompilerM op s ([C.FieldGroup], [C.Stm], [C.Stm])
contextContents = do
  (field_names, field_types, field_values, field_frees) <-
    gets $ unzip4 . DL.toList . compCtxFields
  let fields =
        [ [C.csdecl|$ty:ty $id:name;|]
          | (name, ty) <- zip field_names field_types
        ]
      init_fields =
        [ [C.cstm|ctx->$id:name = $exp:e;|]
          | (name, Just e) <- zip field_names field_values
        ]
  pure (fields, init_fields, catMaybes field_frees)

contextFinalInits :: CompilerM op s [C.Stm]
contextFinalInits = gets compInit

newtype CompilerM op s a
  = CompilerM (ReaderT (CompilerEnv op s) (State (CompilerState s)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (CompilerState s),
      MonadReader (CompilerEnv op s)
    )

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s {compNameSrc = src}

runCompilerM ::
  Operations op s ->
  VNameSource ->
  s ->
  CompilerM op s a ->
  (a, CompilerState s)
runCompilerM ops src userstate (CompilerM m) =
  runState
    (runReaderT m (CompilerEnv ops mempty))
    (newCompilerState src userstate)

getUserState :: CompilerM op s s
getUserState = gets compUserState

modifyUserState :: (s -> s) -> CompilerM op s ()
modifyUserState f = modify $ \compstate ->
  compstate {compUserState = f $ compUserState compstate}

atInit :: C.Stm -> CompilerM op s ()
atInit x = modify $ \s ->
  s {compInit = compInit s ++ [x]}

collect :: CompilerM op s () -> CompilerM op s [C.BlockItem]
collect m = snd <$> collect' m

collect' :: CompilerM op s a -> CompilerM op s (a, [C.BlockItem])
collect' m = do
  old <- gets compItems
  modify $ \s -> s {compItems = mempty}
  x <- m
  new <- gets compItems
  modify $ \s -> s {compItems = old}
  pure (x, DL.toList new)

-- | Used when we, inside an existing 'CompilerM' action, want to
-- generate code for a new function.  Use this so that the compiler
-- understands that previously declared memory doesn't need to be
-- freed inside this action.
inNewFunction :: CompilerM op s a -> CompilerM op s a
inNewFunction m = do
  old_mem <- gets compDeclaredMem
  modify $ \s -> s {compDeclaredMem = mempty}
  x <- local noCached m
  modify $ \s -> s {compDeclaredMem = old_mem}
  pure x
  where
    noCached env = env {envCachedMem = mempty}

item :: C.BlockItem -> CompilerM op s ()
item x = modify $ \s -> s {compItems = DL.snoc (compItems s) x}

items :: [C.BlockItem] -> CompilerM op s ()
items xs = modify $ \s -> s {compItems = DL.append (compItems s) (DL.fromList xs)}

fatMemory :: Space -> CompilerM op s Bool
fatMemory ScalarSpace {} = pure False
fatMemory _ = asks envFatMemory

cacheMem :: C.ToExp a => a -> CompilerM op s (Maybe VName)
cacheMem a = asks $ M.lookup (C.toExp a noLoc) . envCachedMem

-- | Construct a publicly visible definition using the specified name
-- as the template.  The first returned definition is put in the
-- header file, and the second is the implementation.  Returns the public
-- name.
publicDef ::
  String ->
  HeaderSection ->
  (String -> (C.Definition, C.Definition)) ->
  CompilerM op s String
publicDef s h f = do
  s' <- publicName s
  let (pub, priv) = f s'
  headerDecl h pub
  earlyDecl priv
  pure s'

-- | As 'publicDef', but ignores the public name.
publicDef_ ::
  String ->
  HeaderSection ->
  (String -> (C.Definition, C.Definition)) ->
  CompilerM op s ()
publicDef_ s h f = void $ publicDef s h f

headerDecl :: HeaderSection -> C.Definition -> CompilerM op s ()
headerDecl sec def = modify $ \s ->
  s
    { compHeaderDecls =
        M.unionWith
          (<>)
          (compHeaderDecls s)
          (M.singleton sec (DL.singleton def))
    }

libDecl :: C.Definition -> CompilerM op s ()
libDecl def = modify $ \s ->
  s {compLibDecls = compLibDecls s <> DL.singleton def}

earlyDecl :: C.Definition -> CompilerM op s ()
earlyDecl def = modify $ \s ->
  s {compEarlyDecls = compEarlyDecls s <> DL.singleton def}

contextField :: C.Id -> C.Type -> Maybe C.Exp -> CompilerM op s ()
contextField name ty initial = modify $ \s ->
  s {compCtxFields = compCtxFields s <> DL.singleton (name, ty, initial, Nothing)}

contextFieldDyn :: C.Id -> C.Type -> Maybe C.Exp -> C.Stm -> CompilerM op s ()
contextFieldDyn name ty initial free = modify $ \s ->
  s {compCtxFields = compCtxFields s <> DL.singleton (name, ty, initial, Just free)}

profileReport :: C.BlockItem -> CompilerM op s ()
profileReport x = modify $ \s ->
  s {compProfileItems = compProfileItems s <> DL.singleton x}

onClear :: C.BlockItem -> CompilerM op s ()
onClear x = modify $ \s ->
  s {compClearItems = compClearItems s <> DL.singleton x}

stm :: C.Stm -> CompilerM op s ()
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op s ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op s ()
decl x = item [C.citem|$decl:x;|]

-- | Public names must have a consitent prefix.
publicName :: String -> CompilerM op s String
publicName s = pure $ "futhark_" ++ s

-- | The generated code must define a context struct with this name.
contextType :: CompilerM op s C.Type
contextType = do
  name <- publicName "context"
  pure [C.cty|struct $id:name|]

-- | The generated code must define a configuration struct with this
-- name.
configType :: CompilerM op s C.Type
configType = do
  name <- publicName "context_config"
  pure [C.cty|struct $id:name|]

memToCType :: VName -> Space -> CompilerM op s C.Type
memToCType v space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem v
  if refcount && not cached
    then pure $ fatMemType space
    else rawMemCType space

rawMemCType :: Space -> CompilerM op s C.Type
rawMemCType DefaultSpace = pure defaultMemBlockType
rawMemCType (Space sid) = join $ asks envMemoryType <*> pure sid
rawMemCType (ScalarSpace [] t) =
  pure [C.cty|$ty:(primTypeToCType t)[1]|]
rawMemCType (ScalarSpace ds t) =
  pure [C.cty|$ty:(primTypeToCType t)[$exp:(cproduct ds')]|]
  where
    ds' = map (`C.toExp` noLoc) ds

fatMemType :: Space -> C.Type
fatMemType space =
  [C.cty|struct $id:name|]
  where
    name = case space of
      Space sid -> "memblock_" ++ sid
      _ -> "memblock"

fatMemSet :: Space -> String
fatMemSet (Space sid) = "memblock_set_" ++ sid
fatMemSet _ = "memblock_set"

fatMemAlloc :: Space -> String
fatMemAlloc (Space sid) = "memblock_alloc_" ++ sid
fatMemAlloc _ = "memblock_alloc"

fatMemUnRef :: Space -> String
fatMemUnRef (Space sid) = "memblock_unref_" ++ sid
fatMemUnRef _ = "memblock_unref"

rawMem :: VName -> CompilerM op s C.Exp
rawMem v = rawMem' <$> fat <*> pure v
  where
    fat = asks ((&&) . envFatMemory) <*> (isNothing <$> cacheMem v)

rawMem' :: C.ToExp a => Bool -> a -> C.Exp
rawMem' True e = [C.cexp|$exp:e.mem|]
rawMem' False e = [C.cexp|$exp:e|]

allocRawMem ::
  (C.ToExp a, C.ToExp b, C.ToExp c) =>
  a ->
  b ->
  Space ->
  c ->
  CompilerM op s ()
allocRawMem dest size space desc = case space of
  Space sid ->
    join $
      asks envAllocate
        <*> pure [C.cexp|$exp:dest|]
        <*> pure [C.cexp|$exp:size|]
        <*> pure [C.cexp|$exp:desc|]
        <*> pure sid
  _ ->
    stm [C.cstm|$exp:dest = (unsigned char*) malloc((size_t)$exp:size);|]

freeRawMem ::
  (C.ToExp a, C.ToExp b) =>
  a ->
  Space ->
  b ->
  CompilerM op s ()
freeRawMem mem space desc =
  case space of
    Space sid -> do
      free_mem <- asks envDeallocate
      free_mem [C.cexp|$exp:mem|] [C.cexp|$exp:desc|] sid
    _ -> item [C.citem|free($exp:mem);|]

defineMemorySpace :: Space -> CompilerM op s (C.Definition, [C.Definition], C.BlockItem)
defineMemorySpace space = do
  rm <- rawMemCType space
  let structdef =
        [C.cedecl|struct $id:sname { int *references;
                                     $ty:rm mem;
                                     typename int64_t size;
                                     const char *desc; };|]

  contextField peakname [C.cty|typename int64_t|] $ Just [C.cexp|0|]
  contextField usagename [C.cty|typename int64_t|] $ Just [C.cexp|0|]

  -- Unreferencing a memory block consists of decreasing its reference
  -- count and freeing the corresponding memory if the count reaches
  -- zero.
  free <- collect $ freeRawMem [C.cexp|block->mem|] space [C.cexp|desc|]
  ctx_ty <- contextType
  let unrefdef =
        [C.cedecl|int $id:(fatMemUnRef space) ($ty:ctx_ty *ctx, $ty:mty *block, const char *desc) {
  if (block->references != NULL) {
    *(block->references) -= 1;
    if (ctx->detail_memory) {
      fprintf(ctx->log, "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                      desc, block->desc, $string:spacedesc, *(block->references));
    }
    if (*(block->references) == 0) {
      ctx->$id:usagename -= block->size;
      $items:free
      free(block->references);
      if (ctx->detail_memory) {
        fprintf(ctx->log, "%lld bytes freed (now allocated: %lld bytes)\n",
                (long long) block->size, (long long) ctx->$id:usagename);
      }
    }
    block->references = NULL;
  }
  return 0;
}|]

  -- When allocating a memory block we initialise the reference count to 1.
  alloc <-
    collect $
      allocRawMem [C.cexp|block->mem|] [C.cexp|size|] space [C.cexp|desc|]
  let allocdef =
        [C.cedecl|int $id:(fatMemAlloc space) ($ty:ctx_ty *ctx, $ty:mty *block, typename int64_t size, const char *desc) {
  if (size < 0) {
    futhark_panic(1, "Negative allocation of %lld bytes attempted for %s in %s.\n",
          (long long)size, desc, $string:spacedesc, ctx->$id:usagename);
  }
  int ret = $id:(fatMemUnRef space)(ctx, block, desc);

  if (ret != FUTHARK_SUCCESS) {
    return ret;
  }

  if (ctx->detail_memory) {
    fprintf(ctx->log, "Allocating %lld bytes for %s in %s (then allocated: %lld bytes)",
            (long long) size,
            desc, $string:spacedesc,
            (long long) ctx->$id:usagename + size);
  }
  if (ctx->$id:usagename > ctx->$id:peakname) {
    ctx->$id:peakname = ctx->$id:usagename;
    if (ctx->detail_memory) {
      fprintf(ctx->log, " (new peak).\n");
    }
  } else if (ctx->detail_memory) {
    fprintf(ctx->log, ".\n");
  }

  $items:alloc

  if (ctx->error == NULL) {
    block->references = (int*) malloc(sizeof(int));
    *(block->references) = 1;
    block->size = size;
    block->desc = desc;
    ctx->$id:usagename += size;
    return FUTHARK_SUCCESS;
  } else {
    // We are naively assuming that any memory allocation error is due to OOM.
    // We preserve the original error so that a savvy user can perhaps find
    // glory despite our naiveté.

    char *old_error = ctx->error;
    ctx->error = msgprintf("Failed to allocate memory in %s.\nAttempted allocation: %12lld bytes\nCurrently allocated:  %12lld bytes\n%s",
                           $string:spacedesc,
                           (long long) size,
                           (long long) ctx->$id:usagename,
                           old_error);
    free(old_error);
    return FUTHARK_OUT_OF_MEMORY;
  }
  }|]

  -- Memory setting - unreference the destination and increase the
  -- count of the source by one.
  let setdef =
        [C.cedecl|int $id:(fatMemSet space) ($ty:ctx_ty *ctx, $ty:mty *lhs, $ty:mty *rhs, const char *lhs_desc) {
  int ret = $id:(fatMemUnRef space)(ctx, lhs, lhs_desc);
  if (rhs->references != NULL) {
    (*(rhs->references))++;
  }
  *lhs = *rhs;
  return ret;
}
|]

  onClear [C.citem|ctx->$id:peakname = 0;|]

  let peakmsg = "Peak memory usage for " ++ spacedesc ++ ": %lld bytes.\n"
  pure
    ( structdef,
      [unrefdef, allocdef, setdef],
      -- Do not report memory usage for DefaultSpace (CPU memory),
      -- because it would not be accurate anyway.  This whole
      -- tracking probably needs to be rethought.
      if space == DefaultSpace
        then [C.citem|{}|]
        else [C.citem|str_builder(&builder, $string:peakmsg, (long long) ctx->$id:peakname);|]
    )
  where
    mty = fatMemType space
    (peakname, usagename, sname, spacedesc) = case space of
      Space sid ->
        ( C.toIdent ("peak_mem_usage_" ++ sid) noLoc,
          C.toIdent ("cur_mem_usage_" ++ sid) noLoc,
          C.toIdent ("memblock_" ++ sid) noLoc,
          "space '" ++ sid ++ "'"
        )
      _ ->
        ( "peak_mem_usage_default",
          "cur_mem_usage_default",
          "memblock",
          "default space"
        )

declMem :: VName -> Space -> CompilerM op s ()
declMem name space = do
  cached <- isJust <$> cacheMem name
  fat <- fatMemory space
  unless cached $
    if fat
      then modify $ \s -> s {compDeclaredMem = (name, space) : compDeclaredMem s}
      else do
        ty <- memToCType name space
        decl [C.cdecl|$ty:ty $id:name;|]

resetMem :: C.ToExp a => a -> Space -> CompilerM op s ()
resetMem mem space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem mem
  if cached
    then stm [C.cstm|$exp:mem = NULL;|]
    else
      when refcount $
        stm [C.cstm|$exp:mem.references = NULL;|]

setMem :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> CompilerM op s ()
setMem dest src space = do
  refcount <- fatMemory space
  let src_s = pretty $ C.toExp src noLoc
  if refcount
    then
      stm
        [C.cstm|if ($id:(fatMemSet space)(ctx, &$exp:dest, &$exp:src,
                                               $string:src_s) != 0) {
                       return 1;
                     }|]
    else case space of
      ScalarSpace ds _ -> do
        i' <- newVName "i"
        let i = C.toIdent i'
            it = primTypeToCType $ IntType Int32
            ds' = map (`C.toExp` noLoc) ds
            bound = cproduct ds'
        stm
          [C.cstm|for ($ty:it $id:i = 0; $id:i < $exp:bound; $id:i++) {
                            $exp:dest[$id:i] = $exp:src[$id:i];
                  }|]
      _ -> stm [C.cstm|$exp:dest = $exp:src;|]

unRefMem :: C.ToExp a => a -> Space -> CompilerM op s ()
unRefMem mem space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem mem
  let mem_s = pretty $ C.toExp mem noLoc
  when (refcount && not cached) $
    stm
      [C.cstm|if ($id:(fatMemUnRef space)(ctx, &$exp:mem, $string:mem_s) != 0) {
                  return 1;
                }|]

allocMem ::
  (C.ToExp a, C.ToExp b) =>
  a ->
  b ->
  Space ->
  C.Stm ->
  CompilerM op s ()
allocMem mem size space on_failure = do
  refcount <- fatMemory space
  let mem_s = pretty $ C.toExp mem noLoc
  if refcount
    then
      stm
        [C.cstm|if ($id:(fatMemAlloc space)(ctx, &$exp:mem, $exp:size,
                                                 $string:mem_s)) {
                       $stm:on_failure
                     }|]
    else do
      freeRawMem mem space mem_s
      allocRawMem mem size space [C.cexp|desc|]

copyMemoryDefaultSpace ::
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes =
  stm
    [C.cstm|if ($exp:nbytes > 0) {
              memmove($exp:destmem + $exp:destidx,
                      $exp:srcmem + $exp:srcidx,
                      $exp:nbytes);
            }|]

--- Entry points.

criticalSection :: Operations op s -> [C.BlockItem] -> [C.BlockItem]
criticalSection ops x =
  [C.citems|lock_lock(&ctx->lock);
            $items:(fst (opsCritical ops))
            $items:x
            $items:(snd (opsCritical ops))
            lock_unlock(&ctx->lock);
           |]

arrayLibraryFunctions ::
  Publicness ->
  Space ->
  PrimType ->
  Signedness ->
  Int ->
  CompilerM op s Manifest.ArrayOps
arrayLibraryFunctions pub space pt signed rank = do
  let pt' = primAPIType signed pt
      name = arrayName pt signed rank
      arr_name = "futhark_" ++ name
      array_type = [C.cty|struct $id:arr_name|]

  new_array <- publicName $ "new_" ++ name
  new_raw_array <- publicName $ "new_raw_" ++ name
  free_array <- publicName $ "free_" ++ name
  values_array <- publicName $ "values_" ++ name
  values_raw_array <- publicName $ "values_raw_" ++ name
  shape_array <- publicName $ "shape_" ++ name

  let shape_names = ["dim" ++ show i | i <- [0 .. rank - 1]]
      shape_params = [[C.cparam|typename int64_t $id:k|] | k <- shape_names]
      arr_size = cproduct [[C.cexp|$id:k|] | k <- shape_names]
      arr_size_array = cproduct [[C.cexp|arr->shape[$int:i]|] | i <- [0 .. rank - 1]]
  copy <- asks envCopy

  memty <- rawMemCType space

  let prepare_new = do
        resetMem [C.cexp|arr->mem|] space
        allocMem
          [C.cexp|arr->mem|]
          [C.cexp|$exp:arr_size * $int:(primByteSize pt::Int)|]
          space
          [C.cstm|return NULL;|]
        forM_ [0 .. rank - 1] $ \i ->
          let dim_s = "dim" ++ show i
           in stm [C.cstm|arr->shape[$int:i] = $id:dim_s;|]

  new_body <- collect $ do
    prepare_new
    copy
      CopyNoBarrier
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|data|]
      [C.cexp|0|]
      DefaultSpace
      [C.cexp|((size_t)$exp:arr_size) * $int:(primByteSize pt::Int)|]

  new_raw_body <- collect $ do
    prepare_new
    copy
      CopyNoBarrier
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|data|]
      [C.cexp|offset|]
      space
      [C.cexp|((size_t)$exp:arr_size) * $int:(primByteSize pt::Int)|]

  free_body <- collect $ unRefMem [C.cexp|arr->mem|] space

  values_body <-
    collect $
      copy
        CopyNoBarrier
        [C.cexp|data|]
        [C.cexp|0|]
        DefaultSpace
        [C.cexp|arr->mem.mem|]
        [C.cexp|0|]
        space
        [C.cexp|((size_t)$exp:arr_size_array) * $int:(primByteSize pt::Int)|]

  ctx_ty <- contextType
  ops <- asks envOperations

  let proto = case pub of
        Public -> headerDecl (ArrayDecl name)
        Private -> libDecl

  proto
    [C.cedecl|struct $id:arr_name;|]
  proto
    [C.cedecl|$ty:array_type* $id:new_array($ty:ctx_ty *ctx, const $ty:pt' *data, $params:shape_params);|]
  proto
    [C.cedecl|$ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, const $ty:memty data, typename int64_t offset, $params:shape_params);|]
  proto
    [C.cedecl|int $id:free_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  proto
    [C.cedecl|int $id:values_array($ty:ctx_ty *ctx, $ty:array_type *arr, $ty:pt' *data);|]
  proto
    [C.cedecl|$ty:memty $id:values_raw_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  proto
    [C.cedecl|const typename int64_t* $id:shape_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]

  mapM_
    libDecl
    [C.cunit|
          $ty:array_type* $id:new_array($ty:ctx_ty *ctx, const $ty:pt' *data, $params:shape_params) {
            $ty:array_type* bad = NULL;
            $ty:array_type *arr = ($ty:array_type*) malloc(sizeof($ty:array_type));
            if (arr == NULL) {
              return bad;
            }
            $items:(criticalSection ops new_body)
            return arr;
          }

          $ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, const $ty:memty data, typename int64_t offset,
                                            $params:shape_params) {
            $ty:array_type* bad = NULL;
            $ty:array_type *arr = ($ty:array_type*) malloc(sizeof($ty:array_type));
            if (arr == NULL) {
              return bad;
            }
            $items:(criticalSection ops new_raw_body)
            return arr;
          }

          int $id:free_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            $items:(criticalSection ops free_body)
            free(arr);
            return 0;
          }

          int $id:values_array($ty:ctx_ty *ctx, $ty:array_type *arr, $ty:pt' *data) {
            $items:(criticalSection ops values_body)
            return 0;
          }

          $ty:memty $id:values_raw_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            (void)ctx;
            return arr->mem.mem;
          }

          const typename int64_t* $id:shape_array($ty:ctx_ty *ctx, $ty:array_type *arr) {
            (void)ctx;
            return arr->shape;
          }
          |]

  pure $
    Manifest.ArrayOps
      { Manifest.arrayFree = T.pack free_array,
        Manifest.arrayShape = T.pack shape_array,
        Manifest.arrayValues = T.pack values_array,
        Manifest.arrayNew = T.pack new_array
      }

lookupOpaqueType :: String -> OpaqueTypes -> OpaqueType
lookupOpaqueType v (OpaqueTypes types) =
  case lookup v types of
    Just t -> t
    Nothing -> error $ "Unknown opaque type: " ++ show v

opaquePayload :: OpaqueTypes -> OpaqueType -> [ValueType]
opaquePayload _ (OpaqueType ts) = ts
opaquePayload types (OpaqueRecord fs) = concatMap f fs
  where
    f (_, TypeOpaque s) = opaquePayload types $ lookupOpaqueType s types
    f (_, TypeTransparent v) = [v]

opaqueToCType :: String -> CompilerM op s C.Type
opaqueToCType desc = do
  name <- publicName $ opaqueName desc
  pure [C.cty|struct $id:name|]

valueTypeToCType :: Publicness -> ValueType -> CompilerM op s C.Type
valueTypeToCType _ (ValueType signed (Rank 0) pt) =
  pure $ primAPIType signed pt
valueTypeToCType pub (ValueType signed (Rank rank) pt) = do
  name <- publicName $ arrayName pt signed rank
  let add = M.insertWith max (signed, pt, rank) pub
  modify $ \s -> s {compArrayTypes = add $ compArrayTypes s}
  pure [C.cty|struct $id:name|]

entryPointTypeToCType :: Publicness -> EntryPointType -> CompilerM op s C.Type
entryPointTypeToCType _ (TypeOpaque desc) = opaqueToCType desc
entryPointTypeToCType pub (TypeTransparent vt) = valueTypeToCType pub vt

entryTypeName :: EntryPointType -> Manifest.TypeName
entryTypeName (TypeOpaque desc) = T.pack desc
entryTypeName (TypeTransparent vt) = prettyText vt

-- | Figure out which of the members of an opaque type corresponds to
-- which fields.
recordFieldPayloads :: OpaqueTypes -> [EntryPointType] -> [a] -> [[a]]
recordFieldPayloads types = chunks . map typeLength
  where
    typeLength (TypeTransparent _) = 1
    typeLength (TypeOpaque desc) =
      length $ opaquePayload types $ lookupOpaqueType desc types

opaqueProjectFunctions ::
  OpaqueTypes ->
  String ->
  [(Name, EntryPointType)] ->
  [ValueType] ->
  CompilerM op s [Manifest.RecordField]
opaqueProjectFunctions types desc fs vds = do
  opaque_type <- opaqueToCType desc
  ctx_ty <- contextType
  ops <- asks envOperations
  let mkProject (TypeTransparent (ValueType sign (Rank 0) pt)) [(i, _)] = do
        pure
          ( primAPIType sign pt,
            [C.citems|v = obj->$id:(tupleField i);|]
          )
      mkProject (TypeTransparent vt) [(i, _)] = do
        ct <- valueTypeToCType Public vt
        pure
          ( [C.cty|$ty:ct *|],
            criticalSection
              ops
              [C.citems|v = malloc(sizeof($ty:ct));
                        memcpy(v, obj->$id:(tupleField i), sizeof($ty:ct));
                        (void)(*(v->mem.references))++;|]
          )
      mkProject (TypeTransparent _) rep =
        error $ "mkProject: invalid representation of transparent type: " ++ show rep
      mkProject (TypeOpaque f_desc) components = do
        ct <- opaqueToCType f_desc
        let setField j (i, ValueType _ (Rank r) _) =
              if r == 0
                then [C.citems|v->$id:(tupleField j) = obj->$id:(tupleField i);|]
                else
                  [C.citems|v->$id:(tupleField j) = malloc(sizeof(*v->$id:(tupleField j)));
                            *v->$id:(tupleField j) = *obj->$id:(tupleField i);
                            (void)(*(v->$id:(tupleField j)->mem.references))++;|]
        pure
          ( [C.cty|$ty:ct *|],
            criticalSection
              ops
              [C.citems|v = malloc(sizeof($ty:ct));
                        $items:(concat (zipWith setField [0..] components))|]
          )
  let onField ((f, et), elems) = do
        project <- publicName $ "project_" ++ opaqueName desc ++ "_" ++ nameToString f
        (et_ty, project_items) <- mkProject et elems
        headerDecl
          (OpaqueDecl desc)
          [C.cedecl|int $id:project($ty:ctx_ty *ctx, $ty:et_ty *out, const $ty:opaque_type *obj);|]
        libDecl
          [C.cedecl|int $id:project($ty:ctx_ty *ctx, $ty:et_ty *out, const $ty:opaque_type *obj) {
                      (void)ctx;
                      $ty:et_ty v;
                      $items:project_items
                      *out = v;
                      return 0;
                    }|]
        pure $ Manifest.RecordField (nameToText f) (entryTypeName et) (T.pack project)

  mapM onField . zip fs . recordFieldPayloads types (map snd fs) $
    zip [0 ..] vds

opaqueNewFunctions ::
  OpaqueTypes ->
  String ->
  [(Name, EntryPointType)] ->
  [ValueType] ->
  CompilerM op s Manifest.CFuncName
opaqueNewFunctions types desc fs vds = do
  opaque_type <- opaqueToCType desc
  ctx_ty <- contextType
  ops <- asks envOperations
  new <- publicName $ "new_" ++ opaqueName desc

  (params, new_stms) <-
    fmap (unzip . snd)
      . mapAccumLM onField 0
      . zip fs
      . recordFieldPayloads types (map snd fs)
      $ vds

  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:new($ty:ctx_ty *ctx, $ty:opaque_type** out, $params:params);|]
  libDecl
    [C.cedecl|int $id:new($ty:ctx_ty *ctx, $ty:opaque_type** out, $params:params) {
                $ty:opaque_type* v = malloc(sizeof($ty:opaque_type));
                $items:(criticalSection ops new_stms)
                *out = v;
                return 0;
              }|]
  pure $ T.pack new
  where
    onField offset ((f, et), f_vts) = do
      let param_name =
            if all isDigit (nameToString f)
              then C.toIdent ("v" <> f) mempty
              else C.toIdent f mempty
      case et of
        TypeTransparent (ValueType sign (Rank 0) pt) -> do
          let ct = primAPIType sign pt
          pure
            ( offset + 1,
              ( [C.cparam|const $ty:ct $id:param_name|],
                [C.citem|v->$id:(tupleField offset) = $id:param_name;|]
              )
            )
        TypeTransparent vt -> do
          ct <- valueTypeToCType Public vt
          pure
            ( offset + 1,
              ( [C.cparam|const $ty:ct* $id:param_name|],
                [C.citem|{v->$id:(tupleField offset) = malloc(sizeof($ty:ct));
                          *v->$id:(tupleField offset) = *$id:param_name;
                          (void)(*(v->$id:(tupleField offset)->mem.references))++;}|]
              )
            )
        TypeOpaque f_desc -> do
          ct <- opaqueToCType f_desc
          let param_fields = do
                i <- [0 ..]
                pure [C.cexp|$id:param_name->$id:(tupleField i)|]
          pure
            ( offset + length f_vts,
              ( [C.cparam|const $ty:ct* $id:param_name|],
                [C.citem|{$stms:(zipWith3 setFieldField [offset ..] param_fields f_vts)}|]
              )
            )

    setFieldField i e (ValueType _ (Rank r) _)
      | r == 0 =
          [C.cstm|v->$id:(tupleField i) = $exp:e;|]
      | otherwise =
          [C.cstm|{v->$id:(tupleField i) = malloc(sizeof(*$exp:e));
                   *v->$id:(tupleField i) = *$exp:e;
                   (void)(*(v->$id:(tupleField i)->mem.references))++;}|]

processOpaqueRecord ::
  OpaqueTypes ->
  String ->
  OpaqueType ->
  [ValueType] ->
  CompilerM op s (Maybe Manifest.RecordOps)
processOpaqueRecord _ _ (OpaqueType _) _ = pure Nothing
processOpaqueRecord types desc (OpaqueRecord fs) vds =
  Just
    <$> ( Manifest.RecordOps
            <$> opaqueProjectFunctions types desc fs vds
            <*> opaqueNewFunctions types desc fs vds
        )

opaqueLibraryFunctions ::
  OpaqueTypes ->
  String ->
  OpaqueType ->
  CompilerM op s (Manifest.OpaqueOps, Maybe Manifest.RecordOps)
opaqueLibraryFunctions types desc ot = do
  name <- publicName $ opaqueName desc
  free_opaque <- publicName $ "free_" ++ opaqueName desc
  store_opaque <- publicName $ "store_" ++ opaqueName desc
  restore_opaque <- publicName $ "restore_" ++ opaqueName desc

  let opaque_type = [C.cty|struct $id:name|]

      freeComponent i (ValueType signed (Rank rank) pt) = unless (rank == 0) $ do
        let field = tupleField i
        free_array <- publicName $ "free_" ++ arrayName pt signed rank
        -- Protect against NULL here, because we also want to use this
        -- to free partially loaded opaques.
        stm
          [C.cstm|if (obj->$id:field != NULL && (tmp = $id:free_array(ctx, obj->$id:field)) != 0) {
                ret = tmp;
             }|]

      storeComponent i (ValueType sign (Rank 0) pt) =
        let field = tupleField i
         in ( storageSize pt 0 [C.cexp|NULL|],
              storeValueHeader sign pt 0 [C.cexp|NULL|] [C.cexp|out|]
                ++ [C.cstms|memcpy(out, &obj->$id:field, sizeof(obj->$id:field));
                            out += sizeof(obj->$id:field);|]
            )
      storeComponent i (ValueType sign (Rank rank) pt) =
        let arr_name = arrayName pt sign rank
            field = tupleField i
            shape_array = "futhark_shape_" ++ arr_name
            values_array = "futhark_values_" ++ arr_name
            shape' = [C.cexp|$id:shape_array(ctx, obj->$id:field)|]
            num_elems = cproduct [[C.cexp|$exp:shape'[$int:j]|] | j <- [0 .. rank - 1]]
         in ( storageSize pt rank shape',
              storeValueHeader sign pt rank shape' [C.cexp|out|]
                ++ [C.cstms|ret |= $id:values_array(ctx, obj->$id:field, (void*)out);
                            out += $exp:num_elems * $int:(primByteSize pt::Int);|]
            )

  ctx_ty <- contextType

  let vds = opaquePayload types ot
  free_body <- collect $ zipWithM_ freeComponent [0 ..] vds

  store_body <- collect $ do
    let (sizes, stores) = unzip $ zipWith storeComponent [0 ..] vds
        size_vars = map (("size_" ++) . show) [0 .. length sizes - 1]
        size_sum = csum [[C.cexp|$id:size|] | size <- size_vars]
    forM_ (zip size_vars sizes) $ \(v, e) ->
      item [C.citem|typename int64_t $id:v = $exp:e;|]
    stm [C.cstm|*n = $exp:size_sum;|]
    stm [C.cstm|if (p != NULL && *p == NULL) { *p = malloc(*n); }|]
    stm [C.cstm|if (p != NULL) { unsigned char *out = *p; $stms:(concat stores) }|]

  let restoreComponent i (ValueType sign (Rank 0) pt) = do
        let field = tupleField i
            dataptr = "data_" ++ show i
        stms $ loadValueHeader sign pt 0 [C.cexp|NULL|] [C.cexp|src|]
        item [C.citem|const void* $id:dataptr = src;|]
        stm [C.cstm|src += sizeof(obj->$id:field);|]
        pure [C.cstms|memcpy(&obj->$id:field, $id:dataptr, sizeof(obj->$id:field));|]
      restoreComponent i (ValueType sign (Rank rank) pt) = do
        let field = tupleField i
            arr_name = arrayName pt sign rank
            new_array = "futhark_new_" ++ arr_name
            dataptr = "data_" ++ show i
            shapearr = "shape_" ++ show i
            dims = [[C.cexp|$id:shapearr[$int:j]|] | j <- [0 .. rank - 1]]
            num_elems = cproduct dims
        item [C.citem|typename int64_t $id:shapearr[$int:rank] = {0};|]
        stms $ loadValueHeader sign pt rank [C.cexp|$id:shapearr|] [C.cexp|src|]
        item [C.citem|const void* $id:dataptr = src;|]
        stm [C.cstm|obj->$id:field = NULL;|]
        stm [C.cstm|src += $exp:num_elems * $int:(primByteSize pt::Int);|]
        pure
          [C.cstms|
             obj->$id:field = $id:new_array(ctx, $id:dataptr, $args:dims);
             if (obj->$id:field == NULL) { err = 1; }|]

  load_body <- collect $ do
    loads <- concat <$> zipWithM restoreComponent [0 ..] (opaquePayload types ot)
    stm
      [C.cstm|if (err == 0) {
                $stms:loads
              }|]

  headerDecl
    (OpaqueTypeDecl desc)
    [C.cedecl|struct $id:name;|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj);|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:store_opaque($ty:ctx_ty *ctx, const $ty:opaque_type *obj, void **p, size_t *n);|]
  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|$ty:opaque_type* $id:restore_opaque($ty:ctx_ty *ctx, const void *p);|]

  record <- processOpaqueRecord types desc ot vds

  -- We do not need to enclose most bodies in a critical section,
  -- because when we operate on the components of the opaque, we are
  -- calling public API functions that do their own locking.  The
  -- exception is projection, where we fiddle with reference counts.
  mapM_
    libDecl
    [C.cunit|
          int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj) {
            (void)ctx;
            int ret = 0, tmp;
            $items:free_body
            free(obj);
            return ret;
          }

          int $id:store_opaque($ty:ctx_ty *ctx,
                               const $ty:opaque_type *obj, void **p, size_t *n) {
            (void)ctx;
            int ret = 0;
            $items:store_body
            return ret;
          }

          $ty:opaque_type* $id:restore_opaque($ty:ctx_ty *ctx,
                                              const void *p) {
            int err = 0;
            const unsigned char *src = p;
            $ty:opaque_type* obj = malloc(sizeof($ty:opaque_type));
            $items:load_body
            if (err != 0) {
              int ret = 0, tmp;
              $items:free_body
              free(obj);
              obj = NULL;
            }
            return obj;
          }
    |]

  pure
    ( Manifest.OpaqueOps
        { Manifest.opaqueFree = T.pack free_opaque,
          Manifest.opaqueStore = T.pack store_opaque,
          Manifest.opaqueRestore = T.pack restore_opaque
        },
      record
    )

valueDescToType :: ValueDesc -> ValueType
valueDescToType (ScalarValue pt signed _) =
  ValueType signed (Rank 0) pt
valueDescToType (ArrayValue _ _ pt signed shape) =
  ValueType signed (Rank (length shape)) pt

generateArray ::
  Space ->
  ((Signedness, PrimType, Int), Publicness) ->
  CompilerM op s (Maybe (T.Text, Manifest.Type))
generateArray space ((signed, pt, rank), pub) = do
  name <- publicName $ arrayName pt signed rank
  let memty = fatMemType space
  libDecl [C.cedecl|struct $id:name { $ty:memty mem; typename int64_t shape[$int:rank]; };|]
  ops <- arrayLibraryFunctions pub space pt signed rank
  let pt_name = T.pack $ prettySigned (signed == Unsigned) pt
      pretty_name = mconcat (replicate rank "[]") <> pt_name
      arr_type = [C.cty|struct $id:name*|]
  case pub of
    Public ->
      pure $
        Just
          ( pretty_name,
            Manifest.TypeArray (prettyText arr_type) pt_name rank ops
          )
    Private ->
      pure Nothing

generateOpaque ::
  OpaqueTypes ->
  (String, OpaqueType) ->
  CompilerM op s (T.Text, Manifest.Type)
generateOpaque types (desc, ot) = do
  name <- publicName $ opaqueName desc
  members <- zipWithM field (opaquePayload types ot) [(0 :: Int) ..]
  libDecl [C.cedecl|struct $id:name { $sdecls:members };|]
  (ops, record) <- opaqueLibraryFunctions types desc ot
  let opaque_type = [C.cty|struct $id:name*|]
  pure (T.pack desc, Manifest.TypeOpaque (prettyText opaque_type) ops record)
  where
    field vt@(ValueType _ (Rank r) _) i = do
      ct <- valueTypeToCType Private vt
      pure $
        if r == 0
          then [C.csdecl|$ty:ct $id:(tupleField i);|]
          else [C.csdecl|$ty:ct *$id:(tupleField i);|]

generateAPITypes :: Space -> OpaqueTypes -> CompilerM op s (M.Map T.Text Manifest.Type)
generateAPITypes arr_space types@(OpaqueTypes opaques) = do
  mapM_ (findNecessaryArrays . snd) opaques
  array_ts <- mapM (generateArray arr_space) . M.toList =<< gets compArrayTypes
  opaque_ts <- mapM (generateOpaque types) opaques
  pure $ M.fromList $ catMaybes array_ts <> opaque_ts
  where
    -- Ensure that array types will be generated before the opaque
    -- records that allow projection of them.  This is because the
    -- projection functions somewhat uglily directly poke around in
    -- the innards to increment reference counts.
    findNecessaryArrays (OpaqueType _) =
      pure ()
    findNecessaryArrays (OpaqueRecord fs) =
      mapM_ (entryPointTypeToCType Public . snd) fs

allTrue :: [C.Exp] -> C.Exp
allTrue [] = [C.cexp|true|]
allTrue [x] = x
allTrue (x : xs) = [C.cexp|$exp:x && $exp:(allTrue xs)|]

prepareEntryInputs ::
  [ExternalValue] ->
  CompilerM op s ([(C.Param, Maybe C.Exp)], [C.BlockItem])
prepareEntryInputs args = collect' $ zipWithM prepare [(0 :: Int) ..] args
  where
    arg_names = namesFromList $ concatMap evNames args
    evNames (OpaqueValue _ vds) = map vdName vds
    evNames (TransparentValue vd) = [vdName vd]
    vdName (ArrayValue v _ _ _ _) = v
    vdName (ScalarValue _ _ v) = v

    prepare pno (TransparentValue vd) = do
      let pname = "in" ++ show pno
      (ty, check) <- prepareValue Public [C.cexp|$id:pname|] vd
      pure
        ( [C.cparam|const $ty:ty $id:pname|],
          if null check then Nothing else Just $ allTrue check
        )
    prepare pno (OpaqueValue desc vds) = do
      ty <- opaqueToCType desc
      let pname = "in" ++ show pno
          field i ScalarValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
          field i ArrayValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
      checks <- map snd <$> zipWithM (prepareValue Private) (zipWith field [0 ..] vds) vds
      pure
        ( [C.cparam|const $ty:ty *$id:pname|],
          if all null checks
            then Nothing
            else Just $ allTrue $ concat checks
        )

    prepareValue _ src (ScalarValue pt signed name) = do
      let pt' = primAPIType signed pt
          src' = fromStorage pt $ C.toExp src mempty
      stm [C.cstm|$id:name = $exp:src';|]
      pure (pt', [])
    prepareValue pub src vd@(ArrayValue mem _ _ _ shape) = do
      ty <- valueTypeToCType pub $ valueDescToType vd

      stm [C.cstm|$exp:mem = $exp:src->mem;|]

      let rank = length shape
          maybeCopyDim (Var d) i
            | d `notNameIn` arg_names =
                ( Just [C.cstm|$id:d = $exp:src->shape[$int:i];|],
                  [C.cexp|$id:d == $exp:src->shape[$int:i]|]
                )
          maybeCopyDim x i =
            ( Nothing,
              [C.cexp|$exp:x == $exp:src->shape[$int:i]|]
            )

      let (sets, checks) =
            unzip $ zipWith maybeCopyDim shape [0 .. rank - 1]
      stms $ catMaybes sets

      pure ([C.cty|$ty:ty*|], checks)

prepareEntryOutputs :: [ExternalValue] -> CompilerM op s ([C.Param], [C.BlockItem])
prepareEntryOutputs = collect' . zipWithM prepare [(0 :: Int) ..]
  where
    prepare pno (TransparentValue vd) = do
      let pname = "out" ++ show pno
      ty <- valueTypeToCType Public $ valueDescToType vd

      case vd of
        ArrayValue {} -> do
          stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]
          prepareValue [C.cexp|*$id:pname|] vd
          pure [C.cparam|$ty:ty **$id:pname|]
        ScalarValue {} -> do
          prepareValue [C.cexp|*$id:pname|] vd
          pure [C.cparam|$ty:ty *$id:pname|]
    prepare pno (OpaqueValue desc vds) = do
      let pname = "out" ++ show pno
      ty <- opaqueToCType desc
      vd_ts <- mapM (valueTypeToCType Private . valueDescToType) vds

      stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]

      forM_ (zip3 [0 ..] vd_ts vds) $ \(i, ct, vd) -> do
        let field = [C.cexp|((*$id:pname)->$id:(tupleField i))|]
        case vd of
          ScalarValue {} -> pure ()
          ArrayValue {} -> do
            stm [C.cstm|assert(($exp:field = ($ty:ct*) malloc(sizeof($ty:ct))) != NULL);|]
        prepareValue field vd

      pure [C.cparam|$ty:ty **$id:pname|]

    prepareValue dest (ScalarValue t _ name) =
      let name' = toStorage t $ C.toExp name mempty
       in stm [C.cstm|$exp:dest = $exp:name';|]
    prepareValue dest (ArrayValue mem _ _ _ shape) = do
      stm [C.cstm|$exp:dest->mem = $id:mem;|]

      let rank = length shape
          maybeCopyDim (Constant x) i =
            [C.cstm|$exp:dest->shape[$int:i] = $exp:x;|]
          maybeCopyDim (Var d) i =
            [C.cstm|$exp:dest->shape[$int:i] = $id:d;|]
      stms $ zipWith maybeCopyDim shape [0 .. rank - 1]

isValidCName :: Name -> Bool
isValidCName = check . nameToString
  where
    check [] = True -- academic
    check (c : cs) = isAlpha c && all constituent cs
    constituent c = isAlphaNum c || c == '_'

entryName :: Name -> String
entryName v
  | isValidCName v = "entry_" <> nameToString v
  | otherwise = "entry_" <> zEncodeString (nameToString v)

onEntryPoint ::
  [C.BlockItem] ->
  Name ->
  Function op ->
  CompilerM op s (Maybe (C.Definition, (T.Text, Manifest.EntryPoint)))
onEntryPoint _ _ (Function Nothing _ _ _) = pure Nothing
onEntryPoint get_consts fname (Function (Just (EntryPoint ename results args)) outputs inputs _) = inNewFunction $ do
  let out_args = map (\p -> [C.cexp|&$id:(paramName p)|]) outputs
      in_args = map (\p -> [C.cexp|$id:(paramName p)|]) inputs

  inputdecls <- collect $ mapM_ stubParam inputs
  outputdecls <- collect $ mapM_ stubParam outputs
  decl_mem <- declAllocatedMem

  entry_point_function_name <- publicName $ entryName ename

  (inputs', unpack_entry_inputs) <- prepareEntryInputs $ map snd args
  let (entry_point_input_params, entry_point_input_checks) = unzip inputs'

  (entry_point_output_params, pack_entry_outputs) <-
    prepareEntryOutputs $ map snd results

  ctx_ty <- contextType

  headerDecl
    EntryDecl
    [C.cedecl|int $id:entry_point_function_name
                                     ($ty:ctx_ty *ctx,
                                      $params:entry_point_output_params,
                                      $params:entry_point_input_params);|]

  let checks = catMaybes entry_point_input_checks
      check_input =
        if null checks
          then []
          else
            [C.citems|
         if (!($exp:(allTrue (catMaybes entry_point_input_checks)))) {
           ret = 1;
           if (!ctx->error) {
             ctx->error = msgprintf("Error: entry point arguments have invalid sizes.\n");
           }
         }|]

      critical =
        [C.citems|
         $items:decl_mem
         $items:unpack_entry_inputs
         $items:check_input
         if (ret == 0) {
           ret = $id:(funName fname)(ctx, $args:out_args, $args:in_args);
           if (ret == 0) {
             $items:get_consts

             $items:pack_entry_outputs
           }
         }
        |]

  ops <- asks envOperations

  let cdef =
        [C.cedecl|
       int $id:entry_point_function_name
           ($ty:ctx_ty *ctx,
            $params:entry_point_output_params,
            $params:entry_point_input_params) {
         $items:inputdecls
         $items:outputdecls

         int ret = 0;

         $items:(criticalSection ops critical)

         return ret;
       }|]

      manifest =
        Manifest.EntryPoint
          { Manifest.entryPointCFun = T.pack entry_point_function_name,
            -- Note that our convention about what is "input/output"
            -- and what is "results/args" is different between the
            -- manifest and ImpCode.
            Manifest.entryPointOutputs = map outputManifest results,
            Manifest.entryPointInputs = map inputManifest args
          }

  pure $ Just (cdef, (nameToText ename, manifest))
  where
    stubParam (MemParam name space) =
      declMem name space
    stubParam (ScalarParam name ty) = do
      let ty' = primTypeToCType ty
      decl [C.cdecl|$ty:ty' $id:name;|]

    vdType (TransparentValue (ScalarValue pt signed _)) =
      T.pack $ prettySigned (signed == Unsigned) pt
    vdType (TransparentValue (ArrayValue _ _ pt signed shape)) =
      T.pack $
        mconcat (replicate (length shape) "[]")
          <> prettySigned (signed == Unsigned) pt
    vdType (OpaqueValue name _) =
      T.pack name

    outputManifest (u, vd) =
      Manifest.Output
        { Manifest.outputType = vdType vd,
          Manifest.outputUnique = u == Unique
        }
    inputManifest ((v, u), vd) =
      Manifest.Input
        { Manifest.inputName = nameToText v,
          Manifest.inputType = vdType vd,
          Manifest.inputUnique = u == Unique
        }

-- | The result of compilation to C is multiple parts, which can be
-- put together in various ways.  The obvious way is to concatenate
-- all of them, which yields a CLI program.  Another is to compile the
-- library part by itself, and use the header file to call into it.
data CParts = CParts
  { cHeader :: T.Text,
    -- | Utility definitions that must be visible
    -- to both CLI and library parts.
    cUtils :: T.Text,
    cCLI :: T.Text,
    cServer :: T.Text,
    cLib :: T.Text,
    -- | The manifest, in JSON format.
    cJsonManifest :: T.Text
  }

gnuSource :: T.Text
gnuSource =
  [untrimming|
// We need to define _GNU_SOURCE before
// _any_ headers files are imported to get
// the usage statistics of a thread (i.e. have RUSAGE_THREAD) on GNU/Linux
// https://manpages.courier-mta.org/htmlman2/getrusage.2.html
#ifndef _GNU_SOURCE // Avoid possible double-definition warning.
#define _GNU_SOURCE
#endif
|]

-- We may generate variables that are never used (e.g. for
-- certificates) or functions that are never called (e.g. unused
-- intrinsics), and generated code may have other cosmetic issues that
-- compilers warn about.  We disable these warnings to not clutter the
-- compilation logs.
disableWarnings :: T.Text
disableWarnings =
  [untrimming|
#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wunused-label"
#elif __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
|]

-- | Produce header, implementation, and manifest files.
asLibrary :: CParts -> (T.Text, T.Text, T.Text)
asLibrary parts =
  ( "#pragma once\n\n" <> cHeader parts,
    gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cLib parts,
    cJsonManifest parts
  )

-- | As executable with command-line interface.
asExecutable :: CParts -> T.Text
asExecutable parts =
  gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cCLI parts <> cLib parts

-- | As server executable.
asServer :: CParts -> T.Text
asServer parts =
  gnuSource <> disableWarnings <> cHeader parts <> cUtils parts <> cServer parts <> cLib parts

compileProg' ::
  MonadFreshNames m =>
  T.Text ->
  T.Text ->
  Operations op s ->
  s ->
  CompilerM op s () ->
  T.Text ->
  (Space, [Space]) ->
  [Option] ->
  Definitions op ->
  m (CParts, CompilerState s)
compileProg' backend version ops def extra header_extra (arr_space, spaces) options prog = do
  src <- getNameSource
  let ((prototypes, definitions, entry_point_decls, manifest), endstate) =
        runCompilerM ops src def compileProgAction
      initdecls = initDecls endstate
      entrydecls = entryDecls endstate
      arraydecls = arrayDecls endstate
      opaquetypedecls = opaqueTypeDecls endstate
      opaquedecls = opaqueDecls endstate
      miscdecls = miscDecls endstate

  let headerdefs =
        [untrimming|
// Headers
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <float.h>
$header_extra
#ifdef __cplusplus
extern "C" {
#endif

// Initialisation
$initdecls

// Arrays
$arraydecls

// Opaque values
$opaquetypedecls
$opaquedecls

// Entry points
$entrydecls

// Miscellaneous
$miscdecls
#define FUTHARK_BACKEND_$backend
$errorsH

#ifdef __cplusplus
}
#endif
|]

  let utildefs =
        [untrimming|
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
// If NDEBUG is set, the assert() macro will do nothing. Since Futhark
// (unfortunately) makes use of assert() for error detection (and even some
// side effects), we want to avoid that.
#undef NDEBUG
#include <assert.h>
#include <stdarg.h>
$utilH
$cacheH
$halfH
$timingH
|]

  let early_decls = T.unlines $ map prettyText $ DL.toList $ compEarlyDecls endstate
      lib_decls = T.unlines $ map prettyText $ DL.toList $ compLibDecls endstate
      clidefs = cliDefs options manifest
      serverdefs = serverDefs options manifest
      libdefs =
        [untrimming|
#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>

$header_extra

$lockH

#define FUTHARK_F64_ENABLED

$cScalarDefs

$early_decls

$prototypes

$lib_decls

$definitions

$entry_point_decls
  |]

  pure
    ( CParts
        { cHeader = headerdefs,
          cUtils = utildefs,
          cCLI = clidefs,
          cServer = serverdefs,
          cLib = libdefs,
          cJsonManifest = Manifest.manifestToJSON manifest
        },
      endstate
    )
  where
    Definitions types consts (Functions funs) = prog

    compileProgAction = do
      (memstructs, memfuns, memreport) <- unzip3 <$> mapM defineMemorySpace spaces

      get_consts <- compileConstants consts

      ctx_ty <- contextType

      (prototypes, functions) <-
        unzip <$> mapM (compileFun get_consts [[C.cparam|$ty:ctx_ty *ctx|]]) funs

      mapM_ earlyDecl memstructs
      (entry_points, entry_points_manifest) <-
        unzip . catMaybes <$> mapM (uncurry (onEntryPoint get_consts)) funs

      extra

      mapM_ earlyDecl $ concat memfuns

      type_funs <- generateAPITypes arr_space types
      generateCommonLibFuns memreport

      pure
        ( T.unlines $ map prettyText prototypes,
          T.unlines $ map (prettyText . funcToDef) functions,
          T.unlines $ map prettyText entry_points,
          Manifest.Manifest (M.fromList entry_points_manifest) type_funs backend version
        )

    funcToDef func = C.FuncDef func loc
      where
        loc = case func of
          C.OldFunc _ _ _ _ _ _ l -> l
          C.Func _ _ _ _ _ l -> l

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg ::
  MonadFreshNames m =>
  T.Text ->
  T.Text ->
  Operations op () ->
  CompilerM op () () ->
  T.Text ->
  (Space, [Space]) ->
  [Option] ->
  Definitions op ->
  m CParts
compileProg backend version ops extra header_extra (arr_space, spaces) options prog =
  fst <$> compileProg' backend version ops () extra header_extra (arr_space, spaces) options prog

generateCommonLibFuns :: [C.BlockItem] -> CompilerM op s ()
generateCommonLibFuns memreport = do
  ctx <- contextType
  cfg <- configType
  ops <- asks envOperations
  profilereport <- gets $ DL.toList . compProfileItems

  publicDef_ "context_config_set_cache_file" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:cfg* cfg, const char *f);|],
      [C.cedecl|void $id:s($ty:cfg* cfg, const char *f) {
                 cfg->cache_fname = f;
               }|]
    )

  publicDef_ "get_tuning_param_count" InitDecl $ \s ->
    ( [C.cedecl|int $id:s(void);|],
      [C.cedecl|int $id:s(void) {
                return sizeof(tuning_param_names)/sizeof(tuning_param_names[0]);
              }|]
    )

  publicDef_ "get_tuning_param_name" InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return tuning_param_names[i];
              }|]
    )

  publicDef_ "get_tuning_param_class" InitDecl $ \s ->
    ( [C.cedecl|const char* $id:s(int);|],
      [C.cedecl|const char* $id:s(int i) {
                return tuning_param_classes[i];
              }|]
    )

  sync <- publicName "context_sync"
  publicDef_ "context_report" MiscDecl $ \s ->
    ( [C.cedecl|char* $id:s($ty:ctx *ctx);|],
      [C.cedecl|char* $id:s($ty:ctx *ctx) {
                 if ($id:sync(ctx) != 0) {
                   return NULL;
                 }

                 struct str_builder builder;
                 str_builder_init(&builder);
                 $items:memreport
                 if (ctx->profiling) {
                   $items:profilereport
                 }
                 return builder.str;
               }|]
    )

  publicDef_ "context_get_error" MiscDecl $ \s ->
    ( [C.cedecl|char* $id:s($ty:ctx* ctx);|],
      [C.cedecl|char* $id:s($ty:ctx* ctx) {
                         char* error = ctx->error;
                         ctx->error = NULL;
                         return error;
                       }|]
    )

  publicDef_ "context_set_logging_file" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx, typename FILE* f);|],
      [C.cedecl|void $id:s($ty:ctx* ctx, typename FILE* f) {
                  ctx->log = f;
                }|]
    )

  publicDef_ "context_pause_profiling" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx);|],
      [C.cedecl|void $id:s($ty:ctx* ctx) {
                 ctx->profiling_paused = 1;
               }|]
    )

  publicDef_ "context_unpause_profiling" MiscDecl $ \s ->
    ( [C.cedecl|void $id:s($ty:ctx* ctx);|],
      [C.cedecl|void $id:s($ty:ctx* ctx) {
                 ctx->profiling_paused = 0;
               }|]
    )

  clears <- gets $ DL.toList . compClearItems
  publicDef_ "context_clear_caches" MiscDecl $ \s ->
    ( [C.cedecl|int $id:s($ty:ctx* ctx);|],
      [C.cedecl|int $id:s($ty:ctx* ctx) {
                         $items:(criticalSection ops clears)
                         return ctx->error != NULL;
                       }|]
    )

compileConstants :: Constants op -> CompilerM op s [C.BlockItem]
compileConstants (Constants ps init_consts) = do
  ctx_ty <- contextType
  const_fields <- mapM constParamField ps
  -- Avoid an empty struct, as that is apparently undefined behaviour.
  let const_fields'
        | null const_fields = [[C.csdecl|int dummy;|]]
        | otherwise = const_fields
  contextField "constants" [C.cty|struct { $sdecls:const_fields' }|] Nothing
  earlyDecl [C.cedecl|static int init_constants($ty:ctx_ty*);|]
  earlyDecl [C.cedecl|static int free_constants($ty:ctx_ty*);|]

  inNewFunction $ do
    -- We locally define macros for the constants, so that when we
    -- generate assignments to local variables, we actually assign into
    -- the constants struct.  This is not needed for functions, because
    -- they can only read constants, not write them.
    let (defs, undefs) = unzip $ map constMacro ps
    init_consts' <- collect $ do
      mapM_ resetMemConst ps
      compileCode init_consts
    decl_mem <- declAllocatedMem
    free_mem <- freeAllocatedMem
    libDecl
      [C.cedecl|static int init_constants($ty:ctx_ty *ctx) {
        (void)ctx;
        int err = 0;
        $items:defs
        $items:decl_mem
        $items:init_consts'
        $items:free_mem
        $items:undefs
        cleanup:
        return err;
      }|]

  inNewFunction $ do
    free_consts <- collect $ mapM_ freeConst ps
    libDecl
      [C.cedecl|static int free_constants($ty:ctx_ty *ctx) {
        (void)ctx;
        $items:free_consts
        return 0;
      }|]

  mapM getConst ps
  where
    constParamField (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      pure [C.csdecl|$ty:ctp $id:name;|]
    constParamField (MemParam name space) = do
      ty <- memToCType name space
      pure [C.csdecl|$ty:ty $id:name;|]

    constMacro p = ([C.citem|$escstm:def|], [C.citem|$escstm:undef|])
      where
        p' = pretty (C.toIdent (paramName p) mempty)
        def = "#define " ++ p' ++ " (" ++ "ctx->constants." ++ p' ++ ")"
        undef = "#undef " ++ p'

    resetMemConst ScalarParam {} = pure ()
    resetMemConst (MemParam name space) = resetMem name space

    freeConst ScalarParam {} = pure ()
    freeConst (MemParam name space) = unRefMem [C.cexp|ctx->constants.$id:name|] space

    getConst (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      pure [C.citem|$ty:ctp $id:name = ctx->constants.$id:name;|]
    getConst (MemParam name space) = do
      ty <- memToCType name space
      pure [C.citem|$ty:ty $id:name = ctx->constants.$id:name;|]

cachingMemory ::
  M.Map VName Space ->
  ([C.BlockItem] -> [C.Stm] -> CompilerM op s a) ->
  CompilerM op s a
cachingMemory lexical f = do
  -- We only consider lexical 'DefaultSpace' memory blocks to be
  -- cached.  This is not a deep technical restriction, but merely a
  -- heuristic based on GPU memory usually involving larger
  -- allocations, that do not suffer from the overhead of reference
  -- counting.
  let cached = M.keys $ M.filter (== DefaultSpace) lexical

  cached' <- forM cached $ \mem -> do
    size <- newVName $ pretty mem <> "_cached_size"
    pure (mem, size)

  let lexMem env =
        env
          { envCachedMem =
              M.fromList (map (first (`C.toExp` noLoc)) cached')
                <> envCachedMem env
          }

      declCached (mem, size) =
        [ [C.citem|typename int64_t $id:size = 0;|],
          [C.citem|$ty:defaultMemBlockType $id:mem = NULL;|]
        ]

      freeCached (mem, _) =
        [C.cstm|free($id:mem);|]

  local lexMem $ f (concatMap declCached cached') (map freeCached cached')

compileFun :: [C.BlockItem] -> [C.Param] -> (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileFun get_constants extra (fname, func@(Function _ outputs inputs body)) = inNewFunction $ do
  (outparams, out_ptrs) <- unzip <$> mapM compileOutput outputs
  inparams <- mapM compileInput inputs

  cachingMemory (lexicalMemoryUsage func) $ \decl_cached free_cached -> do
    body' <- collect $ compileFunBody out_ptrs outputs body
    decl_mem <- declAllocatedMem
    free_mem <- freeAllocatedMem

    pure
      ( [C.cedecl|static int $id:(funName fname)($params:extra, $params:outparams, $params:inparams);|],
        [C.cfun|static int $id:(funName fname)($params:extra, $params:outparams, $params:inparams) {
               $stms:ignores
               int err = 0;
               $items:decl_cached
               $items:decl_mem
               $items:get_constants
               $items:body'
              cleanup:
               {
               $stms:free_cached
               $items:free_mem
               }
               return err;
  }|]
      )
  where
    -- Ignore all the boilerplate parameters, just in case we don't
    -- actually need to use them.
    ignores = [[C.cstm|(void)$id:p;|] | C.Param (Just p) _ _ _ <- extra]

    compileInput (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      pure [C.cparam|$ty:ctp $id:name|]
    compileInput (MemParam name space) = do
      ty <- memToCType name space
      pure [C.cparam|$ty:ty $id:name|]

    compileOutput (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      p_name <- newVName $ "out_" ++ baseString name
      pure ([C.cparam|$ty:ctp *$id:p_name|], [C.cexp|$id:p_name|])
    compileOutput (MemParam name space) = do
      ty <- memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      pure ([C.cparam|$ty:ty *$id:p_name|], [C.cexp|$id:p_name|])

derefPointer :: C.Exp -> C.Exp -> C.Type -> C.Exp
derefPointer ptr i res_t =
  [C.cexp|(($ty:res_t)$exp:ptr)[$exp:i]|]

volQuals :: Volatility -> [C.TypeQual]
volQuals Volatile = [C.ctyquals|volatile|]
volQuals Nonvolatile = []

writeScalarPointerWithQuals :: PointerQuals op s -> WriteScalar op s
writeScalarPointerWithQuals quals_f dest i elemtype space vol v = do
  quals <- quals_f space
  let quals' = volQuals vol ++ quals
      deref =
        derefPointer
          dest
          i
          [C.cty|$tyquals:quals' $ty:elemtype*|]
  stm [C.cstm|$exp:deref = $exp:v;|]

readScalarPointerWithQuals :: PointerQuals op s -> ReadScalar op s
readScalarPointerWithQuals quals_f dest i elemtype space vol = do
  quals <- quals_f space
  let quals' = volQuals vol ++ quals
  pure $ derefPointer dest i [C.cty|$tyquals:quals' $ty:elemtype*|]

compileExpToName :: String -> PrimType -> Exp -> CompilerM op s VName
compileExpToName _ _ (LeafExp v _) =
  pure v
compileExpToName desc t e = do
  desc' <- newVName desc
  e' <- compileExp e
  decl [C.cdecl|$ty:(primTypeToCType t) $id:desc' = $e';|]
  pure desc'

compileExp :: Exp -> CompilerM op s C.Exp
compileExp = compilePrimExp $ \v -> pure [C.cexp|$id:v|]

-- | Tell me how to compile a @v@, and I'll Compile any @PrimExp v@ for you.
compilePrimExp :: Monad m => (v -> m C.Exp) -> PrimExp v -> m C.Exp
compilePrimExp _ (ValueExp val) =
  pure $ C.toExp val mempty
compilePrimExp f (LeafExp v _) =
  f v
compilePrimExp f (UnOpExp Complement {} x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|~$exp:x'|]
compilePrimExp f (UnOpExp Not {} x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|!$exp:x'|]
compilePrimExp f (UnOpExp (FAbs Float32) x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|(float)fabs($exp:x')|]
compilePrimExp f (UnOpExp (FAbs Float64) x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|fabs($exp:x')|]
compilePrimExp f (UnOpExp SSignum {} x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|($exp:x' > 0 ? 1 : 0) - ($exp:x' < 0 ? 1 : 0)|]
compilePrimExp f (UnOpExp USignum {} x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|($exp:x' > 0 ? 1 : 0) - ($exp:x' < 0 ? 1 : 0) != 0|]
compilePrimExp f (UnOpExp op x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|$id:(pretty op)($exp:x')|]
compilePrimExp f (CmpOpExp cmp x y) = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  pure $ case cmp of
    CmpEq {} -> [C.cexp|$exp:x' == $exp:y'|]
    FCmpLt {} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe {} -> [C.cexp|$exp:x' <= $exp:y'|]
    CmpLlt {} -> [C.cexp|$exp:x' < $exp:y'|]
    CmpLle {} -> [C.cexp|$exp:x' <= $exp:y'|]
    _ -> [C.cexp|$id:(pretty cmp)($exp:x', $exp:y')|]
compilePrimExp f (ConvOpExp conv x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|$id:(pretty conv)($exp:x')|]
compilePrimExp f (BinOpExp bop x y) = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  -- Note that integer addition, subtraction, and multiplication with
  -- OverflowWrap are not handled by explicit operators, but rather by
  -- functions.  This is because we want to implicitly convert them to
  -- unsigned numbers, so we can do overflow without invoking
  -- undefined behaviour.
  pure $ case bop of
    Add _ OverflowUndef -> [C.cexp|$exp:x' + $exp:y'|]
    Sub _ OverflowUndef -> [C.cexp|$exp:x' - $exp:y'|]
    Mul _ OverflowUndef -> [C.cexp|$exp:x' * $exp:y'|]
    FAdd {} -> [C.cexp|$exp:x' + $exp:y'|]
    FSub {} -> [C.cexp|$exp:x' - $exp:y'|]
    FMul {} -> [C.cexp|$exp:x' * $exp:y'|]
    FDiv {} -> [C.cexp|$exp:x' / $exp:y'|]
    Xor {} -> [C.cexp|$exp:x' ^ $exp:y'|]
    And {} -> [C.cexp|$exp:x' & $exp:y'|]
    Or {} -> [C.cexp|$exp:x' | $exp:y'|]
    LogAnd {} -> [C.cexp|$exp:x' && $exp:y'|]
    LogOr {} -> [C.cexp|$exp:x' || $exp:y'|]
    _ -> [C.cexp|$id:(pretty bop)($exp:x', $exp:y')|]
compilePrimExp f (FunExp h args _) = do
  args' <- mapM (compilePrimExp f) args
  pure [C.cexp|$id:(funName (nameFromString h))($args:args')|]

linearCode :: Code op -> [Code op]
linearCode = reverse . go []
  where
    go acc (x :>>: y) =
      go (go acc x) y
    go acc x = x : acc

compileCode :: Code op -> CompilerM op s ()
compileCode (Op op) =
  join $ asks envOpCompiler <*> pure op
compileCode Skip = pure ()
compileCode (Comment s code) = do
  xs <- collect $ compileCode code
  let comment = "// " ++ s
  stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCode (TracePrint msg) = do
  (formatstr, formatargs) <- errorMsgString msg
  stm [C.cstm|fprintf(ctx->log, $string:formatstr, $args:formatargs);|]
compileCode (DebugPrint s (Just e)) = do
  e' <- compileExp e
  stm
    [C.cstm|if (ctx->debugging) {
          fprintf(ctx->log, $string:fmtstr, $exp:s, ($ty:ety)$exp:e', '\n');
       }|]
  where
    (fmt, ety) = case primExpType e of
      IntType _ -> ("llu", [C.cty|long long int|])
      FloatType _ -> ("f", [C.cty|double|])
      _ -> ("d", [C.cty|int|])
    fmtstr = "%s: %" ++ fmt ++ "%c"
compileCode (DebugPrint s Nothing) =
  stm
    [C.cstm|if (ctx->debugging) {
          fprintf(ctx->log, "%s\n", $exp:s);
       }|]
-- :>>: is treated in a special way to detect declare-set pairs in
-- order to generate prettier code.
compileCode (c1 :>>: c2) = go (linearCode (c1 :>>: c2))
  where
    go (DeclareScalar name vol t : SetScalar dest e : code)
      | name == dest = do
          let ct = primTypeToCType t
          e' <- compileExp e
          item [C.citem|$tyquals:(volQuals vol) $ty:ct $id:name = $exp:e';|]
          go code
    go (x : xs) = compileCode x >> go xs
    go [] = pure ()
compileCode (Assert e msg (loc, locs)) = do
  e' <- compileExp e
  err <-
    collect . join $
      asks (opsError . envOperations) <*> pure msg <*> pure stacktrace
  stm [C.cstm|if (!$exp:e') { $items:err }|]
  where
    stacktrace = prettyStacktrace 0 $ map locStr $ loc : locs
compileCode (Allocate _ _ ScalarSpace {}) =
  -- Handled by the declaration of the memory block, which is
  -- translated to an actual array.
  pure ()
compileCode (Allocate name (Count (TPrimExp e)) space) = do
  size <- compileExp e
  cached <- cacheMem name
  case cached of
    Just cur_size ->
      stm
        [C.cstm|if ($exp:cur_size < $exp:size) {
                 err = lexical_realloc(&ctx->error, &$exp:name, &$exp:cur_size, $exp:size);
                 if (err != FUTHARK_SUCCESS) {
                   goto cleanup;
                 }
                }|]
    _ ->
      allocMem name size space [C.cstm|{err = 1; goto cleanup;}|]
compileCode (Free name space) = do
  cached <- isJust <$> cacheMem name
  unless cached $ unRefMem name space
compileCode (For i bound body) = do
  let i' = C.toIdent i
      t = primTypeToCType $ primExpType bound
  bound' <- compileExp bound
  body' <- collect $ compileCode body
  stm
    [C.cstm|for ($ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileCode (While cond body) = do
  cond' <- compileExp $ untyped cond
  body' <- collect $ compileCode body
  stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp $ untyped cond
  tbranch' <- collect $ compileCode tbranch
  fbranch' <- collect $ compileCode fbranch
  stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCode (Copy _ dest (Count destoffset) DefaultSpace src (Count srcoffset) DefaultSpace (Count size)) =
  join $
    copyMemoryDefaultSpace
      <$> rawMem dest
      <*> compileExp (untyped destoffset)
      <*> rawMem src
      <*> compileExp (untyped srcoffset)
      <*> compileExp (untyped size)
compileCode (Copy _ dest (Count destoffset) destspace src (Count srcoffset) srcspace (Count size)) = do
  copy <- asks envCopy
  join $
    copy CopyBarrier
      <$> rawMem dest
      <*> compileExp (untyped destoffset)
      <*> pure destspace
      <*> rawMem src
      <*> compileExp (untyped srcoffset)
      <*> pure srcspace
      <*> compileExp (untyped size)
compileCode (Write _ _ Unit _ _ _) = pure ()
compileCode (Write dest (Count idx) elemtype DefaultSpace vol elemexp) = do
  dest' <- rawMem dest
  deref <-
    derefPointer dest'
      <$> compileExp (untyped idx)
      <*> pure [C.cty|$tyquals:(volQuals vol) $ty:(primStorageType elemtype)*|]
  elemexp' <- toStorage elemtype <$> compileExp elemexp
  stm [C.cstm|$exp:deref = $exp:elemexp';|]
compileCode (Write dest (Count idx) _ ScalarSpace {} _ elemexp) = do
  idx' <- compileExp (untyped idx)
  elemexp' <- compileExp elemexp
  stm [C.cstm|$id:dest[$exp:idx'] = $exp:elemexp';|]
compileCode (Write dest (Count idx) elemtype (Space space) vol elemexp) =
  join $
    asks envWriteScalar
      <*> rawMem dest
      <*> compileExp (untyped idx)
      <*> pure (primStorageType elemtype)
      <*> pure space
      <*> pure vol
      <*> (toStorage elemtype <$> compileExp elemexp)
compileCode (Read x _ _ Unit __ _) =
  stm [C.cstm|$id:x = $exp:(UnitValue);|]
compileCode (Read x src (Count iexp) restype DefaultSpace vol) = do
  src' <- rawMem src
  e <-
    fmap (fromStorage restype) $
      derefPointer src'
        <$> compileExp (untyped iexp)
        <*> pure [C.cty|$tyquals:(volQuals vol) $ty:(primStorageType restype)*|]
  stm [C.cstm|$id:x = $exp:e;|]
compileCode (Read x src (Count iexp) restype (Space space) vol) = do
  e <-
    fmap (fromStorage restype) . join $
      asks envReadScalar
        <*> rawMem src
        <*> compileExp (untyped iexp)
        <*> pure (primStorageType restype)
        <*> pure space
        <*> pure vol
  stm [C.cstm|$id:x = $exp:e;|]
compileCode (Read x src (Count iexp) _ ScalarSpace {} _) = do
  iexp' <- compileExp $ untyped iexp
  stm [C.cstm|$id:x = $id:src[$exp:iexp'];|]
compileCode (DeclareMem name space) =
  declMem name space
compileCode (DeclareScalar name vol t) = do
  let ct = primTypeToCType t
  decl [C.cdecl|$tyquals:(volQuals vol) $ty:ct $id:name;|]
compileCode (DeclareArray name ScalarSpace {} _ _) =
  error $ "Cannot declare array " ++ pretty name ++ " in scalar space."
compileCode (DeclareArray name DefaultSpace t vs) = do
  name_realtype <- newVName $ baseString name ++ "_realtype"
  let ct = primTypeToCType t
  case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Fake a memory block.
  contextField
    (C.toIdent name noLoc)
    [C.cty|struct memblock|]
    $ Just
      [C.cexp|(struct memblock){NULL,
                                (unsigned char*)$id:name_realtype,
                                0,
                                $string:(pretty name)}|]
  item [C.citem|struct memblock $id:name = ctx->$id:name;|]
compileCode (DeclareArray name (Space space) t vs) =
  join $
    asks envStaticArray
      <*> pure name
      <*> pure space
      <*> pure t
      <*> pure vs
-- For assignments of the form 'x = x OP e', we generate C assignment
-- operators to make the resulting code slightly nicer.  This has no
-- effect on performance.
compileCode (SetScalar dest (BinOpExp op (LeafExp x _) y))
  | dest == x,
    Just f <- assignmentOperator op = do
      y' <- compileExp y
      stm [C.cstm|$exp:(f dest y');|]
compileCode (SetScalar dest src) = do
  src' <- compileExp src
  stm [C.cstm|$id:dest = $exp:src';|]
compileCode (SetMem dest src space) =
  setMem dest src space
compileCode (Call dests fname args) =
  join $
    asks (opsCall . envOperations)
      <*> pure dests
      <*> pure fname
      <*> mapM compileArg args
  where
    compileArg (MemArg m) = pure [C.cexp|$exp:m|]
    compileArg (ExpArg e) = compileExp e

compileFunBody :: [C.Exp] -> [Param] -> Code op -> CompilerM op s ()
compileFunBody output_ptrs outputs code = do
  mapM_ declareOutput outputs
  compileCode code
  zipWithM_ setRetVal' output_ptrs outputs
  where
    declareOutput (MemParam name space) =
      declMem name space
    declareOutput (ScalarParam name pt) = do
      let ctp = primTypeToCType pt
      decl [C.cdecl|$ty:ctp $id:name;|]

    setRetVal' p (MemParam name space) = do
      resetMem [C.cexp|*$exp:p|] space
      setMem [C.cexp|*$exp:p|] name space
    setRetVal' p (ScalarParam name _) =
      stm [C.cstm|*$exp:p = $id:name;|]

assignmentOperator :: BinOp -> Maybe (VName -> C.Exp -> C.Exp)
assignmentOperator Add {} = Just $ \d e -> [C.cexp|$id:d += $exp:e|]
assignmentOperator Sub {} = Just $ \d e -> [C.cexp|$id:d -= $exp:e|]
assignmentOperator Mul {} = Just $ \d e -> [C.cexp|$id:d *= $exp:e|]
assignmentOperator _ = Nothing
