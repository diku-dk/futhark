{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg,
    CParts (..),
    asLibrary,
    asExecutable,

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
    Copy,
    StaticArray,

    -- * Monadic compiler interface
    CompilerM,
    CompilerState (compUserState),
    getUserState,
    modifyUserState,
    contextContents,
    contextFinalInits,
    runCompilerM,
    cachingMemory,
    blockScope,
    compileFun,
    compileCode,
    compileExp,
    compilePrimExp,
    compilePrimValue,
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
    HeaderSection (..),
    libDecl,
    earlyDecl,
    publicName,
    contextType,
    contextField,

    -- * Building Blocks
    primTypeToCType,
    copyMemoryDefaultSpace,
  )
where

import Control.Monad.Identity
import Control.Monad.RWS
import Data.Bifunctor (first)
import Data.Bits (shiftR, xor)
import Data.Char (isAlphaNum, isDigit, ord)
import qualified Data.DList as DL
import Data.FileEmbed
import Data.List (unzip4)
import Data.Loc
import qualified Data.Map.Strict as M
import Data.Maybe
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode
import Futhark.IR.Prop (isBuiltInFunction)
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)
import qualified Language.C.Quote.OpenCL as C
import qualified Language.C.Syntax as C
import Text.Printf

data CompilerState s = CompilerState
  { compArrayStructs :: [((C.Type, Int), (C.Type, [C.Definition]))],
    compOpaqueStructs :: [(String, (C.Type, [C.Definition]))],
    compEarlyDecls :: DL.DList C.Definition,
    compInit :: [C.Stm],
    compNameSrc :: VNameSource,
    compUserState :: s,
    compHeaderDecls :: M.Map HeaderSection (DL.DList C.Definition),
    compLibDecls :: DL.DList C.Definition,
    compCtxFields :: DL.DList (C.Id, C.Type, Maybe C.Exp),
    compProfileItems :: DL.DList C.BlockItem,
    compDeclaredMem :: [(VName, Space)]
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s =
  CompilerState
    { compArrayStructs = [],
      compOpaqueStructs = [],
      compEarlyDecls = mempty,
      compInit = [],
      compNameSrc = src,
      compUserState = s,
      compHeaderDecls = mempty,
      compLibDecls = mempty,
      compCtxFields = mempty,
      compProfileItems = mempty,
      compDeclaredMem = mempty
    }

-- | In which part of the header file we put the declaration.  This is
-- to ensure that the header file remains structured and readable.
data HeaderSection
  = ArrayDecl String
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

-- | Copy from one memory block to another.
type Copy op s =
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

defError :: ErrorCompiler op s
defError (ErrorMsg parts) stacktrace = do
  free_all_mem <- collect $ mapM_ (uncurry unRefMem) =<< gets compDeclaredMem
  let onPart (ErrorString s) = return ("%s", [C.cexp|$string:s|])
      onPart (ErrorInt32 x) = ("%d",) <$> compileExp x
  (formatstrs, formatargs) <- unzip <$> mapM onPart parts
  let formatstr = "Error: " ++ concat formatstrs ++ "\n\nBacktrace:\n%s"
  items
    [C.citems|ctx->error = msgprintf($string:formatstr, $args:formatargs, $string:stacktrace);
                  $items:free_all_mem
                  return 1;|]

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
    defCopy destmem destoffset DefaultSpace srcmem srcoffset DefaultSpace size =
      copyMemoryDefaultSpace destmem destoffset srcmem srcoffset size
    defCopy _ _ _ _ _ _ _ =
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

newtype CompilerAcc op s = CompilerAcc
  { accItems :: DL.DList C.BlockItem
  }

instance Semigroup (CompilerAcc op s) where
  CompilerAcc items1 <> CompilerAcc items2 =
    CompilerAcc (items1 <> items2)

instance Monoid (CompilerAcc op s) where
  mempty = CompilerAcc mempty

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

arrayDefinitions, opaqueDefinitions :: CompilerState s -> [C.Definition]
arrayDefinitions = concatMap (snd . snd) . compArrayStructs
opaqueDefinitions = concatMap (snd . snd) . compOpaqueStructs

initDecls, arrayDecls, opaqueDecls, entryDecls, miscDecls :: CompilerState s -> [C.Definition]
initDecls = concatMap (DL.toList . snd) . filter ((== InitDecl) . fst) . M.toList . compHeaderDecls
arrayDecls = concatMap (DL.toList . snd) . filter (isArrayDecl . fst) . M.toList . compHeaderDecls
  where
    isArrayDecl ArrayDecl {} = True
    isArrayDecl _ = False
opaqueDecls = concatMap (DL.toList . snd) . filter (isOpaqueDecl . fst) . M.toList . compHeaderDecls
  where
    isOpaqueDecl OpaqueDecl {} = True
    isOpaqueDecl _ = False
entryDecls = concatMap (DL.toList . snd) . filter ((== EntryDecl) . fst) . M.toList . compHeaderDecls
miscDecls = concatMap (DL.toList . snd) . filter ((== MiscDecl) . fst) . M.toList . compHeaderDecls

contextContents :: CompilerM op s ([C.FieldGroup], [C.Stm])
contextContents = do
  (field_names, field_types, field_values) <- gets $ unzip3 . DL.toList . compCtxFields
  let fields =
        [ [C.csdecl|$ty:ty $id:name;|]
          | (name, ty) <- zip field_names field_types
        ]
      init_fields =
        [ [C.cstm|ctx->$id:name = $exp:e;|]
          | (name, Just e) <- zip field_names field_values
        ]
  return (fields, init_fields)

contextFinalInits :: CompilerM op s [C.Stm]
contextFinalInits = gets compInit

newtype CompilerM op s a
  = CompilerM
      ( RWS
          (CompilerEnv op s)
          (CompilerAcc op s)
          (CompilerState s)
          a
      )
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (CompilerState s),
      MonadReader (CompilerEnv op s),
      MonadWriter (CompilerAcc op s)
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
  let (x, s, _) = runRWS m (CompilerEnv ops mempty) (newCompilerState src userstate)
   in (x, s)

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
collect' m = pass $ do
  (x, w) <- listen m
  return
    ( (x, DL.toList $ accItems w),
      const w {accItems = mempty}
    )

item :: C.BlockItem -> CompilerM op s ()
item x = tell $ mempty {accItems = DL.singleton x}

items :: [C.BlockItem] -> CompilerM op s ()
items = mapM_ item

fatMemory :: Space -> CompilerM op s Bool
fatMemory ScalarSpace {} = return False
fatMemory _ = asks envFatMemory

cacheMem :: C.ToExp a => a -> CompilerM op s (Maybe VName)
cacheMem a = asks $ M.lookup (C.toExp a noLoc) . envCachedMem

instance C.ToIdent Name where
  toIdent = C.toIdent . zEncodeString . nameToString

instance C.ToIdent VName where
  toIdent = C.toIdent . zEncodeString . pretty

instance C.ToExp VName where
  toExp v _ = [C.cexp|$id:v|]

instance C.ToExp IntValue where
  toExp (Int8Value v) = C.toExp v
  toExp (Int16Value v) = C.toExp v
  toExp (Int32Value v) = C.toExp v
  toExp (Int64Value v) = C.toExp v

instance C.ToExp FloatValue where
  toExp (Float32Value v) = C.toExp v
  toExp (Float64Value v) = C.toExp v

instance C.ToExp PrimValue where
  toExp (IntValue v) = C.toExp v
  toExp (FloatValue v) = C.toExp v
  toExp (BoolValue True) = C.toExp (1 :: Int8)
  toExp (BoolValue False) = C.toExp (0 :: Int8)
  toExp Checked = C.toExp (1 :: Int8)

instance C.ToExp SubExp where
  toExp (Var v) = C.toExp v
  toExp (Constant c) = C.toExp c

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
  return s'

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
  s {compCtxFields = compCtxFields s <> DL.singleton (name, ty, initial)}

profileReport :: C.BlockItem -> CompilerM op s ()
profileReport x = modify $ \s ->
  s {compProfileItems = compProfileItems s <> DL.singleton x}

stm :: C.Stm -> CompilerM op s ()
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op s ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op s ()
decl x = item [C.citem|$decl:x;|]

addrOf :: C.Exp -> C.Exp
addrOf e = [C.cexp|&$exp:e|]

-- | Public names must have a consitent prefix.
publicName :: String -> CompilerM op s String
publicName s = return $ "futhark_" ++ s

-- | The generated code must define a struct with this name.
contextType :: CompilerM op s C.Type
contextType = do
  name <- publicName "context"
  return [C.cty|struct $id:name|]

memToCType :: VName -> Space -> CompilerM op s C.Type
memToCType v space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem v
  if refcount && not cached
    then return $ fatMemType space
    else rawMemCType space

rawMemCType :: Space -> CompilerM op s C.Type
rawMemCType DefaultSpace = return defaultMemBlockType
rawMemCType (Space sid) = join $ asks envMemoryType <*> pure sid
rawMemCType (ScalarSpace [] t) =
  return [C.cty|$ty:(primTypeToCType t)[1]|]
rawMemCType (ScalarSpace ds t) =
  return [C.cty|$ty:(primTypeToCType t)[$exp:(cproduct ds')]|]
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
      asks envAllocate <*> pure [C.cexp|$exp:dest|]
        <*> pure [C.cexp|$exp:size|]
        <*> pure [C.cexp|$exp:desc|]
        <*> pure sid
  _ ->
    stm [C.cstm|$exp:dest = (char*) malloc($exp:size);|]

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
        [C.cedecl|static int $id:(fatMemUnRef space) ($ty:ctx_ty *ctx, $ty:mty *block, const char *desc) {
  if (block->references != NULL) {
    *(block->references) -= 1;
    if (ctx->detail_memory) {
      fprintf(stderr, "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                      desc, block->desc, $string:spacedesc, *(block->references));
    }
    if (*(block->references) == 0) {
      ctx->$id:usagename -= block->size;
      $items:free
      free(block->references);
      if (ctx->detail_memory) {
        fprintf(stderr, "%lld bytes freed (now allocated: %lld bytes)\n",
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
        [C.cedecl|static int $id:(fatMemAlloc space) ($ty:ctx_ty *ctx, $ty:mty *block, typename int64_t size, const char *desc) {
  if (size < 0) {
    futhark_panic(1, "Negative allocation of %lld bytes attempted for %s in %s.\n",
          (long long)size, desc, $string:spacedesc, ctx->$id:usagename);
  }
  int ret = $id:(fatMemUnRef space)(ctx, block, desc);

  ctx->$id:usagename += size;
  if (ctx->detail_memory) {
    fprintf(stderr, "Allocating %lld bytes for %s in %s (then allocated: %lld bytes)",
            (long long) size,
            desc, $string:spacedesc,
            (long long) ctx->$id:usagename);
  }
  if (ctx->$id:usagename > ctx->$id:peakname) {
    ctx->$id:peakname = ctx->$id:usagename;
    if (ctx->detail_memory) {
      fprintf(stderr, " (new peak).\n");
    }
  } else if (ctx->detail_memory) {
    fprintf(stderr, ".\n");
  }

  $items:alloc
  block->references = (int*) malloc(sizeof(int));
  *(block->references) = 1;
  block->size = size;
  block->desc = desc;
  return ret;
  }|]

  -- Memory setting - unreference the destination and increase the
  -- count of the source by one.
  let setdef =
        [C.cedecl|static int $id:(fatMemSet space) ($ty:ctx_ty *ctx, $ty:mty *lhs, $ty:mty *rhs, const char *lhs_desc) {
  int ret = $id:(fatMemUnRef space)(ctx, lhs, lhs_desc);
  (*(rhs->references))++;
  *lhs = *rhs;
  return ret;
}
|]

  let peakmsg = "Peak memory usage for " ++ spacedesc ++ ": %lld bytes.\n"
  return
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
  unless cached $ do
    ty <- memToCType name space
    decl [C.cdecl|$ty:ty $id:name;|]
    resetMem name space
    modify $ \s -> s {compDeclaredMem = (name, space) : compDeclaredMem s}

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

primTypeInfo :: PrimType -> Signedness -> C.Exp
primTypeInfo (IntType it) t = case (it, t) of
  (Int8, TypeUnsigned) -> [C.cexp|u8_info|]
  (Int16, TypeUnsigned) -> [C.cexp|u16_info|]
  (Int32, TypeUnsigned) -> [C.cexp|u32_info|]
  (Int64, TypeUnsigned) -> [C.cexp|u64_info|]
  (Int8, _) -> [C.cexp|i8_info|]
  (Int16, _) -> [C.cexp|i16_info|]
  (Int32, _) -> [C.cexp|i32_info|]
  (Int64, _) -> [C.cexp|i64_info|]
primTypeInfo (FloatType Float32) _ = [C.cexp|f32_info|]
primTypeInfo (FloatType Float64) _ = [C.cexp|f64_info|]
primTypeInfo Bool _ = [C.cexp|bool_info|]
primTypeInfo Cert _ = [C.cexp|bool_info|]

copyMemoryDefaultSpace ::
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  CompilerM op s ()
copyMemoryDefaultSpace destmem destidx srcmem srcidx nbytes =
  stm
    [C.cstm|memmove($exp:destmem + $exp:destidx,
                      $exp:srcmem + $exp:srcidx,
                      $exp:nbytes);|]

--- Entry points.

arrayName :: PrimType -> Signedness -> Int -> String
arrayName pt signed rank =
  prettySigned (signed == TypeUnsigned) pt ++ "_" ++ show rank ++ "d"

opaqueName :: String -> [ValueDesc] -> String
opaqueName s _
  | valid = "opaque_" ++ s
  where
    valid =
      head s /= '_'
        && not (isDigit $ head s)
        && all ok s
    ok c = isAlphaNum c || c == '_'
opaqueName s vds = "opaque_" ++ hash (zipWith xor [0 ..] $ map ord (s ++ concatMap p vds))
  where
    p (ScalarValue pt signed _) =
      show (pt, signed)
    p (ArrayValue _ space pt signed dims) =
      show (space, pt, signed, length dims)

    -- FIXME: a stupid hash algorithm; may have collisions.
    hash =
      printf "%x" . foldl xor 0
        . map
          ( iter . (* 0x45d9f3b)
              . iter
              . (* 0x45d9f3b)
              . iter
              . fromIntegral
          )
    iter x = ((x :: Word32) `shiftR` 16) `xor` x

criticalSection :: Operations op s -> [C.BlockItem] -> [C.BlockItem]
criticalSection ops x =
  [C.citems|lock_lock(&ctx->lock);
            $items:(fst (opsCritical ops))
            $items:x
            $items:(snd (opsCritical ops))
            lock_unlock(&ctx->lock);
           |]

arrayLibraryFunctions ::
  Space ->
  PrimType ->
  Signedness ->
  [DimSize] ->
  CompilerM op s [C.Definition]
arrayLibraryFunctions space pt signed shape = do
  let rank = length shape
      pt' = signedPrimTypeToCType signed pt
      name = arrayName pt signed rank
      arr_name = "futhark_" ++ name
      array_type = [C.cty|struct $id:arr_name|]

  new_array <- publicName $ "new_" ++ name
  new_raw_array <- publicName $ "new_raw_" ++ name
  free_array <- publicName $ "free_" ++ name
  values_array <- publicName $ "values_" ++ name
  values_raw_array <- publicName $ "values_raw_" ++ name
  shape_array <- publicName $ "shape_" ++ name

  let shape_names = ["dim" ++ show i | i <- [0 .. rank -1]]
      shape_params = [[C.cparam|typename int64_t $id:k|] | k <- shape_names]
      arr_size = cproduct [[C.cexp|$id:k|] | k <- shape_names]
      arr_size_array = cproduct [[C.cexp|arr->shape[$int:i]|] | i <- [0 .. rank -1]]
  copy <- asks envCopy

  memty <- rawMemCType space

  let prepare_new = do
        resetMem [C.cexp|arr->mem|] space
        allocMem
          [C.cexp|arr->mem|]
          [C.cexp|((size_t)$exp:arr_size) * sizeof($ty:pt')|]
          space
          [C.cstm|return NULL;|]
        forM_ [0 .. rank -1] $ \i ->
          let dim_s = "dim" ++ show i
           in stm [C.cstm|arr->shape[$int:i] = $id:dim_s;|]

  new_body <- collect $ do
    prepare_new
    copy
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|data|]
      [C.cexp|0|]
      DefaultSpace
      [C.cexp|((size_t)$exp:arr_size) * sizeof($ty:pt')|]

  new_raw_body <- collect $ do
    prepare_new
    copy
      [C.cexp|arr->mem.mem|]
      [C.cexp|0|]
      space
      [C.cexp|data|]
      [C.cexp|offset|]
      space
      [C.cexp|((size_t)$exp:arr_size) * sizeof($ty:pt')|]

  free_body <- collect $ unRefMem [C.cexp|arr->mem|] space

  values_body <-
    collect $
      copy
        [C.cexp|data|]
        [C.cexp|0|]
        DefaultSpace
        [C.cexp|arr->mem.mem|]
        [C.cexp|0|]
        space
        [C.cexp|((size_t)$exp:arr_size_array) * sizeof($ty:pt')|]

  ctx_ty <- contextType
  ops <- asks envOperations

  headerDecl
    (ArrayDecl name)
    [C.cedecl|struct $id:arr_name;|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|$ty:array_type* $id:new_array($ty:ctx_ty *ctx, const $ty:pt' *data, $params:shape_params);|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|$ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, const $ty:memty data, int offset, $params:shape_params);|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|int $id:free_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|int $id:values_array($ty:ctx_ty *ctx, $ty:array_type *arr, $ty:pt' *data);|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|$ty:memty $id:values_raw_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]
  headerDecl
    (ArrayDecl name)
    [C.cedecl|const typename int64_t* $id:shape_array($ty:ctx_ty *ctx, $ty:array_type *arr);|]

  return
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

          $ty:array_type* $id:new_raw_array($ty:ctx_ty *ctx, const $ty:memty data, int offset,
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

opaqueLibraryFunctions ::
  String ->
  [ValueDesc] ->
  CompilerM op s [C.Definition]
opaqueLibraryFunctions desc vds = do
  name <- publicName $ opaqueName desc vds
  free_opaque <- publicName $ "free_" ++ opaqueName desc vds

  let opaque_type = [C.cty|struct $id:name|]

      freeComponent _ ScalarValue {} =
        return ()
      freeComponent i (ArrayValue _ _ pt signed shape) = do
        let rank = length shape
        free_array <- publicName $ "free_" ++ arrayName pt signed rank
        stm
          [C.cstm|if ((tmp = $id:free_array(ctx, obj->$id:(tupleField i))) != 0) {
                ret = tmp;
             }|]

  ctx_ty <- contextType

  free_body <- collect $ zipWithM_ freeComponent [0 ..] vds

  headerDecl
    (OpaqueDecl desc)
    [C.cedecl|int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj);|]

  return
    [C.cunit|
          int $id:free_opaque($ty:ctx_ty *ctx, $ty:opaque_type *obj) {
            int ret = 0, tmp;
            $items:free_body
            free(obj);
            return ret;
          }
           |]

valueDescToCType :: ValueDesc -> CompilerM op s C.Type
valueDescToCType (ScalarValue pt signed _) =
  return $ signedPrimTypeToCType signed pt
valueDescToCType (ArrayValue mem space pt signed shape) = do
  let pt' = signedPrimTypeToCType signed pt
      rank = length shape
  exists <- gets $ lookup (pt', rank) . compArrayStructs
  case exists of
    Just (cty, _) -> return cty
    Nothing -> do
      memty <- memToCType mem space
      name <- publicName $ arrayName pt signed rank
      let struct = [C.cedecl|struct $id:name { $ty:memty mem; typename int64_t shape[$int:rank]; };|]
          stype = [C.cty|struct $id:name|]
      library <- arrayLibraryFunctions space pt signed shape
      modify $ \s ->
        s
          { compArrayStructs =
              ((pt', rank), (stype, struct : library)) : compArrayStructs s
          }
      return stype

opaqueToCType :: String -> [ValueDesc] -> CompilerM op s C.Type
opaqueToCType desc vds = do
  name <- publicName $ opaqueName desc vds
  exists <- gets $ lookup name . compOpaqueStructs
  case exists of
    Just (ty, _) -> return ty
    Nothing -> do
      members <- zipWithM field vds [(0 :: Int) ..]
      let struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype = [C.cty|struct $id:name|]
      headerDecl (OpaqueDecl desc) [C.cedecl|struct $id:name;|]
      library <- opaqueLibraryFunctions desc vds
      modify $ \s ->
        s
          { compOpaqueStructs =
              (name, (stype, struct : library)) :
              compOpaqueStructs s
          }
      return stype
  where
    field vd@ScalarValue {} i = do
      ct <- valueDescToCType vd
      return [C.csdecl|$ty:ct $id:(tupleField i);|]
    field vd i = do
      ct <- valueDescToCType vd
      return [C.csdecl|$ty:ct *$id:(tupleField i);|]

externalValueToCType :: ExternalValue -> CompilerM op s C.Type
externalValueToCType (TransparentValue vd) = valueDescToCType vd
externalValueToCType (OpaqueValue desc vds) = opaqueToCType desc vds

prepareEntryInputs :: [ExternalValue] -> CompilerM op s [C.Param]
prepareEntryInputs = zipWithM prepare [(0 :: Int) ..]
  where
    prepare pno (TransparentValue vd) = do
      let pname = "in" ++ show pno
      ty <- prepareValue [C.cexp|$id:pname|] vd
      return [C.cparam|const $ty:ty $id:pname|]
    prepare pno (OpaqueValue desc vds) = do
      ty <- opaqueToCType desc vds
      let pname = "in" ++ show pno
          field i ScalarValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
          field i ArrayValue {} = [C.cexp|$id:pname->$id:(tupleField i)|]
      zipWithM_ prepareValue (zipWith field [0 ..] vds) vds
      return [C.cparam|const $ty:ty *$id:pname|]

    prepareValue src (ScalarValue pt signed name) = do
      let pt' = signedPrimTypeToCType signed pt
      stm [C.cstm|$id:name = $exp:src;|]
      return pt'
    prepareValue src vd@(ArrayValue mem _ _ _ shape) = do
      ty <- valueDescToCType vd

      stm [C.cstm|$exp:mem = $exp:src->mem;|]

      let rank = length shape
          maybeCopyDim (Var d) i =
            Just [C.cstm|$id:d = $exp:src->shape[$int:i];|]
          maybeCopyDim _ _ = Nothing

      stms $ catMaybes $ zipWith maybeCopyDim shape [0 .. rank -1]

      return [C.cty|$ty:ty*|]

prepareEntryOutputs :: [ExternalValue] -> CompilerM op s [C.Param]
prepareEntryOutputs = zipWithM prepare [(0 :: Int) ..]
  where
    prepare pno (TransparentValue vd) = do
      let pname = "out" ++ show pno
      ty <- valueDescToCType vd

      case vd of
        ArrayValue {} -> do
          stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]
          prepareValue [C.cexp|*$id:pname|] vd
          return [C.cparam|$ty:ty **$id:pname|]
        ScalarValue {} -> do
          prepareValue [C.cexp|*$id:pname|] vd
          return [C.cparam|$ty:ty *$id:pname|]
    prepare pno (OpaqueValue desc vds) = do
      let pname = "out" ++ show pno
      ty <- opaqueToCType desc vds
      vd_ts <- mapM valueDescToCType vds

      stm [C.cstm|assert((*$id:pname = ($ty:ty*) malloc(sizeof($ty:ty))) != NULL);|]

      forM_ (zip3 [0 ..] vd_ts vds) $ \(i, ct, vd) -> do
        let field = [C.cexp|(*$id:pname)->$id:(tupleField i)|]
        case vd of
          ScalarValue {} -> return ()
          _ -> stm [C.cstm|assert(($exp:field = ($ty:ct*) malloc(sizeof($ty:ct))) != NULL);|]
        prepareValue field vd

      return [C.cparam|$ty:ty **$id:pname|]

    prepareValue dest (ScalarValue _ _ name) =
      stm [C.cstm|$exp:dest = $id:name;|]
    prepareValue dest (ArrayValue mem _ _ _ shape) = do
      stm [C.cstm|$exp:dest->mem = $id:mem;|]

      let rank = length shape
          maybeCopyDim (Constant x) i =
            [C.cstm|$exp:dest->shape[$int:i] = $exp:x;|]
          maybeCopyDim (Var d) i =
            [C.cstm|$exp:dest->shape[$int:i] = $id:d;|]
      stms $ zipWith maybeCopyDim shape [0 .. rank -1]

onEntryPoint ::
  Name ->
  Function op ->
  CompilerM op s (C.Definition, C.Definition, C.Initializer)
onEntryPoint fname function@(Function _ outputs inputs _ results args) = do
  let out_args = map (\p -> [C.cexp|&$id:(paramName p)|]) outputs
      in_args = map (\p -> [C.cexp|$id:(paramName p)|]) inputs

  inputdecls <- collect $ mapM_ stubParam inputs
  outputdecls <- collect $ mapM_ stubParam outputs

  let entry_point_name = nameToString fname
  entry_point_function_name <- publicName $ "entry_" ++ entry_point_name

  (entry_point_input_params, unpack_entry_inputs) <-
    collect' $ prepareEntryInputs args
  (entry_point_output_params, pack_entry_outputs) <-
    collect' $ prepareEntryOutputs results

  (cli_entry_point, cli_init) <- cliEntryPoint fname function

  ctx_ty <- contextType

  headerDecl
    EntryDecl
    [C.cedecl|int $id:entry_point_function_name
                                     ($ty:ctx_ty *ctx,
                                      $params:entry_point_output_params,
                                      $params:entry_point_input_params);|]

  let critical =
        [C.citems|
         $items:unpack_entry_inputs

         int ret = $id:(funName fname)(ctx, $args:out_args, $args:in_args);

         if (ret == 0) {
           $items:pack_entry_outputs
         }
        |]

  ops <- asks envOperations

  return
    ( [C.cedecl|
       int $id:entry_point_function_name
           ($ty:ctx_ty *ctx,
            $params:entry_point_output_params,
            $params:entry_point_input_params) {
         $items:inputdecls
         $items:outputdecls

         $items:(criticalSection ops critical)

         return ret;
       }|],
      cli_entry_point,
      cli_init
    )
  where
    stubParam (MemParam name space) =
      declMem name space
    stubParam (ScalarParam name ty) = do
      let ty' = primTypeToCType ty
      decl [C.cdecl|$ty:ty' $id:name;|]

--- CLI interface
--
-- Our strategy for CLI entry points is to parse everything into
-- host memory ('DefaultSpace') and copy the result into host memory
-- after the entry point has returned.  We have some ad-hoc frobbery
-- to copy the host-level memory blocks to another memory space if
-- necessary.  This will break if the Futhark entry point uses
-- non-trivial index functions for its input or output.
--
-- The idea here is to keep the nastyness in the wrapper, whilst not
-- messing up anything else.

printPrimStm :: (C.ToExp a, C.ToExp b) => a -> b -> PrimType -> Signedness -> C.Stm
printPrimStm dest val bt ept =
  [C.cstm|write_scalar($exp:dest, binary_output, &$exp:(primTypeInfo bt ept), &$exp:val);|]

-- | Return a statement printing the given external value.
printStm :: ExternalValue -> C.Exp -> CompilerM op s C.Stm
printStm (OpaqueValue desc _) _ =
  return [C.cstm|printf("#<opaque %s>", $string:desc);|]
printStm (TransparentValue (ScalarValue bt ept _)) e =
  return $ printPrimStm [C.cexp|stdout|] e bt ept
printStm (TransparentValue (ArrayValue _ _ bt ept shape)) e = do
  values_array <- publicName $ "values_" ++ name
  shape_array <- publicName $ "shape_" ++ name
  let num_elems = cproduct [[C.cexp|$id:shape_array(ctx, $exp:e)[$int:i]|] | i <- [0 .. rank -1]]
  return
    [C.cstm|{
      $ty:bt' *arr = calloc(sizeof($ty:bt'), $exp:num_elems);
      assert(arr != NULL);
      assert($id:values_array(ctx, $exp:e, arr) == 0);
      write_array(stdout, binary_output, &$exp:(primTypeInfo bt ept), arr,
                  $id:shape_array(ctx, $exp:e), $int:rank);
      free(arr);
    }|]
  where
    rank = length shape
    bt' = primTypeToCType bt
    name = arrayName bt ept rank

readPrimStm :: C.ToExp a => a -> Int -> PrimType -> Signedness -> C.Stm
readPrimStm place i t ept =
  [C.cstm|if (read_scalar(&$exp:(primTypeInfo t ept),&$exp:place) != 0) {
        futhark_panic(1, "Error when reading input #%d of type %s (errno: %s).\n",
              $int:i,
              $exp:(primTypeInfo t ept).type_name,
              strerror(errno));
      }|]

readInputs :: [ExternalValue] -> CompilerM op s [(C.Stm, C.Stm, C.Stm, C.Exp)]
readInputs = zipWithM readInput [0 ..]

readInput :: Int -> ExternalValue -> CompilerM op s (C.Stm, C.Stm, C.Stm, C.Exp)
readInput i (OpaqueValue desc _) = do
  stm [C.cstm|futhark_panic(1, "Cannot read input #%d of type %s\n", $int:i, $string:desc);|]
  return ([C.cstm|;|], [C.cstm|;|], [C.cstm|;|], [C.cexp|NULL|])
readInput i (TransparentValue (ScalarValue t ept _)) = do
  dest <- newVName "read_value"
  item [C.citem|$ty:(primTypeToCType t) $id:dest;|]
  stm $ readPrimStm dest i t ept
  return ([C.cstm|;|], [C.cstm|;|], [C.cstm|;|], [C.cexp|$id:dest|])
readInput i (TransparentValue vd@(ArrayValue _ _ t ept dims)) = do
  dest <- newVName "read_value"
  shape <- newVName "read_shape"
  arr <- newVName "read_arr"
  ty <- valueDescToCType vd
  item [C.citem|$ty:ty *$id:dest;|]

  let t' = signedPrimTypeToCType ept t
      rank = length dims
      name = arrayName t ept rank
      dims_exps = [[C.cexp|$id:shape[$int:j]|] | j <- [0 .. rank -1]]
      dims_s = concat $ replicate rank "[]"

  new_array <- publicName $ "new_" ++ name
  free_array <- publicName $ "free_" ++ name

  items
    [C.citems|
     typename int64_t $id:shape[$int:rank];
     $ty:t' *$id:arr = NULL;
     errno = 0;
     if (read_array(&$exp:(primTypeInfo t ept),
                    (void**) &$id:arr,
                    $id:shape,
                    $int:(length dims))
         != 0) {
       futhark_panic(1, "Cannot read input #%d of type %s%s (errno: %s).\n",
                 $int:i,
                 $string:dims_s,
                 $exp:(primTypeInfo t ept).type_name,
                 strerror(errno));
     }|]

  return
    ( [C.cstm|assert(($exp:dest = $id:new_array(ctx, $id:arr, $args:dims_exps)) != 0);|],
      [C.cstm|assert($id:free_array(ctx, $exp:dest) == 0);|],
      [C.cstm|free($id:arr);|],
      [C.cexp|$id:dest|]
    )

prepareOutputs :: [ExternalValue] -> CompilerM op s [(C.Exp, C.Stm)]
prepareOutputs = mapM prepareResult
  where
    prepareResult ev = do
      ty <- externalValueToCType ev
      result <- newVName "result"

      case ev of
        TransparentValue ScalarValue {} -> do
          item [C.citem|$ty:ty $id:result;|]
          return ([C.cexp|$id:result|], [C.cstm|;|])
        TransparentValue (ArrayValue _ _ t ept dims) -> do
          let name = arrayName t ept $ length dims
          free_array <- publicName $ "free_" ++ name
          item [C.citem|$ty:ty *$id:result;|]
          return
            ( [C.cexp|$id:result|],
              [C.cstm|assert($id:free_array(ctx, $exp:result) == 0);|]
            )
        OpaqueValue desc vds -> do
          free_opaque <- publicName $ "free_" ++ opaqueName desc vds
          item [C.citem|$ty:ty *$id:result;|]
          return
            ( [C.cexp|$id:result|],
              [C.cstm|assert($id:free_opaque(ctx, $exp:result) == 0);|]
            )

printResult :: [(ExternalValue, C.Exp)] -> CompilerM op s [C.Stm]
printResult vs = fmap concat $
  forM vs $ \(v, e) -> do
    p <- printStm v e
    return [p, [C.cstm|printf("\n");|]]

cliEntryPoint ::
  Name ->
  FunctionT a ->
  CompilerM op s (C.Definition, C.Initializer)
cliEntryPoint fname (Function _ _ _ _ results args) = do
  ((pack_input, free_input, free_parsed, input_args), input_items) <-
    collect' $ unzip4 <$> readInputs args

  ((output_vals, free_outputs), output_decls) <-
    collect' $ unzip <$> prepareOutputs results
  printstms <- printResult $ zip results output_vals

  ctx_ty <- contextType
  sync_ctx <- publicName "context_sync"
  error_ctx <- publicName "context_get_error"

  let entry_point_name = nameToString fname
      cli_entry_point_function_name = "futrts_cli_entry_" ++ entry_point_name
  entry_point_function_name <- publicName $ "entry_" ++ entry_point_name

  pause_profiling <- publicName "context_pause_profiling"
  unpause_profiling <- publicName "context_unpause_profiling"

  let run_it =
        [C.citems|
                  int r;
                  // Run the program once.
                  $stms:pack_input
                  if ($id:sync_ctx(ctx) != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  };
                  // Only profile last run.
                  if (profile_run) {
                    $id:unpause_profiling(ctx);
                  }
                  t_start = get_wall_time();
                  r = $id:entry_point_function_name(ctx,
                                                    $args:(map addrOf output_vals),
                                                    $args:input_args);
                  if (r != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  }
                  if ($id:sync_ctx(ctx) != 0) {
                    futhark_panic(1, "%s", $id:error_ctx(ctx));
                  };
                  if (profile_run) {
                    $id:pause_profiling(ctx);
                  }
                  t_end = get_wall_time();
                  long int elapsed_usec = t_end - t_start;
                  if (time_runs && runtime_file != NULL) {
                    fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
                    fflush(runtime_file);
                  }
                  $stms:free_input
                |]

  return
    ( [C.cedecl|static void $id:cli_entry_point_function_name($ty:ctx_ty *ctx) {
    typename int64_t t_start, t_end;
    int time_runs = 0, profile_run = 0;

    // We do not want to profile all the initialisation.
    $id:pause_profiling(ctx);

    // Declare and read input.
    set_binary_mode(stdin);
    $items:input_items

    if (end_of_input() != 0) {
      futhark_panic(1, "Expected EOF on stdin after reading input for %s.\n", $string:(quote (pretty fname)));
    }

    $items:output_decls

    // Warmup run
    if (perform_warmup) {
      $items:run_it
      $stms:free_outputs
    }
    time_runs = 1;
    // Proper run.
    for (int run = 0; run < num_runs; run++) {
      // Only profile last run.
      profile_run = run == num_runs -1;
      $items:run_it
      if (run < num_runs-1) {
        $stms:free_outputs
      }
    }

    // Free the parsed input.
    $stms:free_parsed

    // Print the final result.
    if (binary_output) {
      set_binary_mode(stdout);
    }
    $stms:printstms

    $stms:free_outputs
  }
                |],
      [C.cinit|{ .name = $string:entry_point_name,
                      .fun = $id:cli_entry_point_function_name }|]
    )

genericOptions :: [Option]
genericOptions =
  [ Option
      { optionLongName = "write-runtime-to",
        optionShortName = Just 't',
        optionArgument = RequiredArgument "FILE",
        optionDescription = "Print the time taken to execute the program to the indicated file, an integral number of microseconds.",
        optionAction = set_runtime_file
      },
    Option
      { optionLongName = "runs",
        optionShortName = Just 'r',
        optionArgument = RequiredArgument "INT",
        optionDescription = "Perform NUM runs of the program.\nWith -t, the runtime for each individual run will be printed. \nAdditionally, a single leading warmup run will be performed (not counted).\nOnly the final run will have its result written to stdout.",
        optionAction = set_num_runs
      },
    Option
      { optionLongName = "debugging",
        optionShortName = Just 'D',
        optionArgument = NoArgument,
        optionDescription = "Perform possibly expensive internal correctness checks and verbose logging. Implies -L.",
        optionAction = [C.cstm|futhark_context_config_set_debugging(cfg, 1);|]
      },
    Option
      { optionLongName = "log",
        optionShortName = Just 'L',
        optionArgument = NoArgument,
        optionDescription = "Print various low-overhead logging information to stderr while running.",
        optionAction = [C.cstm|futhark_context_config_set_logging(cfg, 1);|]
      },
    Option
      { optionLongName = "entry-point",
        optionShortName = Just 'e',
        optionArgument = RequiredArgument "NAME",
        optionDescription = "The entry point to run. Defaults to main.",
        optionAction = [C.cstm|if (entry_point != NULL) entry_point = optarg;|]
      },
    Option
      { optionLongName = "binary-output",
        optionShortName = Just 'b',
        optionArgument = NoArgument,
        optionDescription = "Print the program result in the binary output format. The default is human-readable text, which is very slow.",
        optionAction = [C.cstm|binary_output = 1;|]
      },
    Option
      { optionLongName = "help",
        optionShortName = Just 'h',
        optionArgument = NoArgument,
        optionDescription = "Print help information and exit.",
        optionAction =
          [C.cstm|{
                   printf("Usage: %s [OPTION]...\nOptions:\n\n%s", fut_progname, option_descriptions);
                   exit(0);
                  }|]
      }
  ]
  where
    set_runtime_file =
      [C.cstm|{
          runtime_file = fopen(optarg, "w");
          if (runtime_file == NULL) {
            futhark_panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
          }
        }|]
    set_num_runs =
      [C.cstm|{
          num_runs = atoi(optarg);
          perform_warmup = 1;
          if (num_runs <= 0) {
            futhark_panic(1, "Need a positive number of runs, not %s\n", optarg);
          }
        }|]

-- | The result of compilation to C is four parts, which can be put
-- together in various ways.  The obvious way is to concatenate all of
-- them, which yields a CLI program.  Another is to compile the
-- library part by itself, and use the header file to call into it.
data CParts = CParts
  { cHeader :: String,
    -- | Utility definitions that must be visible
    -- to both CLI and library parts.
    cUtils :: String,
    cCLI :: String,
    cLib :: String
  }

-- We may generate variables that are never used (e.g. for
-- certificates) or functions that are never called (e.g. unused
-- intrinsics), and generated code may have other cosmetic issues that
-- compilers warn about.  We disable these warnings to not clutter the
-- compilation logs.
disableWarnings :: String
disableWarnings =
  pretty
    [C.cunit|
$esc:("#ifdef __GNUC__")
$esc:("#pragma GCC diagnostic ignored \"-Wunused-function\"")
$esc:("#pragma GCC diagnostic ignored \"-Wunused-variable\"")
$esc:("#pragma GCC diagnostic ignored \"-Wparentheses\"")
$esc:("#pragma GCC diagnostic ignored \"-Wunused-label\"")
$esc:("#endif")

$esc:("#ifdef __clang__")
$esc:("#pragma clang diagnostic ignored \"-Wunused-function\"")
$esc:("#pragma clang diagnostic ignored \"-Wunused-variable\"")
$esc:("#pragma clang diagnostic ignored \"-Wparentheses\"")
$esc:("#pragma clang diagnostic ignored \"-Wunused-label\"")
$esc:("#endif")
|]

-- | Produce header and implementation files.
asLibrary :: CParts -> (String, String)
asLibrary parts =
  ( "#pragma once\n\n" <> cHeader parts,
    disableWarnings <> cHeader parts <> cUtils parts <> cLib parts
  )

-- | As executable with command-line interface.
asExecutable :: CParts -> String
asExecutable (CParts a b c d) = disableWarnings <> a <> b <> c <> d

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg ::
  MonadFreshNames m =>
  String ->
  Operations op () ->
  CompilerM op () () ->
  String ->
  [Space] ->
  [Option] ->
  Definitions op ->
  m CParts
compileProg backend ops extra header_extra spaces options prog = do
  src <- getNameSource
  let ((prototypes, definitions, entry_points), endstate) =
        runCompilerM ops src () compileProg'
      (entry_point_decls, cli_entry_point_decls, entry_point_inits) =
        unzip3 entry_points
      option_parser = generateOptionParser "parse_options" $ genericOptions ++ options

  let headerdefs =
        [C.cunit|
$esc:("// Headers\n")
$esc:("#include <stdint.h>")
$esc:("#include <stddef.h>")
$esc:("#include <stdbool.h>")
$esc:(header_extra)

$esc:("\n// Initialisation\n")
$edecls:(initDecls endstate)

$esc:("\n// Arrays\n")
$edecls:(arrayDecls endstate)

$esc:("\n// Opaque values\n")
$edecls:(opaqueDecls endstate)

$esc:("\n// Entry points\n")
$edecls:(entryDecls endstate)

$esc:("\n// Miscellaneous\n")
$edecls:(miscDecls endstate)
$esc:("#define FUTHARK_BACKEND_"++backend)
                           |]

  let utildefs =
        [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <stdbool.h>")
$esc:("#include <math.h>")
$esc:("#include <stdint.h>")
// If NDEBUG is set, the assert() macro will do nothing. Since Futhark
// (unfortunately) makes use of assert() for error detection (and even some
// side effects), we want to avoid that.
$esc:("#undef NDEBUG")
$esc:("#include <assert.h>")
$esc:("#include <stdarg.h>")

$esc:util_h

$esc:timing_h
|]

  let clidefs =
        [C.cunit|
$esc:("#include <string.h>")
$esc:("#include <inttypes.h>")
$esc:("#include <errno.h>")
$esc:("#include <ctype.h>")
$esc:("#include <errno.h>")
$esc:("#include <getopt.h>")

$esc:values_h

$esc:("#define __private")

static int binary_output = 0;
static typename FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
// If the entry point is NULL, the program will terminate after doing initialisation and such.
static const char *entry_point = "main";

$esc:tuning_h

$func:option_parser

$edecls:cli_entry_point_decls

typedef void entry_point_fun(struct futhark_context*);

struct entry_point_entry {
  const char *name;
  entry_point_fun *fun;
};

int main(int argc, char** argv) {
  fut_progname = argv[0];

  struct entry_point_entry entry_points[] = {
    $inits:entry_point_inits
  };

  struct futhark_context_config *cfg = futhark_context_config_new();
  assert(cfg != NULL);

  int parsed_options = parse_options(cfg, argc, argv);
  argc -= parsed_options;
  argv += parsed_options;

  if (argc != 0) {
    futhark_panic(1, "Excess non-option: %s\n", argv[0]);
  }

  struct futhark_context *ctx = futhark_context_new(cfg);
  assert (ctx != NULL);

  char* error = futhark_context_get_error(ctx);
  if (error != NULL) {
    futhark_panic(1, "%s", error);
  }

  if (entry_point != NULL) {
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    for (int i = 0; i < num_entry_points; i++) {
      if (strcmp(entry_points[i].name, entry_point) == 0) {
        entry_point_fun = entry_points[i].fun;
        break;
      }
    }

    if (entry_point_fun == NULL) {
      fprintf(stderr, "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                      entry_point);
      for (int i = 0; i < num_entry_points; i++) {
        fprintf(stderr, "%s\n", entry_points[i].name);
      }
      return 1;
    }

    entry_point_fun(ctx);

    if (runtime_file != NULL) {
      fclose(runtime_file);
    }

    char *report = futhark_context_report(ctx);
    fputs(report, stderr);
    free(report);
  }

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return 0;
}
                        |]

  let early_decls = DL.toList $ compEarlyDecls endstate
  let lib_decls = DL.toList $ compLibDecls endstate
  let libdefs =
        [C.cunit|
$esc:("#ifdef _MSC_VER\n#define inline __inline\n#endif")
$esc:("#include <string.h>")
$esc:("#include <inttypes.h>")
$esc:("#include <ctype.h>")
$esc:("#include <errno.h>")
$esc:("#include <assert.h>")

$esc:(header_extra)

$esc:lock_h

$edecls:builtin

$edecls:early_decls

$edecls:prototypes

$edecls:lib_decls

$edecls:(map funcToDef definitions)

$edecls:(arrayDefinitions endstate)

$edecls:(opaqueDefinitions endstate)

$edecls:entry_point_decls
  |]

  return $ CParts (pretty headerdefs) (pretty utildefs) (pretty clidefs) (pretty libdefs)
  where
    compileProg' = do
      let Definitions consts (Functions funs) = prog

      (memstructs, memfuns, memreport) <- unzip3 <$> mapM defineMemorySpace spaces

      get_consts <- compileConstants consts

      ctx_ty <- contextType

      (prototypes, definitions) <-
        unzip <$> mapM (compileFun get_consts [[C.cparam|$ty:ctx_ty *ctx|]]) funs

      mapM_ earlyDecl memstructs
      entry_points <-
        mapM (uncurry onEntryPoint) $ filter (functionEntry . snd) funs

      extra

      mapM_ earlyDecl $ concat memfuns

      commonLibFuns memreport

      return (prototypes, definitions, entry_points)

    funcToDef func = C.FuncDef func loc
      where
        loc = case func of
          C.OldFunc _ _ _ _ _ _ l -> l
          C.Func _ _ _ _ _ l -> l

    builtin =
      cIntOps ++ cFloat32Ops ++ cFloat64Ops ++ cFloatConvOps
        ++ cFloat32Funs
        ++ cFloat64Funs

    util_h = $(embedStringFile "rts/c/util.h")
    values_h = $(embedStringFile "rts/c/values.h")
    timing_h = $(embedStringFile "rts/c/timing.h")
    lock_h = $(embedStringFile "rts/c/lock.h")
    tuning_h = $(embedStringFile "rts/c/tuning.h")

commonLibFuns :: [C.BlockItem] -> CompilerM op s ()
commonLibFuns memreport = do
  ctx <- contextType
  profilereport <- gets $ DL.toList . compProfileItems

  publicDef_ "context_report" MiscDecl $ \s ->
    ( [C.cedecl|char* $id:s($ty:ctx *ctx);|],
      [C.cedecl|char* $id:s($ty:ctx *ctx) {
                 struct str_builder builder;
                 str_builder_init(&builder);
                 if (ctx->detail_memory || ctx->profiling) {
                   $items:memreport
                 }
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

compileConstants :: Constants op -> CompilerM op s [C.BlockItem]
compileConstants (Constants ps init_consts) = do
  ctx_ty <- contextType
  const_fields <- mapM constParamField ps
  -- Avoid an empty struct, as that is apparently undefined behaviour.
  let const_fields'
        | null const_fields = [[C.csdecl|int dummy;|]]
        | otherwise = const_fields
  contextField "constants" [C.cty|struct { $sdecls:const_fields' }|] Nothing
  earlyDecl [C.cedecl|int init_constants($ty:ctx_ty*);|]
  earlyDecl [C.cedecl|int free_constants($ty:ctx_ty*);|]

  -- We locally define macros for the constants, so that when we
  -- generate assignments to local variables, we actually assign into
  -- the constants struct.  This is not needed for functions, because
  -- they can only read constants, not write them.
  let (defs, undefs) = unzip $ map constMacro ps
  init_consts' <- blockScope $ do
    mapM_ resetMemConst ps
    compileCode init_consts
  libDecl
    [C.cedecl|int init_constants($ty:ctx_ty *ctx) {
      (void)ctx;
      int err = 0;
      $items:defs
      $items:init_consts'
      $items:undefs
      cleanup:
      return err;
    }|]

  free_consts <- collect $ mapM_ freeConst ps
  libDecl
    [C.cedecl|int free_constants($ty:ctx_ty *ctx) {
      (void)ctx;
      $items:free_consts
      return 0;
    }|]

  mapM getConst ps
  where
    constParamField (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      return [C.csdecl|$ty:ctp $id:name;|]
    constParamField (MemParam name space) = do
      ty <- memToCType name space
      return [C.csdecl|$ty:ty $id:name;|]

    constMacro p = ([C.citem|$escstm:def|], [C.citem|$escstm:undef|])
      where
        p' = pretty (C.toIdent (paramName p) mempty)
        def = "#define " ++ p' ++ " (" ++ "ctx->constants." ++ p' ++ ")"
        undef = "#undef " ++ p'

    resetMemConst ScalarParam {} = return ()
    resetMemConst (MemParam name space) = resetMem name space

    freeConst ScalarParam {} = return ()
    freeConst (MemParam name space) = unRefMem [C.cexp|ctx->constants.$id:name|] space

    getConst (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      return [C.citem|$ty:ctp $id:name = ctx->constants.$id:name;|]
    getConst (MemParam name space) = do
      ty <- memToCType name space
      return [C.citem|$ty:ty $id:name = ctx->constants.$id:name;|]

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
    return (mem, size)

  let lexMem env =
        env
          { envCachedMem =
              M.fromList (map (first (`C.toExp` noLoc)) cached')
                <> envCachedMem env
          }

      declCached (mem, size) =
        [ [C.citem|size_t $id:size = 0;|],
          [C.citem|$ty:defaultMemBlockType $id:mem = NULL;|]
        ]

      freeCached (mem, _) =
        [C.cstm|free($id:mem);|]

  local lexMem $ f (concatMap declCached cached') (map freeCached cached')

compileFun :: [C.BlockItem] -> [C.Param] -> (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileFun get_constants extra (fname, func@(Function _ outputs inputs body _ _)) = do
  (outparams, out_ptrs) <- unzip <$> mapM compileOutput outputs
  inparams <- mapM compileInput inputs

  cachingMemory (lexicalMemoryUsage func) $ \decl_cached free_cached -> do
    body' <- blockScope $ compileFunBody out_ptrs outputs body

    return
      ( [C.cedecl|static int $id:(funName fname)($params:extra, $params:outparams, $params:inparams);|],
        [C.cfun|static int $id:(funName fname)($params:extra, $params:outparams, $params:inparams) {
               $stms:ignores
               int err = 0;
               $items:decl_cached
               $items:get_constants
               $items:body'
              cleanup:
               {}
               $stms:free_cached
               return err;
  }|]
      )
  where
    -- Ignore all the boilerplate parameters, just in case we don't
    -- actually need to use them.
    ignores = [[C.cstm|(void)$id:p;|] | C.Param (Just p) _ _ _ <- extra]

    compileInput (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      return [C.cparam|$ty:ctp $id:name|]
    compileInput (MemParam name space) = do
      ty <- memToCType name space
      return [C.cparam|$ty:ty $id:name|]

    compileOutput (ScalarParam name bt) = do
      let ctp = primTypeToCType bt
      p_name <- newVName $ "out_" ++ baseString name
      return ([C.cparam|$ty:ctp *$id:p_name|], [C.cexp|$id:p_name|])
    compileOutput (MemParam name space) = do
      ty <- memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      return ([C.cparam|$ty:ty *$id:p_name|], [C.cexp|$id:p_name|])

compilePrimValue :: PrimValue -> C.Exp
compilePrimValue (IntValue (Int8Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int16Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int32Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int64Value k)) = [C.cexp|$int:k|]
compilePrimValue (FloatValue (Float64Value x))
  | isInfinite x =
    if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
  | isNaN x =
    [C.cexp|NAN|]
  | otherwise =
    [C.cexp|$double:x|]
compilePrimValue (FloatValue (Float32Value x))
  | isInfinite x =
    if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
  | isNaN x =
    [C.cexp|NAN|]
  | otherwise =
    [C.cexp|$float:x|]
compilePrimValue (BoolValue b) =
  [C.cexp|$int:b'|]
  where
    b' :: Int
    b' = if b then 1 else 0
compilePrimValue Checked =
  [C.cexp|0|]

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
  return $ derefPointer dest i [C.cty|$tyquals:quals' $ty:elemtype*|]

compileExpToName :: String -> PrimType -> Exp -> CompilerM op s VName
compileExpToName _ _ (LeafExp (ScalarVar v) _) =
  return v
compileExpToName desc t e = do
  desc' <- newVName desc
  e' <- compileExp e
  decl [C.cdecl|$ty:(primTypeToCType t) $id:desc' = $e';|]
  return desc'

compileExp :: Exp -> CompilerM op s C.Exp
compileExp = compilePrimExp compileLeaf
  where
    compileLeaf (ScalarVar src) =
      return [C.cexp|$id:src|]
    compileLeaf (Index src (Count iexp) restype DefaultSpace vol) = do
      src' <- rawMem src
      derefPointer src'
        <$> compileExp (untyped iexp)
        <*> pure [C.cty|$tyquals:(volQuals vol) $ty:(primTypeToCType restype)*|]
    compileLeaf (Index src (Count iexp) restype (Space space) vol) =
      join $
        asks envReadScalar
          <*> rawMem src
          <*> compileExp (untyped iexp)
          <*> pure (primTypeToCType restype)
          <*> pure space
          <*> pure vol
    compileLeaf (Index src (Count iexp) _ ScalarSpace {} _) = do
      iexp' <- compileExp $ untyped iexp
      return [C.cexp|$id:src[$exp:iexp']|]
    compileLeaf (SizeOf t) =
      return [C.cexp|(typename int64_t)sizeof($ty:t')|]
      where
        t' = primTypeToCType t

-- | Tell me how to compile a @v@, and I'll Compile any @PrimExp v@ for you.
compilePrimExp :: Monad m => (v -> m C.Exp) -> PrimExp v -> m C.Exp
compilePrimExp _ (ValueExp val) =
  return $ compilePrimValue val
compilePrimExp f (LeafExp v _) =
  f v
compilePrimExp f (UnOpExp Complement {} x) = do
  x' <- compilePrimExp f x
  return [C.cexp|~$exp:x'|]
compilePrimExp f (UnOpExp Not {} x) = do
  x' <- compilePrimExp f x
  return [C.cexp|!$exp:x'|]
compilePrimExp f (UnOpExp Abs {} x) = do
  x' <- compilePrimExp f x
  return [C.cexp|abs($exp:x')|]
compilePrimExp f (UnOpExp (FAbs Float32) x) = do
  x' <- compilePrimExp f x
  return [C.cexp|(float)fabs($exp:x')|]
compilePrimExp f (UnOpExp (FAbs Float64) x) = do
  x' <- compilePrimExp f x
  return [C.cexp|fabs($exp:x')|]
compilePrimExp f (UnOpExp SSignum {} x) = do
  x' <- compilePrimExp f x
  return [C.cexp|($exp:x' > 0) - ($exp:x' < 0)|]
compilePrimExp f (UnOpExp USignum {} x) = do
  x' <- compilePrimExp f x
  return [C.cexp|($exp:x' > 0) - ($exp:x' < 0) != 0|]
compilePrimExp f (CmpOpExp cmp x y) = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  return $ case cmp of
    CmpEq {} -> [C.cexp|$exp:x' == $exp:y'|]
    FCmpLt {} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe {} -> [C.cexp|$exp:x' <= $exp:y'|]
    CmpLlt {} -> [C.cexp|$exp:x' < $exp:y'|]
    CmpLle {} -> [C.cexp|$exp:x' <= $exp:y'|]
    _ -> [C.cexp|$id:(pretty cmp)($exp:x', $exp:y')|]
compilePrimExp f (ConvOpExp conv x) = do
  x' <- compilePrimExp f x
  return [C.cexp|$id:(pretty conv)($exp:x')|]
compilePrimExp f (BinOpExp bop x y) = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  -- Note that integer addition, subtraction, and multiplication with
  -- OverflowWrap are not handled by explicit operators, but rather by
  -- functions.  This is because we want to implicitly convert them to
  -- unsigned numbers, so we can do overflow without invoking
  -- undefined behaviour.
  return $ case bop of
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
    Shl {} -> [C.cexp|$exp:x' << $exp:y'|]
    LogAnd {} -> [C.cexp|$exp:x' && $exp:y'|]
    LogOr {} -> [C.cexp|$exp:x' || $exp:y'|]
    _ -> [C.cexp|$id:(pretty bop)($exp:x', $exp:y')|]
compilePrimExp f (FunExp h args _) = do
  args' <- mapM (compilePrimExp f) args
  return [C.cexp|$id:(funName (nameFromString h))($args:args')|]

compileCode :: Code op -> CompilerM op s ()
compileCode (Op op) =
  join $ asks envOpCompiler <*> pure op
compileCode Skip = return ()
compileCode (Comment s code) = do
  xs <- blockScope $ compileCode code
  let comment = "// " ++ s
  stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCode (DebugPrint s (Just e)) = do
  e' <- compileExp e
  stm
    [C.cstm|if (ctx->debugging) {
          fprintf(stderr, $string:fmtstr, $exp:s, ($ty:ety)$exp:e', '\n');
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
          fprintf(stderr, "%s\n", $exp:s);
       }|]
compileCode c
  | Just (name, vol, t, e, c') <- declareAndSet c = do
    let ct = primTypeToCType t
    e' <- compileExp e
    item [C.citem|$tyquals:(volQuals vol) $ty:ct $id:name = $exp:e';|]
    compileCode c'
compileCode (c1 :>>: c2) = compileCode c1 >> compileCode c2
compileCode (Assert e msg (loc, locs)) = do
  e' <- compileExp e
  err <-
    collect $
      join $
        asks (opsError . envOperations) <*> pure msg <*> pure stacktrace
  stm [C.cstm|if (!$exp:e') { $items:err }|]
  where
    stacktrace = prettyStacktrace 0 $ map locStr $ loc : locs
compileCode (Allocate _ _ ScalarSpace {}) =
  -- Handled by the declaration of the memory block, which is
  -- translated to an actual array.
  return ()
compileCode (Allocate name (Count (TPrimExp e)) space) = do
  size <- compileExp e
  cached <- cacheMem name
  case cached of
    Just cur_size ->
      stm
        [C.cstm|if ($exp:cur_size < (size_t)$exp:size) {
                    $exp:name = realloc($exp:name, $exp:size);
                    $exp:cur_size = $exp:size;
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
  body' <- blockScope $ compileCode body
  stm
    [C.cstm|for ($ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileCode (While cond body) = do
  cond' <- compileExp $ untyped cond
  body' <- blockScope $ compileCode body
  stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp $ untyped cond
  tbranch' <- blockScope $ compileCode tbranch
  fbranch' <- blockScope $ compileCode fbranch
  stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCode (Copy dest (Count destoffset) DefaultSpace src (Count srcoffset) DefaultSpace (Count size)) =
  join $
    copyMemoryDefaultSpace
      <$> rawMem dest
      <*> compileExp (untyped destoffset)
      <*> rawMem src
      <*> compileExp (untyped srcoffset)
      <*> compileExp (untyped size)
compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) srcspace (Count size)) = do
  copy <- asks envCopy
  join $
    copy
      <$> rawMem dest
      <*> compileExp (untyped destoffset)
      <*> pure destspace
      <*> rawMem src
      <*> compileExp (untyped srcoffset)
      <*> pure srcspace
      <*> compileExp (untyped size)
compileCode (Write dest (Count idx) elemtype DefaultSpace vol elemexp) = do
  dest' <- rawMem dest
  deref <-
    derefPointer dest'
      <$> compileExp (untyped idx)
      <*> pure [C.cty|$tyquals:(volQuals vol) $ty:(primTypeToCType elemtype)*|]
  elemexp' <- compileExp elemexp
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
      <*> pure (primTypeToCType elemtype)
      <*> pure space
      <*> pure vol
      <*> compileExp elemexp
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
      let vs'' = [[C.cinit|$exp:(compilePrimValue v)|] | v <- vs']
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Fake a memory block.
  contextField
    (C.toIdent name noLoc)
    [C.cty|struct memblock|]
    $ Just [C.cexp|(struct memblock){NULL, (char*)$id:name_realtype, 0}|]
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
compileCode (SetScalar dest (BinOpExp op (LeafExp (ScalarVar x) _) y))
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
    compileArg (MemArg m) = return [C.cexp|$exp:m|]
    compileArg (ExpArg e) = compileExp e

blockScope :: CompilerM op s () -> CompilerM op s [C.BlockItem]
blockScope = fmap snd . blockScope'

blockScope' :: CompilerM op s a -> CompilerM op s (a, [C.BlockItem])
blockScope' m = do
  old_allocs <- gets compDeclaredMem
  (x, xs) <- pass $ do
    (x, w) <- listen m
    let xs = DL.toList $ accItems w
    return ((x, xs), const mempty)
  new_allocs <- gets $ filter (`notElem` old_allocs) . compDeclaredMem
  modify $ \s -> s {compDeclaredMem = old_allocs}
  releases <- collect $ mapM_ (uncurry unRefMem) new_allocs
  return (x, xs <> releases)

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

declareAndSet :: Code op -> Maybe (VName, Volatility, PrimType, Exp, Code op)
declareAndSet code = do
  (DeclareScalar name vol t, code') <- nextCode code
  (SetScalar dest e, code'') <- nextCode code'
  guard $ name == dest
  Just (name, vol, t, e, code'')

nextCode :: Code op -> Maybe (Code op, Code op)
nextCode (x :>>: y)
  | Just (x_a, x_b) <- nextCode x =
    Just (x_a, x_b <> y)
  | otherwise =
    Just (x, y)
nextCode _ = Nothing

assignmentOperator :: BinOp -> Maybe (VName -> C.Exp -> C.Exp)
assignmentOperator Add {} = Just $ \d e -> [C.cexp|$id:d += $exp:e|]
assignmentOperator Sub {} = Just $ \d e -> [C.cexp|$id:d -= $exp:e|]
assignmentOperator Mul {} = Just $ \d e -> [C.cexp|$id:d *= $exp:e|]
assignmentOperator _ = Nothing

-- | Return an expression multiplying together the given expressions.
-- If an empty list is given, the expression @1@ is returned.
cproduct :: [C.Exp] -> C.Exp
cproduct [] = [C.cexp|1|]
cproduct (e : es) = foldl mult e es
  where
    mult x y = [C.cexp|$exp:x * $exp:y|]
