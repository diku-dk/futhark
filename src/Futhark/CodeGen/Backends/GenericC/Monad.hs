{-# LANGUAGE QuasiQuotes #-}

-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC.Monad
  ( -- * Pluggable compiler
    Operations (..),
    Publicness (..),
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
    DoCopy,

    -- * Monadic compiler interface
    CompilerM,
    CompilerState (..),
    CompilerEnv (..),
    getUserState,
    modifyUserState,
    generateProgramStruct,
    runCompilerM,
    inNewFunction,
    cachingMemory,
    volQuals,
    rawMem,
    item,
    items,
    stm,
    stms,
    decl,
    headerDecl,
    publicDef,
    publicDef_,
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
    freeRawMem,
    allocRawMem,
    fatMemType,
    declAllocatedMem,
    freeAllocatedMem,
    collect,
    collect',
    contextType,
    configType,

    -- * Building Blocks
    copyMemoryDefaultSpace,
    derefPointer,
    setMem,
    allocMem,
    unRefMem,
    declMem,
    resetMem,
    fatMemAlloc,
    fatMemSet,
    fatMemUnRef,
    criticalSection,
    module Futhark.CodeGen.Backends.SimpleRep,
  )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (first)
import Data.DList qualified as DL
import Data.List (unzip4)
import Data.Loc
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.SimpleRep
import Futhark.CodeGen.ImpCode
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

-- How public an array type definition sould be.  Public types show up
-- in the generated API, while private types are used only to
-- implement the members of opaques.
data Publicness = Private | Public
  deriving (Eq, Ord, Show)

type ArrayType = (Signedness, PrimType, Int)

data CompilerState s = CompilerState
  { compArrayTypes :: M.Map ArrayType Publicness,
    compEarlyDecls :: DL.DList C.Definition,
    compNameSrc :: VNameSource,
    compUserState :: s,
    compHeaderDecls :: M.Map HeaderSection (DL.DList C.Definition),
    compLibDecls :: DL.DList C.Definition,
    compCtxFields :: DL.DList (C.Id, C.Type, Maybe C.Exp, Maybe (C.Stm, C.Stm)),
    compClearItems :: DL.DList C.BlockItem,
    compDeclaredMem :: [(VName, Space)],
    compItems :: DL.DList C.BlockItem
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s =
  CompilerState
    { compArrayTypes = mempty,
      compEarlyDecls = mempty,
      compNameSrc = src,
      compUserState = s,
      compHeaderDecls = mempty,
      compLibDecls = mempty,
      compCtxFields = mempty,
      compClearItems = mempty,
      compDeclaredMem = mempty,
      compItems = mempty
    }

-- | In which part of the header file we put the declaration.  This is
-- to ensure that the header file remains structured and readable.
data HeaderSection
  = ArrayDecl Name
  | OpaqueTypeDecl Name
  | OpaqueDecl Name
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
type PointerQuals = String -> [C.TypeQual]

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

-- | De-allocate the given memory block, with the given tag, with the
-- given size,, which is in the given memory space.
type Deallocate op s = C.Exp -> C.Exp -> C.Exp -> SpaceId -> CompilerM op s ()

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

-- | Perform an 'Copy'.  It is expected that these functions are
-- each specialised on which spaces they operate on, so that is not part of their arguments.
type DoCopy op s =
  CopyBarrier ->
  PrimType ->
  [Count Elements C.Exp] ->
  C.Exp ->
  ( Count Elements C.Exp,
    [Count Elements C.Exp]
  ) ->
  C.Exp ->
  ( Count Elements C.Exp,
    [Count Elements C.Exp]
  ) ->
  CompilerM op s ()

-- | Call a function.
type CallCompiler op s = [VName] -> Name -> [C.Exp] -> CompilerM op s ()

data Operations op s = Operations
  { opsWriteScalar :: WriteScalar op s,
    opsReadScalar :: ReadScalar op s,
    opsAllocate :: Allocate op s,
    opsDeallocate :: Deallocate op s,
    opsCopy :: Copy op s,
    opsMemoryType :: MemoryType op s,
    opsCompiler :: OpCompiler op s,
    opsError :: ErrorCompiler op s,
    opsCall :: CallCompiler op s,
    -- | @(dst,src)@-space mapping to copy functions.
    opsCopies :: M.Map (Space, Space) (DoCopy op s),
    -- | If true, use reference counting.  Otherwise, bare
    -- pointers.
    opsFatMemory :: Bool,
    -- | Code to bracket critical sections.
    opsCritical :: ([C.BlockItem], [C.BlockItem])
  }

freeAllocatedMem :: CompilerM op s [C.BlockItem]
freeAllocatedMem = collect $ mapM_ (uncurry unRefMem) =<< gets compDeclaredMem

declAllocatedMem :: CompilerM op s [C.BlockItem]
declAllocatedMem = collect $ mapM_ f =<< gets compDeclaredMem
  where
    f (name, space) = do
      ty <- memToCType name space
      decl [C.cdecl|$ty:ty $id:name;|]
      resetMem name space

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

contextContents :: CompilerM op s ([C.FieldGroup], [C.Stm], [C.Stm])
contextContents = do
  (field_names, field_types, field_values, field_frees) <-
    gets $ unzip4 . DL.toList . compCtxFields
  let fields =
        [ [C.csdecl|$ty:ty $id:name;|]
          | (name, ty) <- zip field_names field_types
        ]
      init_fields =
        [ [C.cstm|ctx->program->$id:name = $exp:e;|]
          | (name, Just e) <- zip field_names field_values
        ]
      (setup, free) = unzip $ catMaybes field_frees
  pure (fields, init_fields <> setup, free)

generateProgramStruct :: CompilerM op s ()
generateProgramStruct = do
  (fields, init_fields, free_fields) <- contextContents
  mapM_
    earlyDecl
    [C.cunit|struct program {
               $sdecls:fields
             };
             static void setup_program(struct futhark_context* ctx) {
               (void)ctx;
               int error = 0;
               (void)error;
               ctx->program = malloc(sizeof(struct program));
               $stms:init_fields
             }
             static void teardown_program(struct futhark_context *ctx) {
               (void)ctx;
               int error = 0;
               (void)error;
               $stms:free_fields
               free(ctx->program);
             }|]

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
fatMemory _ = asks $ opsFatMemory . envOperations

cacheMem :: (C.ToExp a) => a -> CompilerM op s (Maybe VName)
cacheMem a = asks $ M.lookup (C.toExp a noLoc) . envCachedMem

-- | Construct a publicly visible definition using the specified name
-- as the template.  The first returned definition is put in the
-- header file, and the second is the implementation.  Returns the public
-- name.
publicDef ::
  T.Text ->
  HeaderSection ->
  (T.Text -> (C.Definition, C.Definition)) ->
  CompilerM op s T.Text
publicDef s h f = do
  s' <- publicName s
  let (pub, priv) = f s'
  headerDecl h pub
  earlyDecl priv
  pure s'

-- | As 'publicDef', but ignores the public name.
publicDef_ ::
  T.Text ->
  HeaderSection ->
  (T.Text -> (C.Definition, C.Definition)) ->
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

contextFieldDyn :: C.Id -> C.Type -> C.Stm -> C.Stm -> CompilerM op s ()
contextFieldDyn name ty create free = modify $ \s ->
  s {compCtxFields = compCtxFields s <> DL.singleton (name, ty, Nothing, Just (create, free))}

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
publicName :: T.Text -> CompilerM op s T.Text
publicName s = pure $ "futhark_" <> s

memToCType :: VName -> Space -> CompilerM op s C.Type
memToCType v space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem v
  if refcount && not cached
    then pure $ fatMemType space
    else rawMemCType space

rawMemCType :: Space -> CompilerM op s C.Type
rawMemCType DefaultSpace = pure defaultMemBlockType
rawMemCType (Space sid) = join $ asks (opsMemoryType . envOperations) <*> pure sid
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
    fat = asks ((&&) . opsFatMemory . envOperations) <*> (isNothing <$> cacheMem v)

rawMem' :: (C.ToExp a) => Bool -> a -> C.Exp
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
      asks (opsAllocate . envOperations)
        <*> pure [C.cexp|$exp:dest|]
        <*> pure [C.cexp|$exp:size|]
        <*> pure [C.cexp|$exp:desc|]
        <*> pure sid
  _ ->
    stm
      [C.cstm|host_alloc(ctx, (size_t)$exp:size, $exp:desc, (size_t*)&$exp:size, (void*)&$exp:dest);|]

freeRawMem ::
  (C.ToExp a, C.ToExp b, C.ToExp c) =>
  a ->
  b ->
  Space ->
  c ->
  CompilerM op s ()
freeRawMem mem size space desc =
  case space of
    Space sid -> do
      free_mem <- asks (opsDeallocate . envOperations)
      free_mem [C.cexp|$exp:mem|] [C.cexp|$exp:size|] [C.cexp|$exp:desc|] sid
    _ ->
      item
        [C.citem|host_free(ctx, (size_t)$exp:size, $exp:desc, (void*)$exp:mem);|]

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

resetMem :: (C.ToExp a) => a -> Space -> CompilerM op s ()
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
  let src_s = T.unpack $ expText $ C.toExp src noLoc
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

unRefMem :: (C.ToExp a) => a -> Space -> CompilerM op s ()
unRefMem mem space = do
  refcount <- fatMemory space
  cached <- isJust <$> cacheMem mem
  let mem_s = T.unpack $ expText $ C.toExp mem noLoc
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
  let mem_s = T.unpack $ expText $ C.toExp mem noLoc
  if refcount
    then
      stm
        [C.cstm|if ($id:(fatMemAlloc space)(ctx, &$exp:mem, $exp:size,
                                                 $string:mem_s)) {
                       $stm:on_failure
                     }|]
    else do
      freeRawMem mem size space mem_s
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

cachingMemory ::
  M.Map VName Space ->
  ([C.BlockItem] -> [C.Stm] -> CompilerM op s a) ->
  CompilerM op s a
cachingMemory lexical f = do
  -- We only consider lexical 'DefaultSpace' memory blocks to be
  -- cached.  This is not a deep technical restriction, but merely a
  -- heuristic based on GPU memory usually involving larger
  -- allocations, that do not suffer from the overhead of reference
  -- counting.  Beware: there is code elsewhere in codegen that
  -- assumes lexical memory is DefaultSpace too.
  let cached = M.keys $ M.filter (== DefaultSpace) lexical

  cached' <- forM cached $ \mem -> do
    size <- newVName $ prettyString mem <> "_cached_size"
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

derefPointer :: C.Exp -> C.Exp -> C.Type -> C.Exp
derefPointer ptr i res_t =
  [C.cexp|(($ty:res_t)$exp:ptr)[$exp:i]|]

volQuals :: Volatility -> [C.TypeQual]
volQuals Volatile = [C.ctyquals|volatile|]
volQuals Nonvolatile = []

writeScalarPointerWithQuals :: PointerQuals -> WriteScalar op s
writeScalarPointerWithQuals quals_f dest i elemtype space vol v = do
  let quals' = volQuals vol ++ quals_f space
      deref = derefPointer dest i [C.cty|$tyquals:quals' $ty:elemtype*|]
  stm [C.cstm|$exp:deref = $exp:v;|]

readScalarPointerWithQuals :: PointerQuals -> ReadScalar op s
readScalarPointerWithQuals quals_f dest i elemtype space vol = do
  let quals' = volQuals vol ++ quals_f space
  pure $ derefPointer dest i [C.cty|$tyquals:quals' $ty:elemtype*|]

criticalSection :: Operations op s -> [C.BlockItem] -> [C.BlockItem]
criticalSection ops x =
  [C.citems|lock_lock(&ctx->lock);
            $items:(fst (opsCritical ops))
            $items:x
            $items:(snd (opsCritical ops))
            lock_unlock(&ctx->lock);
           |]

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
