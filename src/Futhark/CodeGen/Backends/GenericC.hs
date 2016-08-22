{-# LANGUAGE QuasiQuotes, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | C code generator framework.
module Futhark.CodeGen.Backends.GenericC
  ( compileProg
  -- * Pluggable compiler
  , Operations (..)
  , defaultOperations
  , OpCompiler

  , PointerQuals
  , MemoryType
  , WriteScalar
  , writeScalarPointerWithQuals
  , ReadScalar
  , readScalarPointerWithQuals
  , Allocate
  , Deallocate
  , Copy
  -- * Monadic compiler interface
  , CompilerM
  , CompilerState (compUserState)
  , getUserState
  , putUserState
  , modifyUserState
  , runCompilerM
  , blockScope
  , compileFun
  , compileCode
  , compileExp
  , compileExpToName
  , dimSizeToExp
  , rawMem
  , item
  , stm
  , stms
  , decl
  -- * Building Blocks
  , primTypeToCType
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import qualified Data.HashMap.Lazy as HM
import qualified Data.DList as DL
import Data.List
import Data.Maybe
import Data.FileEmbed

import Prelude

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.OpenCL as C

import Futhark.CodeGen.ImpCode hiding (dimSizeToExp)
import Futhark.MonadFreshNames
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.Backends.GenericC.Options
import Futhark.Util.Pretty hiding (space, spaces)
import Futhark.Util (zEncodeString)
import Futhark.Representation.AST.Attributes (builtInFunctions)

data CompilerState s = CompilerState {
    compTypeStructs :: [([Type], (C.Type, C.Definition))]
  , compVarDefinitions :: [C.Definition]
  , compInit :: [C.Stm]
  , compNameSrc :: VNameSource
  , compUserState :: s
  }

newCompilerState :: VNameSource -> s -> CompilerState s
newCompilerState src s = CompilerState { compTypeStructs = []
                                       , compVarDefinitions = []
                                       , compInit = []
                                       , compNameSrc = src
                                       , compUserState = s
                                       }

-- | A substitute expression compiler, tried before the main
-- compilation function.
type OpCompiler op s = op -> CompilerM op s ()

-- | The address space qualifiers for a pointer of the given type with
-- the given annotation.
type PointerQuals op s = String -> CompilerM op s [C.TypeQual]

-- | The type of a memory block in the given memory space.
type MemoryType op s = SpaceId -> CompilerM op s C.Type

-- | Write a scalar to the given memory block with the given index and
-- in the given memory space.
type WriteScalar op s = C.Exp -> C.Exp -> C.Type -> SpaceId -> C.Exp
                        -> CompilerM op s ()

-- | Read a scalar from the given memory block with the given index and
-- in the given memory space.
type ReadScalar op s = C.Exp -> C.Exp -> C.Type -> SpaceId
                       -> CompilerM op s C.Exp

-- | Allocate a memory block of the given size in the given memory
-- space, saving a reference in the given variable name.
type Allocate op s = C.Exp -> C.Exp -> SpaceId
                     -> CompilerM op s ()

-- | De-allocate the given memory block which is in the given memory
-- space.
type Deallocate op s = C.Exp -> SpaceId -> CompilerM op s ()


-- | Copy from one memory block to another.
type Copy op s = C.Exp -> C.Exp -> Space ->
                 C.Exp -> C.Exp -> Space ->
                 C.Exp ->
                 CompilerM op s ()

data Operations op s =
  Operations { opsWriteScalar :: WriteScalar op s
             , opsReadScalar :: ReadScalar op s
             , opsAllocate :: Allocate op s
             , opsDeallocate :: Deallocate op s
             , opsCopy :: Copy op s

             , opsMemoryType :: MemoryType op s
             , opsCompiler :: OpCompiler op s

             , opsFatMemory :: Bool
               -- ^ If true, use reference counting.  Otherwise, bare
               -- pointers.
             }

-- | A set of operations that fail for every operation involving
-- non-default memory spaces.  Uses plain pointers and @malloc@ for
-- memory management.
defaultOperations :: Operations op s
defaultOperations = Operations { opsWriteScalar = defWriteScalar
                               , opsReadScalar = defReadScalar
                               , opsAllocate  = defAllocate
                               , opsDeallocate  = defDeallocate
                               , opsCopy = defCopy
                               , opsMemoryType = defMemoryType
                               , opsCompiler = defCompiler
                               , opsFatMemory = True
                               }
  where defWriteScalar _ _ _ _ _ =
          fail "Cannot write to non-default memory space because I am dumb"
        defReadScalar _ _ _ _ =
          fail "Cannot read from non-default memory space"
        defAllocate _ _ _ =
          fail "Cannot allocate in non-default memory space"
        defDeallocate _ _ =
          fail "Cannot deallocate in non-default memory space"
        defCopy _ _ _ _ _ _ _ =
          fail "Cannot copy to or from non-default memory space"
        defMemoryType _ =
          fail "Has no type for non-default memory space"
        defCompiler _ =
          fail "The default compiler cannot compile extended operations"

data CompilerEnv op s = CompilerEnv {
    envOperations :: Operations op s
  , envFtable     :: HM.HashMap Name [Type]
  }

data CompilerAcc op s = CompilerAcc {
    accItems :: DL.DList C.BlockItem
  , accDeclaredMem :: [(VName,Space)]
  }

instance Monoid (CompilerAcc op s) where
  CompilerAcc items1 declared1 `mappend` CompilerAcc items2 declared2 =
    CompilerAcc (items1<>items2) (declared1<>declared2)
  mempty = CompilerAcc mempty mempty

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

envFatMemory :: CompilerEnv op s -> Bool
envFatMemory = opsFatMemory . envOperations

newCompilerEnv :: Functions op -> Operations op s
               -> CompilerEnv op s
newCompilerEnv (Functions funs) ops =
  CompilerEnv { envOperations = ops
              , envFtable = ftable <> builtinFtable
              }
  where ftable = HM.fromList $ map funReturn funs
        funReturn (name, fun) =
          (name, paramsTypes $ functionOutput fun)
        builtinFtable =
          HM.map (map Scalar . snd) builtInFunctions

-- | Return a list of struct definitions for the tuples and arrays
-- seen during compilation.  The list is sorted according to
-- dependencies, such that a struct at index N only depends on structs
-- at positions >N.
typeDefinitions :: CompilerState s -> [C.Definition]
typeDefinitions = reverse . map (snd . snd) . compTypeStructs

newtype CompilerM op s a = CompilerM (RWS
                                      (CompilerEnv op s)
                                      (CompilerAcc op s)
                                      (CompilerState s) a)
  deriving (Functor, Applicative, Monad,
            MonadState (CompilerState s),
            MonadReader (CompilerEnv op s),
            MonadWriter (CompilerAcc op s))

instance MonadFreshNames (CompilerM op s) where
  getNameSource = gets compNameSrc
  putNameSource src = modify $ \s -> s { compNameSrc = src }

runCompilerM :: Functions op -> Operations op s -> VNameSource -> s
             -> CompilerM op s a
             -> (a, CompilerState s)
runCompilerM prog ops src userstate (CompilerM m) =
  let (x, s, _) = runRWS m (newCompilerEnv prog ops) (newCompilerState src userstate)
  in (x, s)

getUserState :: CompilerM op s s
getUserState = gets compUserState

putUserState :: s -> CompilerM op s ()
putUserState s = modify $ \compstate -> compstate { compUserState = s }

modifyUserState :: (s -> s) -> CompilerM op s ()
modifyUserState f = modify $ \compstate ->
  compstate { compUserState = f $ compUserState compstate }

collect :: CompilerM op s () -> CompilerM op s [C.BlockItem]
collect m = pass $ do
  ((), w) <- listen m
  return (DL.toList $ accItems w,
          const w { accItems = mempty })

collect' :: CompilerM op s a -> CompilerM op s (a, [C.BlockItem])
collect' m = pass $ do
  (x, w) <- listen m
  return ((x, DL.toList $ accItems w),
          const w { accItems = mempty})

lookupFunction :: Name -> CompilerM op s [Type]
lookupFunction name = do
  res <- asks $ HM.lookup name . envFtable
  case res of
    Nothing -> fail $ "Function " ++ nameToString name ++ " not found."
    Just ts -> return ts

item :: C.BlockItem -> CompilerM op s ()
item x = tell $ mempty { accItems = DL.singleton x }

instance C.ToIdent VName where
  toIdent = C.toIdent . zEncodeString . textual

instance C.ToExp VName where
  toExp v _ = [C.cexp|$id:v|]

stm :: C.Stm -> CompilerM op s ()
stm (C.Block items _) = mapM_ item items
stm (C.Default s _) = stm s
stm s = item [C.citem|$stm:s|]

stms :: [C.Stm] -> CompilerM op s ()
stms = mapM_ stm

decl :: C.InitGroup -> CompilerM op s ()
decl x = item [C.citem|$decl:x;|]

valueTypeName :: Type -> String
valueTypeName (Scalar t) = pretty $ primTypeToCType t
valueTypeName (Mem _ (Space space)) = space ++ "_mem"
valueTypeName (Mem _ DefaultSpace) = "mem"

typeName :: [Type] -> String
typeName [t] = valueTypeName t
typeName ts  = "tuple_" ++ intercalate "_" (map (typeName . pure) ts)

memToCType :: Space -> CompilerM op s C.Type
memToCType space = do
  refcount <- asks envFatMemory
  if refcount
     then return $ fatMemType space
     else rawMemCType space

rawMemCType :: Space -> CompilerM op s C.Type
rawMemCType DefaultSpace = return defaultMemBlockType
rawMemCType (Space sid) = join $ asks envMemoryType <*> pure sid

fatMemType :: Space -> C.Type
fatMemType space =
  [C.cty|struct $id:name|]
  where name = case space of
          DefaultSpace -> "memblock"
          Space sid    -> "memblock_" ++ sid

fatMemSet :: Space -> String
fatMemSet DefaultSpace = "memblock_set"
fatMemSet (Space sid) = "memblock_set_" ++ sid

fatMemAlloc :: Space -> String
fatMemAlloc DefaultSpace = "memblock_alloc"
fatMemAlloc (Space sid) = "memblock_alloc_" ++ sid

fatMemUnRef :: Space -> String
fatMemUnRef DefaultSpace = "memblock_unref"
fatMemUnRef (Space sid) = "memblock_unref_" ++ sid

rawMem :: C.ToIdent a => a -> CompilerM op s C.Exp
rawMem v =
  rawMem' <$> asks envFatMemory <*> pure (var v)

rawMem' :: Bool -> C.Exp -> C.Exp
rawMem' True  e = [C.cexp|$exp:e.mem|]
rawMem' False e = e

defineMemorySpace :: Space -> CompilerM op s [C.Definition]
defineMemorySpace space = do
  rm <- rawMemCType space
  let structdef = [C.cedecl|struct $id:sname { int *references; $ty:rm mem; };|]

  -- Unreferencing a memory block consists of decreasing its reference
  -- count and freeing the corresponding memory if the count reaches
  -- zero.
  free <- case space of
    Space sid -> do free_mem <- asks envDeallocate
                    collect $ free_mem [C.cexp|block->mem|] sid
    DefaultSpace -> return [[C.citem|free(block->mem);|]]
  let unrefdef = [C.cedecl|static void $id:(fatMemUnRef space) ($ty:mty *block) {
  if (block->references != NULL) {
    *(block->references) -= 1;
    if (*(block->references) == 0) {
      $items:free
      free(block->references);
      block->references = NULL;
    }
  }
}|]

  -- When allocating a memory block we initialise the reference count to 1.
  alloc <- collect $
    case space of
      DefaultSpace ->
        stm [C.cstm|block->mem = (char*) malloc(size);|]
      Space sid ->
        join $ asks envAllocate <*> pure [C.cexp|block->mem|] <*>
        pure [C.cexp|size|] <*> pure sid
  let allocdef = [C.cedecl|static void $id:(fatMemAlloc space) ($ty:mty *block, typename int32_t size) {
  $id:(fatMemUnRef space)(block);
  $items:alloc
  block->references = (int*) malloc(sizeof(int));
  *(block->references) = 1;
  }|]

  -- Memory setting - unreference the destination and increase the
  -- count of the source by one.
  let setdef = [C.cedecl|static void $id:(fatMemSet space) ($ty:mty *lhs, $ty:mty *rhs) {
  $id:(fatMemUnRef space)(lhs);
  (*(rhs->references))++;
  *lhs = *rhs;
}
|]

  return [structdef, unrefdef, allocdef, setdef]
  where mty = fatMemType space
        sname = case space of
          DefaultSpace -> "memblock"
          Space sid    -> "memblock_" ++ sid

declMem :: VName -> Space -> CompilerM op s ()
declMem name space = do
  ty <- memToCType space
  decl [C.cdecl|$ty:ty $id:name;|]
  resetMem $ var name
  tell $ mempty { accDeclaredMem = [(name, space)] }

resetMem :: C.Exp -> CompilerM op s ()
resetMem mem = do
  refcount <- asks envFatMemory
  when refcount $
    stm [C.cstm|$exp:mem.references = NULL;|]

setMem :: C.Exp -> VName -> Space -> CompilerM op s ()
setMem dest src space = do
  refcount <- asks envFatMemory
  if refcount
    then stm [C.cstm|$id:(fatMemSet space)(&$exp:dest, &$id:src);|]
    else stm [C.cstm|$exp:dest = $id:src;|]

unRefMem :: C.Exp -> Space -> CompilerM op s ()
unRefMem mem space =
  stm [C.cstm|$id:(fatMemUnRef space)(&$exp:mem);|]

allocMem :: C.ToIdent a =>
            a -> C.Exp -> Space -> CompilerM op s ()
allocMem name size space = do
  refcount <- asks envFatMemory
  if refcount
    then stm [C.cstm|$id:(fatMemAlloc space)(&$id:name, $exp:size);|]
    else alloc $ var name
  where alloc dest = case space of
          DefaultSpace ->
            stm [C.cstm|$exp:dest = (char*) malloc($exp:size);|]
          Space sid ->
            join $ asks envAllocate <*> rawMem name <*>
            pure size <*> pure sid

typeToCType :: [Type] -> CompilerM op s C.Type
typeToCType [Scalar bt] = return $ primTypeToCType bt
typeToCType [Mem _ space] = memToCType space
typeToCType t = do
  ty <- gets $ find (sameRepresentation t . fst) . compTypeStructs
  case ty of
    Just (_, (cty, _)) -> return cty
    Nothing -> do
      members <- zipWithM field t [(0::Int)..]
      let name = typeName t
          struct = [C.cedecl|struct $id:name { $sdecls:members };|]
          stype  = [C.cty|struct $id:name|]
      modify $ \s -> s { compTypeStructs = (t, (stype,struct)) : compTypeStructs s }
      return stype
        where field et i = do
                ct <- typeToCType [et]
                return [C.csdecl|$ty:ct $id:(tupleField i);|]

printPrimStm :: C.Exp -> PrimType -> C.Stm
printPrimStm val (IntType Int8) = [C.cstm|printf("%hhdi8", $exp:val);|]
printPrimStm val (IntType Int16) = [C.cstm|printf("%hdi16", $exp:val);|]
printPrimStm val (IntType Int32) = [C.cstm|printf("%di32", $exp:val);|]
printPrimStm val (IntType Int64) = [C.cstm|printf("%lldi64", $exp:val);|]
printPrimStm val Bool = [C.cstm|printf($exp:val ? "True" : "False");|]
printPrimStm val (FloatType Float32) = [C.cstm|printf("%.6ff32", $exp:val);|]
printPrimStm val (FloatType Float64) = [C.cstm|printf("%.6ff64", $exp:val);|]
printPrimStm _ Cert = [C.cstm|printf("Checked");|]

-- | Return a statement printing the given value.
printStm :: ValueDecl -> CompilerM op s C.Stm
printStm (ScalarValue bt name) =
  return $ printPrimStm (var name) bt
printStm (ArrayValue mem bt shape) = do
  mem' <- rawMem mem
  printArrayStm mem' bt shape

printArrayStm :: C.Exp -> PrimType -> [DimSize] -> CompilerM op s C.Stm
printArrayStm mem bt [] =
  return $ printPrimStm val bt
  where val = [C.cexp|*$exp:mem|]
printArrayStm mem bt (dim:shape) = do
  i <- newVName "print_i"
  v <- newVName "print_elem"
  let dim' = dimSizeToExp dim
      shape' = cproduct $ map dimSizeToExp shape
      bt'  = primTypeToCType bt
  printelem <- printArrayStm (var v) bt shape
  return [C.cstm|{
               if ($exp:dim' * $exp:shape' == 0) {
                   printf("empty(%s)", $exp:(ppArrayType bt (length shape)));
               } else {
                   int $id:i;
                   putchar('[');
                   for ($id:i = 0; $id:i < $exp:dim'; $id:i++) {
                           $ty:bt' *$id:v = (($ty:bt'*) $exp:mem) + $id:i * $exp:shape';
                           $stm:printelem
                           if ($id:i != $exp:dim'-1) {
                             printf(", ");
                           }
                   }
               putchar(']');
               }
             }|]


readFun :: PrimType -> Maybe String
readFun (IntType Int8) = Just "read_int8"
readFun (IntType Int16) = Just "read_int16"
readFun (IntType Int32) = Just "read_int32"
readFun (IntType Int64) = Just "read_int64"
readFun Bool = Just "read_bool"
readFun (FloatType Float32) = Just "read_float"
readFun (FloatType Float64) = Just "read_double"
readFun _    = Nothing

paramsTypes :: [Param] -> [Type]
paramsTypes = map paramType
  where paramType (MemParam _ size space) = Mem size space
        paramType (ScalarParam _ t) = Scalar t

readPrimStm :: C.Exp -> PrimType -> C.Stm
readPrimStm place t
  | Just f <- readFun t =
    [C.cstm|if ($id:f(&$exp:place) != 0) {
          panic(1, "Syntax error when reading %s.\n", $string:(pretty t));
        }|]
readPrimStm _ Cert =
  [C.cstm|;|]
readPrimStm _ t =
  [C.cstm|{
        panic(1, "Cannot read %s.\n", $string:(pretty t));
      }|]

-- | Our strategy for main() is to parse everything into host memory
-- ('DefaultSpace') and copy the result into host memory after the
-- @main@ function has returned.  We have some ad-hoc frobbery to copy
-- the host-level memory blocks to another memory space if necessary.
-- This will break if the Futhark main function uses non-trivial index
-- functions for its input or output.
--
-- The idea here is to keep the nastyness in main(), whilst not
-- messing up anything else.
mainCall :: [C.Stm] -> Name -> Function op
         -> CompilerM op s ([C.BlockItem],[C.BlockItem],[C.BlockItem],[C.BlockItem])
mainCall pre_timing fname (Function _ outputs inputs _ results args) = do
  crettype <- typeToCType $ paramsTypes outputs
  ret <- newVName "main_ret"
  refcount <- asks envFatMemory
  let readstms = readInputs refcount inputs args
  (argexps, prepare) <- collect' $ mapM prepareArg inputs
  -- unpackResults may copy back to DefaultSpace.
  unpackstms <- unpackResults ret outputs
  freestms <- freeResults ret outputs
  -- makeParam will always create DefaultSpace memory.
  inputdecls <- collect $ mapM_ makeParam inputs
  outputdecls <- collect $ mapM_ stubParam outputs
  free_in <- collect $ mapM_ freeParam inputs
  printstms <- printResult results
  return ([C.citems|
               /* Declare and read input. */
               $items:inputdecls
               $ty:crettype $id:ret;
               $stms:readstms
               $items:prepare
               $items:outputdecls
             |],
          [C.citems|
               /* Run the program once. */
               t_start = get_wall_time();
               $id:ret = $id:(funName fname)($args:argexps);
               $stms:pre_timing
               t_end = get_wall_time();
               long int elapsed_usec = t_end - t_start;
               if (time_runs && runtime_file != NULL) {
                 fprintf(runtime_file, "%ld\n", elapsed_usec);
               }
             |],
          [C.citems|
               $items:free_in
               /* Print the final result. */
               $items:unpackstms
               $stms:printstms
             |],
          freestms)
  where makeParam (MemParam name _ _) = do
          declMem name DefaultSpace
          allocMem name [C.cexp|0|] DefaultSpace
        makeParam (ScalarParam name ty) = do
          let ty' = primTypeToCType ty
          decl [C.cdecl|$ty:ty' $id:name;|]

        stubParam (MemParam name _ _) =
          declMem name DefaultSpace
        stubParam (ScalarParam name ty) = do
          let ty' = primTypeToCType ty
          decl [C.cdecl|$ty:ty' $id:name;|]

        freeParam (MemParam name _ _) =
          unRefMem (var name) DefaultSpace
        freeParam ScalarParam{} =
          return ()

prepareArg :: Param -> CompilerM op s C.Exp
prepareArg (MemParam name size (Space sid)) = do
  -- Futhark main expects some other memory space than default, so
  -- create a new memory block and copy it there.
  name' <- newVName $ baseString name <> "_" <> sid
  copy <- asks envCopy
  let size' = dimSizeToExp size
      dest = rawMem' True $ var name'
      src = rawMem' True $ var name
  declMem name' $ Space sid
  allocMem name' size' $ Space sid
  copy dest [C.cexp|0|] (Space sid) src [C.cexp|0|] DefaultSpace size'
  return [C.cexp|$id:name'|]

prepareArg p = return $ var $ paramName p

readInputs :: Bool -> [Param] -> [ValueDecl] -> [C.Stm]
readInputs refcount inputparams = snd . mapAccumL (readInput refcount memsizes) mempty
  where memsizes = sizeVars inputparams

readInput :: Bool -> HM.HashMap VName VName -> [VName] -> ValueDecl
          -> ([VName], C.Stm)
readInput _ _ known_sizes (ScalarValue t name) =
  (known_sizes, readPrimStm (var name) t)
readInput refcount memsizes known_sizes (ArrayValue name t shape)
  | Just f <- readFun t =
  -- We need to create an array for the array parser to put
  -- the shapes.
  let t' = primTypeToCType t
      rank = length shape
      maybeCopyDim (ConstSize x) i =
        assertSameSize x [C.cexp|shape[$int:i]|]
      maybeCopyDim (VarSize d) i
        | d `elem` known_sizes =
            assertSameSize d [C.cexp|shape[$int:i]|]
        | otherwise =
            [C.cstm|$id:d = shape[$int:i];|]
      copyshape = zipWith maybeCopyDim shape [0..rank-1]
      memsize = cproduct $ [C.cexp|sizeof($ty:t')|] :
                             [ [C.cexp|shape[$int:i]|] |
                              i <- [0..rank-1] ]
      copymemsize = case HM.lookup name memsizes of
        Nothing -> []
        Just sizevar -> [[C.cstm|$id:sizevar = $exp:memsize;|]]
      dest = rawMem' refcount $ var name
  in (known_sizes ++ wrote_sizes,
      [C.cstm|{
        typename int64_t shape[$int:rank];
        if (read_array(sizeof($ty:t'),
                       $id:f,
                       (void**)& $exp:dest,
                       shape,
                       $int:(length shape))
            != 0) {
          panic(1, "Syntax error when reading %s.\n", $string:(ppArrayType t rank));
        }
        $stms:copyshape
        $stms:copymemsize
      }|])
  | otherwise =
    (known_sizes, [C.cstm|panic(1, "Cannot read %s.\n", $string:(pretty t));|])
  where wrote_sizes = mapMaybe isVarSize shape
        isVarSize ConstSize{} = Nothing
        isVarSize (VarSize d) = Just d

        assertSameSize expected got =
          [C.cstm|if ($exp:expected != $exp:got) {
                    fprintf(stderr, "Parameter %s has bad dimension (expected %d, got %d).\n",
                            $string:(baseString name), $exp:expected, $exp:got);
                    abort();
                  }|]

sizeVars :: [Param] -> HM.HashMap VName VName
sizeVars = mconcat . map sizeVars'
  where sizeVars' (MemParam parname (VarSize memsizename) _) =
          HM.singleton parname memsizename
        sizeVars' _ =
          HM.empty

printResult :: [ValueDecl] -> CompilerM op s [C.Stm]
printResult vs = fmap concat $ forM vs $ \v -> do
  p <- printStm v
  return [p, [C.cstm|printf("\n");|]]

unpackResults :: VName -> [Param] -> CompilerM op s [C.BlockItem]
unpackResults ret [p] =
  collect $ unpackResult (var ret) p
unpackResults ret outparams =
  collect $ zipWithM_ assign outparams [0..]
  where assign param i =
          unpackResult (tupleFieldExp (var ret) i) param

unpackResult :: C.Exp -> Param -> CompilerM op s ()
unpackResult ret (ScalarParam name _) =
  stm [C.cstm|$id:name = $exp:ret;|]
unpackResult ret (MemParam name _ DefaultSpace) =
  stm [C.cstm|$id:name = $exp:ret;|]
unpackResult ret (MemParam name size (Space srcspace)) = do
  copy <- asks envCopy
  let size' = dimSizeToExp size
  allocMem name size' DefaultSpace
  copy (rawMem' True $ var name) [C.cexp|0|] DefaultSpace
    (rawMem' True ret) [C.cexp|0|] (Space srcspace) size'

freeResults :: VName -> [Param] -> CompilerM op s [C.BlockItem]
freeResults ret [p] =
  collect $ freeResult (var ret) p
freeResults ret outparams =
  collect $ zipWithM_ free outparams [0..]
  where free param i = freeResult (tupleFieldExp (var ret) i) param

freeResult :: C.Exp -> Param -> CompilerM op s ()
freeResult _ ScalarParam{} =
  return ()
freeResult e (MemParam _ _ space) =
  unRefMem e space

benchmarkOptions :: [Option]
benchmarkOptions =
   [ Option { optionLongName = "write-runtime-to"
            , optionShortName = Just 't'
            , optionArgument = RequiredArgument
            , optionAction = set_runtime_file
            }
   , Option { optionLongName = "runs"
            , optionShortName = Just 'r'
            , optionArgument = RequiredArgument
            , optionAction = set_num_runs
            }
   ]
  where set_runtime_file = [C.cstm|{
          runtime_file = fopen(optarg, "w");
          if (runtime_file == NULL) {
            panic(1, "Cannot open %s: %s", optarg, strerror(errno));
          }
        }|]
        set_num_runs = [C.cstm|{
          num_runs = atoi(optarg);
          perform_warmup = 1;
          if (num_runs <= 0) {
            panic(1, "Need a positive number of runs, not %s", optarg);
          }
        }|]

-- | Compile imperative program to a C program.  Always uses the
-- function named "main" as entry point, so make sure it is defined.
compileProg :: MonadFreshNames m =>
               Operations op s
            -> s
            -> [Space]
            -> [C.Definition] -> [C.Stm] -> [C.Stm] -> [C.BlockItem]
            -> [Option]
            -> Functions op
            -> m String
compileProg ops userstate spaces decls pre_main_stms pre_timing post_main_items options prog@(Functions funs) = do
  src <- getNameSource
  let ((memtypes, prototypes, definitions,
        (main_pre, main, main_post, free_out)), endstate) =
        runCompilerM prog ops src userstate compileProg'
  return $ pretty [C.cunit|
$esc:("#include <stdio.h>")
$esc:("#include <stdlib.h>")
$esc:("#include <string.h>")
$esc:("#include <stdint.h>")
$esc:("#include <math.h>")
$esc:("#include <ctype.h>")
$esc:("#include <errno.h>")
$esc:("#include <assert.h>")
$esc:("#include <getopt.h>")

$esc:panic_h

$esc:timing_h

$edecls:decls

$edecls:memtypes

$edecls:(typeDefinitions endstate)

$edecls:(compVarDefinitions endstate)

$edecls:prototypes

$edecls:builtin

static int detail_timing = 0;

$edecls:(map funcToDef definitions)

$esc:reader_h

static typename FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;

$func:(generateOptionParser "parse_options" (benchmarkOptions++options))

int main(int argc, char** argv) {
  typename int64_t t_start, t_end;
  int time_runs;

  fut_progname = argv[0];

  $stms:(compInit endstate)

  int parsed_options = parse_options(argc, argv);
  argc -= parsed_options;
  argv += parsed_options;

  $stms:pre_main_stms
  $items:main_pre
  /* Warmup run */
  if (perform_warmup) {
    time_runs = 0;
    $items:main
    $items:free_out
  }
  time_runs = 1;
  /* Proper run. */
  for (int run = 0; run < num_runs; run++) {
    if (run == num_runs-1) {
      detail_timing = 1;
    }
    $items:main
    if (run < num_runs-1) {
      $items:free_out
    }
  }
  $items:main_post
  $items:post_main_items
  $items:free_out
  if (runtime_file != NULL) {
    fclose(runtime_file);
  }
  return 0;
}

|]
  where compileProg' = do
          (prototypes, definitions) <- unzip <$> mapM compileFun funs
          let mainname = nameFromString "main"
          main <- case lookup mainname funs of
                    Nothing   -> fail "GenericC.compileProg: No main function"
                    Just func -> mainCall pre_timing mainname func
          memtypes <- concat <$> mapM defineMemorySpace spaces
          return (memtypes,  prototypes, definitions, main)
        funcToDef func = C.FuncDef func loc
          where loc = case func of
                        C.OldFunc _ _ _ _ _ _ l -> l
                        C.Func _ _ _ _ _ l      -> l

        builtin = map asDecl builtInFunctionDefs ++
                  cIntOps ++ cFloat32Ops ++ cFloat64Ops ++ cFloatConvOps
          where asDecl fun = [C.cedecl|$func:fun|]

        panic_h = $(embedStringFile "rts/c/panic.h")
        reader_h = $(embedStringFile "rts/c/reader.h")
        timing_h = $(embedStringFile "rts/c/timing.h")

compileFun :: (Name, Function op) -> CompilerM op s (C.Definition, C.Func)
compileFun (fname, Function _ outputs inputs body _ _) = do
  args' <- mapM compileInput inputs
  (retval, body') <- blockScope' $ do
    mapM_ compileOutput outputs
    compileFunBody outputs body
  crettype <- typeToCType $ paramsTypes outputs
  return ([C.cedecl|static $ty:crettype $id:(funName fname)( $params:args' );|],
          [C.cfun|static $ty:crettype $id:(funName fname)( $params:args' ) {
             $items:body'
             return $id:retval;
}|])
  where compileInput (ScalarParam name bt) = do
          let ctp = primTypeToCType bt
          return [C.cparam|$ty:ctp $id:name|]
        compileInput (MemParam name _ space) = do
          ty <- memToCType space
          return [C.cparam|$ty:ty $id:name|]

        compileOutput (ScalarParam name bt) = do
          let ctp = primTypeToCType bt
          decl [C.cdecl|$ty:ctp $id:name;|]
        compileOutput (MemParam name _ space) =
          declMem name space

compilePrimValue :: PrimValue -> C.Exp

compilePrimValue (IntValue (Int8Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int16Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int32Value k)) = [C.cexp|$int:k|]
compilePrimValue (IntValue (Int64Value k)) = [C.cexp|$int:k|]

compilePrimValue (FloatValue (Float64Value x))
  | isInfinite x =
      if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
  | otherwise =
      [C.cexp|$double:(toRational x)|]
compilePrimValue (FloatValue (Float32Value x))
  | isInfinite x =
      if x > 0 then [C.cexp|INFINITY|] else [C.cexp|-INFINITY|]
  | otherwise =
      [C.cexp|$float:(toRational x)|]

compilePrimValue (BoolValue b) =
  [C.cexp|$int:b'|]
  where b' :: Int
        b' = if b then 1 else 0

compilePrimValue Checked =
  [C.cexp|0|]

dimSizeToExp :: DimSize -> C.Exp
dimSizeToExp (ConstSize x) = [C.cexp|$int:x|]
dimSizeToExp (VarSize v)   = var v

derefPointer :: C.Exp -> C.Exp -> C.Type -> C.Exp
derefPointer ptr i res_t =
  [C.cexp|*(($ty:res_t)&($exp:ptr[$exp:i]))|]

writeScalarPointerWithQuals :: PointerQuals op s -> WriteScalar op s
writeScalarPointerWithQuals quals_f dest i elemtype space v = do
  quals <- quals_f space
  let deref = derefPointer dest i
              [C.cty|$tyquals:quals $ty:elemtype*|]
  stm [C.cstm|$exp:deref = $exp:v;|]

readScalarPointerWithQuals :: PointerQuals op s -> ReadScalar op s
readScalarPointerWithQuals quals_f dest i elemtype space = do
  quals <- quals_f space
  return $ derefPointer dest i [C.cty|$tyquals:quals $ty:elemtype*|]

compileExpToName :: String -> PrimType -> Exp -> CompilerM op s VName
compileExpToName _ _ (ScalarVar v) =
  return v
compileExpToName desc t e = do
  desc' <- newVName desc
  e' <- compileExp e
  decl [C.cdecl|$ty:(primTypeToCType t) $id:desc' = $e';|]
  return desc'

compileExp :: Exp -> CompilerM op s C.Exp

compileExp (Constant val) = return $ compilePrimValue val

compileExp (ScalarVar src) =
  return [C.cexp|$id:src|]

compileExp (Index src (Count iexp) restype DefaultSpace) = do
  src' <- rawMem src
  derefPointer src'
    <$> compileExp iexp
    <*> pure [C.cty|$ty:(primTypeToCType restype)*|]

compileExp (Index src (Count iexp) restype (Space space)) =
  join $ asks envReadScalar
    <*> rawMem src <*> compileExp iexp
    <*> pure (primTypeToCType restype) <*> pure space

compileExp (UnOp Complement{} x) = do
  x' <- compileExp x
  return [C.cexp|~$exp:x'|]

compileExp (UnOp Not{} x) = do
  x' <- compileExp x
  return [C.cexp|!$exp:x'|]

compileExp (UnOp Abs{} x) = do
  x' <- compileExp x
  return [C.cexp|abs($exp:x')|]

compileExp (UnOp (FAbs Float32) x) = do
  x' <- compileExp x
  return [C.cexp|(float)fabs($exp:x')|]

compileExp (UnOp (FAbs Float64) x) = do
  x' <- compileExp x
  return [C.cexp|fabs($exp:x')|]

compileExp (UnOp SSignum{} x) = do
  x' <- compileExp x
  return [C.cexp|($exp:x' > 0) - ($exp:x' < 0)|]

compileExp (UnOp USignum{} x) = do
  x' <- compileExp x
  return [C.cexp|($exp:x' > 0) - ($exp:x' < 0) != 0|]

compileExp (CmpOp cmp x y) = do
  x' <- compileExp x
  y' <- compileExp y
  return $ case cmp of
    CmpEq{} -> [C.cexp|$exp:x' == $exp:y'|]

    FCmpLt{} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe{} -> [C.cexp|$exp:x' <= $exp:y'|]

    _ -> [C.cexp|$id:(pretty cmp)($exp:x', $exp:y')|]

compileExp (ConvOp conv x) = do
  x' <- compileExp x
  return [C.cexp|$id:(pretty conv)($exp:x')|]

compileExp (BinOp bop x y) = do
  x' <- compileExp x
  y' <- compileExp y
  return $ case bop of
             Add{} -> [C.cexp|$exp:x' + $exp:y'|]
             FAdd{} -> [C.cexp|$exp:x' + $exp:y'|]
             Sub{} -> [C.cexp|$exp:x' - $exp:y'|]
             FSub{} -> [C.cexp|$exp:x' - $exp:y'|]
             Mul{} -> [C.cexp|$exp:x' * $exp:y'|]
             FMul{} -> [C.cexp|$exp:x' * $exp:y'|]
             FDiv{} -> [C.cexp|$exp:x' / $exp:y'|]
             Xor{} -> [C.cexp|$exp:x' ^ $exp:y'|]
             And{} -> [C.cexp|$exp:x' & $exp:y'|]
             Or{} -> [C.cexp|$exp:x' | $exp:y'|]
             Shl{} -> [C.cexp|$exp:x' << $exp:y'|]
             LogAnd{} -> [C.cexp|$exp:x' && $exp:y'|]
             LogOr{} -> [C.cexp|$exp:x' || $exp:y'|]
             _ -> [C.cexp|$id:(pretty bop)($exp:x', $exp:y')|]

compileExp (SizeOf t) =
  return [C.cexp|(sizeof($ty:t'))|]
  where t' = primTypeToCType t

compileExp (Cond c t f) = do
  c' <- compileExp c
  t' <- compileExp t
  f' <- compileExp f
  return [C.cexp|$exp:c' ? $exp:t' : $exp:f'|]

compileCode :: Code op -> CompilerM op s ()

compileCode (Op op) =
  join $ asks envOpCompiler <*> pure op

compileCode Skip = return ()

compileCode (Comment s code) = do
  items <- blockScope $ compileCode code
  stm [C.cstm|$comment:("// " ++ s)
              { $items:items }
             |]

compileCode c
  | Just (name, t, e, c') <- declareAndSet c = do
    let ct = primTypeToCType t
    e' <- compileExp e
    item [C.citem|$ty:ct $id:name = $exp:e';|]
    compileCode c'

compileCode (c1 :>>: c2) = compileCode c1 >> compileCode c2

compileCode (Assert e loc) = do
  e' <- compileExp e
  stm [C.cstm|if (!$exp:e') {
                   fprintf(stderr, "Assertion %s at %s failed.\n",
                                   $string:(pretty e), $string:(locStr loc));
                   abort();
                 }|]

compileCode (Allocate name (Count e) space) = do
  size <- compileExp e
  allocMem name size space

compileCode (For i bound body) = do
  let i' = textual i
  bound' <- compileExp bound
  body'  <- blockScope $ compileCode body
  stm [C.cstm|for (int $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]

compileCode (While cond body) = do
  cond' <- compileExp cond
  body' <- blockScope $ compileCode body
  stm [C.cstm|while ($exp:cond') {
            $items:body'
          }|]

compileCode (If cond tbranch fbranch) = do
  cond' <- compileExp cond
  tbranch' <- blockScope $ compileCode tbranch
  fbranch' <- blockScope $ compileCode fbranch
  stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]

compileCode (Copy dest (Count destoffset) DefaultSpace src (Count srcoffset) DefaultSpace (Count size)) = do
  destoffset' <- compileExp destoffset
  srcoffset' <- compileExp srcoffset
  size' <- compileExp size
  dest' <- rawMem dest
  src' <- rawMem src
  stm [C.cstm|memmove($exp:dest' + $exp:destoffset',
                      $exp:src' + $exp:srcoffset',
                      $exp:size');|]

compileCode (Copy dest (Count destoffset) destspace src (Count srcoffset) srcspace (Count size)) = do
  copy <- asks envCopy
  join $ copy
    <$> rawMem dest <*> compileExp destoffset <*> pure destspace
    <*> rawMem src <*> compileExp srcoffset <*> pure srcspace
    <*> compileExp size

compileCode (Write dest (Count idx) elemtype DefaultSpace elemexp) = do
  dest' <- rawMem dest
  deref <- derefPointer dest'
           <$> compileExp idx
           <*> pure [C.cty|$ty:(primTypeToCType elemtype)*|]
  elemexp' <- compileExp elemexp
  stm [C.cstm|$exp:deref = $exp:elemexp';|]

compileCode (Write dest (Count idx) elemtype (Space space) elemexp) =
  join $ asks envWriteScalar
    <*> rawMem dest
    <*> compileExp idx
    <*> pure (primTypeToCType elemtype)
    <*> pure space
    <*> compileExp elemexp

compileCode (DeclareMem name space) =
  declMem name space

compileCode (DeclareScalar name t) = do
  let ct = primTypeToCType t
  decl [C.cdecl|$ty:ct $id:name;|]

-- For assignments of the form 'x = x OP e', we generate C assignment
-- operators to make the resulting code slightly nicer.  This has no
-- effect on performance.
compileCode (SetScalar dest (BinOp op (ScalarVar x) y))
  | dest == x, Just f <- assignmentOperator op = do
      y' <- compileExp y
      stm [C.cstm|$exp:(f dest y');|]

compileCode (SetScalar dest src) = do
  src' <- compileExp src
  stm [C.cstm|$id:dest = $exp:src';|]

compileCode (SetMem dest src space) =
  setMem (var dest) src space

compileCode (Call dests fname args) = do
  args' <- mapM compileExp args
  outtypes <- lookupFunction fname
  crestype <- typeToCType outtypes
  case dests of
    [dest] ->
      stm [C.cstm|$id:dest = $id:(funName fname)($args:args');|]
    _        -> do
      ret <- newVName "call_ret"
      decl [C.cdecl|$ty:crestype $id:ret;|]
      stm [C.cstm|$id:ret = $id:(funName fname)($args:args');|]
      forM_ (zip [0..] dests) $ \(i,dest) ->
        stm [C.cstm|$id:dest = $exp:(tupleFieldExp (var ret) i);|]

blockScope :: CompilerM op s () -> CompilerM op s [C.BlockItem]
blockScope = fmap snd . blockScope'

blockScope' :: CompilerM op s a -> CompilerM op s (a, [C.BlockItem])
blockScope' m = pass $ do
  (x, w) <- listen m
  let items = DL.toList $ accItems w
  releases <- collect $ forM_ (accDeclaredMem w) $ \(mem, space) ->
    unRefMem (var mem) space
  return ((x, items ++ releases),
          const mempty)

compileFunBody :: [Param] -> Code op -> CompilerM op s VName
compileFunBody outputs code = do
  retval <- newVName "retval"
  bodytype <- typeToCType $ paramsTypes outputs
  compileCode code
  decl [C.cdecl|$ty:bodytype $id:retval;|]
  let setRetVal' i (MemParam name _ space) = do
        let field = tupleFieldExp (var retval) i
        resetMem field
        setMem field name space
      setRetVal' i (ScalarParam name _) =
        stm [C.cstm|$exp:(tupleFieldExp (var retval) i) = $id:name;|]
  case outputs of
    [output] -> stm [C.cstm|$id:retval = $id:(paramName output);|]
    _        -> zipWithM_ setRetVal' [0..] outputs
  return retval

ppArrayType :: PrimType -> Int -> String
ppArrayType t 0 = pretty t
ppArrayType t n = "[]" ++ ppArrayType t (n-1)

declareAndSet :: Code op -> Maybe (VName, PrimType, Exp, Code op)
declareAndSet (DeclareScalar name t :>>: (SetScalar dest e :>>: c))
  | name == dest = Just (name, t, e, c)
declareAndSet ((DeclareScalar name t :>>: SetScalar dest e) :>>: c)
  | name == dest = Just (name, t, e, c)
declareAndSet _ = Nothing

assignmentOperator :: BinOp -> Maybe (VName -> C.Exp -> C.Exp)
assignmentOperator Add{}  = Just $ \d e -> [C.cexp|$id:d += $exp:e|]
assignmentOperator Sub{} = Just $ \d e -> [C.cexp|$id:d -= $exp:e|]
assignmentOperator Mul{} = Just $ \d e -> [C.cexp|$id:d *= $exp:e|]
assignmentOperator _     = Nothing

-- | Return an expression multiplying together the given expressions.
-- If an empty list is given, the expression @1@ is returned.
cproduct :: [C.Exp] -> C.Exp
cproduct []     = [C.cexp|1|]
cproduct (e:es) = foldl mult e es
  where mult x y = [C.cexp|$exp:x * $exp:y|]

-- | Turn a name into a C expression consisting of just that name.
var :: C.ToIdent a => a -> C.Exp
var k = [C.cexp|$id:k|]
