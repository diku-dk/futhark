{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | C code generator.  This module can convert a correct ImpCode
-- program to an equivalent ISPC program.
module Futhark.CodeGen.Backends.MulticoreISPC
  ( compileProg,
    GC.CParts (..),
    GC.asLibrary,
    GC.asExecutable,
    GC.asServer,
    operations,
  )
where

import Control.Monad
import qualified Data.Text as T
import Futhark.CodeGen.ImpCode.Multicore
import qualified Futhark.CodeGen.ImpGen.Multicore as ImpGen
import Futhark.IR.MCMem (MCMem, Prog)
import Futhark.MonadFreshNames
import qualified Language.C.Quote.ISPC as C
import qualified Language.C.Syntax as C
import qualified Futhark.CodeGen.Backends.GenericC as GC
import qualified Futhark.CodeGen.Backends.MulticoreC as MC
import Futhark.CodeGen.RTS.C (errorsH, ispcUtilH, uniformH)
import Futhark.CodeGen.Backends.SimpleRep (fromStorage, toStorage, primStorageType, cScalarDefs, funName)
import Futhark.IR.Prop (isBuiltInFunction)
import Data.Maybe
import qualified Data.Map as M
import Control.Monad.Reader
import Data.Bifunctor
import Data.Loc (noLoc)
import Data.List (unzip4)
import qualified Data.DList as DL
import NeatInterpolation (untrimming)
import Futhark.Util.Pretty (prettyText)
import Control.Lens (over, each)
import Control.Monad.State

type ISPCCompilerM a = GC.CompilerM Multicore ISPCState a
data ISPCState = ISPCState
  { sDefs :: DL.DList C.Definition
  , sUniform :: Names
  }

instance Semigroup ISPCState where
  ISPCState d1 u1 <> ISPCState d2 u2 = ISPCState (d1 <> d2) (u1 <> u2)

instance Monoid ISPCState where
  mempty = ISPCState mempty mempty
  mappend = (<>)

-- | Compile the program to C and ISPC code using multicore operations.
compileProg ::
  MonadFreshNames m => T.Text -> T.Text -> Prog MCMem -> m (ImpGen.Warnings, (GC.CParts, T.Text))
compileProg header version prog = do
  (ws, defs) <- ImpGen.compileProg prog
  let Functions funs = defFuns defs

  (ws', (cparts, endstate)) <-
    traverse
      (GC.compileProg'
        "ispc_multicore"
        version
        operations
        (MC.generateContext >> mapM_ compileBuiltinFun funs)
        header
        [DefaultSpace]
        MC.cliOptions
      ) (ws, defs)

  let ispc_decls = T.unlines $ map prettyText $ DL.toList $ sDefs $ GC.compUserState endstate

  let ispcdefs =
        [untrimming|
#define bool uint8 // This is a workaround around an ISPC bug, stdbool doesn't get included
typedef int64 int64_t;
typedef int32 int32_t;
typedef int16 int16_t;
typedef int8 int8_t;
typedef int8 char;
typedef unsigned int64 uint64_t;
typedef unsigned int32 uint32_t;
typedef unsigned int16 uint16_t;
typedef unsigned int8 uint8_t;
#define volatile

$errorsH

#define INFINITY (floatbits((uniform int)0x7f800000))
#define NAN (floatbits((uniform int)0x7fc00000))
#define fabs(x) abs(x)
#define FUTHARK_F64_ENABLED
$cScalarDefs

$uniformH

$ispcUtilH

#ifndef __ISPC_STRUCT_memblock__
#define __ISPC_STRUCT_memblock__
struct memblock {
    int32_t * references;
    uint8_t * mem;
    int64_t size;
    const int8_t * desc;
};
#endif

$ispc_decls|]

  pure (ws', (cparts, ispcdefs))

operations :: GC.Operations Multicore ISPCState
operations =
  MC.operations
    { GC.opsCompiler = compileOp
    }

ispcDecl :: C.Definition -> ISPCCompilerM ()
ispcDecl def =
  GC.modifyUserState (\s -> s { sDefs = sDefs s <> DL.singleton def })

ispcEarlyDecl :: C.Definition -> ISPCCompilerM ()
ispcEarlyDecl def =
  GC.modifyUserState (\s -> s { sDefs = DL.singleton def <> sDefs s })

ispcDef :: MC.DefSpecifier ISPCState
ispcDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  pure s'

-- Expose struct to both ISPC and C
sharedDef :: MC.DefSpecifier ISPCState
sharedDef s f = do
  s' <- MC.multicoreName s
  ispcDecl =<< f s'
  -- Workaround for https://github.com/ispc/ispc/issues/2277
  dummy <- newVName "dummy_struct_usage"
  ispcDecl [C.cedecl|export void $id:dummy(uniform struct $id:s' * uniform a) { (void)a; }|]
  pure s'

-- Copy memory where one of the operands is using an AoS layout.
copyMemoryAOS ::
  PrimType ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  C.Exp ->
  GC.CompilerM op s ()
copyMemoryAOS pt destmem destidx srcmem srcidx nbytes =
  GC.stm
    [C.cstm|if ($exp:nbytes > 0) {
              $id:overload($exp:destmem + $exp:destidx,
                      $exp:srcmem + $exp:srcidx,
                      extract($exp:nbytes, 0));
            }|]
  where 
    size = show (primByteSize pt :: Integer)
    overload = "memmove_" <> size

-- ISPC has no string literals, so this makes one in C and exposes it via an external function
makeStringLiteral :: String -> ISPCCompilerM Name
makeStringLiteral str = do
  name <- MC.multicoreDef "strlit_shim" $ \s ->
    pure [C.cedecl|char* $id:s() { return $string:str; }|]
  ispcDecl
    [C.cedecl|extern "C" unmasked uniform char* uniform $id:name();|]
  pure name

-- Allocate memory in ISPC
allocMem :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> C.Stm -> ISPCCompilerM ()
allocMem mem size space on_failure = GC.allocMem' mem size space on_failure stmt
  where
    stmt space' mem' size' mem_s' on_failure' = do
      strlit <- makeStringLiteral mem_s'
      pure
        [C.cstm|if ($id:(GC.fatMemAlloc space')(ctx, &$exp:mem', $exp:size',
                  $id:strlit())) {
                  $stm:on_failure'
        }|]

-- Set memory in ISPC
setMem :: (C.ToExp a, C.ToExp b) => a -> b -> Space -> ISPCCompilerM ()
setMem dest src space = GC.setMem' dest src space stmt
  where
    stmt space' dest' src' src_s' = do
      strlit <- makeStringLiteral src_s'
      pure
        [C.cstm|if ($id:(GC.fatMemSet space')(ctx, &$exp:dest', &$exp:src',
          $id:strlit()) != 0) {
          err = 1;
        }|]

-- Unref memory in ISPC
unRefMem :: C.ToExp a => a -> Space -> ISPCCompilerM ()
unRefMem mem space = GC.unRefMem' mem space cstm
  where
    cstm s m m_s = do
      strlit <- makeStringLiteral m_s
      pure
        [C.cstm|if ($id:(GC.fatMemUnRef s)(ctx, &$exp:m, $id:strlit()) != 0) {
          err = 1;
        }|]

-- Free memory in ISPC
freeAllocatedMem :: ISPCCompilerM [C.BlockItem]
freeAllocatedMem = GC.freeAllocatedMem' unRefMem

-- Given a ImpCode function, generate all the required machinery for calling
-- it in ISPC, both in a varying or uniform context. This involves handling
-- for the fact that ISPC cannot pass structs by value to external functions.
compileBuiltinFun :: (Name, Function op) -> ISPCCompilerM ()
compileBuiltinFun (fname, func@(Function _ outputs inputs _ _ _))
  | isNothing $ functionEntry func = do
    let extra = [[C.cparam|uniform struct futhark_context * uniform ctx|]]
        extra_c = [[C.cparam|struct futhark_context * ctx|]]
        extra_exp = [[C.cexp|$id:p|] | C.Param (Just p) _ _ _ <- extra]

    (inparams_c, in_args_c) <- unzip <$> mapM (compileInputsExtern []) inputs
    (outparams_c, out_args_c) <- unzip <$> mapM (compileOutputsExtern []) outputs

    (inparams_extern, _) <- unzip <$> mapM (compileInputsExtern [C.ctyquals|uniform|]) inputs
    (outparams_extern, _) <- unzip <$> mapM (compileOutputsExtern [C.ctyquals|uniform|]) outputs

    (inparams_uni, in_args_noderef) <- unzip <$> mapM compileInputsUniform inputs
    (outparams_uni, out_args_noderef) <- unzip <$> mapM compileOutputsUniform outputs

    (inparams_varying, in_args_vary, prebody_in') <- unzip3 <$> mapM compileInputsVarying inputs
    (outparams_varying, out_args_vary, prebody_out', postbody_out') <- unzip4 <$> mapM compileOutputsVarying outputs
    let (prebody_in, prebody_out, postbody_out) = over each concat (prebody_in', prebody_out', postbody_out')

    GC.libDecl =<< pure
      [C.cedecl|int $id:((funName fname) ++ "_extern")($params:extra_c, $params:outparams_c, $params:inparams_c) {               
                  return $id:(funName fname)($args:extra_exp, $args:out_args_c, $args:in_args_c);
                }|]

    let ispc_extern =
          [C.cedecl|extern "C" uniform int $id:((funName fname) ++ "_extern")
                      ($params:extra, $params:outparams_extern, $params:inparams_extern);|]

        ispc_uniform =
          [C.cedecl|uniform int $id:(funName fname)
                    ($params:extra, $params:outparams_uni, $params:inparams_uni) { 
                      return $id:(funName $ fname<>"_extern")(
                        $args:extra_exp,
                        $args:out_args_noderef,
                        $args:in_args_noderef);
                    }|]

        ispc_varying =
          [C.cedecl|uniform int $id:(funName fname)
                    ($params:extra, $params:outparams_varying, $params:inparams_varying) { 
                        uniform int err = 0;
                        $items:prebody_in
                        $items:prebody_out
                        foreach_active (i) {
                          err |= $id:(funName $ fname<>"_extern")(
                            $args:extra_exp,
                            $args:out_args_vary,
                            $args:in_args_vary);
                        }
                        $items:postbody_out
                        return err;
                    }|]

    mapM_ ispcEarlyDecl [ispc_varying, ispc_uniform, ispc_extern]
  | otherwise = pure ()
  where
    compileInputsExtern vari (ScalarParam name bt) = do
      let ctp = GC.primTypeToCType bt
      pure ([C.cparam|$tyquals:vari $ty:ctp $id:name|], [C.cexp|$id:name|])
    compileInputsExtern vari (MemParam name space) = do
      ty <- GC.memToCType name space
      pure ([C.cparam|$tyquals:vari $ty:ty * $tyquals:vari $id:name|], [C.cexp|*$id:name|])

    compileOutputsExtern vari (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      let ctp = GC.primTypeToCType bt
      pure ([C.cparam|$tyquals:vari $ty:ctp * $tyquals:vari $id:p_name|], [C.cexp|$id:p_name|])
    compileOutputsExtern vari (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      pure ([C.cparam|$tyquals:vari $ty:ty * $tyquals:vari $id:p_name|], [C.cexp|$id:p_name|])

    compileInputsUniform (ScalarParam name bt) = do
      let ctp    = GC.primTypeToCType bt
          params = [C.cparam|uniform $ty:ctp $id:name|]
          args   = [C.cexp|$id:name|]
      pure (params, args)
    compileInputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      let params = [C.cparam|uniform $ty:ty $id:name|]
          args   = [C.cexp|&$id:name|]
      pure (params, args)

    compileOutputsUniform (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      let ctp    = GC.primTypeToCType bt
          params = [C.cparam|uniform $ty:ctp *uniform $id:p_name|]
          args   = [C.cexp|$id:p_name|]
      pure (params, args)
    compileOutputsUniform (MemParam name space) = do
      ty <- GC.memToCType name space
      p_name <- newVName $ baseString name ++ "_p"
      let params = [C.cparam|uniform $ty:ty $id:p_name|]
          args   = [C.cexp|&$id:p_name|]
      pure (params, args)

    compileInputsVarying (ScalarParam name bt) = do
      let ctp      = GC.primTypeToCType bt
          params   = [C.cparam|$ty:ctp $id:name|]
          args     = [C.cexp|extract($id:name,i)|]
          pre_body = []
      pure (params, args, pre_body)
    compileInputsVarying (MemParam name space) = do
      typ <- GC.memToCType name space
      newvn <- newVName $ "aos_" <> baseString name
      let params   = [C.cparam|$ty:typ $id:name|]
          args     = [C.cexp|&$id:(newvn)[i]|]
          pre_body = [C.citems|uniform $ty:typ $id:(newvn)[programCount];
                               $id:(newvn)[programIndex] = $id:name;|]
      pure (params, args, pre_body)

    compileOutputsVarying (ScalarParam name bt) = do
      p_name <- newVName $ "out_" ++ baseString name
      deref_name <- newVName $ "aos_" ++ baseString name
      vari_p_name <- newVName $ "convert_" ++ baseString name
      let ctp       = GC.primTypeToCType bt
          pre_body  = [C.citems|varying $ty:ctp $id:vari_p_name = *$id:p_name;
                                uniform $ty:ctp $id:deref_name[programCount];
                                $id:deref_name[programIndex] = $id:vari_p_name;|]
          post_body = [C.citems|*$id:p_name = $id:(deref_name)[programIndex];|]
          params    = [C.cparam|varying $ty:ctp * uniform $id:p_name|]
          args      = [C.cexp|&$id:(deref_name)[i]|]
      pure (params, args, pre_body, post_body)
    compileOutputsVarying (MemParam name space) = do
      typ <- GC.memToCType name space
      newvn <- newVName $ "aos_" <> baseString name
      let params   = [C.cparam|$ty:typ $id:name|]
          args     = [C.cexp|&$id:(newvn)[i]|]
          pre_body = [C.citems|uniform $ty:typ $id:(newvn)[programCount];
                       $id:(newvn)[programIndex] = $id:name;|]
      pure (params,args,pre_body, [])

-- Handle logging an error message in ISPC
handleError :: ErrorMsg Exp -> String -> ISPCCompilerM ()
handleError msg stacktrace = do
  -- Get format sting
  (formatstr, formatargs) <- GC.errorMsgString msg
  let formatstr' = "Error: " <> formatstr <> "\n\nBacktrace:\n%s"
  -- Get args types and names for shim
  let arg_types = errorMsgArgTypes msg
  arg_names <- mapM (newVName . const "arg") arg_types
  let params = zipWith (\ty name -> [C.cparam|$ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  let params_uni = zipWith (\ty name -> [C.cparam|uniform $ty:(GC.primTypeToCType ty) $id:name|]) arg_types arg_names
  -- Make shim
  let formatargs' = mapArgNames msg formatargs arg_names
  shim <- MC.multicoreDef "assert_shim" $ \s -> do
    pure
      [C.cedecl|void $id:s(struct futhark_context* ctx, $params:params) {
        if (ctx->error == NULL)
          ctx->error = msgprintf($string:formatstr', $args:formatargs', $string:stacktrace);
      }|]
  ispcDecl
    [C.cedecl|extern "C" unmasked void $id:shim(uniform struct futhark_context* uniform, $params:params_uni);|]
  -- Call the shim
  args <- getErrorValExps msg
  uni <- newVName "uni"
  let args' = map (\x -> [C.cexp|extract($exp:x, $id:uni)|]) args
  GC.items
    [C.citems|
      foreach_active($id:uni) {
        $id:shim(ctx, $args:args');
        err = FUTHARK_PROGRAM_ERROR;
      }|]
  where
    getErrorVal (ErrorString _) = Nothing
    getErrorVal (ErrorVal _ v) = Just v

    getErrorValExps (ErrorMsg m) = mapM GC.compileExp $ mapMaybe getErrorVal m

    mapArgNames' (x:xs) (y:ys) (t:ts)
      | isJust $ getErrorVal x = [C.cexp|$id:t|] : mapArgNames' xs ys ts
      | otherwise = y : mapArgNames' xs ys (t:ts)
    mapArgNames' _ ys [] = ys
    mapArgNames' _ _ _ = []

    mapArgNames (ErrorMsg parts) = mapArgNames' parts

-- TODO(pema): Deduplicate some of the C code in the generic C generator
-- Compile a block of code with ISPC specific semantics, falling back
-- to generic C when this semantics is not needed.
compileCode :: MCCode -> ISPCCompilerM ()
compileCode (Comment s code) = do
  xs <- GC.collect $ compileCode code
  let comment = "// " ++ s
  GC.stm
    [C.cstm|$comment:comment
              { $items:xs }
             |]
compileCode (DeclareScalar name _ t) = do
  let ct = GC.primTypeToCType t
  quals <- getVariabilityQuals name
  GC.decl [C.cdecl|$tyquals:quals $ty:ct $id:name;|]
compileCode (DeclareArray name DefaultSpace t vs) = do
  name_realtype <- newVName $ baseString name ++ "_realtype"
  let ct = GC.primTypeToCType t
  case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      GC.earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Fake a memory block.
  GC.contextField
    (C.toIdent name noLoc)
    [C.cty|struct memblock|]
    $ Just [C.cexp|(struct memblock){NULL, (char*)$id:name_realtype, 0}|]
  -- Make an exported C shim to access it
  shim <- MC.multicoreDef "get_static_array_shim" $ \f ->
    pure [C.cedecl|struct memblock* $id:f(struct futhark_context* ctx) { return &ctx->$id:name; }|]
  ispcDecl [C.cedecl|extern "C" unmasked uniform struct memblock * uniform
                        $id:shim(uniform struct futhark_context* uniform ctx);|]
  -- Call it
  GC.item [C.citem|uniform struct memblock $id:name = *$id:shim(ctx);|]
compileCode (c1 :>>: c2) = go (GC.linearCode (c1 :>>: c2))
  where
    go (DeclareScalar name _ t : SetScalar dest e : code)
      | name == dest = do
        let ct = GC.primTypeToCType t
        e' <- GC.compileExp e
        quals <- getVariabilityQuals name
        GC.item [C.citem|$tyquals:quals $ty:ct $id:name = $exp:e';|]
        go code
    go (x : xs) = compileCode x >> go xs
    go [] = pure ()
compileCode (Allocate name (Count (TPrimExp e)) space) = do
  size <- GC.compileExp e
  cached <- GC.cacheMem name
  case cached of
    Just cur_size ->
      GC.stm -- TODO(pema): Handle errors here
        [C.cstm|if ($exp:cur_size < $exp:size) {
                  lexical_realloc_ispc(&$exp:name, &$exp:cur_size, $exp:size);
                }|]
    _ ->
      allocMem name size space [C.cstm|{err = 1;}|]
compileCode (SetMem dest src space) =
  setMem dest src space
compileCode (Write dest (Count idx) elemtype DefaultSpace _ elemexp)
  | isConstExp (untyped idx) = do
    cached <- isJust <$> GC.cacheMem dest
    let typ =
          if cached
            then [C.cty|varying $ty:(primStorageType elemtype)* uniform|]
            else [C.cty|$ty:(primStorageType elemtype)*|]
    dest' <- GC.rawMem dest
    idxexp <- GC.compileExp (untyped idx)
    varis <- mapM getVariability (namesToList $ freeIn idx)
    let quals = if all (==Uniform) varis then [C.ctyquals|uniform|] else []
    tmp <- newVName "tmp_idx"
    -- Disambiguate the variability of the constant index
    GC.decl [C.cdecl|$tyquals:quals typename int64_t $id:tmp = $exp:idxexp;|]
    let deref =
          GC.derefPointer
            dest'
            [C.cexp|$id:tmp|]
            [C.cty|$ty:typ|]
    elemexp' <- toStorage elemtype <$> GC.compileExp elemexp
    GC.stm [C.cstm|$exp:deref = $exp:elemexp';|]
  | otherwise = do
    cached <- isJust <$> GC.cacheMem dest
    let typ =
          if cached
            then [C.cty|varying $ty:(primStorageType elemtype)* uniform|]
            else [C.cty|$ty:(primStorageType elemtype)*|]
    dest' <- GC.rawMem dest
    deref <-
      GC.derefPointer dest'
        <$> GC.compileExp (untyped idx)
        <*> pure [C.cty|$ty:typ|]
    elemexp' <- toStorage elemtype <$> GC.compileExp elemexp
    GC.stm [C.cstm|$exp:deref = $exp:elemexp';|]
  where
    isConstExp = isSimple . constFoldPrimExp
    isSimple (ValueExp _) = True
    isSimple _ = False
compileCode (Read x src (Count iexp) restype DefaultSpace vol) = do
  src' <- GC.rawMem src
  cached <- isJust <$> GC.cacheMem src
  let typ =
        if cached
          then [C.cty|varying $ty:(primStorageType restype)* uniform|]
          else [C.cty|$ty:(primStorageType restype)*|]
  e <-
    fmap (fromStorage restype) $
      GC.derefPointer src'
        <$> GC.compileExp (untyped iexp)
        <*> pure [C.cty|$tyquals:(GC.volQuals vol) $ty:typ|]
  GC.stm [C.cstm|$id:x = $exp:e;|]
compileCode code@(Copy dest pt (Count destoffset) DefaultSpace src (Count srcoffset) DefaultSpace (Count size)) = do
  dm <- isJust <$> GC.cacheMem dest
  sm <- isJust <$> GC.cacheMem src
  if dm || sm
    then
      join $
        copyMemoryAOS pt
          <$> GC.rawMem dest
          <*> GC.compileExp (untyped destoffset)
          <*> GC.rawMem src
          <*> GC.compileExp (untyped srcoffset)
          <*> GC.compileExp (untyped size)
    else
      GC.compileCode code
compileCode (Free name space) = do
  cached <- isJust <$> GC.cacheMem name
  unless cached $ unRefMem name space
compileCode (For i bound body) = do
  let i' = C.toIdent i
      t = GC.primTypeToCType $ primExpType bound
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCode body
  quals <- getVariabilityQuals i
  GC.stm
    [C.cstm|for ($tyquals:quals $ty:t $id:i' = 0; $id:i' < $exp:bound'; $id:i'++) {
            $items:body'
          }|]
compileCode (While cond body) = do
  cond' <- GC.compileExp $ untyped cond
  body' <- GC.collect $ compileCode body
  GC.stm
    [C.cstm|while ($exp:cond') {
            $items:body'
          }|]
compileCode (If cond tbranch fbranch) = do
  cond' <- GC.compileExp $ untyped cond
  tbranch' <- GC.collect $ compileCode tbranch
  fbranch' <- GC.collect $ compileCode fbranch
  GC.stm $ case (tbranch', fbranch') of
    (_, []) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' }|]
    ([], _) ->
      [C.cstm|if (!($exp:cond')) { $items:fbranch' }|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCode (Call dests fname args) =
  defCallIspc dests fname =<< mapM compileArg args
  where
    compileArg (MemArg m) = pure [C.cexp|$exp:m|]
    compileArg (ExpArg e) = GC.compileExp e
    defCallIspc dests' fname' args' = do
      let out_args = [[C.cexp|&$id:d|] | d <- dests']
          args''
            | isBuiltInFunction fname' = args'
            | otherwise = [C.cexp|ctx|] : out_args ++ args'
      case dests' of
        [d]
          | isBuiltInFunction fname' ->
              GC.stm [C.cstm|$id:d = $id:(funName fname')($args:args'');|]
        _ ->
          GC.item [C.citem|if ($id:(funName fname')($args:args'') != 0) { err = 1; }|]
compileCode (Assert e msg (loc, locs)) = do
  e' <- GC.compileExp e
  err <- GC.collect $ handleError msg stacktrace
  GC.stm [C.cstm|if (!$exp:e') { $items:err }|]
  where
    stacktrace = prettyStacktrace 0 $ map locStr $ loc : locs
compileCode code =
  GC.compileCode code

compileGetStructVals ::
  Name ->
  [VName] ->
  [(C.Type, MC.ValueType)] ->
  ISPCCompilerM [C.BlockItem]
compileGetStructVals struct a b = concat <$> zipWithM field a b
  where
    struct' = struct <> "_"
    field name (ty, MC.Prim pt) = do
      let inner = [C.cexp|$id:struct'->$id:(MC.closureFreeStructField name)|]
      pure [C.citems|uniform $ty:ty $id:name = $exp:(fromStorage pt inner);|]
    field name (_, _) = do
      strlit <- makeStringLiteral $ pretty name
      pure [C.citems|uniform struct memblock $id:name;
                     $id:name.desc = $id:strlit();
                     $id:name.mem = $id:struct'->$id:(MC.closureFreeStructField name);
                     $id:name.size = 0;
                     $id:name.references = NULL;|]

-- Generate a segop function for top_level and potentially nested SegOp code
compileOp :: GC.OpCompiler Multicore ISPCState
compileOp (SegOp name params seq_task par_task retvals (SchedulerInfo e sched)) = do
  let (ParallelTask seq_code) = seq_task
  free_ctypes <- mapM MC.paramToCType params
  retval_ctypes <- mapM MC.paramToCType retvals
  let free_args = map paramName params
      retval_args = map paramName retvals
      free = zip free_args free_ctypes
      retval = zip retval_args retval_ctypes

  e' <- GC.compileExp e

  let lexical = lexicalMemoryUsageMC $ Function Nothing [] params seq_code [] []

  fstruct <-
    MC.prepareTaskStruct sharedDef "task" free_args free_ctypes retval_args retval_ctypes

  fpar_task <- MC.generateParLoopFn lexical (name ++ "_task") seq_code fstruct free retval
  MC.addTimingFields fpar_task

  let ftask_name = fstruct <> "_task"

  toC <- GC.collect $ do
    GC.decl [C.cdecl|struct scheduler_segop $id:ftask_name;|]
    GC.stm [C.cstm|$id:ftask_name.args = args;|]
    GC.stm [C.cstm|$id:ftask_name.top_level_fn = $id:fpar_task;|]
    GC.stm [C.cstm|$id:ftask_name.name = $string:(nameToString fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.iterations = iterations;|]
    -- Create the timing fields for the task
    GC.stm [C.cstm|$id:ftask_name.task_time = &ctx->$id:(MC.functionTiming fpar_task);|]
    GC.stm [C.cstm|$id:ftask_name.task_iter = &ctx->$id:(MC.functionIterations fpar_task);|]

    case sched of
        Dynamic -> GC.stm [C.cstm|$id:ftask_name.sched = DYNAMIC;|]
        Static -> GC.stm [C.cstm|$id:ftask_name.sched = STATIC;|]

    -- Generate the nested segop function if available
    fnpar_task <- case par_task of
        Just (ParallelTask nested_code) -> do
            let lexical_nested = lexicalMemoryUsageMC $ Function Nothing [] params nested_code [] []
            fnpar_task <- MC.generateParLoopFn lexical_nested (name ++ "_nested_task") nested_code fstruct free retval
            GC.stm [C.cstm|$id:ftask_name.nested_fn = $id:fnpar_task;|]
            pure $ zip [fnpar_task] [True]
        Nothing -> do
            GC.stm [C.cstm|$id:ftask_name.nested_fn=NULL;|]
            pure mempty

    GC.stm [C.cstm|return scheduler_prepare_task(&ctx->scheduler, &$id:ftask_name);|]

    -- Add profile fields for -P option
    mapM_ GC.profileReport $ MC.multiCoreReport $ (fpar_task, True) : fnpar_task

  schedn <- MC.multicoreDef "schedule_shim" $ \s ->
    pure [C.cedecl|int $id:s(struct futhark_context* ctx, void* args, typename int64_t iterations) {
        $items:toC
    }|]

  ispcDecl [C.cedecl|extern "C" unmasked uniform int $id:schedn 
                        (struct futhark_context uniform * uniform ctx, 
                        struct $id:fstruct uniform * uniform args, 
                        uniform int iterations);|]

  aos_name <- newVName "aos"
  GC.stm [C.cstm|$escstm:("#if ISPC")|]
  GC.items [C.citems|
    #if ISPC
    uniform struct $id:fstruct $id:aos_name[programCount];
    $id:aos_name[programIndex] = $id:(fstruct <> "_");
    foreach_active (i) {
      if (err == 0) {
        err = $id:schedn(ctx, &$id:aos_name[i], extract($exp:e', i));
      }
    }|]
  -- TODO(pema): We can't do much else here^^ than set the error code and hope for the best
  GC.stm [C.cstm|$escstm:("#else")|]
  GC.items [C.citems|
    err = $id:schedn(ctx, &$id:(fstruct <> "_"), $exp:e');
    if (err != 0) {
      goto cleanup;
    }|]
  GC.stm [C.cstm|$escstm:("#endif")|]
compileOp (ISPCKernel body free) = do
  free_ctypes <- mapM MC.paramToCType free
  let free_args = map paramName free

  let lexical = lexicalMemoryUsageMC $ Function Nothing [] free body [] []
  -- Generate ISPC kernel
  fstruct <- MC.prepareTaskStruct sharedDef "param_struct" free_args free_ctypes [] []
  let fstruct' = fstruct <> "_"

  ispcShim <- ispcDef "loop_ispc" $ \s -> do
    mainBody <- GC.inNewFunction $ analyzeVariability body $ cachingMemoryISPC lexical $ \decl_cached free_cached ->
      GC.collect $ do
        GC.decl [C.cdecl|uniform struct futhark_context * uniform ctx = $id:fstruct'->ctx;|]
        GC.items =<< compileGetStructVals fstruct free_args free_ctypes
        GC.decl [C.cdecl|uniform int err = 0;|]
        body' <- GC.collect $ compileCode body
        mapM_ GC.item decl_cached
        mapM_ GC.item =<< GC.declAllocatedMem
        mapM_ GC.item body'
        free_mem <- freeAllocatedMem
        GC.stm [C.cstm|cleanup: {$stms:free_cached $items:free_mem}|]
        GC.stm [C.cstm|return err;|]
    pure
      [C.cedecl|
        export static uniform int $id:s(uniform typename int64_t start,
                                        uniform typename int64_t end,
                                        struct $id:fstruct uniform * uniform $id:fstruct' ) {
          $items:mainBody
        }|]

  -- Generate C code to call into ISPC kernel
  GC.items [C.citems|
    err = $id:ispcShim(start, end, & $id:fstruct');
    if (err != 0) {
      goto cleanup;
    }|]
compileOp (ForEach i bound body) = do
  bound' <- GC.compileExp bound
  body' <- GC.collect $ compileCode body
  GC.stm [C.cstm|
    foreach ($id:i = 0 ... extract($exp:bound', 0)) {
      $items:body'
    }|]
compileOp (ForEachActive name body) = do
  body' <- GC.collect $ compileCode body
  GC.stm [C.cstm|
    foreach_active ($id:name) {
      $items:body'
    }|]
compileOp (ExtractLane dest tar lane) = do
  tar' <- GC.compileExp tar
  lane' <- GC.compileExp lane
  GC.stm [C.cstm|$id:dest = extract($exp:tar', $exp:lane');|]
compileOp op = MC.compileOp op

cachingMemoryISPC ::
  M.Map VName Space ->
  ([C.BlockItem] -> [C.Stm] -> GC.CompilerM op s a) ->
  GC.CompilerM op s a
cachingMemoryISPC lexical f = do
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
          { GC.envCachedMem =
              M.fromList (map (first (`C.toExp` noLoc)) cached')
                <> GC.envCachedMem env
          }

      declCached (mem, size) =
        [ [C.citem|size_t $id:size = 0;|],
          [C.citem|varying unsigned char * uniform $id:mem = NULL;|]
        ]

      freeCached (mem, _) =
        [C.cstm|free($id:mem);|]

  local lexMem $ f (concatMap declCached cached') (map freeCached cached')

-- TODO(pema): Move this somewhere else
-- Variability analysis
type Dependencies = M.Map VName Names

data Variability = Uniform | Varying
  deriving (Eq, Ord, Show)

newtype VariabilityM a
  = VariabilityM (ReaderT Names (State Dependencies) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Dependencies,
      MonadReader Names
    )

execVariabilityM :: VariabilityM a -> Dependencies
execVariabilityM (VariabilityM m) =
  execState (runReaderT m mempty) mempty

-- Extend the set of dependencies with a new one
addDeps :: VName -> Names -> VariabilityM ()
addDeps v ns = do
  deps <- get
  env <- ask
  case M.lookup v deps of
    Nothing -> put $ M.insert v (ns <> env) deps
    Just ns' -> put $ M.insert v (ns <> ns') deps

-- Find all the dependencies in a body of code
findDeps :: MCCode -> VariabilityM ()
findDeps (x :>>: y) = do
  findDeps x
  findDeps y
findDeps (If cond x y) =
  local (<> freeIn cond) $ do
    findDeps x
    findDeps y
findDeps (For idx bound x) = do
  addDeps idx free
  local (<> free) $ findDeps x
  where free = freeIn bound
findDeps (While cond x) = do
  local (<> freeIn cond) $ findDeps x
findDeps (Comment _ x) =
  findDeps x
findDeps (Op (SegOp _ free _ _ retvals _)) =
  mapM_ (\x ->
    addDeps (paramName x) $
      namesFromList $ map paramName free) retvals
findDeps (Op (ForEach _ _ body)) =
  findDeps body
findDeps (Op (ForEachActive _ body)) =
  findDeps body
findDeps (SetScalar name e) =
  addDeps name $ freeIn e
findDeps (Call tars _ args) =
  mapM_ (\x -> addDeps x $ freeIn args) tars
findDeps (Read x arr (Count iexp) _ DefaultSpace _) = do
  addDeps x $ freeIn (untyped iexp)
  addDeps x $ oneName arr
findDeps (Op (GetLoopBounds x y)) = do
  addDeps x mempty
  addDeps y mempty
findDeps (Op (ExtractLane x _ _)) = do
  addDeps x mempty
findDeps _ = pure ()

-- Take a list of dependencies and iterate them to a fixed point.
depsFixedPoint :: Dependencies -> Dependencies
depsFixedPoint deps =
  if deps == deps'
    then deps
    else depsFixedPoint deps'
  where
    grow names =
      namesFromList $
        concatMap (\n -> n : namesToList (M.findWithDefault mempty n deps)) $
          namesToList names
    deps' = M.map grow deps

-- Find roots of variance. These are memory blocks declared in
-- the current scope as well as loop indices of foreach loops.
findVarying :: MCCode -> [VName]
findVarying (x :>>: y) = findVarying x ++ findVarying y
findVarying (If _ x y) = findVarying x ++ findVarying y
findVarying (For _ _ x) = findVarying x
findVarying (While _ x) = findVarying x
findVarying (Comment _ x) = findVarying x
findVarying (Op (ForEachActive _ body)) = findVarying body
findVarying (Op (ForEach idx _ body)) = idx : findVarying body
findVarying (DeclareMem mem _) = [mem]
findVarying _ = []

-- Analyze variability in a body of code and run an action with
-- info about that variability in the compiler state.
analyzeVariability :: MCCode -> ISPCCompilerM a -> ISPCCompilerM a
analyzeVariability code m = do
  let roots = findVarying code
  let deps = M.toList $ depsFixedPoint $ execVariabilityM $ findDeps code
  let safelist = filter (\(_, b) -> not $ any (`nameIn` b) roots) deps
  let safe = namesFromList $ map fst safelist
  pre_state <- GC.getUserState
  GC.modifyUserState (\s -> s { sUniform = safe })
  a <- m
  GC.modifyUserState (\s -> s { sUniform = sUniform pre_state })
  pure a

-- Get the variability of a variable
getVariability :: VName -> ISPCCompilerM Variability
getVariability name = do
  uniforms <- sUniform <$> GC.getUserState
  pure $ if nameIn name uniforms
    then Uniform
    else Varying

-- Get the variability qualifiers of a variable
getVariabilityQuals :: VName -> ISPCCompilerM [C.TypeQual]
getVariabilityQuals name = variQuals <$> getVariability name
  where variQuals Uniform = [C.ctyquals|uniform|]
        variQuals Varying = []
