{-# LANGUAGE QuasiQuotes #-}

-- | Translation of ImpCode Exp and Code to C.
module Futhark.CodeGen.Backends.GenericC.Code
  ( compilePrimExp,
    compileExp,
    compileCode,
    compileDest,
    compileArg,
    compileCopy,
    compileCopyWith,
    errorMsgString,
    linearCode,
  )
where

import Control.Monad
import Control.Monad.Reader (asks)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Monad
import Futhark.CodeGen.ImpCode
import Futhark.IR.Prop (isBuiltInFunction)
import Futhark.MonadFreshNames
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

errorMsgString :: ErrorMsg Exp -> CompilerM op s (String, [C.Exp])
errorMsgString (ErrorMsg parts) = do
  let boolStr e = [C.cexp|($exp:e) ? "true" : "false"|]
      asLongLong e = [C.cexp|(long long int)$exp:e|]
      asDouble e = [C.cexp|(double)$exp:e|]
      onPart (ErrorString s) = pure ("%s", [C.cexp|$string:(T.unpack s)|])
      onPart (ErrorVal Bool x) = ("%s",) . boolStr <$> compileExp x
      onPart (ErrorVal Unit _) = pure ("%s", [C.cexp|"()"|])
      onPart (ErrorVal (IntType Int8) x) = ("%hhd",) <$> compileExp x
      onPart (ErrorVal (IntType Int16) x) = ("%hd",) <$> compileExp x
      onPart (ErrorVal (IntType Int32) x) = ("%d",) <$> compileExp x
      onPart (ErrorVal (IntType Int64) x) = ("%lld",) . asLongLong <$> compileExp x
      onPart (ErrorVal (FloatType Float16) x) = ("%f",) . asDouble <$> compileExp x
      onPart (ErrorVal (FloatType Float32) x) = ("%f",) . asDouble <$> compileExp x
      onPart (ErrorVal (FloatType Float64) x) = ("%f",) <$> compileExp x
  (formatstrs, formatargs) <- mapAndUnzipM onPart parts
  pure (mconcat formatstrs, formatargs)

compileExp :: Exp -> CompilerM op s C.Exp
compileExp = compilePrimExp $ \v -> pure [C.cexp|$id:v|]

-- | Tell me how to compile a @v@, and I'll Compile any @PrimExp v@ for you.
compilePrimExp :: (Monad m) => (v -> m C.Exp) -> PrimExp v -> m C.Exp
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
  pure [C.cexp|$id:(prettyString op)($exp:x')|]
compilePrimExp f (CmpOpExp cmp x y) = do
  x' <- compilePrimExp f x
  y' <- compilePrimExp f y
  pure $ case cmp of
    CmpEq {} -> [C.cexp|$exp:x' == $exp:y'|]
    FCmpLt {} -> [C.cexp|$exp:x' < $exp:y'|]
    FCmpLe {} -> [C.cexp|$exp:x' <= $exp:y'|]
    CmpLlt {} -> [C.cexp|$exp:x' < $exp:y'|]
    CmpLle {} -> [C.cexp|$exp:x' <= $exp:y'|]
    _ -> [C.cexp|$id:(prettyString cmp)($exp:x', $exp:y')|]
compilePrimExp f (ConvOpExp conv x) = do
  x' <- compilePrimExp f x
  pure [C.cexp|$id:(prettyString conv)($exp:x')|]
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
    _ -> [C.cexp|$id:(prettyString bop)($exp:x', $exp:y')|]
compilePrimExp f (FunExp h args _) = do
  args' <- mapM (compilePrimExp f) args
  pure [C.cexp|$id:(funName (nameFromString h))($args:args')|]

linearCode :: Code op -> [Code op]
linearCode = reverse . go []
  where
    go acc (x :>>: y) =
      go (go acc x) y
    go acc x = x : acc

assignmentOperator :: BinOp -> Maybe (VName -> C.Exp -> C.Exp)
assignmentOperator Add {} = Just $ \d e -> [C.cexp|$id:d += $exp:e|]
assignmentOperator Sub {} = Just $ \d e -> [C.cexp|$id:d -= $exp:e|]
assignmentOperator Mul {} = Just $ \d e -> [C.cexp|$id:d *= $exp:e|]
assignmentOperator _ = Nothing

generateRead ::
  C.Exp ->
  C.Exp ->
  PrimType ->
  Space ->
  Volatility ->
  CompilerM op s C.Exp
generateRead _ _ Unit _ _ =
  pure [C.cexp|$exp:(UnitValue)|]
generateRead src iexp _ ScalarSpace {} _ =
  pure [C.cexp|$exp:src[$exp:iexp]|]
generateRead src iexp restype DefaultSpace vol =
  pure . fromStorage restype $
    derefPointer
      src
      iexp
      [C.cty|$tyquals:(volQuals vol) $ty:(primStorageType restype)*|]
generateRead src iexp restype (Space space) vol = do
  reader <- asks (opsReadScalar . envOperations)
  fromStorage restype <$> reader src iexp (primStorageType restype) space vol

generateWrite ::
  C.Exp ->
  C.Exp ->
  PrimType ->
  Space ->
  Volatility ->
  C.Exp ->
  CompilerM op s ()
generateWrite _ _ Unit _ _ _ = pure ()
generateWrite dest idx _ ScalarSpace {} _ elemexp = do
  stm [C.cstm|$exp:dest[$exp:idx] = $exp:elemexp;|]
generateWrite dest idx elemtype DefaultSpace vol elemexp = do
  let deref =
        derefPointer
          dest
          idx
          [C.cty|$tyquals:(volQuals vol) $ty:(primStorageType elemtype)*|]
      elemexp' = toStorage elemtype elemexp
  stm [C.cstm|$exp:deref = $exp:elemexp';|]
generateWrite dest idx elemtype (Space space) vol elemexp = do
  writer <- asks (opsWriteScalar . envOperations)
  writer dest idx (primStorageType elemtype) space vol (toStorage elemtype elemexp)

compileRead ::
  VName ->
  Count u (TPrimExp t VName) ->
  PrimType ->
  Space ->
  Volatility ->
  CompilerM op s C.Exp
compileRead src (Count iexp) restype space vol = do
  src' <- rawMem src
  iexp' <- compileExp (untyped iexp)
  generateRead src' iexp' restype space vol

memNeedsWrapping :: VName -> CompilerM op s Bool
memNeedsWrapping v = do
  refcount <- fatMemory DefaultSpace
  cached <- isJust <$> cacheMem v
  pure $ refcount && cached

-- | Compile an argument to a function applicaiton.
compileArg :: Arg -> CompilerM op s C.Exp
compileArg (MemArg m) = do
  -- Function might expect fat memory, so if this is a lexical/cached
  -- raw pointer, we have to wrap it in a struct.
  wrap <- memNeedsWrapping m
  if wrap
    then pure [C.cexp|($ty:(fatMemType DefaultSpace)) {.references = NULL, .mem = $exp:m}|]
    else pure [C.cexp|$exp:m|]
compileArg (ExpArg e) = compileExp e

-- | Prepare a destination for function application.
compileDest :: VName -> CompilerM op s (VName, [C.Stm])
compileDest v = do
  -- Function result be fat memory, so if target is a raw pointer, we
  -- have to wrap it in a struct and unwrap it afterwards.
  wrap <- memNeedsWrapping v
  if wrap
    then do
      v' <- newVName $ baseString v <> "_struct"
      item [C.citem|$ty:(fatMemType DefaultSpace) $id:v' = {.references = NULL, .mem = $exp:v};|]
      pure (v', [C.cstms|$id:v = $id:v'.mem;|])
    else pure (v, mempty)

compileCode :: Code op -> CompilerM op s ()
compileCode (Op op) =
  join $ asks (opsCompiler . envOperations) <*> pure op
compileCode Skip = pure ()
compileCode (Comment s code) = do
  xs <- collect $ compileCode code
  let comment = "// " ++ T.unpack s
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
    go (DeclareScalar name vol t : Read dest src i restype space read_vol : code)
      | name == dest = do
          let ct = primTypeToCType t
          e <- compileRead src i restype space read_vol
          item [C.citem|$tyquals:(volQuals vol) $ty:ct $id:name = $exp:e;|]
          go code
    go (DeclareScalar name vol t : Call [dest] fname args : code)
      | name == dest,
        isBuiltInFunction fname = do
          let ct = primTypeToCType t
          args' <- mapM compileArg args
          item [C.citem|$tyquals:(volQuals vol) $ty:ct $id:name = $id:(funName fname)($args:args');|]
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
    stacktrace = T.unpack $ prettyStacktrace 0 $ map locText $ loc : locs
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
                 err = lexical_realloc(ctx, &$exp:name, &$exp:cur_size, $exp:size);
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
    (_, [C.BlockStm x@C.If {}]) ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else $stm:x|]
    _ ->
      [C.cstm|if ($exp:cond') { $items:tbranch' } else { $items:fbranch' }|]
compileCode (Copy t shape (dst, dstspace) (dstoffset, dststrides) (src, srcspace) (srcoffset, srcstrides)) = do
  cp <- asks $ M.lookup (dstspace, srcspace) . opsCopies . envOperations
  case cp of
    Just cp' | t /= Unit -> do
      shape' <- traverse (traverse (compileExp . untyped)) shape
      dst' <- rawMem dst
      src' <- rawMem src
      dstoffset' <- traverse (compileExp . untyped) dstoffset
      dststrides' <- traverse (traverse (compileExp . untyped)) dststrides
      srcoffset' <- traverse (compileExp . untyped) srcoffset
      srcstrides' <- traverse (traverse (compileExp . untyped)) srcstrides
      cp' CopyBarrier t shape' dst' (dstoffset', dststrides') src' (srcoffset', srcstrides')
    _ ->
      compileCopy t shape (dst, dstspace) (dstoffset, dststrides) (src, srcspace) (srcoffset, srcstrides)
compileCode (Write _ _ Unit _ _ _) = pure ()
compileCode (Write dst (Count idx) elemtype space vol elemexp) = do
  dst' <- rawMem dst
  idx' <- compileExp (untyped idx)
  elemexp' <- compileExp elemexp
  generateWrite dst' idx' elemtype space vol elemexp'
compileCode (Read x src i restype space vol) = do
  e <- compileRead src i restype space vol
  stm [C.cstm|$id:x = $exp:e;|]
compileCode (DeclareMem name space) =
  declMem name space
compileCode (DeclareScalar name vol t) = do
  let ct = primTypeToCType t
  decl [C.cdecl|$tyquals:(volQuals vol) $ty:ct $id:name;|]
compileCode (DeclareArray name t vs) = do
  name_realtype <- newVName $ baseString name ++ "_realtype"
  let ct = primTypeToCType t
  case vs of
    ArrayValues vs' -> do
      let vs'' = [[C.cinit|$exp:v|] | v <- vs']
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:(length vs')] = {$inits:vs''};|]
    ArrayZeros n ->
      earlyDecl [C.cedecl|static $ty:ct $id:name_realtype[$int:n];|]
  -- Fake a memory block.
  item
    [C.citem|struct memblock $id:name =
               (struct memblock){NULL,
                                 (unsigned char*)$id:name_realtype,
                                 0,
                                 $string:(prettyString name)};|]
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
compileCode (Call [dest] fname args)
  | isBuiltInFunction fname = do
      args' <- mapM compileArg args
      stm [C.cstm|$id:dest = $id:(funName fname)($args:args');|]
compileCode (Call dests fname args) = do
  (dests', unpack_dest) <- mapAndUnzipM compileDest dests
  join $
    asks (opsCall . envOperations)
      <*> pure dests'
      <*> pure fname
      <*> mapM compileArg args
  stms $ mconcat unpack_dest

-- | Compile an 'Copy' using sequential nested loops, but
-- parameterised over how to do the reads and writes.
compileCopyWith ::
  [Count Elements (TExp Int64)] ->
  (C.Exp -> C.Exp -> CompilerM op s ()) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  (C.Exp -> CompilerM op s C.Exp) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  CompilerM op s ()
compileCopyWith shape doWrite dst_lmad doRead src_lmad = do
  let (dstoffset, dststrides) = dst_lmad
      (srcoffset, srcstrides) = src_lmad
  shape' <- mapM (compileExp . untyped . unCount) shape
  body <- collect $ do
    dst_i <-
      compileExp . untyped . unCount $
        dstoffset + sum (zipWith (*) is' dststrides)
    src_i <-
      compileExp . untyped . unCount $
        srcoffset + sum (zipWith (*) is' srcstrides)
    doWrite dst_i =<< doRead src_i
  items $ loops (zip is shape') body
  where
    r = length shape
    is = map (VName "i") [0 .. r - 1]
    is' :: [Count Elements (TExp Int64)]
    is' = map (elements . le64) is
    loops [] body = body
    loops ((i, n) : ins) body =
      [C.citems|for (typename int64_t $id:i = 0; $id:i < $exp:n; $id:i++)
                  { $items:(loops ins body) }|]

-- | Compile an 'Copy' using sequential nested loops and
-- t'Read'/t'Write' of individual scalars.  This always works, but can
-- be pretty slow if those reads and writes are costly.
compileCopy ::
  PrimType ->
  [Count Elements (TExp Int64)] ->
  (VName, Space) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  (VName, Space) ->
  ( Count Elements (TExp Int64),
    [Count Elements (TExp Int64)]
  ) ->
  CompilerM op s ()
compileCopy t shape (dst, dstspace) dst_lmad (src, srcspace) src_lmad = do
  src' <- rawMem src
  dst' <- rawMem dst
  let doWrite dst_i = generateWrite dst' dst_i t dstspace Nonvolatile
      doRead src_i = generateRead src' src_i t srcspace Nonvolatile
  compileCopyWith shape doWrite dst_lmad doRead src_lmad
