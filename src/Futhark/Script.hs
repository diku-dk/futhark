-- | FutharkScript is a (tiny) subset of Futhark used to write small
-- expressions that are evaluated by server executables.  The @futhark
-- literate@ command is the main user.
module Futhark.Script
  ( -- * Server
    ScriptServer,
    withScriptServer,
    withScriptServer',

    -- * Expressions, values, and types
    Func (..),
    Exp (..),
    parseExp,
    parseExpFromText,
    varsInExp,
    ScriptValueType (..),
    ScriptValue (..),
    scriptValueType,
    serverVarsInValue,
    ValOrVar (..),
    ExpValue,

    -- * Evaluation
    EvalBuiltin,
    scriptBuiltin,
    evalExp,
    getExpValue,
    evalExpToGround,
    valueToExp,
    freeValue,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char
import Data.Foldable (toList)
import Data.Functor
import Data.IORef
import Data.List (intersperse)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Storable qualified as SVec
import Data.Void
import Data.Word (Word8)
import Futhark.Data.Parser qualified as V
import Futhark.Server
import Futhark.Server.Values (getValue, putValue)
import Futhark.Test.Values qualified as V
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (line, sep, space, (</>))
import Language.Futhark.Core (Name, nameFromText, nameToText)
import Language.Futhark.Tuple (areTupleFields)
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (charLiteral)

type TypeMap = M.Map TypeName (Maybe [(Name, TypeName)])

typeMap :: (MonadIO m) => Server -> m TypeMap
typeMap server = do
  liftIO $ either (pure mempty) onTypes =<< cmdTypes server
  where
    onTypes types = M.fromList . zip types <$> mapM onType types
    onType t =
      either (const Nothing) (Just . map onField) <$> cmdFields server t
    onField = bimap nameFromText (T.drop 1) . T.breakOn " "

isRecord :: TypeName -> TypeMap -> Maybe [(Name, TypeName)]
isRecord t m = join $ M.lookup t m

isTuple :: TypeName -> TypeMap -> Maybe [TypeName]
isTuple t m = areTupleFields . M.fromList =<< isRecord t m

-- | Like a 'Server', but keeps a bit more state to make FutharkScript
-- more convenient.
data ScriptServer = ScriptServer
  { scriptServer :: Server,
    scriptCounter :: IORef Int,
    scriptTypes :: TypeMap
  }

-- | Run an action with a 'ScriptServer' produced by an existing
-- 'Server', without shutting it down at the end.
withScriptServer' :: (MonadIO m) => Server -> (ScriptServer -> m a) -> m a
withScriptServer' server f = do
  counter <- liftIO $ newIORef 0
  types <- typeMap server
  f $ ScriptServer server counter types

-- | Start a server, execute an action, then shut down the server.
-- Similar to 'withServer'.
withScriptServer :: ServerCfg -> (ScriptServer -> IO a) -> IO a
withScriptServer cfg f =
  withServer cfg $ flip withScriptServer' f

-- | A function called in a 'Call' expression can be either a Futhark
-- function or a builtin function.
data Func = FuncFut EntryName | FuncBuiltin T.Text
  deriving (Show)

-- | A FutharkScript expression.  This is a simple AST that might not
-- correspond exactly to what the user wrote (e.g. no parentheses or
-- source locations).  This is fine for small expressions, which is
-- all this is meant for.
data Exp
  = Call Func [Exp]
  | Const V.Value
  | Tuple [Exp]
  | Record [(T.Text, Exp)]
  | StringLit T.Text
  | Let [VarName] Exp Exp
  | -- | Server-side variable, *not* Futhark variable (these are
    -- handled in 'Call').
    ServerVar TypeName VarName
  deriving (Show)

instance Pretty Func where
  pretty (FuncFut f) = pretty f
  pretty (FuncBuiltin f) = "$" <> pretty f

instance Pretty Exp where
  pretty = pprPrec (0 :: Int)
    where
      pprPrec _ (ServerVar _ v) = "$" <> pretty v
      pprPrec _ (Const v) = stack $ map pretty $ T.lines $ V.valueText v
      pprPrec i (Let pat e1 e2) =
        parensIf (i > 0) $ "let" <+> pat' <+> equals <+> pretty e1 <+> "in" <+> pretty e2
        where
          pat' = case pat of
            [x] -> pretty x
            _ -> parens $ align $ commasep $ map pretty pat
      pprPrec _ (Call v []) = pretty v
      pprPrec i (Call v args) =
        parensIf (i > 0) $ pretty v <+> hsep (map (align . pprPrec 1) args)
      pprPrec _ (Tuple vs) =
        parens $ commasep $ map (align . pretty) vs
      pprPrec _ (StringLit s) = pretty $ show s
      pprPrec _ (Record m) = braces $ align $ commasep $ map field m
        where
          field (k, v) = align (pretty k <> equals <> pretty v)

type Parser = Parsec Void T.Text

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

inParens :: Parser () -> Parser a -> Parser a
inParens sep = between (lexeme sep "(") (lexeme sep ")")

inBraces :: Parser () -> Parser a -> Parser a
inBraces sep = between (lexeme sep "{") (lexeme sep "}")

-- | Parse a FutharkScript expression, given a whitespace parser.
parseExp :: Parsec Void T.Text () -> Parsec Void T.Text Exp
parseExp sep =
  choice
    [ lexeme sep "let"
        $> Let
        <*> pPat
        <* lexeme sep "="
        <*> parseExp sep
        <* lexeme sep "in"
        <*> parseExp sep,
      try $ Call <$> parseFunc <*> many pAtom,
      pAtom
    ]
    <?> "expression"
  where
    pField = (,) <$> pVarName <*> (pEquals *> parseExp sep)
    pEquals = lexeme sep "="
    pComma = lexeme sep ","
    mkTuple [v] = v
    mkTuple vs = Tuple vs

    pAtom =
      choice
        [ try $ inParens sep (mkTuple <$> (parseExp sep `sepBy` pComma)),
          inParens sep $ parseExp sep,
          inBraces sep (Record <$> (pField `sepBy` pComma)),
          StringLit . T.pack <$> lexeme sep ("\"" *> manyTill charLiteral "\""),
          Const <$> V.parseValue sep,
          Call <$> parseFunc <*> pure []
        ]

    pPat =
      choice
        [ inParens sep $ pVarName `sepBy` pComma,
          pure <$> pVarName
        ]

    parseFunc =
      choice
        [ FuncBuiltin <$> ("$" *> pVarName),
          FuncFut <$> pVarName
        ]

    reserved = ["let", "in"]

    pVarName = lexeme sep . try $ do
      v <- fmap T.pack $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
      guard $ v `notElem` reserved
      pure v
      where
        constituent c = isAlphaNum c || c == '\'' || c == '_'

-- | Parse a FutharkScript expression with normal whitespace handling.
parseExpFromText :: FilePath -> T.Text -> Either T.Text Exp
parseExpFromText f s =
  either (Left . T.pack . errorBundlePretty) Right $ parse (parseExp space <* eof) f s

readVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> m V.Value
readVar server v =
  either throwError pure =<< liftIO (getValue server v)

writeVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> V.Value -> m ()
writeVar server v val =
  cmdMaybe $ liftIO (putValue server v val)

-- | A ScriptValue is either a base value or a partially applied
-- function.  We don't have real first-class functions in
-- FutharkScript, but we sort of have closures.
data ScriptValue v
  = SValue TypeName v
  | -- | Ins, then outs.  Yes, this is the opposite of more or less
    -- everywhere else.
    SFun EntryName [TypeName] [TypeName] [ScriptValue v]
  deriving (Show)

instance Functor ScriptValue where
  fmap = fmapDefault

instance Foldable ScriptValue where
  foldMap = foldMapDefault

instance Traversable ScriptValue where
  traverse f (SValue t v) = SValue t <$> f v
  traverse f (SFun fname ins outs vs) =
    SFun fname ins outs <$> traverse (traverse f) vs

-- | The type of a 'ScriptValue' - either a value type or a function type.
data ScriptValueType
  = STValue TypeName
  | -- | Ins, then outs.
    STFun [TypeName] [TypeName]
  deriving (Eq, Show)

instance Pretty ScriptValueType where
  pretty (STValue t) = pretty t
  pretty (STFun ins outs) =
    hsep $ intersperse "->" (map pretty ins ++ [outs'])
    where
      outs' = case outs of
        [out] -> pretty out
        _ -> parens $ commasep $ map pretty outs

-- | A Haskell-level value or a variable on the server.
data ValOrVar = VVal V.Value | VVar VarName
  deriving (Show)

-- | The intermediate values produced by an expression - in
-- particular, these may not be on the server.
type ExpValue = V.Compound (ScriptValue ValOrVar)

-- | The type of a 'ScriptValue'.
scriptValueType :: ScriptValue v -> ScriptValueType
scriptValueType (SValue t _) = STValue t
scriptValueType (SFun _ ins outs _) = STFun ins outs

-- | The set of server-side variables in the value.
serverVarsInValue :: ExpValue -> S.Set VarName
serverVarsInValue = S.fromList . concatMap isVar . toList
  where
    isVar (SValue _ (VVar x)) = [x]
    isVar (SValue _ (VVal _)) = []
    isVar (SFun _ _ _ closure) = concatMap isVar $ toList closure

-- | Convert a value into a corresponding expression.
valueToExp :: ExpValue -> Exp
valueToExp (V.ValueAtom (SValue t (VVar v))) =
  ServerVar t v
valueToExp (V.ValueAtom (SValue _ (VVal v))) =
  Const v
valueToExp (V.ValueAtom (SFun fname _ _ closure)) =
  Call (FuncFut fname) $ map (valueToExp . V.ValueAtom) closure
valueToExp (V.ValueRecord fs) =
  Record $ M.toList $ M.map valueToExp fs
valueToExp (V.ValueTuple fs) =
  Tuple $ map valueToExp fs

-- Decompose a type name into a rank and an element type.
parseTypeName :: TypeName -> Maybe (Int, V.PrimType)
parseTypeName s
  | Just pt <- lookup s m =
      Just (0, pt)
  | "[]" `T.isPrefixOf` s = do
      (d, pt) <- parseTypeName (T.drop 2 s)
      pure (d + 1, pt)
  | otherwise = Nothing
  where
    prims = [minBound .. maxBound]
    primtexts = map (V.valueTypeText . V.ValueType []) prims
    m = zip primtexts prims

coerceValue :: TypeName -> V.Value -> Maybe V.Value
coerceValue t v = do
  (_, pt) <- parseTypeName t
  case v of
    V.I8Value shape vs ->
      coerceInts pt shape $ map toInteger $ SVec.toList vs
    V.I16Value shape vs ->
      coerceInts pt shape $ map toInteger $ SVec.toList vs
    V.I32Value shape vs ->
      coerceInts pt shape $ map toInteger $ SVec.toList vs
    V.I64Value shape vs ->
      coerceInts pt shape $ map toInteger $ SVec.toList vs
    _ ->
      Nothing
  where
    coerceInts V.I8 shape =
      Just . V.I8Value shape . SVec.fromList . map fromInteger
    coerceInts V.I16 shape =
      Just . V.I16Value shape . SVec.fromList . map fromInteger
    coerceInts V.I32 shape =
      Just . V.I32Value shape . SVec.fromList . map fromInteger
    coerceInts V.I64 shape =
      Just . V.I64Value shape . SVec.fromList . map fromInteger
    coerceInts V.F32 shape =
      Just . V.F32Value shape . SVec.fromList . map fromInteger
    coerceInts V.F64 shape =
      Just . V.F64Value shape . SVec.fromList . map fromInteger
    coerceInts _ _ =
      const Nothing

-- | How to evaluate a builtin function.
type EvalBuiltin m = T.Text -> [V.CompoundValue] -> m V.CompoundValue

loadData ::
  (MonadIO m, MonadError T.Text m) =>
  FilePath ->
  m (V.Compound V.Value)
loadData datafile = do
  contents <- liftIO $ LBS.readFile datafile
  let maybe_vs = V.readValues contents
  case maybe_vs of
    Nothing ->
      throwError $ "Failed to read data file " <> T.pack datafile
    Just [v] ->
      pure $ V.ValueAtom v
    Just vs ->
      pure $ V.ValueTuple $ map V.ValueAtom vs

pathArg ::
  (MonadError T.Text f) =>
  FilePath ->
  T.Text ->
  [V.Compound V.Value] ->
  f FilePath
pathArg dir cmd vs =
  case vs of
    [V.ValueAtom v]
      | Just path <- V.getValue v ->
          pure $ dir </> map (chr . fromIntegral) (path :: [Word8])
    _ ->
      throwError $
        "$"
          <> cmd
          <> " does not accept arguments of types: "
          <> T.intercalate ", " (map (prettyText . fmap V.valueType) vs)

-- | Handles the following builtin functions: @loaddata@, @loadbytes@.
-- Fails for everything else. The 'FilePath' indicates the directory
-- that files should be read relative to.
scriptBuiltin :: (MonadIO m, MonadError T.Text m) => FilePath -> EvalBuiltin m
scriptBuiltin dir "loaddata" vs = do
  loadData =<< pathArg dir "loaddata" vs
scriptBuiltin dir "loadbytes" vs = do
  fmap (V.ValueAtom . V.putValue1) . liftIO . BS.readFile
    =<< pathArg dir "loadbytes" vs
scriptBuiltin _ f _ =
  throwError $ "Unknown builtin function $" <> prettyText f

-- | Symbol table used for local variable lookups during expression evaluation.
type VTable = M.Map VarName ExpValue

-- | Evaluate a FutharkScript expression relative to some running server.
evalExp ::
  forall m.
  (MonadError T.Text m, MonadIO m) =>
  EvalBuiltin m ->
  ScriptServer ->
  Exp ->
  m ExpValue
evalExp builtin sserver top_level_e = do
  vars <- liftIO $ newIORef []
  let ( ScriptServer
          { scriptServer = server,
            scriptCounter = counter,
            scriptTypes = types
          }
        ) = sserver
      newVar base = liftIO $ do
        x <- readIORef counter
        modifyIORef counter (+ 1)
        let v = base <> prettyText x
        modifyIORef vars (v :)
        pure v

      mkRecord t vs = do
        v <- newVar "record"
        cmdMaybe $ cmdNew server v t vs
        pure v

      getField from (f, _) = do
        to <- newVar "field"
        cmdMaybe $ cmdProject server to from $ nameToText f
        pure to

      toVal :: ValOrVar -> m V.Value
      toVal (VVal v) = pure v
      toVal (VVar v) = readVar server v

      toVar :: ValOrVar -> m VarName
      toVar (VVar v) = pure v
      toVar (VVal val) = do
        v <- newVar "const"
        writeVar server v val
        pure v

      scriptValueToValOrVar (SFun f _ _ _) =
        throwError $ "Function " <> f <> " not fully applied."
      scriptValueToValOrVar (SValue _ v) =
        pure v

      scriptValueToVal :: ScriptValue ValOrVar -> m V.Value
      scriptValueToVal = toVal <=< scriptValueToValOrVar

      scriptValueToVar :: ScriptValue ValOrVar -> m VarName
      scriptValueToVar = toVar <=< scriptValueToValOrVar

      interValToVal :: ExpValue -> m V.CompoundValue
      interValToVal = traverse scriptValueToVal

      -- Apart from type checking, this function also converts
      -- FutharkScript tuples/records to Futhark-level tuples/records,
      -- as well as maps between different names for the same
      -- tuple/record.
      --
      -- We also implicitly convert the types of constants.
      interValToVar :: m VarName -> TypeName -> ExpValue -> m VarName
      interValToVar _ t (V.ValueAtom v)
        | STValue t == scriptValueType v = scriptValueToVar v
      interValToVar bad t (V.ValueTuple vs)
        | Just ts <- isTuple t types,
          length vs == length ts =
            mkRecord t =<< zipWithM (interValToVar bad) ts vs
      interValToVar bad t (V.ValueRecord vs)
        | Just fs <- isRecord t types,
          Just vs' <- mapM ((`M.lookup` vs) . nameToText . fst) fs =
            mkRecord t =<< zipWithM (interValToVar bad) (map snd fs) vs'
      interValToVar _ t (V.ValueAtom (SValue vt (VVar v)))
        | Just t_fs <- isRecord t types,
          Just vt_fs <- isRecord vt types,
          vt_fs == t_fs =
            mkRecord t =<< mapM (getField v) vt_fs
      interValToVar _ t (V.ValueAtom (SValue _ (VVal v)))
        | Just v' <- coerceValue t v =
            scriptValueToVar $ SValue t $ VVal v'
      interValToVar bad _ _ = bad

      valToInterVal :: V.CompoundValue -> ExpValue
      valToInterVal = fmap $ \v ->
        SValue (V.valueTypeTextNoDims (V.valueType v)) $ VVal v

      letMatch :: [VarName] -> ExpValue -> m VTable
      letMatch vs val
        | vals <- V.unCompound val,
          length vs == length vals =
            pure $ M.fromList (zip vs vals)
        | otherwise =
            throwError $
              "Pat: "
                <> prettyTextOneLine vs
                <> "\nDoes not match value of type: "
                <> prettyTextOneLine (fmap scriptValueType val)

      evalExp' :: VTable -> Exp -> m ExpValue
      evalExp' _ (ServerVar t v) =
        pure $ V.ValueAtom $ SValue t $ VVar v
      evalExp' vtable (Call (FuncBuiltin name) es) = do
        v <- builtin name =<< mapM (interValToVal <=< evalExp' vtable) es
        pure $ valToInterVal v
      evalExp' vtable (Call (FuncFut name) es)
        | Just e <- M.lookup name vtable = do
            unless (null es) $
              throwError $
                "Locally bound name cannot be invoked as a function: " <> prettyText name
            pure e
      evalExp' vtable (Call (FuncFut name) es) = do
        in_types <- fmap (map inputType) $ cmdEither $ cmdInputs server name
        out_types <- fmap (map outputType) $ cmdEither $ cmdOutputs server name

        es' <- mapM (evalExp' vtable) es
        let es_types = map (fmap scriptValueType) es'

        let cannotApply =
              throwError $
                "Function \""
                  <> name
                  <> "\" expects "
                  <> prettyText (length in_types)
                  <> " argument(s) of types:\n"
                  <> T.intercalate "\n" (map prettyTextOneLine in_types)
                  <> "\nBut applied to "
                  <> prettyText (length es_types)
                  <> " argument(s) of types:\n"
                  <> T.intercalate "\n" (map prettyTextOneLine es_types)

            tryApply args = do
              arg_types <- zipWithM (interValToVar cannotApply) in_types args

              if length in_types == length arg_types
                then do
                  outs <- replicateM (length out_types) $ newVar "out"
                  void $ cmdEither $ cmdCall server name outs arg_types
                  pure $ V.mkCompound $ map V.ValueAtom $ zipWith SValue out_types $ map VVar outs
                else
                  pure . V.ValueAtom . SFun name in_types out_types $
                    zipWith SValue in_types $
                      map VVar arg_types

        -- Careful to not require saturated application, but do still
        -- check for over-saturation.
        when (length es_types > length in_types) cannotApply

        -- Allow automatic uncurrying if applicable.
        case es' of
          [V.ValueTuple es''] | length es'' == length in_types -> tryApply es''
          _ -> tryApply es'
      evalExp' _ (StringLit s) =
        case V.putValue s of
          Just s' ->
            pure $ V.ValueAtom $ SValue (V.valueTypeTextNoDims (V.valueType s')) $ VVal s'
          Nothing -> error $ "Unable to write value " ++ prettyString s
      evalExp' _ (Const val) =
        pure $ V.ValueAtom $ SValue (V.valueTypeTextNoDims (V.valueType val)) $ VVal val
      evalExp' vtable (Tuple es) =
        V.ValueTuple <$> mapM (evalExp' vtable) es
      evalExp' vtable e@(Record m) = do
        when (length (nubOrd (map fst m)) /= length (map fst m)) $
          throwError $
            "Record " <> prettyText e <> " has duplicate fields."
        V.ValueRecord <$> traverse (evalExp' vtable) (M.fromList m)
      evalExp' vtable (Let pat e1 e2) = do
        v <- evalExp' vtable e1
        pat_vtable <- letMatch pat v
        evalExp' (pat_vtable <> vtable) e2

  let freeNonresultVars v = do
        let v_vars = serverVarsInValue v
        to_free <- liftIO $ filter (`S.notMember` v_vars) <$> readIORef vars
        cmdMaybe $ cmdFree server to_free
        pure v
      freeVarsOnError e = do
        -- We are intentionally ignoring any errors produced by
        -- cmdFree, because we already have another error to
        -- propagate.  Also, not all of the variables that we put in
        -- 'vars' might actually exist server-side, if we failed in a
        -- Call.
        void $ liftIO $ cmdFree server =<< readIORef vars
        throwError e
  (freeNonresultVars =<< evalExp' mempty top_level_e) `catchError` freeVarsOnError

-- | Read actual values from the server.  Fails for values that have
-- no well-defined external representation.
getExpValue ::
  (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m V.CompoundValue
getExpValue server e =
  traverse toGround =<< traverse (traverse onLeaf) e
  where
    onLeaf (VVar v) = readVar (scriptServer server) v
    onLeaf (VVal v) = pure v
    toGround (SFun fname _ _ _) =
      throwError $ "Function " <> fname <> " not fully applied."
    toGround (SValue _ v) = pure v

-- | Like 'evalExp', but requires all values to be non-functional.  If
-- the value has a bad type, return that type instead.  Other
-- evaluation problems (e.g. type failures) raise errors.
evalExpToGround ::
  (MonadError T.Text m, MonadIO m) =>
  EvalBuiltin m ->
  ScriptServer ->
  Exp ->
  m (Either (V.Compound ScriptValueType) V.CompoundValue)
evalExpToGround builtin server e = do
  v <- evalExp builtin server e
  -- This assumes that the only error that can occur during
  -- getExpValue is trying to read an opaque.
  (Right <$> getExpValue server v)
    `catchError` const (pure $ Left $ fmap scriptValueType v)

-- | The set of Futhark variables that are referenced by the
-- expression - these will have to be entry points in the Futhark
-- program.
varsInExp :: Exp -> S.Set EntryName
varsInExp ServerVar {} = mempty
varsInExp (Call (FuncFut v) es) = S.insert v $ foldMap varsInExp es
varsInExp (Call (FuncBuiltin _) es) = foldMap varsInExp es
varsInExp (Tuple es) = foldMap varsInExp es
varsInExp (Record fs) = foldMap (foldMap varsInExp) fs
varsInExp Const {} = mempty
varsInExp StringLit {} = mempty
varsInExp (Let pat e1 e2) = varsInExp e1 <> S.filter (`notElem` pat) (varsInExp e2)

-- | Release all the server-side variables in the value.  Yes,
-- FutharkScript has manual memory management...
freeValue :: (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m ()
freeValue server =
  cmdMaybe . cmdFree (scriptServer server) . S.toList . serverVarsInValue
