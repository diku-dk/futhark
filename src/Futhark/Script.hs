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
    valToExpValue,
    storeExpValue,

    -- * Evaluation
    EvalBuiltin,
    scriptBuiltin,
    evalExp,
    getExpValue,
    getHaskellValue,
    evalExpToGround,
    valueToExp,
    freeValue,
  )
where

import Control.Monad
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import Data.Binary qualified as Bin
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
import Language.Futhark.Tuple (areTupleFields, tupleFieldNames)
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
    scriptTypes :: TypeMap,
    scriptVars :: IORef [VarName]
  }

-- | Run an action with a 'ScriptServer' produced by an existing
-- 'Server', without shutting it down at the end.
withScriptServer' :: (MonadIO m) => Server -> (ScriptServer -> m a) -> m a
withScriptServer' server f = do
  counter <- liftIO $ newIORef 0
  vars <- liftIO $ newIORef []
  types <- typeMap server
  f $ ScriptServer server counter types vars

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
  | Project Exp T.Text
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
      pprPrec _ (Project e f) =
        pprPrec 1 e <> "." <> pretty f
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
    [ pLet,
      try $ Call <$> pFunc <*> some pAtom,
      pAtom
    ]
    <?> "expression"
  where
    pField = (,) <$> lVarName <*> (pEquals *> parseExp sep)
    pEquals = lexeme sep "="
    pComma = lexeme sep ","
    mkTuple [v] = v
    mkTuple vs = Tuple vs

    pLet =
      lexeme sep "let"
        $> Let
        <*> pPat
        <* lexeme sep "="
        <*> parseExp sep
        <*> choice
          [ lexeme sep "in" *> parseExp sep,
            pLet
          ]

    pProject e =
      choice
        [ lexeme sep "." *> (pFieldName >>= pProject . Project e),
          pure e
        ]

    pAtom =
      choice
        [ try $ inParens sep (mkTuple <$> (parseExp sep `sepEndBy` pComma)),
          inParens sep $ parseExp sep,
          inBraces sep (Record <$> (pField `sepEndBy` pComma)),
          StringLit . T.pack <$> lexeme sep ("\"" *> manyTill charLiteral "\""),
          Const <$> V.parseValue sep,
          Call <$> pFunc <*> pure []
        ]
        >>= pProject

    pPat =
      choice
        [ inParens sep $ lVarName `sepEndBy` pComma,
          pure <$> lVarName
        ]

    pFunc =
      choice
        [ FuncBuiltin <$> ("$" *> lVarName),
          FuncFut <$> lVarName
        ]

    reserved = ["let", "in"]

    lVarName = lexeme sep . try $ do
      v <- fmap T.pack $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
      guard $ v `notElem` reserved
      pure v
      where
        constituent c = isAlphaNum c || c == '\'' || c == '_'

    lIntStr = lexeme sep . try . fmap T.pack $ some $ satisfy isDigit

    pFieldName = lVarName <|> lIntStr

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

-- | Turn a purely manifested value into an 'ExpValue'.
valToExpValue :: V.CompoundValue -> ExpValue
valToExpValue = fmap $ \v ->
  SValue (V.valueTypeTextNoDims (V.valueType v)) $ VVal v

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

-- | Store the provided value to the specified file. Fails if `ExpValue` is not
-- either a primitive or a single variable stored on the server. TODO: make this
-- handle arbitrary values.
storeExpValue ::
  (MonadIO m, MonadError T.Text m) =>
  ScriptServer ->
  FilePath ->
  ExpValue ->
  m ()
storeExpValue server path (V.ValueAtom (SValue _ v)) = do
  case v of
    VVal vv' ->
      liftIO $ LBS.writeFile path $ Bin.encode vv'
    VVar vv' ->
      cmdMaybe $ cmdStore (scriptServer server) path [vv']
storeExpValue _ _ v =
  throwError $
    "Cannot store value of type " <> prettyText (fmap scriptValueType v)

-- | How to evaluate a builtin function.
type EvalBuiltin m = ScriptServer -> T.Text -> [ExpValue] -> m ExpValue

loadData ::
  (MonadIO m, MonadError T.Text m) =>
  FilePath ->
  m ExpValue
loadData datafile = do
  contents <- liftIO $ LBS.readFile datafile
  let maybe_vs = V.readValues contents
  case maybe_vs of
    Nothing ->
      throwError $ "Failed to read data file " <> T.pack datafile
    Just [v] ->
      pure $ valToExpValue $ V.ValueAtom v
    Just vs ->
      pure $ valToExpValue $ V.ValueTuple $ map V.ValueAtom vs

wrongArguments ::
  (MonadError T.Text m) => T.Text -> [ExpValue] -> m a
wrongArguments fname vs =
  throwError $
    "$"
      <> fname
      <> " does not accept arguments of types: "
      <> T.intercalate ", " (map (prettyText . fmap scriptValueType) vs)

pathArg ::
  (MonadIO m, MonadError T.Text m) =>
  FilePath ->
  ScriptServer ->
  T.Text ->
  [ExpValue] ->
  m FilePath
pathArg dir server cmd vs@[v] = do
  v' <- getHaskellValue server v
  case v' of
    Just path ->
      pure $ dir </> map (chr . fromIntegral) (path :: [Word8])
    _ ->
      wrongArguments cmd vs
pathArg _ _ cmd vs =
  wrongArguments cmd vs

newVar :: (MonadIO m) => ScriptServer -> T.Text -> m T.Text
newVar server base = liftIO $ do
  x <- readIORef counter
  modifyIORef counter (+ 1)
  let v = base <> prettyText x
  modifyIORef vars (v :)
  pure v
  where
    vars = scriptVars server
    counter = scriptCounter server

-- | Handles the following builtin functions: @loaddata@, @loadbytes@.
-- Fails for everything else. The 'FilePath' indicates the directory
-- that files should be read relative to.
scriptBuiltin :: (MonadIO m, MonadError T.Text m) => FilePath -> EvalBuiltin m
scriptBuiltin dir server "loaddata" vs =
  loadData =<< pathArg dir server "loaddata" vs
scriptBuiltin dir server "loadbytes" vs =
  fmap (V.ValueAtom . SValue "[]u8" . VVal . V.putValue1) . liftIO . BS.readFile
    =<< pathArg dir server "loadbytes" vs
scriptBuiltin dir server "restore" vs
  | [tv, fv] <- vs = do
      tv' <- getHaskellValue server tv
      fv' <- getHaskellValue server fv
      case (tv', fv') of
        (Just tname, Just fname) -> do
          let tname' = T.pack $ map (chr . fromIntegral) (tname :: [Word8])
              fname' = dir </> map (chr . fromIntegral) (fname :: [Word8])
          v <- newVar server "restore"
          cmdMaybe $ cmdRestore (scriptServer server) fname' [(v, tname')]
          pure $ V.ValueAtom $ SValue tname' $ VVar v
        _ ->
          wrongArguments "restore" vs
  | otherwise =
      wrongArguments "restore" vs
scriptBuiltin _ _ f _ =
  throwError $ "Unknown builtin function $" <> prettyText f

-- | Symbol table used for local variable lookups during expression evaluation.
type VTable = M.Map VarName ExpValue

cannotApply ::
  (MonadError T.Text m, Pretty a, Pretty b) =>
  T.Text ->
  [a] ->
  [b] ->
  m c
cannotApply fname expected actual =
  throwError $
    "Function \""
      <> fname
      <> "\" expects "
      <> prettyText (length expected)
      <> " argument(s) of types:\n"
      <> T.intercalate "\n" (map prettyTextOneLine expected)
      <> "\nBut applied to "
      <> prettyText (length actual)
      <> " argument(s) of types:\n"
      <> T.intercalate "\n" (map prettyTextOneLine actual)

getField ::
  (MonadIO m, MonadError T.Text m) =>
  ScriptServer ->
  T.Text ->
  (Name, b) ->
  m VarName
getField server from (f, _) = do
  to <- newVar server "field"
  cmdMaybe $ cmdProject (scriptServer server) to from $ nameToText f
  pure to

unTuple ::
  (MonadIO m, MonadError T.Text m) =>
  ScriptServer ->
  ExpValue ->
  m [ExpValue]
unTuple _ (V.ValueTuple vs) = pure vs
unTuple server (V.ValueAtom (SValue t (VVar v)))
  | Just ts <- isTuple t $ scriptTypes server =
      forM (zip tupleFieldNames ts) $ \(k, kt) ->
        V.ValueAtom . SValue kt . VVar <$> getField server v (k, kt)
unTuple _ v = pure [v]

project ::
  (MonadIO m, MonadError T.Text m) =>
  ScriptServer ->
  ExpValue ->
  T.Text ->
  m ExpValue
project _ (V.ValueRecord fs) k =
  case M.lookup k fs of
    Nothing -> throwError $ "Unknown field: " <> k
    Just v -> pure v
project server (V.ValueAtom (SValue t (VVar v))) f
  | Just fs <- isRecord t $ scriptTypes server =
      case lookup f' fs of
        Nothing -> throwError $ "Type " <> t <> " does not have a field " <> f <> "."
        Just ft ->
          V.ValueAtom . SValue ft . VVar <$> getField server v (f', ft)
  where
    f' = nameFromText f
project _ _ _ =
  throwError "Cannot project from non-record."

-- | Evaluate a FutharkScript expression relative to some running server.
evalExp ::
  forall m.
  (MonadError T.Text m, MonadIO m) =>
  EvalBuiltin m ->
  ScriptServer ->
  Exp ->
  m ExpValue
evalExp builtin sserver top_level_e = do
  let ( ScriptServer
          { scriptServer = server,
            scriptTypes = types,
            scriptVars = vars
          }
        ) = sserver
  old_vars <- liftIO $ readIORef vars
  let newVar' = newVar sserver

      mkRecord t vs = do
        v <- newVar' "record"
        cmdMaybe $ cmdNew server v t vs
        pure v

      toVar :: ValOrVar -> m VarName
      toVar (VVar v) = pure v
      toVar (VVal val) = do
        v <- newVar' "const"
        writeVar server v val
        pure v

      scriptValueToValOrVar (SFun f _ _ _) =
        throwError $ "Function " <> f <> " not fully applied."
      scriptValueToValOrVar (SValue _ v) =
        pure v

      scriptValueToVar :: ScriptValue ValOrVar -> m VarName
      scriptValueToVar = toVar <=< scriptValueToValOrVar

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
            mkRecord t =<< mapM (getField sserver v) vt_fs
      interValToVar _ t (V.ValueAtom (SValue _ (VVal v)))
        | Just v' <- coerceValue t v =
            scriptValueToVar $ SValue t $ VVal v'
      interValToVar bad _ _ = bad

      letMatch :: [VarName] -> ExpValue -> m VTable
      letMatch vs val = do
        vals <- unTuple sserver val
        if length vs == length vs
          then
            pure $ M.fromList (zip vs vals)
          else
            throwError $
              "Pat: "
                <> prettyTextOneLine vs
                <> "\nDoes not match value of type: "
                <> prettyTextOneLine (fmap scriptValueType val)

      evalExp' :: VTable -> Exp -> m ExpValue
      evalExp' _ (ServerVar t v) =
        pure $ V.ValueAtom $ SValue t $ VVar v
      evalExp' vtable (Project e f) = do
        e' <- evalExp' vtable e
        project sserver e' f
      evalExp' vtable (Call (FuncBuiltin name) es) =
        builtin sserver name =<< mapM (evalExp' vtable) es
      evalExp' vtable (Call (FuncFut name) es)
        | Just e <- M.lookup name vtable = do
            unless (null es) $
              throwError $
                "Locally bound name cannot be invoked as a function: " <> prettyText name
            pure e
        | otherwise = do
            in_types <- fmap (map inputType) $ cmdEither $ cmdInputs server name
            out_types <- fmap (map outputType) $ cmdEither $ cmdOutputs server name

            es' <- mapM (evalExp' vtable) es

            let bad = cannotApply name in_types $ map (fmap scriptValueType) es'
                tryApply args = do
                  arg_types <- zipWithM (interValToVar bad) in_types args

                  if length in_types == length arg_types
                    then do
                      outs <- replicateM (length out_types) $ newVar' "out"
                      void $ cmdEither $ cmdCall server name outs arg_types
                      pure . V.mkCompound . map V.ValueAtom $
                        zipWith SValue out_types (map VVar outs)
                    else
                      pure . V.ValueAtom . SFun name in_types out_types $
                        zipWith SValue in_types (map VVar arg_types)

            -- Careful to not require saturated application, but do still
            -- check for over-saturation.
            when (length es > length in_types) bad

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
        let keep_vars = serverVarsInValue v <> S.fromList old_vars
        to_free <- liftIO $ filter (`S.notMember` keep_vars) <$> readIORef vars
        cmdMaybe $ cmdFree server to_free
        liftIO $ writeIORef vars $ S.toList keep_vars
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

primArrayType :: TypeName -> Bool
primArrayType s = case fmap T.uncons <$> T.uncons s of
  Just ('[', Just (']', s')) -> primArrayType s'
  _ -> s `elem` ["i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f16", "f32", "f64", "bool"]

-- | Read actual compound values from the server. Fails for values that have no
-- well-defined external representation.
getExpValue ::
  (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m V.CompoundValue
getExpValue _ (V.ValueAtom (SFun fname _ _ _)) =
  throwError $ "Function " <> fname <> " not fully applied."
getExpValue server (V.ValueAtom (SValue t (VVar v)))
  | Just fs <- isRecord t types =
      tupleOrRecord . M.fromList . zip (map fst fs)
        <$> mapM (onField v) fs
  | not $ primArrayType t =
      throwError $ "Type " <> t <> " has no external representation."
  | otherwise =
      V.ValueAtom <$> readVar (scriptServer server) v
  where
    types = scriptTypes server

    tupleOrRecord m =
      maybe (V.ValueRecord $ M.mapKeys nameToText m) V.ValueTuple $ areTupleFields m

    onField from (f, ft) = do
      to <- getField server from (f, ft)
      getExpValue server $ V.ValueAtom $ SValue ft $ VVar to
getExpValue server (V.ValueTuple vs) =
  V.ValueTuple <$> traverse (getExpValue server) vs
getExpValue server (V.ValueRecord fs) =
  V.ValueRecord <$> traverse (getExpValue server) fs
getExpValue _ (V.ValueAtom (SValue _ (VVal v))) = pure $ V.ValueAtom v

-- | Retrieve a Haskell value from an 'ExpValue'. This returns 'Just' if the
-- 'ExpValue' is an atom with a non-opaque type.
getHaskellValue :: (V.GetValue t, MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m (Maybe t)
getHaskellValue server v = do
  v' <- getExpValue server v
  case v' of
    V.ValueAtom v'' ->
      pure $ V.getValue v''
    _ -> pure Nothing

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
varsInExp (Project e _) = varsInExp e
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
