{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | FutharkScript is a (tiny) subset of Futhark used to write small
-- expressions that are evaluated by server executables.  The @futhark
-- literate@ command is the main user.
module Futhark.Script
  ( -- * Server
    ScriptServer,
    withScriptServer,

    -- * Expressions, values, and types
    Func (..),
    Exp (..),
    parseExp,
    varsInExp,
    ScriptValueType (..),
    ScriptValue (..),
    scriptValueType,
    ExpValue,

    -- * Evaluation
    EvalBuiltin,
    evalExp,
    getExpValue,
    evalExpToGround,
    valueToExp,
    freeValue,
  )
where

import Control.Monad.Except
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Foldable (toList)
import Data.IORef
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.Void
import Futhark.Server
import qualified Futhark.Test.Values as V
import qualified Futhark.Test.Values.Parser as V
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (float, line, sep, string, (</>), (<|>))
import Language.Futhark.Prop (primValueType)
import Language.Futhark.Syntax (PrimValue (..))
import System.IO
import System.IO.Temp
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer (charLiteral)

-- | Like a 'Server', but keeps a bit more state to make FutharkScript
-- more convenient.
data ScriptServer = ScriptServer Server (IORef Int)

-- | Start a server, execute an action, then shut down the server.
-- Similar to 'withServer'.
withScriptServer :: FilePath -> [FilePath] -> (ScriptServer -> IO a) -> IO a
withScriptServer prog options f = withServer prog options $ \server -> do
  counter <- newIORef 0
  f $ ScriptServer server counter

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
  | Const PrimValue
  | Tuple [Exp]
  | Record [(T.Text, Exp)]
  | StringLit T.Text
  | -- | Server-side variable, *not* Futhark variable (these are
    -- handled in 'Call').
    ServerVar TypeName VarName
  deriving (Show)

instance Pretty Func where
  ppr (FuncFut f) = ppr f
  ppr (FuncBuiltin f) = "$" <> ppr f

instance Pretty Exp where
  ppr = pprPrec 0
  pprPrec _ (ServerVar _ v) = "$" <> ppr v
  pprPrec _ (Const v) = ppr v
  pprPrec _ (Call v []) = ppr v
  pprPrec i (Call v args) =
    parensIf (i > 0) $ ppr v <+> spread (map (pprPrec 1) args)
  pprPrec _ (Tuple vs) =
    parens $ commasep $ map ppr vs
  pprPrec _ (StringLit s) = ppr $ show s
  pprPrec _ (Record m) = braces $ commasep $ map field m
    where
      field (k, v) = ppr k <> equals <> ppr v

type Parser = Parsec Void T.Text

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

inParens :: Parser () -> Parser a -> Parser a
inParens sep = between (lexeme sep "(") (lexeme sep ")")

inBraces :: Parser () -> Parser a -> Parser a
inBraces sep = between (lexeme sep "{") (lexeme sep "}")

-- | Parse a FutharkScript expression.
parseExp :: Parser () -> Parser Exp
parseExp sep =
  choice
    [ inParens sep (mkTuple <$> (parseExp sep `sepBy` pComma)),
      inBraces sep (Record <$> (pField `sepBy` pComma)),
      Call <$> lexeme sep parseFunc <*> many (parseExp sep),
      Const <$> lexeme sep V.parsePrimValue,
      StringLit . T.pack <$> lexeme sep ("\"" *> manyTill charLiteral "\"")
    ]
  where
    pField = (,) <$> parseEntryName <*> (pEquals *> parseExp sep)
    pEquals = lexeme sep "="
    pComma = lexeme sep ","
    mkTuple [v] = v
    mkTuple vs = Tuple vs

    parseFunc =
      choice
        [ FuncBuiltin <$> ("$" *> parseEntryName),
          FuncFut <$> parseEntryName
        ]

    parseEntryName =
      fmap T.pack $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
      where
        constituent c = isAlphaNum c || c == '_'

prettyFailure :: CmdFailure -> T.Text
prettyFailure (CmdFailure bef aft) =
  T.unlines $ bef ++ aft

cmdMaybe :: (MonadError T.Text m, MonadIO m) => IO (Maybe CmdFailure) -> m ()
cmdMaybe m = maybe (pure ()) (throwError . prettyFailure) =<< liftIO m

cmdEither :: (MonadError T.Text m, MonadIO m) => IO (Either CmdFailure a) -> m a
cmdEither m = either (throwError . prettyFailure) pure =<< liftIO m

readVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> m V.Value
readVar server v =
  either throwError pure <=< liftIO $
    withSystemTempFile "futhark-server-read" $ \tmpf tmpf_h -> do
      hClose tmpf_h
      store_res <- cmdStore server tmpf [v]
      case store_res of
        Just err -> pure $ Left $ prettyFailure err
        Nothing -> do
          s <- LBS.readFile tmpf
          case V.readValues s of
            Just [val] -> pure $ Right val
            Just [] -> pure $ Left "Cannot read opaque value from Futhark server."
            _ -> pure $ Left "Invalid data file produced by Futhark server."

writeVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> V.Value -> m ()
writeVar server v val =
  cmdMaybe . liftIO . withSystemTempFile "futhark-server-write" $ \tmpf tmpf_h -> do
    LBS.hPutStr tmpf_h $ Bin.encode val
    hClose tmpf_h
    -- We are not using prettyprinting for the type, because we don't
    -- want the sizes of the dimensions.
    let V.ValueType dims t = V.valueType val
        t' = mconcat (map (const "[]") dims) <> prettyText t
    cmdRestore server tmpf [(v, t')]

-- | A ScriptValue is either a base value or a partially applied
-- function.  We don't have real first-class functions in
-- FutharkScript, but we sort of have closures.
data ScriptValue v
  = SValue TypeName v
  | -- | Ins, then outs.  Yes, this is the opposite of more or less
    -- everywhere else.
    SFun EntryName [TypeName] [TypeName] [ScriptValue v]

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
  ppr (STValue t) = ppr t
  ppr (STFun ins outs) =
    spread $ intersperse "->" (map ppr ins ++ [outs'])
    where
      outs' = case outs of
        [out] -> strictText out
        _ -> parens $ commasep $ map strictText outs

data ValOrVar = VVal V.Value | VVar VarName
  deriving (Show)

-- | The intermediate values used during expression evaluation - in
-- particular, these may not be on the server.
type InterValue = V.Compound (ScriptValue ValOrVar)

-- | The value that is produced by expression evaluation.  This
-- representation keeps all values on the server.
type ExpValue = V.Compound (ScriptValue VarName)

-- | The type of a 'ScriptValue'.
scriptValueType :: ScriptValue v -> ScriptValueType
scriptValueType (SValue t _) = STValue t
scriptValueType (SFun _ ins outs _) = STFun ins outs

serverVarsInValue :: ExpValue -> S.Set VarName
serverVarsInValue = S.fromList . concatMap isVar . toList
  where
    isVar (SValue _ x) = [x]
    isVar (SFun _ _ _ closure) = concatMap isVar $ toList closure

-- | Convert a value into a corresponding expression.
valueToExp :: ExpValue -> Exp
valueToExp (V.ValueAtom (SValue t v)) =
  ServerVar t v
valueToExp (V.ValueAtom (SFun fname _ _ closure)) =
  Call (FuncFut fname) $ map (valueToExp . V.ValueAtom) closure
valueToExp (V.ValueRecord fs) =
  Record $ M.toList $ M.map valueToExp fs
valueToExp (V.ValueTuple fs) =
  Tuple $ map valueToExp fs

-- | How to evaluate a builtin function.
type EvalBuiltin m = T.Text -> [V.CompoundValue] -> m V.CompoundValue

-- | Evaluate a FutharkScript expression relative to some running server.
evalExp :: forall m. (MonadError T.Text m, MonadIO m) => EvalBuiltin m -> ScriptServer -> Exp -> m ExpValue
evalExp builtin (ScriptServer server counter) top_level_e = do
  vars <- liftIO $ newIORef []
  let newVar base = liftIO $ do
        x <- readIORef counter
        modifyIORef counter (+ 1)
        let v = base <> prettyText x
        modifyIORef vars (v :)
        pure v

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

      interValToVal :: InterValue -> m V.CompoundValue
      interValToVal = traverse scriptValueToVal

      interValToVar :: InterValue -> m VarName
      interValToVar (V.ValueAtom v) = scriptValueToVar v
      interValToVar _ = throwError "Unexpected tuple or record value."

      valToInterVal :: V.CompoundValue -> InterValue
      valToInterVal = fmap $ \v ->
        SValue (prettyText (V.valueType v)) $ VVal v

      interValToExpVal :: InterValue -> m ExpValue
      interValToExpVal = traverse (traverse toVar)

      simpleType (V.ValueAtom (STValue _)) = True
      simpleType _ = False

      evalExp' :: Exp -> m InterValue
      evalExp' (ServerVar t v) =
        pure $ V.ValueAtom $ SValue t $ VVar v
      evalExp' (Call (FuncBuiltin name) es) = do
        v <- builtin name =<< mapM (interValToVal <=< evalExp') es
        pure $ valToInterVal v
      evalExp' (Call (FuncFut name) es) = do
        in_types <- cmdEither $ cmdInputs server name
        out_types <- cmdEither $ cmdOutputs server name

        es' <- mapM evalExp' es
        let es_types = map (fmap scriptValueType) es'

        unless (all simpleType es_types) $
          throwError $
            "Literate Futhark does not support passing script-constructed records, tuples, or functions to entry points.\n"
              <> "Create a Futhark wrapper function."

        -- Careful to not require saturated application.
        unless (and $ zipWith (==) es_types (map (V.ValueAtom . STValue) in_types)) $
          throwError $
            "Function \"" <> name <> "\" expects arguments of types:\n"
              <> prettyText (V.ValueTuple $ map V.ValueAtom in_types)
              <> "\nBut called with arguments of types:\n"
              <> prettyText (V.ValueTuple $ map V.ValueAtom es_types)

        ins <- mapM (interValToVar <=< evalExp') es

        if length in_types == length ins
          then do
            outs <- replicateM (length out_types) $ newVar "out"
            void $ cmdEither $ cmdCall server name outs ins
            pure $ V.mkCompound $ zipWith SValue out_types $ map VVar outs
          else
            pure . V.ValueAtom . SFun name in_types out_types $
              zipWith SValue in_types $ map VVar ins
      evalExp' (StringLit s) =
        case V.putValue s of
          Just s' ->
            pure $ V.ValueAtom $ SValue (prettyText (V.valueType s')) $ VVal s'
          Nothing -> error $ "Unable to write value " ++ pretty s
      evalExp' (Const val) =
        case V.putValue val of
          Just val' ->
            pure $ V.ValueAtom $ SValue (prettyText (primValueType val)) $ VVal val'
          Nothing -> error $ "Unable to write value " ++ pretty val
      evalExp' (Tuple es) =
        V.ValueTuple <$> mapM evalExp' es
      evalExp' e@(Record m) = do
        when (length (nubOrd (map fst m)) /= length (map fst m)) $
          throwError $ "Record " <> prettyText e <> " has duplicate fields."
        V.ValueRecord <$> traverse evalExp' (M.fromList m)

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
  (freeNonresultVars =<< interValToExpVal =<< evalExp' top_level_e) `catchError` freeVarsOnError

-- | Read actual values from the server.  Fails for values that have
-- no well-defined external representation.
getExpValue ::
  (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m V.CompoundValue
getExpValue (ScriptServer server _) e =
  traverse toGround =<< traverse (traverse (readVar server)) e
  where
    toGround (SFun fname _ _ _) =
      throwError $ "Function " <> fname <> " not fully applied."
    toGround (SValue _ v) = pure v

-- | Like 'evalExp', but requires all values to be non-functional.
evalExpToGround ::
  (MonadError T.Text m, MonadIO m) => EvalBuiltin m -> ScriptServer -> Exp -> m V.CompoundValue
evalExpToGround builtin server e = getExpValue server =<< evalExp builtin server e

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

-- | Release all the server-side variables in the value.  Yes,
-- FutharkScript has manual memory management...
freeValue :: (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m ()
freeValue (ScriptServer server _) =
  cmdMaybe . cmdFree server . S.toList . serverVarsInValue
