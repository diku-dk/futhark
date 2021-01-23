{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FutharkScript is a (tiny) subset of Futhark used to write small
-- expressions that are evaluated by server executables.  The @futhark
-- script@ command is the main user.
module Futhark.Script
  ( -- * Server
    ScriptServer,
    withScriptServer,

    -- * Expressions, values, and types
    Exp (..),
    parseExp,
    varsInExp,
    ScriptValueType (..),
    ScriptValue (..),
    scriptValueType,
    ExpValue,

    -- * Evaluation
    evalExp,
    getExpValue,
    evalExpToGround,
    valueToExp,
    freeValue,
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.Foldable (toList)
import Data.Functor
import Data.IORef
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import Data.Void
import Futhark.Server
import qualified Futhark.Test.Values as V
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (float, line, sep, string, (</>), (<|>))
import Language.Futhark.Prop (primValueType)
import Language.Futhark.Syntax
  ( FloatValue (..),
    IntValue (..),
    PrimValue (..),
  )
import System.IO
import System.IO.Temp
import Text.Megaparsec

-- | Like a 'Server', but keeps a bit more state to make FutharkScript
-- more convenient.
data ScriptServer = ScriptServer Server (IORef Int)

-- | Start a server, execute an action, then shut down the server.
-- Similar to 'withServer'.
withScriptServer :: FilePath -> [FilePath] -> (ScriptServer -> IO a) -> IO a
withScriptServer prog options f = withServer prog options $ \server -> do
  counter <- newIORef 0
  f $ ScriptServer server counter

data Exp
  = Call EntryName [Exp]
  | Const PrimValue
  | Tuple [Exp]
  | Record [(T.Text, Exp)]
  | -- | Server-side variable, *not* Futhark variable (these are
    -- handled in 'Call').
    ServerVar TypeName VarName
  deriving (Show)

instance Pretty Exp where
  ppr = pprPrec 0
  pprPrec _ (ServerVar _ v) = "$" <> strictText v
  pprPrec _ (Const v) = ppr v
  pprPrec i (Call v args) =
    parensIf (i > 0) $ strictText v <+> spread (map (pprPrec 1) args)
  pprPrec _ (Tuple vs) =
    parens $ commasep $ map ppr vs
  pprPrec _ (Record m) = braces $ commasep $ map field m
    where
      field (k, v) = ppr k <> equals <> ppr v

type Parser = Parsec Void T.Text

parseEntryName :: Parser EntryName
parseEntryName =
  fmap T.pack $ (:) <$> satisfy isAlpha <*> many (satisfy constituent)
  where
    constituent c = isAlphaNum c || c == '_'

parseSign :: Parser String
parseSign = maybe "" T.unpack <$> optional "-"

parseDouble :: Parser Double
parseDouble = do
  sign <- parseSign
  x <- some (satisfy isDigit)
  ( do
      void "."
      y <- some (satisfy isDigit)
      pure $ read $ sign <> x <> "." <> y
    )
    <|> pure (read $ sign <> x)

parseInteger :: Parser Integer
parseInteger = read <$> ((<>) <$> pSign <*> some (satisfy isDigit))
  where
    pSign = maybe "" T.unpack <$> optional "-"

parseIntConst :: Parser PrimValue
parseIntConst = do
  x <- parseInteger
  choice
    [ signed Int8Value x "i8",
      signed Int16Value x "i16",
      signed Int32Value x "i32",
      signed Int64Value x "i64",
      unsigned Int8Value x "u8",
      unsigned Int16Value x "u16",
      unsigned Int32Value x "u32",
      unsigned Int64Value x "u64"
    ]
  where
    signed mk x suffix =
      suffix $> SignedValue (mk (fromInteger x))
    unsigned mk x suffix =
      suffix $> UnsignedValue (mk (fromInteger x))

parseFloatConst :: Parser PrimValue
parseFloatConst = do
  x <- parseDouble
  choice
    [ float Float32Value x "f32",
      float Float64Value x "f64"
    ]
  where
    float mk x suffix =
      suffix $> FloatValue (mk (realToFrac x))

parsePrimValue :: Parser PrimValue
parsePrimValue =
  choice
    [ try parseIntConst,
      parseFloatConst,
      "true" $> BoolValue True,
      "false" $> BoolValue False
    ]

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

inParens :: Parser () -> Parser a -> Parser a
inParens sep = between (lexeme sep "(") (lexeme sep ")")

inBraces :: Parser () -> Parser a -> Parser a
inBraces sep = between (lexeme sep "{") (lexeme sep "}")

parseExp :: Parser () -> Parser Exp
parseExp sep =
  choice
    [ inParens sep (mkTuple <$> (parseExp sep `sepBy` pComma)),
      inBraces sep (Record <$> (pField `sepBy` pComma)),
      Call <$> lexeme sep parseEntryName <*> many (parseExp sep),
      Const <$> lexeme sep parsePrimValue
    ]
  where
    pField = (,) <$> parseEntryName <*> (pEquals *> parseExp sep)
    pEquals = lexeme sep "="
    pComma = lexeme sep ","
    mkTuple [v] = v
    mkTuple vs = Tuple vs

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
            _ -> pure $ Left "Invalid data file produced by Futhark server."

writeVar :: (MonadError T.Text m, MonadIO m) => Server -> VarName -> PrimValue -> m ()
writeVar server v val =
  cmdMaybe . liftIO . withSystemTempFile "futhark-server-write" $ \tmpf tmpf_h -> do
    T.hPutStr tmpf_h $ prettyText val
    hClose tmpf_h
    let t = prettyText $ primValueType val
    cmdRestore server tmpf [(v, t)]

-- | A ScriptValue is either a base value or a partially applied
-- function.  We don't have real first-class functions in
-- FutharkScript, but we sort of have closures.
data ScriptValue v
  = SValue TypeName v
  | -- | Ins, then outs.
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
  deriving (Show)

instance Pretty ScriptValueType where
  ppr (STValue t) = ppr t
  ppr (STFun ins outs) =
    spread $ intersperse "->" (map ppr ins ++ [outs'])
    where
      outs' = case outs of
        [out] -> strictText out
        _ -> parens $ commasep $ map strictText outs

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
  Call fname $ map (valueToExp . V.ValueAtom) closure
valueToExp (V.ValueRecord fs) =
  Record $ M.toList $ M.map valueToExp fs
valueToExp (V.ValueTuple fs) =
  Tuple $ map valueToExp fs

evalExp :: (MonadError T.Text m, MonadIO m) => ScriptServer -> Exp -> m ExpValue
evalExp (ScriptServer server counter) top_level_e = do
  vars <- liftIO $ newIORef []
  let newVar base = liftIO $ do
        x <- readIORef counter
        modifyIORef counter (+ 1)
        let v = base <> prettyText x
        modifyIORef vars (v :)
        pure v

      evalExpToVar e = do
        vs <- evalExpToVars e
        case vs of
          V.ValueAtom (SValue _ v) -> pure v
          V.ValueAtom SFun {} ->
            throwError $ "Expression " <> prettyText e <> " not fully applied."
          _ ->
            throwError $ "Expression " <> prettyText e <> " produced more than one value."
      evalExpToVars (ServerVar t v) =
        pure $ V.ValueAtom $ SValue t v
      evalExpToVars (Call name es) = do
        ins <- mapM evalExpToVar es
        in_types <- cmdEither $ cmdInputs server name
        out_types <- cmdEither $ cmdOutputs server name
        if length in_types == length ins
          then do
            outs <- replicateM (length out_types) $ newVar "out"
            void $ cmdEither $ cmdCall server name outs ins
            pure $ V.mkCompound $ zipWith SValue out_types outs
          else
            pure . V.ValueAtom . SFun name in_types out_types $
              zipWith SValue in_types ins
      evalExpToVars (Const val) = do
        v <- newVar "const"
        writeVar server v val
        pure $ V.ValueAtom $ SValue (prettyText (primValueType val)) v
      evalExpToVars (Tuple es) =
        V.ValueTuple <$> mapM evalExpToVars es
      evalExpToVars e@(Record m) = do
        when (length (nubOrd (map fst m)) /= length (map fst m)) $
          throwError $ "Record " <> prettyText e <> " has duplicate fields."
        V.ValueRecord <$> traverse evalExpToVars (M.fromList m)

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
  (freeNonresultVars =<< evalExpToVars top_level_e) `catchError` freeVarsOnError

-- | Read actual values from the server.  Fails for values that have
-- no well-defined external representation.
getExpValue ::
  (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m (V.Compound V.Value)
getExpValue (ScriptServer server _) e =
  traverse toGround =<< traverse (traverse (readVar server)) e
  where
    toGround (SFun fname _ _ _) =
      throwError $ "Function " <> fname <> " not fully applied."
    toGround (SValue _ v) = pure v

-- | Like 'evalExp', but requires all values to be non-functional.
evalExpToGround ::
  (MonadError T.Text m, MonadIO m) => ScriptServer -> Exp -> m (V.Compound V.Value)
evalExpToGround server e = getExpValue server =<< evalExp server e

-- | The set of Futhark variables that are referenced by the
-- expression - these will have to be entry points in the Futhark
-- program.
varsInExp :: Exp -> S.Set EntryName
varsInExp ServerVar {} = mempty
varsInExp (Call v es) = S.insert v $ foldMap varsInExp es
varsInExp (Tuple es) = foldMap varsInExp es
varsInExp (Record fs) = foldMap (foldMap varsInExp) fs
varsInExp Const {} = mempty

-- | Release all the server-side variables in the value.  Yes,
-- FutharkScript has manual memory management...
freeValue :: (MonadError T.Text m, MonadIO m) => ScriptServer -> ExpValue -> m ()
freeValue (ScriptServer server _) =
  cmdMaybe . cmdFree server . S.toList . serverVarsInValue
