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
    evalExp,
    getExpValue,
    evalExpToGround,
    valueToExp,
    freeValue,
  )
where

import Control.Monad.Except
import Data.Char
import Data.Foldable (toList)
import Data.Functor
import Data.IORef
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.Void
import qualified Futhark.Data.Parser as V
import Futhark.Server
import Futhark.Server.Values (getValue, putValue)
import qualified Futhark.Test.Values as V
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty hiding (float, line, sep, space, string, (</>), (<|>))
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import Text.Megaparsec.Char.Lexer (charLiteral)

-- | Like a 'Server', but keeps a bit more state to make FutharkScript
-- more convenient.
data ScriptServer = ScriptServer Server (IORef Int)

-- | Run an action with a 'ScriptServer' produced by an existing
-- 'Server', without shutting it down at the end.
withScriptServer' :: MonadIO m => Server -> (ScriptServer -> m a) -> m a
withScriptServer' server f = do
  counter <- liftIO $ newIORef 0
  f $ ScriptServer server counter

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
  ppr (FuncFut f) = ppr f
  ppr (FuncBuiltin f) = "$" <> ppr f

instance Pretty Exp where
  ppr = pprPrec 0
  pprPrec _ (ServerVar _ v) = "$" <> ppr v
  pprPrec _ (Const v) = stack $ map strictText $ T.lines $ V.valueText v
  pprPrec i (Let pat e1 e2) =
    parensIf (i > 0) $ "let" <+> pat' <+> equals <+> ppr e1 <+> "in" <+> ppr e2
    where
      pat' = case pat of
        [x] -> ppr x
        _ -> parens $ commasep $ map ppr pat
  pprPrec _ (Call v []) = ppr v
  pprPrec i (Call v args) =
    parensIf (i > 0) $ ppr v <+> spread (map (align . pprPrec 1) args)
  pprPrec _ (Tuple vs) =
    parens $ commasep $ map (align . ppr) vs
  pprPrec _ (StringLit s) = ppr $ show s
  pprPrec _ (Record m) = braces $ commasep $ map field m
    where
      field (k, v) = align (ppr k <> equals <> ppr v)

type Parser = Parsec Void T.Text

lexeme :: Parser () -> Parser a -> Parser a
lexeme sep p = p <* sep

inParens :: Parser () -> Parser a -> Parser a
inParens sep = between (lexeme sep "(") (lexeme sep ")")

inBraces :: Parser () -> Parser a -> Parser a
inBraces sep = between (lexeme sep "{") (lexeme sep "}")

-- | Parse a FutharkScript expression, given a whitespace parser.
parseExp :: Parser () -> Parser Exp
parseExp sep =
  choice
    [ lexeme sep "let" $> Let
        <*> pPat <* lexeme sep "="
        <*> parseExp sep <* lexeme sep "in"
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
          Const <$> V.parseValue sep,
          StringLit . T.pack <$> lexeme sep ("\"" *> manyTill charLiteral "\""),
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
        constituent c = isAlphaNum c || c == '_'

-- | Parse a FutharkScript expression with normal whitespace handling.
parseExpFromText :: FilePath -> T.Text -> Either T.Text Exp
parseExpFromText f s =
  either (Left . T.pack . errorBundlePretty) Right $ parse (parseExp space) f s

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
  ppr (STValue t) = ppr t
  ppr (STFun ins outs) =
    spread $ intersperse "->" (map ppr ins ++ [outs'])
    where
      outs' = case outs of
        [out] -> strictText out
        _ -> parens $ commasep $ map strictText outs

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

-- | How to evaluate a builtin function.
type EvalBuiltin m = T.Text -> [V.CompoundValue] -> m V.CompoundValue

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

      interValToVal :: ExpValue -> m V.CompoundValue
      interValToVal = traverse scriptValueToVal

      interValToVar :: ExpValue -> m VarName
      interValToVar (V.ValueAtom v) = scriptValueToVar v
      interValToVar _ = throwError "Unexpected tuple or record value."

      valToInterVal :: V.CompoundValue -> ExpValue
      valToInterVal = fmap $ \v ->
        SValue (V.valueTypeTextNoDims (V.valueType v)) $ VVal v

      simpleType (V.ValueAtom (STValue _)) = True
      simpleType _ = False

      letMatch :: [VarName] -> ExpValue -> m VTable
      letMatch vs val
        | vals <- V.unCompound val,
          length vs == length vals =
          pure $ M.fromList (zip vs vals)
        | otherwise =
          throwError $
            "Pat: " <> prettyTextOneLine vs
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
            throwError $ "Locally bound name cannot be invoked as a function: " <> prettyText name
          pure e
      evalExp' vtable (Call (FuncFut name) es) = do
        in_types <- fmap (map inputType) $ cmdEither $ cmdInputs server name
        out_types <- fmap (map outputType) $ cmdEither $ cmdOutputs server name

        es' <- mapM (evalExp' vtable) es
        let es_types = map (fmap scriptValueType) es'

        unless (all simpleType es_types) $
          throwError $
            "Literate Futhark does not support passing script-constructed records, tuples, or functions to entry points.\n"
              <> "Create a Futhark wrapper function."

        -- Careful to not require saturated application, but do still
        -- check for over-saturation.
        let too_many = length es_types > length in_types
            too_wrong = zipWith (/=) es_types (map (V.ValueAtom . STValue) in_types)
        when (or $ too_many : too_wrong) . throwError $
          "Function \"" <> name <> "\" expects arguments of types:\n"
            <> prettyText (V.mkCompound $ map V.ValueAtom in_types)
            <> "\nBut called with arguments of types:\n"
            <> prettyText (V.mkCompound $ map V.ValueAtom es_types)

        ins <- mapM (interValToVar <=< evalExp' vtable) es

        if length in_types == length ins
          then do
            outs <- replicateM (length out_types) $ newVar "out"
            void $ cmdEither $ cmdCall server name outs ins
            pure $ V.mkCompound $ map V.ValueAtom $ zipWith SValue out_types $ map VVar outs
          else
            pure . V.ValueAtom . SFun name in_types out_types $
              zipWith SValue in_types $ map VVar ins
      evalExp' _ (StringLit s) =
        case V.putValue s of
          Just s' ->
            pure $ V.ValueAtom $ SValue (V.valueTypeText (V.valueType s')) $ VVal s'
          Nothing -> error $ "Unable to write value " ++ pretty s
      evalExp' _ (Const val) =
        pure $ V.ValueAtom $ SValue (V.valueTypeTextNoDims (V.valueType val)) $ VVal val
      evalExp' vtable (Tuple es) =
        V.ValueTuple <$> mapM (evalExp' vtable) es
      evalExp' vtable e@(Record m) = do
        when (length (nubOrd (map fst m)) /= length (map fst m)) $
          throwError $ "Record " <> prettyText e <> " has duplicate fields."
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
getExpValue (ScriptServer server _) e =
  traverse toGround =<< traverse (traverse onLeaf) e
  where
    onLeaf (VVar v) = readVar server v
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
freeValue (ScriptServer server _) =
  cmdMaybe . cmdFree server . S.toList . serverVarsInValue
