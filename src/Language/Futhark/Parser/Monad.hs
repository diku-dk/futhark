{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions and definitions used in the Happy-generated
-- parser.  They are defined here because the @.y@ file is opaque to
-- linters and other tools.  In particular, we cannot enable warnings
-- for that file, because Happy-generated code is very dirty by GHC's
-- standards.
module Language.Futhark.Parser.Monad
  ( ParserMonad,
    ParserState,
    ReadLineMonad (..),
    parseInMonad,
    parse,
    getLinesFromM,
    lexer,
    mustBeEmpty,
    arrayFromList,
    combArrayElements,
    binOp,
    binOpName,
    mustBe,
    floatNegate,
    intNegate,
    primNegate,
    primTypeFromName,
    applyExp,
    patternExp,
    addDocSpec,
    addAttrSpec,
    addDoc,
    addAttr,
    twoDotsRange,
    SyntaxError (..),
    emptyArrayError,
    parseError,
    parseErrorAt,
    backOneCol,

    -- * Reexports
    L,
    Token,
  )
where

import Control.Applicative (liftA)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.State
import Data.Array hiding (index)
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import Futhark.Util.Loc
import Futhark.Util.Pretty hiding (line)
import Language.Futhark.Parser.Lexer
import Language.Futhark.Parser.Lexer.Wrapper (LexerError (..))
import Language.Futhark.Pretty ()
import Language.Futhark.Prop
import Language.Futhark.Syntax
import Prelude hiding (mod)

addDoc :: DocComment -> UncheckedDec -> UncheckedDec
addDoc doc (ValDec val) = ValDec (val {valBindDoc = Just doc})
addDoc doc (TypeDec tp) = TypeDec (tp {typeDoc = Just doc})
addDoc doc (SigDec sig) = SigDec (sig {sigDoc = Just doc})
addDoc doc (ModDec mod) = ModDec (mod {modDoc = Just doc})
addDoc _ dec = dec

addDocSpec :: DocComment -> SpecBase NoInfo Name -> SpecBase NoInfo Name
addDocSpec doc (TypeAbbrSpec tpsig) = TypeAbbrSpec (tpsig {typeDoc = Just doc})
addDocSpec doc (ValSpec name ps t NoInfo _ loc) = ValSpec name ps t NoInfo (Just doc) loc
addDocSpec doc (TypeSpec l name ps _ loc) = TypeSpec l name ps (Just doc) loc
addDocSpec doc (ModSpec name se _ loc) = ModSpec name se (Just doc) loc
addDocSpec _ spec = spec

addAttr :: AttrInfo Name -> UncheckedDec -> UncheckedDec
addAttr attr (ValDec val) =
  ValDec $ val {valBindAttrs = attr : valBindAttrs val}
addAttr _ dec = dec

-- We will extend this function once we actually start tracking these.
addAttrSpec :: AttrInfo Name -> UncheckedSpec -> UncheckedSpec
addAttrSpec _attr dec = dec

mustBe :: L Token -> String -> ParserMonad ()
mustBe (L _ (ID got)) expected
  | nameToString got == expected = pure ()
mustBe (L loc _) expected =
  parseErrorAt loc . Just $
    "Only the keyword '" <> expected <> "' may appear here."

mustBeEmpty :: Located loc => loc -> ValueType -> ParserMonad ()
mustBeEmpty _ (Array _ _ (ShapeDecl dims) _)
  | 0 `elem` dims = pure ()
mustBeEmpty loc t =
  parseErrorAt loc $ Just $ pretty t ++ " is not an empty array."

data ParserState = ParserState
  { _parserFile :: FilePath,
    parserInput :: T.Text,
    parserLexical :: ([L Token], Pos)
  }

type ParserMonad = ExceptT SyntaxError (StateT ParserState ReadLineMonad)

data ReadLineMonad a
  = Value a
  | GetLine (Maybe T.Text -> ReadLineMonad a)

readLineFromMonad :: ReadLineMonad (Maybe T.Text)
readLineFromMonad = GetLine Value

instance Monad ReadLineMonad where
  Value x >>= f = f x
  GetLine g >>= f = GetLine $ g >=> f

instance Functor ReadLineMonad where
  fmap = liftA

instance Applicative ReadLineMonad where
  pure = Value
  (<*>) = ap

getLinesFromM :: Monad m => m T.Text -> ReadLineMonad a -> m a
getLinesFromM _ (Value x) = pure x
getLinesFromM fetch (GetLine f) = do
  s <- fetch
  getLinesFromM fetch $ f $ Just s

getNoLines :: ReadLineMonad a -> Either SyntaxError a
getNoLines (Value x) = Right x
getNoLines (GetLine f) = getNoLines $ f Nothing

combArrayElements :: Value -> [Value] -> Either SyntaxError Value
combArrayElements = foldM comb
  where
    comb x y
      | valueType x == valueType y = Right x
      | otherwise =
          Left . SyntaxError NoLoc $
            "Elements " <> pretty x <> " and "
              <> pretty y
              <> " cannot exist in same array."

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l - 1) l

applyExp :: [UncheckedExp] -> ParserMonad UncheckedExp
applyExp all_es@((Constr n [] _ loc1) : es) =
  pure $ Constr n es NoInfo (srcspan loc1 (last all_es))
applyExp es =
  foldM op (head es) (tail es)
  where
    op (AppExp (Index e is floc) _) (ArrayLit xs _ xloc) =
      parseErrorAt (srcspan floc xloc) . Just . pretty $
        "Incorrect syntax for multi-dimensional indexing."
          </> "Use" <+> align (ppr index)
      where
        index = AppExp (Index e (is ++ map DimFix xs) xloc) NoInfo
    op f x =
      pure $ AppExp (Apply f x NoInfo (srcspan f x)) NoInfo

patternExp :: UncheckedPat -> ParserMonad UncheckedExp
patternExp (Id v _ loc) = pure $ Var (qualName v) NoInfo loc
patternExp (TuplePat pats loc) = TupLit <$> mapM patternExp pats <*> pure loc
patternExp (Wildcard _ loc) = parseErrorAt loc $ Just "cannot have wildcard here."
patternExp (PatLit _ _ loc) = parseErrorAt loc $ Just "cannot have literal here."
patternExp (PatConstr _ _ _ loc) = parseErrorAt loc $ Just "cannot have constructor here."
patternExp (PatAttr _ p _) = patternExp p
patternExp (PatAscription pat _ _) = patternExp pat
patternExp (PatParens pat _) = patternExp pat
patternExp (RecordPat fs loc) = RecordLit <$> mapM field fs <*> pure loc
  where
    field (name, pat) = RecordFieldExplicit name <$> patternExp pat <*> pure loc

eof :: Pos -> L Token
eof pos = L (Loc pos pos) EOF

binOpName :: L Token -> (QualName Name, Loc)
binOpName (L loc (SYMBOL _ qs op)) = (QualName qs op, loc)
binOpName t = error $ "binOpName: unexpected " ++ show t

binOp :: UncheckedExp -> L Token -> UncheckedExp -> UncheckedExp
binOp x (L loc (SYMBOL _ qs op)) y =
  AppExp (BinOp (QualName qs op, srclocOf loc) NoInfo (x, NoInfo) (y, NoInfo) (srcspan x y)) NoInfo
binOp _ t _ = error $ "binOp: unexpected " ++ show t

getTokens :: ParserMonad ([L Token], Pos)
getTokens = lift $ gets parserLexical

putTokens :: ([L Token], Pos) -> ParserMonad ()
putTokens l = lift $ modify $ \env -> env {parserLexical = l}

primTypeFromName :: Loc -> Name -> ParserMonad PrimType
primTypeFromName loc s = maybe boom pure $ M.lookup s namesToPrimTypes
  where
    boom = parseErrorAt loc $ Just $ "No type named " ++ nameToString s

intNegate :: IntValue -> IntValue
intNegate (Int8Value v) = Int8Value (-v)
intNegate (Int16Value v) = Int16Value (-v)
intNegate (Int32Value v) = Int32Value (-v)
intNegate (Int64Value v) = Int64Value (-v)

floatNegate :: FloatValue -> FloatValue
floatNegate (Float16Value v) = Float16Value (-v)
floatNegate (Float32Value v) = Float32Value (-v)
floatNegate (Float64Value v) = Float64Value (-v)

primNegate :: PrimValue -> PrimValue
primNegate (FloatValue v) = FloatValue $ floatNegate v
primNegate (SignedValue v) = SignedValue $ intNegate v
primNegate (UnsignedValue v) = UnsignedValue $ intNegate v
primNegate (BoolValue v) = BoolValue $ not v

readLine :: ParserMonad (Maybe T.Text)
readLine = do
  s <- lift $ lift readLineFromMonad
  case s of
    Just s' ->
      lift $ modify $ \env -> env {parserInput = parserInput env <> "\n" <> s'}
    Nothing -> pure ()
  pure s

lexer :: (L Token -> ParserMonad a) -> ParserMonad a
lexer cont = do
  (ts, pos) <- getTokens
  case ts of
    [] -> do
      ended <- lift $ runExceptT $ cont $ eof pos
      case ended of
        Right x -> pure x
        Left parse_e -> do
          line <- readLine
          ts' <-
            case line of
              Nothing -> throwError parse_e
              Just line' -> pure $ scanTokensText (advancePos pos '\n') line'
          (ts'', pos') <- either (throwError . lexerErrToParseErr) pure ts'
          case ts'' of
            [] -> cont $ eof pos
            xs -> do
              putTokens (xs, pos')
              lexer cont
    (x : xs) -> do
      putTokens (xs, pos)
      cont x

parseError :: (L Token, [String]) -> ParserMonad a
parseError (L loc EOF, expected) =
  parseErrorAt (locOf loc) . Just . unlines $
    [ "Unexpected end of file.",
      "Expected one of the following: " ++ unwords expected
    ]
parseError (L loc DOC {}, _) =
  parseErrorAt (locOf loc) $
    Just "Documentation comments ('-- |') are only permitted when preceding declarations."
parseError (L loc _, expected) = do
  input <- lift $ gets parserInput
  let ~(Loc (Pos _ _ _ beg) (Pos _ _ _ end)) = locOf loc
      tok_src = T.take (end - beg + 1) $ T.drop beg input
  parseErrorAt loc . Just . unlines $
    [ "Unexpected token: '" <> T.unpack tok_src <> "'",
      "Expected one of the following: " <> unwords expected
    ]

parseErrorAt :: Located loc => loc -> Maybe String -> ParserMonad a
parseErrorAt loc Nothing = throwError $ SyntaxError (locOf loc) "Syntax error."
parseErrorAt loc (Just s) = throwError $ SyntaxError (locOf loc) s

emptyArrayError :: Loc -> ParserMonad a
emptyArrayError loc =
  parseErrorAt loc $
    Just "write empty arrays as 'empty(t)', for element type 't'.\n"

twoDotsRange :: Loc -> ParserMonad a
twoDotsRange loc = parseErrorAt loc $ Just "use '...' for ranges, not '..'.\n"

-- | Move the end position back one column.
backOneCol :: Loc -> Loc
backOneCol (Loc start (Pos f l c o)) = Loc start $ Pos f l (c - 1) (o - 1)
backOneCol NoLoc = NoLoc

--- Now for the parser interface.

-- | A syntax error.
data SyntaxError = SyntaxError {syntaxErrorLoc :: Loc, syntaxErrorMsg :: String}

lexerErrToParseErr :: LexerError -> SyntaxError
lexerErrToParseErr (LexerError loc msg) = SyntaxError loc msg

parseInMonad :: ParserMonad a -> FilePath -> T.Text -> ReadLineMonad (Either SyntaxError a)
parseInMonad p file program =
  either
    (pure . Left . lexerErrToParseErr)
    (evalStateT (runExceptT p) . env)
    (scanTokensText (Pos file 1 1 0) program)
  where
    env = ParserState file program

parse :: ParserMonad a -> FilePath -> T.Text -> Either SyntaxError a
parse p file program = join $ getNoLines $ parseInMonad p file program
