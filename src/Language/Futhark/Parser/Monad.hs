-- | Utility functions and definitions used in the Happy-generated
-- parser.  They are defined here because the @.y@ file is opaque to
-- linters and other tools.  In particular, we cannot enable warnings
-- for that file, because Happy-generated code is very dirty by GHC's
-- standards.
module Language.Futhark.Parser.Monad
  ( ParserMonad,
    ParserState,
    Comment (..),
    parse,
    parseWithComments,
    lexer,
    mustBeEmpty,
    arrayFromList,
    binOp,
    binOpName,
    mustBe,
    primNegate,
    applyExp,
    arrayLitExp,
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

import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Array hiding (index)
import Data.ByteString.Lazy qualified as BS
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Futhark.Util.Loc
import Futhark.Util.Pretty hiding (line, line')
import Language.Futhark.Parser.Lexer
import Language.Futhark.Parser.Lexer.Wrapper (AlexInput, LexerError (..), initialLexerState)
import Language.Futhark.Pretty ()
import Language.Futhark.Prop
import Language.Futhark.Syntax
import Prelude hiding (mod)

addDoc :: DocComment -> UncheckedDec -> UncheckedDec
addDoc doc (ValDec val) = ValDec (val {valBindDoc = Just doc})
addDoc doc (TypeDec tp) = TypeDec (tp {typeDoc = Just doc})
addDoc doc (ModTypeDec sig) = ModTypeDec (sig {modTypeDoc = Just doc})
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

mustBe :: L Token -> T.Text -> ParserMonad ()
mustBe (L _ (ID got)) expected
  | nameToText got == expected = pure ()
mustBe (L loc _) expected =
  parseErrorAt loc . Just $
    "Only the keyword '" <> expected <> "' may appear here."

mustBeEmpty :: (Located loc) => loc -> ValueType -> ParserMonad ()
mustBeEmpty _ (Array _ (Shape dims) _)
  | 0 `elem` dims = pure ()
mustBeEmpty loc t =
  parseErrorAt loc $ Just $ prettyText t <> " is not an empty array."

-- | A comment consists of its starting and end position, as well as
-- its text.  The contents include the comment start marker.
data Comment = Comment {commentLoc :: Loc, commentText :: T.Text}
  deriving (Eq, Ord, Show)

instance Located Comment where
  locOf = commentLoc

data ParserState = ParserState
  { _parserFile :: FilePath,
    parserInput :: T.Text,
    -- | Note: reverse order.
    parserComments :: [Comment],
    parserLexerState :: AlexInput
  }

type ParserMonad = ExceptT SyntaxError (State ParserState)

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l - 1) l

arrayLitExp :: [UncheckedExp] -> SrcLoc -> UncheckedExp
arrayLitExp es loc
  | Just (v : vs) <- mapM isLiteral es,
    all ((primValueType v ==) . primValueType) vs =
      ArrayVal (v : vs) (primValueType v) loc
  | otherwise =
      ArrayLit es NoInfo loc
  where
    isLiteral (Literal v _) = Just v
    isLiteral _ = Nothing

applyExp :: NE.NonEmpty UncheckedExp -> ParserMonad UncheckedExp
applyExp all_es@((Constr n [] _ loc1) NE.:| es) =
  pure $ Constr n es NoInfo (srcspan loc1 (NE.last all_es))
applyExp es =
  foldM op (NE.head es) (NE.tail es)
  where
    op (AppExp (Index e is floc) _) (ArrayLit xs _ xloc) =
      parseErrorAt (srcspan floc xloc) . Just . docText $
        "Incorrect syntax for multi-dimensional indexing."
          </> "Use"
          <+> align (pretty index)
      where
        index = AppExp (Index e (is ++ map DimFix xs) xloc) NoInfo
    op f x = pure $ mkApplyUT f x

patternExp :: UncheckedPat t -> ParserMonad UncheckedExp
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

binOpName :: L Token -> (QualName Name, Loc)
binOpName (L loc (SYMBOL _ qs op)) = (QualName qs op, loc)
binOpName t = error $ "binOpName: unexpected " ++ show t

binOp :: UncheckedExp -> L Token -> UncheckedExp -> UncheckedExp
binOp x (L loc (SYMBOL _ qs op)) y =
  AppExp (BinOp (QualName qs op, srclocOf loc) NoInfo (x, NoInfo) (y, NoInfo) (srcspan x y)) NoInfo
binOp _ t _ = error $ "binOp: unexpected " ++ show t

putComment :: Comment -> ParserMonad ()
putComment c = lift $ modify $ \env ->
  env {parserComments = c : parserComments env}

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

lexer :: (L Token -> ParserMonad a) -> ParserMonad a
lexer cont = do
  ls <- lift $ gets parserLexerState
  case getToken ls of
    Left e ->
      throwError $ lexerErrToParseErr e
    Right (ls', (start, end, tok)) -> do
      let loc = Loc start end
      lift $ modify $ \s -> s {parserLexerState = ls'}
      case tok of
        COMMENT text -> do
          putComment $ Comment loc text
          lexer cont
        _ ->
          cont $ L loc tok

parseError :: (L Token, [String]) -> ParserMonad a
parseError (L loc EOF, expected) =
  parseErrorAt (locOf loc) . Just . T.unlines $
    [ "Unexpected end of file.",
      "Expected one of the following: " <> T.unwords (map T.pack expected)
    ]
parseError (L loc DOC {}, _) =
  parseErrorAt (locOf loc) $
    Just "Documentation comments ('-- |') are only permitted when preceding declarations."
parseError (L loc (ERROR "\""), _) =
  parseErrorAt (locOf loc) $
    Just "Unclosed string literal."
parseError (L loc _, expected) = do
  input <- lift $ gets parserInput
  let ~(Loc (Pos _ _ _ beg) (Pos _ _ _ end)) = locOf loc
      tok_src = T.take (end - beg) $ T.drop beg input
  parseErrorAt loc . Just . T.unlines $
    [ "Unexpected token: '" <> tok_src <> "'",
      "Expected one of the following: " <> T.unwords (map T.pack expected)
    ]

parseErrorAt :: (Located loc) => loc -> Maybe T.Text -> ParserMonad a
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
data SyntaxError = SyntaxError {syntaxErrorLoc :: Loc, syntaxErrorMsg :: T.Text}

lexerErrToParseErr :: LexerError -> SyntaxError
lexerErrToParseErr (LexerError loc msg) = SyntaxError loc msg

parseWithComments ::
  ParserMonad a ->
  FilePath ->
  T.Text ->
  Either SyntaxError (a, [Comment])
parseWithComments p file program =
  onRes $ runState (runExceptT p) env
  where
    env =
      ParserState
        file
        program
        []
        (initialLexerState start $ BS.fromStrict . T.encodeUtf8 $ program)
    start = Pos file 1 1 0
    onRes (Left err, _) = Left err
    onRes (Right x, s) = Right (x, reverse $ parserComments s)

parse :: ParserMonad a -> FilePath -> T.Text -> Either SyntaxError a
parse p file program = fst <$> parseWithComments p file program
