{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions and definitions used in the Happy-generated
-- parser.  They are defined here because the @.y@ file is opaque to
-- linters and other tools.  In particular, we cannot enable warnings
-- for that file, because Happy-generated code is very dirty by GHC's
-- standards.
module Language.Futhark.Parser.Monad
  ( ParserMonad,
    ParserEnv,
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
    ParseError (..),
    emptyArrayError,
    parseError,
    parseErrorAt,

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
import Futhark.Util.Loc hiding (L) -- Lexer has replacements.
import Futhark.Util.Pretty hiding (line)
import Language.Futhark.Parser.Lexer
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
addDocSpec doc (ValSpec name ps t _ loc) = ValSpec name ps t (Just doc) loc
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

mustBeEmpty :: SrcLoc -> ValueType -> ParserMonad ()
mustBeEmpty _ (Array _ _ _ (ShapeDecl dims))
  | 0 `elem` dims = pure ()
mustBeEmpty loc t =
  parseErrorAt loc $ Just $ pretty t ++ " is not an empty array."

newtype ParserEnv = ParserEnv
  { parserFile :: FilePath
  }

type ParserMonad =
  ExceptT String (StateT ParserEnv (StateT ([L Token], Pos) ReadLineMonad))

data ReadLineMonad a
  = Value a
  | GetLine (Maybe T.Text -> ReadLineMonad a)

readLineFromMonad :: ReadLineMonad (Maybe T.Text)
readLineFromMonad = GetLine Value

instance Monad ReadLineMonad where
  return = pure
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

getNoLines :: ReadLineMonad a -> Either String a
getNoLines (Value x) = Right x
getNoLines (GetLine f) = getNoLines $ f Nothing

combArrayElements :: Value -> [Value] -> Either String Value
combArrayElements = foldM comb
  where
    comb x y
      | valueType x == valueType y = Right x
      | otherwise =
        Left $
          "Elements " <> pretty x <> " and "
            <> pretty y
            <> " cannot exist in same array."

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l -1) l

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
eof pos = L (SrcLoc $ Loc pos pos) EOF

binOpName :: L Token -> (QualName Name, SrcLoc)
binOpName (L loc (SYMBOL _ qs op)) = (QualName qs op, loc)
binOpName t = error $ "binOpName: unexpected " ++ show t

binOp :: UncheckedExp -> L Token -> UncheckedExp -> UncheckedExp
binOp x (L loc (SYMBOL _ qs op)) y =
  AppExp (BinOp (QualName qs op, loc) NoInfo (x, NoInfo) (y, NoInfo) (srcspan x y)) NoInfo
binOp _ t _ = error $ "binOp: unexpected " ++ show t

getTokens :: ParserMonad ([L Token], Pos)
getTokens = lift $ lift get

putTokens :: ([L Token], Pos) -> ParserMonad ()
putTokens = lift . lift . put

primTypeFromName :: SrcLoc -> Name -> ParserMonad PrimType
primTypeFromName loc s = maybe boom pure $ M.lookup s namesToPrimTypes
  where
    boom = parseErrorAt loc $ Just $ "No type named " ++ nameToString s

intNegate :: IntValue -> IntValue
intNegate (Int8Value v) = Int8Value (- v)
intNegate (Int16Value v) = Int16Value (- v)
intNegate (Int32Value v) = Int32Value (- v)
intNegate (Int64Value v) = Int64Value (- v)

floatNegate :: FloatValue -> FloatValue
floatNegate (Float16Value v) = Float16Value (- v)
floatNegate (Float32Value v) = Float32Value (- v)
floatNegate (Float64Value v) = Float64Value (- v)

primNegate :: PrimValue -> PrimValue
primNegate (FloatValue v) = FloatValue $ floatNegate v
primNegate (SignedValue v) = SignedValue $ intNegate v
primNegate (UnsignedValue v) = UnsignedValue $ intNegate v
primNegate (BoolValue v) = BoolValue $ not v

readLine :: ParserMonad (Maybe T.Text)
readLine = lift $ lift $ lift readLineFromMonad

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
          (ts'', pos') <-
            case ts' of
              Right x -> pure x
              Left lex_e -> throwError lex_e
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
  parseErrorAt (srclocOf loc) . Just . unlines $
    [ "unexpected end of file.",
      "Expected one of the following: " ++ unwords expected
    ]
parseError (L loc DOC {}, _) =
  parseErrorAt (srclocOf loc) $
    Just "documentation comments ('-- |') are only permitted when preceding declarations."
parseError (L loc tok, expected) =
  parseErrorAt loc . Just . unlines $
    [ "unexpected " ++ show tok,
      "Expected one of the following: " ++ unwords expected
    ]

parseErrorAt :: SrcLoc -> Maybe String -> ParserMonad a
parseErrorAt loc Nothing = throwError $ "Error at " ++ locStr loc ++ ": Parse error."
parseErrorAt loc (Just s) = throwError $ "Error at " ++ locStr loc ++ ": " ++ s

emptyArrayError :: SrcLoc -> ParserMonad a
emptyArrayError loc =
  parseErrorAt loc $
    Just "write empty arrays as 'empty(t)', for element type 't'.\n"

twoDotsRange :: SrcLoc -> ParserMonad a
twoDotsRange loc = parseErrorAt loc $ Just "use '...' for ranges, not '..'.\n"

--- Now for the parser interface.

-- | A parse error.  Use 'show' to get a human-readable description.
newtype ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> FilePath -> T.Text -> ReadLineMonad (Either ParseError a)
parseInMonad p file program =
  either (Left . ParseError) Right
    <$> either
      (pure . Left)
      (evalStateT (evalStateT (runExceptT p) env))
      (scanTokensText (Pos file 1 1 0) program)
  where
    env = ParserEnv {parserFile = file}

parse :: ParserMonad a -> FilePath -> T.Text -> Either ParseError a
parse p file program =
  either (Left . ParseError) id $ getNoLines $ parseInMonad p file program
