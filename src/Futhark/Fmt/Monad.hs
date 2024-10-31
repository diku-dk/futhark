module Futhark.Fmt.Monad
  ( Fmt,
    -- functions for building fmt
    nil,
    nest,
    stdNest,
    text,
    space,
    hardline,
    line,
    sep,
    sepLine,
    brackets,
    braces,
    parens,
    (<|>),
    (<+>),
    (</>),
    (<:>),
    (<+/>),
    (<:/>),
    hardIndent,
    indent,
    hardStdIndent,
    stdIndent,
    pretty,
    FmtM,
    fmtComments,
    popComments,
    runFormat,
    Format (..),
    align,
    fmtCopyLoc,
    comment,
    sepArgs,
    localLayout,
    localLayoutList,
    sepDecs,
    fmtByLayout,
    addComments,
  )
where

import Control.Monad.Reader
  ( MonadReader (..),
    ReaderT (..),
  )
import Control.Monad.State
  ( MonadState (..),
    State,
    evalState,
    gets,
    modify,
  )
import Data.ByteString qualified as BS
import Data.Loc (Loc (..), Located (..), posCoff, posLine)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Language.Futhark.Parser.Monad (Comment (..))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)

-- These are right associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.

infixr 7 <+/>

infixr 6 <:/>

infixr 6 <:>

infixr 6 <+>

infixr 6 </>

infixr 4 <|>

type Fmt = FmtM (P.Doc ())

instance Located Fmt where
  locOf _ = NoLoc -- FIXME: This is nonsense.

-- | This function allows to inspect the layout of an expression @a@ and if it
-- is singleline line then use format @s@ and if it is multiline format @m@.
fmtByLayout ::
  (Located a) => a -> Fmt -> Fmt -> Fmt
fmtByLayout a s m =
  s
    <|> ( case lineLayout a of
            Just SingleLine -> s
            _any -> m
        )

-- | This function determines the Layout of @a@ and if it is singleline then it
-- updates the monads enviroment to format singleline style otherwise format using
-- multiline style. It determines this by checking if the location of @a@ spans
-- over two or more lines.
localLayout :: (Located a) => a -> FmtM b -> FmtM b
localLayout a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ fromMaybe lo $ lineLayout a) m
    SingleLine -> m

-- | This function determines the Layout of @[a]@ and if it is singleline then it
-- updates the monads enviroment to format singleline style otherwise format using
-- multiline style. It determines this by checking if the locations of @[a]@
-- start and end at any different line number.
localLayoutList :: (Located a) => [a] -> FmtM b -> FmtM b
localLayoutList a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ fromMaybe lo $ lineLayoutList a) m
    SingleLine -> m

-- | This function uses the location of @a@ and prepends comments if the
-- comments location is less than the location of @a@. It format @b@ in
-- accordance with if @a@ is singleline or multiline using 'localLayout'. At last
-- it internally sets the state of the 'FmtM' monad to consider trailing
-- comments if they exists. This function should be always used when possible to
-- wrap Fmt around. It currently does not handle trailing comment perfectly.
-- See tests/fmt/traillingComments1.fut or the other test.
addComments :: (Located a) => a -> Fmt -> Fmt
addComments a b = localLayout a $ do
  c <- fmtComments a
  f <- b
  setTrailingComment a
  pure $ c <> f

prependSepComments :: (Located a) => a -> Fmt -> Fmt
prependSepComments a b = do
  fmcs <- fcs
  b' <- b
  pure $ fromMaybe mempty fmcs <> b'
  where
    fcs = do
      s <- get
      case comments s of
        c : cs | locOf a /= NoLoc && locOf a > locOf c -> do
          put $ s {comments = cs}
          mcs <- fcs
          pre' <- pre
          pure $ Just $ pre' <> fmtComment c <> maybe mempty (P.line <>) mcs
        _any -> pure Nothing
    fmtComment = P.pretty . commentText
    pre = do
      lastO <- gets lastOutput
      case lastO of
        Nothing -> nil
        Just Line -> nil
        Just _ -> modify (\s -> s {lastOutput = Just Line}) >> hardline

-- | The internal state of the formatter monad 'FmtM'.
data FmtState = FmtState
  { -- | The comments that will be inserted, ordered by increasing order in regards to location.
    comments :: [Comment],
    -- | The original source file that is being formatted.
    file :: BS.ByteString,
    -- | Pending comment to be inserted before next newline (reverse order).
    pendingComments :: !(Maybe Comment),
    -- | Keeps track of what type the last output was.
    lastOutput :: !(Maybe LastOutput)
  }
  deriving (Show, Eq, Ord)

-- | A data type to describe the last output used during formatting.
data LastOutput = Line | Space | Text | Comm deriving (Show, Eq, Ord)

-- | A data type to describe the layout the formatter is using currently.
data Layout = MultiLine | SingleLine deriving (Show, Eq)

-- | The format monad used to keep track of comments and layout. It is a a
-- combincation of a reader and state monad. The comments and reading from the
-- input file are the state monads job to deal with. While the reader monad
-- deals with the propagating the current layout.
type FmtM a = ReaderT Layout (State FmtState) a

-- | A typeclass that defines how an type can be formatted.
class Format a where
  fmt :: a -> Fmt

instance Format Comment where
  fmt = comment . commentText

-- | Prepends comments.
fmtComments :: (Located a) => a -> Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a /= NoLoc && locOf a > locOf c -> do
      put $ s {comments = cs}
      pre <:> fmt c <:> fmtComments a
    _any -> pure mempty
  where
    pre = do
      lastO <- gets lastOutput
      case lastO of
        Nothing -> nil
        Just Line -> nil
        Just _ -> modify (\s -> s {lastOutput = Just Line}) >> hardline

-- | If the next comment is a trailing comment then it is added to be a pending
-- comment that is added at next line.
setTrailingComment :: (Located a) => a -> FmtM ()
setTrailingComment a = do
  s <- get
  case comments s of
    c : cs | locOf a /= NoLoc -> case (locOf a, locOf c) of
      -- comment on same line as term a
      (Loc _sALoc eALoc, Loc _sCLoc eCLoc) | posLine eALoc == posLine eCLoc -> do
        put $
          s
            { comments = cs,
              pendingComments = Just c
            }
      _any -> pure ()
    _ -> pure ()

-- | Determines the layout of @a@ by checking if it spans a single line or two
-- or more lines.
lineLayout :: (Located a) => a -> Maybe Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
        then Just SingleLine
        else Just MultiLine
    NoLoc -> Nothing -- error "Formatting term without location."

-- | Determines the layout of @[a]@ by checking if it spans a single line or two
-- or more lines.
lineLayoutList :: (Located a) => [a] -> Maybe Layout
lineLayoutList as =
  case concatMap auxiliary as of
    [] -> Nothing
    (t : ts) | any (/= t) ts -> Just MultiLine
    _ -> Just SingleLine
  where
    auxiliary a =
      case locOf a of
        Loc start end -> [posLine start, posLine end]
        NoLoc -> [] -- error "Formatting term without location"

-- | Retrieves the last comments from the monad and concatenates them together.
popComments :: Fmt
popComments = do
  cs <- gets comments
  modify (\s -> s {comments = []})
  sep nil $ map fmt cs

-- | Using the location of @a@ get the segment of text in the original file to
-- create a @Fmt@.
fmtCopyLoc :: (Located a) => a -> Fmt
fmtCopyLoc a = do
  f <- gets file
  case locOf a of
    Loc sPos ePos ->
      let sOff = posCoff sPos
          eOff = posCoff ePos
       in case T.decodeUtf8' $ BS.take (eOff - sOff) $ BS.drop sOff f of
            Left err -> error $ show err
            Right lit -> text lit
    NoLoc -> error "Formatting term without location"

-- | Given a formatter @FmtM a@, a sequence of comments ordered in increasing
-- order by location, and the original text files content. Run the formatter and
-- create @a@.
runFormat :: FmtM a -> [Comment] -> T.Text -> a
runFormat format cs file = evalState (runReaderT format e) s
  where
    s =
      FmtState
        { comments = cs,
          file = T.encodeUtf8 file,
          pendingComments = Nothing,
          lastOutput = Nothing
        }
    e = MultiLine

-- | An empty input.
nil :: Fmt
nil = pure mempty

-- | Indents everything after a line occurs if in multiline and if in singleline
-- then indent.
nest :: Int -> Fmt -> Fmt
nest i a = a <|> (P.nest i <$> a)

-- | A space.
space :: Fmt
space = modify (\s -> s {lastOutput = Just Space}) >> pure P.space

-- | Forces a line to be used regardless of layout, this should ideally not be
-- used.
hardline :: Fmt
hardline = do
  pc <- gets pendingComments
  case pc of
    Just c -> do
      modify $ \s ->
        s
          { pendingComments = Nothing,
            lastOutput = Just Line
          }
      space <:> fmt c
    Nothing -> do
      modify $ \s -> s {lastOutput = Just Line}
      pure P.line

-- | A line or a space depending on layout.
line :: Fmt
line = space <|> hardline

-- | A comment.
comment :: T.Text -> Fmt
comment c = do
  modify (\s -> s {lastOutput = Just Line})
  pure $ P.pretty c <> P.line

-- in order to handle trailing comments its VERY important to
-- evaluate the seperator after each element in the list.
sep :: Fmt -> [Fmt] -> Fmt
sep _ [] = nil
sep s (a : as) = auxiliary a as
  where
    auxiliary acc [] = acc
    auxiliary acc (x : xs) =
      auxiliary (acc <:> prependSepComments s (prependSepComments x (s <:> x))) xs

-- | Seperates element by a @s@ followed by a space in singleline layout and
-- seperates by a line followed by a @s@ in multine layout.
sepLine :: Fmt -> [Fmt] -> Fmt
sepLine s = sep (s <:> space <|> hardline <:> s)

-- | This is used for function arguments. It seperates multiline arguments by
-- lines and singleline arguments by spaces.
sepArgs :: [Fmt] -> Fmt
sepArgs [] = nil
sepArgs ls
  | any ((== Just MultiLine) . lineLayout) ls =
      sep nil $ zipWith3 auxiliary [0 :: Int ..] los ls
  | otherwise = align $ sep line ls
  where
    auxiliary 0 _ x = x
    auxiliary _ (Just SingleLine) x = space <:> x
    auxiliary _ _ x = hardline <:> x
    los = lineLayout <$> ls

-- | Nest but with the standard value of two spaces.
stdNest :: Fmt -> Fmt
stdNest = nest 2

-- | Aligns line by line.
align :: Fmt -> Fmt
align a = P.align <$> a

-- | Indents everything by @i@, should never be used.
hardIndent :: Int -> Fmt -> Fmt
hardIndent i a = P.indent i <$> a

-- | Indents if in multiline by @i@ if in singleline it does not indent.
indent :: Int -> Fmt -> Fmt
indent i a = a <|> hardIndent i a

-- | Hard indents with the standard size of two.
hardStdIndent :: Fmt -> Fmt
hardStdIndent = hardIndent 2

-- | Idents with the standard size of two.
stdIndent :: Fmt -> Fmt
stdIndent = indent 2

-- | Creates a piece of text, it should not contain any new lines.
text :: T.Text -> Fmt
text t = do
  modify (\s -> s {lastOutput = Just Text})
  pure $ P.pretty t

-- | Adds brackets.
brackets :: Fmt -> Fmt
brackets a = text "[" <:> a <:> text "]"

-- | Adds braces.
braces :: Fmt -> Fmt
braces a = text "{" <:> a <:> text "}"

-- | Add parenthesis.
parens :: Fmt -> Fmt
parens a = text "(" <:> a <:> text ")"

-- | Depending on if @b@ is multiline then add a line between them and an indent
-- to @b@. If singleline then just seperate by a single space.
(<+/>) :: (Format b, Located b) => Fmt -> b -> Fmt
(<+/>) a b =
  case lineLayout b of
    Just MultiLine -> a </> hardStdIndent (fmt b)
    Just SingleLine -> a <+> fmt b
    Nothing -> do
      lo <- ask
      case lo of
        MultiLine -> a </> hardStdIndent (fmt b)
        SingleLine -> a <+> fmt b

-- | If in a singleline layout then concatenate with 'nil' and in multiline
-- concatenate by a line.
(<:/>) :: Fmt -> Fmt -> Fmt
a <:/> b = a <:> (nil <|> hardline) <:> b

-- | Concatenates @a@ and @b@.
(<:>) :: Fmt -> Fmt -> Fmt
a <:> b = do
  c <- fmtComments a
  a' <- a
  setTrailingComment a
  b' <- b
  pure $ c <> a' <> b'

-- | Concatenate with a space between.
(<+>) :: Fmt -> Fmt -> Fmt
a <+> b = a <:> space <:> b

-- | Concatenate with a space if in singleline layout and concatenate by a
-- line in multiline.
(</>) :: Fmt -> Fmt -> Fmt
a </> b = a <:> line <:> b

-- | If in a singleline layout then choose @a@, if in a multiline layout choose
-- @b@.
(<|>) :: Fmt -> Fmt -> Fmt
a <|> b = do
  lo <- ask
  if lo == SingleLine
    then a
    else b

-- | If in singleline layout seperate by spaces. In a multiline layout seperate
-- by a single line if two neighbouring elements are singleline. Otherwise
-- sepereate by two lines.
sepDecs :: [Fmt] -> Fmt
sepDecs [] = nil
sepDecs as@(x : xs) = sep space as <|> (x <:> auxiliary x xs)
  where
    auxiliary _ [] = nil
    auxiliary prev (y : ys) = p <:> y <:> auxiliary y ys
      where
        p =
          case (lineLayout y, lineLayout prev) of
            (Just SingleLine, Just SingleLine) -> hardline
            _any -> hardline <:> hardline

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: P.Doc () -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts
