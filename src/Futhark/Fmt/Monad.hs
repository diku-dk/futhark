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
    addComments,
    sepDecs,
    fmtByLayout,
  )
where

import Control.Monad (foldM)
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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Language.Futhark.Parser.Monad (Comment (..))
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)

-- These are left associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.

infixl 7 <+/>

infixl 6 <:/>

infixl 6 <:>

infixl 6 <+>

infixl 6 </>

infixl 4 <|>

type Fmt = P.Doc ()

-- | This function allows to inspect the layout of an expression @a@ and if it
-- is singleline line then use format @s@ and if it is multiline format @m@.
fmtByLayout ::
  (Located a, Format s, Format m) => a -> s -> m -> FmtM Fmt
fmtByLayout a s m =
  case lineLayout a of
    SingleLine -> fmt s
    MultiLine -> fmt m

-- | This function determines the Layout of @a@ and if it is singleline then it
-- updates the monads enviroment to format singleline style otherwise format using
-- multiline style. It determines this by checking if the location of @a@ spans
-- over two or more lines.
localLayout :: (Located a) => a -> FmtM b -> FmtM b
localLayout a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayout a) m
    SingleLine -> m

-- | This function determines the Layout of @[a]@ and if it is singleline then it
-- updates the monads enviroment to format singleline style otherwise format using
-- multiline style. It determines this by checking if the locations of @[a]@
-- start and end at any different line number.
localLayoutList :: (Located a) => [a] -> FmtM b -> FmtM b
localLayoutList a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayoutList a) m
    SingleLine -> m

-- | This function uses the location of @a@ and prepends comments if the
-- comments location is less than the location of @a@. It format @b@ in
-- accordance with if @a@ is singleline or multiline using 'localLayout'. At last
-- it internally sets the state of the 'FmtM' monad to consider trailing
-- comments if they exists. This function should be always used when possible to
-- wrap FmtM Fmt around. It currently does not handle trailing comment perfectly.
-- See tests/fmt/traillingComments1.fut or the other test.
addComments :: (Located a, Format b) => a -> b -> FmtM Fmt
addComments a b = localLayout a $ do
  c <- fmtComments a
  f <- fmt b
  setTrailingComment a
  pure $ c <> f

-- | The internal state of the formatter monad 'FmtM'.
data FmtState = FmtState
  { -- | The comments that will be inserted, ordered by increasing order in regards to location.
    comments :: [Comment],
    -- The original source file that is being formatted.
    file :: BS.ByteString,
    -- Pending comment to be inserted before next newline (reverse order).
    pendingComments :: !(Maybe Comment),
    -- Keeps track of what type the last output was.
    lastOutput :: !(Maybe LastOutput)
  }
  deriving (Show, Eq, Ord)

-- | A data type to describe the last output used during formatting.
data LastOutput = Line | Space | Text deriving (Show, Eq, Ord)

-- | A data type to describe the layout the formatter is using currently.
data Layout = MultiLine | SingleLine deriving (Show, Eq)

-- | The format monad used to keep track of comments and layout. It is a a
-- combincation of a reader and state monad. The comments and reading from the
-- input file are the state monads job to deal with. While the reader monad
-- deals with the propagating the current layout.
type FmtM a = ReaderT Layout (State FmtState) a

-- | A typeclass that defines how an type can be formatted.
class Format a where
  fmt :: a -> FmtM Fmt

instance Format (FmtM Fmt) where
  fmt = id

instance Format Comment where
  fmt = comment . commentText

-- | Prepends comments.
fmtComments :: (Located a) => a -> FmtM Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
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
    c : cs -> case (locOf a, locOf c) of
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
lineLayout :: (Located a) => a -> Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
        then SingleLine
        else MultiLine
    NoLoc -> error "Formatting term without location."

-- | Determines the layout of @[a]@ by checking if it spans a single line or two
-- or more lines.
lineLayoutList :: (Located a) => [a] -> Layout
lineLayoutList as =
  case concatMap auxiliary as of
    (t : ts) | any (/= t) ts -> MultiLine
    _ -> SingleLine
  where
    auxiliary a =
      case locOf a of
        Loc start end -> [posLine start, posLine end]
        NoLoc -> error "Formatting term without location"

-- | Retrieves the last comments from the monad and concatenates them together.
popComments :: FmtM Fmt
popComments = do
  cs <- gets comments
  modify (\s -> s {comments = []})
  sep nil cs

-- | Using the location of @a@ get the segment of text in the original file to
-- create a @FmtM Fmt@.
fmtCopyLoc :: (Located a) => a -> FmtM Fmt
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
nil :: FmtM Fmt
nil = pure mempty

-- | Indents everything after a line occurs if in multiline and if in singleline
-- then indent.
nest :: (Format a) => Int -> a -> FmtM Fmt
nest i a = fmt a <|> (P.nest i <$> fmt a)

-- | A space.
space :: FmtM Fmt
space = modify (\s -> s {lastOutput = Just Space}) >> pure P.space

-- | Forces a line to be used regardless of layout, this should ideally not be
-- used.
hardline :: FmtM Fmt
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
line :: FmtM Fmt
line = space <|> hardline

-- | A comment.
comment :: T.Text -> FmtM Fmt
comment c = do
  modify (\s -> s {lastOutput = Just Line})
  pure $ P.pretty c <> P.line

-- in order to handle trailing comments its VERY important to
-- evaluate the seperator after each element in the list.
sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep _ [] = nil
sep s (a : as) = do
  a' <- fmt a
  foldM aux a' as
  where
    aux acc next = do
      s' <- fmt s -- MUST be formatted before next
      next' <- fmt next
      pure $ acc <> s' <> next'

-- | Seperates element by a @s@ followed by a space in singleline layout and
-- seperates by a line followed by a @s@ in multine layout.
sepLine :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepLine s = sep (s <:> space <|> hardline <:> s)

-- | This is used for function arguments. It seperates multiline arguments by
-- lines and singleline arguments by spaces.
sepArgs :: (Format a, Located a) => [a] -> FmtM Fmt
sepArgs [] = nil
sepArgs ls
  | any ((== MultiLine) . lineLayout) ls =
      sep nil $ zipWith3 auxiliary [0 :: Int ..] los ls
  | otherwise = align $ sep line ls
  where
    auxiliary 0 _ x = fmt x
    auxiliary _ SingleLine x = space <:> x
    auxiliary _ MultiLine x = hardline <:> x
    los = lineLayout <$> ls

-- | Nest but with the standard value of two spaces.
stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

-- | Aligns line by line.
align :: (Format a) => a -> FmtM Fmt
align a = P.align <$> fmt a

-- | Indents everything by @i@, should never be used.
hardIndent :: (Format a) => Int -> a -> FmtM Fmt
hardIndent i a = P.indent i <$> fmt a

-- | Indents if in multiline by @i@ if in singleline it does not indent.
indent :: (Format a) => Int -> a -> FmtM Fmt
indent i a = fmt a <|> hardIndent i a

-- | Hard indents with the standard size of two.
hardStdIndent :: (Format a) => a -> FmtM Fmt
hardStdIndent = hardIndent 2

-- | Idents with the standard size of two.
stdIndent :: (Format a) => a -> FmtM Fmt
stdIndent = indent 2

-- | Creates a piece of text, it should not contain any new lines.
text :: T.Text -> FmtM Fmt
text t = do
  modify (\s -> s {lastOutput = Just Text})
  pure $ P.pretty t

-- | Adds brackets.
brackets :: (Format a) => a -> FmtM Fmt
brackets a = text "[" <:> fmt a <:> text "]"

-- | Adds braces.
braces :: (Format a) => a -> FmtM Fmt
braces a = text "{" <:> fmt a <:> text "}"

-- | Add parenthesis.
parens :: (Format a) => a -> FmtM Fmt
parens a = text "(" <:> fmt a <:> text ")"

-- | Depending on if @b@ is multiline then add a line between them and an indent
-- to @b@. If singleline then just seperate by a single space.
(<+/>) :: (Format a, Format b, Located b) => a -> b -> FmtM Fmt
(<+/>) a b =
  case lineLayout b of
    MultiLine -> a </> hardStdIndent b
    SingleLine -> a <+> b

-- | If in a singleline layout then concatenate with 'nil' and in multiline
-- concatenate by a line.
(<:/>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:/> b = a <:> (nil <|> hardline) <:> b

-- | Concatenates @a@ and @b@.
(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = (<>) <$> fmt a <*> fmt b

-- | Concatenate with a space between.
(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

-- | Concatenate with a space if in singleline layout and concatenate by a
-- line in multiline.
(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

-- | If in a singleline layout then choose @a@, if in a multiline layout choose
-- @b@.
(<|>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <|> b = do
  lo <- ask
  if lo == SingleLine
    then fmt a
    else fmt b

-- | If in singleline layout seperate by spaces. In a multiline layout seperate
-- by a single line if two neighbouring elements are singleline. Otherwise
-- sepereate by two lines.
sepDecs ::
  (Format a, Located a) =>
  [a] ->
  FmtM Fmt
sepDecs [] = nil
sepDecs as@(x : xs) = sep space as <|> (x <:> auxiliary x xs)
  where
    auxiliary _ [] = nil
    auxiliary prev (y : ys) = p <:> fmt y <:> auxiliary y ys
      where
        p =
          case (lineLayout y, lineLayout prev) of
            (SingleLine, SingleLine) -> line
            _any -> line <:> line

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts
