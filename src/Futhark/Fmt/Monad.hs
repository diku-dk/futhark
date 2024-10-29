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
    indent,
    softIndent,
    stdIndent,
    softStdIndent,
    sepFilter,
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
    prependComments,
    sepDecs,
    fmtByLayout,
  )
where

import Control.Monad(foldM)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
  ( MonadReader (..),
    ReaderT (..),
  )
import Control.Monad.State
  ( MonadState (..),
    StateT,
    evalStateT,
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
-- is singline line then use format @s@ and if it is multiline format @m@.
fmtByLayout ::
  (Located a, Format s, Format m) => a -> s -> m -> FmtM Fmt
fmtByLayout a s m =
  case lineLayout a of
    SingleLine -> fmt s
    MultiLine -> fmt m

-- | This function determines the Layout of @a@ and if it is singline then it
-- updates the monads enviroment to format singline style otherwise format using
-- multiline style. It determines this by checking if the location of @a@ spans
-- over two or more lines.
localLayout :: (Located a) => a -> FmtM b -> FmtM b
localLayout a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayout a) m
    SingleLine -> m

-- | This function determines the Layout of @[a]@ and if it is singline then it
-- updates the monads enviroment to format singline style otherwise format using
-- multiline style. It determines this by checking if the locations of @[a]@
-- start and end at any different line number.
localLayoutList :: (Located a) => [a] -> FmtM b -> FmtM b
localLayoutList a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayoutList a) m
    SingleLine -> m

-- | parses comments infront of a and converts a to fmt using formatting function f
prependComments :: (Located a, Format b) => a -> b -> FmtM Fmt
prependComments a b = localLayout a $ do
  c <- fmtComments a
  f <- fmt b
  setTrailingComment a
  pure $ c <> f

data FmtState = FmtState
  { -- | The list comments
    comments :: [Comment], 
    -- The original source file
    file :: BS.ByteString,
    -- pending comment to be inserted before next newline (reverse order)
    pendingComments :: !(Maybe Comment),
    -- keeps track of what type the last output was
    lastOutput :: !(Maybe LastOutput)
  }
  deriving (Show, Eq, Ord)

data LastOutput = Line | Space | Text deriving (Show, Eq, Ord)

data Layout = MultiLine | SingleLine deriving (Show, Eq)

-- State monad to keep track of comments and layout.
type FmtM a = ReaderT Layout (StateT FmtState Identity) a

class Format a where
  fmt :: a -> FmtM Fmt

instance Format (FmtM Fmt) where
  fmt = id

instance Format Comment where
  fmt = comment . commentText

-- Functions for operating on FmtM monad
fmtComments :: (Located a) => a -> FmtM Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s {comments = cs}
      pre <- fmtPre 
      f <- fmtComments a -- fmts remaining comments
      c' <- fmt c
      pure $ pre <> c' <> f 
    _any -> pure mempty
    where
      fmtPre = do
        lastO <- gets lastOutput
        case lastO of 
          Nothing -> nil
          Just Line -> nil
          Just _ -> modify (\s -> s{lastOutput = Just Line}) >> hardline
                    
setTrailingComment :: (Located a) => a -> FmtM ()
setTrailingComment a = do 
  s <- get
  case comments s of 
    c : cs  -> case (locOf a, locOf c) of 
      -- comment on same line as term a
      (Loc _sALoc eALoc, Loc _sCLoc eCLoc) | posLine eALoc == posLine eCLoc -> do
          put $ s {comments = cs,
                   pendingComments = Just c} 
      _any -> pure ()
    _ -> pure ()

lineLayout :: (Located a) => a -> Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
        then SingleLine
        else MultiLine
    NoLoc -> error "Formatting term without location"

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

popComments :: FmtM Fmt
popComments = do
  cs <- gets comments
  modify (\s -> s {comments = []})
  sep nil cs

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

runFormat :: FmtM a -> [Comment] -> T.Text -> a
runFormat format cs file = runIdentity $ evalStateT (runReaderT format e) s
  where
    s =
      FmtState
        { comments = cs,
          file = T.encodeUtf8 file,
          pendingComments = Nothing,
          lastOutput = Nothing
        }
    e = MultiLine

nil :: FmtM Fmt
nil = pure mempty

nest :: (Format a) => Int -> a -> FmtM Fmt
nest i a = fmt a <|> (P.nest i <$> fmt a)

space :: FmtM Fmt
space = modify (\s -> s{lastOutput = Just Space}) >> pure P.space

hardline :: FmtM Fmt
hardline = do 
  pc <- gets pendingComments
  case pc of
    Just c -> do 
      modify $ \s -> s{pendingComments = Nothing,
                       lastOutput = Just Line}
      (P.space <>) <$> fmt c
    Nothing -> pure P.line

line :: FmtM Fmt
line = space <|> hardline

comment :: T.Text -> FmtM Fmt
comment c = pure $ P.pretty c <> P.line

-- in order to handle trailing comments its VERY important to
-- evaluate the seperator after each element in the list
sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep _ [] = nil
sep s (a:as) = do 
  a' <- fmt a
  foldM aux a' as
  where 
    aux acc next = do
      s' <- fmt s -- MUST be formatted before next
      next' <- fmt next
      pure $ acc <> s' <> next'


sepLine :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepLine s = sep (s <:> space <|> hardline <:> s)

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

stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

align :: (Format a) => a -> FmtM Fmt
align a = fmt a <|> (P.align <$> fmt a)

indent :: (Format a) => Int -> a -> FmtM Fmt
indent i a = P.indent i <$> fmt a

softIndent :: (Format a) => Int -> a -> FmtM Fmt
softIndent i a = fmt a <|> indent i a

stdIndent :: (Format a) => a -> FmtM Fmt
stdIndent = indent 2

softStdIndent :: (Format a) => a -> FmtM Fmt
softStdIndent = softIndent 2

text :: T.Text -> FmtM Fmt
text t = do 
  modify (\s -> s{lastOutput = Just Text})
  pure $ P.pretty t

brackets :: (Format a) => a -> FmtM Fmt
brackets a = text "[" <:> fmt a <:> text "]"

braces :: (Format a) => a -> FmtM Fmt
braces a = text "{" <:> fmt a <:> text "}"

parens :: (Format a) => a -> FmtM Fmt
parens a = text "(" <:> fmt a <:> text ")"

(<+/>) :: (Format a, Format b, Located b) => a -> b -> FmtM Fmt
(<+/>) a b =
  case lineLayout b of
    MultiLine -> a </> stdIndent b
    SingleLine -> a <+> b

(<:/>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:/> b = a <:> (nil <|> hardline) <:> b

(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = (<>) <$> fmt a <*> fmt b

(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

(<|>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <|> b = do
  lo <- ask
  if lo == SingleLine
    then fmt a
    else fmt b

sepFilter :: (Format a, Format b) => [Bool] -> a -> [b] -> FmtM Fmt
sepFilter bs s xs =
  sep s $
    map snd $
      filter fst $
        zip bs xs

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
