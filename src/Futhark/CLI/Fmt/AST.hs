module Futhark.CLI.Fmt.AST
  ( Fmt,
    -- functions for building fmt
    nil,
    nest,
    stdNest,
    code,
    space,
    line,
    comment,
    sep,
    sepSpace,
    sepLine,
    brackets,
    braces,
    parens,
    (<+>),
    (</>),
    (<:>),
    colon,
    sepNonEmpty,
    pretty,
    isEmpty,
    FmtM,
    fmtComments,
    buildFmt,
    popComments,
    sepByLayout,
    runFormat,
    Format (..)
  )
where

import Data.Text qualified as T
import Prettyprinter qualified as P hiding (Doc)
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (renderStrict)
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Reader
    ( ReaderT(..), MonadReader(..) )
import Control.Monad.State
    ( StateT, gets, modify, MonadState(..), evalStateT )
import Data.Loc ( Located(..), Loc(..), posLine )
import Language.Futhark.Parser.Monad ( Comment(..) )

-- These are left associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.
infixl 6 <:>
infixl 6 <+>
infixl 6 </>
  
type Fmt = Doc ()

newtype FmtState = FmtState
  {comments :: [Comment]}
  deriving (Show, Eq, Ord)

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
      fmt c <:> fmtComments a -- fmts remaining comments
    _ -> nil

-- parses comments infront of a and converts a to fmt using formatting function f
buildFmt :: (Located a) => a -> FmtM Fmt -> FmtM Fmt -> FmtM Fmt
buildFmt a single multi = local (const $ lineLayout a) $ do
  c <- fmtComments a
  m <- ask
  a' <- if m == SingleLine then single else multi
  -- c' <- trailingComment a
  pure $ c <> a'

lineLayout :: (Located a) => a -> Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
        then SingleLine
        else MultiLine
    NoLoc -> undefined -- should throw an error

popComments :: FmtM Fmt
popComments = do
  cs <- gets comments
  modify (\s -> s {comments = []})
  sep nil cs

sepByLayout :: (Located a, Format b, Format c) => a -> b -> c -> FmtM Fmt
sepByLayout loc a b =
  case lineLayout loc of
    MultiLine -> stdNest (a </> b)
    SingleLine -> a <+> b

runFormat :: FmtM a -> [Comment] -> a
runFormat format cs = runIdentity $ evalStateT (runReaderT format e) s
  where
    s = FmtState {comments = cs}
    e = MultiLine

nil :: FmtM Fmt
nil = pure mempty

nest :: (Format a) => Int -> a -> FmtM Fmt
nest i = fmap (P.nest i) . fmt

space :: FmtM Fmt
space = pure P.space

line :: FmtM Fmt
line = pure P.line

sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep s xs = aux <$> fmt s <*> mapM fmt xs
  where
    aux z = P.concatWith (\a b -> a <> z <> b)

stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

code :: T.Text -> FmtM Fmt
code = pure . P.pretty

comment :: T.Text -> FmtM Fmt
comment = (<:> line) . code

brackets :: (Format a) => a -> FmtM Fmt
brackets = fmap P.brackets . fmt

braces :: (Format a) => a -> FmtM Fmt
braces = fmap P.braces . fmt

parens :: (Format a) => a -> FmtM Fmt
parens = fmap P.parens . fmt

sepSpace :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepSpace s = sep (fmt s <:> space)

sepLine :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepLine s = sep (line <:> fmt s)

(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = (P.<>) <$> fmt a <*> fmt b

(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

colon :: FmtM Fmt
colon = pure P.colon

sepNonEmpty :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepNonEmpty = sep

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts

isEmpty :: Fmt -> Bool
isEmpty = const False
