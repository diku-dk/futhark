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
    (<+/>),
    indent,
    stdIndent,
    colon,
    sepNonEmpty,
    pretty,
    isEmpty,
    FmtM,
    fmtComments,
    buildFmt,
    popComments,
    runFormat,
    Format (..),
    align,
    sepLoc,
    fmtByLoc
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
import Data.Loc ( Located(..), Loc(..), posLine, posCoff)
import Language.Futhark.Parser.Monad ( Comment(..) )

-- These are left associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.

infixl 6 <:>
infixl 6 <+>
infixl 6 </>
infixl 7 <+/>
  
newtype Fmt = Fmt (Doc ())


data FmtState = FmtState
  {comments :: [Comment], -- the comments
   file :: T.Text} -- The original source file 
  deriving (Show, Eq, Ord)

data Layout = MultiLine | SingleLine deriving (Show, Eq)

-- State monad to keep track of comments and layout.
type FmtM a = ReaderT Layout (StateT FmtState Identity) a

class Format a where
  fmt :: a -> FmtM Fmt

instance Format (FmtM Fmt) where
  fmt = id

instance Format T.Text where
  fmt = code

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
  Fmt c <- fmtComments a
  m <- ask
  Fmt a' <- if m == SingleLine then single else multi
  -- c' <- trailingComment a
  pure $ toFmt $ c <> a'

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

fmtByLoc :: Located a => a -> FmtM Fmt 
fmtByLoc a = do
  f <- gets file
  case locOf a of
    Loc sPos ePos ->
      let sOff = posCoff sPos
          eOff = posCoff ePos
      in code $ T.take (eOff-sOff) $ T.drop sOff f
    NoLoc -> undefined

(<+/>) :: (Format a, Format b, Located b) => a -> b -> FmtM Fmt
(<+/>) a b =
  case lineLayout b of
    MultiLine -> a </> stdIndent b
    SingleLine -> a <+> b

runFormat :: FmtM a -> [Comment] -> T.Text -> a
runFormat format cs file = runIdentity $ evalStateT (runReaderT format e) s
  where
    s = FmtState {comments = cs,
                  file = file }
    e = MultiLine

nil :: FmtM Fmt
nil = pure $ toFmt mempty

mapFmt :: (Doc () -> Doc ()) -> Fmt -> Fmt
mapFmt f (Fmt a) = Fmt $ f a

mapFmtM :: (Doc () -> Doc ()) -> FmtM Fmt -> FmtM Fmt
mapFmtM f = fmap (mapFmt f)

unFmt :: Fmt -> Doc ()
unFmt (Fmt a) = a

unFmtM :: FmtM Fmt -> FmtM (Doc ())
unFmtM = fmap unFmt

toFmt :: Doc () -> Fmt
toFmt = Fmt

toFmtM :: FmtM (Doc ()) -> FmtM Fmt
toFmtM = fmap toFmt

nest :: (Format a) => Int -> a -> FmtM Fmt
nest i = mapFmtM (P.nest i) . fmt

space :: FmtM Fmt
space = pure $ Fmt P.space

line :: FmtM Fmt
line = pure $ Fmt P.line

sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep s xs = toFmtM $ aux <$> unFmtM (fmt s) <*> mapM (unFmtM . fmt) xs
  where
    aux z = P.concatWith (\a b -> a <> z <> b)

-- I am not sure this fulfills idemppotence.
sepLoc :: (Format a, Located a) => [a] -> FmtM Fmt
sepLoc [] = nil
sepLoc (y:ys) = fmt y <:> auxiliary Nothing nil ys
  where
    auxiliary :: (Format a, Located a) => Maybe a -> FmtM Fmt -> [a] -> FmtM Fmt
    auxiliary _ acc [] = acc
    auxiliary prev acc (x:xs) =
      if lineLayout x == SingleLine && isSame
        then auxiliary (Just x) (acc <+> x) xs
        else auxiliary (Just x) (acc </> x) xs
      where
        
        isSame =
          case (locOf <$> prev, locOf x) of
            (Just (Loc s0 e0), Loc s1 e1) ->
              posLine s0 == posLine s1 &&
              posLine e0 == posLine e1
            _any -> True

stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

align :: (Format a) => a -> FmtM Fmt
align = mapFmtM P.align . fmt

indent :: (Format a) => Int -> a -> FmtM Fmt
indent i = mapFmtM (P.indent i) . fmt

stdIndent :: Format a => a -> FmtM Fmt
stdIndent = indent 2

code :: T.Text -> FmtM Fmt
code = pure . toFmt . P.pretty

comment :: T.Text -> FmtM Fmt
comment = (<:> line) . code

brackets :: (Format a) => a -> FmtM Fmt
brackets = mapFmtM P.brackets . fmt

braces :: (Format a) => a -> FmtM Fmt
braces = mapFmtM P.braces . fmt

parens :: (Format a) => a -> FmtM Fmt
parens = mapFmtM P.parens . fmt

sepSpace :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepSpace s = sep (fmt s <:> space)

sepLine :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepLine s = sep (line <:> fmt s)

(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = toFmtM $ (P.<>) <$> unFmtM (fmt a) <*> unFmtM (fmt b)

(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

colon :: FmtM Fmt
colon = pure $ toFmt P.colon

sepNonEmpty :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sepNonEmpty = sep

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts . unFmt

isEmpty :: Fmt -> Bool
isEmpty = const False
