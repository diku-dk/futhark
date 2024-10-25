module Futhark.Fmt.Monad
  ( Fmt,
    -- functions for building fmt
    nil,
    nest,
    stdNest,
    code,
    space,
    line,
    softline, 
    sep,
    brackets,
    braces,
    parens,
    (<|>),
    (<+>),
    (</>),
    (<:>),
    (<+/>),
    (<:/>),
    (<:+/>),
    indent,
    softIndent,
    stdIndent,
    softStdIndent,
    colon,
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
    sepLoc,
    localLayout,
    localLayoutList,
    prependComments
  )
where

import Data.Text qualified as T
import Data.Text.Encoding qualified as T 
import Data.ByteString qualified as BS
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Reader
    ( ReaderT(..), MonadReader(..) )
import Control.Monad.State
    ( StateT, gets, modify, MonadState(..), evalStateT )
import Data.Loc ( Located(..), Loc(..), posLine, posCoff )
import Language.Futhark.Parser.Monad ( Comment(..) )

-- These are left associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.

infixl 7 <+/>
infixl 6 <:+/>
infixl 6 <:/>
infixl 6 <:>
infixl 6 <+>
infixl 6 </>
infixl 4 <|>


type Fmt = P.Doc ()

localLayout :: (Located a) => a -> FmtM b -> FmtM b
localLayout a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayout a) m
    SingleLine -> m

localLayoutList :: (Located a) => [a] -> FmtM b -> FmtM b
localLayoutList a m = do
  lo <- ask
  case lo of
    MultiLine -> local (const $ lineLayoutList a) m
    SingleLine -> m
  
-- parses comments infront of a and converts a to fmt using formatting function f
prependComments :: (Located a, Format b) => a -> b -> FmtM Fmt
prependComments a b = localLayout a $ do
  c <- fmtComments a
  f <- fmt b
  tc <- fmtTrailingComment a 
  pure $ c <> f <> tc

data FmtState = FmtState
  {comments :: [Comment], -- the comments
   file :: BS.ByteString} -- The original source file 
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
      f <- fmtComments a
      c' <- fmt c
      pure $ c' <> f -- fmts remaining comments
    _any -> pure mempty

fmtTrailingComment :: (Located a) => a -> FmtM Fmt
fmtTrailingComment a = do 
  layout <- ask
  if layout == MultiLine then do
    s <- get
    case comments s of
      c : cs | locOf a == locOf c -> do 
        put $ s {comments = cs} 
        fmt c 
      _any -> pure mempty 
  else pure mempty 

lineLayout :: (Located a) => a -> Layout
lineLayout a =
  case locOf a of
    Loc start end ->
      if posLine start == posLine end
        then SingleLine
        else MultiLine
    NoLoc -> undefined -- should throw an error

lineLayoutList :: (Located a) => [a] -> Layout
lineLayoutList as =
  case concatMap auxiliary as of
    (t:ts) | any (/=t) ts -> MultiLine
    _ -> SingleLine
  where
    auxiliary a =
      case locOf a of
        Loc start end -> [posLine start, posLine end]
        NoLoc -> undefined -- should throw an error

popComments :: FmtM Fmt
popComments = do
  cs <- gets comments
  modify (\s -> s {comments = []})
  sep nil cs

fmtCopyLoc :: Located a => a -> FmtM Fmt 
fmtCopyLoc a = do
  f <- gets file
  case locOf a of
    Loc sPos ePos ->
      let sOff = posCoff sPos
          eOff = posCoff ePos
      in case T.decodeASCII' $ BS.take (eOff-sOff) $ BS.drop sOff f of 
        Nothing -> undefined -- Should throw an error TODO
        Just lit -> code lit
    NoLoc -> undefined -- should throw an error TODO

(<+/>) :: (Format a, Format b, Located b) => a -> b -> FmtM Fmt
(<+/>) a b =
  case lineLayout b of
    MultiLine -> a </> stdIndent b
    SingleLine -> a <+> b

runFormat :: FmtM a -> [Comment] -> T.Text -> a
runFormat format cs file = runIdentity $ evalStateT (runReaderT format e) s
  where
    s = FmtState {comments = cs,
                  file = T.encodeUtf8 file }
    e = MultiLine

nil :: FmtM Fmt
nil = pure mempty

nest :: (Format a) => Int -> a -> FmtM Fmt
nest i a = fmt a <|> (P.nest i <$> fmt a)

space :: FmtM Fmt
space = pure P.space

line :: FmtM Fmt
line = pure P.line

softline :: FmtM Fmt
softline = space <|> line

(<|>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <|> b = do
  lo <- ask
  if lo == SingleLine
    then fmt a
    else fmt b

comment :: T.Text -> FmtM Fmt
comment c = pure $ P.pretty c <> P.line

sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep s as = aux <$> fmt s <*> mapM fmt as
  where
    aux s' = P.concatWith (\a b -> a <> s' <> b)

-- I am not sure this fulfills idemppotence.
sepLoc :: (Format a, Located a) => [a] -> FmtM Fmt
sepLoc [] = nil
sepLoc ls
  | any ((==MultiLine) . lineLayout) ls =
      sep nil $ zipWith3 auxiliary [0 :: Int ..] los ls
  | otherwise = align $ sep softline ls
  where 
    auxiliary 0 _ x = fmt x
    auxiliary _ SingleLine x = space <:> x
    auxiliary _ MultiLine x = line <:> x
    los = drop (length ls - 1) $ cycle $ lineLayout <$> ls
    

stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

align :: (Format a) => a -> FmtM Fmt
align a = fmt a <|> (P.align <$> fmt a)

indent :: (Format a) => Int -> a -> FmtM Fmt
indent i a = P.indent i <$> fmt a

softIndent :: (Format a) => Int -> a -> FmtM Fmt
softIndent i a = fmt a <|> indent i a

stdIndent :: Format a => a -> FmtM Fmt
stdIndent = indent 2

softStdIndent :: Format a => a -> FmtM Fmt
softStdIndent = softIndent 2

code :: T.Text -> FmtM Fmt
code = pure . P.pretty

brackets :: (Format a) => a -> FmtM Fmt
brackets a = code "[" <:> fmt a <:> code "]" 

braces :: (Format a) => a -> FmtM Fmt
braces a = code "{" <:> fmt a <:> code "}"

parens :: (Format a) => a -> FmtM Fmt
parens a = code "(" <:> fmt a <:> code ")"

(<:/>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:/> b = a <:> (nil <|> line) <:> b

(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = (<>) <$> fmt a <*> fmt b

(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

(<:+/>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:+/> b = a <:> softline <:> b

colon :: FmtM Fmt
colon = pure P.colon

sepFilter :: (Format a, Format b) => [Bool] -> a -> [b] -> FmtM Fmt
sepFilter bs s xs =
  sep s
  $ map snd
  $ filter fst
  $ zip bs xs

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts
