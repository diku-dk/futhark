module Futhark.CLI.Fmt.AST
  ( Fmt,
    -- functions for building fmt
    nil,
    nest,
    stdNest,
    code,
    space,
    line,
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
    indent,
    stdIndent,
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
    simplify,
    simplifyByLoc,
    simplifyByLocList,
    comment,
    sepLoc
  )
where

import Data.Text qualified as T
import Prettyprinter qualified as P
import Prettyprinter.Render.Text (renderStrict)
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Reader
    ( ReaderT(..), MonadReader(..) )
import Control.Monad.State
    ( StateT, gets, modify, MonadState(..), evalStateT )
import Data.Loc ( Located(..), Loc(..), posLine, posCoff, locStart)
import Language.Futhark.Parser.Monad ( Comment(..) )
import Data.Maybe (isJust)

-- These are left associative since we want to evaluate the monadic
-- computation from left to right. Since the left most expression is
-- printed first and our monad is checking if a comment should be
-- printed.

infixl 6 <:>
infixl 6 <+>
infixl 6 </>
infixl 7 <+/>
infixl 4 <|>
infixl 6 <:/>

data Fmt
  = Nil
  | Space
  | Line
  | Doc !(P.Doc ())
  | Align !Fmt
  | Indent !Int !Fmt
  | Nest !Int !Fmt
  | Sep !Fmt ![Fmt]
  | Union Fmt Fmt
  | Con !Fmt !Fmt deriving Show

flatten :: Fmt -> Fmt
flatten Nil = Nil
flatten Space = Space
flatten Line = Space
flatten (Doc a) = Doc a
flatten (Align a) = flatten a
flatten (Indent _ a) = flatten a
flatten (Nest _ a) = flatten a
flatten (Sep a as) = Sep (flatten a) (flatten <$> as)
flatten (Union a _) = a
flatten (Con a b) = Con (flatten a) (flatten b)

compileToDoc :: Fmt -> P.Doc () 
compileToDoc Nil = mempty
compileToDoc Space = P.space
compileToDoc Line = P.line
compileToDoc (Doc a) = a
compileToDoc (Align a) = P.align $ compileToDoc a
compileToDoc (Indent i a) = P.indent i $ compileToDoc a
compileToDoc (Nest i a) = P.nest i $ compileToDoc a
compileToDoc (Sep a as) =
  P.concatWith (\x y -> x <> compileToDoc a <> y)
  $ compileToDoc <$> as
compileToDoc (Union _ a) = compileToDoc a
compileToDoc (Con a b) = compileToDoc a <> compileToDoc b

compile :: Fmt -> Fmt
compile = Doc . compileToDoc

simplify :: FmtM Fmt -> FmtM Fmt
simplify = fmap compile

compileToDocByLayout :: Layout -> Fmt -> P.Doc ()
compileToDocByLayout MultiLine = compileToDoc
compileToDocByLayout SingleLine = compileToDoc . flatten

-- parses comments infront of a and converts a to fmt using formatting function f
simplifyByLoc :: (Located a, Format b) => a -> b -> FmtM Fmt
simplifyByLoc a b = local (const $ lineLayout a) $ do
  c <- compileToDoc <$> fmtComments a
  f <- compileToDocByLayout <$> ask <*> fmt b
  pure $ Doc $ c <> f

simplifyByLocList :: (Located a, Format b) => [a] -> b -> FmtM Fmt
simplifyByLocList a b = local (const $ lineLayoutList a) $ do
  c <- compileToDoc <$> fmtComments a
  f <- compileToDocByLayout <$> ask <*> fmt b
  pure $ Doc $ c <> f

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
  
instance Format Comment where
  fmt = comment . commentText
  
prettyComment :: Comment -> P.Doc ()
prettyComment = P.pretty . commentText

-- Functions for operating on FmtM monad
fmtComments :: (Located a) => a -> FmtM Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s {comments = cs}
      f <- compileToDoc <$> fmtComments a
      c' <- compileToDoc <$> comment (commentText c)
      pure $ Doc $ c' <> f -- fmts remaining comments
    _any -> pure $ Doc mempty

fmtTrailingComment :: (Located a) => a -> FmtM Fmt
fmtTrailingComment a = do
  s <- get
  case comments s of
    c : cs | lineLayout a == SingleLine && aux a == aux c -> do
      put $ s {comments = cs}
      fmt $ comment $ commentText c
    _any -> pure $ Doc mempty
  where
    aux n = s
      where
        Loc s _ = locStart $ locOf n

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
  pure $ Doc $ P.vsep $ prettyComment <$> cs

fmtCopyLoc :: Located a => a -> FmtM Fmt 
fmtCopyLoc a = do
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
nil = pure $ Doc mempty

nest :: (Format a) => Int -> a -> FmtM Fmt
nest i = fmap (Nest i) . fmt

space :: FmtM Fmt
space = pure Space

line :: FmtM Fmt
line = pure Line

(<|>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <|> b = Union <$> fmt a <*> fmt b

comment :: T.Text -> FmtM Fmt
comment c = pure (Doc $ P.pretty c <> P.line)

sep :: (Format a, Format b) => a -> [b] -> FmtM Fmt
sep s as = Sep <$> fmt s <*> mapM fmt as

-- I am not sure this fulfills idemppotence.
sepLoc :: (Format a, Located a) => [a] -> FmtM Fmt
sepLoc [] = nil
sepLoc ls@(y:ys)
  | lineLayout y == MultiLine && allIsSame ys = align $ fmt y <+> sep space ys
  | otherwise = align $ sep line ls 
  where
    allIsSame [] = True
    allIsSame (x:xs) = all (isSame x) xs
    isSame a b =
      case (locOf a, locOf b) of
        (Loc s0 e0, Loc s1 e1) ->
          posLine s0 == posLine s1 &&
          posLine e0 == posLine e1
        _any -> False

stdNest :: (Format a) => a -> FmtM Fmt
stdNest = nest 2

align :: (Format a) => a -> FmtM Fmt
align = fmap Align . fmt

indent :: (Format a) => Int -> a -> FmtM Fmt
indent i = fmap (Indent i) . fmt

stdIndent :: Format a => a -> FmtM Fmt
stdIndent = indent 2

code :: T.Text -> FmtM Fmt
code = pure . Doc . P.pretty

brackets :: (Format a) => a -> FmtM Fmt
brackets a = code "[" <:> fmt a <:> code "]" 

braces :: (Format a) => a -> FmtM Fmt
braces a = code "{" <:> fmt a <:> code "}"

parens :: (Format a) => a -> FmtM Fmt
parens a = code "(" <:> fmt a <:> code ")"

(<:/>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:/> b = a <:> (nil <|> line) <:> b

(<:>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <:> b = Con <$> fmt a <*> fmt b

(<+>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM Fmt
a </> b = a <:> line <:> b

colon :: FmtM Fmt
colon = pure $ Doc P.colon

sepFilter :: (Format a, Format b) => [Bool] -> a -> [b] -> FmtM Fmt
sepFilter bs s xs =
  sep s
  $ map snd
  $ filter fst
  $ zip bs xs

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts . compileToDoc
