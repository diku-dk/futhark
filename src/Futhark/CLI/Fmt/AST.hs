module Futhark.CLI.Fmt.AST
  ( Fmt,
    FmtIR,
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
    -- sepLoc,
    fmtByLoc,
    buildFmtByLoc,
    comment
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
infixl 4 <|>
  
newtype Fmt = Fmt (Doc ())

fmtToIR :: Fmt -> FmtIR
fmtToIR (Fmt a) = Lit a

data FmtIR
  = Nil
  | Space
  | Line
  | Lit (Doc ())
  | Align FmtIR
  | Indent Int FmtIR
  | Nest Int FmtIR
  | Sep FmtIR [FmtIR]
  | Union FmtIR FmtIR
  | Con FmtIR FmtIR deriving Show

flatten :: FmtIR -> FmtIR
flatten Nil = Nil
flatten Space = Space
flatten Line = Space
flatten (Lit a) = Lit a
flatten (Align a) = flatten a
flatten (Indent _ a) = flatten a
flatten (Nest _ a) = flatten a
flatten (Sep a as) = Sep (flatten a) (flatten <$> as)
flatten (Union a _) = a
flatten (Con a b) = Con (flatten a) (flatten b)

compileToDoc :: FmtIR -> Doc () 
compileToDoc Nil = mempty
compileToDoc Space = P.space
compileToDoc Line = P.line
compileToDoc (Lit a) = a
compileToDoc (Align a) = P.align $ compileToDoc a
compileToDoc (Indent i a) = P.indent i $ compileToDoc a
compileToDoc (Nest i a) = P.nest i $ compileToDoc a
compileToDoc (Sep a as) =
  P.concatWith (\x y -> x <> compileToDoc a <> y)
  $ compileToDoc <$> as
compileToDoc (Union _ a) = compileToDoc a
compileToDoc (Con a b) = compileToDoc a <> compileToDoc b

compile :: FmtIR -> Fmt
compile = Fmt . compileToDoc

compileM :: FmtM FmtIR -> FmtM Fmt
compileM = fmap compile

compileByLayout :: Layout -> FmtIR -> Fmt
compileByLayout MultiLine = compile
compileByLayout SingleLine = compile . flatten

-- parses comments infront of a and converts a to fmt using formatting function f
buildFmt :: (Located a, Format b) => a -> b -> b -> FmtM Fmt
buildFmt a single multi = local (const $ lineLayout a) $ do
  Fmt c <- fmtComments a
  m <- ask
  Fmt f <- compile <$> if m == SingleLine then fmtIR single else fmtIR multi
  pure $ Fmt $ c <> f

buildFmtByLoc :: (Located a, Format b) => a -> b -> FmtM Fmt
buildFmtByLoc a fmt_ir = local (const $ lineLayout a) $ do
  Fmt c <- fmtComments a
  Fmt f <- compileByLayout <$> ask <*> fmtIR fmt_ir
  pure $ Fmt $ c <> f

data FmtState = FmtState
  {comments :: [Comment], -- the comments
   file :: T.Text} -- The original source file 
  deriving (Show, Eq, Ord)

data Layout = MultiLine | SingleLine deriving (Show, Eq)

-- State monad to keep track of comments and layout.
type FmtM a = ReaderT Layout (StateT FmtState Identity) a

class Format a where
  fmtIR :: a -> FmtM FmtIR
  fmtIR = fmap fmtToIR . fmt
  
  fmt :: a -> FmtM Fmt
  fmt = compileM . fmtIR

instance Format (FmtM FmtIR) where
  fmtIR = id
  
instance Format (FmtM Fmt) where
  fmtIR = fmap fmtToIR
  
instance Format Comment where
  fmtIR = comment . commentText
  
prettyComment :: Comment -> Doc ()
prettyComment = P.pretty . commentText

-- Functions for operating on FmtM monad
fmtComments :: (Located a) => a -> FmtM Fmt
fmtComments a = do
  s <- get
  case comments s of
    c : cs | locOf a > locOf c -> do
      put $ s {comments = cs}
      Fmt f <- fmtComments a
      Fmt c' <- fmt $ comment $ commentText c
      pure $ Fmt $ c' <> f -- fmts remaining comments
    _any -> pure $ Fmt mempty

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
  pure $ Fmt $ mconcat $ prettyComment <$> cs

fmtByLoc :: Located a => a -> FmtM FmtIR 
fmtByLoc a = do
  f <- gets file
  case locOf a of
    Loc sPos ePos ->
      let sOff = posCoff sPos
          eOff = posCoff ePos
      in code $ T.take (eOff-sOff) $ T.drop sOff f
    NoLoc -> undefined

(<+/>) :: (Format a, Format b, Located b) => a -> b -> FmtM FmtIR
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

nil :: FmtM FmtIR
nil = pure $ Lit mempty

nest :: (Format a) => Int -> a -> FmtM FmtIR
nest i = fmap (Nest i) . fmtIR

space :: FmtM FmtIR
space = pure Space

line :: FmtM FmtIR
line = pure Line

(<|>) :: (Format a, Format b) => a -> b -> FmtM FmtIR
a <|> b = Union <$> fmtIR a <*> fmtIR b

comment :: T.Text -> FmtM FmtIR
comment c = pure (Lit $ P.pretty c <> P.line)

sep :: (Format a, Format b) => a -> [b] -> FmtM FmtIR
sep s as = Sep <$> fmtIR s <*> mapM fmtIR as

-- I am not sure this fulfills idemppotence.
{-
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
-}

stdNest :: (Format a) => a -> FmtM FmtIR
stdNest = nest 2

align :: (Format a) => a -> FmtM FmtIR
align = fmap Align . fmtIR

indent :: (Format a) => Int -> a -> FmtM FmtIR
indent i = fmap (Indent i) . fmtIR

stdIndent :: Format a => a -> FmtM FmtIR
stdIndent = indent 2

code :: T.Text -> FmtM FmtIR
code = pure . Lit . P.pretty

brackets :: (Format a) => a -> FmtM FmtIR
brackets a = code "[" <:> fmtIR a <:> code "]" 

braces :: (Format a) => a -> FmtM FmtIR
braces a = code "{" <:> fmtIR a <:> code "}"

parens :: (Format a) => a -> FmtM FmtIR
parens a = code "(" <:> fmtIR a <:> code ")"

(<:>) :: (Format a, Format b) => a -> b -> FmtM FmtIR
a <:> b = Con <$> fmtIR a <*> fmtIR b

(<+>) :: (Format a, Format b) => a -> b -> FmtM FmtIR
a <+> b = a <:> space <:> b

(</>) :: (Format a, Format b) => a -> b -> FmtM FmtIR
a </> b = a <:> line <:> b

colon :: FmtM FmtIR
colon = pure $ Lit P.colon

sepNonEmpty :: (Format a, Format b) => a -> [b] -> FmtM FmtIR
sepNonEmpty = sep

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty (Fmt a) = renderStrict $ P.layoutPretty layoutOpts a

isEmpty :: FmtIR -> Bool
isEmpty = const False
