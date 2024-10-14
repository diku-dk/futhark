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
    colon,
    sepNonEmpty,
    pretty,
    isEmpty
  )
where

import Data.Text qualified as T
import Prettyprinter qualified as P hiding (Doc(..))
import Prettyprinter (Doc (..))
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

type Fmt = Doc ()

nil :: Fmt
nil = mempty

nest :: Int -> Fmt -> Fmt
nest = P.nest

space :: Fmt
space = P.space

line :: Fmt
line = P.line

sep :: Fmt -> [Fmt] -> Fmt
sep s = P.concatWith (\a b -> a <> s <> b)

stdNest :: Fmt -> Fmt
stdNest = nest 2

code :: T.Text -> Fmt
code = P.pretty

comment :: T.Text -> Fmt
comment = (<> line) . code

brackets :: Fmt -> Fmt
brackets = P.brackets

braces :: Fmt -> Fmt
braces = P.braces

parens :: Fmt -> Fmt
parens = P.parens

sepSpace :: Fmt -> [Fmt] -> Fmt
sepSpace s = sep (s <> space)

sepLine :: Fmt -> [Fmt] -> Fmt
sepLine s = sep (line <> s)

(<+>) :: Fmt -> Fmt -> Fmt
(<+>) = (P.<+>)

(</>) :: Fmt -> Fmt -> Fmt
a </> b = a <> line <> b

colon :: Fmt
colon = P.colon

sepNonEmpty :: Fmt -> [Fmt] -> Fmt
sepNonEmpty = sep

layoutOpts :: P.LayoutOptions
layoutOpts = P.LayoutOptions P.Unbounded

pretty :: Fmt -> T.Text
pretty = renderStrict . P.layoutPretty layoutOpts

isEmpty :: Fmt -> Bool
isEmpty = const False
