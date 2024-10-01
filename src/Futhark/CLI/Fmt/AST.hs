module Futhark.CLI.Fmt.AST (
    Fmt,
  
    -- functions for building fmt 
    nil, 
    nest, 
    code,
    space,
    line,
    comment,
    group,
    sep,
    brackets,
    braces,
    parens,
    (<+>),
    (</>),
    colon,

    -- functions for manipulating fmt
    flatten,
    pretty

    ) where 

import Data.Text qualified as T

infixr 6 :<>
infixr 6 <+> 
infixr 6 </> 
infixr 5 :<|>

-- | Invariant: Should not contain newline characters.
type Comment = T.Text
type Line = T.Text

-- | The result of formatting is a list of lines.  This is useful as
-- we might want to modify every line of a prettyprinted
-- sub-expression, to e.g. prepend indentation.
-- Note: this is not applicable but I will keep this here for now.
data Fmt = Nil
         | Fmt :<> Fmt
         | Nest Int Fmt
         | Code T.Text 
         | Line (Maybe Comment) 
         | Space
         | Fmt :<|> Fmt
         deriving (Eq, Show)

instance Semigroup Fmt where
    (<>) = (:<>)

instance Monoid Fmt where
    mempty = Nil


nil :: Fmt
nil = Nil

nest :: Int -> Fmt -> Fmt
nest = Nest

code :: T.Text -> Fmt
code = Code 

line :: Fmt 
line = Line Nothing

space :: Fmt
space = Space

comment :: T.Text -> Fmt
comment = Line . Just

group :: Fmt -> Fmt 
group x = flatten x :<|> x 

flatten :: Fmt -> Fmt
flatten fmt = 
    let (cs, a') = flatten' fmt
        cs' = mconcat $ map (Line . Just) cs
    in a' :<> cs' -- should comment appear before or after flat-line?
    where flatten' :: Fmt -> ([T.Text], Fmt)
          flatten' Nil = ([], nil)
          flatten' (x :<> y) = 
            let (a, x') = flatten' x
                (b, y') = flatten' y
            in (a ++ b, x' <> y')
          flatten' (Nest _i x) = flatten' x
          flatten' (Code x) = ([], code x)
          flatten' (Line Nothing) = ([], space)
          flatten' (Line (Just c)) = ([c], space)
          flatten' Space = ([], space)
          flatten' (x :<|> _) = flatten' x

pretty :: Fmt -> Line
pretty = layout 
    where layout :: Fmt -> T.Text
          layout Nil = ""
          layout (x :<> y) = layout x <> layout y    
          layout (Nest i x) = indnt <> layout x
            where indnt = T.pack $ replicate i ' '
          layout (Code s) = s
          layout (Line Nothing) = "\n"
          layout (Line (Just c)) = c <> "\n"  
          layout Space = " "
          -- always choose the first format for now
          -- TO DO: Have a better way to choose layout
          layout (x :<|> _) = layout x


brackets :: Fmt -> Fmt
brackets fmt = code "[" <> fmt <> code "]"

braces :: Fmt -> Fmt
braces fmt = code "{" <> fmt <> code "}"

parens :: Fmt -> Fmt
parens fmt = code "(" <> fmt <> code ")"

sep :: Fmt -> [Fmt] -> Fmt
sep _s [] = nil
sep _s [x] = x
sep s (x:xs) = x <> s <> sep s xs 

(<+>) :: Fmt -> Fmt -> Fmt
Nil <+> y = y
x <+> Nil = x
x <+> y = x <> space <> y

(</>) :: Fmt -> Fmt -> Fmt
Nil </> y = y
x </> Nil = x
x </> y = x <> line <> y

colon :: Fmt
colon = code ":"
