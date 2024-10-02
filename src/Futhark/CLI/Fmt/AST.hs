module Futhark.CLI.Fmt.AST (
    Fmt,
  
    -- functions for building fmt 
    nil, 
    nest, 
    stdNest,
    code,
    space,
    line,
    comment,
    group,
    sep,
    sepSpace,
    sepLine,
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

data FmtTxt = Empty
            | T.Text `Txt` FmtTxt
            | Int `NewLine` FmtTxt
            deriving (Eq, Show)  
                

instance Semigroup Fmt where
    (<>) = (:<>)

instance Monoid Fmt where
    mempty = Nil


nil :: Fmt
nil = Nil

nest :: Int -> Fmt -> Fmt
nest = Nest

stdNest :: Fmt -> Fmt
stdNest = Nest 2 

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

pretty :: Int -> Fmt -> T.Text
pretty w a = layout (best w 0 a) 
    where layout :: FmtTxt -> T.Text
          layout Empty = ""
          layout (t `Txt` res) = t <> layout res 
          layout (i `NewLine` res) = "\n" <> T.replicate i " " <> layout res

isEmpty :: Fmt -> Bool
isEmpty (Code "") = True
isEmpty Nil = True
isEmpty (x :<|> _) = isEmpty x
isEmpty (x :<> y) = isEmpty x && isEmpty y
isEmpty _ = False

{-
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
-}

best :: Int -> Int -> Fmt -> FmtTxt
best w k x = be w k [(0,x)]

be :: Int -> Int -> [(Int, Fmt)] -> FmtTxt
be _w _k [] = Empty
be w k ((_i,Nil):z) = be w k z
be w k ((i,x :<> y):z) = be w k ((i,x):(i,y):z)
be w k ((i,Nest j x):z) = be w k ((i+j,x):z)
be w k ((_i,Code s):z) = s `Txt` be w (k + T.length s) z
be w k ((_i,Space):z) = " " `Txt` be w (k + 1) z
be w _k ((i,Line Nothing):z) = i `NewLine` be w i z
be w _k ((i,Line (Just a)):z) = a `Txt` (i `NewLine` be w i z)
be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z))
                                     (be w k ((i,y):z))

better :: Int -> Int -> FmtTxt -> FmtTxt -> FmtTxt
better w k x y = if fits (w-k) x then x else y

-- | Anything fits for now since we do not care.
fits :: Int -> FmtTxt -> Bool
fits _w _e = True

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

sepSpace :: Fmt -> [Fmt] -> Fmt
sepSpace s = sep (s <> space)

sepLine :: Fmt -> [Fmt] -> Fmt
sepLine s = sep (line <> s)

(<+>) :: Fmt -> Fmt -> Fmt
x <+> y = x <> space <> y

(</>) :: Fmt -> Fmt -> Fmt
x </> y = x <> line <> y

colon :: Fmt
colon = code ":"
