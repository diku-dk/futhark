module Futhark.CLI.Fmt.AST (

    -- functions for building fmt 
    nil, 
    nest, 
    code,
    space,
    line,
    comment,
    group,

    -- functions for manipulating fmt
    flatten,
    pretty

    ) where 

import Data.Text qualified as T
import Data.Text.IO qualified as T
import qualified Data.Text.Lazy.Lens as T

infixr 6 :<> 
infixr 5 :<|>

type Comment = T.Text
type Line = T.Text

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