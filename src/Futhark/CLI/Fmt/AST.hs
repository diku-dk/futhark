module Futhark.CLI.Fmt.AST (

    -- functions for building fmt 
    nil, 
    nest, 
    code,
    line,
    group,
    ) where 

import Data.Text qualified as T
import Data.Text.IO qualified as T

infixr 6 :<> 
infixr 5 :<|>

data Fmt = Nil
         | Fmt :<> Fmt
         | Nest Int Fmt
         | Code T.Text 
         | Line
         | Fmt :<|> Fmt
         | Comment T.Text deriving (Eq, Ord, Show)

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
line = Line

group :: Fmt -> Fmt 
group x = flatten x :<|> x 

flatten :: Fmt -> Fmt
flatten Nil = Nil
flatten (x :<> y) = flatten x :<> flatten y
flatten (Nest i x) = Nest i (flatten x)
flatten (Code x) = Code x
flatten Line = Code " "
flatten (x :<|> y) = flatten x
flatten (Comment x) = Comment x
