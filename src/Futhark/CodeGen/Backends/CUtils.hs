{-# LANGUAGE QuasiQuotes #-}
-- | Simple C utility combinators.  You should import this qualified, as it shadows some standard names.
module Futhark.CodeGen.Backends.CUtils
  ( product
  , sum
  , var
  )
  where

import Prelude hiding (product, sum)

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

-- | Return an expression multiplying together the given expressions.
-- If an empty list is given, the expression @1@ is returned.
product :: [C.Exp] -> C.Exp
product []     = [C.cexp|1|]
product (e:es) = foldl mult e es
  where mult x y = [C.cexp|$exp:x * $exp:y|]

-- | Return an expression summing together the given expressions.
-- If an empty list is given, the expression @0@ is returned.
sum :: [C.Exp] -> C.Exp
sum []     = [C.cexp|0|]
sum (e:es) = foldl add e es
  where add x y = [C.cexp|$exp:x + $exp:y|]

-- | Turn a name into a C expression consisting of just that name.
var :: String -> C.Exp
var k = [C.cexp|$id:k|]
