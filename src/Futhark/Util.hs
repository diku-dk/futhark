-- | Non-Futhark-specific utilities.  If you find yourself writing
-- general functions on generic data structures, consider putting them
-- here.
--
-- Sometimes it is also preferable to copy a small function rather
-- than introducing a large dependency.  In this case, make sure to
-- note where you got it from (and make sure that the license is
-- compatible).
module Futhark.Util
       (mapAccumLM,
        chunk,
        mapEither,
        maybeNth,
        zEncodeString
       )
       where

import Numeric
import Data.Char
import Data.List
import Data.Either

-- | Like 'mapAccumL', but monadic.
mapAccumLM :: Monad m =>
              (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumLM _ acc [] = return (acc, [])
mapAccumLM f acc (x:xs) = do
  (acc', x') <- f acc x
  (acc'', xs') <- mapAccumLM f acc' xs
  return (acc'', x':xs')

-- | @chunk n a@ splits @a@ into @n@-size-chunks.  If the length of
-- @a@ is not divisible by @n@, the last chunk will have fewer than
-- @n@ elements (but it will never be empty).
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  let (bef,aft) = splitAt n xs
  in bef : chunk n aft

-- | A combination of 'map' and 'partitionEithers'.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f l = partitionEithers $ map f l

-- | Return the list element at the given index, if the index is valid.
maybeNth :: Integral int => int -> [a] -> Maybe a
maybeNth i l
  | i >= 0, v:_ <- genericDrop i l = Just v
  | otherwise                      = Nothing

-- Z-encoding from https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames
--
-- Slightly simplified as we do not need it to deal with tuples and
-- the like.
--
-- (c) The University of Glasgow, 1997-2006


type UserString = String        -- As the user typed it
type EncodedString = String     -- Encoded form

zEncodeString :: UserString -> EncodedString
zEncodeString "" = ""
zEncodeString (c:cs) = encodeDigitChar c ++ concatMap encodeChar cs

unencodedChar :: Char -> Bool   -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar '_' = True
unencodedChar c   =  isAsciiLower c
                  || isAsciiUpper c
                  || isDigit c

-- If a digit is at the start of a symbol then we need to encode it.
-- Otherwise names like 9pH-0.1 give linker errors.
encodeDigitChar :: Char -> EncodedString
encodeDigitChar c | isDigit c = encodeAsUnicodeCharar c
                  | otherwise = encodeChar c

encodeChar :: Char -> EncodedString
encodeChar c | unencodedChar c = [c]     -- Common case first

-- Constructors
encodeChar '('  = "ZL"   -- Needed for things like (,), and (->)
encodeChar ')'  = "ZR"   -- For symmetry with (
encodeChar '['  = "ZM"
encodeChar ']'  = "ZN"
encodeChar ':'  = "ZC"
encodeChar 'Z'  = "ZZ"

-- Variables
encodeChar 'z'  = "zz"
encodeChar '&'  = "za"
encodeChar '|'  = "zb"
encodeChar '^'  = "zc"
encodeChar '$'  = "zd"
encodeChar '='  = "ze"
encodeChar '>'  = "zg"
encodeChar '#'  = "zh"
encodeChar '.'  = "zi"
encodeChar '<'  = "zl"
encodeChar '-'  = "zm"
encodeChar '!'  = "zn"
encodeChar '+'  = "zp"
encodeChar '\'' = "zq"
encodeChar '\\' = "zr"
encodeChar '/'  = "zs"
encodeChar '*'  = "zt"
encodeChar '_'  = "zu"
encodeChar '%'  = "zv"
encodeChar c    = encodeAsUnicodeCharar c

encodeAsUnicodeCharar :: Char -> EncodedString
encodeAsUnicodeCharar c = 'z' : if isDigit (head hex_str) then hex_str
                                                           else '0':hex_str
  where hex_str = showHex (ord c) "U"
