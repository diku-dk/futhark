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
        chunks,
        dropAt,
        mapEither,
        maybeNth,
        splitAt3,
        focusNth,
        unixEnvironment,
        directoryContents,
        zEncodeString,
        usesInPlaceLowering,
        usesMemoryBlockMergingCoalescing,
        usesMemoryBlockMergingReuse
       )
       where

import Numeric
import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import System.Environment
import System.IO.Unsafe
import System.Directory.Tree (readDirectoryWith, flattenDir,
                              DirTree(File), AnchoredDirTree(..))

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

-- | @chunks ns a@ splits @a@ into chunks determined by the elements
-- of @ns@.  It must hold that @sum ns == length a@, or the resulting
-- list may contain too few chunks, or not all elements of @a@.
chunks :: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks (n:ns) xs =
  let (bef,aft) = splitAt n xs
  in bef : chunks ns aft

-- | @dropAt i n@ drops @n@ elements starting at element @i@.
dropAt :: Int -> Int -> [a] -> [a]
dropAt i n xs = take i xs ++ drop (i+n) xs

-- | A combination of 'map' and 'partitionEithers'.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f l = partitionEithers $ map f l

-- | Return the list element at the given index, if the index is valid.
maybeNth :: Integral int => int -> [a] -> Maybe a
maybeNth i l
  | i >= 0, v:_ <- genericDrop i l = Just v
  | otherwise                      = Nothing

-- | Like 'splitAt', but produces three lists.
splitAt3 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 n m l =
  let (xs, l') = splitAt n l
      (ys, zs) = splitAt m l'
  in (xs, ys, zs)

-- | Return the list element at the given index, if the index is
-- valid, along with the elements before and after.
focusNth :: Integral int => int -> [a] -> Maybe ([a], a, [a])
focusNth i xs
  | (bef, x:aft) <- genericSplitAt i xs = Just (bef, x, aft)
  | otherwise                           = Nothing

{-# NOINLINE unixEnvironment #-}
-- | The Unix environment when the Futhark compiler started.
unixEnvironment :: [(String,String)]
unixEnvironment = unsafePerformIO getEnvironment

-- | Every non-directory file contained in a directory tree.
directoryContents :: FilePath -> IO [FilePath]
directoryContents dir = do
  _ :/ tree <- readDirectoryWith return dir
  return $ mapMaybe isFile $ flattenDir tree
  where isFile (File _ path) = Just path
        isFile _             = Nothing

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

-- Is an environment variable set to 0 or 1?  If 0, return False; if 1, True;
-- otherwise the default value.
isEnvVarSet :: String -> Bool -> Bool
isEnvVarSet name default_val = fromMaybe default_val $ do
  val <- lookup name unixEnvironment
  case val of
    "0" -> return False
    "1" -> return True
    _ -> Nothing

-- Do we use in-place lowering?  Currently enabled by default.  Disable by
-- setting the environment variable IN_PLACE_LOWERING=0.
usesInPlaceLowering :: Bool
usesInPlaceLowering =
  isEnvVarSet "IN_PLACE_LOWERING" True

-- Do we use the coalescing part of memory block merging?  Currently disabled by
-- default.  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_COALESCING=1.
usesMemoryBlockMergingCoalescing :: Bool
usesMemoryBlockMergingCoalescing =
  isEnvVarSet "MEMORY_BLOCK_MERGING_COALESCING" False

-- Do we use the reuse part of memory block merging?  Currently disabled by
-- default.  Enable by setting the environment variable
-- MEMORY_BLOCK_MERGING_REUSE=1.
usesMemoryBlockMergingReuse :: Bool
usesMemoryBlockMergingReuse =
  isEnvVarSet "MEMORY_BLOCK_MERGING_REUSE" False
