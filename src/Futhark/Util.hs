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
        takeLast,
        dropLast,
        mapEither,
        maybeNth,
        maybeHead,
        splitFromEnd,
        splitAt3,
        splitAt4,
        focusNth,
        unixEnvironment,
        isEnvVarSet,
        runProgramWithExitCode,
        directoryContents,
        roundFloat, ceilFloat, floorFloat,
        roundDouble, ceilDouble, floorDouble,
        lgamma, lgammaf, tgamma, tgammaf,
        fromPOSIX,
        toPOSIX,
        trim,
        pmapIO,
        UserString,
        EncodedString,
        zEncodeString
       )
       where

import Numeric
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Char
import Data.List
import Data.Either
import Data.Maybe
import System.Environment
import System.IO.Unsafe
import qualified System.Directory.Tree as Dir
import System.Process.ByteString
import System.Exit
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath as Native

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

-- | @takeLast n l@ takes the last @n@ elements of @l@.
takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

-- | @dropLast n l@ drops the last @n@ elements of @l@.
dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

-- | A combination of 'map' and 'partitionEithers'.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f l = partitionEithers $ map f l

-- | Return the list element at the given index, if the index is valid.
maybeNth :: Integral int => int -> [a] -> Maybe a
maybeNth i l
  | i >= 0, v:_ <- genericDrop i l = Just v
  | otherwise                      = Nothing

-- | Return the first element of the list, if it exists.
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

-- | Like 'splitAt', but from the end.
splitFromEnd :: Int -> [a] -> ([a], [a])
splitFromEnd i l = splitAt (length l - i) l

-- | Like 'splitAt', but produces three lists.
splitAt3 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 n m l =
  let (xs, l') = splitAt n l
      (ys, zs) = splitAt m l'
  in (xs, ys, zs)

-- | Like 'splitAt', but produces four lists.
splitAt4 :: Int -> Int -> Int -> [a] -> ([a], [a], [a], [a])
splitAt4 n m k l =
  let (xs, l') = splitAt n l
      (ys, l'') = splitAt m l'
      (zs, vs) = splitAt k l''
  in (xs, ys, zs, vs)

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

-- Is an environment variable set to 0 or 1?  If 0, return False; if 1, True;
-- otherwise the default value.
isEnvVarSet :: String -> Bool -> Bool
isEnvVarSet name default_val = fromMaybe default_val $ do
  val <- lookup name unixEnvironment
  case val of
    "0" -> return False
    "1" -> return True
    _ -> Nothing

-- | Like 'readProcessWithExitCode', but also wraps exceptions when
-- the indicated binary cannot be launched, or some other exception is
-- thrown.  Also does shenanigans to handle improperly encoded outputs.
runProgramWithExitCode :: FilePath -> [String] -> BS.ByteString
                       -> IO (Either IOException (ExitCode, String, String))
runProgramWithExitCode exe args inp =
  (Right . postprocess <$> readProcessWithExitCode exe args inp)
  `catch` \e -> return (Left e)
  where decode = T.unpack . T.decodeUtf8With T.lenientDecode
        postprocess (err, stdout, stderr) =
          (err, decode stdout, decode stderr)

-- | Every non-directory file contained in a directory tree.
directoryContents :: FilePath -> IO [FilePath]
directoryContents dir = do
  _ Dir.:/ tree <- Dir.readDirectoryWith return dir
  case Dir.failures tree of
    Dir.Failed _ err : _ -> throw err
    _ -> return $ mapMaybe isFile $ Dir.flattenDir tree
  where isFile (Dir.File _ path) = Just path
        isFile _                 = Nothing

foreign import ccall "nearbyint" c_nearbyint :: Double -> Double
foreign import ccall "nearbyintf" c_nearbyintf :: Float -> Float
foreign import ccall "ceil" c_ceil :: Double -> Double
foreign import ccall "ceilf" c_ceilf :: Float -> Float
foreign import ccall "floor" c_floor :: Double -> Double
foreign import ccall "floorf" c_floorf :: Float -> Float

-- | Round a single-precision floating point number correctly.
roundFloat :: Float -> Float
roundFloat = c_nearbyintf

-- | Round a single-precision floating point number upwards correctly.
ceilFloat :: Float -> Float
ceilFloat = c_ceilf

-- | Round a single-precision floating point number downwards correctly.
floorFloat :: Float -> Float
floorFloat = c_floorf

-- | Round a double-precision floating point number correctly.
roundDouble :: Double -> Double
roundDouble = c_nearbyint

-- | Round a double-precision floating point number upwards correctly.
ceilDouble :: Double -> Double
ceilDouble = c_ceil

-- | Round a double-precision floating point number downwards correctly.
floorDouble :: Double -> Double
floorDouble = c_floor

foreign import ccall "lgamma" c_lgamma :: Double -> Double
foreign import ccall "lgammaf" c_lgammaf :: Float -> Float
foreign import ccall "tgamma" c_tgamma :: Double -> Double
foreign import ccall "tgammaf" c_tgammaf :: Float -> Float

-- | The system-level @lgamma()@ function.
lgamma :: Double -> Double
lgamma = c_lgamma

-- | The system-level @lgammaf()@ function.
lgammaf :: Float -> Float
lgammaf = c_lgammaf

-- | The system-level @tgamma()@ function.
tgamma :: Double -> Double
tgamma = c_tgamma

-- | The system-level @tgammaf()@ function.
tgammaf :: Float -> Float
tgammaf = c_tgammaf

-- | Turn a POSIX filepath into a filepath for the native system.
toPOSIX :: Native.FilePath -> Posix.FilePath
toPOSIX = Posix.joinPath . Native.splitDirectories

-- | Some bad operating systems do not use forward slash as
-- directory separator - this is where we convert Futhark includes
-- (which always use forward slash) to native paths.
fromPOSIX :: Posix.FilePath -> Native.FilePath
fromPOSIX = Native.joinPath . Posix.splitDirectories

-- | Remove leading and trailing whitespace from a string.  Not an
-- efficient implementation!
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

fork :: (a -> IO b) -> a -> IO (MVar b)
fork f x = do cell <- newEmptyMVar
              void $ forkIO $ do result <- f x
                                 putMVar cell result
              return cell

pmapIO :: (a -> IO b) -> [a] -> IO [b]
pmapIO f elems = go elems []
  where
    go [] res = return res
    go xs res = do
      numThreads <- getNumCapabilities
      let (e,es) = splitAt numThreads xs
      mvars  <- mapM (fork f) e
      result <- mapM takeMVar mvars
      go es (result ++ res)

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
