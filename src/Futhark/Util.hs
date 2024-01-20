-- | Non-Futhark-specific utilities.  If you find yourself writing
-- general functions on generic data structures, consider putting them
-- here.
--
-- Sometimes it is also preferable to copy a small function rather
-- than introducing a large dependency.  In this case, make sure to
-- note where you got it from (and make sure that the license is
-- compatible).
module Futhark.Util
  ( nubOrd,
    nubByOrd,
    mapAccumLM,
    maxinum,
    chunk,
    chunks,
    chunkLike,
    dropAt,
    takeLast,
    dropLast,
    mapEither,
    partitionMaybe,
    maybeNth,
    maybeHead,
    lookupWithIndex,
    splitFromEnd,
    splitAt3,
    focusNth,
    focusMaybe,
    hashText,
    showText,
    unixEnvironment,
    isEnvVarAtLeast,
    startupTime,
    fancyTerminal,
    hFancyTerminal,
    runProgramWithExitCode,
    directoryContents,
    fromPOSIX,
    toPOSIX,
    trim,
    pmapIO,
    interactWithFileSafely,
    convFloat,
    UserText,
    EncodedText,
    zEncodeText,
    atMostChars,
    invertMap,
    cartesian,
    traverseFold,
    fixPoint,
    concatMapM,
    topologicalSort,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.State
import Crypto.Hash.MD5 as MD5
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Char
import Data.Either
import Data.Foldable (fold, toList)
import Data.Function ((&))
import Data.IntMap qualified as IM
import Data.List (findIndex, foldl', genericDrop, genericSplitAt, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Tuple (swap)
import Numeric
import System.Directory.Tree qualified as Dir
import System.Environment
import System.Exit
import System.FilePath qualified as Native
import System.FilePath.Posix qualified as Posix
import System.IO (Handle, hIsTerminalDevice, stdout)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe
import System.Process.ByteString
import Text.Read (readMaybe)

-- | Like @nub@, but without the quadratic runtime.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = nubByOrd compare

-- | Like @nubBy@, but without the quadratic runtime.
nubByOrd :: (a -> a -> Ordering) -> [a] -> [a]
nubByOrd cmp = map NE.head . NE.groupBy eq . sortBy cmp
  where
    eq x y = cmp x y == EQ

-- | Like 'Data.Traversable.mapAccumL', but monadic and generalised to
-- any 'Traversable'.
mapAccumLM ::
  (Monad m, Traversable t) =>
  (acc -> x -> m (acc, y)) ->
  acc ->
  t x ->
  m (acc, t y)
mapAccumLM op initial l = do
  (l', acc) <- runStateT (traverse f l) initial
  pure (acc, l')
  where
    f x = do
      acc <- get
      (acc', y) <- lift $ op acc x
      put acc'
      pure y

-- | @chunk n a@ splits @a@ into @n@-size-chunks.  If the length of
-- @a@ is not divisible by @n@, the last chunk will have fewer than
-- @n@ elements (but it will never be empty).
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  let (bef, aft) = splitAt n xs
   in bef : chunk n aft

-- | @chunks ns a@ splits @a@ into chunks determined by the elements
-- of @ns@.  It must hold that @sum ns == length a@, or the resulting
-- list may contain too few chunks, or not all elements of @a@.
chunks :: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks (n : ns) xs =
  let (bef, aft) = splitAt n xs
   in bef : chunks ns aft

-- | @chunkLike xss ys@ chunks the elements of @ys@ to match the
-- elements of @xss@.  The sum of the lengths of the sublists of @xss@
-- must match the length of @ys@.
chunkLike :: [[a]] -> [b] -> [[b]]
chunkLike as = chunks (map length as)

-- | Like 'maximum', but returns zero for an empty list.
maxinum :: (Num a, Ord a, Foldable f) => f a -> a
maxinum = foldl' max 0

-- | @dropAt i n@ drops @n@ elements starting at element @i@.
dropAt :: Int -> Int -> [a] -> [a]
dropAt i n xs = take i xs ++ drop (i + n) xs

-- | @takeLast n l@ takes the last @n@ elements of @l@.
takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

-- | @dropLast n l@ drops the last @n@ elements of @l@.
dropLast :: Int -> [a] -> [a]
dropLast n = reverse . drop n . reverse

-- | A combination of 'map' and 'partitionEithers'.
mapEither :: (a -> Either b c) -> [a] -> ([b], [c])
mapEither f l = partitionEithers $ map f l

-- | A combination of 'Data.List.partition' and 'mapMaybe'
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f = helper ([], [])
  where
    helper (acc1, acc2) [] = (reverse acc1, reverse acc2)
    helper (acc1, acc2) (x : xs) =
      case f x of
        Just x' -> helper (x' : acc1, acc2) xs
        Nothing -> helper (acc1, x : acc2) xs

-- | Return the list element at the given index, if the index is valid.
maybeNth :: (Integral int) => int -> [a] -> Maybe a
maybeNth i l
  | i >= 0, v : _ <- genericDrop i l = Just v
  | otherwise = Nothing

-- | Return the first element of the list, if it exists.
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

-- | Lookup a value, returning also the index at which it appears.
lookupWithIndex :: (Eq a) => a -> [(a, b)] -> Maybe (Int, b)
lookupWithIndex needle haystack =
  lookup needle $ zip (map fst haystack) (zip [0 ..] (map snd haystack))

-- | Like 'splitAt', but from the end.
splitFromEnd :: Int -> [a] -> ([a], [a])
splitFromEnd i l = splitAt (length l - i) l

-- | Like 'splitAt', but produces three lists.
splitAt3 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 n m l =
  let (xs, l') = splitAt n l
      (ys, zs) = splitAt m l'
   in (xs, ys, zs)

-- | Return the list element at the given index, if the index is
-- valid, along with the elements before and after.
focusNth :: (Integral int) => int -> [a] -> Maybe ([a], a, [a])
focusNth i xs
  | (bef, x : aft) <- genericSplitAt i xs = Just (bef, x, aft)
  | otherwise = Nothing

-- | Return the first list element that satisifes a predicate, along with the
-- elements before and after.
focusMaybe :: (a -> Maybe b) -> [a] -> Maybe ([a], b, [a])
focusMaybe f xs = do
  idx <- findIndex (isJust . f) xs
  (before, focus, after) <- focusNth idx xs
  res <- f focus
  pure (before, res, after)

-- | Compute a hash of a text that is stable across OS versions.
-- Returns the hash as a text as well, ready for human consumption.
hashText :: T.Text -> T.Text
hashText =
  T.decodeUtf8With T.lenientDecode . Base16.encode . MD5.hash . T.encodeUtf8

-- | Like 'show', but produces text.
showText :: (Show a) => a -> T.Text
showText = T.pack . show

{-# NOINLINE unixEnvironment #-}

-- | The Unix environment when the Futhark compiler started.
unixEnvironment :: [(String, String)]
unixEnvironment = unsafePerformIO getEnvironment

-- | True if the environment variable, viewed as an integer, has at
-- least this numeric value.  Returns False if variable is unset or
-- not numeric.
isEnvVarAtLeast :: String -> Int -> Bool
isEnvVarAtLeast s x =
  case readMaybe =<< lookup s unixEnvironment of
    Just y -> y >= x
    _ -> False

{-# NOINLINE startupTime #-}

-- | The time at which the process started - or more accurately, the
-- first time this binding was forced.
startupTime :: UTCTime
startupTime = unsafePerformIO getCurrentTime

{-# NOINLINE fancyTerminal #-}

-- | Are we running in a terminal capable of fancy commands and
-- visualisation?
fancyTerminal :: Bool
fancyTerminal = unsafePerformIO $ hFancyTerminal stdout

-- | Is this handle connected to a terminal capable of fancy commands
-- and visualisation?
hFancyTerminal :: Handle -> IO Bool
hFancyTerminal h = do
  isTTY <- hIsTerminalDevice h
  isDumb <- (Just "dumb" ==) <$> lookupEnv "TERM"
  pure $ isTTY && not isDumb

-- | Like 'readProcessWithExitCode', but also wraps exceptions when
-- the indicated binary cannot be launched, or some other exception is
-- thrown.  Also does shenanigans to handle improperly encoded outputs.
runProgramWithExitCode ::
  FilePath ->
  [String] ->
  BS.ByteString ->
  IO (Either IOException (ExitCode, String, String))
runProgramWithExitCode exe args inp =
  (Right . postprocess <$> readProcessWithExitCode exe args inp)
    `catch` \e -> pure (Left e)
  where
    decode = T.unpack . T.decodeUtf8With T.lenientDecode
    postprocess (code, out, err) =
      (code, decode out, decode err)

-- | Every non-directory file contained in a directory tree.
directoryContents :: FilePath -> IO [FilePath]
directoryContents dir = do
  _ Dir.:/ tree <- Dir.readDirectoryWith pure dir
  case Dir.failures tree of
    Dir.Failed _ err : _ -> throw err
    _ -> pure $ mapMaybe isFile $ Dir.flattenDir tree
  where
    isFile (Dir.File _ path) = Just path
    isFile _ = Nothing

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

-- | Run various 'IO' actions concurrently, possibly with a bound on
-- the number of threads.  The list must be finite.  The ordering of
-- the result list is not deterministic - add your own sorting if
-- needed.  If any of the actions throw an exception, then that
-- exception is propagated to this function.
pmapIO :: Maybe Int -> (a -> IO b) -> [a] -> IO [b]
pmapIO concurrency f elems = do
  tasks <- newMVar elems
  results <- newEmptyMVar
  num_threads <- maybe getNumCapabilities pure concurrency
  replicateM_ num_threads $ forkIO $ worker tasks results
  replicateM (length elems) $ getResult results
  where
    worker tasks results = do
      task <- modifyMVar tasks getTask
      case task of
        Nothing -> pure ()
        Just x -> do
          y <- (Right <$> f x) `catch` (pure . Left)
          putMVar results y
          worker tasks results

    getTask [] = pure ([], Nothing)
    getTask (task : tasks) = pure (tasks, Just task)

    getResult results = do
      res <- takeMVar results
      case res of
        Left err -> throw (err :: SomeException)
        Right v -> pure v

-- | Do some operation on a file, returning 'Nothing' if the file does
-- not exist, and 'Left' if some other error occurs.
interactWithFileSafely :: IO a -> IO (Maybe (Either String a))
interactWithFileSafely m =
  (Just . Right <$> m) `catch` couldNotRead
  where
    couldNotRead e
      | isDoesNotExistError e =
          pure Nothing
      | otherwise =
          pure $ Just $ Left $ show e

-- | Convert between different floating-point types, preserving
-- infinities and NaNs.
convFloat :: (RealFloat from, RealFloat to) => from -> to
convFloat v
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v

-- Z-encoding from https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/SymbolNames
--
-- Slightly simplified as we do not need it to deal with tuples and
-- the like.
--
-- (c) The University of Glasgow, 1997-2006

-- | As the user typed it.
type UserString = String

-- | Encoded form.
type EncodedString = String

-- | As 'zEncodeText', but for strings.
zEncodeString :: UserString -> EncodedString
zEncodeString "" = ""
zEncodeString (c : cs) = encodeDigitChar c ++ concatMap encodeChar cs

-- | As the user typed it.
type UserText = T.Text

-- | Encoded form.
type EncodedText = T.Text

-- | Z-encode a text using a slightly simplified variant of GHC
-- Z-encoding.  The encoded string is a valid identifier in most
-- programming languages.
zEncodeText :: UserText -> EncodedText
zEncodeText = T.pack . zEncodeString . T.unpack

unencodedChar :: Char -> Bool -- True for chars that don't need encoding
unencodedChar 'Z' = False
unencodedChar 'z' = False
unencodedChar '_' = True
unencodedChar c =
  isAsciiLower c
    || isAsciiUpper c
    || isDigit c

-- If a digit is at the start of a symbol then we need to encode it.
-- Otherwise names like 9pH-0.1 give linker errors.
encodeDigitChar :: Char -> EncodedString
encodeDigitChar c
  | isDigit c = encodeAsUnicodeCharar c
  | otherwise = encodeChar c

encodeChar :: Char -> EncodedString
encodeChar c | unencodedChar c = [c] -- Common case first

-- Constructors
encodeChar '(' = "ZL" -- Needed for things like (,), and (->)
encodeChar ')' = "ZR" -- For symmetry with (
encodeChar '[' = "ZM"
encodeChar ']' = "ZN"
encodeChar ':' = "ZC"
encodeChar 'Z' = "ZZ"
-- Variables
encodeChar 'z' = "zz"
encodeChar '&' = "za"
encodeChar '|' = "zb"
encodeChar '^' = "zc"
encodeChar '$' = "zd"
encodeChar '=' = "ze"
encodeChar '>' = "zg"
encodeChar '#' = "zh"
encodeChar '.' = "zi"
encodeChar '<' = "zl"
encodeChar '-' = "zm"
encodeChar '!' = "zn"
encodeChar '+' = "zp"
encodeChar '\'' = "zq"
encodeChar '\\' = "zr"
encodeChar '/' = "zs"
encodeChar '*' = "zt"
encodeChar '_' = "zu"
encodeChar '%' = "zv"
encodeChar c = encodeAsUnicodeCharar c

encodeAsUnicodeCharar :: Char -> EncodedString
encodeAsUnicodeCharar c =
  'z'
    : if maybe False isDigit $ maybeHead hex_str
      then hex_str
      else '0' : hex_str
  where
    hex_str = showHex (ord c) "U"

-- | Truncate to at most this many characters, making the last three
-- characters "..." if truncation is necessary.
atMostChars :: Int -> T.Text -> T.Text
atMostChars n s
  | T.length s > n = T.take (n - 3) s <> "..."
  | otherwise = s

-- | Invert a map, handling duplicate values (now keys) by
-- constructing a set of corresponding values.
invertMap :: (Ord v, Ord k) => M.Map k v -> M.Map v (S.Set k)
invertMap m =
  foldr
    (uncurry (M.insertWith (<>)) . swap . first S.singleton)
    mempty
    (M.toList m)

-- | Compute the cartesian product of two foldable collections, using the given
-- combinator function.
cartesian :: (Monoid m, Foldable t) => (a -> a -> m) -> t a -> t a -> m
cartesian f xs ys =
  [(x, y) | x <- toList xs, y <- toList ys]
    & foldMap (uncurry f)

-- | Applicatively fold a traversable.
traverseFold :: (Monoid m, Traversable t, Applicative f) => (a -> f m) -> t a -> f m
traverseFold f = fmap fold . traverse f

-- | Perform fixpoint iteration.
fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x =
  let x' = f x
   in if x' == x then x else fixPoint f x'

-- | Like 'concatMap', but monoidal and monadic.
concatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f xs = mconcat <$> mapM f xs

-- | Topological sorting of an array with an adjancency function, if
-- there is a cycle, it causes an error. @dep a b@ means @a -> b@,
-- and the returned array guarantee that for i < j:
--
-- @not ( dep (ret !! j) (ret !! i) )@.
topologicalSort :: (a -> a -> Bool) -> [a] -> [a]
topologicalSort dep nodes =
  fst $ execState (mapM_ (sorting . snd) nodes_idx) (mempty, mempty)
  where
    nodes_idx = zip nodes [0 ..]
    depends_of a (b, i) =
      if a `dep` b
        then Just i
        else Nothing

    -- Using an IntMap Bool
    -- when reading a lookup:
    -- \* Nothing : never explored
    -- \* Just True : being explored
    -- \* Just False : explored
    sorting i = do
      status <- gets $ IM.lookup i . snd
      when (status == Just True) $ error "topological sorting has encountered a cycle"
      unless (status == Just False) $ do
        let node = nodes !! i
        modify $ second $ IM.insert i True
        mapM_ sorting $ mapMaybe (depends_of node) nodes_idx
        modify $ bimap (node :) (IM.insert i False)
