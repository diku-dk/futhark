{-# LANGUAGE OverloadedStrings #-}
-- | Randomly generate Futhark input files containing values of a
-- specified type and shape.
module Main (main) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.State
import qualified Data.Binary as Bin (put)
import Data.Binary.IEEE754
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Word

import Prelude

import System.Console.GetOpt
import System.Random

import Language.Futhark.Syntax
import Language.Futhark.Attributes (UncheckedTypeExp, namesToPrimTypes)
import Language.Futhark.Parser
import Language.Futhark.Pretty ()

import Futhark.Util.Options
import Futhark.Util.Pretty

main :: IO ()
main = mainWithOptions initialDataOptions commandLineOptions f
  where f [] config =
          Just $ zipWithM_ ($) (optOrders config) $ map mkStdGen [optSeed config..]
        f _ _ =
          Nothing

data BinaryOutputFormat = AllData
                        | NoHeader
                        | OnlyHeader
                        deriving (Eq, Ord, Show)

data OutputFormat = Text
                  | Binary BinaryOutputFormat
                  deriving (Eq, Ord, Show)

data DataOptions = DataOptions
                   { optSeed :: Int
                   , optRange :: RandomConfiguration
                   , optOrders :: [StdGen -> IO ()]
                   , format :: OutputFormat
                   }

initialDataOptions :: DataOptions
initialDataOptions = DataOptions 0 initialRandomConfiguration [] Text

commandLineOptions :: [FunOptDescr DataOptions]
commandLineOptions = [
    Option "s" ["seed"]
    (ReqArg (\n ->
              case reads n of
                [(n', "")] ->
                  Right $ \config -> config { optSeed = n' }
                _ ->
                  Left $ error $ "'" ++ n ++ "' is not an integer.")
     "SEED")
    "The seed to use when initialising the RNG."
  , Option "g" ["generate"]
    (ReqArg (\t ->
              case tryMakeGenerator t of
                Right g ->
                  Right $ \config ->
                  config { optOrders =
                             optOrders config ++
                             [g (optRange config) (format config)]
                         }
                Left err ->
                  Left $ error err)
     "TYPE")
    "Generate a random value of this type."
  , Option [] ["text"]
    (NoArg $ Right $ \opts ->
        opts { format = Text })
    "Output data in text format (must precede --generate)."
  , Option "b" ["binary"]
    (NoArg $ Right $ \opts ->
        opts { format = Binary AllData })
    "Output data in binary Futhark format (must precede --generate)."
  , Option [] ["binary-no-header"]
    (NoArg $ Right $ \opts ->
        opts { format = Binary NoHeader })
    "Output data in binary Futhark format without header (must precede --generate)."
  , Option [] ["binary-only-header"]
    (NoArg $ Right $ \opts ->
        opts { format = Binary OnlyHeader })
    "Only output binary Futhark format header for data (must precede --generate)."
  , setRangeOption "i8" seti8Range
  , setRangeOption "i16" seti16Range
  , setRangeOption "i32" seti32Range
  , setRangeOption "i64" seti64Range
  , setRangeOption "u8" setu8Range
  , setRangeOption "u16" setu16Range
  , setRangeOption "u32" setu32Range
  , setRangeOption "u64" setu64Range
  , setRangeOption "f32" setf32Range
  , setRangeOption "f64" setf64Range
  ]

setRangeOption :: Read a => String
                -> (Range a -> RandomConfiguration -> RandomConfiguration)
                -> FunOptDescr DataOptions
setRangeOption tname set =
  Option "" [name]
  (ReqArg (\b ->
            let (lower,rest) = span (/=':') b
                upper = drop 1 rest
            in case (reads lower, reads upper) of
              ([(lower', "")], [(upper', "")]) ->
                Right $ \config ->
                config { optRange = set (lower', upper') $ optRange config }
              _ ->
                Left $ error $ "Invalid bounds: " ++ b
            )
   "MIN:MAX") $
  "Range of " ++ tname ++ " values."
  where name = tname ++ "-bounds"

tryMakeGenerator :: String -> Either String (RandomConfiguration -> OutputFormat -> StdGen  -> IO ())
tryMakeGenerator t = do
  t' <- toSimpleType =<< either (Left . show) Right (parseType name (T.pack t))
  return $ \conf fmt stdgen -> do
    let (v, _) = randomValue conf t' stdgen
    case fmt of
      Text -> printSimpleValueT v
      Binary binfmt -> printSimpleValueB binfmt t' v
  where name = "option " ++ t

data SimpleType = SimpleArray SimpleType Int
                | SimplePrim PrimType
                  deriving (Show)

toSimpleType :: UncheckedTypeExp -> Either String SimpleType
toSimpleType TETuple{} = Left "Cannot handle tuples yet."
toSimpleType TERecord{} = Left "Cannot handle records yet."
toSimpleType (TEUnique t _) = toSimpleType t
toSimpleType (TEArray t d _) =
  SimpleArray <$> toSimpleType t <*> constantDim d
  where constantDim (ConstDim k) = Right k
        constantDim _ = Left "Array has non-constant dimension declaration."
toSimpleType (TEVar (QualName [] v) _)
  | Just t <- HM.lookup v namesToPrimTypes = Right $ SimplePrim t
toSimpleType (TEVar v _) =
  Left $ "Unknown type " ++ pretty v

data SimpleValue = SimpleArrayValue [SimpleValue]
                 | SimplePrimValue PrimValue
                   deriving (Show)

-- Ordinary prettyprinting consumes too much memory, likely because it
-- manifests the string to print instead of doing it lazily, which is
-- a bad idea for giant values.  This is likely because it tries to do
-- a good job with respect to line wrapping and the like.  We opt to
-- do a bad job instead, but one that we can do much faster.
printSimpleValueT :: SimpleValue -> IO ()
printSimpleValueT = (>>putStrLn "") . flip evalStateT 0 . p
  where elements_per_line = 20 :: Int

        p (SimplePrimValue v) = do
          maybeNewline
          lift $ putStr $ pretty v
        p (SimpleArrayValue []) =
          lift $ putStr "[]"
        p (SimpleArrayValue (v:vs)) = do
          lift $ putStr "["
          p v
          forM_ vs $ \v' -> do
            lift $ putStr ", "
            p v'
          lift $ putStr "]"

        maybeNewline = do
          i <- get
          if i >= elements_per_line
            then do lift $ putStrLn ""
                    put 0
            else put $ i + 1

binaryFormatVersion :: Int
binaryFormatVersion = 1

printSimpleValueB :: BinaryOutputFormat -> SimpleType -> SimpleValue -> IO ()
printSimpleValueB fmt st sv =
  BL.putStr $ runPut $ printHeader () >> printData ()

  where
    printHeader _ | fmt == NoHeader = return ()
    printHeader _ = do
      Bin.put 'b'
      putWord8 $ fromIntegral binaryFormatVersion
      let dims = getDims st
      putWord8 $ fromIntegral $ length dims
      putElemType st
      case sv of
        SimplePrimValue _ -> return ()
        SimpleArrayValue _ -> mapM_ (putWord64le . fromIntegral) dims

    printData _ | fmt == OnlyHeader = return ()
    printData _ = pSimpleValue sv

    -- Simply calling @Bin.put (" i8" :: String)@ would cause a lot of bytes to
    -- be written. Doing it this way will only write 4 bytes.
    putElemType (SimplePrim (Signed Int8))  = mapM_ Bin.put ("  i8" :: String)
    putElemType (SimplePrim (Signed Int16)) = mapM_ Bin.put (" i16" :: String)
    putElemType (SimplePrim (Signed Int32)) = mapM_ Bin.put (" i32" :: String)
    putElemType (SimplePrim (Signed Int64)) = mapM_ Bin.put (" i64" :: String)
    putElemType (SimplePrim (FloatType Float32)) = mapM_ Bin.put (" f32" :: String)
    putElemType (SimplePrim (FloatType Float64)) = mapM_ Bin.put (" f64" :: String)
    putElemType (SimplePrim Bool) = mapM_ Bin.put ("bool" :: String)
    putElemType (SimplePrim (Unsigned t)) = putElemType $ SimplePrim $ Signed t
    putElemType (SimpleArray ty _) = putElemType ty

    getDims (SimplePrim _) = []
    getDims (SimpleArray ty dim) = dim : getDims ty

    pSimpleValue :: SimpleValue -> Put
    pSimpleValue (SimplePrimValue pv) = p pv
    pSimpleValue (SimpleArrayValue svs) = mapM_ pSimpleValue svs

    p :: PrimValue -> Put
    p (SignedValue (Int8Value v))    = putWord8    $ fromIntegral $ fromEnum v
    p (SignedValue (Int16Value v))   = putWord16le $ fromIntegral $ fromEnum v
    p (SignedValue (Int32Value v))   = putWord32le $ fromIntegral $ fromEnum v
    p (SignedValue (Int64Value v))   = putWord64le $ fromIntegral $ fromEnum v
    p (UnsignedValue (Int8Value v))  = putWord8    $ fromIntegral $ fromEnum v
    p (UnsignedValue (Int16Value v)) = putWord16le $ fromIntegral $ fromEnum v
    p (UnsignedValue (Int32Value v)) = putWord32le $ fromIntegral $ fromEnum v
    p (UnsignedValue (Int64Value v)) = putWord64le $ fromIntegral $ fromEnum v
    p (FloatValue (Float32Value v))  = putFloat32le v
    p (FloatValue (Float64Value v))  = putFloat64le v
    p (BoolValue v)                  = putWord8 $ if v then 1 else 0

-- | Closed interval, as in @System.Random@.
type Range a = (a, a)

data RandomConfiguration = RandomConfiguration
                           { i8Range  :: Range Int8
                           , i16Range :: Range Int16
                           , i32Range :: Range Int32
                           , i64Range :: Range Int64
                           , u8Range  :: Range Word8
                           , u16Range :: Range Word16
                           , u32Range :: Range Word32
                           , u64Range :: Range Word64
                           , f32Range :: Range Float
                           , f64Range :: Range Double
                           }

-- The following lines provide evidence about how Haskells record
-- system sucks.
seti8Range :: Range Int8 -> RandomConfiguration -> RandomConfiguration
seti8Range bounds config = config { i8Range = bounds }
seti16Range :: Range Int16 -> RandomConfiguration -> RandomConfiguration
seti16Range bounds config = config { i16Range = bounds }
seti32Range :: Range Int32 -> RandomConfiguration -> RandomConfiguration
seti32Range bounds config = config { i32Range = bounds }
seti64Range :: Range Int64 -> RandomConfiguration -> RandomConfiguration
seti64Range bounds config = config { i64Range = bounds }

setu8Range :: Range Word8 -> RandomConfiguration -> RandomConfiguration
setu8Range bounds config = config { u8Range = bounds }
setu16Range :: Range Word16 -> RandomConfiguration -> RandomConfiguration
setu16Range bounds config = config { u16Range = bounds }
setu32Range :: Range Word32 -> RandomConfiguration -> RandomConfiguration
setu32Range bounds config = config { u32Range = bounds }
setu64Range :: Range Word64 -> RandomConfiguration -> RandomConfiguration
setu64Range bounds config = config { u64Range = bounds }

setf32Range :: Range Float -> RandomConfiguration -> RandomConfiguration
setf32Range bounds config = config { f32Range = bounds }
setf64Range :: Range Double -> RandomConfiguration -> RandomConfiguration
setf64Range bounds config = config { f64Range = bounds }

initialRandomConfiguration :: RandomConfiguration
initialRandomConfiguration = RandomConfiguration
  (minBound, maxBound) (minBound, maxBound) (minBound, maxBound) (minBound, maxBound)
  (minBound, maxBound) (minBound, maxBound) (minBound, maxBound) (minBound, maxBound)
  (0.0, 1.0) (0.0, 1.0)

randomValue :: RandomConfiguration -> SimpleType -> StdGen -> (SimpleValue, StdGen)
randomValue conf (SimplePrim (Signed Int8)) stdgen =
  randomC conf i8Range stdgen
randomValue conf (SimplePrim (Signed Int16)) stdgen =
  randomC conf i16Range stdgen
randomValue conf (SimplePrim (Signed Int32)) stdgen =
  randomC conf i32Range stdgen
randomValue conf (SimplePrim (Signed Int64)) stdgen =
  randomC conf i64Range stdgen

randomValue conf (SimplePrim (Unsigned Int8)) stdgen =
  randomC conf u8Range stdgen
randomValue conf (SimplePrim (Unsigned Int16)) stdgen =
  randomC conf u16Range stdgen
randomValue conf (SimplePrim (Unsigned Int32)) stdgen =
  randomC conf u32Range stdgen
randomValue conf (SimplePrim (Unsigned Int64)) stdgen =
  randomC conf u64Range stdgen

randomValue _ (SimplePrim Bool) stdgen =
  first (SimplePrimValue . BoolValue) $ random stdgen

randomValue conf (SimplePrim (FloatType Float32)) stdgen =
  randomC conf f32Range stdgen
randomValue conf (SimplePrim (FloatType Float64)) stdgen =
  randomC conf f64Range stdgen

randomValue conf (SimpleArray t d) stdgen =
  first SimpleArrayValue $ uncurry (flip (,)) $
  mapAccumL f stdgen [0..d-1]
  where f stdgen' _ = uncurry (flip (,)) $ randomValue conf t stdgen'

class ToFuthark a where
  toFuthark :: a -> SimpleValue

instance ToFuthark Int8 where
  toFuthark = SimplePrimValue . SignedValue . Int8Value
instance ToFuthark Int16 where
  toFuthark = SimplePrimValue . SignedValue . Int16Value
instance ToFuthark Int32 where
  toFuthark = SimplePrimValue . SignedValue . Int32Value
instance ToFuthark Int64 where
  toFuthark = SimplePrimValue . SignedValue . Int64Value
instance ToFuthark Word8 where
  toFuthark = SimplePrimValue . UnsignedValue . Int8Value . fromIntegral
instance ToFuthark Word16 where
  toFuthark = SimplePrimValue . UnsignedValue . Int16Value . fromIntegral
instance ToFuthark Word32 where
  toFuthark = SimplePrimValue . UnsignedValue . Int32Value . fromIntegral
instance ToFuthark Word64 where
  toFuthark = SimplePrimValue . UnsignedValue . Int64Value . fromIntegral
instance ToFuthark Float where
  toFuthark = SimplePrimValue . FloatValue . Float32Value
instance ToFuthark Double where
  toFuthark = SimplePrimValue . FloatValue . Float64Value

randomC :: (ToFuthark a, Random a) =>
           RandomConfiguration -> (RandomConfiguration -> Range a) -> StdGen
        -> (SimpleValue, StdGen)
randomC conf pick = first toFuthark . randomR (pick conf)
