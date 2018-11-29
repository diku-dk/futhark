{-# LANGUAGE OverloadedStrings #-}
-- | Randomly generate Futhark input files containing values of a
-- specified type and shape.
module Main (main) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word
import qualified Data.Vector.Unboxed.Mutable as UMVec
import qualified Data.Vector.Unboxed as UVec
import Data.Vector.Generic (freeze)

import System.Console.GetOpt
import System.Random

import Language.Futhark.Syntax hiding (Value, PrimValue(..), IntValue(..), FloatValue(..))
import Language.Futhark.Attributes (UncheckedTypeExp, namesToPrimTypes)
import Language.Futhark.Parser
import Language.Futhark.Pretty ()

import Futhark.Test.Values
import Futhark.Util.Options

main :: IO ()
main = mainWithOptions initialDataOptions commandLineOptions "options..." f
  where f [] config
          | null $ optOrders config = Just $ do
              maybe_vs <- readValues <$> BS.getContents
              case maybe_vs of
                Nothing -> error "Malformed data on standard input."
                Just vs ->
                  case format config of
                    Text -> mapM_ (putStrLn . pretty) vs
                    Binary -> mapM_ (BS.putStr . Bin.encode) vs
                    Type -> mapM_ (putStrLn . pretty . valueType) vs
          | otherwise =
              Just $ zipWithM_ ($) (optOrders config) $ map mkStdGen [optSeed config..]
        f _ _ =
          Nothing

data OutputFormat = Text
                  | Binary
                  | Type
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
    (NoArg $ Right $ \opts -> opts { format = Text })
    "Output data in text format (must precede --generate)."
  , Option "b" ["binary"]
    (NoArg $ Right $ \opts -> opts { format = Binary })
    "Output data in binary Futhark format (must precede --generate)."
  , Option "t" ["type"]
    (NoArg $ Right $ \opts -> opts { format = Type })
    "Output the type (textually) rather than the value (must precede --generate)."
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
tryMakeGenerator t
  | Just vs <- readValues $ BS.pack t =
      return $ \_ fmt _ -> mapM_ (putValue fmt) vs
  | otherwise = do
  t' <- toValueType =<< either (Left . show) Right (parseType name (T.pack t))
  return $ \conf fmt stdgen -> do
    let (v, _) = randomValue conf t' stdgen
    putValue fmt v
  where name = "option " ++ t
        putValue Text = putStrLn . pretty
        putValue Binary = BS.putStr . Bin.encode
        putValue Type = putStrLn . pretty . valueType

toValueType :: UncheckedTypeExp -> Either String ValueType
toValueType TETuple{} = Left "Cannot handle tuples yet."
toValueType TERecord{} = Left "Cannot handle records yet."
toValueType TEApply{} = Left "Cannot handle type applications yet."
toValueType TEArrow{} = Left "Cannot generate functions."
toValueType TEEnum{} = Left "Cannot handle enums yet."
toValueType (TEUnique t _) = toValueType t
toValueType (TEArray t d _) = do
  d' <- constantDim d
  ValueType ds t' <- toValueType t
  return $ ValueType (d':ds) t'
  where constantDim (ConstDim k) = Right k
        constantDim _ = Left "Array has non-constant dimension declaration."
toValueType (TEVar (QualName [] v) _)
  | Just t <- M.lookup v namesToPrimTypes = Right $ ValueType [] t
toValueType (TEVar v _) =
  Left $ "Unknown type " ++ pretty v

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

randomValue :: RandomConfiguration -> ValueType -> StdGen -> (Value, StdGen)
randomValue conf (ValueType ds t) stdgen =
  case t of
    Signed Int8  -> gen  i8Range Int8Value
    Signed Int16 -> gen i16Range Int16Value
    Signed Int32 -> gen i32Range Int32Value
    Signed Int64 -> gen i64Range Int64Value
    Unsigned Int8  -> gen  u8Range Word8Value
    Unsigned Int16 -> gen u16Range Word16Value
    Unsigned Int32 -> gen u32Range Word32Value
    Unsigned Int64 -> gen u64Range Word64Value
    FloatType Float32 -> gen f32Range Float32Value
    FloatType Float64 -> gen f64Range Float64Value
    Bool -> gen (const (False,True)) BoolValue
  where gen range final = randomVector (range conf) final ds stdgen

randomVector :: (UMVec.Unbox v, Random v) =>
                Range v
             -> (UVec.Vector Int -> UVec.Vector v -> Value)
             -> [Int] -> StdGen
             -> (Value, StdGen)
randomVector range final ds stdgen = runST $ do
  -- USe some nice impure computation where we can preallocate a
  -- vector of the desired size, populate it via the random number
  -- generator, and then finally reutrn a frozen binary vector.
  arr <- UMVec.new n
  let fill stdgen' i
        | i < n = do
            let (v, stdgen'') = randomR range stdgen'
            UMVec.write arr i v
            fill stdgen'' $! i+1
        | otherwise = do
            arr' <- final (UVec.fromList ds) <$> freeze arr
            return (arr', stdgen')
  fill stdgen 0
  where n = product ds
