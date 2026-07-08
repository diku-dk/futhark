{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | @futhark dataset@
module Futhark.CLI.Dataset (main) where

import Control.Monad
import Data.Binary qualified as Bin
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word
import Futhark.Data qualified as V
import Futhark.Data.Reader (readValues)
import Futhark.Test.Values
  ( RandomConfiguration (..),
    Range,
    initialRandomConfiguration,
    randomValue,
  )
import Futhark.Util.Options
import Language.Futhark.Parser
import Language.Futhark.Pretty ()
import Language.Futhark.Prop (UncheckedTypeExp)
import Language.Futhark.Syntax hiding
  ( FloatValue (..),
    IntValue (..),
    PrimValue (..),
    ValueType,
  )
import System.Exit
import System.IO

-- | Run @futhark dataset@.
main :: String -> [String] -> IO ()
main = mainWithOptions initialDataOptions commandLineOptions "options..." f
  where
    f [] config
      | null $ optOrders config = Just $ do
          maybe_vs <- readValues <$> BS.getContents
          case maybe_vs of
            Nothing -> do
              hPutStrLn stderr "Malformed data on standard input."
              exitFailure
            Just vs ->
              case format config of
                Text -> mapM_ (T.putStrLn . V.valueText) vs
                Binary -> mapM_ (BS.putStr . Bin.encode) vs
                Type -> mapM_ (T.putStrLn . V.valueTypeText . V.valueType) vs
      | otherwise =
          Just $
            zipWithM_
              ($)
              (optOrders config)
              [fromIntegral (optSeed config) ..]
    f _ _ =
      Nothing

data OutputFormat
  = Text
  | Binary
  | Type
  deriving (Eq, Ord, Show)

data DataOptions = DataOptions
  { optSeed :: Int,
    optRange :: RandomConfiguration,
    optOrders :: [Word64 -> IO ()],
    format :: OutputFormat
  }

initialDataOptions :: DataOptions
initialDataOptions = DataOptions 1 initialRandomConfiguration [] Text

commandLineOptions :: [FunOptDescr DataOptions]
commandLineOptions =
  [ Option
      "s"
      ["seed"]
      ( ReqArg
          ( \n ->
              case reads n of
                [(n', "")] ->
                  Right $ \config -> config {optSeed = n'}
                _ ->
                  Left $ do
                    hPutStrLn stderr $ "'" ++ n ++ "' is not an integer."
                    exitFailure
          )
          "SEED"
      )
      "The seed to use when initialising the RNG.",
    Option
      "g"
      ["generate"]
      ( ReqArg
          ( \t ->
              case tryMakeGenerator t of
                Right g ->
                  Right $ \config ->
                    config
                      { optOrders =
                          optOrders config
                            ++ [g (optRange config) (format config)]
                      }
                Left err ->
                  Left $ do
                    T.hPutStrLn stderr err
                    exitFailure
          )
          "TYPE"
      )
      "Generate a random value of this type.",
    Option
      []
      ["text"]
      (NoArg $ Right $ \opts -> opts {format = Text})
      "Output data in text format (default; must precede --generate).",
    Option
      "b"
      ["binary"]
      (NoArg $ Right $ \opts -> opts {format = Binary})
      "Output data in binary Futhark format (must precede --generate).",
    Option
      "t"
      ["type"]
      (NoArg $ Right $ \opts -> opts {format = Type})
      "Output the type (textually) rather than the value (must precede --generate).",
    setRangeOption "i8" seti8Range,
    setRangeOption "i16" seti16Range,
    setRangeOption "i32" seti32Range,
    setRangeOption "i64" seti64Range,
    setRangeOption "u8" setu8Range,
    setRangeOption "u16" setu16Range,
    setRangeOption "u32" setu32Range,
    setRangeOption "u64" setu64Range,
    setRangeOption "f16" setf16Range,
    setRangeOption "f32" setf32Range,
    setRangeOption "f64" setf64Range
  ]

setRangeOption ::
  (Read a) =>
  String ->
  (Range a -> RandomConfiguration -> RandomConfiguration) ->
  FunOptDescr DataOptions
setRangeOption tname set =
  Option
    ""
    [name]
    ( ReqArg
        ( \b ->
            let (lower, rest) = span (/= ':') b
                upper = drop 1 rest
             in case (reads lower, reads upper) of
                  ([(lower', "")], [(upper', "")]) ->
                    Right $ \config ->
                      config {optRange = set (lower', upper') $ optRange config}
                  _ ->
                    Left $ do
                      hPutStrLn stderr $ "Invalid bounds for " ++ tname ++ ": " ++ b
                      exitFailure
        )
        "MIN:MAX"
    )
    $ "Range of " ++ tname ++ " values."
  where
    name = tname ++ "-bounds"

tryMakeGenerator ::
  String ->
  Either T.Text (RandomConfiguration -> OutputFormat -> Word64 -> IO ())
tryMakeGenerator t
  | Just vs <- readValues $ BS.pack t =
      pure $ \_ fmt _ -> mapM_ (outValue fmt) vs
  | otherwise = do
      t' <- toValueType =<< either (Left . syntaxErrorMsg) Right (parseType name (T.pack t))
      pure $ \conf fmt seed -> do
        let v = randomValue conf t' seed
        outValue fmt v
  where
    name = "option " ++ t
    outValue Text = T.putStrLn . V.valueText
    outValue Binary = BS.putStr . Bin.encode
    outValue Type = T.putStrLn . V.valueTypeText . V.valueType

toValueType :: UncheckedTypeExp -> Either T.Text V.ValueType
toValueType TETuple {} = Left "Cannot handle tuples yet."
toValueType TERecord {} = Left "Cannot handle records yet."
toValueType TEApply {} = Left "Cannot handle type applications yet."
toValueType TEArrow {} = Left "Cannot generate functions."
toValueType TESum {} = Left "Cannot handle sumtypes yet."
toValueType TEDim {} = Left "Cannot handle existential sizes."
toValueType (TEParens t _) = toValueType t
toValueType (TEUnique t _) = toValueType t
toValueType (TEArray d t _) = do
  d' <- constantDim d
  V.ValueType ds t' <- toValueType t
  pure $ V.ValueType (d' : ds) t'
  where
    constantDim (SizeExp (IntLit k _ _) _) = Right $ fromInteger k
    constantDim _ = Left "Array has non-constant dimension declaration."
toValueType (TEVar (QualName [] v) _)
  | Just t <- lookup v m = Right $ V.ValueType [] t
  where
    m = map f [minBound .. maxBound]
    f t = (nameFromText (V.primTypeText t), t)
toValueType (TEVar v _) =
  Left $ "Unknown type " <> prettyText v

-- The following lines provide evidence about how Haskells record
-- system sucks.
seti8Range :: Range Int8 -> RandomConfiguration -> RandomConfiguration
seti8Range bounds config = config {i8Range = bounds}

seti16Range :: Range Int16 -> RandomConfiguration -> RandomConfiguration
seti16Range bounds config = config {i16Range = bounds}

seti32Range :: Range Int32 -> RandomConfiguration -> RandomConfiguration
seti32Range bounds config = config {i32Range = bounds}

seti64Range :: Range Int64 -> RandomConfiguration -> RandomConfiguration
seti64Range bounds config = config {i64Range = bounds}

setu8Range :: Range Word8 -> RandomConfiguration -> RandomConfiguration
setu8Range bounds config = config {u8Range = bounds}

setu16Range :: Range Word16 -> RandomConfiguration -> RandomConfiguration
setu16Range bounds config = config {u16Range = bounds}

setu32Range :: Range Word32 -> RandomConfiguration -> RandomConfiguration
setu32Range bounds config = config {u32Range = bounds}

setu64Range :: Range Word64 -> RandomConfiguration -> RandomConfiguration
setu64Range bounds config = config {u64Range = bounds}

setf16Range :: Range Half -> RandomConfiguration -> RandomConfiguration
setf16Range bounds config = config {f16Range = bounds}

setf32Range :: Range Float -> RandomConfiguration -> RandomConfiguration
setf32Range bounds config = config {f32Range = bounds}

setf64Range :: Range Double -> RandomConfiguration -> RandomConfiguration
setf64Range bounds config = config {f64Range = bounds}
