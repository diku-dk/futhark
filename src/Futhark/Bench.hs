{-# LANGUAGE OverloadedStrings #-}
-- | Facilities for handling Futhark benchmark results.  A Futhark
-- benchmark program is just like a Futhark test program.
module Futhark.Bench
  ( RunResult (..)
  , DataResult(..)
  , BenchResult(..)
  , encodeBenchResults
  , decodeBenchResults
  )
  where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Aeson as JSON

newtype RunResult = RunResult { runMicroseconds :: Int }
                  deriving (Eq, Show)
data DataResult = DataResult String (Either T.Text ([RunResult], T.Text))
                deriving (Eq, Show)
data BenchResult = BenchResult FilePath [DataResult]
                 deriving (Eq, Show)

-- Intermediate types to help write the JSON instances.
newtype DataResults = DataResults { unDataResults :: [DataResult] }
newtype BenchResults = BenchResults { unBenchResults :: [BenchResult] }

instance JSON.ToJSON RunResult where
  toJSON = JSON.toJSON . runMicroseconds

instance JSON.FromJSON RunResult where
  parseJSON = fmap RunResult . JSON.parseJSON

instance JSON.ToJSON DataResults where
  toJSON (DataResults rs) =
    JSON.object $ map dataResultJSON rs
  toEncoding (DataResults rs) =
    JSON.pairs $ mconcat $ map (uncurry (JSON..=) . dataResultJSON) rs

instance JSON.FromJSON DataResults where
  parseJSON = JSON.withObject "datasets" $ \o ->
    DataResults <$> mapM datasetResult (HM.toList o)
    where datasetResult (k, v) =
            DataResult (T.unpack k) <$>
            ((Right <$> success v) <|> (Left <$> JSON.parseJSON v))
          success = JSON.withObject "result" $ \o ->
            (,) <$> o JSON..: "runtimes" <*> o JSON..: "stderr"

dataResultJSON :: DataResult -> (T.Text, JSON.Value)
dataResultJSON (DataResult desc (Left err)) =
  (T.pack desc, JSON.toJSON err)
dataResultJSON (DataResult desc (Right (runtimes, progerr))) =
  (T.pack desc, JSON.object
                [("runtimes", JSON.toJSON $ map runMicroseconds runtimes),
                 ("stderr", JSON.toJSON progerr)])

benchResultJSON :: BenchResult -> (T.Text, JSON.Value)
benchResultJSON (BenchResult prog r) =
  (T.pack prog,
   JSON.Object $ HM.singleton "datasets" (JSON.toJSON $ DataResults r))

instance JSON.ToJSON BenchResults where
  toJSON (BenchResults rs) =
    JSON.Object $ HM.fromList $ map benchResultJSON rs

instance JSON.FromJSON BenchResults where
  parseJSON = JSON.withObject "benchmarks" $ \o ->
    BenchResults <$> mapM onBenchmark (HM.toList o)
    where onBenchmark (k, v) =
            BenchResult (T.unpack k) <$>
            JSON.withObject "benchmark" onBenchmark' v
          onBenchmark' o =
            fmap unDataResults . JSON.parseJSON =<< o JSON..: "datasets"

-- | Transform benchmark results to a JSON bytestring.
encodeBenchResults :: [BenchResult] -> LBS.ByteString
encodeBenchResults = JSON.encode . BenchResults

-- | Decode benchmark results from a JSON bytestring.
decodeBenchResults :: LBS.ByteString -> Either String [BenchResult]
decodeBenchResults = fmap unBenchResults . JSON.eitherDecode'
