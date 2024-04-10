{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Futhark.CLI.View (main) where

import Control.Monad
import Data.Functor.Identity
import Control.Monad.IO.Class
import Futhark.Analysis.View
import Futhark.Compiler
import Futhark.Util.Options
import Futhark.Util.Pretty (hPutDoc, putDoc, Pretty (pretty), prettyString)
import Language.Futhark.Warnings
import System.IO
import Futhark.Analysis.View.Representation
import qualified Data.Map as M
import qualified Futhark.SoP.SoP as SoP
import Language.Futhark (VName (VName), nameFromString, nameToString, baseString)
import Data.List.NonEmpty (fromList)

-- import Futhark.Internalise.Defunctionalise as Defunctionalise
-- import Futhark.Internalise.Defunctorise as Defunctorise
-- import Futhark.Internalise.FullNormalise as FullNormalise
-- import Control.Monad.State
-- import Debug.Trace (traceM)

newtype RefineConfig = RefineConfig
  { checkWarn :: Bool }

newRefineConfig :: RefineConfig
newRefineConfig = RefineConfig True

options :: [FunOptDescr RefineConfig]
options =
  [ ]

tests :: [(FilePath, View)]
tests =
  [ ("tests/refinement/part2indices.fut",
     View (Forall (nom "i") (Iota (Var (nom "n"))))
          (Cases (fromList [
            (Idx (Var (nom "conds")) (Var (nom "i")),
             SoP (SoP.int2SoP (-1))
               ~+~ Sum (Var (nom "j"))
                       (SoP (SoP.int2SoP 0))
                       (Var (nom "i"))
                       (Indicator (Idx (Var (nom "conds")) (Var (nom "j"))))
            ),
            (Not (Idx (Var (nom "conds")) (Var (nom "i"))),
               SoP (SoP.int2SoP (-1))
                 ~+~ Sum (Var (nom "j"))
                         (SoP (SoP.int2SoP 0))
                         (Var (nom "n") ~-~ SoP (SoP.int2SoP 1))
                         (Indicator (Idx (Var (nom "conds")) (Var (nom "j"))))
                 ~+~ Sum (Var (nom "j"))
                         (SoP (SoP.int2SoP 0))
                         (Var (nom "i"))
                         (Indicator (Not (Idx (Var (nom "conds")) (Var (nom "j")))))
            )
          ]))
    )
  ]
  where
    nom name = VName (nameFromString name) 0

corruptId :: VName -> VName
corruptId (VName vn _) = VName vn 1337

corruptIds :: ASTMappable x => x -> x
corruptIds x = do
  runIdentity $ astMap substituter x
  where
    substituter =
      ASTMapper
        { mapOnExp = onExp }
    onExp (Var vn) = pure $ Var (corruptId vn)
    onExp e = astMap substituter e

corruptDomIds :: Domain -> Domain
corruptDomIds (Iota n) = Iota $ corruptIds n
corruptDomIds (Range a b) = Range (corruptIds a) (corruptIds b)
corruptDomIds (Union k m dom) = Union (corruptId k) (corruptIds m) (corruptDomIds dom)

-- Set all VName ids to the same value for janky equality.
corruptView :: View -> View
corruptView (View (Forall i dom) cases) =
  corruptIds $ View (Forall i (corruptDomIds dom)) cases
corruptView v = corruptIds v


-- | Run tests on all files in the "tests/refinement" directory.
runAllTests :: IO ()
runAllTests = do
  -- files <- filter hasFutExt <$> listDirectory "tests/refinement"
  -- let files' = filter hasFutExt files
  mapM_ runTest tests
  -- where
  --   hasFutExt fp = ".fut" == (reverse . take 4 . reverse $ fp)

-- | Run test on a specific file.
runTest :: (FilePath, View) -> IO ()
runTest (file, expected) = do
  putStrLn $ "Running test on file: " ++ file
  (_warnings, imports, src) <- readProgramOrDie file
  let res = M.mapKeys baseString $ mkViewProg src imports
  let actual = res M.! "inds"
  putStrLn ("Actual:\n" <> prettyString (corruptView actual))
  putStrLn ("Expected:\n" <> prettyString (corruptView expected))
  putStrLn ("lol:\n" <> show (corruptView expected == corruptView actual))
  if prettyString (corruptView expected) == prettyString (corruptView actual)
  then putStrLn $ "Test passed: " ++ file
  else error $ "Test failed: " ++ file

-- | Run @futhark refinement@.
main :: String -> [String] -> IO ()
main = mainWithOptions newRefineConfig options "program" $ \args cfg ->
  case args of
    ["test"] -> Just runAllTests
    [file] -> Just $ do
      (warnings, imports, src) <- readProgramOrDie file
      when (checkWarn cfg && anyWarnings warnings) $
        liftIO $
          hPutDoc stderr $
            prettyWarnings warnings
      -- putStrLn $ "Proved: " <> take 100 (show (mkViewProg vns imps)) <> "..."
      -- let valbinds = flip evalState src $
      --                  Defunctorise.transformProg imports
      --                  >>= FullNormalise.transformProg
      -- let res = mkViewProg src valbinds
      let res = mkViewProg src imports
      putStrLn "\nIndex function:\n---------------\n"
      putDoc (pretty res)
    _ -> Nothing

