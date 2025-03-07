{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Futhark.Analysis.Properties.UnifyTests (tests) where

import Data.Map qualified as M
import Futhark.Analysis.Properties.IndexFn
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.IndexFnPlus ()
import Futhark.Analysis.Properties.Symbol
import Futhark.Analysis.Properties.SymbolPlus ()
import Futhark.Analysis.Properties.Unify
import Futhark.MonadFreshNames
import Futhark.SoP.SoP (int2SoP, scaleSoP, sym2SoP, (.*.), (.+.))
import Futhark.Util.Pretty (prettyString)
import Test.Tasty
import Test.Tasty.HUnit

runTest :: IndexFnM (Maybe (Substitution Symbol)) -> Maybe (Substitution Symbol)
runTest test = fst $ runIndexFnM test blankNameSource

getValue :: IndexFnM a -> a
getValue test = fst $ runIndexFnM test blankNameSource

tests :: TestTree
tests =
  testGroup
    "Properties.Unify"
    [ testCase "Add" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .+. hole y) (name2SoP z .+. name2SoP w)
          )
          @??= x2z_y2w,
      testCase "Multiply" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .*. hole y) (name2SoP z .*. name2SoP w)
          )
          @??= x2z_y2w,
      testCase "Multiply constant" $
        run
          ( \(x, y, _, w, _, _, _, _, _) ->
              unify (hole x .*. hole y) (int2SoP 2 .*. name2SoP w)
          )
          @??= (x2z_y2w >>= \s -> pure $ s {mapping = M.adjust (const $ int2SoP 2) x $ mapping s}),
      testCase "First is scaled" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (scaleSoP 2 (hole x) .+. hole y) (name2SoP z .+. name2SoP w)
          )
          @??= Nothing,
      testCase "Second is scaled" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .+. hole y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
          )
          @??= (x2z_y2w >>= \s -> pure $ s {mapping = M.adjust (scaleSoP 2) y $ mapping s}),
      testCase "Both scaled, but permuted" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (scaleSoP 2 (hole x) .+. hole y) (name2SoP z .+. scaleSoP 2 (name2SoP w))
          )
          @??= x2w_y2z,
      testCase "Wrong operator" $
        run
          ( \(x, y, _, _, _, _, _, _, _) ->
              unify (hole x .*. hole y) (name2SoP x .+. name2SoP y)
          )
          @??= Nothing,
      testCase "One has constant" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .+. hole y .+. int2SoP 2) (name2SoP z .+. name2SoP w)
          )
          @??= Nothing,
      testCase "Different constants" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .+. hole y .+. int2SoP 1) (name2SoP z .+. name2SoP w .+. int2SoP 2)
          )
          @??= Nothing,
      testCase "Same constant" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (hole x .+. hole y .+. int2SoP 2) (name2SoP z .+. name2SoP w .+. int2SoP 2)
          )
          @??= x2z_y2w,
      -- Indexing.
      testCase "Indexing" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (Idx (Hole x) (hole y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 1))
          )
          @??= x2z_y2w,
      testCase "Indexing different constant" $
        run
          ( \(x, y, z, w, _, _, _, _, _) ->
              unify (Idx (Hole x) (hole y .+. int2SoP 1)) (Idx (Var z) (name2SoP w .+. int2SoP 2))
          )
          @??= Nothing,
      -- Substituting with quantifiers.
      testCase "Bound names are not substituted" $
        run
          ( \(x, y, z, w, a, b, c, d, _) ->
              unify (Sum x (hole y) (hole z) (Hole w)) (Sum a (name2SoP b) (name2SoP c) (Var d))
          )
          @??= y2b_z2c_w2d,
      testCase "Bound names are renamed" $
        run
          ( \(x, _, _, _, a, b, c, d, _) ->
              unify (Hole x) (Sum a (name2SoP b) (name2SoP c) (Var d))
          )
          @??= let renamed_lin_comb = getValue $ do
                     (_, _, _, _, a, b, c, d, _) <- varsM
                     _ <- newVName "k" -- Simulate "k" introduced by Unify.
                     vns <- getNameSource
                     rename vns $ sym2SoP (Sum a (name2SoP b) (name2SoP c) (Var d))
                in Just (mkSub x renamed_lin_comb),
      testCase "These shouldn't unify because w is different from a!" $
        -- Pattern should require `Idx (Var c) (Var a)`.
        run
          ( \(x, y, z, w, a, b, c, d, _) ->
              unify
                (Idx (Hole z) (hole x) ~+~ Sum d (hole x) (hole y) (Hole z))
                (Idx (Var c) (name2SoP w) ~+~ Sum d (name2SoP a) (name2SoP b) (Var c))
          )
          @??= Nothing,
      -- TODO This test shouldn't be allowed since we assume VNames in first argument are holes?
      -- , testCase "Substitute only some symbols" $
      --     run_xyzw (\(x,y,z,_) ->
      --       unify (name2SoP x .*. name2SoP y .*. name2SoP w) (name2SoP z .*. name2SoP w .*. name2SoP w)
      --     ) @??= x2z_y2w
      -- Index functions.
      testCase "Iota index functions" $
        -- Pattern should require `Idx (Var c) (Var a)`.
        run
          ( \(x, y, z, w, a, b, c, d, _) ->
              unify
                ( IndexFn
                    { iterator = Forall x (Iota (hole y)),
                      body =
                        cases
                          [ (name2SoP x :== int2SoP 0, hole z),
                            (name2SoP x :/= int2SoP 0, hole w)
                          ]
                    }
                )
                ( IndexFn
                    { iterator = Forall a (Iota (name2SoP b)),
                      body =
                        cases
                          [ (name2SoP a :== int2SoP 0, name2SoP c),
                            (name2SoP a :/= int2SoP 0, name2SoP d)
                          ]
                    }
                )
          )
          @??= (mkSub x (name2SoP a) <>)
          <$> y2b_z2c_w2d
    ]
  where
    mkSub a b = Substitution {mapping = mkRep a b, vns = mempty}
    name2SoP = sym2SoP . Var
    hole = sym2SoP . Hole
    a ~+~ b = sym2SoP a .+. sym2SoP b

    varsM =
      (,,,,,,,,)
        <$> newVName "x"
        <*> newVName "y"
        <*> newVName "z"
        <*> newVName "w"
        <*> newVName "a"
        <*> newVName "b"
        <*> newVName "c"
        <*> newVName "d"
        <*> newVName "i"
    (x, y, z, w, a, b, c, d, _) = getValue varsM

    x2z_y2w = Just $ mkSub x (name2SoP z) <> mkSub y (name2SoP w)
    x2w_y2z = Just $ mkSub x (name2SoP w) <> mkSub y (name2SoP z)
    y2b_z2c_w2d =
      Just $ mkSub y (name2SoP b) <> mkSub z (name2SoP c) <> mkSub w (name2SoP d)

    run f = runTest (varsM >>= f)
    actual @??= expected =
      assertEqual
        ( "expected: "
            <> prettyString expected
            <> "\nbut got: "
            <> prettyString actual
        )
        expected
        actual
    infix 1 @??=
