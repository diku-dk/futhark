module Main (main) where

import Data.Text qualified as T
import Language.WGSL
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty
import Test.Tasty.HUnit

-- | Render an AST fragment as a code generator would.
render :: (Pretty a) => a -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | A pointer type is spelled @ptr<AS,T,AM>@, where the access mode is
-- optional.  See https://www.w3.org/TR/WGSL/#ref-ptr-types.
pointerTests :: TestTree
pointerTests =
  testGroup
    "Pointer"
    [ testCase "without access mode" $
        render (Pointer Int32 FunctionSpace Nothing)
          @?= "ptr<function, i32>",
      testCase "with access mode" $
        render (Pointer Int32 FunctionSpace (Just ReadWrite))
          @?= "ptr<function, i32, read_write>",
      testCase "uniform" $
        render (Pointer Float32 Uniform Nothing)
          @?= "ptr<uniform, f32>",
      testCase "vector store type" $
        render (Pointer (Vec3 Float32) Workgroup (Just ReadOnly))
          @?= "ptr<workgroup, vec3<f32>, read>"
    ]

-- | A literal is never negative, and its suffix fixes its type.  See
-- https://www.w3.org/TR/WGSL/#numeric-literals.
intLitTests :: TestTree
intLitTests =
  testGroup
    "intExp"
    [ testCase "no suffix" $
        fmap render (intExp 5 AbstractInt) @?= Just "5",
      testCase "i suffix" $
        fmap render (intExp 5 I32) @?= Just "5i",
      testCase "u suffix" $
        fmap render (intExp 5 U32) @?= Just "5u",
      testCase "negated literal" $
        fmap render (intExp (-5) I32) @?= Just "-5i",
      testCase "largest i32" $
        fmap render (intExp 2147483647 I32) @?= Just "2147483647i",
      testCase "i32 overflow" $
        fmap render (intExp 2147483648 I32) @?= Nothing,
      -- The literal is 2147483648, which i32 cannot represent, even though
      -- the negated value can be.
      testCase "most negative i32" $
        fmap render (intExp (-2147483648) I32) @?= Nothing,
      testCase "largest u32" $
        fmap render (intExp 4294967295 U32) @?= Just "4294967295u",
      testCase "u32 overflow" $
        fmap render (intExp 4294967296 U32) @?= Nothing,
      testCase "negative u32" $
        fmap render (intExp (-1) U32) @?= Nothing,
      testCase "largest AbstractInt" $
        fmap render (intExp maxBound AbstractInt) @?= Just "9223372036854775807",
      testCase "AbstractInt overflow" $
        fmap render (intExp minBound AbstractInt) @?= Nothing
    ]

-- | WGSL has no literal for infinities or NaN, and the magnitude of a literal
-- must not exceed the largest finite value of its type.  See
-- https://www.w3.org/TR/WGSL/#floating-point-types.
floatLitTests :: TestTree
floatLitTests =
  testGroup
    "floatExp"
    [ testCase "no suffix" $
        fmap render (floatExp 1.5 AbstractFloat) @?= Just "1.5",
      testCase "f suffix" $
        fmap render (floatExp 1.5 F32) @?= Just "1.5f",
      testCase "h suffix" $
        fmap render (floatExp 1.5 F16) @?= Just "1.5h",
      testCase "negated literal" $
        fmap render (floatExp (-1.5) F32) @?= Just "-1.5f",
      testCase "exponent" $
        fmap render (floatExp 1.0e-2 F32) @?= Just "1.0e-2f",
      testCase "infinity" $
        fmap render (floatExp (1 / 0) AbstractFloat) @?= Nothing,
      testCase "negative infinity" $
        fmap render (floatExp (-1 / 0) F32) @?= Nothing,
      testCase "NaN" $
        fmap render (floatExp (0 / 0) F32) @?= Nothing,
      testCase "largest f32" $
        fmap render (floatExp 3.40282346638528859812e38 F32)
          @?= Just "3.4028234663852886e38f",
      testCase "f32 overflow" $
        fmap render (floatExp 1.0e300 F32) @?= Nothing,
      testCase "f64 magnitude is an AbstractFloat" $
        fmap render (floatExp 1.0e300 AbstractFloat) @?= Just "1.0e300",
      testCase "largest f16" $
        fmap render (floatExp 65504 F16) @?= Just "65504.0h",
      testCase "f16 overflow" $
        fmap render (floatExp 65505 F16) @?= Nothing
    ]

tests :: TestTree
tests =
  testGroup
    "Language.WGSL"
    [ testGroup "types" [pointerTests],
      testGroup "expressions" [intLitTests, floatLitTests]
    ]

main :: IO ()
main = defaultMain tests
