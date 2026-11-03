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

tests :: TestTree
tests =
  testGroup
    "Language.WGSL"
    [ testGroup "types" [pointerTests]
    ]

main :: IO ()
main = defaultMain tests
