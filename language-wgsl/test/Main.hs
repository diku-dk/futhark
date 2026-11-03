module Main (main) where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Language.WGSL
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty, vsep)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty
import Test.Tasty.HUnit

-- | The offsets, alignment and size of a structure, which the tests check
-- together.
structLayout :: [Type] -> (Maybe [Int], Maybe Int, Maybe Int)
structLayout = structLayoutOf . map (Field [] "m")

-- | As 'structLayout', but for members given in full, so that they may carry
-- layout attributes.
structLayoutOf :: [Field] -> (Maybe [Int], Maybe Int, Maybe Int)
structLayoutOf fs = (offsetOfMembers fs, alignOfStruct fs, sizeOfStruct fs)

-- | The alignment and size of a type, which the tests check together.
layout :: Type -> Maybe (Int, Int)
layout t = (,) <$> alignOf t <*> sizeOf t

-- | A convenient literal for tests that only need some expression.
one, two :: Exp
one = IntExp 1 AbstractInt
two = IntExp 2 AbstractInt

-- | Render a statement sequence, as a code generator would.
renderBody :: [Stmt] -> T.Text
renderBody =
  renderStrict . layoutPretty defaultLayoutOptions . vsep . map pretty

-- | Render an AST fragment as a code generator would.
render :: (Pretty a) => a -> T.Text
render = renderStrict . layoutPretty defaultLayoutOptions . pretty

-- | A pointer type is spelled @ptr<AS,T,AM>@, where the access mode is
-- optional.  See https://www.w3.org/TR/WGSL/#ref-ptr-types.
pointerTests :: TestTree
pointerTests =
  testGroup
    "Ptr"
    [ testCase "without access mode" $
        render (Ptr I32 Function Nothing)
          @?= "ptr<function, i32>",
      testCase "with access mode" $
        render (Ptr I32 Function (Just ReadWrite))
          @?= "ptr<function, i32, read_write>",
      testCase "uniform" $
        render (Ptr F32 Uniform Nothing)
          @?= "ptr<uniform, f32>",
      testCase "vector store type" $
        render (Ptr (Vec3 F32) Workgroup (Just Read))
          @?= "ptr<workgroup, vec3<f32>, read>",
      testCase "storage" $
        render (Ptr I32 Storage (Just Read))
          @?= "ptr<storage, i32, read>"
    ]

-- | An access mode is a predeclared enumerant.  See
-- https://www.w3.org/TR/WGSL/#memory-access-mode.
accessModeTests :: TestTree
accessModeTests =
  testGroup
    "AccessMode"
    [ testCase "read" $ render Read @?= "read",
      testCase "write" $ render Write @?= "write",
      testCase "read_write" $ render ReadWrite @?= "read_write"
    ]

-- | The address space of a module-scope variable is given as a template list
-- after the @var@ keyword, with the access mode second if present.  See
-- https://www.w3.org/TR/WGSL/#var-decls.
globalVarTests :: TestTree
globalVarTests =
  testGroup
    "GlobalVar"
    [ testCase "without access mode" $
        render (GlobalVar [] Workgroup Nothing "x" (Just I32) Nothing)
          @?= "var<workgroup> x : i32;",
      testCase "with access mode" $
        render (GlobalVar [] Storage (Just ReadWrite) "x" (Just (Array I32 Nothing)) Nothing)
          @?= "var<storage, read_write> x : array<i32>;",
      testCase "private" $
        render (GlobalVar [] Private Nothing "x" (Just F32) Nothing)
          @?= "var<private> x : f32;",
      testCase "immediate" $
        render (GlobalVar [] Immediate Nothing "x" (Just (Named "S")) Nothing)
          @?= "var<immediate> x : S;",
      testCase "with attributes" $
        render (GlobalVar (bindingAttribs 0 1) Storage (Just Read) "x" (Just (Array F32 Nothing)) Nothing)
          @?= "@group(0) @binding(1)\nvar<storage, read> x : array<f32>;"
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
        fmap render (intExp 5 LitI32) @?= Just "5i",
      testCase "u suffix" $
        fmap render (intExp 5 LitU32) @?= Just "5u",
      testCase "negated literal" $
        fmap render (intExp (-5) LitI32) @?= Just "-5i",
      testCase "largest i32" $
        fmap render (intExp 2147483647 LitI32) @?= Just "2147483647i",
      testCase "i32 overflow" $
        fmap render (intExp 2147483648 LitI32) @?= Nothing,
      -- The literal is 2147483648, which i32 cannot represent, even though
      -- the negated value can be.
      testCase "most negative i32" $
        fmap render (intExp (-2147483648) LitI32) @?= Nothing,
      testCase "largest u32" $
        fmap render (intExp 4294967295 LitU32) @?= Just "4294967295u",
      testCase "u32 overflow" $
        fmap render (intExp 4294967296 LitU32) @?= Nothing,
      testCase "negative u32" $
        fmap render (intExp (-1) LitU32) @?= Nothing,
      testCase "largest Nothing" $
        fmap render (intExp maxBound AbstractInt) @?= Just "9223372036854775807",
      testCase "Nothing overflow" $
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
        fmap render (floatExp 1.5 LitF32) @?= Just "1.5f",
      testCase "h suffix" $
        fmap render (floatExp 1.5 LitF16) @?= Just "1.5h",
      testCase "negated literal" $
        fmap render (floatExp (-1.5) LitF32) @?= Just "-1.5f",
      testCase "exponent" $
        fmap render (floatExp 1.0e-2 LitF32) @?= Just "1.0e-2f",
      testCase "infinity" $
        fmap render (floatExp (1 / 0) AbstractFloat) @?= Nothing,
      testCase "negative infinity" $
        fmap render (floatExp (-1 / 0) LitF32) @?= Nothing,
      testCase "NaN" $
        fmap render (floatExp (0 / 0) LitF32) @?= Nothing,
      testCase "largest f32" $
        fmap render (floatExp 3.40282346638528859812e38 LitF32)
          @?= Just "3.4028234663852886e38f",
      testCase "f32 overflow" $
        fmap render (floatExp 1.0e300 LitF32) @?= Nothing,
      testCase "f64 magnitude is an Nothing" $
        fmap render (floatExp 1.0e300 AbstractFloat) @?= Just "1.0e300",
      testCase "largest f16" $
        fmap render (floatExp 65504 LitF16) @?= Just "65504.0h",
      testCase "f16 overflow" $
        fmap render (floatExp 65505 LitF16) @?= Nothing
    ]

-- | An assignment's left-hand side is an assignable expression.  See
-- https://www.w3.org/TR/WGSL/#assignment.
assignTests :: TestTree
assignTests =
  testGroup
    "Assign"
    [ testCase "variable" $
        render (Assign (VarExp "x") SimpleAssign (VarExp "y")) @?= "x = y;",
      testCase "dereference" $
        render (Assign (UnOpExp Indirection (VarExp "p")) SimpleAssign (VarExp "x")) @?= "(*p) = x;",
      testCase "index" $
        render (Assign (IndexExp (VarExp "a") (IntExp 0 AbstractInt)) SimpleAssign (IntExp 1 AbstractInt))
          @?= "a[0] = 1;",
      testCase "member" $
        render (Assign (FieldExp (VarExp "s") "f") SimpleAssign (VarExp "y")) @?= "s.f = y;"
    ]

-- | A return statement supplies a value exactly when the enclosing function
-- has a return type.  See https://www.w3.org/TR/WGSL/#return-statement.
returnTests :: TestTree
returnTests =
  testGroup
    "Return"
    [ testCase "without value" $
        render (Return Nothing) @?= "return;",
      testCase "with value" $
        render (Return (Just (VarExp "x"))) @?= "return x;"
    ]

-- | A function declaration has at most one return type, which may carry
-- attributes.  See https://www.w3.org/TR/WGSL/#function-declaration-sec.
funDeclTests :: TestTree
funDeclTests =
  testGroup
    "FunDecl"
    [ testCase "without return type" $
        render (FunDecl "f" [] [Param [] "a" I32] Nothing [Call "g" [] [VarExp "a"]])
          @?= "fn f(\n  a : i32\n) {\n  g(a);\n}",
      testCase "with return type" $
        render
          (FunDecl "f" [] [] (Just ([], I32)) [Return (Just (IntExp 1 AbstractInt))])
          @?= "fn f() -> i32 {\n  return 1;\n}",
      testCase "empty body" $
        render (FunDecl "f" [] [] Nothing []) @?= "fn f() { }",
      testCase "with return attributes" $
        render
          ( FunDecl
              "vs"
              [Attrib "vertex" []]
              []
              (Just ([Attrib "builtin" [VarExp "position"]], Vec4 F32))
              [Return (Just (VarExp "p"))]
          )
          @?= "@vertex\nfn vs() -> @builtin(position) vec4<f32> {\n  return p;\n}"
    ]

sequenceTests :: TestTree
sequenceTests =
  testGroup
    "sequences"
    [ testCase "semicolons" $
        renderBody [Call "f" [] [], Let "x" Nothing one, Call "g" [] []]
          @?= "f();\nlet x = 1;\ng();",
      -- The separator used to end up inside the comment.
      testCase "comment" $
        renderBody [Call "f" [] [], Comment "note", Call "g" [] []]
          @?= "f();\n// note\ng();",
      testCase "compound statement" $
        renderBody [If (VarExp "c") [Call "f" [] []] [] Nothing, Call "g" [] []]
          @?= "if c {\n  f();\n}\ng();",
      -- An empty body used to render as a stray empty statement.
      testCase "empty body" $
        render (While (VarExp "c") []) @?= "while c { }",
      testCase "empty statement" $
        render (While (VarExp "c") [Empty]) @?= "while c {\n  ;\n}",
      testCase "block introduces a scope" $
        render (Compound [Let "x" Nothing one]) @?= "{\n  let x = 1;\n}",
      -- An absent else clause is not the same as an empty one.
      testCase "absent else" $
        render (If (VarExp "c") [] [] Nothing) @?= "if c { }",
      testCase "empty else" $
        render (If (VarExp "c") [] [] (Just [])) @?= "if c { } else { }"
    ]

-- | A for loop's update is an assignment or a call. See
-- https://www.w3.org/TR/WGSL/#for-statement.
forTests :: TestTree
forTests =
  testGroup
    "For"
    [ testCase "assignment update" $
        render
          ( For
              "i"
              one
              (BinOpExp Less (VarExp "i") one)
              (UpdateAssign (VarExp "i") SimpleAssign one)
              [Call "f" [] []]
          )
          @?= "for (var i = 1; (i < 1); i = 1) {\n  f();\n}",
      testCase "call update" $
        render (For "i" one (VarExp "c") (UpdateCall "step" [] [VarExp "i"]) [])
          @?= "for (var i = 1; c; step(i)) { }"
    ]

-- | A callee may carry a template argument list.  See
-- https://www.w3.org/TR/WGSL/#syntax-call_phrase.
callTests :: TestTree
callTests =
  testGroup
    "CallExp"
    [ testCase "without template arguments" $
        render (CallExp "f" [] [VarExp "x"]) @?= "f(x)",
      testCase "without arguments" $
        render (CallExp "atomicLoad" [] []) @?= "atomicLoad()",
      testCase "with a type argument" $
        render (CallExp "vec2" [TypeExp F32] [one, one])
          @?= "vec2<f32>(1, 1)",
      testCase "bitcast" $
        render (bitcast I32 (VarExp "x")) @?= "bitcast<i32>(x)",
      -- The template argument list end is disambiguated by the grammar, so a
      -- nested list needs no space between the closing delimiters.
      testCase "nested template arguments" $
        render (bitcast (Vec2 F16) (VarExp "x"))
          @?= "bitcast<vec2<f16>>(x)"
    ]

-- | The alignment and size table.  See
-- https://www.w3.org/TR/WGSL/#alignment-and-size.
layoutTests :: TestTree
layoutTests =
  testGroup
    "alignOf and sizeOf"
    [ -- bool is not host-shareable, but it does have a fixed footprint.
      testCase "bool" $ layout Bool @?= Just (4, 4),
      testCase "i32" $ layout I32 @?= Just (4, 4),
      testCase "f16" $ layout F16 @?= Just (2, 2),
      testCase "atomic" $ layout (Atomic I32) @?= Just (4, 4),
      testCase "vec2<f32>" $ layout (Vec2 F32) @?= Just (8, 8),
      testCase "vec3<f32>" $ layout (Vec3 F32) @?= Just (16, 12),
      testCase "vec4<f32>" $ layout (Vec4 F32) @?= Just (16, 16),
      testCase "vec2<f16>" $ layout (Vec2 F16) @?= Just (4, 4),
      testCase "vec3<f16>" $ layout (Vec3 F16) @?= Just (8, 6),
      testCase "vec4<f16>" $ layout (Vec4 F16) @?= Just (8, 8),
      testCase "named type" $ layout (Named "S") @?= Nothing,
      testCase "pointer" $ layout (Ptr I32 Function Nothing) @?= Nothing,
      testCase "texture" $ layout (SampledTexture Tex2d F32) @?= Nothing,
      testCase "sampler" $ layout Sampler @?= Nothing
    ]

-- | Texture and sampler types are opaque: they have no layout, and are not
-- host-shareable.  There is no other way to spell them, so no type that does
-- have a layout can be mistaken for one.  See
-- https://www.w3.org/TR/WGSL/#texture-sampler-types.
opaqueTests :: TestTree
opaqueTests =
  testGroup
    "textures and samplers"
    [ testCase "no layout" $
        map layout opaques @?= map (const Nothing) opaques,
      testCase "not host-shareable" $
        filter hostShareable opaques @?= [],
      testCase "sampled" $
        map
          render
          [ SampledTexture Tex1d F32,
            SampledTexture Tex2d F32,
            SampledTexture Tex2dArray I32,
            SampledTexture Tex3d U32,
            SampledTexture TexCube F32,
            SampledTexture TexCubeArray U32
          ]
          @?= [ "texture_1d<f32>",
                "texture_2d<f32>",
                "texture_2d_array<i32>",
                "texture_3d<u32>",
                "texture_cube<f32>",
                "texture_cube_array<u32>"
              ],
      testCase "multisampled" $
        render (MultisampledTexture F32) @?= "texture_multisampled_2d<f32>",
      testCase "external" $ render ExternalTexture @?= "texture_external",
      testCase "depth" $
        map
          render
          [ DepthTexture Depth2d,
            DepthTexture Depth2dArray,
            DepthTexture DepthCube,
            DepthTexture DepthCubeArray,
            DepthTexture DepthMultisampled2d
          ]
          @?= [ "texture_depth_2d",
                "texture_depth_2d_array",
                "texture_depth_cube",
                "texture_depth_cube_array",
                "texture_depth_multisampled_2d"
              ],
      testCase "storage" $
        map
          render
          [ StorageTexture StorageTex1d Rgba8Unorm Write,
            StorageTexture StorageTex2d R32Float Read,
            StorageTexture StorageTex2dArray Rgb10a2Uint ReadWrite,
            StorageTexture StorageTex3d Rg11b10Ufloat Write
          ]
          @?= [ "texture_storage_1d<rgba8unorm, write>",
                "texture_storage_2d<r32float, read>",
                "texture_storage_2d_array<rgb10a2uint, read_write>",
                "texture_storage_3d<rg11b10ufloat, write>"
              ],
      testCase "samplers" $
        map render [Sampler, SamplerComparison] @?= ["sampler", "sampler_comparison"]
    ]

-- | One of every texture and sampler family.
opaques :: [Type]
opaques =
  [ SampledTexture Tex2d F32,
    MultisampledTexture F32,
    ExternalTexture,
    StorageTexture StorageTex2d Rgba8Unorm Write,
    DepthTexture Depth2d,
    Sampler,
    SamplerComparison
  ]

-- | A matrix is laid out as an array of its columns, so @matCxR@ has the
-- alignment of @vecR@ and the size of @array<vecR, C>@.  See
-- https://www.w3.org/TR/WGSL/#alignment-and-size.
matrixLayoutTests :: TestTree
matrixLayoutTests =
  testGroup
    "matrices"
    [ testCase "mat2x2<f32>" $ layout (Mat2x2 F32) @?= Just (8, 16),
      testCase "mat3x2<f32>" $ layout (Mat3x2 F32) @?= Just (8, 24),
      testCase "mat4x2<f32>" $ layout (Mat4x2 F32) @?= Just (8, 32),
      -- The columns are padded from 12 bytes to a stride of 16.
      testCase "mat2x3<f32>" $ layout (Mat2x3 F32) @?= Just (16, 32),
      testCase "mat3x3<f32>" $ layout (Mat3x3 F32) @?= Just (16, 48),
      testCase "mat4x3<f32>" $ layout (Mat4x3 F32) @?= Just (16, 64),
      testCase "mat2x4<f32>" $ layout (Mat2x4 F32) @?= Just (16, 32),
      testCase "mat3x4<f32>" $ layout (Mat3x4 F32) @?= Just (16, 48),
      testCase "mat4x4<f32>" $ layout (Mat4x4 F32) @?= Just (16, 64),
      testCase "mat2x2<f16>" $ layout (Mat2x2 F16) @?= Just (4, 8),
      testCase "mat4x2<f16>" $ layout (Mat4x2 F16) @?= Just (4, 16),
      testCase "mat2x3<f16>" $ layout (Mat2x3 F16) @?= Just (8, 16),
      testCase "mat4x3<f16>" $ layout (Mat4x3 F16) @?= Just (8, 32),
      testCase "mat4x4<f16>" $ layout (Mat4x4 F16) @?= Just (8, 32)
    ]

-- | At least one type of every shape, for properties that should hold of all
-- of them.
representativeTypes :: [Type]
representativeTypes =
  [Bool, I32, U32, F16, F32]
    ++ [Vec2 F32, Vec3 F32, Vec4 F32, Vec2 F16]
    ++ [ Mat2x2 F32,
         Mat2x3 F32,
         Mat2x4 F32,
         Mat3x2 F32,
         Mat3x3 F32,
         Mat3x4 F32,
         Mat4x2 F32,
         Mat4x3 F32,
         Mat4x4 F32,
         Mat2x2 F16
       ]
    ++ [Atomic I32, Atomic U32]
    ++ [ Array F32 Nothing,
         Array F32 (Just (IntExp 8 AbstractInt)),
         Array F32 (Just (IntExp 8 LitI32)),
         Array (Named "S") (Just (IntExp 3 AbstractInt)),
         Array (Array F32 (Just (IntExp 2 AbstractInt))) Nothing
       ]
    ++ [ Ptr I32 Function Nothing,
         Ptr I32 Function (Just ReadWrite),
         Ptr I32 Storage (Just Read),
         Ptr (Array F32 Nothing) Private Nothing
       ]
    ++ [Named "S", Named "T"]
    ++ [ SampledTexture Tex1d F32,
         SampledTexture Tex2d F32,
         SampledTexture Tex2d I32,
         SampledTexture Tex2dArray F32,
         SampledTexture Tex3d F32,
         SampledTexture TexCube F32,
         SampledTexture TexCubeArray F32,
         MultisampledTexture F32,
         ExternalTexture,
         StorageTexture StorageTex1d Rgba8Unorm Write,
         StorageTexture StorageTex2d Rgba8Unorm Write,
         StorageTexture StorageTex2d R32Float Write,
         StorageTexture StorageTex2d R32Float Read,
         StorageTexture StorageTex2dArray Rgba8Unorm Write,
         StorageTexture StorageTex3d Rgba8Unorm Write,
         DepthTexture Depth2d,
         DepthTexture Depth2dArray,
         DepthTexture DepthCube,
         DepthTexture DepthCubeArray,
         DepthTexture DepthMultisampled2d,
         Sampler,
         SamplerComparison
       ]

-- | Distinct types must have distinct spellings.  Were two of them to spell
-- the same WGSL type, they could disagree about its layout, which is what an
-- open-ended escape hatch in 'Type' used to allow.
injectivityTests :: TestTree
injectivityTests =
  testGroup
    "spellings are distinct"
    [ testCase "the sample has no duplicates" $
        nub representativeTypes @?= representativeTypes,
      testCase "no two types spell the same" $
        let spellings = map render representativeTypes
         in nub spellings @?= spellings
    ]

-- | Every type is spelled as an identifier with an optional template argument
-- list.  See https://www.w3.org/TR/WGSL/#type-specifiers.
typeSpellingTests :: TestTree
typeSpellingTests =
  testGroup
    "spelling"
    [ testCase "scalars" $
        map render [Bool, I32, U32, F16, F32]
          @?= ["bool", "i32", "u32", "f16", "f32"],
      testCase "vector" $ render (Vec3 F32) @?= "vec3<f32>",
      testCase "matrix" $ render (Mat4x3 F16) @?= "mat4x3<f16>",
      testCase "atomic" $ render (Atomic U32) @?= "atomic<u32>",
      testCase "runtime-sized array" $ render (Array F32 Nothing) @?= "array<f32>",
      testCase "fixed-size array" $
        render (Array F32 (Just (IntExp 8 AbstractInt))) @?= "array<f32, 8>",
      -- Element and store types are no longer restricted to scalars.
      testCase "array of struct" $
        render (Array (Named "S") (Just (IntExp 3 AbstractInt))) @?= "array<S, 3>",
      testCase "array of array" $
        render
          ( Array
              (Array F32 (Just (IntExp 2 AbstractInt)))
              (Just (IntExp 3 AbstractInt))
          )
          @?= "array<array<f32, 2>, 3>",
      -- The specification's own example of a pointer type.
      testCase "pointer to array" $
        render (Ptr (Array F32 (Just (IntExp 50 AbstractInt))) Private Nothing)
          @?= "ptr<private, array<f32, 50>>",
      testCase "sampler" $ render Sampler @?= "sampler"
    ]

-- | An array's size is its element count times the element stride.  See
-- https://www.w3.org/TR/WGSL/#array-layout-examples.
arrayLayoutTests :: TestTree
arrayLayoutTests =
  testGroup
    "arrays"
    [ testCase "strideOf" $
        map strideOf [F32, Vec3 F32, F16, Named "S"]
          @?= [Just 4, Just 16, Just 2, Nothing],
      testCase "array<f32, 8>" $
        layout (Array F32 (Just (IntExp 8 AbstractInt))) @?= Just (4, 32),
      -- The element is padded from 12 bytes to a stride of 16.
      testCase "array<vec3<f32>, 8>" $
        layout (Array (Vec3 F32) (Just (IntExp 8 AbstractInt)))
          @?= Just (16, 128),
      testCase "runtime-sized array" $
        (alignOf (Array F32 Nothing), sizeOf (Array F32 Nothing))
          @?= (Just 4, Nothing),
      testCase "overridable element count" $
        layout (Array F32 (Just (VarExp "n"))) @?= Nothing
    ]

-- | See https://www.w3.org/TR/WGSL/#structure-member-layout.
structLayoutTests :: TestTree
structLayoutTests =
  testGroup
    "structures"
    [ -- struct A from the specification's example.
      testCase "implicit padding" $
        structLayout [F32, F32, Vec2 F32, F32]
          @?= (Just [0, 4, 8, 16], Just 8, Just 24),
      -- The first four members of struct B from the same example.
      testCase "vector alignment" $
        structLayout
          [Vec2 F32, Vec3 F32, F32, F32]
          @?= (Just [0, 16, 28, 32], Just 16, Just 48),
      testCase "single member" $
        structLayout [I32] @?= (Just [0], Just 4, Just 4),
      -- The offsets and alignment of a storage buffer's header are known even
      -- though its size is not.
      testCase "trailing runtime-sized array" $
        structLayout [U32, Array F32 Nothing]
          @?= (Just [0, 4], Just 4, Nothing),
      -- A member without a size stops the offsets of later members from
      -- being known, but the alignment still is.
      testCase "member without a size" $
        structLayout [Array F32 Nothing, U32]
          @?= (Nothing, Just 4, Nothing),
      testCase "no members" $
        structLayout [] @?= (Nothing, Nothing, Nothing),
      testCase "member without a layout" $
        structLayout [I32, Named "S"]
          @?= (Nothing, Nothing, Nothing)
    ]

-- | @\@align@ and @\@size@ override a member's alignment and size.  See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
memberLayoutTests :: TestTree
memberLayoutTests =
  testGroup
    "layout attributes"
    [ testCase "alignOfMember" $
        ( alignOfMember (Field [] "m" F32),
          alignOfMember (Field [Attrib "align" [IntExp 16 AbstractInt]] "m" F32)
        )
          @?= (Just 4, Just 16),
      testCase "sizeOfMember" $
        ( sizeOfMember (Field [] "m" F32),
          sizeOfMember (Field [Attrib "size" [IntExp 16 AbstractInt]] "m" F32)
        )
          @?= (Just 4, Just 16),
      testCase "unrelated attribute" $
        alignOfMember (Field [Attrib "location" [IntExp 0 AbstractInt]] "m" F32)
          @?= Just 4,
      -- The argument of @align or @size need only be a const-expression, and
      -- one that is not a literal leaves the member's layout unknown.  It
      -- must not fall back to the layout of the member's type.
      testCase "alignment that is not a literal" $
        ( alignOfMember (Field [Attrib "align" [VarExp "N"]] "m" F32),
          sizeOfMember (Field [Attrib "size" [VarExp "N"]] "m" F32)
        )
          @?= (Nothing, Nothing),
      testCase "structure with an unknown member alignment" $
        structLayoutOf
          [ Field [] "a" F32,
            Field [Attrib "align" [VarExp "N"]] "b" F32
          ]
          @?= (Nothing, Nothing, Nothing),
      -- struct A from the specification's example with explicit member sizes:
      -- the @size(16) on the last member grows the structure from 24 to 32.
      testCase "explicit size" $
        structLayoutOf
          [ Field [] "u" F32,
            Field [] "v" F32,
            Field [] "w" (Vec2 F32),
            Field [Attrib "size" [IntExp 16 AbstractInt]] "x" F32
          ]
          @?= (Just [0, 4, 8, 16], Just 8, Just 32),
      -- The same example's struct B places its @align(16) member at offset 48
      -- rather than 40.
      testCase "explicit alignment" $
        structLayoutOf
          [ Field [] "a" (Vec2 F32),
            Field [] "b" (Vec3 F32),
            Field [] "c" F32,
            Field [] "d" F32,
            Field [Attrib "align" [IntExp 16 AbstractInt]] "e" F32
          ]
          @?= (Just [0, 16, 28, 32, 48], Just 16, Just 64)
    ]

-- | A structure member may carry attributes, and so may an override
-- declaration.
attributeTests :: TestTree
attributeTests =
  testGroup
    "attributes"
    [ testCase "structure member" $
        render
          ( StructDecl
              "S"
              [ Field [] "a" I32,
                Field [Attrib "size" [IntExp 16 AbstractInt]] "b" F32
              ]
          )
          @?= "struct S {\n  a : i32,\n  @size(16) b : f32\n}",
      testCase "override identifier" $
        render
          (GlobalOverride [Attrib "id" [IntExp 7 AbstractInt]] "n" (Just I32) Nothing)
          @?= "@id(7)\noverride n : i32;",
      testCase "override without attributes" $
        render (GlobalOverride [] "n" (Just I32) (Just (IntExp 0 AbstractInt)))
          @?= "override n : i32 = 0;"
    ]

-- | See https://www.w3.org/TR/WGSL/#host-shareable-types.
hostShareableTests :: TestTree
hostShareableTests =
  testGroup
    "hostShareable"
    [ testCase "numeric scalar" $ hostShareable I32 @?= True,
      testCase "bool" $ hostShareable Bool @?= False,
      testCase "numeric vector" $ hostShareable (Vec3 F32) @?= True,
      testCase "vector of bool" $ hostShareable (Vec3 Bool) @?= False,
      testCase "atomic" $ hostShareable (Atomic I32) @?= True,
      testCase "runtime-sized array" $
        hostShareable (Array F32 Nothing) @?= True,
      testCase "fixed-size array" $
        hostShareable (Array F32 (Just (IntExp 8 AbstractInt))) @?= True,
      testCase "overridable element count" $
        hostShareable (Array F32 (Just (VarExp "n"))) @?= False,
      testCase "named type" $ hostShareable (Named "S") @?= False,
      testCase "pointer" $
        hostShareable (Ptr I32 Function Nothing) @?= False
    ]

-- | Operators are a closed set, spelled as the grammar spells them.  See
-- https://www.w3.org/TR/WGSL/#expressions.
operatorTests :: TestTree
operatorTests =
  testGroup
    "operators"
    [ testCase "binary" $
        map
          (\op -> render (BinOpExp op (VarExp "a") (VarExp "b")))
          [ Add,
            Subtract,
            Multiply,
            Divide,
            Remainder,
            BitwiseAnd,
            BitwiseOr,
            BitwiseExclusiveOr,
            ShiftLeft,
            ShiftRight,
            Less,
            LessEqual,
            Greater,
            GreaterEqual,
            Equal,
            NotEqual,
            ShortCircuitAnd,
            ShortCircuitOr
          ]
          @?= [ "(a + b)",
                "(a - b)",
                "(a * b)",
                "(a / b)",
                "(a % b)",
                "(a & b)",
                "(a | b)",
                "(a ^ b)",
                "(a << b)",
                "(a >> b)",
                "(a < b)",
                "(a <= b)",
                "(a > b)",
                "(a >= b)",
                "(a == b)",
                "(a != b)",
                "(a && b)",
                "(a || b)"
              ],
      testCase "unary" $
        map
          (\op -> render (UnOpExp op (VarExp "a")))
          [Negate, LogicalNegate, BitwiseComplement, Indirection, AddressOf]
          @?= ["(-a)", "(!a)", "(~a)", "(*a)", "(&a)"]
    ]

-- | Directives precede all declarations in a module.  See
-- https://www.w3.org/TR/WGSL/#directives.
directiveTests :: TestTree
directiveTests =
  testGroup
    "directives"
    [ testCase "enable" $
        render (Enable (EnableF16 :| [])) @?= "enable f16;",
      testCase "enable several" $
        render (Enable (EnableF16 :| [EnableSubgroups]))
          @?= "enable f16, subgroups;",
      testCase "every enable extension" $
        map
          (render . (\e -> Enable (e :| [])))
          [ EnableF16,
            EnableClipDistances,
            EnableDualSourceBlending,
            EnableSubgroups,
            EnablePrimitiveIndex,
            EnableSubgroupSizeControl
          ]
          @?= [ "enable f16;",
                "enable clip_distances;",
                "enable dual_source_blending;",
                "enable subgroups;",
                "enable primitive_index;",
                "enable subgroup_size_control;"
              ],
      testCase "requires" $
        render (Requires (TextureFormatsTier1 :| [PointerCompositeAccess]))
          @?= "requires texture_formats_tier1, pointer_composite_access;",
      testCase "diagnostic" $
        render (Diagnostic Off "derivative_uniformity")
          @?= "diagnostic(off, derivative_uniformity);",
      testCase "severities" $
        map (render . (`Diagnostic` "r")) [Error, Warning, Info, Off]
          @?= [ "diagnostic(error, r);",
                "diagnostic(warning, r);",
                "diagnostic(info, r);",
                "diagnostic(off, r);"
              ]
    ]

-- | A module is its directives followed by its declarations.  The f16 type
-- cannot legally be used without the directive that enables it, which is the
-- reason a module needs a representation of its own.
moduleTests :: TestTree
moduleTests =
  testGroup
    "Module"
    [ testCase "directives first" $
        render
          ( Module
              [Enable (EnableF16 :| [])]
              [GlobalVar [] Private Nothing "x" (Just F16) Nothing]
          )
          @?= "enable f16;\nvar<private> x : f16;",
      testCase "without directives" $
        render (Module [] [GlobalVar [] Private Nothing "x" (Just F32) Nothing])
          @?= "var<private> x : f32;",
      testCase "const_assert" $
        render (Module [] [GlobalAssert (VarExp "c")])
          @?= "const_assert c;",
      testCase "structure" $
        render
          ( Module
              []
              [GlobalStruct (StructDecl "S" [Field [] "a" I32, Field [] "b" F32])]
          )
          @?= "struct S {\n  a : i32,\n  b : f32\n}"
    ]

-- | An @if@ may be followed by any number of @else if@ clauses.  See
-- https://www.w3.org/TR/WGSL/#if-statement.
elseIfTests :: TestTree
elseIfTests =
  testGroup
    "else if"
    [ testCase "one clause" $
        render (If (VarExp "a") [Break] [(VarExp "b", [Continue])] Nothing)
          @?= "if a {\n  break;\n} else if b {\n  continue;\n}",
      testCase "chain with else" $
        render
          ( If
              (VarExp "a")
              [Break]
              [(VarExp "b", [Continue]), (VarExp "c", [Discard])]
              (Just [Return Nothing])
          )
          @?= "if a {\n  break;\n} else if b {\n  continue;\n}"
            <> " else if c {\n  discard;\n} else {\n  return;\n}"
    ]

-- | See https://www.w3.org/TR/WGSL/#switch-statement.
switchTests :: TestTree
switchTests =
  testGroup
    "switch"
    [ testCase "several selectors" $
        render (Switch (VarExp "x") (Case (CaseExp one :| [CaseExp two]) [Break] :| []))
          @?= "switch x {\n  case 1, 2: {\n    break;\n  }\n}",
      -- 'default' may appear among a case clause's selectors, as well as
      -- being a clause of its own.
      testCase "default as a selector" $
        render (Switch (VarExp "x") (Case (CaseExp one :| [CaseDefault]) [] :| []))
          @?= "switch x {\n  case 1, default: { }\n}",
      testCase "default clause" $
        render (Switch (VarExp "x") (DefaultCase [Discard] :| []))
          @?= "switch x {\n  default: {\n    discard;\n  }\n}",
      testCase "several clauses" $
        render
          ( Switch
              (VarExp "x")
              (Case (CaseExp one :| []) [Break] :| [DefaultCase []])
          )
          @?= "switch x {\n  case 1: {\n    break;\n  }\n  default: { }\n}"
    ]

-- | A @loop@ may end in a @continuing@ block, which may end in a @break if@.
-- See https://www.w3.org/TR/WGSL/#loop-statement.
loopTests :: TestTree
loopTests =
  testGroup
    "loop"
    [ testCase "without continuing" $
        render (Loop [Break] Nothing) @?= "loop {\n  break;\n}",
      testCase "with continuing" $
        render (Loop [Call "f" [] []] (Just (Continuing [Assign (VarExp "i") SimpleAssign one] Nothing)))
          @?= "loop {\n  f();\n  continuing {\n    i = 1;\n  }\n}",
      testCase "with break if" $
        render (Loop [] (Just (Continuing [] (Just (VarExp "done")))))
          @?= "loop {\n  continuing {\n    break if done;\n  }\n}",
      testCase "jumps" $
        map render [Break, Continue, Discard]
          @?= ["break;", "continue;", "discard;"]
    ]

-- | An assignment carries an operator, which may be a compound one.  See
-- https://www.w3.org/TR/WGSL/#assignment.
assignOpTests :: TestTree
assignOpTests =
  testGroup
    "assignment operators"
    [ testCase "every operator" $
        map
          (\op -> render (Assign (VarExp "x") op one))
          [ SimpleAssign,
            AddAssign,
            SubtractAssign,
            MultiplyAssign,
            DivideAssign,
            RemainderAssign,
            BitwiseAndAssign,
            BitwiseOrAssign,
            BitwiseExclusiveOrAssign,
            ShiftLeftAssign,
            ShiftRightAssign
          ]
          @?= [ "x = 1;",
                "x += 1;",
                "x -= 1;",
                "x *= 1;",
                "x /= 1;",
                "x %= 1;",
                "x &= 1;",
                "x |= 1;",
                "x ^= 1;",
                "x <<= 1;",
                "x >>= 1;"
              ],
      testCase "phony assignment" $
        render (PhonyAssign (CallExp "f" [] [])) @?= "_ = f();",
      testCase "increment and decrement" $
        map render [Increment (VarExp "x"), Decrement (IndexExp (VarExp "a") one)]
          @?= ["x++;", "a[1]--;"],
      testCase "const_assert" $
        render (ConstAssert (VarExp "c")) @?= "const_assert c;"
    ]

-- | A @let@, @const@ or @var@ names an optionally typed identifier, and a
-- @var@ may have an initialiser.  See
-- https://www.w3.org/TR/WGSL/#var-and-value.
localDeclTests :: TestTree
localDeclTests =
  testGroup
    "local declarations"
    [ testCase "let" $
        map render [Let "a" Nothing one, Let "a" (Just F32) one]
          @?= ["let a = 1;", "let a : f32 = 1;"],
      testCase "const" $
        map render [Const "a" Nothing one, Const "a" (Just F32) one]
          @?= ["const a = 1;", "const a : f32 = 1;"],
      testCase "var" $
        map
          render
          [ DeclareVar "a" (Just I32) Nothing,
            DeclareVar "a" (Just I32) (Just one),
            DeclareVar "a" Nothing (Just one)
          ]
          @?= ["var a : i32;", "var a : i32 = 1;", "var a = 1;"]
    ]

-- | The update of a @for@ loop may be any of the variable-updating
-- statements, or a call.  See https://www.w3.org/TR/WGSL/#for-statement.
forUpdateTests :: TestTree
forUpdateTests =
  testGroup
    "for updates"
    [ testCase "every form" $
        map
          (\u -> render (For "i" one (VarExp "c") u []))
          [ UpdateAssign (VarExp "i") AddAssign one,
            UpdatePhonyAssign (VarExp "i"),
            UpdateIncrement (VarExp "i"),
            UpdateDecrement (VarExp "i"),
            UpdateCall "step" [] [VarExp "i"]
          ]
          @?= [ "for (var i = 1; c; i += 1) { }",
                "for (var i = 1; c; _ = i) { }",
                "for (var i = 1; c; i++) { }",
                "for (var i = 1; c; i--) { }",
                "for (var i = 1; c; step(i)) { }"
              ]
    ]

-- | The remaining module-scope declarations.  See
-- https://www.w3.org/TR/WGSL/#module-scope-declarations.
globalDeclTests :: TestTree
globalDeclTests =
  testGroup
    "other declarations"
    [ testCase "alias" $
        render (GlobalAlias "i16" I32) @?= "alias i16 = i32;",
      testCase "const" $
        map render [GlobalConst "N" Nothing one, GlobalConst "N" (Just I32) one]
          @?= ["const N = 1;", "const N : i32 = 1;"],
      -- A module-scope variable's type is optional, and one in the private
      -- address space may have an initialiser.
      testCase "variable initialiser" $
        map
          render
          [ GlobalVar [] Private Nothing "x" (Just F32) Nothing,
            GlobalVar [] Private Nothing "x" Nothing (Just one),
            GlobalVar [] Private Nothing "x" (Just F32) (Just one)
          ]
          @?= ["var<private> x : f32;", "var<private> x = 1;", "var<private> x : f32 = 1;"],
      testCase "override without a type" $
        render (GlobalOverride [] "k" Nothing (Just one)) @?= "override k = 1;"
    ]

-- | An index may be applied to any expression, and chained.  Swizzles use
-- the same slot as member access, so 'FieldExp' spells them.  See
-- https://www.w3.org/TR/WGSL/#component-reference-expr.
indexTests :: TestTree
indexTests =
  testGroup
    "indexing"
    [ testCase "identifier" $
        render (IndexExp (VarExp "a") (VarExp "i")) @?= "a[i]",
      -- Indexing a nested array needs an index of an index.
      testCase "chained" $
        render (IndexExp (IndexExp (VarExp "a") (VarExp "i")) (VarExp "j"))
          @?= "a[i][j]",
      testCase "call result" $
        render (IndexExp (CallExp "f" [] [VarExp "x"]) (VarExp "i")) @?= "f(x)[i]",
      testCase "through a pointer" $
        render (IndexExp (UnOpExp Indirection (VarExp "p")) (VarExp "i"))
          @?= "(*p)[i]",
      testCase "member" $
        render (IndexExp (FieldExp (VarExp "s") "f") (VarExp "i")) @?= "s.f[i]",
      testCase "swizzle of an index" $
        render (FieldExp (IndexExp (VarExp "a") (VarExp "i")) "xyz") @?= "a[i].xyz",
      testCase "as an assignment target" $
        render
          ( Assign
              (IndexExp (IndexExp (VarExp "a") (VarExp "i")) (VarExp "j"))
              SimpleAssign
              one
          )
          @?= "a[i][j] = 1;"
    ]

tests :: TestTree
tests =
  testGroup
    "Language.WGSL"
    [ testGroup "types" [typeSpellingTests, injectivityTests, pointerTests],
      testGroup
        "memory"
        [ accessModeTests,
          layoutTests,
          matrixLayoutTests,
          opaqueTests,
          arrayLayoutTests,
          structLayoutTests,
          memberLayoutTests,
          hostShareableTests
        ],
      testGroup
        "expressions"
        [operatorTests, intLitTests, floatLitTests, callTests, indexTests],
      testGroup
        "statements"
        [ sequenceTests,
          assignTests,
          returnTests,
          forTests,
          elseIfTests,
          switchTests,
          loopTests,
          assignOpTests,
          localDeclTests,
          forUpdateTests
        ],
      testGroup
        "declarations"
        [ globalVarTests,
          globalDeclTests,
          funDeclTests,
          attributeTests,
          directiveTests,
          moduleTests
        ]
    ]

main :: IO ()
main = defaultMain tests
