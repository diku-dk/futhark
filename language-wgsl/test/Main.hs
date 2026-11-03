module Main (main) where

import Data.Text qualified as T
import Language.WGSL
import Prettyprinter (Pretty, defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty
import Test.Tasty.HUnit

-- | A convenient literal for tests that only need some expression.
one :: Exp
one = IntExp 1 AbstractInt

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
        render (Pointer Int32 Function Nothing)
          @?= "ptr<function, i32>",
      testCase "with access mode" $
        render (Pointer Int32 Function (Just ReadWrite))
          @?= "ptr<function, i32, read_write>",
      testCase "uniform" $
        render (Pointer Float32 Uniform Nothing)
          @?= "ptr<uniform, f32>",
      testCase "vector store type" $
        render (Pointer (Vec3 Float32) Workgroup (Just Read))
          @?= "ptr<workgroup, vec3<f32>, read>",
      testCase "storage" $
        render (Pointer Int32 Storage (Just Read))
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
-- https://www.w3.org/TR/WGSL/#var-decls.  Note that a declaration without
-- attributes is currently rendered with a leading line break.
varDeclTests :: TestTree
varDeclTests =
  testGroup
    "VarDecl"
    [ testCase "without access mode" $
        render (VarDecl [] Workgroup Nothing "x" (Prim Int32))
          @?= "\nvar<workgroup> x : i32;",
      testCase "with access mode" $
        render (VarDecl [] Storage (Just ReadWrite) "x" (Array Int32 Nothing))
          @?= "\nvar<storage, read_write> x : array<i32>;",
      testCase "private" $
        render (VarDecl [] Private Nothing "x" (Prim Float32))
          @?= "\nvar<private> x : f32;",
      testCase "immediate" $
        render (VarDecl [] Immediate Nothing "x" (Named "S"))
          @?= "\nvar<immediate> x : S;",
      testCase "with attributes" $
        render (VarDecl (bindingAttribs 0 1) Storage (Just Read) "x" (Array Float32 Nothing))
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

-- | An assignment's left-hand side is an assignable expression.  See
-- https://www.w3.org/TR/WGSL/#assignment.
assignTests :: TestTree
assignTests =
  testGroup
    "Assign"
    [ testCase "variable" $
        render (Assign (VarExp "x") (VarExp "y")) @?= "x = y;",
      testCase "dereference" $
        render (Assign (UnOpExp "*" (VarExp "p")) (VarExp "x")) @?= "(*p) = x;",
      testCase "index" $
        render (Assign (IndexExp "a" (IntExp 0 AbstractInt)) (IntExp 1 AbstractInt))
          @?= "a[0] = 1;",
      testCase "member" $
        render (Assign (FieldExp (VarExp "s") "f") (VarExp "y")) @?= "s.f = y;"
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
        render (FunDecl "f" [] [Param "a" (Prim Int32) []] Nothing (Call "g" [VarExp "a"]))
          @?= "\nfn f(\n  a : i32\n) {\n  g(a);\n}",
      testCase "with return type" $
        render (FunDecl "f" [] [] (Just ([], Prim Int32)) (Return (Just (IntExp 1 AbstractInt))))
          @?= "\nfn f() -> i32 {\n  return 1;\n}",
      testCase "with return attributes" $
        render
          ( FunDecl
              "vs"
              [Attrib "vertex" []]
              []
              (Just ([Attrib "builtin" [VarExp "position"]], Prim (Vec4 Float32)))
              (Return (Just (VarExp "p")))
          )
          @?= "@vertex\nfn vs() -> @builtin(position) vec4<f32> {\n  return p;\n}"
    ]

-- | Each statement carries its own terminator, and a compound statement takes
-- none.  See https://www.w3.org/TR/WGSL/#statements.
seqTests :: TestTree
seqTests =
  testGroup
    "Seq"
    [ testCase "terminators" $
        render (stmts [Call "f" [], Let "x" one, Call "g" []])
          @?= "f();\nlet x = 1;\ng();",
      -- The separator used to end up inside the comment.
      testCase "comment" $
        render (stmts [Call "f" [], Comment "note", Call "g" []])
          @?= "f();\n// note\ng();",
      testCase "compound statement" $
        render (stmts [If (VarExp "c") (Call "f" []) Skip, Call "g" []])
          @?= "if c {\n  f();\n}\ng();"
    ]

-- | A for loop's update is an assignment or a call, and is not terminated.
-- See https://www.w3.org/TR/WGSL/#for-statement.
forTests :: TestTree
forTests =
  testGroup
    "For"
    [ testCase "assignment update" $
        render (For "i" one (BinOpExp "<" (VarExp "i") one) (UpdateAssign (VarExp "i") one) (Call "f" []))
          @?= "for (var i = 1; (i < 1); i = 1) {\n  f();\n}",
      testCase "call update" $
        render (For "i" one (VarExp "c") (UpdateCall "step" [VarExp "i"]) Skip)
          @?= "for (var i = 1; c; step(i)) {\n  ;\n}"
    ]

tests :: TestTree
tests =
  testGroup
    "Language.WGSL"
    [ testGroup "types" [pointerTests],
      testGroup "memory" [accessModeTests],
      testGroup "expressions" [intLitTests, floatLitTests],
      testGroup "statements" [seqTests, assignTests, returnTests, forTests],
      testGroup "declarations" [varDeclTests, funDeclTests]
    ]

main :: IO ()
main = defaultMain tests
