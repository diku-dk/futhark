{-# LANGUAGE PostfixOperators #-}


module Futhark.CodeGen.Backends.GenericCSharp.AST
  ( CSExp(..)
  , CSType(..)
  , CSComp(..)
  , CSPrim(..)
  , CSInt(..)
  , CSUInt(..)
  , CSFloat(..)
  , CSIdx (..)
  , CSArg (..)
  , CSStmt(..)
  , module Language.Futhark.Core
  , CSProg(..)
  , CSExcept(..)
  , CSFunDef(..)
  , CSFunDefArg
  , CSClassDef(..)
  , CSConstructorDef(..)
  )
  where

import Language.Futhark.Core
import Data.List(intersperse)
import Futhark.Util.Pretty
import Language.C.Quote.OpenCL()
import qualified Language.C.Syntax as C

data MemT = Pointer
          deriving (Eq, Show)

data ArgMemType = ArgOut
                | ArgRef
                deriving (Eq, Show)

instance Pretty ArgMemType where
  ppr ArgOut = text "out"
  ppr ArgRef = text "ref"

instance Pretty CSComp where
  ppr (ArrayT t) = ppr t <> text "[]"
  ppr (TupleT ts) = parens(commasep $ map ppr ts)
  ppr (SystemTupleT ts) = text "Tuple" <> angles(commasep $ map ppr ts)

data CSInt = Int8T
           | Int16T
           | Int32T
           | Int64T
           deriving (Eq, Show)

data CSUInt = UInt8T
            | UInt16T
            | UInt32T
            | UInt64T
            deriving (Eq, Show)

data CSFloat = FloatT
             | DoubleT
             deriving (Eq, Show)

data CSType = Composite CSComp
            | PointerT CSType
            | Primitive CSPrim
            | MemoryT String
            | CLMemoryT String
            | CustomT String
            | StaticT CSType
            | OutT CSType
            | RefT CSType
            | VoidT
            deriving (Eq, Show)

data CSComp = ArrayT CSType
            | TupleT [CSType]
            | SystemTupleT [CSType]
            deriving (Eq, Show)

data CSPrim = CSInt CSInt
            | CSUInt CSUInt
            | CSFloat CSFloat
            | BoolT
            | ByteT
            | StringT
            | IntPtrT
            deriving (Eq, Show)

instance Pretty CSType where
  ppr (Composite t) = ppr t
  ppr (PointerT t) = ppr t <> text "*"
  ppr (Primitive t) = ppr t
  ppr (CustomT t) = text t
  ppr (MemoryT _) = text "byte[]"
  ppr (CLMemoryT _) = text "CLMemoryHandle"
  ppr (StaticT t) = text "static" <+> ppr t
  ppr (OutT t) = text "out" <+> ppr t
  ppr (RefT t) = text "ref" <+> ppr t
  ppr VoidT = text "void"

instance Pretty CSPrim where
  ppr BoolT = text "bool"
  ppr ByteT = text "byte"
  ppr (CSInt t) = ppr t
  ppr (CSUInt t) = ppr t
  ppr (CSFloat t) = ppr t
  ppr StringT = text "string"
  ppr IntPtrT = text "IntPtr"

instance Pretty CSInt where
  ppr Int8T = text "sbyte"
  ppr Int16T = text "short"
  ppr Int32T = text "int"
  ppr Int64T = text "long"

instance Pretty CSUInt where
  ppr UInt8T = text "byte"
  ppr UInt16T = text "ushort"
  ppr UInt32T = text "uint"
  ppr UInt64T = text "ulong"

instance Pretty CSFloat where
  ppr FloatT = text "float"
  ppr DoubleT = text "double"

data UnOp = Not -- ^ Boolean negation.
          | Complement -- ^ Bitwise complement.
          | Negate -- ^ Numerical negation.
          | Abs -- ^ Absolute/numerical value.
            deriving (Eq, Show)

data CSExp = Integer Integer
           | Bool Bool
           | Float Double
           | String String
           | RawStringLiteral String
           | Var String
           | Addr CSExp
           | Ref CSExp
           | Out CSExp
           | Deref String
           | BinOp String CSExp CSExp
           | PreUnOp String CSExp
           | PostUnOp String CSExp
           | Ternary CSExp CSExp CSExp
           | Cond CSExp CSExp CSExp
           | Index CSExp CSIdx
           | Pair CSExp CSExp
           | Call CSExp [CSArg]
           | CallMethod CSExp CSExp [CSArg]
           | CreateObject CSExp [CSArg]
           | CreateArray CSType [CSExp]
           | CreateSystemTuple [CSExp]
           | AllocArray CSType CSExp
           | Cast CSType CSExp
           | Tuple [CSExp]
           | Array [CSExp]
           | Field CSExp String
           | Lambda CSExp [CSStmt]
           | Collection String [CSExp]
           | This CSExp
           | Null
           -- | Dict [(CSExp, CSExp)]
           deriving (Eq, Show)

instance Pretty CSExp where
  ppr (Integer x) = ppr x
  ppr (Float x)
    | isInfinite x = text $ if x > 0 then "Double.PositiveInfinity" else "Double.NegativeInfinity"
    | otherwise = ppr x
  ppr (Bool True) = text "true"
  ppr (Bool False) = text "false"
  ppr (String x) = text $ show x
  ppr (RawStringLiteral s) = text "@\"" <> text s <> text "\""
  ppr (Var n) = text $ map (\x -> if x == '\'' then 'm' else x) n
  ppr (Addr e) =  text "&" <> ppr e
  ppr (Ref e) =  text "ref" <+> ppr e
  ppr (Out e) =  text "out" <+> ppr e
  ppr (Deref n) =  text "*" <> text (map (\x -> if x == '\'' then 'm' else x) n)
  ppr (BinOp s e1 e2) = parens(ppr e1 <+> text s <+> ppr e2)
  ppr (PreUnOp s e) = text s <> parens (ppr e)
  ppr (PostUnOp s e) = parens (ppr e) <> text s
  ppr (Ternary b e1 e2) = ppr b <+> text "?" <+> ppr e1 <+> colon <+> ppr e2
  ppr (Cond e1 e2 e3) = text "if" <+> parens(ppr e1) <> braces(ppr e2) <+> text "else" <> braces(ppr e3)
  ppr (Cast bt src) = parens(ppr bt) <+> ppr src
  ppr (Index src (IdxExp idx)) = ppr src <> brackets(ppr idx)
  ppr (Index src (IdxRange from to)) = text "MySlice" <> parens(commasep $ map ppr [src, from, to])
  ppr (Pair e1 e2) = braces(ppr e1 <> comma <> ppr e2)
  ppr (Call fun args) = ppr fun <> parens(commasep $ map ppr args)
  ppr (CallMethod obj method args) = ppr obj <> dot <> ppr method <> parens(commasep $ map ppr args)
  ppr (CreateObject className args) = text "new" <+> ppr className <> parens(commasep $ map ppr args)
  ppr (CreateArray t vs) = text "new" <+> ppr t <> text "[]" <+> braces(commasep $ map ppr vs)
  ppr (CreateSystemTuple exps) = text "Tuple.Create" <> parens(commasep $ map ppr exps)
  ppr (Tuple exps) = parens(commasep $ map ppr exps)
  ppr (Array exps) = braces(commasep $ map ppr exps) -- uhoh is this right?
  ppr (Field obj field) = ppr obj <> dot <> text field
  ppr (Lambda expr [Exp e]) = ppr expr <+> text "=>" <+> ppr e
  ppr (Lambda expr stmts) = ppr expr <+> text "=>" <+> braces(stack $ map ppr stmts)
  ppr (Collection collection exps) = text "new" <+> text collection <> braces(commasep $ map ppr exps)
  ppr (This e) = text "this" <> dot <> ppr e
  ppr Null = text "null"
  ppr (AllocArray t len) = text "new" <+> ppr t <> lbracket <> ppr len <> rbracket
  --ppr (Dict exps) = undefined


data CSIdx = IdxRange CSExp CSExp
           | IdxExp CSExp
               deriving (Eq, Show)

data CSArg = ArgKeyword String CSArg -- please don't assign multiple keywords with the same argument
           | Arg (Maybe ArgMemType) CSExp
           deriving (Eq, Show)

instance Pretty CSArg where
  ppr (ArgKeyword kw arg) = text kw <> colon <+> ppr arg
  ppr (Arg (Just mt) arg) = ppr mt <+> ppr arg
  ppr (Arg Nothing arg) = ppr arg

data CSStmt = If CSExp [CSStmt] [CSStmt]
            | Try [CSStmt] [CSExcept]
            | While CSExp [CSStmt]
            | For String CSExp [CSStmt]
            | ForEach String CSExp [CSStmt]
            | UsingWith CSStmt [CSStmt]
            | Unsafe [CSStmt]
            | Fixed CSStmt [CSStmt]
            | Assign CSExp CSExp
            | Reassign CSExp CSExp
            | AssignOp String CSExp CSExp
            | AssignTyped CSType CSExp (Maybe CSExp)

            | Comment String [CSStmt]
            | Assert CSExp [CSExp]
            | Throw CSExp
            | Exp CSExp
            | Return CSExp
            | Pass
              -- Definition-like statements.
            | Using (Maybe String) String
            | StaticFunDef CSFunDef
            | PublicFunDef CSFunDef
            | FunDef CSFunDef
            | Namespace String [CSStmt]
            | ClassDef CSClassDef
            | ConstructorDef CSConstructorDef
            | StructDef String [(CSType, String)]

            | CSDecl C.BlockItem

              -- Some arbitrary string of CS code.
            | Escape String
                deriving (Eq, Show)

instance Pretty CSStmt where
  ppr (If cond tbranch []) =
    text "if" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr tbranch) </>
    rbrace

  ppr (If cond tbranch fbranch) =
    text "if" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr tbranch) </>
    rbrace </>
    text "else" </>
    lbrace </>
    indent 4 (stack $ map ppr fbranch) </>
    rbrace

  ppr (Try stmts excepts) =
    text "try" </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace </>
    stack (map ppr excepts)

  ppr (While cond body) =
    text "while" <+> parens(ppr cond) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace

  ppr (For i what body) =
    text "for" <+> parens(initialize <> limit <> inc) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace
    where initialize = text "int" <+> text i <+> text "= 0" <+> semi
          limit = text i <+> langle <+> ppr what <+> semi
          inc = text i <> text "++"

  ppr (ForEach i what body) =
    text "foreach" <+> parens initialize </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace
    where initialize = text "var" <+> text i <+> text "in " <+> ppr what

  ppr (Using (Just as) from) =
    text "using" <+> text as <+> text "=" <+> text from <> semi

  ppr (Using Nothing from) =
    text "using" <+> text from <> semi

  ppr (Unsafe stmts) =
    text "unsafe" </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace

  ppr (Fixed (AssignTyped _ ptr e) stmts) =
    text "fixed" <+> parens(text "void*" <+> ppr ptr <+> text "=" <+> ppr e) </>
    lbrace </>
    indent 4 (stack $ map ppr stmts) </>
    rbrace

  ppr (Fixed _ _) = undefined

  ppr (UsingWith assignment body) =
    text "using" <+> parens(ppr assignment) </>
    lbrace </>
    indent 4 (stack $ map ppr body) </>
    rbrace

  ppr (Assign e1 e2) = text "var" <+> ppr e1 <+> equals <+> ppr e2 <> semi
  ppr (Reassign e1 e2) = ppr e1 <+> equals <+> ppr e2 <> semi
  ppr (AssignTyped t e1 Nothing) = ppr t <+> ppr e1 <> semi
  ppr (AssignTyped t e1 (Just e2)) = ppr t <+> ppr e1 <+> equals <+> ppr e2 <> semi

  ppr (AssignOp op e1 e2) = ppr e1 <+> text (op ++ "=") <+> ppr e2 <> semi

  ppr (Comment s body) = text "//" <> text s </> stack (map ppr body)

  ppr (Assert e []) =
    text "futharkAssert" <> parens(ppr e) <> semi

  ppr (Assert e exps) =
    let exps' = stack $ intersperse (text ",") $ map ppr exps
        formattedString = text "String.Format" <> parens exps'
    in text "futharkAssert" <> parens(ppr e <> text "," <+> formattedString) <> semi

  ppr (Throw e) = text "throw" <+> ppr e <> semi

  ppr (Exp e) = ppr e <> semi

  ppr (Return e) = text "return" <+> ppr e <> semi

  ppr (ClassDef d) = ppr d

  ppr (StaticFunDef d) = text "static" <+> ppr d

  ppr (PublicFunDef d) = text "public" <+> ppr d

  ppr (FunDef d) = ppr d

  ppr (ConstructorDef d) = ppr d

  ppr (StructDef name assignments) = text "public struct" <+> text name <> braces(stack $ map (\(tp,field) -> text "public" <+> ppr tp <+> text field <> semi) assignments)

  ppr (Namespace name csstms) = text "namespace" <+> text name <> braces(stack $ map ppr csstms)

  ppr (CSDecl decl) = text $ show decl

  ppr (Escape s) = stack $ map text $ lines s

  ppr Pass = empty

instance Pretty CSFunDef where
  ppr (Def fname retType args stmts) =
    ppr retType <+> text fname <> parens( commasep(map ppr' args) ) </>
    lbrace </>
    indent 4 (stack (map ppr stmts)) </>
    rbrace
    where ppr' (tp, var) = ppr tp <+> text var

instance Pretty CSClassDef where
  ppr (Class cname body) =
    text "class" <+> text cname </>
    lbrace </>
    indent 4 (stack (map ppr body)) </>
    rbrace

  ppr (PublicClass cname body) =
    text "public" <+> text "class" <+> text cname </>
    lbrace </>
    indent 4 (stack (map ppr body)) </>
    rbrace

instance Pretty CSConstructorDef where
  ppr (ClassConstructor cname params body) =
    text "public" <+> text cname <> parens(commasep $ map ppr' params) </>
    lbrace </>
    indent 4 (stack (map ppr body)) </>
    rbrace
    where ppr' (tp, var) = ppr tp <+> text var

instance Pretty CSExcept where
  ppr (Catch csexp stmts) =
    text "catch" <+> parens(ppr csexp <+> text "e") </>
    lbrace </>
    indent 4 (stack (map ppr stmts)) </>
    rbrace

data CSExcept = Catch CSExp [CSStmt]
              deriving (Eq, Show)

type CSFunDefArg = (CSType, String)
data CSFunDef = Def String CSType [CSFunDefArg] [CSStmt]
                  deriving (Eq, Show)

data CSClassDef = Class String [CSStmt]
                | PublicClass String [CSStmt]
                deriving (Eq, Show)

data CSConstructorDef = ClassConstructor String [CSFunDefArg] [CSStmt]
                deriving (Eq, Show)

newtype CSProg = CSProg [CSStmt]
                   deriving (Eq, Show)

instance Pretty CSProg where
  ppr (CSProg stms) = stack (map ppr stms)
