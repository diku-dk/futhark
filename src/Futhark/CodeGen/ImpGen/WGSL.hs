module Futhark.CodeGen.ImpGen.WGSL
  ( Ident,
    PrimType (..),
    Typ (..),
    BinOp,
    UnOp,
    Exp (..),
    Stmt (..),
    Attrib (..),
    Param (..),
    Function (..)
  )
where

import Data.Text qualified as T
import Futhark.Util.Pretty

type Ident = T.Text

data PrimType = Bool | Int32 | UInt32 | Float16 | Float32
              | Vec2 PrimType | Vec3 PrimType | Vec4 PrimType

data Typ = Prim PrimType | Named Ident

type BinOp = T.Text
type UnOp = T.Text

data Exp
  = BoolExp Bool
  | IntExp Int
  | FloatExp Double
  | StringExp T.Text
  | VarExp Ident
  | BinOpExp BinOp Exp Exp
  | UnOpExp UnOp Exp
  | CallExp Ident [Exp]
  | IndexExp Ident Exp

data Stmt
  = Skip
  | Seq Stmt Stmt
  | DeclareVar Ident Typ
  | Assign Ident Exp
  | AssignIndex Ident Exp Exp
  | If Exp [Stmt] [Stmt]
  | For Ident Exp Exp Stmt [Stmt]
  | While Exp [Stmt]

data Attrib = Attrib Ident [Exp]
data Param = Param Ident Typ [Attrib]

data Function = Function
  { funName :: Ident,
    funAttribs :: [Attrib],
    funParams :: [Param],
    funBody :: [Stmt]
  }

instance Pretty PrimType where
  pretty Bool = "bool"
  pretty Int32 = "i32"
  pretty UInt32 = "u32"
  pretty Float16 = "f16"
  pretty Float32 = "f32"
  pretty (Vec2 t) = "vec2<" <> pretty t <> ">"
  pretty (Vec3 t) = "vec3<" <> pretty t <> ">"
  pretty (Vec4 t) = "vec4<" <> pretty t <> ">"

instance Pretty Typ where
  pretty (Prim t) = pretty t
  pretty (Named t) = pretty t

instance Pretty Exp where
  pretty (BoolExp x) = pretty x
  pretty (IntExp x) = pretty x
  pretty (FloatExp x) = pretty x
  pretty (StringExp x) = pretty $ show x
  pretty (VarExp x) = pretty x
  pretty (UnOpExp op e) = parens (pretty op <> pretty e)
  pretty (BinOpExp op e1 e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (CallExp f args) = pretty f <> parens (commasep $ map pretty args)
  pretty (IndexExp x i) = pretty x <> brackets (pretty i)

instance Pretty Stmt where
  pretty Skip = ";"
  pretty (Seq s1 s2) = semistack [pretty s1, pretty s2]
  pretty (DeclareVar x t) = "var" <+> pretty x <+> ":" <+> pretty t
  pretty (Assign x e) = pretty x <+> "=" <+> pretty e
  pretty (AssignIndex x i e) =
    pretty x <> brackets (pretty i) <+> "=" <+> pretty e
  pretty (If cond [] []) = "if" <+> pretty cond <+> "{ }"
  pretty (If cond th []) =
    "if" <+> pretty cond <+> "{"
    </> indent 2 (semistack (map pretty th))
    </> "}"
  pretty (If cond [] el) =
    "if" <+> pretty cond <+> "{ }"
    </> "else {"
    </> indent 2 (semistack (map pretty el))
    </> "}"
  pretty (If cond th el) =
    "if" <+> pretty cond <+> "{"
    </> indent 2 (semistack (map pretty th))
    </> "} else {"
    </> indent 2 (semistack (map pretty el))
    </> "}"
  pretty (For x initializer cond upd body) =
    "for" <+> parens ("var" <+> pretty x <+> "=" <+> pretty initializer <> ";"
                     <+> pretty cond <> ";" <+> pretty upd) <+> "{"
    </> indent 2 (semistack (map pretty body))
    </> "}"
  pretty (While cond body) =
    "while" <+> pretty cond <+> "{"
    </> indent 2 (semistack (map pretty body))
    </> "}"

instance Pretty Attrib where
  pretty (Attrib name []) = "@" <> pretty name
  pretty (Attrib name args) =
    "@" <> pretty name <> parens (commasep $ map pretty args)

instance Pretty Param where
  pretty (Param name typ attribs) = stack
    [ hsep (map pretty attribs),
      pretty name <+> ":" <+> pretty typ
    ]

prettyParams :: [Param] -> Doc a
prettyParams [] = "()"
prettyParams params = "(" </> indent 2 (commastack (map pretty params)) </> ")"

instance Pretty Function where
  pretty (Function name attribs params body) = stack
    [ hsep (map pretty attribs),
      "fn" <+> pretty name <> prettyParams params <+> "{",
      indent 2 (semistack (map pretty body)),
      "}"
    ]
