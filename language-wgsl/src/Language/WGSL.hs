module Language.WGSL
  ( Ident,
    PrimType (..),
    hsLayout,
    structLayout,
    Typ (..),
    BinOp,
    UnOp,
    Exp (..),
    Stmt (..),
    Attrib (..),
    Param (..),
    Function (..),
    Field (..),
    Struct (..),
    Declaration (..),
    AccessMode (..),
    AddressSpace (..),
    stmts,
    bindingAttribs,
    to_i32,
    prettyDecls,
  )
where

import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Prettyprinter

type Ident = T.Text

data PrimType
  = Bool
  | Int32
  | UInt32
  | Float16
  | Float32
  | Vec2 PrimType
  | Vec3 PrimType
  | Vec4 PrimType
  | Atomic PrimType
  deriving (Show)

-- | AlignOf and SizeOf of host-shareable primitive types.
--
-- See https://www.w3.org/TR/WGSL/#alignment-and-size.
hsLayout :: PrimType -> Maybe (Int, Int)
hsLayout Bool = Nothing
hsLayout Int32 = Just (4, 4)
hsLayout UInt32 = Just (4, 4)
hsLayout Float16 = Just (2, 2)
hsLayout Float32 = Just (4, 4)
hsLayout (Vec2 t) =
  (\(a, s) -> (a * 2, s * 2)) <$> hsLayout t
hsLayout (Vec3 t) =
  (\(a, s) -> (a * 4, s * 3)) <$> hsLayout t
hsLayout (Vec4 t) =
  (\(a, s) -> (a * 4, s * 4)) <$> hsLayout t
hsLayout (Atomic t) = hsLayout t

-- | Field offsets, AlignOf, and SizeOf of a host-shareable struct type with the
-- given fields.
structLayout :: [PrimType] -> Maybe ([Int], Int, Int)
structLayout [] = Nothing
structLayout fields = do
  fieldLayouts <- mapM hsLayout fields
  let (fieldAligns, fieldSizes) = unzip fieldLayouts
  let structAlign = maximum fieldAligns
  let fieldOffsets =
        scanl
          (\prev_off (al, prev_sz) -> roundUp al (prev_off + prev_sz))
          0
          (zip (tail fieldAligns) fieldSizes)
  let structSize = roundUp structAlign (last fieldOffsets + last fieldSizes)
  pure (fieldOffsets, structAlign, structSize)
  where
    roundUp k n = ceiling ((fromIntegral n :: Double) / fromIntegral k) * k

data Typ = Prim PrimType | Array PrimType (Maybe Exp) | Named Ident | Pointer PrimType AddressSpace (Maybe AccessMode)

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
  | FieldExp Exp Ident

data Stmt
  = Skip
  | Comment T.Text
  | Seq Stmt Stmt
  | Let Ident Exp
  | DeclareVar Ident Typ
  | Assign Ident Exp
  | AssignIndex Ident Exp Exp
  | If Exp Stmt Stmt
  | For Ident Exp Exp Stmt Stmt
  | While Exp Stmt
  | Call Ident [Exp]

data Attrib = Attrib Ident [Exp]

data Param = Param Ident Typ [Attrib]

data Function = Function
  { funName :: Ident,
    funAttribs :: [Attrib],
    funParams :: [Param],
    funOutput :: [Param],
    funBody :: Stmt
  }

data Field = Field Ident Typ

data Struct = Struct Ident [Field]

data AccessMode = ReadOnly | ReadWrite

-- Uniform buffers are always read-only.
data AddressSpace = Storage AccessMode | Uniform | Workgroup | FunctionSpace

data Declaration
  = FunDecl Function
  | StructDecl Struct
  | VarDecl [Attrib] AddressSpace Ident Typ
  | OverrideDecl Ident Typ (Maybe Exp)

stmts :: [Stmt] -> Stmt
stmts [] = Skip
stmts [s] = s
stmts (s : ss) = Seq s (stmts ss)

bindingAttribs :: Int -> Int -> [Attrib]
bindingAttribs grp binding =
  [Attrib "group" [IntExp grp], Attrib "binding" [IntExp binding]]

to_i32 :: Exp -> Exp
to_i32 e = CallExp "bitcast<i32>" [e]

--- Prettyprinting definitions

-- | Separate with commas.
commasep :: [Doc a] -> Doc a
commasep = hsep . punctuate comma

-- | Like commasep, but a newline after every comma.
commastack :: [Doc a] -> Doc a
commastack = align . vsep . punctuate comma

-- | Separate with semicolons and newlines.
semistack :: [Doc a] -> Doc a
semistack = align . vsep . punctuate semi

-- | Separate with linebreaks.
stack :: [Doc a] -> Doc a
stack = align . mconcat . punctuate line

(</>) :: Doc a -> Doc a -> Doc a
a </> b = a <> line <> b

instance Pretty PrimType where
  pretty Bool = "bool"
  pretty Int32 = "i32"
  pretty UInt32 = "u32"
  pretty Float16 = "f16"
  pretty Float32 = "f32"
  pretty (Vec2 t) = "vec2<" <> pretty t <> ">"
  pretty (Vec3 t) = "vec3<" <> pretty t <> ">"
  pretty (Vec4 t) = "vec4<" <> pretty t <> ">"
  pretty (Atomic t) = "atomic<" <> pretty t <> ">"

instance Pretty Typ where
  pretty (Prim t) = pretty t
  pretty (Array t Nothing) = "array<" <> pretty t <> ">"
  pretty (Array t sz) = "array<" <> pretty t <> ", " <> pretty sz <> ">"
  pretty (Named t) = pretty t
  pretty (Pointer t as am) = "ptr<" <> pretty as <> ", " <> pretty t <> maybe "" pretty am <> ">"

instance Pretty Exp where
  pretty (BoolExp True) = "true"
  pretty (BoolExp False) = "false"
  pretty (IntExp x) = pretty x
  pretty (FloatExp x) = pretty x
  pretty (StringExp x) = pretty $ show x
  pretty (VarExp x) = pretty x
  pretty (UnOpExp op e) = parens (pretty op <> pretty e)
  pretty (BinOpExp op e1 e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (CallExp f args) = pretty f <> parens (commasep $ map pretty args)
  pretty (IndexExp x i) = pretty x <> brackets (pretty i)
  pretty (FieldExp x y) = pretty x <> "." <> pretty y

instance Pretty Stmt where
  pretty Skip = ";"
  pretty (Comment c) = vsep (map ("//" <+>) (pretty <$> T.lines c))
  pretty (Seq s1 s2) = semistack [pretty s1, pretty s2]
  pretty (Let x e) = "let" <+> pretty x <+> "=" <+> pretty e
  pretty (DeclareVar x t) = "var" <+> pretty x <+> ":" <+> pretty t
  pretty (Assign x e) = pretty x <+> "=" <+> pretty e
  pretty (AssignIndex x i e) =
    pretty x <> brackets (pretty i) <+> "=" <+> pretty e
  pretty (If cond Skip Skip) = "if" <+> pretty cond <+> "{ }"
  pretty (If cond th Skip) =
    "if"
      <+> pretty cond
      <+> "{"
      </> indent 2 (pretty th) <> ";"
      </> "}"
  pretty (If cond Skip el) =
    "if"
      <+> pretty cond
      <+> "{ }"
      </> "else {"
      </> indent 2 (pretty el) <> ";"
      </> "}"
  pretty (If cond th el) =
    "if"
      <+> pretty cond
      <+> "{"
      </> indent 2 (pretty th) <> ";"
      </> "} else {"
      </> indent 2 (pretty el) <> ";"
      </> "}"
  pretty (For x initializer cond upd body) =
    "for"
      <+> parens
        ( "var"
            <+> pretty x
            <+> "="
            <+> pretty initializer
              <> ";"
            <+> pretty cond
              <> ";"
            <+> pretty upd
        )
      <+> "{"
      </> indent 2 (pretty body) <> ";"
      </> "}"
  pretty (While cond body) =
    "while"
      <+> pretty cond
      <+> "{"
      </> indent 2 (pretty body) <> ";"
      </> "}"
  pretty (Call f args) = pretty f <> parens (commasep $ map pretty args)

instance Pretty Attrib where
  pretty (Attrib name []) = "@" <> pretty name
  pretty (Attrib name args) =
    "@" <> pretty name <> parens (commasep $ map pretty args)

instance Pretty Param where
  pretty (Param name typ attribs)
    | null attribs = pretty name <+> ":" <+> pretty typ
    | otherwise =
        stack
          [ hsep (map pretty attribs),
            pretty name <+> ":" <+> pretty typ
          ]

prettyParams :: [Param] -> Doc a
prettyParams [] = "()"
prettyParams params = "(" </> indent 2 (commastack (map pretty params)) </> ")"

prettyAssignOutParams :: [Param] -> Doc a
prettyAssignOutParams [] = ""
prettyAssignOutParams params = stack (map prettyAssign params)
  where
    prettyAssign (Param name _ _) =
      indent 2 "*" <> pretty name <> " = " <> pretty (T.stripSuffix "_out" name) <> ";"

instance Pretty Function where
  pretty (Function name attribs in_params out_params body) = do
    stack $ hsep (map pretty attribs) : function
    where
      funBody = indent 2 (pretty body) <> ";"
      funDecls = let local_decls = map (\(Param v typ _) -> case typ of
                              Pointer t _ _ -> DeclareVar (fromMaybe v (T.stripSuffix "_out" v)) (Prim t)
                              _             -> error "Can only return primitive types!") out_params
                 in stack (map (\decl -> indent 2 (pretty decl) <> ";") local_decls)
      function = case out_params of
        [] ->
          ["fn" <+> pretty name <> prettyParams in_params <+> "{", funBody, "}"]
        [Param ret_id (Pointer t _ _) _] ->
          ["fn" <+> pretty name <> prettyParams in_params <+> "->" <+> pretty t <+> "{",
          funDecls,
          funBody,
          indent 2 "return " <> pretty (T.stripSuffix "_out" ret_id) <> ";", 
          "}"]
        _ ->
          ["fn" <+> pretty name <> prettyParams (in_params ++ out_params) <+> "{", 
           funDecls,
           funBody, 
           prettyAssignOutParams out_params,
           "}"]

instance Pretty Field where
  pretty (Field name typ) = pretty name <+> ":" <+> pretty typ

instance Pretty Struct where
  pretty (Struct name fields) =
    "struct"
      <+> pretty name
      <+> "{"
      </> indent 2 (commastack (map pretty fields))
      </> "}"

instance Pretty AccessMode where
  pretty ReadOnly = "read"
  pretty ReadWrite = "read_write"

instance Pretty AddressSpace where
  pretty (Storage am) = "storage" <> "," <> pretty am
  pretty Uniform = "uniform"
  pretty Workgroup = "workgroup"
  pretty FunctionSpace = "function"

instance Pretty Declaration where
  pretty (FunDecl fun) = pretty fun
  pretty (StructDecl struct) = pretty struct
  pretty (VarDecl attribs as name typ) =
    hsep (map pretty attribs)
      </> "var<"
        <> pretty as
        <> ">"
      <+> pretty name
      <+> ":"
      <+> pretty typ
        <> ";"
  pretty (OverrideDecl name typ Nothing) =
    "override" <+> pretty name <+> ":" <+> pretty typ <> ";"
  pretty (OverrideDecl name typ (Just initial)) =
    "override"
      <+> pretty name
      <+> ":"
      <+> pretty typ
      <+> "="
      <+> pretty initial
        <> ";"

prettyDecls :: [Declaration] -> Doc a
prettyDecls decls = stack (map pretty decls)
