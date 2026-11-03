-- | Basic WGSL definitions, including AST definitions. These are intended to
-- correspond closely with the terminology and structure of the WGSL
-- specification, but it is not machine-generated.
--
-- The definitions here are abstract in the sense that they cannot represent
-- syntactic details of a WGSL program, e.g., whether an integer literal is
-- written hexadecimally. This makes these definitions less useful for parsing,
-- but fine for code generation. I would not be opposed to adding a concrete
-- syntax module in the future.
module Language.WGSL
  ( Ident,
    PrimType (..),
    hsLayout,
    structLayout,
    Type (..),
    BinOp,
    UnOp,
    IntLitType (..),
    FloatLitType (..),
    Exp (..),
    intExp,
    floatExp,
    Stmt (..),
    Attrib (..),
    Param (..),
    FunDecl (..),
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

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Prettyprinter

-- | A WGSL identifier. We assume (but do not check) that these are valid WGSL
-- identifiers. The printer will certainly produce invalid WGSL if this
-- constraint is violated.
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
          (zip (drop 1 fieldAligns) fieldSizes)
  let structSize = roundUp structAlign (last fieldOffsets + last fieldSizes)
  pure (fieldOffsets, structAlign, structSize)
  where
    roundUp k n = ceiling ((fromIntegral n :: Double) / fromIntegral k) * k

data Type
  = Prim PrimType
  | Array PrimType (Maybe Exp)
  | Named Ident
  | Pointer PrimType AddressSpace (Maybe AccessMode)

type BinOp = T.Text

type UnOp = T.Text

-- | The type denoted by an integer literal, which is determined by its
-- suffix.  See https://www.w3.org/TR/WGSL/#numeric-literals.
data IntLitType
  = -- | No suffix: the literal denotes an @AbstractInt@.
    AbstractInt
  | -- | The @i@ suffix.
    I32
  | -- | The @u@ suffix.
    U32
  deriving (Eq, Ord, Show)

-- | The type denoted by a floating point literal, which is determined by its
-- suffix.  See https://www.w3.org/TR/WGSL/#numeric-literals.
data FloatLitType
  = -- | No suffix: the literal denotes an @AbstractFloat@.
    AbstractFloat
  | -- | The @f@ suffix.
    F32
  | -- | The @h@ suffix.  Using it requires the @enable f16;@ directive.
    F16
  deriving (Eq, Ord, Show)

data Exp
  = BoolExp Bool
  | -- | An integer literal.  Prefer 'intExp', which checks that the value is
    -- representable.
    IntExp Int IntLitType
  | -- | A floating point literal.  Prefer 'floatExp', which checks that the
    -- value is representable.
    FloatExp Double FloatLitType
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
  | DeclareVar Ident Type
  | Assign Ident Exp
  | AssignIndex Ident Exp Exp
  | If Exp Stmt Stmt
  | For Ident Exp Exp Stmt Stmt
  | While Exp Stmt
  | Call Ident [Exp]

data Attrib = Attrib Ident [Exp]

data Param = Param Ident Type [Attrib]

-- | A function declaration.  See
-- https://www.w3.org/TR/WGSL/#function-declaration-sec.
data FunDecl = FunDecl
  { funName :: Ident,
    funAttribs :: [Attrib],
    funParams :: [Param],
    funOutput :: [Param],
    funBody :: Stmt
  }

data Field = Field Ident Type

data Struct = Struct Ident [Field]

-- | The access modes supported by a memory view.  See
-- https://www.w3.org/TR/WGSL/#memory-access-mode.
data AccessMode = Read | Write | ReadWrite
  deriving (Eq, Ord, Show)

-- | The address spaces that can be written in WGSL source.  The @handle@
-- address space is never written, so it is not represented.  Only @Storage@
-- supports more than one access mode.  See
-- https://www.w3.org/TR/WGSL/#address-spaces.
data AddressSpace
  = Function
  | Private
  | Workgroup
  | Uniform
  | Storage
  | Immediate
  deriving (Eq, Ord, Show)

-- | A module-scope declaration.  See
-- https://www.w3.org/TR/WGSL/#module-scope-declarations.
data Declaration
  = FunctionDecl FunDecl
  | StructDecl Struct
  | -- | The access mode may only be given along with the address space, and
    -- defaults to the one of the address space when omitted.
    VarDecl [Attrib] AddressSpace (Maybe AccessMode) Ident Type
  | OverrideDecl Ident Type (Maybe Exp)

stmts :: [Stmt] -> Stmt
stmts [] = Skip
stmts [s] = s
stmts (s : ss) = Seq s (stmts ss)

bindingAttribs :: Int -> Int -> [Attrib]
bindingAttribs grp binding =
  [ Attrib "group" [IntExp grp AbstractInt],
    Attrib "binding" [IntExp binding AbstractInt]
  ]

-- | Construct an integer literal, or 'Nothing' if the value cannot be denoted
-- by a literal of the given type.
intExp :: Int -> IntLitType -> Maybe Exp
intExp v t
  | representable = Just $ IntExp v t
  | otherwise = Nothing
  where
    representable = case t of
      AbstractInt -> magnitude <= 2 ^ (63 :: Int) - 1
      I32 -> magnitude <= 2 ^ (31 :: Int) - 1
      U32 -> toInteger v >= 0 && toInteger v <= 2 ^ (32 :: Int) - 1
    magnitude = abs $ toInteger v

-- | Construct a floating point literal, or 'Nothing' if the value cannot be
-- denoted by a literal of the given type. WGSL has no literal for infinities or
-- NaN, and the magnitude must not exceed the largest finite value of the type.
floatExp :: Double -> FloatLitType -> Maybe Exp
floatExp v t
  | isNaN v || isInfinite v = Nothing
  | maybe False (abs v >) largestFinite = Nothing
  | otherwise = Just $ FloatExp v t
  where
    largestFinite = case t of
      -- Any finite Double is an AbstractFloat.
      AbstractFloat -> Nothing
      F32 -> Just 3.40282346638528859812e38
      F16 -> Just 65504

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

instance Pretty Type where
  pretty (Prim t) = pretty t
  pretty (Array t Nothing) = "array<" <> pretty t <> ">"
  pretty (Array t sz) = "array<" <> pretty t <> ", " <> pretty sz <> ">"
  pretty (Named t) = pretty t
  pretty (Pointer t as am) = "ptr<" <> pretty as <> ", " <> pretty t <> maybe "" ((", " <>) . pretty) am <> ">"

intSuffix :: IntLitType -> Doc a
intSuffix AbstractInt = mempty
intSuffix I32 = "i"
intSuffix U32 = "u"

floatSuffix :: FloatLitType -> Doc a
floatSuffix AbstractFloat = mempty
floatSuffix F32 = "f"
floatSuffix F16 = "h"

instance Pretty Exp where
  pretty (BoolExp True) = "true"
  pretty (BoolExp False) = "false"
  pretty (IntExp x t) = pretty x <> intSuffix t
  pretty (FloatExp x t) = pretty x <> floatSuffix t
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
        </> indent 2 (pretty th)
      <> ";"
        </> "}"
  pretty (If cond Skip el) =
    "if"
      <+> pretty cond
      <+> "{ }"
        </> "else {"
        </> indent 2 (pretty el)
      <> ";"
        </> "}"
  pretty (If cond th el) =
    "if"
      <+> pretty cond
      <+> "{"
        </> indent 2 (pretty th)
      <> ";"
        </> "} else {"
        </> indent 2 (pretty el)
      <> ";"
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
        </> indent 2 (pretty body)
      <> ";"
        </> "}"
  pretty (While cond body) =
    "while"
      <+> pretty cond
      <+> "{"
        </> indent 2 (pretty body)
      <> ";"
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

instance Pretty FunDecl where
  pretty (FunDecl name attribs in_params out_params body) = do
    stack $ hsep (map pretty attribs) : function
    where
      funBody = indent 2 (pretty body) <> ";"
      funDecls =
        let local_decls =
              map
                ( \(Param v typ _) -> case typ of
                    Pointer t _ _ -> DeclareVar (fromMaybe v (T.stripSuffix "_out" v)) (Prim t)
                    _ -> error "Can only return primitive types!"
                )
                out_params
         in stack (map (\decl -> indent 2 (pretty decl) <> ";") local_decls)
      function = case out_params of
        [] ->
          ["fn" <+> pretty name <> prettyParams in_params <+> "{", funBody, "}"]
        [Param ret_id (Pointer t _ _) _] ->
          [ "fn" <+> pretty name <> prettyParams in_params <+> "->" <+> pretty t <+> "{",
            funDecls,
            funBody,
            indent 2 "return " <> pretty (T.stripSuffix "_out" ret_id) <> ";",
            "}"
          ]
        _ ->
          [ "fn" <+> pretty name <> prettyParams (in_params ++ out_params) <+> "{",
            funDecls,
            funBody,
            prettyAssignOutParams out_params,
            "}"
          ]

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
  pretty Read = "read"
  pretty Write = "write"
  pretty ReadWrite = "read_write"

instance Pretty AddressSpace where
  pretty Storage = "storage"
  pretty Private = "private"
  pretty Immediate = "immediate"
  pretty Uniform = "uniform"
  pretty Workgroup = "workgroup"
  pretty Function = "function"

instance Pretty Declaration where
  pretty (FunctionDecl fun) = pretty fun
  pretty (StructDecl struct) = pretty struct
  pretty (VarDecl attribs as am name typ) =
    hsep (map pretty attribs)
      </> "var<"
      <> pretty as
      <> maybe "" ((", " <>) . pretty) am
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
