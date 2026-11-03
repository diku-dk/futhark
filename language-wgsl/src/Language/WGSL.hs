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
    roundUp,
    alignOf,
    sizeOf,
    elementStride,
    hostShareable,
    alignOfStruct,
    offsetOfMembers,
    sizeOfStruct,
    Type (..),
    BinOp,
    UnOp,
    IntLitType (..),
    FloatLitType (..),
    Exp (..),
    intExp,
    floatExp,
    Stmt (..),
    ForUpdate (..),
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
    bitcast,
    prettyDecls,
  )
where

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

data Type
  = Prim PrimType
  | Array PrimType (Maybe Exp)
  | Named Ident
  | Pointer PrimType AddressSpace (Maybe AccessMode)

-- | @roundUp(k, n)@, as used by the layout rules.
roundUp :: Int -> Int -> Int
roundUp k n = ((n + k - 1) `div` k) * k

-- | @AlignOf@ of a primitive type.  Every primitive type has a fixed
-- footprint, so this is total, even for the ones that are not
-- 'hostShareable'.  See
-- https://www.w3.org/TR/WGSL/#alignment-and-size.
primAlignOf :: PrimType -> Int
primAlignOf Bool = 4
primAlignOf Int32 = 4
primAlignOf UInt32 = 4
primAlignOf Float16 = 2
primAlignOf Float32 = 4
primAlignOf (Vec2 t) = 2 * primAlignOf t
primAlignOf (Vec3 t) = 4 * primAlignOf t
primAlignOf (Vec4 t) = 4 * primAlignOf t
primAlignOf (Atomic _) = 4

-- | @SizeOf@ of a primitive type.
primSizeOf :: PrimType -> Int
primSizeOf Bool = 4
primSizeOf Int32 = 4
primSizeOf UInt32 = 4
primSizeOf Float16 = 2
primSizeOf Float32 = 4
primSizeOf (Vec2 t) = 2 * primSizeOf t
primSizeOf (Vec3 t) = 3 * primSizeOf t
primSizeOf (Vec4 t) = 4 * primSizeOf t
primSizeOf (Atomic _) = 4

-- | @AlignOf@ of a type, or 'Nothing' if it has none. The latter occurs for
-- named types and pointers.
alignOf :: Type -> Maybe Int
alignOf (Prim t) = Just $ primAlignOf t
alignOf (Array t _) = Just $ primAlignOf t
alignOf (Named _) = Nothing
alignOf Pointer {} = Nothing

-- | The element stride of an array with the given element type, which is the
-- element size rounded up to the element alignment. See
-- https://www.w3.org/TR/WGSL/#array-layout-examples.
elementStride :: PrimType -> Int
elementStride t = roundUp (primAlignOf t) (primSizeOf t)

-- | @SizeOf@ of a type. An array has a size only if its element count is a
-- literal. Named types and pointers have no size.
sizeOf :: Type -> Maybe Int
sizeOf (Prim t) = Just $ primSizeOf t
sizeOf (Array t (Just (IntExp n _)))
  | n >= 0 = Just $ n * elementStride t
sizeOf Array {} = Nothing
sizeOf (Named _) = Nothing
sizeOf Pointer {} = Nothing

-- | Whether a type is host-shareable, and so may be the store type of a uniform
-- or storage buffer. A named type is not resolved here, so it is reported as
-- not host-shareable. See https://www.w3.org/TR/WGSL/#host-shareable-types.
hostShareable :: Type -> Bool
hostShareable (Prim t) = numeric t
  where
    -- bool is not a numeric scalar type, and a vector of bool is not a
    -- numeric vector type.
    numeric Bool = False
    numeric (Vec2 t') = numeric t'
    numeric (Vec3 t') = numeric t'
    numeric (Vec4 t') = numeric t'
    numeric _ = True
-- A fixed-size array is host-shareable only if it has a creation-fixed
-- footprint, so its element count must be a literal. A runtime-sized array has
-- no count at all.
hostShareable (Array t Nothing) = hostShareable (Prim t)
hostShareable (Array t (Just (IntExp _ _))) = hostShareable (Prim t)
hostShareable Array {} = False
hostShareable (Named _) = False
hostShareable Pointer {} = False

-- | @AlignOf@ of a structure with the given member types, which is the greatest
-- alignment of any member. Returns 'Nothing' if there are no members, since a
-- WGSL structure must have at least one, or if some member has no @AlignOf@.
-- Layout attributes are not taken into account. See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
alignOfStruct :: [Type] -> Maybe Int
alignOfStruct [] = Nothing
alignOfStruct members = maximum <$> mapM alignOf members

-- | @OffsetOfMember@ of every member of a structure with the given member
-- types. These are known even when the structure has no @SizeOf@, as when its
-- last member is a runtime-sized array. Layout attributes are not taken into
-- account.
offsetOfMembers :: [Type] -> Maybe [Int]
offsetOfMembers [] = Nothing
offsetOfMembers members = do
  aligns <- mapM alignOf $ drop 1 members
  sizes <- mapM sizeOf $ init members
  pure $
    scanl
      (\prev_off (al, prev_sz) -> roundUp al (prev_off + prev_sz))
      0
      (zip aligns sizes)

-- | @SizeOf@ of a structure with the given member types, which is the end of
-- its last member rounded up to the structure's alignment. 'Nothing' if any
-- member has no @SizeOf@, which includes a trailing runtime-sized array. Layout
-- attributes are not taken into account.
sizeOfStruct :: [Type] -> Maybe Int
sizeOfStruct members = do
  align <- alignOfStruct members
  offsets <- offsetOfMembers members
  last_size <- sizeOf $ last members
  pure $ roundUp align (last offsets + last_size)

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
  | -- | A call.  The first expression list is the template argument list of
    -- the callee, as in @bitcast<i32>(x)@; it is usually empty.  See
    -- https://www.w3.org/TR/WGSL/#syntax-call_phrase.
    CallExp Ident [Exp] [Exp]
  | -- | A type used where the grammar expects an expression, which is where
    -- template arguments live.
    TypeExp Type
  | IndexExp Ident Exp
  | FieldExp Exp Ident

data Stmt
  = Skip
  | -- | This technically is not a statement in WGSL, but it is useful for
    -- legibility of generated code. Produces a line comment. The comment may
    -- have linebreaks.
    Comment T.Text
  | Seq Stmt Stmt
  | Let Ident Exp
  | DeclareVar Ident Type
  | -- | The left-hand side must be an assignable expression.  See
    -- https://www.w3.org/TR/WGSL/#assignment.
    Assign Exp Exp
  | If Exp Stmt Stmt
  | For Ident Exp Exp ForUpdate Stmt
  | While Exp Stmt
  | Call Ident [Exp] [Exp]
  | -- | The expression must be present exactly when the enclosing function
    -- has a return type.
    Return (Maybe Exp)

-- | The update of a @for@ loop, which the grammar restricts to an assignment
-- or a function call.  See https://www.w3.org/TR/WGSL/#for-statement.
data ForUpdate
  = -- | The left-hand side must be an assignable expression.
    UpdateAssign Exp Exp
  | UpdateCall Ident [Exp] [Exp]

data Attrib = Attrib Ident [Exp]

data Param = Param Ident Type [Attrib]

-- | A function declaration.  See
-- https://www.w3.org/TR/WGSL/#function-declaration-sec.
data FunDecl = FunDecl
  { funName :: Ident,
    funAttribs :: [Attrib],
    funParams :: [Param],
    funReturn :: Maybe ([Attrib], Type),
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

-- | Reinterpret the bits of an expression as another type.  See
-- https://www.w3.org/TR/WGSL/#bitcast-builtin.
bitcast :: Type -> Exp -> Exp
bitcast t e = CallExp "bitcast" [TypeExp t] [e]

--- Prettyprinting definitions

-- | A template argument list, which is omitted when empty.
templateList :: [Exp] -> Doc a
templateList [] = mempty
templateList args = "<" <> commasep (map pretty args) <> ">"

-- | Separate with commas.
commasep :: [Doc a] -> Doc a
commasep = hsep . punctuate comma

-- | Like commasep, but a newline after every comma.
commastack :: [Doc a] -> Doc a
commastack = align . vsep . punctuate comma

-- | Separate with semicolons and newlines.
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
  pretty (CallExp f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args)
  pretty (TypeExp t) = pretty t
  pretty (IndexExp x i) = pretty x <> brackets (pretty i)
  pretty (FieldExp x y) = pretty x <> "." <> pretty y

instance Pretty Stmt where
  pretty Skip = ";"
  pretty (Comment c) = vsep (map ("//" <+>) (pretty <$> T.lines c))
  pretty (Seq s1 s2) = stack [pretty s1, pretty s2]
  pretty (Let x e) = "let" <+> pretty x <+> "=" <+> pretty e <> ";"
  pretty (DeclareVar x t) = "var" <+> pretty x <+> ":" <+> pretty t <> ";"
  pretty (Assign lhs e) = pretty lhs <+> "=" <+> pretty e <> ";"
  pretty (If cond Skip Skip) = "if" <+> pretty cond <+> "{ }"
  pretty (If cond th Skip) =
    "if"
      <+> pretty cond
      <+> "{"
        </> indent 2 (pretty th)
        </> "}"
  pretty (If cond Skip el) =
    "if"
      <+> pretty cond
      <+> "{ }"
        </> "else {"
        </> indent 2 (pretty el)
        </> "}"
  pretty (If cond th el) =
    "if"
      <+> pretty cond
      <+> "{"
        </> indent 2 (pretty th)
        </> "} else {"
        </> indent 2 (pretty el)
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
        </> "}"
  pretty (While cond body) =
    "while"
      <+> pretty cond
      <+> "{"
        </> indent 2 (pretty body)
        </> "}"
  pretty (Call f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args) <> ";"
  pretty (Return Nothing) = "return;"
  pretty (Return (Just e)) = "return" <+> pretty e <> ";"

instance Pretty ForUpdate where
  pretty (UpdateAssign lhs e) = pretty lhs <+> "=" <+> pretty e
  pretty (UpdateCall f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args)

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

instance Pretty FunDecl where
  pretty (FunDecl name attribs params ret body) =
    stack
      [ hsep (map pretty attribs),
        header,
        indent 2 (pretty body),
        "}"
      ]
    where
      header = case ret of
        Nothing ->
          "fn" <+> pretty name <> prettyParams params <+> "{"
        Just (ret_attribs, t) ->
          "fn"
            <+> pretty name
            <> prettyParams params
            <+> "->"
            <+> hsep (map pretty ret_attribs ++ [pretty t])
            <+> "{"

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
