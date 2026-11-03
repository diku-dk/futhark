-- | Basic WGSL definitions, including AST definitions. These are intended to
-- correspond closely with the terminology and structure of the WGSL
-- specification, but it is not automatically generated from the specification,
-- and so may have inconsistencies.
--
-- The definitions here are abstract in the sense that they cannot represent
-- syntactic details of a WGSL program, e.g., whether an integer literal is
-- written hexadecimally. This makes these definitions less useful for parsing,
-- but fine for code generation. I would not be opposed to adding a concrete
-- syntax module in the future.
--
-- To produce WGSL code as text, use the 'Pretty' instance for the various
-- constructs.
--
-- Known and intentional deviations from the specification:
--
-- 1. Statements do not have attributes
--
-- 2. Assignment targets are ordinary expressions, not a separate
--    @lhs_expression@.
--
-- 3. When printed, expressions are always fully parenthesised, so operator
--    precedence is not modelled.
module Language.WGSL
  ( -- * Identifiers
    Ident,

    -- * Types
    Type (..),
    TextureDim (..),
    StorageTextureDim (..),
    DepthTexture (..),
    TexelFormat (..),

    -- * Memory
    AccessMode (..),
    AddressSpace (..),

    -- ** Alignment and size
    roundUp,
    roundDown,
    alignOf,
    sizeOf,
    strideOf,
    hostShareable,
    alignOfMember,
    sizeOfMember,
    alignOfStruct,
    offsetOfMembers,
    sizeOfStruct,

    -- * Expressions
    Expression (..),
    BinaryOperator (..),
    UnaryOperator (..),
    IntLiteralType (..),
    FloatLiteralType (..),
    intExp,
    floatExp,
    bitcast,

    -- * Statements
    Statement (..),
    AssignmentOperator (..),
    ForUpdate (..),
    SwitchClause (..),
    CaseSelector (..),
    Continuing (..),

    -- * Declarations
    Attribute (..),
    bindingAttribs,
    Param (..),
    FunctionDecl (..),
    StructMember (..),
    StructDecl (..),
    GlobalDecl (..),

    -- * Modules
    Module (..),
    Directive (..),
    Severity (..),
    EnableExtension (..),
    LanguageExtension (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Prettyprinter

-- | A WGSL identifier, which is not checked for validity.
type Ident = T.Text

-- | A type as it occurs in WGSL source. See
-- https://www.w3.org/TR/WGSL/#type-specifiers.
data Type
  = Bool
  | I32
  | U32
  | F16
  | F32
  | Vec2 Type
  | Vec3 Type
  | Vec4 Type
  | -- | @matCxR@ has @C@ columns of @R@ rows.
    Mat2x2 Type
  | Mat2x3 Type
  | Mat2x4 Type
  | Mat3x2 Type
  | Mat3x3 Type
  | Mat3x4 Type
  | Mat4x2 Type
  | Mat4x3 Type
  | Mat4x4 Type
  | Atomic Type
  | -- | An array, which is runtime-sized when the element count is absent.
    Array Type (Maybe Expression)
  | Ptr Type AddressSpace (Maybe AccessMode)
  | -- | A structure or type alias, referred to by name. Its definition is not
    -- known here, so it has no layout.
    Named Ident
  | -- | See https://www.w3.org/TR/WGSL/#sampled-texture-type.
    TextureSampled TextureDim Type
  | -- | Always two-dimensional.  See
    -- https://www.w3.org/TR/WGSL/#multisampled-texture-type.
    TextureMultisampled Type
  | -- | An external sampled texture type.  See
    -- https://www.w3.org/TR/WGSL/#external-texture-type.
    TextureExternal
  | -- | A storage texture type.  See
    -- https://www.w3.org/TR/WGSL/#texture-storage.
    TextureStorage StorageTextureDim TexelFormat AccessMode
  | -- | A depth texture type.  See
    -- https://www.w3.org/TR/WGSL/#texture-depth.
    TextureDepth DepthTexture
  | -- | A sampler.  See https://www.w3.org/TR/WGSL/#sampler-type.
    Sampler
  | -- | A comparison sampler.
    SamplerComparison
  deriving (Eq, Ord, Show)

-- | The dimensionality and arrangement of a sampled texture.  See
-- https://www.w3.org/TR/WGSL/#sampled-texture-type.
data TextureDim
  = Tex1d
  | Tex2d
  | Tex2dArray
  | Tex3d
  | TexCube
  | TexCubeArray
  deriving (Eq, Ord, Show)

-- | The dimensionality and arrangement of a storage texture.  See
-- https://www.w3.org/TR/WGSL/#texture-storage.
data StorageTextureDim
  = StorageTex1d
  | StorageTex2d
  | StorageTex2dArray
  | StorageTex3d
  deriving (Eq, Ord, Show)

-- | A depth texture type.  See https://www.w3.org/TR/WGSL/#texture-depth.
data DepthTexture
  = Depth2d
  | Depth2dArray
  | DepthCube
  | DepthCubeArray
  | DepthMultisampled2d
  deriving (Eq, Ord, Show)

-- | A texel format, used to parameterise the storage texture types.  See
-- https://www.w3.org/TR/WGSL/#texel-formats.
data TexelFormat
  = Rgba8Unorm
  | Rgba8Snorm
  | Rgba8Uint
  | Rgba8Sint
  | Rgba16Unorm
  | Rgba16Snorm
  | Rgba16Uint
  | Rgba16Sint
  | Rgba16Float
  | Rg8Unorm
  | Rg8Snorm
  | Rg8Uint
  | Rg8Sint
  | Rg16Unorm
  | Rg16Snorm
  | Rg16Uint
  | Rg16Sint
  | Rg16Float
  | R32Uint
  | R32Sint
  | R32Float
  | Rg32Uint
  | Rg32Sint
  | Rg32Float
  | Rgba32Uint
  | Rgba32Sint
  | Rgba32Float
  | Bgra8Unorm
  | R8Unorm
  | R8Snorm
  | R8Uint
  | R8Sint
  | R16Unorm
  | R16Snorm
  | R16Uint
  | R16Sint
  | R16Float
  | Rgb10a2Unorm
  | Rgb10a2Uint
  | Rg11b10Ufloat
  deriving (Eq, Ord, Show)

-- | @roundUp(k, n)@, which is @ceiling(n / k) * k@, for positive integers.
-- See https://www.w3.org/TR/WGSL/#roundup.
roundUp :: Int -> Int -> Int
roundUp k n = ((n + k - 1) `div` k) * k

-- | @roundDown(k, n)@, which is @floor(n / k) * k@, for positive integers.
-- See https://www.w3.org/TR/WGSL/#rounddown.
roundDown :: Int -> Int -> Int
roundDown k n = (n `div` k) * k

-- | @AlignOf@ of a type.  'Nothing' for pointers, texture and sampler types,
-- and for a 'Named' type, which is not resolved here.  See
-- https://www.w3.org/TR/WGSL/#alignment-and-size.
alignOf :: Type -> Maybe Int
alignOf Bool = Just 4
alignOf I32 = Just 4
alignOf U32 = Just 4
alignOf F16 = Just 2
alignOf F32 = Just 4
alignOf (Vec2 t) = (2 *) <$> alignOf t
alignOf (Vec3 t) = (4 *) <$> alignOf t
alignOf (Vec4 t) = (4 *) <$> alignOf t
-- The alignment of a matrix is that of its columns.
alignOf (Mat2x2 t) = alignOf $ Vec2 t
alignOf (Mat3x2 t) = alignOf $ Vec2 t
alignOf (Mat4x2 t) = alignOf $ Vec2 t
alignOf (Mat2x3 t) = alignOf $ Vec3 t
alignOf (Mat3x3 t) = alignOf $ Vec3 t
alignOf (Mat4x3 t) = alignOf $ Vec3 t
alignOf (Mat2x4 t) = alignOf $ Vec4 t
alignOf (Mat3x4 t) = alignOf $ Vec4 t
alignOf (Mat4x4 t) = alignOf $ Vec4 t
alignOf (Atomic _) = Just 4
alignOf (Array t _) = alignOf t
alignOf Ptr {} = Nothing
alignOf (Named _) = Nothing
alignOf TextureSampled {} = Nothing
alignOf (TextureMultisampled _) = Nothing
alignOf TextureExternal = Nothing
alignOf TextureStorage {} = Nothing
alignOf (TextureDepth _) = Nothing
alignOf Sampler = Nothing
alignOf SamplerComparison = Nothing

-- | @SizeOf@ of a type.  'Nothing' as for 'alignOf', and also for an array
-- whose element count is not a literal.  See
-- https://www.w3.org/TR/WGSL/#alignment-and-size.
sizeOf :: Type -> Maybe Int
sizeOf Bool = Just 4
sizeOf I32 = Just 4
sizeOf U32 = Just 4
sizeOf F16 = Just 2
sizeOf F32 = Just 4
sizeOf (Vec2 t) = (2 *) <$> sizeOf t
sizeOf (Vec3 t) = (3 *) <$> sizeOf t
sizeOf (Vec4 t) = (4 *) <$> sizeOf t
-- A matrix is laid out as an array of its columns.
sizeOf (Mat2x2 t) = (2 *) <$> strideOf (Vec2 t)
sizeOf (Mat3x2 t) = (3 *) <$> strideOf (Vec2 t)
sizeOf (Mat4x2 t) = (4 *) <$> strideOf (Vec2 t)
sizeOf (Mat2x3 t) = (2 *) <$> strideOf (Vec3 t)
sizeOf (Mat3x3 t) = (3 *) <$> strideOf (Vec3 t)
sizeOf (Mat4x3 t) = (4 *) <$> strideOf (Vec3 t)
sizeOf (Mat2x4 t) = (2 *) <$> strideOf (Vec4 t)
sizeOf (Mat3x4 t) = (3 *) <$> strideOf (Vec4 t)
sizeOf (Mat4x4 t) = (4 *) <$> strideOf (Vec4 t)
sizeOf (Atomic _) = Just 4
sizeOf (Array t (Just (ExpInt n _)))
  | n >= 0 = (n *) <$> strideOf t
sizeOf Array {} = Nothing
sizeOf Ptr {} = Nothing
sizeOf (Named _) = Nothing
sizeOf TextureSampled {} = Nothing
sizeOf (TextureMultisampled _) = Nothing
sizeOf TextureExternal = Nothing
sizeOf TextureStorage {} = Nothing
sizeOf (TextureDepth _) = Nothing
sizeOf Sampler = Nothing
sizeOf SamplerComparison = Nothing

-- | @StrideOf@ of an array with the given element type.  See
-- https://www.w3.org/TR/WGSL/#internal-value-layout.
strideOf :: Type -> Maybe Int
strideOf t = roundUp <$> alignOf t <*> sizeOf t

-- | Whether a type is host-shareable.  A 'Named' type is not resolved here,
-- and so is reported as not host-shareable.  See
-- https://www.w3.org/TR/WGSL/#host-shareable-types.
hostShareable :: Type -> Bool
-- bool is not a numeric scalar type, and a vector of bool is not a numeric
-- vector type.
hostShareable Bool = False
hostShareable I32 = True
hostShareable U32 = True
hostShareable F16 = True
hostShareable F32 = True
hostShareable (Vec2 t) = hostShareable t
hostShareable (Vec3 t) = hostShareable t
hostShareable (Vec4 t) = hostShareable t
hostShareable (Mat2x2 t) = hostShareable t
hostShareable (Mat2x3 t) = hostShareable t
hostShareable (Mat2x4 t) = hostShareable t
hostShareable (Mat3x2 t) = hostShareable t
hostShareable (Mat3x3 t) = hostShareable t
hostShareable (Mat3x4 t) = hostShareable t
hostShareable (Mat4x2 t) = hostShareable t
hostShareable (Mat4x3 t) = hostShareable t
hostShareable (Mat4x4 t) = hostShareable t
hostShareable (Atomic _) = True
-- A fixed-size array is host-shareable only if it has a creation-fixed
-- footprint, so its element count must be a literal.  A runtime-sized array
-- has no count at all.
hostShareable (Array t Nothing) = hostShareable t
hostShareable (Array t (Just (ExpInt _ _))) = hostShareable t
hostShareable Array {} = False
hostShareable Ptr {} = False
hostShareable (Named _) = False
hostShareable TextureSampled {} = False
hostShareable (TextureMultisampled _) = False
hostShareable TextureExternal = False
hostShareable TextureStorage {} = False
hostShareable (TextureDepth _) = False
hostShareable Sampler = False
hostShareable SamplerComparison = False

-- | @AlignOfMember@ of a structure member.  'Nothing' if its @\@align@
-- argument is not a literal.  See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
alignOfMember :: StructMember -> Maybe Int
alignOfMember (StructMember attribs _ t) =
  case [args | Attribute name args <- attribs, name == "align"] of
    [] -> alignOf t
    [[ExpInt k _]] -> Just k
    _ -> Nothing

-- | @SizeOfMember@ of a structure member.  'Nothing' if its @\@size@
-- argument is not a literal.  See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
sizeOfMember :: StructMember -> Maybe Int
sizeOfMember (StructMember attribs _ t) =
  case [args | Attribute name args <- attribs, name == "size"] of
    [] -> sizeOf t
    [[ExpInt k _]] -> Just k
    _ -> Nothing

-- | @AlignOf@ of a structure with the given members.  'Nothing' if there are
-- no members.  See https://www.w3.org/TR/WGSL/#structure-member-layout.
alignOfStruct :: [StructMember] -> Maybe Int
alignOfStruct [] = Nothing
alignOfStruct members = maximum <$> mapM alignOfMember members

-- | @OffsetOfMember@ of every member of a structure, which is known even when
-- the structure has no @SizeOf@.  See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
offsetOfMembers :: [StructMember] -> Maybe [Int]
offsetOfMembers [] = Nothing
offsetOfMembers members = do
  aligns <- mapM alignOfMember $ drop 1 members
  sizes <- mapM sizeOfMember $ init members
  pure $
    scanl
      (\prev_off (al, prev_sz) -> roundUp al (prev_off + prev_sz))
      0
      (zip aligns sizes)

-- | @SizeOf@ of a structure with the given members.  'Nothing' if any member
-- has no @SizeOfMember@.  See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
sizeOfStruct :: [StructMember] -> Maybe Int
sizeOfStruct members = do
  struct_align <- alignOfStruct members
  offsets <- offsetOfMembers members
  last_size <- sizeOfMember $ last members
  pure $ roundUp struct_align (last offsets + last_size)

-- | A binary operator. See https://www.w3.org/TR/WGSL/#expressions.
data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | BitwiseAnd
  | BitwiseOr
  | BitwiseExclusiveOr
  | ShiftLeft
  | ShiftRight
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Equal
  | NotEqual
  | ShortCircuitAnd
  | ShortCircuitOr
  deriving (Eq, Ord, Show)

-- | A unary operator.  See https://www.w3.org/TR/WGSL/#expressions.
data UnaryOperator
  = Negate
  | LogicalNegate
  | BitwiseComplement
  | Indirection
  | AddressOf
  deriving (Eq, Ord, Show)

-- | The type denoted by an integer literal, which is determined by its
-- suffix.  See https://www.w3.org/TR/WGSL/#numeric-literals.
data IntLiteralType
  = -- | No suffix.
    AbstractInt
  | -- | The @i@ suffix.
    LitI32
  | -- | The @u@ suffix.
    LitU32
  deriving (Eq, Ord, Show)

-- | The type denoted by a floating point literal, which is determined by its
-- suffix.  See https://www.w3.org/TR/WGSL/#numeric-literals.
data FloatLiteralType
  = -- | No suffix.
    AbstractFloat
  | -- | The @f@ suffix.
    LitF32
  | -- | The @h@ suffix.  Using it requires the @enable f16;@ directive.
    LitF16
  deriving (Eq, Ord, Show)

-- | An expression.  See https://www.w3.org/TR/WGSL/#expressions.
data Expression
  = ExpBool Bool
  | -- | An integer literal.  Prefer 'intExp', which checks that the value can
    -- be denoted by a literal of the given type.
    ExpInt Int IntLiteralType
  | -- | A floating point literal.  Prefer 'floatExp'.
    ExpFloat Double FloatLiteralType
  | ExpIdent Ident
  | ExpBinary BinaryOperator Expression Expression
  | ExpUnary UnaryOperator Expression
  | -- | A call.  The first list is the callee's template arguments, as in
    -- @bitcast<i32>(x)@.
    ExpCall Ident [Expression] [Expression]
  | -- | A type in expression position, as a template argument is.
    ExpType Type
  | ExpIndex Expression Expression
  | ExpMember Expression Ident
  deriving (Eq, Ord, Show)

-- | A statement.  See https://www.w3.org/TR/WGSL/#statements.
data Statement
  = -- | The empty statement.
    Empty
  | -- | A line comment, which is not a statement in WGSL, but is useful in
    -- generated code.  May contain linebreaks.
    Comment T.Text
  | Compound [Statement]
  | -- | @let x = e@, with an optional type.
    Let Ident (Maybe Type) Expression
  | -- | @const x = e@, with an optional type.
    Const Ident (Maybe Type) Expression
  | -- | @var x : T = e@.
    Var Ident (Maybe Type) (Maybe Expression)
  | Assign Expression AssignmentOperator Expression
  | -- | @_ = e@.
    PhonyAssign Expression
  | Increment Expression
  | Decrement Expression
  | -- | @const_assert e@.  'GlobalAssert' is its module-scope counterpart.
    ConstAssert Expression
  | -- | An @if@, its @else if@ clauses, and its optional @else@ clause.
    If Expression [Statement] [(Expression, [Statement])] (Maybe [Statement])
  | Switch Expression (NonEmpty SwitchClause)
  | -- | A @loop@, with its optional @continuing@ block.
    Loop [Statement] (Maybe Continuing)
  | For Ident Expression Expression ForUpdate [Statement]
  | While Expression [Statement]
  | Break
  | Continue
  | Discard
  | Call Ident [Expression] [Expression]
  | Return (Maybe Expression)
  deriving (Eq, Ord, Show)

-- | A clause of a @switch@ statement.  See
-- https://www.w3.org/TR/WGSL/#switch-statement.
data SwitchClause
  = -- | A @case@ with one or more selectors.
    Case (NonEmpty CaseSelector) [Statement]
  | -- | A @default@ clause on its own.
    DefaultCase [Statement]
  deriving (Eq, Ord, Show)

-- | A selector of a @case@ clause.  See
-- https://www.w3.org/TR/WGSL/#switch-statement.
data CaseSelector
  = CaseExp Expression
  | CaseDefault
  deriving (Eq, Ord, Show)

-- | The @continuing@ block of a @loop@, which may end in a @break if@.  See
-- https://www.w3.org/TR/WGSL/#continuing-statement.
data Continuing = Continuing [Statement] (Maybe Expression)
  deriving (Eq, Ord, Show)

-- | The operator of an assignment: plain assignment, or one of the compound
-- assignment operators.  See https://www.w3.org/TR/WGSL/#assignment.
data AssignmentOperator
  = AssignSimple
  | AssignAdd
  | AssignSubtract
  | AssignMultiply
  | AssignDivide
  | AssignRemainder
  | AssignBitwiseAnd
  | AssignBitwiseOr
  | AssignBitwiseExclusiveOr
  | AssignShiftLeft
  | AssignShiftRight
  deriving (Eq, Ord, Show)

-- | The update of a @for@ loop, which the grammar restricts to the statements
-- that update a variable, or to a function call. These mirror the
-- correspondingly named statements. See
-- https://www.w3.org/TR/WGSL/#for-statement.
data ForUpdate
  = -- | The left-hand side must be an assignable expression.
    UpdateAssign Expression AssignmentOperator Expression
  | UpdatePhonyAssign Expression
  | UpdateIncrement Expression
  | UpdateDecrement Expression
  | UpdateCall Ident [Expression] [Expression]
  deriving (Eq, Ord, Show)

-- | An attribute.  See https://www.w3.org/TR/WGSL/#attributes.
data Attribute = Attribute Ident [Expression]
  deriving (Eq, Ord, Show)

-- | A formal parameter of a function.  See
-- https://www.w3.org/TR/WGSL/#function-declaration-sec.
data Param = Param [Attribute] Ident Type
  deriving (Eq, Ord, Show)

-- | A function declaration.  See
-- https://www.w3.org/TR/WGSL/#function-declaration-sec.
data FunctionDecl = FunctionDecl
  { functionName :: Ident,
    functionAttributes :: [Attribute],
    functionParams :: [Param],
    functionReturn :: Maybe ([Attribute], Type),
    functionBody :: [Statement]
  }
  deriving (Eq, Ord, Show)

-- | A structure member.  See https://www.w3.org/TR/WGSL/#struct-types.
data StructMember = StructMember [Attribute] Ident Type
  deriving (Eq, Ord, Show)

-- | A structure declaration.  See
-- https://www.w3.org/TR/WGSL/#struct-types.
data StructDecl = StructDecl Ident [StructMember]
  deriving (Eq, Ord, Show)

-- | The access modes supported by a memory view.  See
-- https://www.w3.org/TR/WGSL/#memory-access-mode.
data AccessMode = Read | Write | ReadWrite
  deriving (Eq, Ord, Show)

-- | An address space.  The @handle@ space is never written in WGSL source, so
-- it is not represented.  See https://www.w3.org/TR/WGSL/#address-spaces.
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
data GlobalDecl
  = GlobalFunction FunctionDecl
  | GlobalStruct StructDecl
  | -- | The access mode may only be given along with the address space, and
    -- defaults to the one of the address space when omitted.
    GlobalVariable [Attribute] AddressSpace (Maybe AccessMode) Ident (Maybe Type) (Maybe Expression)
  | GlobalOverride [Attribute] Ident (Maybe Type) (Maybe Expression)
  | -- | A module-scope @const@, which takes no attributes.
    GlobalConst Ident (Maybe Type) Expression
  | -- | A type alias.
    GlobalAlias Ident Type
  | -- | A module-scope @const_assert@.
    GlobalAssert Expression
  deriving (Eq, Ord, Show)

-- | A WGSL module: its directives, then its declarations.  See
-- https://www.w3.org/TR/WGSL/#wgsl-module-definition.
data Module = Module [Directive] [GlobalDecl]
  deriving (Eq, Ord, Show)

-- | A directive.  See https://www.w3.org/TR/WGSL/#directives.
data Directive
  = Enable (NonEmpty EnableExtension)
  | Requires (NonEmpty LanguageExtension)
  | -- | A diagnostic filter.  Rule names are not a closed set, so the rule is
    -- given as an identifier.
    Diagnostic Severity Ident
  deriving (Eq, Ord, Show)

-- | The severity of a diagnostic.  See
-- https://www.w3.org/TR/WGSL/#diagnostics.
data Severity = Error | Warning | Info | Off
  deriving (Eq, Ord, Show)

-- | An extension that an @enable@ directive may enable.  See
-- https://www.w3.org/TR/WGSL/#enable-extensions.
data EnableExtension
  = EnableF16
  | EnableClipDistances
  | EnableDualSourceBlending
  | EnableSubgroups
  | EnablePrimitiveIndex
  | EnableSubgroupSizeControl
  deriving (Eq, Ord, Show)

-- | A language extension that a @requires@ directive may require.  See
-- https://www.w3.org/TR/WGSL/#language-extensions.
data LanguageExtension
  = ReadonlyAndReadwriteStorageTextures
  | Packed4x8IntegerDotProduct
  | UnrestrictedPointerParameters
  | PointerCompositeAccess
  | UniformBufferStandardLayout
  | SubgroupId
  | SubgroupUniformity
  | TextureAndSamplerLet
  | TextureFormatsTier1
  | LinearIndexing
  | ImmediateAddressSpace
  | FragmentDepth
  | BufferView
  | SwizzleAssignment
  deriving (Eq, Ord, Show)

-- | The @\@group@ and @\@binding@ attributes for the given group and
-- binding.
bindingAttribs :: Int -> Int -> [Attribute]
bindingAttribs grp binding =
  [ Attribute "group" [ExpInt grp AbstractInt],
    Attribute "binding" [ExpInt binding AbstractInt]
  ]

-- | Construct an integer literal, or 'Nothing' if the value cannot be denoted
-- by a literal of the given type.
intExp :: Int -> IntLiteralType -> Maybe Expression
intExp v t
  | representable = Just $ ExpInt v t
  | otherwise = Nothing
  where
    representable = case t of
      AbstractInt -> magnitude <= 2 ^ (63 :: Int) - 1
      LitI32 -> magnitude <= 2 ^ (31 :: Int) - 1
      LitU32 -> toInteger v >= 0 && toInteger v <= 2 ^ (32 :: Int) - 1
    magnitude = abs $ toInteger v

-- | Construct a floating point literal, or 'Nothing' if the value cannot be
-- denoted by a literal of the given type.
floatExp :: Double -> FloatLiteralType -> Maybe Expression
floatExp v t
  | isNaN v || isInfinite v = Nothing
  | maybe False (abs v >) largestFinite = Nothing
  | otherwise = Just $ ExpFloat v t
  where
    largestFinite = case t of
      -- Any finite Double denotes an AbstractFloat.
      AbstractFloat -> Nothing
      LitF32 -> Just 3.40282346638528859812e38
      LitF16 -> Just 65504

-- | Reinterpret the bits of an expression as another type.  See
-- https://www.w3.org/TR/WGSL/#bitcast-builtin.
bitcast :: Type -> Expression -> Expression
bitcast t e = ExpCall "bitcast" [ExpType t] [e]

-- | A template argument list, which is omitted when empty.
templateArgs :: [Doc a] -> Doc a
templateArgs [] = mempty
templateArgs args = "<" <> commasep args <> ">"

templateList :: [Expression] -> Doc a
templateList = templateArgs . map pretty

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

instance Pretty Type where
  pretty Bool = "bool"
  pretty I32 = "i32"
  pretty U32 = "u32"
  pretty F16 = "f16"
  pretty F32 = "f32"
  pretty (Vec2 t) = generator "vec2" [pretty t]
  pretty (Vec3 t) = generator "vec3" [pretty t]
  pretty (Vec4 t) = generator "vec4" [pretty t]
  pretty (Mat2x2 t) = generator "mat2x2" [pretty t]
  pretty (Mat2x3 t) = generator "mat2x3" [pretty t]
  pretty (Mat2x4 t) = generator "mat2x4" [pretty t]
  pretty (Mat3x2 t) = generator "mat3x2" [pretty t]
  pretty (Mat3x3 t) = generator "mat3x3" [pretty t]
  pretty (Mat3x4 t) = generator "mat3x4" [pretty t]
  pretty (Mat4x2 t) = generator "mat4x2" [pretty t]
  pretty (Mat4x3 t) = generator "mat4x3" [pretty t]
  pretty (Mat4x4 t) = generator "mat4x4" [pretty t]
  pretty (Atomic t) = generator "atomic" [pretty t]
  pretty (Array t Nothing) = generator "array" [pretty t]
  pretty (Array t (Just n)) = generator "array" [pretty t, pretty n]
  pretty (Ptr t as am) =
    generator "ptr" $ [pretty as, pretty t] ++ maybe [] (pure . pretty) am
  pretty (Named t) = pretty t
  pretty (TextureSampled dim t) = generator (textureDim dim) [pretty t]
  pretty (TextureMultisampled t) =
    generator "texture_multisampled_2d" [pretty t]
  pretty TextureExternal = "texture_external"
  pretty (TextureStorage dim fmt am) =
    generator (storageTextureDim dim) [pretty fmt, pretty am]
  pretty (TextureDepth d) = pretty d
  pretty Sampler = "sampler"
  pretty SamplerComparison = "sampler_comparison"

-- | The name of the sampled texture type-generator for a dimensionality.
textureDim :: TextureDim -> Ident
textureDim Tex1d = "texture_1d"
textureDim Tex2d = "texture_2d"
textureDim Tex2dArray = "texture_2d_array"
textureDim Tex3d = "texture_3d"
textureDim TexCube = "texture_cube"
textureDim TexCubeArray = "texture_cube_array"

-- | The name of the storage texture type-generator for a dimensionality.
storageTextureDim :: StorageTextureDim -> Ident
storageTextureDim StorageTex1d = "texture_storage_1d"
storageTextureDim StorageTex2d = "texture_storage_2d"
storageTextureDim StorageTex2dArray = "texture_storage_2d_array"
storageTextureDim StorageTex3d = "texture_storage_3d"

instance Pretty DepthTexture where
  pretty Depth2d = "texture_depth_2d"
  pretty Depth2dArray = "texture_depth_2d_array"
  pretty DepthCube = "texture_depth_cube"
  pretty DepthCubeArray = "texture_depth_cube_array"
  pretty DepthMultisampled2d = "texture_depth_multisampled_2d"

instance Pretty TexelFormat where
  pretty Rgba8Unorm = "rgba8unorm"
  pretty Rgba8Snorm = "rgba8snorm"
  pretty Rgba8Uint = "rgba8uint"
  pretty Rgba8Sint = "rgba8sint"
  pretty Rgba16Unorm = "rgba16unorm"
  pretty Rgba16Snorm = "rgba16snorm"
  pretty Rgba16Uint = "rgba16uint"
  pretty Rgba16Sint = "rgba16sint"
  pretty Rgba16Float = "rgba16float"
  pretty Rg8Unorm = "rg8unorm"
  pretty Rg8Snorm = "rg8snorm"
  pretty Rg8Uint = "rg8uint"
  pretty Rg8Sint = "rg8sint"
  pretty Rg16Unorm = "rg16unorm"
  pretty Rg16Snorm = "rg16snorm"
  pretty Rg16Uint = "rg16uint"
  pretty Rg16Sint = "rg16sint"
  pretty Rg16Float = "rg16float"
  pretty R32Uint = "r32uint"
  pretty R32Sint = "r32sint"
  pretty R32Float = "r32float"
  pretty Rg32Uint = "rg32uint"
  pretty Rg32Sint = "rg32sint"
  pretty Rg32Float = "rg32float"
  pretty Rgba32Uint = "rgba32uint"
  pretty Rgba32Sint = "rgba32sint"
  pretty Rgba32Float = "rgba32float"
  pretty Bgra8Unorm = "bgra8unorm"
  pretty R8Unorm = "r8unorm"
  pretty R8Snorm = "r8snorm"
  pretty R8Uint = "r8uint"
  pretty R8Sint = "r8sint"
  pretty R16Unorm = "r16unorm"
  pretty R16Snorm = "r16snorm"
  pretty R16Uint = "r16uint"
  pretty R16Sint = "r16sint"
  pretty R16Float = "r16float"
  pretty Rgb10a2Unorm = "rgb10a2unorm"
  pretty Rgb10a2Uint = "rgb10a2uint"
  pretty Rg11b10Ufloat = "rg11b10ufloat"

-- | A predeclared type-generator applied to its template arguments.
generator :: Ident -> [Doc a] -> Doc a
generator name args = pretty name <> templateArgs args

intSuffix :: IntLiteralType -> Doc a
intSuffix AbstractInt = mempty
intSuffix LitI32 = "i"
intSuffix LitU32 = "u"

floatSuffix :: FloatLiteralType -> Doc a
floatSuffix AbstractFloat = mempty
floatSuffix LitF32 = "f"
floatSuffix LitF16 = "h"

instance Pretty BinaryOperator where
  pretty Add = "+"
  pretty Subtract = "-"
  pretty Multiply = "*"
  pretty Divide = "/"
  pretty Remainder = "%"
  pretty BitwiseAnd = "&"
  pretty BitwiseOr = "|"
  pretty BitwiseExclusiveOr = "^"
  pretty ShiftLeft = "<<"
  pretty ShiftRight = ">>"
  pretty Less = "<"
  pretty LessEqual = "<="
  pretty Greater = ">"
  pretty GreaterEqual = ">="
  pretty Equal = "=="
  pretty NotEqual = "!="
  pretty ShortCircuitAnd = "&&"
  pretty ShortCircuitOr = "||"

instance Pretty UnaryOperator where
  pretty Negate = "-"
  pretty LogicalNegate = "!"
  pretty BitwiseComplement = "~"
  pretty Indirection = "*"
  pretty AddressOf = "&"

instance Pretty Expression where
  pretty (ExpBool True) = "true"
  pretty (ExpBool False) = "false"
  pretty (ExpInt x t) = pretty x <> intSuffix t
  pretty (ExpFloat x t) = pretty x <> floatSuffix t
  pretty (ExpIdent x) = pretty x
  pretty (ExpUnary op e) = parens (pretty op <> pretty e)
  pretty (ExpBinary op e1 e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (ExpCall f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args)
  pretty (ExpType t) = pretty t
  pretty (ExpIndex e i) = pretty e <> brackets (pretty i)
  pretty (ExpMember x y) = pretty x <> "." <> pretty y

instance Pretty Statement where
  pretty Empty = ";"
  pretty (Comment c) = vsep (map ("//" <+>) (pretty <$> T.lines c))
  pretty (Compound body) = block body
  pretty (Let x t e) =
    "let" <+> optionallyTyped x t <+> "=" <+> pretty e <> ";"
  pretty (Const x t e) =
    "const" <+> optionallyTyped x t <+> "=" <+> pretty e <> ";"
  pretty (Var x t e) =
    "var" <+> optionallyTyped x t <> prettyInit e <> ";"
  pretty (Assign lhs op e) = pretty lhs <+> pretty op <+> pretty e <> ";"
  pretty (PhonyAssign e) = "_" <+> "=" <+> pretty e <> ";"
  pretty (Increment e) = pretty e <> "++;"
  pretty (Decrement e) = pretty e <> "--;"
  pretty (ConstAssert e) = "const_assert" <+> pretty e <> ";"
  pretty (If cond th elifs el) =
    hsep $
      ["if" <+> pretty cond <+> block th]
        ++ map elseIf elifs
        ++ maybe [] (\body -> ["else" <+> block body]) el
    where
      elseIf (c, body) = "else if" <+> pretty c <+> block body
  pretty (Switch e clauses) =
    "switch" <+> pretty e <+> braceBlock (map pretty (NE.toList clauses))
  pretty (Loop body continuing) =
    "loop"
      <+> braceBlock (map pretty body ++ maybe [] (pure . pretty) continuing)
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
      <+> block body
  pretty (While cond body) = "while" <+> pretty cond <+> block body
  pretty (Call f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args) <> ";"
  pretty (Return Nothing) = "return;"
  pretty (Return (Just e)) = "return" <+> pretty e <> ";"
  pretty Break = "break;"
  pretty Continue = "continue;"
  pretty Discard = "discard;"

instance Pretty SwitchClause where
  pretty (Case selectors body) =
    "case"
      <+> commasep (map pretty (NE.toList selectors))
      <> ":"
      <+> block body
  pretty (DefaultCase body) = "default:" <+> block body

instance Pretty CaseSelector where
  pretty (CaseExp e) = pretty e
  pretty CaseDefault = "default"

instance Pretty Continuing where
  pretty (Continuing body breakIf) =
    "continuing"
      <+> braceBlock
        ( map pretty body
            ++ maybe [] (\e -> ["break if" <+> pretty e <> ";"]) breakIf
        )

-- | A brace-enclosed statement sequence, as every block in WGSL is.  See
-- https://www.w3.org/TR/WGSL/#compound-statement-section.
block :: [Statement] -> Doc a
block = braceBlock . map pretty

-- | An @optionally_typed_ident@.
optionallyTyped :: Ident -> Maybe Type -> Doc a
optionallyTyped x t = pretty x <> maybe mempty ((" :" <+>) . pretty) t

-- | An optional initialiser.
prettyInit :: Maybe Expression -> Doc a
prettyInit = maybe mempty ((" =" <+>) . pretty)

instance Pretty AssignmentOperator where
  pretty AssignSimple = "="
  pretty AssignAdd = "+="
  pretty AssignSubtract = "-="
  pretty AssignMultiply = "*="
  pretty AssignDivide = "/="
  pretty AssignRemainder = "%="
  pretty AssignBitwiseAnd = "&="
  pretty AssignBitwiseOr = "|="
  pretty AssignBitwiseExclusiveOr = "^="
  pretty AssignShiftLeft = "<<="
  pretty AssignShiftRight = ">>="

-- | A brace-enclosed, indented sequence.
braceBlock :: [Doc a] -> Doc a
braceBlock [] = "{ }"
braceBlock docs = "{" </> indent 2 (stack docs) </> "}"

instance Pretty ForUpdate where
  pretty (UpdateAssign lhs op e) = pretty lhs <+> pretty op <+> pretty e
  pretty (UpdatePhonyAssign e) = "_" <+> "=" <+> pretty e
  pretty (UpdateIncrement e) = pretty e <> "++"
  pretty (UpdateDecrement e) = pretty e <> "--"
  pretty (UpdateCall f targs args) =
    pretty f <> templateList targs <> parens (commasep $ map pretty args)

instance Pretty Attribute where
  pretty (Attribute name []) = "@" <> pretty name
  pretty (Attribute name args) =
    "@" <> pretty name <> parens (commasep $ map pretty args)

instance Pretty Param where
  pretty (Param attribs name typ) =
    hsep $ map pretty attribs ++ [pretty name <+> ":" <+> pretty typ]

prettyParams :: [Param] -> Doc a
prettyParams [] = "()"
prettyParams params = "(" </> indent 2 (commastack (map pretty params)) </> ")"

instance Pretty FunctionDecl where
  pretty (FunctionDecl name attribs params ret body) =
    prettyAttribs attribs <> header <+> block body
    where
      header = case ret of
        Nothing -> "fn" <+> pretty name <> prettyParams params
        Just (ret_attribs, t) ->
          "fn"
            <+> pretty name
            <> prettyParams params
            <+> "->"
            <+> hsep (map pretty ret_attribs ++ [pretty t])

-- | Attributes, followed by a line break if there are any at all.
prettyAttribs :: [Attribute] -> Doc a
prettyAttribs [] = mempty
prettyAttribs attribs = hsep (map pretty attribs) <> line

instance Pretty StructMember where
  pretty (StructMember attribs name typ) =
    hsep $ map pretty attribs ++ [pretty name <+> ":" <+> pretty typ]

instance Pretty StructDecl where
  pretty (StructDecl name fields) =
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

instance Pretty Severity where
  pretty Error = "error"
  pretty Warning = "warning"
  pretty Info = "info"
  pretty Off = "off"

instance Pretty EnableExtension where
  pretty EnableF16 = "f16"
  pretty EnableClipDistances = "clip_distances"
  pretty EnableDualSourceBlending = "dual_source_blending"
  pretty EnableSubgroups = "subgroups"
  pretty EnablePrimitiveIndex = "primitive_index"
  pretty EnableSubgroupSizeControl = "subgroup_size_control"

instance Pretty LanguageExtension where
  pretty ReadonlyAndReadwriteStorageTextures =
    "readonly_and_readwrite_storage_textures"
  pretty Packed4x8IntegerDotProduct = "packed_4x8_integer_dot_product"
  pretty UnrestrictedPointerParameters = "unrestricted_pointer_parameters"
  pretty PointerCompositeAccess = "pointer_composite_access"
  pretty UniformBufferStandardLayout = "uniform_buffer_standard_layout"
  pretty SubgroupId = "subgroup_id"
  pretty SubgroupUniformity = "subgroup_uniformity"
  pretty TextureAndSamplerLet = "texture_and_sampler_let"
  pretty TextureFormatsTier1 = "texture_formats_tier1"
  pretty LinearIndexing = "linear_indexing"
  pretty ImmediateAddressSpace = "immediate_address_space"
  pretty FragmentDepth = "fragment_depth"
  pretty BufferView = "buffer_view"
  pretty SwizzleAssignment = "swizzle_assignment"

instance Pretty Directive where
  pretty (Enable exts) =
    "enable" <+> commasep (map pretty (NE.toList exts)) <> ";"
  pretty (Requires exts) =
    "requires" <+> commasep (map pretty (NE.toList exts)) <> ";"
  pretty (Diagnostic severity rule) =
    "diagnostic"
      <> parens (pretty severity <> "," <+> pretty rule)
      <> ";"

instance Pretty Module where
  pretty (Module directives decls) =
    stack $ map pretty directives ++ map pretty decls

instance Pretty GlobalDecl where
  pretty (GlobalFunction fun) = pretty fun
  pretty (GlobalStruct struct) = pretty struct
  pretty (GlobalVariable attribs as am name typ e) =
    prettyAttribs attribs
      <> "var<"
      <> pretty as
      <> maybe "" ((", " <>) . pretty) am
      <> ">"
      <+> optionallyTyped name typ
      <> prettyInit e
      <> ";"
  pretty (GlobalOverride attribs name typ e) =
    prettyAttribs attribs
      <> "override"
      <+> optionallyTyped name typ
      <> prettyInit e
      <> ";"
  pretty (GlobalConst name typ e) =
    "const" <+> optionallyTyped name typ <+> "=" <+> pretty e <> ";"
  pretty (GlobalAlias name t) =
    "alias" <+> pretty name <+> "=" <+> pretty t <> ";"
  pretty (GlobalAssert e) = "const_assert" <+> pretty e <> ";"
