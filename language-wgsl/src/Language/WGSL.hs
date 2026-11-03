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
-- To produce WGSL code as text, use the 'Pretty' definition.
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
    Exp (..),
    BinOp (..),
    UnOp (..),
    IntLitType (..),
    FloatLitType (..),
    intExp,
    floatExp,
    bitcast,

    -- * Statements
    Stmt (..),
    AssignOp (..),
    ForUpdate (..),
    SwitchClause (..),
    CaseSelector (..),
    Continuing (..),

    -- * Declarations
    Attrib (..),
    bindingAttribs,
    Param (..),
    FunDecl (..),
    Field (..),
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

-- | A WGSL identifier. We assume (but do not check) that these are valid WGSL
-- identifiers. The printer will certainly produce invalid WGSL if this
-- constraint is violated.
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
    Array Type (Maybe Exp)
  | Ptr Type AddressSpace (Maybe AccessMode)
  | -- | A structure or type alias, referred to by name. Its definition is not
    -- known here, so it has no layout.
    Named Ident
  | -- | A sampled texture type.  The sampled type must be @f32@, @i32@ or
    -- @u32@.  See https://www.w3.org/TR/WGSL/#sampled-texture-type.
    SampledTexture TextureDim Type
  | -- | A multisampled texture type, which is only ever two-dimensional.  See
    -- https://www.w3.org/TR/WGSL/#multisampled-texture-type.
    MultisampledTexture Type
  | -- | An external sampled texture type.  See
    -- https://www.w3.org/TR/WGSL/#external-texture-type.
    ExternalTexture
  | -- | A storage texture type.  See
    -- https://www.w3.org/TR/WGSL/#texture-storage.
    StorageTexture StorageTextureDim TexelFormat AccessMode
  | -- | A depth texture type.  See
    -- https://www.w3.org/TR/WGSL/#texture-depth.
    DepthTexture DepthTexture
  | -- | A sampler.  See https://www.w3.org/TR/WGSL/#sampler-type.
    Sampler
  | -- | A comparison sampler.
    SamplerComparison
  deriving (Eq, Ord, Show)

-- | The dimensionality and arrangement of a sampled texture.
data TextureDim
  = Tex1d
  | Tex2d
  | Tex2dArray
  | Tex3d
  | TexCube
  | TexCubeArray
  deriving (Eq, Ord, Show)

-- | The dimensionality and arrangement of a storage texture.
data StorageTextureDim
  = StorageTex1d
  | StorageTex2d
  | StorageTex2dArray
  | StorageTex3d
  deriving (Eq, Ord, Show)

-- | A depth texture type, which is never parameterised.
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

-- | Round @n@ up to the next multiple of @k@, as WGSL specifies: @roundUp(k,
-- n)@ is @ceiling(n / k) * k@, defined for positive integers. The alignment and
-- size rules are phrased in terms of this function, so a caller computing its
-- own offsets or strides should round with it rather than by hand. See
-- https://www.w3.org/TR/WGSL/#roundup.
roundUp :: Int -> Int -> Int
roundUp k n = ((n + k - 1) `div` k) * k

-- | Round @n@ down to the previous multiple of @k@, as WGSL specifies:
-- @roundDown(k, n)@ is @floor(n / k) * k@, defined for positive integers. None
-- of the layout rules use it; the specification needs it only to size the array
-- of a buffer view. See https://www.w3.org/TR/WGSL/#rounddown.
roundDown :: Int -> Int -> Int
roundDown k n = (n `div` k) * k

-- | @AlignOf@ of a type, or 'Nothing' if it has none, which is the case for
-- pointers and for the opaque texture and sampler types, and for a 'Named'
-- type, which is not resolved here. Note that this is defined for types that
-- are not 'hostShareable' but do have a fixed footprint, such as @bool@. See
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
alignOf SampledTexture {} = Nothing
alignOf (MultisampledTexture _) = Nothing
alignOf ExternalTexture = Nothing
alignOf StorageTexture {} = Nothing
alignOf (DepthTexture _) = Nothing
alignOf Sampler = Nothing
alignOf SamplerComparison = Nothing

-- | @SizeOf@ of a type, or 'Nothing' if it has none.  An array has a size only
-- if its element count is a literal: a runtime-sized array has no size until a
-- buffer is bound, and an overridable count is not known until pipeline
-- creation.
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
sizeOf (Array t (Just (IntExp n _)))
  | n >= 0 = (n *) <$> strideOf t
sizeOf Array {} = Nothing
sizeOf Ptr {} = Nothing
sizeOf (Named _) = Nothing
sizeOf SampledTexture {} = Nothing
sizeOf (MultisampledTexture _) = Nothing
sizeOf ExternalTexture = Nothing
sizeOf StorageTexture {} = Nothing
sizeOf (DepthTexture _) = Nothing
sizeOf Sampler = Nothing
sizeOf SamplerComparison = Nothing

-- | @StrideOf@ of an array with the given element type: the distance from the
-- start of one element to the start of the next, which is the element size
-- rounded up to the element alignment. See
-- https://www.w3.org/TR/WGSL/#internal-value-layout.
strideOf :: Type -> Maybe Int
strideOf t = roundUp <$> alignOf t <*> sizeOf t

-- | Whether a type is host-shareable, and so may be the store type of a
-- uniform or storage buffer.  This is conservative: a named type is not
-- resolved here, so it is reported as not host-shareable.  See
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
hostShareable (Array t (Just (IntExp _ _))) = hostShareable t
hostShareable Array {} = False
hostShareable Ptr {} = False
hostShareable (Named _) = False
hostShareable SampledTexture {} = False
hostShareable (MultisampledTexture _) = False
hostShareable ExternalTexture = False
hostShareable StorageTexture {} = False
hostShareable (DepthTexture _) = False
hostShareable Sampler = False
hostShareable SamplerComparison = False

-- | @AlignOfMember@ of a structure member: the argument of its @\@align@
-- attribute if it has one, and otherwise the @AlignOf@ of its type. REturns
-- 'Nothing' if the alignment is a non-literal. See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
alignOfMember :: Field -> Maybe Int
alignOfMember (Field attribs _ t) =
  case [args | Attrib name args <- attribs, name == "align"] of
    [] -> alignOf t
    [[IntExp k _]] -> Just k
    _ -> Nothing

-- | @SizeOfMember@ of a structure member: the argument of its @\@size@
-- attribute if it has one, and otherwise the @SizeOf@ of its type, with
-- 'Nothing' arising as for 'alignOfMember'. WGSL requires the argument to be
-- at least the @SizeOf@ of the member's type, which is not checked here. See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
sizeOfMember :: Field -> Maybe Int
sizeOfMember (Field attribs _ t) =
  case [args | Attrib name args <- attribs, name == "size"] of
    [] -> sizeOf t
    [[IntExp k _]] -> Just k
    _ -> Nothing

-- | @AlignOf@ of a structure with the given members, which is the greatest
-- @AlignOfMember@ of any of them. 'Nothing' if there are no members or if some
-- member has no @AlignOfMember@. See
-- https://www.w3.org/TR/WGSL/#structure-member-layout.
alignOfStruct :: [Field] -> Maybe Int
alignOfStruct [] = Nothing
alignOfStruct members = maximum <$> mapM alignOfMember members

-- | @OffsetOfMember@ of every member of a structure with the given members.
-- These are known even when the structure has no @SizeOf@, as when its last
-- member is a runtime-sized array, because only the preceding members
-- contribute.
offsetOfMembers :: [Field] -> Maybe [Int]
offsetOfMembers [] = Nothing
offsetOfMembers members = do
  aligns <- mapM alignOfMember $ drop 1 members
  sizes <- mapM sizeOfMember $ init members
  pure $
    scanl
      (\prev_off (al, prev_sz) -> roundUp al (prev_off + prev_sz))
      0
      (zip aligns sizes)

-- | @SizeOf@ of a structure with the given members, which is the end of its
-- last member rounded up to the structure's alignment.  'Nothing' if any
-- member has no @SizeOfMember@, which includes a trailing runtime-sized
-- array.
sizeOfStruct :: [Field] -> Maybe Int
sizeOfStruct members = do
  struct_align <- alignOfStruct members
  offsets <- offsetOfMembers members
  last_size <- sizeOfMember $ last members
  pure $ roundUp struct_align (last offsets + last_size)

-- | A binary operator. See https://www.w3.org/TR/WGSL/#expressions.
data BinOp
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

-- | A unary operator.  'Indirection' and 'AddressOf' are the only ones that
-- may also appear in an assignment's left-hand side.
data UnOp
  = Negate
  | LogicalNegate
  | BitwiseComplement
  | Indirection
  | AddressOf
  deriving (Eq, Ord, Show)

-- | The type denoted by an integer literal, which is determined by its
-- suffix.  See https://www.w3.org/TR/WGSL/#numeric-literals.
data IntLitType
  = -- | No suffix.
    AbstractInt
  | -- | The @i@ suffix.
    LitI32
  | -- | The @u@ suffix.
    LitU32
  deriving (Eq, Ord, Show)

-- | The type denoted by a floating point literal, which is determined by its
-- suffix.
data FloatLitType
  = -- | No suffix.
    AbstractFloat
  | -- | The @f@ suffix.
    LitF32
  | -- | The @h@ suffix.  Using it requires the @enable f16;@ directive.
    LitF16
  deriving (Eq, Ord, Show)

data Exp
  = BoolExp Bool
  | -- | An integer literal.  Prefer 'intExp', which checks that the value can
    -- be denoted by a literal of the given type.
    IntExp Int IntLitType
  | -- | A floating point literal.  Prefer 'floatExp'.
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
  deriving (Eq, Ord, Show)

data Stmt
  = -- | The empty statement.
    Empty
  | -- | This technically is not a statement in WGSL, but it is useful for
    -- legibility of generated code. Produces a line comment. The comment may
    -- have linebreaks.
    Comment T.Text
  | -- | A compound statement, which introduces a new scope.
    Compound [Stmt]
  | -- | @let x = e@, with an optional type.
    Let Ident (Maybe Type) Exp
  | -- | @const x = e@, with an optional type.
    Const Ident (Maybe Type) Exp
  | -- | @var x : T = e@.  At least one of the type and the initialiser must be
    -- present, or the type of the variable is not known.
    DeclareVar Ident (Maybe Type) (Maybe Exp)
  | -- | The left-hand side must be an assignable expression.  See
    -- https://www.w3.org/TR/WGSL/#assignment.
    Assign Exp AssignOp Exp
  | -- | @_ = e@, which evaluates @e@ and discards the result.
    PhonyAssign Exp
  | Increment Exp
  | Decrement Exp
  | -- | A @const_assert@ statement.  'GlobalAssert' is its module-scope
    -- counterpart.
    ConstAssert Exp
  | -- | An @if@, its @else if@ clauses, and its optional @else@ clause.
    If Exp [Stmt] [(Exp, [Stmt])] (Maybe [Stmt])
  | Switch Exp (NonEmpty SwitchClause)
  | -- | A @loop@, with its optional @continuing@ block.
    Loop [Stmt] (Maybe Continuing)
  | For Ident Exp Exp ForUpdate [Stmt]
  | While Exp [Stmt]
  | Break
  | Continue
  | Discard
  | Call Ident [Exp] [Exp]
  | -- | The expression must be present exactly when the enclosing function
    -- has a return type.
    Return (Maybe Exp)
  deriving (Eq, Ord, Show)

-- | A clause of a @switch@ statement.  See
-- https://www.w3.org/TR/WGSL/#switch-statement.
data SwitchClause
  = -- | A @case@ with one or more selectors.
    Case (NonEmpty CaseSelector) [Stmt]
  | -- | A @default@ clause on its own.  Equivalently, 'Case' may list
    -- 'CaseDefault' among its selectors.
    DefaultCase [Stmt]
  deriving (Eq, Ord, Show)

-- | A selector of a @case@ clause.
data CaseSelector
  = CaseExp Exp
  | CaseDefault
  deriving (Eq, Ord, Show)

-- | The @continuing@ block of a @loop@, which may end in a @break if@.  See
-- https://www.w3.org/TR/WGSL/#continuing-statement.
data Continuing = Continuing [Stmt] (Maybe Exp)
  deriving (Eq, Ord, Show)

-- | The operator of an assignment: plain assignment, or one of the compound
-- assignment operators.  See https://www.w3.org/TR/WGSL/#assignment.
data AssignOp
  = SimpleAssign
  | AddAssign
  | SubtractAssign
  | MultiplyAssign
  | DivideAssign
  | RemainderAssign
  | BitwiseAndAssign
  | BitwiseOrAssign
  | BitwiseExclusiveOrAssign
  | ShiftLeftAssign
  | ShiftRightAssign
  deriving (Eq, Ord, Show)

-- | The update of a @for@ loop, which the grammar restricts to the statements
-- that update a variable, or to a function call. These mirror the
-- correspondingly named statements. See
-- https://www.w3.org/TR/WGSL/#for-statement.
data ForUpdate
  = -- | The left-hand side must be an assignable expression.
    UpdateAssign Exp AssignOp Exp
  | UpdatePhonyAssign Exp
  | UpdateIncrement Exp
  | UpdateDecrement Exp
  | UpdateCall Ident [Exp] [Exp]
  deriving (Eq, Ord, Show)

data Attrib = Attrib Ident [Exp]
  deriving (Eq, Ord, Show)

data Param = Param Ident Type [Attrib]
  deriving (Eq, Ord, Show)

-- | A function declaration.  See
-- https://www.w3.org/TR/WGSL/#function-declaration-sec.
data FunDecl = FunDecl
  { funName :: Ident,
    funAttribs :: [Attrib],
    funParams :: [Param],
    funReturn :: Maybe ([Attrib], Type),
    funBody :: [Stmt]
  }
  deriving (Eq, Ord, Show)

data Field = Field [Attrib] Ident Type
  deriving (Eq, Ord, Show)

data StructDecl = StructDecl Ident [Field]
  deriving (Eq, Ord, Show)

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
data GlobalDecl
  = GlobalFun FunDecl
  | GlobalStruct StructDecl
  | -- | The access mode may only be given along with the address space, and
    -- defaults to the one of the address space when omitted.
    GlobalVar [Attrib] AddressSpace (Maybe AccessMode) Ident Type
  | GlobalOverride [Attrib] Ident Type (Maybe Exp)
  | -- | A module-scope @const_assert@.
    GlobalAssert Exp
  deriving (Eq, Ord, Show)

-- | A WGSL module.  Directives must precede all declarations, which this
-- representation enforces.  See
-- https://www.w3.org/TR/WGSL/#wgsl-module-definition.
data Module = Module [Directive] [GlobalDecl]
  deriving (Eq, Ord, Show)

-- | A directive.  See https://www.w3.org/TR/WGSL/#directives.
data Directive
  = Enable (NonEmpty EnableExtension)
  | Requires (NonEmpty LanguageExtension)
  | -- | A diagnostic filter.  The rule name is not a closed set, as
    -- implementations may provide their own, so it is spelled out.
    Diagnostic Severity Ident
  deriving (Eq, Ord, Show)

-- | The severity of a diagnostic, in the specification's order from greatest
-- to least.  See https://www.w3.org/TR/WGSL/#diagnostics.
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
      LitI32 -> magnitude <= 2 ^ (31 :: Int) - 1
      LitU32 -> toInteger v >= 0 && toInteger v <= 2 ^ (32 :: Int) - 1
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
      -- Any finite Double denotes an AbstractFloat.
      AbstractFloat -> Nothing
      LitF32 -> Just 3.40282346638528859812e38
      LitF16 -> Just 65504

-- | Reinterpret the bits of an expression as another type.  See
-- https://www.w3.org/TR/WGSL/#bitcast-builtin.
bitcast :: Type -> Exp -> Exp
bitcast t e = CallExp "bitcast" [TypeExp t] [e]

-- | A template argument list, which is omitted when empty.
templateArgs :: [Doc a] -> Doc a
templateArgs [] = mempty
templateArgs args = "<" <> commasep args <> ">"

templateList :: [Exp] -> Doc a
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
  pretty (SampledTexture dim t) = generator (textureDim dim) [pretty t]
  pretty (MultisampledTexture t) =
    generator "texture_multisampled_2d" [pretty t]
  pretty ExternalTexture = "texture_external"
  pretty (StorageTexture dim fmt am) =
    generator (storageTextureDim dim) [pretty fmt, pretty am]
  pretty (DepthTexture d) = pretty d
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

intSuffix :: IntLitType -> Doc a
intSuffix AbstractInt = mempty
intSuffix LitI32 = "i"
intSuffix LitU32 = "u"

floatSuffix :: FloatLitType -> Doc a
floatSuffix AbstractFloat = mempty
floatSuffix LitF32 = "f"
floatSuffix LitF16 = "h"

instance Pretty BinOp where
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

instance Pretty UnOp where
  pretty Negate = "-"
  pretty LogicalNegate = "!"
  pretty BitwiseComplement = "~"
  pretty Indirection = "*"
  pretty AddressOf = "&"

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
  pretty Empty = ";"
  pretty (Comment c) = vsep (map ("//" <+>) (pretty <$> T.lines c))
  pretty (Compound body) = block body
  pretty (Let x t e) =
    "let" <+> optionallyTyped x t <+> "=" <+> pretty e <> ";"
  pretty (Const x t e) =
    "const" <+> optionallyTyped x t <+> "=" <+> pretty e <> ";"
  pretty (DeclareVar x t e) =
    "var"
      <+> optionallyTyped x t
      <> maybe mempty ((" =" <+>) . pretty) e
      <> ";"
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
block :: [Stmt] -> Doc a
block = braceBlock . map pretty

-- | An @optionally_typed_ident@.
optionallyTyped :: Ident -> Maybe Type -> Doc a
optionallyTyped x t = pretty x <> maybe mempty ((" :" <+>) . pretty) t

instance Pretty AssignOp where
  pretty SimpleAssign = "="
  pretty AddAssign = "+="
  pretty SubtractAssign = "-="
  pretty MultiplyAssign = "*="
  pretty DivideAssign = "/="
  pretty RemainderAssign = "%="
  pretty BitwiseAndAssign = "&="
  pretty BitwiseOrAssign = "|="
  pretty BitwiseExclusiveOrAssign = "^="
  pretty ShiftLeftAssign = "<<="
  pretty ShiftRightAssign = ">>="

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
prettyAttribs :: [Attrib] -> Doc a
prettyAttribs [] = mempty
prettyAttribs attribs = hsep (map pretty attribs) <> line

instance Pretty Field where
  pretty (Field attribs name typ) =
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
  pretty (GlobalFun fun) = pretty fun
  pretty (GlobalStruct struct) = pretty struct
  pretty (GlobalVar attribs as am name typ) =
    prettyAttribs attribs
      <> "var<"
      <> pretty as
      <> maybe "" ((", " <>) . pretty) am
      <> ">"
      <+> pretty name
      <+> ":"
      <+> pretty typ
      <> ";"
  pretty (GlobalOverride attribs name typ Nothing) =
    prettyAttribs attribs
      <> "override"
      <+> pretty name
      <+> ":"
      <+> pretty typ
      <> ";"
  pretty (GlobalAssert e) = "const_assert" <+> pretty e <> ";"
  pretty (GlobalOverride attribs name typ (Just initial)) =
    prettyAttribs attribs
      <> "override"
      <+> pretty name
      <+> ":"
      <+> pretty typ
      <+> "="
      <+> pretty initial
      <> ";"
