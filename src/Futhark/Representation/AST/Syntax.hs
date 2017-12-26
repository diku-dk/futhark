{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
-- | Futhark core language skeleton.  Concrete representations further
-- extend this skeleton by defining a "lore", which specifies concrete
-- annotations ("Futhark.Representation.AST.Annotations") and
-- semantics.
module Futhark.Representation.AST.Syntax
  (
    module Language.Futhark.Core
  , module Futhark.Representation.AST.Annotations
  , module Futhark.Representation.AST.Syntax.Core

  -- * Types
  , Uniqueness(..)
  , NoUniqueness(..)
  , Rank(..)
  , ArrayShape(..)
  , Space (..)
  , TypeBase(..)
  , Diet(..)

  -- * Values
  , PrimValue(..)
  , Value(..)

  -- * Abstract syntax tree
  , Ident (..)
  , SubExp(..)
  , Bindage (..)
  , PatElem
  , PatElemT (..)
  , PatternT (..)
  , Pattern
  , StmAux(..)
  , Stm(..)
  , Stms
  , Result
  , BodyT(..)
  , Body
  , BasicOp (..)
  , UnOp (..)
  , BinOp (..)
  , CmpOp (..)
  , ConvOp (..)
  , DimChange (..)
  , ShapeChange
  , ExpT(..)
  , Exp
  , LoopForm (..)
  , IfAttr (..)
  , IfSort (..)
  , Safety (..)
  , LambdaT(..)
  , Lambda
  , ExtLambdaT (..)
  , ExtLambda

  -- * Definitions
  , ParamT (..)
  , FParam
  , LParam
  , FunDefT (..)
  , FunDef
  , EntryPoint
  , EntryPointType(..)
  , ProgT(..)
  , Prog

  -- * Utils
  , oneStm
  , stmsFromList
  , stmsToList
  , stmsHead
  )
  where

import Data.Foldable
import Data.Loc
import qualified Data.Sequence as Seq
import qualified Data.Semigroup as Sem

import Language.Futhark.Core
import Futhark.Representation.AST.Annotations
import Futhark.Representation.AST.Syntax.Core

-- | A type alias for namespace control.
type PatElem lore = PatElemT (LetAttr lore)

-- | A pattern is conceptually just a list of names and their types.
data PatternT attr =
  Pattern { patternContextElements :: [PatElemT attr]
            -- ^ existential context (sizes and memory blocks)
          , patternValueElements   :: [PatElemT attr]
            -- ^ "real" values
          }
  deriving (Ord, Show, Eq)

instance Sem.Semigroup (PatternT lore) where
  Pattern cs1 vs1 <> Pattern cs2 vs2 = Pattern (cs1++cs2) (vs1++vs2)

instance Monoid (PatternT lore) where
  mempty = Pattern [] []
  mappend = (Sem.<>)

-- | A type alias for namespace control.
type Pattern lore = PatternT (LetAttr lore)

-- | Auxilliary Information associated with a statement.
data StmAux attr = StmAux { stmAuxCerts :: !Certificates
                          , stmAuxAttr :: attr
                          }
                  deriving (Ord, Show, Eq)

-- | A local variable binding.
data Stm lore = Let { stmPattern :: Pattern lore
                    , stmAux :: StmAux (ExpAttr lore)
                    , stmExp :: Exp lore
                    }

deriving instance Annotations lore => Ord (Stm lore)
deriving instance Annotations lore => Show (Stm lore)
deriving instance Annotations lore => Eq (Stm lore)

-- | A sequence of statements.
type Stms lore = Seq.Seq (Stm lore)

oneStm :: Stm lore -> Stms lore
oneStm = Seq.singleton

stmsFromList :: [Stm lore] -> Stms lore
stmsFromList = Seq.fromList

stmsToList :: Stms lore -> [Stm lore]
stmsToList = toList

stmsHead :: Stms lore -> Maybe (Stm lore, Stms lore)
stmsHead stms = case Seq.viewl stms of stm Seq.:< stms' -> Just (stm, stms')
                                       Seq.EmptyL       -> Nothing

-- | The result of a body is a sequence of subexpressions.
type Result = [SubExp]

-- | A body consists of a number of bindings, terminating in a result
-- (essentially a tuple literal).
data BodyT lore = Body { bodyAttr :: BodyAttr lore
                       , bodyStms :: Stms lore
                       , bodyResult :: Result
                       }

deriving instance Annotations lore => Ord (BodyT lore)
deriving instance Annotations lore => Show (BodyT lore)
deriving instance Annotations lore => Eq (BodyT lore)

-- | Type alias for namespace reasons.
type Body = BodyT

-- | The new dimension in a 'Reshape'-like operation.  This allows us to
-- disambiguate "real" reshapes, that change the actual shape of the
-- array, from type coercions that are just present to make the types
-- work out.
data DimChange d = DimCoercion d
                   -- ^ The new dimension is guaranteed to be numerically
                   -- equal to the old one.
                 | DimNew d
                   -- ^ The new dimension is not necessarily numerically
                   -- equal to the old one.
                 deriving (Eq, Ord, Show)

instance Functor DimChange where
  fmap f (DimCoercion d) = DimCoercion $ f d
  fmap f (DimNew      d) = DimNew $ f d

instance Foldable DimChange where
  foldMap f (DimCoercion d) = f d
  foldMap f (DimNew      d) = f d

instance Traversable DimChange where
  traverse f (DimCoercion d) = DimCoercion <$> f d
  traverse f (DimNew      d) = DimNew <$> f d

-- | A list of 'DimChange's, indicating the new dimensions of an array.
type ShapeChange d = [DimChange d]

-- | A primitive operation that returns something of known size and
-- does not itself contain any bindings.
data BasicOp lore
  = SubExp SubExp
    -- ^ A variable or constant.

  | Opaque SubExp
    -- ^ Semantically and operationally just identity, but is
    -- invisible/impenetrable to optimisations (hopefully).  This is
    -- just a hack to avoid optimisation (so, to work around compiler
    -- limitations).

  | ArrayLit  [SubExp] Type
    -- ^ Array literals, e.g., @[ [1+x, 3], [2, 1+4] ]@.
    -- Second arg is the element type of the rows of the array.
    -- Scalar operations

  | UnOp UnOp SubExp
    -- ^ Unary operation.

  | BinOp BinOp SubExp SubExp
    -- ^ Binary operation.

  | CmpOp CmpOp SubExp SubExp
    -- ^ Comparison - result type is always boolean.

  | ConvOp ConvOp SubExp
    -- ^ Conversion "casting".

  | Assert SubExp String (SrcLoc, [SrcLoc])
  -- ^ Turn a boolean into a certificate, halting the program with the
  -- given error message if the boolean is false.

  -- Primitive array operations

  | Index VName (Slice SubExp)
  -- ^ The certificates for bounds-checking are part of the 'Stm'.

  | Split Int [SubExp] VName
  -- ^ 2nd arg is sizes of arrays you get back, which is
  -- different from what the external language does.
  -- In the core language,
  -- @a = [1,2,3,4]; split@0( (1,0,2) , a ) = {[1], [], [2,3]}@

  | Concat Int VName [VName] SubExp
  -- ^ @concat@0([1],[2, 3, 4]) = [1, 2, 3, 4]@.

  | Copy VName
  -- ^ Copy the given array.  The result will not alias anything.

  | Manifest [Int] VName
  -- ^ Manifest an array with dimensions represented in the given
  -- order.  The result will not alias anything.

  -- Array construction.
  | Iota SubExp SubExp SubExp IntType
  -- ^ @iota(n, x, s) = [x,x+s,..,x+(n-1)*s]@.
  --
  -- The 'IntType' indicates the type of the array returned and the
  -- offset/stride arguments, but not the length argument.

  | Replicate Shape SubExp
  -- ^ @replicate([3][2],1) = [[1,1], [1,1], [1,1]]@

  | Repeat [Shape] Shape VName
  -- ^ Repeat each dimension of the input array some number of times,
  -- given by the corresponding shape.  For an array of rank @k@, the
  -- list must contain @k@ shapes.  A shape may be empty (in which
  -- case the dimension is not repeated, but it is still present).
  -- The last shape indicates the amount of extra innermost
  -- dimensions.  All other extra dimensions are added *before* the original dimension.

  | Scratch PrimType [SubExp]
  -- ^ Create array of given type and shape, with undefined elements.

  -- Array index space transformation.
  | Reshape (ShapeChange SubExp) VName
   -- ^ 1st arg is the new shape, 2nd arg is the input array *)

  | Rearrange [Int] VName
  -- ^ Permute the dimensions of the input array.  The list
  -- of integers is a list of dimensions (0-indexed), which
  -- must be a permutation of @[0,n-1]@, where @n@ is the
  -- number of dimensions in the input array.

  | Rotate [SubExp] VName
  -- ^ Rotate the dimensions of the input array.  The list of
  -- subexpressions specify how much each dimension is rotated.  The
  -- length of this list must be equal to the rank of the array.

  | Partition Int VName [VName]
    -- ^ First variable is the flag array, second is the element
    -- arrays.  If no arrays are given, the returned offsets are zero,
    -- and no arrays are returned.
  deriving (Eq, Ord, Show)

-- | The root Futhark expression type.  The 'Op' constructor contains
-- a lore-specific operation.  Do-loops, branches and function calls
-- are special.  Everything else is a simple 'BasicOp'.
data ExpT lore
  = BasicOp (BasicOp lore)
    -- ^ A simple (non-recursive) operation.

  | Apply  Name [(SubExp, Diet)] [RetType lore] (Safety, SrcLoc, [SrcLoc])

  | If     SubExp (BodyT lore) (BodyT lore) (IfAttr (BranchType lore))

  | DoLoop [(FParam lore, SubExp)] [(FParam lore, SubExp)] (LoopForm lore) (BodyT lore)
    -- ^ @loop {a} = {v} (for i < n|while b) do b@.  The merge
    -- parameters are divided into context and value part.

  | Op (Op lore)

deriving instance Annotations lore => Eq (ExpT lore)
deriving instance Annotations lore => Show (ExpT lore)
deriving instance Annotations lore => Ord (ExpT lore)

-- | Whether something is safe or unsafe (mostly function calls, and
-- in the context of whether operations are dynamically checked).
-- When we inline an 'Unsafe' function, we remove all safety checks in
-- its body.  The 'Ord' instance picks 'Unsafe' as being less than
-- 'Safe'.
data Safety = Unsafe | Safe deriving (Eq, Ord, Show)

-- | For-loop or while-loop?
data LoopForm lore = ForLoop VName IntType SubExp [(LParam lore,VName)]
                   | WhileLoop VName

deriving instance Annotations lore => Eq (LoopForm lore)
deriving instance Annotations lore => Show (LoopForm lore)
deriving instance Annotations lore => Ord (LoopForm lore)

-- | Data associated with a branch.
data IfAttr rt = IfAttr { ifReturns :: [rt]
                        , ifSort :: IfSort
                        }
                 deriving (Eq, Show, Ord)

data IfSort = IfNormal -- ^ An ordinary branch.
            | IfFallback -- ^ A branch where the "true" case is what
                         -- we are actually interested in, and the
                         -- "false" case is only present as a fallback
                         -- for when the true case cannot be safely
                         -- evaluated.  the compiler is permitted to
                         -- optimise away the branch if the true case
                         -- contains only safe statements.
            deriving (Eq, Show, Ord)

-- | A type alias for namespace control.
type Exp = ExpT

-- | Anonymous function for use in a SOAC.
data LambdaT lore =
  Lambda { lambdaParams     :: [LParam lore]
         , lambdaBody       :: BodyT lore
         , lambdaReturnType :: [Type]
         }

deriving instance Annotations lore => Eq (LambdaT lore)
deriving instance Annotations lore => Show (LambdaT lore)
deriving instance Annotations lore => Ord (LambdaT lore)

-- | Type alias for namespacing reasons.
type Lambda = LambdaT

-- | Anonymous function for use in a SOAC, with an existential return
-- type.
data ExtLambdaT lore =
  ExtLambda { extLambdaParams     :: [LParam lore]
            , extLambdaBody       :: BodyT lore
            , extLambdaReturnType :: [ExtType]
            }

deriving instance Annotations lore => Eq (ExtLambdaT lore)
deriving instance Annotations lore => Show (ExtLambdaT lore)
deriving instance Annotations lore => Ord (ExtLambdaT lore)

type ExtLambda = ExtLambdaT

type FParam lore = ParamT (FParamAttr lore)

type LParam lore = ParamT (LParamAttr lore)

-- | Function Declarations
data FunDefT lore = FunDef { funDefEntryPoint :: Maybe EntryPoint
                             -- ^ Contains a value if this function is
                             -- an entry point.
                           , funDefName :: Name
                           , funDefRetType :: [RetType lore]
                           , funDefParams :: [FParam lore]
                           , funDefBody :: BodyT lore
                           }

deriving instance Annotations lore => Eq (FunDefT lore)
deriving instance Annotations lore => Show (FunDefT lore)
deriving instance Annotations lore => Ord (FunDefT lore)

-- | Information about the parameters and return value of an entry
-- point.  The first element is for parameters, the second for return
-- value.
type EntryPoint = ([EntryPointType], [EntryPointType])

-- | Every entry point argument and return value has an annotation
-- indicating how it maps to the original source program type.
data EntryPointType = TypeUnsigned
                      -- ^ Is an unsigned integer or array of unsigned
                      -- integers.
                    | TypeOpaque String Int
                      -- ^ A black box type comprising this many core
                      -- values.  The string is a human-readable
                      -- description with no other semantics.
                    | TypeDirect
                      -- ^ Maps directly.
                    deriving (Eq, Show, Ord)

-- | Type alias for namespace reasons.
type FunDef = FunDefT

-- | An entire Futhark program.
newtype ProgT lore = Prog { progFunctions :: [FunDef lore] }
                     deriving (Eq, Ord, Show)

-- | Type alias for namespace reasons.
type Prog = ProgT
