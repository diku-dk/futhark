{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
-- | Facilities for determining which names are used in some syntactic
-- construct.  The most important interface is the 'FreeIn' class and
-- its instances, but for reasons related to the Haskell type system,
-- some constructs have specialised functions.
module Futhark.Representation.AST.Attributes.Names
       ( -- * Free names
         Names
       , nameIn
       , oneName
       , namesFromList
       , namesToList
       , namesIntersection
       , namesIntersect
       , namesSubtract
       , mapNames
       -- * Class
       , FreeIn (..)
       , freeIn
       -- * Specialised Functions
       , freeInStmsAndRes
       -- * Bound Names
       , boundInBody
       , boundByStm
       , boundByStms
       , boundByLambda
       -- * Efficient computation
       , FreeAttr(..)
       , FV
       , fvBind
       , fvName
       , fvNames
       )
       where

import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Foldable

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Traversals
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Scope
import Futhark.Util.Pretty

-- | A set of names.
newtype Names = Names { unNames :: IM.IntMap VName }
              deriving (Eq, Show)

instance Semigroup Names where
  vs1 <> vs2 = Names $ unNames vs1 <> unNames vs2

instance Monoid Names where
  mempty = Names mempty

instance Pretty Names where
  ppr = ppr . namesToList

-- | Does the set of names contain this name?
nameIn :: VName -> Names -> Bool
nameIn v (Names vs) = baseTag v `IM.member` vs

-- | Construct a name set from a list.  Slow.
namesFromList :: [VName] -> Names
namesFromList vs = Names $ IM.fromList $ zip (map baseTag vs) vs

-- | Turn a name set into a list of names.  Slow.
namesToList :: Names -> [VName]
namesToList = IM.elems . unNames

-- | Construct a name set from a single name.
oneName :: VName -> Names
oneName v = Names $ IM.singleton (baseTag v) v

-- | The intersection of two name sets.
namesIntersection :: Names -> Names -> Names
namesIntersection (Names vs1) (Names vs2) = Names $ IM.intersection vs1 vs2

-- | Do the two name sets intersect?
namesIntersect :: Names -> Names -> Bool
namesIntersect vs1 vs2 = not $ IM.disjoint (unNames vs1) (unNames vs2)

-- | Subtract the latter name set from the former.
namesSubtract :: Names -> Names -> Names
namesSubtract (Names vs1) (Names vs2) = Names $ IM.difference vs1 vs2

-- | Map over the names in a set.
mapNames :: (VName -> VName) -> Names -> Names
mapNames f vs = namesFromList $ map f $ namesToList vs

-- | A computation to build a free variable set.
newtype FV = FV { unFV :: Names }
-- Right now the variable set is just stored explicitly, without the
-- fancy functional representation that GHC uses.  Turns out it's
-- faster this way.

instance Monoid FV where
  mempty = FV mempty

instance Semigroup FV where
  FV fv1 <> FV fv2 = FV $ fv1 <> fv2

-- | Consider a variable to be bound in the given 'FV' computation.
fvBind :: Names -> FV -> FV
fvBind vs (FV fv) = FV $ fv `namesSubtract` vs

-- | Take note of a variable reference.
fvName :: VName -> FV
fvName v = FV $ oneName v

-- | Take note of a set of variable references.
fvNames :: Names -> FV
fvNames = FV

freeWalker :: (FreeAttr (ExpAttr lore),
               FreeAttr (BodyAttr lore),
               FreeIn (FParamAttr lore),
               FreeIn (LParamAttr lore),
               FreeIn (LetAttr lore),
               FreeIn (Op lore)) =>
              Walker lore (State FV)
freeWalker = identityWalker {
               walkOnSubExp = modify . (<>) . freeIn'
             , walkOnBody = modify . (<>) . freeIn'
             , walkOnVName = modify . (<>) . fvName
             , walkOnOp = modify . (<>) . freeIn'
             }

-- | Return the set of variable names that are free in the given
-- statements and result.  Filters away the names that are bound by
-- the statements.
freeInStmsAndRes :: (FreeIn (Op lore),
                     FreeIn (LetAttr lore),
                     FreeIn (LParamAttr lore),
                     FreeIn (FParamAttr lore),
                     FreeAttr (BodyAttr lore),
                     FreeAttr (ExpAttr lore)) =>
                    Stms lore -> Result -> FV
freeInStmsAndRes stms res =
  fvBind (boundByStms stms) $ foldMap freeIn' stms <> freeIn' res

-- | A class indicating that we can obtain free variable information
-- from values of this type.
class FreeIn a where
  freeIn' :: a -> FV
  freeIn' = fvNames . freeIn

-- | The free variables of some syntactic construct.
freeIn :: FreeIn a => a -> Names
freeIn = unFV . freeIn'

instance FreeIn FV where
  freeIn' = id

instance FreeIn () where
  freeIn' () = mempty

instance FreeIn Int where
  freeIn' = const mempty

instance (FreeIn a, FreeIn b) => FreeIn (a,b) where
  freeIn' (a,b) = freeIn' a <> freeIn' b

instance (FreeIn a, FreeIn b, FreeIn c) => FreeIn (a,b,c) where
  freeIn' (a,b,c) = freeIn' a <> freeIn' b <> freeIn' c

instance FreeIn a => FreeIn [a] where
  freeIn' = foldMap freeIn'

instance (FreeAttr (ExpAttr lore),
          FreeAttr (BodyAttr lore),
          FreeIn (FParamAttr lore),
          FreeIn (LParamAttr lore),
          FreeIn (LetAttr lore),
          FreeIn (Op lore)) => FreeIn (Lambda lore) where
  freeIn' (Lambda params body rettype) =
    fvBind (namesFromList $ map paramName params) $
    freeIn' rettype <> freeIn' params <> freeIn' body

instance (FreeAttr (ExpAttr lore),
          FreeAttr (BodyAttr lore),
          FreeIn (FParamAttr lore),
          FreeIn (LParamAttr lore),
          FreeIn (LetAttr lore),
          FreeIn (Op lore)) => FreeIn (Body lore) where
  freeIn' (Body attr stms res) =
    precomputed attr $ freeIn' attr <> freeInStmsAndRes stms res

instance (FreeAttr (ExpAttr lore),
          FreeAttr (BodyAttr lore),
          FreeIn (FParamAttr lore),
          FreeIn (LParamAttr lore),
          FreeIn (LetAttr lore),
          FreeIn (Op lore)) => FreeIn (Exp lore) where
  freeIn' (DoLoop ctxmerge valmerge form loopbody) =
    let (ctxparams, ctxinits) = unzip ctxmerge
        (valparams, valinits) = unzip valmerge
        bound_here = namesFromList $ M.keys $
                     scopeOf form <>
                     scopeOfFParams (ctxparams ++ valparams)
    in fvBind bound_here $
       freeIn' (ctxinits ++ valinits) <> freeIn' form <>
       freeIn' (ctxparams ++ valparams) <> freeIn' loopbody
  freeIn' e = execState (walkExpM freeWalker e) mempty

instance (FreeAttr (ExpAttr lore),
          FreeAttr (BodyAttr lore),
          FreeIn (FParamAttr lore),
          FreeIn (LParamAttr lore),
          FreeIn (LetAttr lore),
          FreeIn (Op lore)) => FreeIn (Stm lore) where
  freeIn' (Let pat (StmAux cs attr) e) =
    freeIn' cs <> precomputed attr (freeIn' attr <> freeIn' e <> freeIn' pat)

instance FreeIn (Stm lore) => FreeIn (Stms lore) where
  freeIn' = foldMap freeIn'

instance FreeIn Names where
  freeIn' = fvNames

instance FreeIn Bool where
  freeIn' _ = mempty

instance FreeIn a => FreeIn (Maybe a) where
  freeIn' = maybe mempty freeIn'

instance FreeIn VName where
  freeIn' = fvName

instance FreeIn Ident where
  freeIn' = freeIn' . identType

instance FreeIn SubExp where
  freeIn' (Var v) = freeIn' v
  freeIn' Constant{} = mempty

instance FreeIn Space where
  freeIn' (ScalarSpace d _) = freeIn' d
  freeIn' DefaultSpace = mempty
  freeIn' (Space _) = mempty

instance FreeIn d => FreeIn (ShapeBase d) where
  freeIn' = freeIn' . shapeDims

instance FreeIn d => FreeIn (Ext d) where
  freeIn' (Free x) = freeIn' x
  freeIn' (Ext _)  = mempty

instance FreeIn shape => FreeIn (TypeBase shape u) where
  freeIn' (Array _ shape _) = freeIn' shape
  freeIn' (Mem s)           = freeIn' s
  freeIn' (Prim _)          = mempty

instance FreeIn attr => FreeIn (Param attr) where
  freeIn' (Param _ attr) = freeIn' attr

instance FreeIn attr => FreeIn (PatElemT attr) where
  freeIn' (PatElem _ attr) = freeIn' attr

instance FreeIn (LParamAttr lore) => FreeIn (LoopForm lore) where
  freeIn' (ForLoop _ _ bound loop_vars) = freeIn' bound <> freeIn' loop_vars
  freeIn' (WhileLoop cond) = freeIn' cond

instance FreeIn d => FreeIn (DimChange d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance FreeIn d => FreeIn (DimIndex d) where
  freeIn' = Data.Foldable.foldMap freeIn'

instance FreeIn attr => FreeIn (PatternT attr) where
  freeIn' (Pattern context values) =
    fvBind bound_here $ freeIn' $ context ++ values
    where bound_here = namesFromList $ map patElemName $ context ++ values

instance FreeIn Certificates where
  freeIn' (Certificates cs) = freeIn' cs

instance FreeIn attr => FreeIn (StmAux attr) where
  freeIn' (StmAux cs attr) = freeIn' cs <> freeIn' attr

instance FreeIn a => FreeIn (IfAttr a) where
  freeIn' (IfAttr r _) = freeIn' r

-- | Either return precomputed free names stored in the attribute, or
-- the freshly computed names.  Relies on lazy evaluation to avoid the
-- work.
class FreeIn attr => FreeAttr attr where
  precomputed :: attr -> FV -> FV
  precomputed _ = id

instance FreeAttr () where

instance (FreeAttr a, FreeIn b) => FreeAttr (a,b) where
  precomputed (a,_) = precomputed a

instance FreeAttr a => FreeAttr [a] where
  precomputed [] = id
  precomputed (a:_) = precomputed a

instance FreeAttr a => FreeAttr (Maybe a) where
  precomputed Nothing = id
  precomputed (Just a) = precomputed a

-- | The names bound by the bindings immediately in a 'Body'.
boundInBody :: Body lore -> Names
boundInBody = boundByStms . bodyStms

-- | The names bound by a binding.
boundByStm :: Stm lore -> Names
boundByStm = namesFromList . patternNames . stmPattern

-- | The names bound by the bindings.
boundByStms :: Stms lore -> Names
boundByStms = foldMap boundByStm

-- | The names of the lambda parameters plus the index parameter.
boundByLambda :: Lambda lore -> [VName]
boundByLambda lam = map paramName (lambdaParams lam)
