{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Internalise.TypesValues
  (
   -- * Internalising types
    BoundInTypes
  , boundInTypes
  , internaliseReturnType
  , internaliseEntryReturnType
  , internaliseParamTypes
  , internaliseType
  , internalisePrimType
  , internalisedTypeSize
  , internaliseSumType

  -- * Internalising values
  , internalisePrimValue
  )
  where

import Control.Monad.State
import Control.Monad.Reader
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

import qualified Language.Futhark as E
import Futhark.Representation.SOACS as I
import Futhark.Internalise.Monad

internaliseUniqueness :: E.Uniqueness -> I.Uniqueness
internaliseUniqueness E.Nonunique = I.Nonunique
internaliseUniqueness E.Unique = I.Unique

-- | The names that are bound for some types, either implicitly or
-- explicitly.
newtype BoundInTypes = BoundInTypes (S.Set VName)
                       deriving (Semigroup, Monoid)

-- | Determine the names bound for some types.
boundInTypes :: [E.TypeParam] -> BoundInTypes
boundInTypes = BoundInTypes . S.fromList . mapMaybe isTypeParam
  where isTypeParam (E.TypeParamDim v _) = Just v
        isTypeParam _ = Nothing

internaliseParamTypes :: BoundInTypes
                      -> M.Map VName VName
                      -> [E.TypeBase (E.DimDecl VName) ()]
                      -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                       ConstParams)
internaliseParamTypes (BoundInTypes bound) pnames ts =
  runInternaliseTypeM $ withDims (bound' <> M.map (Free . Var) pnames) $
  mapM internaliseTypeM ts
  where bound' = M.fromList (zip (S.toList bound)
                                 (map (Free . Var) $ S.toList bound))

internaliseReturnType :: E.TypeBase (E.DimDecl VName) ()
                      -> InternaliseM ([I.TypeBase ExtShape Uniqueness],
                                       ConstParams)
internaliseReturnType t = do
  (ts', cm') <- internaliseEntryReturnType t
  return (concat ts', cm')

-- | As 'internaliseReturnType', but returns components of a top-level
-- tuple type piecemeal.
internaliseEntryReturnType :: E.TypeBase (E.DimDecl VName) ()
                           -> InternaliseM ([[I.TypeBase ExtShape Uniqueness]],
                                            ConstParams)
internaliseEntryReturnType t = do
  let ts = case E.isTupleRecord t of Just tts | not $ null tts -> tts
                                     _ -> [t]
  runInternaliseTypeM $ mapM internaliseTypeM ts

internaliseType :: E.TypeBase (E.DimDecl VName) ()
                -> InternaliseM [I.TypeBase I.ExtShape Uniqueness]
internaliseType =
  fmap fst . runInternaliseTypeM . internaliseTypeM

newId :: InternaliseTypeM Int
newId = do (i,cm) <- get
           put (i + 1, cm)
           return i

internaliseDim :: E.DimDecl VName
               -> InternaliseTypeM ExtSize
internaliseDim d =
  case d of
    E.AnyDim -> Ext <$> newId
    E.ConstDim n -> return $ Free $ intConst I.Int32 $ toInteger n
    E.NamedDim name -> namedDim name
  where namedDim (E.QualName _ name) = do
          subst <- liftInternaliseM $ asks $ M.lookup name . envSubsts
          is_dim <- lookupDim name
          is_const <- liftInternaliseM $ lookupConst name

          case (is_dim, is_const, subst) of
            (Just dim, _, _) -> return dim

            (Nothing, Nothing, Just [v]) -> return $ I.Free v

            (_, Just (fname, _, _, _), _) -> do
              (i,cm) <- get
              case find ((==fname) . fst) cm of
                Just (_, known) -> return $ I.Free $ I.Var known
                Nothing -> do new <- liftInternaliseM $ newVName $ baseString name
                              put (i, (fname,new):cm)
                              return $ I.Free $ I.Var new
            _ -> return $ I.Free $ I.Var name

internaliseTypeM :: E.StructType
                 -> InternaliseTypeM [I.TypeBase ExtShape Uniqueness]
internaliseTypeM orig_t =
  case orig_t of
    E.Array _ u et shape -> do
      dims <- internaliseShape shape
      ets <- internaliseTypeM $ E.Scalar et
      return [I.arrayOf et' (Shape dims) $ internaliseUniqueness u | et' <- ets ]
    E.Scalar (E.Prim bt) ->
      return [I.Prim $ internalisePrimType bt]
    E.Scalar (E.Record ets)
      -- XXX: we map empty records to bools, because otherwise
      -- arrays of unit will lose their sizes.
      | null ets -> return [I.Prim I.Bool]
      | otherwise ->
          concat <$> mapM (internaliseTypeM . snd) (E.sortFields ets)
    E.Scalar E.TypeVar{} ->
      error "internaliseTypeM: cannot handle type variable."
    E.Scalar E.Arrow{} ->
      error $ "internaliseTypeM: cannot handle function type: " ++ pretty orig_t
    E.Scalar (E.Sum cs) -> do
      (ts, _) <- internaliseConstructors <$>
                 traverse (fmap concat . mapM internaliseTypeM) cs
      return $ I.Prim (I.IntType I.Int8) : ts

  where internaliseShape = mapM internaliseDim . E.shapeDims

internaliseConstructors :: M.Map Name [I.TypeBase ExtShape Uniqueness]
                        -> ([I.TypeBase ExtShape Uniqueness],
                            M.Map Name (Int, [Int]))
internaliseConstructors cs =
  foldl' onConstructor mempty $ zip (E.sortConstrs cs) [0..]
  where onConstructor (ts, mapping) ((c, c_ts), i) =
          let (_, js, new_ts) =
                foldl' f (zip ts [0..], mempty, mempty) c_ts
          in (ts ++ new_ts, M.insert c (i, js) mapping)
          where f (ts', js, new_ts) t
                  | Just (_, j) <- find ((==t) . fst) ts' =
                      (delete (t, j) ts',
                       js ++ [j],
                       new_ts)
                  | otherwise =
                      (ts',
                       js ++ [length ts + length new_ts],
                       new_ts ++ [t])

internaliseSumType :: M.Map Name [E.StructType]
                   -> InternaliseM (([I.TypeBase ExtShape Uniqueness],
                                     M.Map Name (Int, [Int])),
                                     ConstParams)
internaliseSumType cs =
  runInternaliseTypeM $ internaliseConstructors <$>
  traverse (fmap concat . mapM internaliseTypeM) cs

-- | How many core language values are needed to represent one source
-- language value of the given type?
internalisedTypeSize :: E.TypeBase (E.DimDecl VName) () -> InternaliseM Int
internalisedTypeSize = fmap length . internaliseType

-- | Convert an external primitive to an internal primitive.
internalisePrimType :: E.PrimType -> I.PrimType
internalisePrimType (E.Signed t) = I.IntType t
internalisePrimType (E.Unsigned t) = I.IntType t
internalisePrimType (E.FloatType t) = I.FloatType t
internalisePrimType E.Bool = I.Bool

-- | Convert an external primitive value to an internal primitive value.
internalisePrimValue :: E.PrimValue -> I.PrimValue
internalisePrimValue (E.SignedValue v) = I.IntValue v
internalisePrimValue (E.UnsignedValue v) = I.IntValue v
internalisePrimValue (E.FloatValue v) = I.FloatValue v
internalisePrimValue (E.BoolValue b) = I.BoolValue b
