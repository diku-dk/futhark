module Language.Futhark.Interpreter.FFI.Push
  ( put,
    get,
    hasLazy,
    getLazy,
    lazyGet,
    ResShape (..),
    resultShape,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.Except (throwError)
import Data.Array qualified as A
import Data.Map qualified as M
import Data.Text qualified as T
import Language.Futhark.Interpreter.FFI.ServerM
import Language.Futhark.Interpreter.Values qualified as I
import Language.Futhark.Syntax (Int64, Name, nameToString)

toArray :: [a] -> A.Array Int a
toArray vs = A.listArray (0, length vs - 1) vs

get :: I.ValueShape -> ValueRef -> ServerM (I.Value m)
get I.ShapeLeaf vr = I.ValuePrim <$> getPrim vr
get rshp@(I.ShapeDim {}) vr = getArray rshp
  where
    getArray :: I.ValueShape -> ServerM (I.Value m)
    getArray shp = getArray' shp []
    getArray' :: I.ValueShape -> [Int64] -> ServerM (I.Value m)
    getArray' shp@(I.ShapeDim n cshp) is = I.ValueArray shp . toArray <$> mapM (getArray' cshp . (: is)) [0 .. n - 1]
    getArray' cshp is = index (reverse is) vr >>= get cshp
get (I.ShapeRecord sm) vr =
  I.ValueRecord
    <$> sequence (M.mapWithKey (\fn cshp -> project vr fn >>= get cshp) sm)
get shp@(I.ShapeSum sm) vr = do
  vn <- variant vr
  shps <- throwNothing ("Invalid variant " ++ nameToString vn ++ " in shape " ++ show sm ++ ".") $ M.lookup vn sm
  I.ValueSum shp vn <$> (destruct vr >>= zipWithM get shps)

-- | Does this value contain any references to values residing on a server?
hasLazy :: I.Value m -> Bool
hasLazy I.ValueLazyFFI {} = True
hasLazy (I.ValueArray _ arr) = any hasLazy $ A.elems arr
hasLazy (I.ValueRecord fs) = any hasLazy fs
hasLazy (I.ValueSum _ _ vs) = any hasLazy vs
hasLazy _ = False

-- | Get all the values residing on the server. The resulting 'I.Value' has no
-- 'I.ValueLazyFFI' in it.
getLazy :: I.Value a -> ServerM (I.Value a)
getLazy (I.ValueArray shp arr) = I.ValueArray shp <$> mapM getLazy arr
getLazy (I.ValueRecord m) = I.ValueRecord <$> mapM getLazy m
getLazy (I.ValueSum shp vn vs) = I.ValueSum shp vn <$> mapM getLazy vs
getLazy (I.ValueLazyFFI shp r os) = do
  v <- get shp r
  pure $ foldl (\(I.ValueArray _ a) i -> a A.! fromIntegral i) v $ reverse os
getLazy v = pure v

lazyGet :: I.ValueShape -> ValueRef -> ServerM (I.Value m)
lazyGet shp@(I.ShapeDim {}) vr = pure $ I.ValueLazyFFI shp vr []
lazyGet shp vr = get shp vr

put :: TypeName -> I.Value m -> ServerM ValueRef
put _ (I.ValuePrim p) = putPrim p
put tn pv@(I.ValueArray shp _)
  -- TODO: array elements residing on the server currently have to be fetched
  -- first, but this could be optimised further.
  | hasLazy pv = put tn =<< getLazy pv
  | otherwise = do
      et <- elemType tn
      mapM (put et) (flatten pv) >>= mkArray tn (dims shp)
  where
    flatten :: I.Value m -> [I.Value m]
    flatten (I.ValueArray _ a) = foldl (\o n -> o ++ flatten n) [] $ A.elems a
    flatten v = [v]
    dims :: I.ValueShape -> [Int64]
    dims (I.ShapeDim n cshp) = n : dims cshp
    dims _ = []
put tn (I.ValueRecord vm) = do
  fm <- M.fromList <$> fieldOrder tn
  vrm <- sequence $ M.intersectionWith put fm vm
  mkRecord tn vrm
put tn (I.ValueSum _ vn vs) = do
  vts <-
    throwNothing ("Invalid variant " ++ nameToString vn ++ " in type " ++ T.unpack tn ++ ".") . M.lookup vn
      =<< variants tn
  vrs <- zipWithM put vts vs
  mkSum tn vn vrs
-- The value already resides on the server.
put _ (I.ValueLazyFFI _ r []) = pure r
put tn v@(I.ValueLazyFFI {}) = do
  -- A partially indexed array cannot be constructed on the server, as the
  -- 'index' command only indexes all the way down to an element, so this
  -- one does have to go through the interpreter.
  iv <- getLazy v
  put tn iv
put _ v = error $ "Values of type " ++ show v ++ " are unsupported in FFI."

-- | How much of the shape of a value its type determines. Array dimensions may
-- be unknown at the type level, so they must be extracted from the actual
-- server-side value.
data ResShape
  = -- | An array of any rank; the argument describes its elements (after
    -- stripping array dimensions).
    ResArray ResShape
  | -- | A record, whose fields are described individually.
    ResRecord (M.Map Name ResShape)
  | -- | Determined by the type alone.
    ResKnown I.ValueShape

-- | Determine the shape of a value residing on the server, consulting the
-- server for whatever the type did not settle.
resultShape :: ResShape -> ValueRef -> ServerM I.ValueShape
resultShape (ResKnown shp) _ = pure shp
resultShape (ResRecord fs) vr =
  I.ShapeRecord <$> M.traverseWithKey (\f shp -> resultShape shp =<< project vr f) fs
resultShape (ResArray eshp) vr = do
  dims <- shape vr
  foldr I.ShapeDim <$> elemShape (length dims) eshp vr <*> pure dims

-- | The shape of the elements of an array. The elements cannot be inspected
-- one at a time, as there may not be any, so a record is instead unzipped
-- into one array per field - which has a shape even when it is empty. The
-- outer dimensions of those arrays are the ones we started with, and are
-- dropped again.
elemShape :: Int -> ResShape -> ValueRef -> ServerM I.ValueShape
elemShape _ (ResKnown shp) _ = pure shp
elemShape k (ResRecord fs) arr = do
  etn <- elemType =<< vtype arr
  order <- fieldOrder etn
  refs <- unzipArray arr $ length order
  I.ShapeRecord . M.fromList <$> zipWithM onField order refs
  where
    onField (f, _) ref = do
      shp <- maybe (unknownField f) (\e -> resultShape (arrayOf e) ref) $ M.lookup f fs
      pure (f, dropDims k shp)
    unknownField f =
      throwError $ "Unzipping produced unexpected field " ++ nameToString f ++ "."
elemShape _ (ResArray _) _ =
  -- 'ResArray' covers every dimension at once, so it never describes the
  -- elements of an array.
  throwError "Array element is itself an array."

-- | The shape of an array whose elements are described by the argument.
arrayOf :: ResShape -> ResShape
arrayOf (ResArray eshp) = ResArray eshp
arrayOf shp = ResArray shp

dropDims :: Int -> I.ValueShape -> I.ValueShape
dropDims 0 shp = shp
dropDims k (I.ShapeDim _ shp) = dropDims (k - 1) shp
dropDims _ _ = error "Unzipped field has too few dimensions."
