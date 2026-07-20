module Language.Futhark.Interpreter.FFI.Push
  ( put,
    get,
  )
where

import Control.Monad (zipWithM)
import Data.Array qualified as A
import Data.Map qualified as M
import Data.Text qualified as T
import Language.Futhark.Interpreter.FFI.ServerM
import Language.Futhark.Interpreter.Values qualified as I
import Language.Futhark.Syntax (Int64, nameToString)

toArray :: [a] -> A.Array Int a
toArray vs = A.listArray (0, length vs - 1) vs

get :: I.ValueShape -> ValueRef -> ServerM (I.Value m)
get I.ShapeLeaf vr = I.ValuePrim <$> getPrim vr
get rshp@(I.ShapeDim {}) vr = getArray rshp
  where
    getArray :: I.ValueShape -> ServerM (I.Value m)
    getArray shp = getArray' shp []
    getArray' :: I.ValueShape -> [Int64] -> ServerM (I.Value m)
    getArray' shp@(I.ShapeDim n cshp) is = I.ValueArray shp . toArray <$> mapM (getArray' cshp . (:is)) [0..n-1]
    getArray' cshp is = index (reverse is) vr >>= get cshp
get (I.ShapeRecord sm) vr =
  I.ValueRecord <$> (sequence $ M.mapWithKey (\fn cshp -> project vr fn >>= get cshp) sm)
get shp@(I.ShapeSum sm) vr = do
  vn <- variant vr
  shps <- throwNothing ("Invalid variant " ++ nameToString vn ++ " in shape " ++ show sm ++ ".") $ M.lookup vn sm
  I.ValueSum shp vn <$> (destruct vr >>= zipWithM get shps)

put :: TypeName -> I.Value m -> ServerM ValueRef
put _ (I.ValuePrim p) = putPrim p
put tn pv@(I.ValueArray shp _) = do
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
  fm <- fields tn
  vrm <- sequence $ M.intersectionWith put fm vm
  mkRecord tn vrm
put tn (I.ValueSum _ vn vs) = do
  vts <- M.lookup vn <$> variants tn >>= throwNothing ("Invalid variant " ++ nameToString vn ++ " in type " ++ T.unpack tn ++ ".")
  vrs <- zipWithM put vts vs
  mkSum tn vn vrs
put _ v = error $ "Values of type " ++ show v ++ " are unsupported in FFI."
