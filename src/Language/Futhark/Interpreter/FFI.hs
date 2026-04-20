module Language.Futhark.Interpreter.FFI
  ( ExTypeAtom,
    ExType,
    ExValueAtom,
    ExValue,
    Function (..),
    Interface (..),
    InFunction,
    InInterface,
    ExFunction,
    ExInterface,
    fromInterpreterValue,
    toInterpreterValue,
  )
where

import Data.Array qualified as A
import Data.Map qualified as M
import Language.Futhark.Core (Name, nameFromText, nameToText)
import Language.Futhark.Interpreter.FFI.UIDs
import Language.Futhark.Interpreter.FFI.Util.NDArray qualified as ND
import Language.Futhark.Interpreter.FFI.Values
import Language.Futhark.Interpreter.Values qualified as I
import Language.Futhark.Interpreter.Values qualified as S

data Function a
  = Function [Type a] (Type a)
  deriving (Show, Eq)

newtype Interface a
  = Interface (M.Map Name (Function a))
  deriving (Show)

type InFunction = Function PrimitiveType

type InInterface = Interface PrimitiveType

type ExTypeAtom = Either TypeUID PrimitiveType

type ExValueAtom = Either Location PrimitiveValue

type ExType = Type ExTypeAtom

type ExValue = Value ExValueAtom

type ExFunction = Function ExTypeAtom

type ExInterface = Interface ExTypeAtom

fromInterpreterValue :: I.Value m -> ExValue
fromInterpreterValue (I.ValuePrim v) = Atom $ Right $ fromPrimValue v
fromInterpreterValue iv@(I.ValueArray _ _) = Array $ fmap fromInterpreterValue $ ND.fromList (dims iv) $ flatten iv
  where
    flatten :: I.Value m -> [I.Value m]
    flatten (I.ValueArray _ a) = concatMap flatten $ A.elems a
    flatten v = [v]

    dims :: I.Value m -> [Int]
    dims (I.ValueArray _ a) = let (l, u) = A.bounds a in u - l + 1 : dims (a A.! 0)
    dims _ = []
fromInterpreterValue (I.ValueRecord m) = Record $ M.map fromInterpreterValue $ M.mapKeys nameToText m
fromInterpreterValue (I.ValueSum _ n v) = Sum (nameToText n) $ map fromInterpreterValue v
fromInterpreterValue (I.ValueExt l _) = Atom $ Left l
fromInterpreterValue _ = error "TODO (qu9wdaoijlm)"

toInterpreterValue :: ExValue -> I.Value m
toInterpreterValue (Atom (Right v)) = I.ValuePrim $ toPrimValue v
-- TODO: Add shape
toInterpreterValue (Array nd) = unflatten []
  where
    unflatten :: [Int] -> I.Value m
    unflatten idx =
      if length idx == ND.rank nd
        then toInterpreterValue $ nd ND.! reverse idx
        else
          let ni = ND.shape nd !! length idx
           in I.ValueArray S.ShapeLeaf $ A.listArray (0, ni - 1) $ map (unflatten . (: idx)) [0 .. ni - 1]
toInterpreterValue (Record m) = I.ValueRecord $ M.map toInterpreterValue $ M.mapKeys nameFromText m
-- TODO: Add shape
toInterpreterValue (Sum n v) = I.ValueSum (I.ShapeSum M.empty) (nameFromText n) $ map toInterpreterValue v
toInterpreterValue (Atom (Left vid)) = I.ValueExt vid Nothing
