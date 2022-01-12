{-# LANGUAGE OverloadedStrings #-}

module Futhark.IR.Mem.Interval
  ( disjointZ3,
    Interval (..),
    distributeOffset,
    intervalOverlap,
    selfOverlap,
    primBool,
    intervalPairs,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (delete, find, intersect, partition, sort, sortBy, zip4, zip5, zipWith5, (\\))
import qualified Data.Map.Strict as M
import Debug.Trace
import qualified Futhark.Analysis.AlgSimplify2 as AlgSimplify
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Prop
import Futhark.IR.Syntax hiding (Result)
import Futhark.Util.Pretty
import Z3.Monad

traceWith :: Pretty a => String -> a -> a
traceWith s a = trace (s <> ": " <> pretty a) a

data Interval = Interval
  { lowerBound :: TPrimExp Int64 VName,
    numElements :: TPrimExp Int64 VName,
    stride :: TPrimExp Int64 VName
  }
  deriving (Show)

instance Pretty Interval where
  ppr (Interval lb ne st) =
    braces $
      semisep
        [ "lowerBound: " <> ppr lb,
          "numElements: " <> ppr ne,
          "stride: " <> ppr st
        ]

instance FreeIn Interval where
  freeIn' (Interval lb ne st) = freeIn' lb <> freeIn' ne <> freeIn' st

distributeOffset :: MonadFail m => AlgSimplify.SofP -> [Interval] -> m [Interval]
distributeOffset [] interval = return interval
distributeOffset offset [] = fail $ "Cannot distribute offset " <> pretty offset <> " across empty interval"
distributeOffset offset [Interval lb ne 1] = return [Interval (lb + TPrimExp (AlgSimplify.sumToExp offset)) ne 1]
distributeOffset offset (Interval lb ne st0 : is) = do
  -- If a term 't' in the offset contains a multiple of the stride: Subtract `t`
  -- from the offset, add `t / st` to the lower bound.
  --
  -- Example: The offset is `a + b * b * 2` and the stride is `b * b`. The
  -- remaining offset should be `a` and the new lower bound should be `2`.
  AlgSimplify.Prod neg st <-
    maybe (fail "Stride should have exactly one term") return $
      justOne $
        AlgSimplify.simplify0 $ untyped st0
  -- We do not support negative strides here. They should've been normalized.
  if neg
    then fail "Stride should be positive"
    else case find (`AlgSimplify.isMultipleOf` st) offset of
      Just t@(AlgSimplify.Prod False as') ->
        distributeOffset (t `delete` offset) $ Interval (lb + TPrimExp (AlgSimplify.sumToExp [AlgSimplify.Prod False $ traceWith "res" $ traceWith "as'" as' \\ traceWith "st" st])) ne st0 : is
      Just (AlgSimplify.Prod True _) -> fail "Offset term should be positive"
      Nothing -> do
        rest <- distributeOffset offset is
        return $ Interval lb ne st0 : rest
  where
    justOne :: [a] -> Maybe a
    justOne [a] = Just a
    justOne _ = Nothing

intervalOverlap :: [(VName, PrimExp VName)] -> Names -> Interval -> Interval -> Bool
intervalOverlap less_thans non_negatives (Interval lb1 ne1 st1) (Interval lb2 ne2 st2)
  | st1 == st2,
    AlgSimplify.lessThanish less_thans non_negatives lb1 lb2,
    AlgSimplify.lessThanish less_thans non_negatives (lb1 + ne1 - 1) lb2 =
    False
  | st1 == st2,
    AlgSimplify.lessThanish less_thans non_negatives lb2 lb1,
    AlgSimplify.lessThanish less_thans non_negatives (lb2 + ne2 - 1) lb1 =
    False
  | otherwise = True

primBool :: TPrimExp Bool VName -> Maybe Bool
primBool p
  | Just (BoolValue b) <- evalPrimExp (const Nothing) $ untyped p = Just b
  | otherwise = Nothing

primInt :: TPrimExp Int64 VName -> Maybe Int64
primInt p
  | Just (IntValue (Int64Value i)) <- evalPrimExp (const Nothing) $ untyped p = Just i
  | otherwise = Nothing

intervalPairs :: [Interval] -> [Interval] -> [(Interval, Interval)]
intervalPairs = intervalPairs' []
  where
    intervalPairs' :: [(Interval, Interval)] -> [Interval] -> [Interval] -> [(Interval, Interval)]
    intervalPairs' acc [] [] = reverse acc
    intervalPairs' acc (i@(Interval lb _ st) : is) [] = intervalPairs' ((i, Interval lb 1 st) : acc) is []
    intervalPairs' acc [] (i@(Interval lb _ st) : is) = intervalPairs' ((Interval lb 1 st, i) : acc) [] is
    intervalPairs' acc (i1@(Interval lb1 _ st1) : is1) (i2@(Interval lb2 _ st2) : is2)
      | st1 == st2 = intervalPairs' ((i1, i2) : acc) is1 is2
      | otherwise =
        let res1 = intervalPairs' ((i1, Interval lb1 1 st1) : acc) is1 (i2 : is2)
            res2 = intervalPairs' ((Interval lb2 1 st2, i2) : acc) (i1 : is1) is2
         in if length res1 <= length res2
              then res1
              else res2

-- | Returns true if the intervals are self-overlapping, meaning that for a
-- given dimension d, the stride of d is larger than the aggregate spans of the
-- lower dimensions.
selfOverlap :: [(VName, PrimExp VName)] -> Names -> [Interval] -> Bool
selfOverlap less_thans non_negatives is =
  -- TODO: Do we need to do something clever using some ranges of known values?
  selfOverlap' 0 $ reverse is
  where
    selfOverlap' acc (x : xs) =
      let interval_span = (lowerBound x + numElements x - 1) * stride x
       in AlgSimplify.lessThanish less_thans non_negatives (AlgSimplify.simplify' acc) (AlgSimplify.simplify' $ stride x)
            && selfOverlap' (acc + interval_span) xs
    selfOverlap' _ [] = False

primTypeSort :: PrimType -> Z3 Sort
primTypeSort (IntType _) = mkIntSort
primTypeSort (FloatType _) = mkRealSort
primTypeSort Bool = mkBoolSort
primTypeSort _ = undefined

valToZ3 :: M.Map VName Type -> VName -> Z3 AST
valToZ3 scope vname =
  case M.lookup vname scope of
    Just (Prim pt) -> mkFreshVar (pretty vname) =<< primTypeSort pt
    Just _ -> undefined
    Nothing -> undefined

cmpOpToZ3 :: CmpOp -> AST -> AST -> Z3 AST
cmpOpToZ3 (CmpEq _) = mkEq
cmpOpToZ3 (CmpUlt _) = mkLt
cmpOpToZ3 (CmpUle _) = mkLe
cmpOpToZ3 (CmpSlt _) = mkLt
cmpOpToZ3 (CmpSle _) = mkLe
cmpOpToZ3 _ = undefined

binOpToZ3 :: BinOp -> AST -> AST -> Z3 AST
binOpToZ3 (Add _ _) x y = mkAdd [x, y]
binOpToZ3 (Sub _ _) x y = mkSub [x, y]
binOpToZ3 (Mul _ _) x y = mkMul [x, y]
binOpToZ3 (And _) x y = mkAnd [x, y]
binOpToZ3 (Or _) x y = mkOr [x, y]
binOpToZ3 _ x y = undefined

primExpToZ3 :: M.Map VName AST -> PrimExp VName -> Z3 AST
primExpToZ3 var_table (LeafExp vn _) = return $ var_table M.! vn
primExpToZ3 var_table (ValueExp (IntValue v)) = mkInteger $ valueIntegral v
primExpToZ3 var_table (ValueExp (FloatValue v)) = mkRealNum $ valueRational v
primExpToZ3 var_table (ValueExp (BoolValue b)) = mkBool b
primExpToZ3 var_table (BinOpExp bop e1 e2) =
  join $
    binOpToZ3 bop <$> primExpToZ3 var_table e1
      <*> primExpToZ3 var_table e2
primExpToZ3 var_table (CmpOpExp cop e1 e2) =
  join $
    cmpOpToZ3 cop <$> primExpToZ3 var_table e1
      <*> primExpToZ3 var_table e2
primExpToZ3 var_table _ = undefined

disjointZ3 :: M.Map VName Type -> [(VName, PrimExp VName)] -> Names -> Interval -> Interval -> IO Bool
disjointZ3 scope less_thans non_negatives i1@(Interval lb1 ne1 st1) i2@(Interval lb2 ne2 st2)
  | st1 == st2 = do
    let frees = namesToList $ freeIn less_thans <> freeIn non_negatives <> freeIn i1 <> freeIn i2
    result <- evalZ3With Nothing (opt "timeout" (30 :: Integer)) $ do
      var_table <- M.fromList <$> mapM (\x -> (,) x <$> valToZ3 scope x) frees
      non_negs <- mapM (\vn -> join $ mkLe <$> mkInteger 0 <*> return (var_table M.! vn)) $ namesToList non_negatives
      lts <- mapM (\(vn, pe) -> join $ mkLt <$> return (var_table M.! vn) <*> primExpToZ3 var_table pe) less_thans
      apps <- mapM toApp $ M.elems var_table
      implies1 <-
        mkAnd
          =<< sequence
            [ join $ mkLt <$> mkInteger 0 <*> primExpToZ3 var_table (untyped ne1),
              join $ mkLt <$> mkInteger 0 <*> primExpToZ3 var_table (untyped ne2)
            ]
      implies2 <-
        mkOr
          =<< sequence
            [ mkAnd
                =<< sequence
                  [ join $ mkLt <$> primExpToZ3 var_table (untyped lb1) <*> primExpToZ3 var_table (untyped lb2),
                    join $ mkLe <$> primExpToZ3 var_table (untyped $ lb1 + ne1) <*> primExpToZ3 var_table (untyped lb2)
                  ],
              mkAnd
                =<< sequence
                  [ join $ mkLt <$> primExpToZ3 var_table (untyped lb2) <*> primExpToZ3 var_table (untyped lb1),
                    join $ mkLe <$> primExpToZ3 var_table (untyped $ lb2 + ne2) <*> primExpToZ3 var_table (untyped lb1)
                  ]
            ]
      assert =<< mkAnd non_negs
      assert =<< mkAnd lts
      assert =<< mkForallConst [] apps =<< mkImplies implies1 implies2
      sexp <- solverToString
      liftIO $ putStrLn sexp
      check
    case result of
      Sat -> return True
      _ -> return False
  | otherwise = return False

g = VName (nameFromString "g") 0

g' = TPrimExp $ LeafExp g $ IntType Int64

n = VName (nameFromString "n") 1

n' = TPrimExp $ LeafExp n $ IntType Int64

types = M.fromList [(g, Prim $ IntType Int64), (n, Prim $ IntType Int64)]

lessthans = [(g, untyped $ n' - (1 :: TPrimExp Int64 VName))]

nonnegs = (namesFromList [n, g])

int1 = Interval 0 g' 1

int2 = Interval g' 1 1
