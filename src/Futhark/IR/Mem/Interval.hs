{-# LANGUAGE OverloadedStrings #-}

module Futhark.IR.Mem.Interval
  ( disjointZ3,
    Interval (..),
    distributeOffset,
    expandOffset,
    intervalOverlap,
    selfOverlap,
    primBool,
    intervalPairs,
    lessThanZ3,
    selfOverlapZ3,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List (delete, find, intersect, maximumBy, minimumBy, partition, sort, sortBy, zip4, zip5, zipWith5, (\\))
import qualified Data.Map.Strict as M
-- import Debug.Trace
import qualified Futhark.Analysis.AlgSimplify2 as AlgSimplify
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Prop
import Futhark.IR.Syntax hiding (Result)
import Futhark.Util
import Futhark.Util.Pretty
import Z3.Monad

traceWith' :: Show a => String -> a -> a
traceWith' _ a = a

traceWith :: Pretty a => String -> a -> a
traceWith _ a = a

trace _ a = a

data Interval = Interval
  { lowerBound :: TPrimExp Int64 VName,
    numElements :: TPrimExp Int64 VName,
    stride :: TPrimExp Int64 VName
  }
  deriving (Show, Eq)

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
distributeOffset offset (Interval lb ne st0 : is)
  | st <- AlgSimplify.Prod False [untyped st0],
    Just (before, quotient, after) <- focusMaybe (`AlgSimplify.maybeDivide` st) offset =
      distributeOffset (before <> after) $
        Interval (lb + TPrimExp (AlgSimplify.sumToExp [quotient])) ne st0 : is
  | [st] <- AlgSimplify.simplify0 $ untyped st0,
    Just (before, quotient, after) <- focusMaybe (`AlgSimplify.maybeDivide` st) offset =
      distributeOffset (before <> after) $
        Interval (lb + TPrimExp (AlgSimplify.sumToExp [quotient])) ne st0 : is
  | otherwise = do
      rest <- distributeOffset offset is
      return $ Interval lb ne st0 : rest

findMostComplexTerm :: AlgSimplify.SofP -> (AlgSimplify.Prod, AlgSimplify.SofP)
findMostComplexTerm prods =
  let max_prod = maximumBy (compare `on` (length . AlgSimplify.atoms)) prods
   in (max_prod, prods \\ [max_prod])

findClosestStride :: [PrimExp VName] -> [Interval] -> (PrimExp VName, [PrimExp VName])
findClosestStride offset_term is =
  let strides = map (untyped . stride) is
      p =
        minimumBy
          ( compare
              `on` ( (\(AlgSimplify.Prod _ xs) -> length (offset_term \\ xs))
                       . minimumBy (compare `on` \s -> (length (offset_term \\ AlgSimplify.atoms s)))
                       . AlgSimplify.simplify0
                   )
          )
          strides
   in ( p,
        (offset_term \\) $
          AlgSimplify.atoms $
            minimumBy (compare `on` \s -> length (offset_term \\ AlgSimplify.atoms s)) $
              AlgSimplify.simplify0 p
      )

expandOffset :: AlgSimplify.SofP -> [Interval] -> Maybe (AlgSimplify.SofP)
expandOffset [] _ = Nothing
expandOffset offset i1
  | (AlgSimplify.Prod b term_to_add, offset_rest) <- traceWith "findMostComplexTerm" $ findMostComplexTerm $ traceWith "Initial offset" offset, -- Find gnb
    (closest_stride, first_term_divisor) <- traceWith "findClosestStride" $ findClosestStride term_to_add i1, -- find (nb-b, g)
    target <- [AlgSimplify.Prod b $ closest_stride : first_term_divisor], -- g(nb-b)
    diff <- traceWith "sumOfProducts" $ AlgSimplify.sumOfProducts $ traceWith "sumToExp" $ AlgSimplify.sumToExp $ AlgSimplify.Prod b term_to_add : map AlgSimplify.negate target, -- gnb - gnb + gb = gnb - g(nb-b)
    replacement <- target <> diff -- gnb = g(nb-b) + gnb - gnb + gb
    =
      Just $ traceWith "Expandoffset returns!" (replacement <> offset_rest)

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
  not $ selfOverlap' 0 $ reverse is
  where
    selfOverlap' acc (x : xs) =
      let interval_span = (lowerBound x + numElements x - 1) * stride x
       in (traceWith ("Is " <> pretty (AlgSimplify.simplify' acc) <> " less than " <> pretty (AlgSimplify.simplify' $ stride x) <> "?") $ AlgSimplify.lessThanish less_thans non_negatives (AlgSimplify.simplify' acc) (AlgSimplify.simplify' $ stride x))
            && selfOverlap' (acc + interval_span) xs
    selfOverlap' _ [] = True

-- | Returns @Nothing@ if there is no overlap or @Just the-problem-interval@
selfOverlapZ3 :: M.Map VName Type -> [PrimExp VName] -> [(VName, PrimExp VName)] -> [PrimExp VName] -> [Interval] -> IO (Maybe Interval)
selfOverlapZ3 scope asserts less_thans non_negatives is =
  selfOverlap' 0 (reverse is)
  where
    selfOverlap' acc (x : xs) = do
      let interval_span = (lowerBound x + numElements x - 1) * stride x
      res <-
        lessThanZ3
          scope
          asserts
          less_thans
          non_negatives
          (untyped $ AlgSimplify.simplify' acc)
          (untyped $ AlgSimplify.simplify' $ stride x)
      if traceWith ("Is (z3) " <> pretty acc <> " less than " <> pretty (stride x)) res
        then selfOverlap' (acc + interval_span) xs
        else pure $ Just x
    selfOverlap' _ [] = pure Nothing

primTypeSort :: MonadZ3 z3 => PrimType -> z3 Sort
primTypeSort (IntType _) = mkIntSort
primTypeSort (FloatType _) = mkRealSort
primTypeSort Bool = mkBoolSort
primTypeSort pt = error $ "Unsupported PrimType " <> pretty pt

valToZ3 :: MonadZ3 z3 => M.Map VName Type -> VName -> z3 (Maybe AST)
valToZ3 scope vname =
  case M.lookup vname scope of
    Just (Prim pt) -> fmap Just (mkFreshVar (pretty vname) =<< primTypeSort pt)
    Just _ -> error $ "Unsupported type for vname " <> pretty vname
    Nothing -> trace ("Couldn't find vname " <> pretty vname) $ return Nothing

cmpOpToZ3 :: MonadZ3 z3 => CmpOp -> AST -> AST -> z3 AST
cmpOpToZ3 (CmpEq _) = mkEq
cmpOpToZ3 (CmpUlt _) = mkLt
cmpOpToZ3 (CmpUle _) = mkLe
cmpOpToZ3 (CmpSlt _) = mkLt
cmpOpToZ3 (CmpSle _) = mkLe
cmpOpToZ3 (FCmpLe _) = mkLe
cmpOpToZ3 (FCmpLt _) = mkLt
cmpOpToZ3 c = error $ "Unsupported CmpOp " <> pretty c

binOpToZ3 :: MonadZ3 z3 => BinOp -> AST -> AST -> z3 AST
binOpToZ3 (Add _ _) x y = mkAdd [x, y]
binOpToZ3 (Sub _ _) x y = mkSub [x, y]
binOpToZ3 (Mul _ _) x y = mkMul [x, y]
binOpToZ3 LogAnd x y = mkAnd [x, y]
binOpToZ3 LogOr x y = mkOr [x, y]
binOpToZ3 (SDiv _ _) x y = mkDiv x y
binOpToZ3 (SDivUp _ _) x y = mkDiv x y
binOpToZ3 (SMin _) x y = mkMin x y
binOpToZ3 (UMin _) x y = mkMin x y
binOpToZ3 (SMax _) x y = mkMax x y
binOpToZ3 (UMax _) x y = mkMax x y
binOpToZ3 (SDivUp _ _) x y = mkDiv x y
binOpToZ3 (SQuot _ _) x y = mkDiv x y
binOpToZ3 (SMod _ _) x y = mkMod x y
binOpToZ3 (UMod _ _) x y = mkMod x y
binOpToZ3 (FMod _) x y = mkMod x y
binOpToZ3 b _ _ = error $ "Unsupported BinOp " <> pretty b

convOpToZ3 :: MonadZ3 z3 => ConvOp -> AST -> z3 AST
convOpToZ3 (SExt _ _) x = return x
convOpToZ3 (SIToFP _ _) x = mkInt2Real x
convOpToZ3 (UIToFP _ _) x = mkInt2Real x
convOpToZ3 (FPToSI _ _) x = mkReal2Int x
convOpToZ3 (FPToUI _ _) x = mkReal2Int x
convOpToZ3 c _ = error $ "Unsupported ConvOp " <> pretty c

unOpToZ3 :: MonadZ3 z3 => UnOp -> AST -> z3 AST
unOpToZ3 Not x = mkNot x
unOpToZ3 u _ = error $ "Unsupported UnOp " <> pretty u

primExpToZ3 :: MonadZ3 z3 => M.Map VName AST -> PrimExp VName -> z3 AST
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
primExpToZ3 var_table (ConvOpExp c e) = convOpToZ3 c =<< primExpToZ3 var_table e
primExpToZ3 var_table (UnOpExp u e) = unOpToZ3 u =<< primExpToZ3 var_table e
primExpToZ3 var_table (FunExp name [e1] _)
  | name == "sqrt64" || name == "sqrt32" || name == "sqrt16" = do
      e1' <- primExpToZ3 var_table e1
      exponent <- mkRealNum 0.5
      mkPower e1' exponent
primExpToZ3 var_table e = error $ "Unsupported exp " <> pretty e

mkMin :: MonadZ3 z3 => AST -> AST -> z3 AST
mkMin e1 e2 = do
  cond <- mkLe e1 e2
  mkIte cond e1 e2

mkMax :: MonadZ3 z3 => AST -> AST -> z3 AST
mkMax e1 e2 = do
  cond <- mkGe e1 e2
  mkIte cond e1 e2

lessThanZ3 :: M.Map VName Type -> [PrimExp VName] -> [(VName, PrimExp VName)] -> [PrimExp VName] -> PrimExp VName -> PrimExp VName -> IO Bool
lessThanZ3 scope asserts less_thans non_negatives pe1 pe2 = do
  let frees = namesToList $ freeIn less_thans <> freeIn non_negatives <> freeIn pe1 <> freeIn pe2 <> freeIn asserts
  result <- evalZ3With Nothing (opt "timeout" (1000 :: Integer)) $ do
    -- result <- evalZ3 $ do
    maybe_var_table <- sequence <$> mapM (\x -> fmap ((,) x) <$> valToZ3 scope x) frees
    case fmap M.fromList maybe_var_table of
      Nothing -> return Undef
      Just var_table -> do
        non_negs <- mapM (\vn -> join $ mkLe <$> mkInteger 0 <*> primExpToZ3 var_table vn) non_negatives
        asserts <- mapM (\vn -> primExpToZ3 var_table vn) asserts
        lts <- mapM (\(vn, pe) -> join $ mkLt <$> return (var_table M.! vn) <*> primExpToZ3 var_table pe) less_thans
        apps <- mapM toApp $ M.elems var_table

        premise <-
          mkAnd $
            non_negs
              <> lts
              <> asserts

        conclusion <- join $ mkLe <$> primExpToZ3 var_table pe1 <*> primExpToZ3 var_table pe2
        assert =<< mkForallConst [] apps =<< mkImplies premise conclusion
        -- sexp <- solverToString
        -- liftIO $ putStrLn sexp
        check
  case result of
    Sat -> return True
    _ -> return False

disjointZ3 :: M.Map VName Type -> [PrimExp VName] -> [(VName, PrimExp VName)] -> [PrimExp VName] -> Interval -> Interval -> IO Bool
disjointZ3 scope asserts less_thans non_negatives i1@(Interval lb1 ne1 st1) i2@(Interval lb2 ne2 st2)
  | st1 == st2 = do
      let frees = namesToList $ freeIn less_thans <> freeIn non_negatives <> freeIn i1 <> freeIn i2 <> freeIn asserts
      result <- evalZ3With Nothing (opt "timeout" (1000 :: Integer)) $ do
        -- result <- evalZ3 $ do
        maybe_var_table <- sequence <$> mapM (\x -> fmap ((,) x) <$> valToZ3 scope x) frees
        case fmap M.fromList maybe_var_table of
          Nothing -> return Undef
          Just var_table -> do
            non_negs <- mapM (\vn -> join $ mkLe <$> mkInteger 0 <*> primExpToZ3 var_table vn) non_negatives
            asserts <- mapM (\vn -> primExpToZ3 var_table vn) asserts
            lts <- mapM (\(vn, pe) -> join $ mkLt <$> return (var_table M.! vn) <*> primExpToZ3 var_table pe) less_thans
            nes <-
              sequence
                [ join $ mkLt <$> mkInteger 0 <*> primExpToZ3 var_table (untyped ne1),
                  join $ mkLt <$> mkInteger 0 <*> primExpToZ3 var_table (untyped ne2)
                ]
            apps <- mapM toApp $ M.elems var_table

            premise <-
              mkAnd $
                nes
                  <> non_negs
                  <> lts
                  <> asserts

            conclusion <-
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
            assert =<< mkForallConst [] apps =<< mkImplies premise conclusion
            -- sexp <- solverToString
            -- liftIO $ putStrLn sexp
            check
      case traceWith'
        ( "disjointZ3\ni1: " <> pretty i1
            <> "\ni2: "
            <> pretty i2
            <> "\nless_thans: "
            <> pretty less_thans
            <> "\nnon_negatives: "
            <> pretty non_negatives
        )
        $ result of
        Sat -> return True
        _ -> return False
  | otherwise = return False

vname s i = VName (nameFromString s) i

b_8622 = vname "b" 8622

i_9625 = vname "i" 9625

i_9633 = vname "i" 9633

gtid_8705 = vname "gtid" 8705

num_blocks_8621 = vname "num_blocks" 8621

u name = LeafExp name (IntType Int64)

t = TPrimExp . u

add_nw64 = (+)

mul_nw64 = (*)

sub64 = (-)

sub_nw64 = (-)

types =
  M.fromList
    [ (b_8622, Prim $ IntType Int64),
      (i_9625, Prim $ IntType Int64),
      (i_9633, Prim $ IntType Int64),
      (gtid_8705, Prim $ IntType Int64),
      (num_blocks_8621, Prim $ IntType Int64)
    ]

lessthans =
  [ (i_9633, u b_8622),
    (i_9625, untyped $ sub64 (t num_blocks_8621) (1 :: TPrimExp Int64 VName)),
    (gtid_8705, untyped $ sub64 (t num_blocks_8621) 1)
  ]

nonnegs = namesFromList [b_8622, i_9625, i_9633, gtid_8705, num_blocks_8621]

int1 = Interval (add_nw64 (add_nw64 (mul_nw64 (t b_8622) (t b_8622)) (mul_nw64 (mul_nw64 (t b_8622) (t b_8622)) (t i_9625))) (t i_9633)) 1 1

int2 = Interval 0 (t b_8622) 1

-- i1: {lowerBound: add_nw64 (add_nw64 (mul_nw64 (b_8622) (b_8622)) (mul_nw64 (mul_nw64 (b_8622) (b_8622)) (i_9625))) (i_9633);
--  numElements: 1i64; stride: 1i64}

-- i2: {lowerBound: 0i64; numElements: b_8622; stride: 1i64}
