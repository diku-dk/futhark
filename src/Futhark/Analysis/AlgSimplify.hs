{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplify
  ( simplify,
    Atom (..),
    Term (..),
    NestedSum (..),
    SumNode (..),
    Sum,
    flattenNestedSum,
    simplifySum,
    simplifySum',
  )
where

import Data.Bits (xor)
import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Futhark.Util.Pretty
import GHC.Generics
import Prelude hiding (negate)

type Exp = PrimExp VName

type LookupTable = M.Map VName Exp

newtype NestedSum = NestedSum {unNested :: SumNode NestedSum}
  deriving (Show, Eq, Generic)

type Sum = SumNode ()

data SumNode nest
  = Sum Overflow [Term nest]
  deriving (Show, Eq, Generic)

instance Pretty nest => Pretty (SumNode nest) where
  ppr (Sum _ terms) = "Sum " <> brackets (commasep $ fmap ppr terms)

data Term nest
  = Negated (Term nest)
  | Product Overflow [Atom nest]
  deriving (Show, Eq, Generic)

instance Pretty nest => Pretty (Term nest) where
  ppr (Negated term) = "-" <> ppr term
  ppr (Product _ atoms) = "Product " <> brackets (commasep $ fmap ppr atoms)

data Atom nest
  = Exp Exp
  | Nested nest
  deriving (Show, Eq, Generic)

instance Pretty nest => Pretty (Atom nest) where
  ppr (Exp e) = ppr e
  ppr (Nested s) = parens $ ppr s

simplify :: LookupTable -> TPrimExp Int64 VName -> TPrimExp Int64 VName
simplify table e =
  untyped e
    & sumOfProducts
    & simplifySum
    & unNested
    & lookupExps table
    & NestedSum
    & sumToExp
    & TPrimExp

class HasLookup nest where
  lookupExpsInNest :: LookupTable -> nest -> nest

instance HasLookup NestedSum where
  lookupExpsInNest table (NestedSum s) = NestedSum $ lookupExps table s

lookupExps :: HasLookup nest => LookupTable -> SumNode nest -> SumNode nest
lookupExps table (Sum o terms) =
  Sum o $ fmap (lookupExpsInTerm table) terms

lookupExpsInTerm :: HasLookup nest => LookupTable -> Term nest -> Term nest
lookupExpsInTerm table (Negated term) = Negated $ lookupExpsInTerm table term
lookupExpsInTerm table (Product o' atoms) = Product o' $ fmap (lookupExpsInAtom table) atoms

lookupExpsInAtom :: HasLookup nest => LookupTable -> Atom nest -> Atom nest
lookupExpsInAtom table e@(Exp (LeafExp v _)) = maybe e (lookupExpsInAtom table . Exp) $ M.lookup v table
lookupExpsInAtom table (Nested s) = Nested $ lookupExpsInNest table s
lookupExpsInAtom table a = a

sumOfProducts :: Exp -> NestedSum
sumOfProducts e =
  NestedSum $
    case e of
      BinOpExp (Add Int64 o) e1 e2 ->
        let terms =
              ( case sumOfProducts e1 of
                  NestedSum (Sum o' terms') | o == o' -> terms'
                  _ -> [Product o [Exp e1]]
              )
                <> ( case sumOfProducts e2 of
                       NestedSum (Sum o' terms') | o == o' -> terms'
                       _ -> [Product o [Exp e2]]
                   )
         in Sum o terms
      BinOpExp (Sub Int64 o) (ValueExp (IntValue (Int64Value 0))) e'
        | NestedSum (Sum o' terms) <- sumOfProducts e',
          o == o' ->
          Sum o $ fmap Negated terms
      BinOpExp (Sub Int64 o) e1 e2 ->
        let terms =
              ( case sumOfProducts e1 of
                  NestedSum (Sum o' terms') | o == o' -> terms'
                  _ -> [Product o [Exp e1]]
              )
                <> ( case sumOfProducts e2 of
                       NestedSum (Sum o' terms') | o == o' -> fmap Negated terms'
                       _ -> [Negated $ Product o [Exp e2]]
                   )
         in Sum o terms
      BinOpExp (Mul Int64 o) e1 e2 ->
        let atoms =
              ( case productOfSums e1 of
                  Product o' atoms' | o == o' -> atoms'
                  _ -> [Exp e1]
              )
                <> ( case productOfSums e2 of
                       Product o' atoms' | o == o' -> atoms'
                       _ -> [Exp e2]
                   )
         in Sum o [Product o atoms]
      _ -> Sum OverflowUndef [Product OverflowUndef [Exp e]]

productOfSums :: Exp -> Term NestedSum
productOfSums e =
  case e of
    BinOpExp (Mul Int64 o) e1 e2 ->
      let atoms =
            ( case productOfSums e1 of
                Product o' atoms' | o == o' -> atoms'
                _ -> [Exp e1]
            )
              <> ( case productOfSums e2 of
                     Product o' atoms' | o == o' -> atoms'
                     _ -> [Exp e2]
                 )
       in Product o atoms
    BinOpExp (Add Int64 _) _ _ ->
      Product OverflowUndef [Nested (sumOfProducts e)]
    _ -> Product OverflowUndef [Exp e]

removeNegations :: [Term NestedSum] -> [Term NestedSum]
removeNegations [] = []
removeNegations (t : ts) =
  case simplifyTerm $ Negated t of
    Just t' ->
      case break (== t') ts of
        (start, _ : rest) -> removeNegations $ start <> rest
        _ -> t : removeNegations ts
    Nothing -> t : removeNegations ts

simplifySum :: NestedSum -> NestedSum
simplifySum s =
  case simplifySum' s of
    Just s'
      | s' == s -> s
      | otherwise -> simplifySum s'
    Nothing ->
      NestedSum $ Sum OverflowUndef []

simplifySum' :: NestedSum -> Maybe NestedSum
simplifySum' (NestedSum (Sum o [])) = Nothing
simplifySum' (NestedSum (Sum o ts)) =
  case removeNegations $ mapMaybe simplifyTerm ts of
    [] -> Nothing
    s -> Just $ NestedSum $ Sum o s

simplifyTerm :: Term NestedSum -> Maybe (Term NestedSum)
simplifyTerm (Product o []) = Nothing
simplifyTerm (Product o atoms) =
  let atoms' = mapMaybe simplifyAtom atoms
   in if null atoms'
        then Nothing
        else
          if Exp (ValueExp $ IntValue $ Int64Value 0) `elem` atoms'
            then Just $ Product o [Exp (ValueExp $ IntValue $ Int64Value 0)]
            else Just $ Product o atoms'
simplifyTerm (Negated (Negated t)) =
  simplifyTerm t
simplifyTerm (Negated t) =
  Negated <$> simplifyTerm t

simplifyAtom :: Atom NestedSum -> Maybe (Atom NestedSum)
simplifyAtom (Exp e) = Just $ Exp e
simplifyAtom (Nested s) =
  case simplifySum' s of
    Nothing -> Just $ Exp $ ValueExp $ IntValue $ Int64Value 0
    Just s' -> Just $ Nested s'

sumToExp :: NestedSum -> Exp
sumToExp (NestedSum (Sum _ [])) =
  ValueExp $ IntValue $ Int64Value 0
sumToExp (NestedSum (Sum o [x])) = termToExp x
sumToExp (NestedSum (Sum o (x : xs))) = BinOpExp (Add Int64 o) (termToExp x) (sumToExp $ NestedSum $ Sum o xs)

termToExp :: Term NestedSum -> Exp
termToExp (Negated t) = BinOpExp (Sub Int64 OverflowUndef) (ValueExp $ IntValue $ Int64Value 0) $ termToExp t
termToExp (Product o []) = ValueExp $ IntValue $ Int64Value 0
termToExp (Product o (atom : atoms)) =
  foldl (BinOpExp (Mul Int64 o)) (atomToExp atom) $ map atomToExp atoms

atomToExp :: Atom NestedSum -> Exp
atomToExp (Exp e) = e
atomToExp (Nested s) = sumToExp s

data Prod
  = Prod
      Bool
      -- ^ True if the product is negated
      [Exp]
  deriving (Show, Eq, Ord)

instance Pretty Prod where
  ppr (Prod b exps) =
    (if b then "-" else "") <> ppr exps

negate :: Prod -> Prod
negate (Prod b es) = Prod (not b) es

flattenNestedSum :: NestedSum -> [Prod]
flattenNestedSum (NestedSum (Sum o ts)) = concatMap flattenTerm ts

flattenTerm :: Term NestedSum -> [Prod]
flattenTerm (Negated t) = map negate $ flattenTerm t
flattenTerm (Product o atoms) =
  case map flattenAtom atoms of
    (first : rest) -> foldr helper first rest
    [] -> []
  where
    helper acc xs = [Prod (b `xor` b') (x <> a) | Prod b a <- acc, Prod b' x <- xs]

flattenAtom :: Atom NestedSum -> [Prod]
flattenAtom (Exp e) = [Prod False [e]]
flattenAtom (Nested s) = flattenNestedSum s

var :: Int -> Exp
var i = LeafExp (VName (nameFromString $ "var_" <> show i) i) (IntType Int64)

val :: Int64 -> Exp
val i = ValueExp $ IntValue $ Int64Value i

add :: Exp -> Exp -> Exp
add x y = BinOpExp (Add Int64 OverflowUndef) x y

mul :: Exp -> Exp -> Exp
mul x y = BinOpExp (Mul Int64 OverflowUndef) x y

test :: NestedSum
test =
  NestedSum $
    Sum
      OverflowUndef
      [ Product
          OverflowUndef
          [ Nested $
              NestedSum $
                Sum
                  OverflowUndef
                  [ Product OverflowUndef [Exp $ var 1, Exp $ var 2],
                    Product OverflowUndef [Exp $ var 3, Exp $ var 4]
                  ],
            Exp $ var 5
          ],
        Product
          OverflowUndef
          [Exp $ var 6],
        Product OverflowUndef [Exp $ var 7]
      ]

test2 :: NestedSum
test2 =
  NestedSum $
    Sum
      OverflowUndef
      [ Product
          OverflowUndef
          [ Nested $
              NestedSum $
                Sum
                  OverflowUndef
                  [ Product OverflowUndef [Exp $ var 1],
                    Negated $ Product OverflowUndef [Exp $ var 2]
                  ],
            Nested $
              NestedSum $
                Sum
                  OverflowUndef
                  [ Product OverflowUndef [Exp $ var 3],
                    Product OverflowUndef [Exp $ var 4]
                  ],
            Exp $ var 5
          ],
        Product OverflowUndef [Exp $ var 6],
        Product OverflowUndef [Exp $ var 7]
      ]
