{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Futhark.Analysis.AlgSimplify (simplify, Atom (..), Term (..), Sum (..), simplifySum, simplifySum') where

import Data.Function ((&))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Futhark.Analysis.PrimExp
import Futhark.IR.Syntax.Core
import Futhark.Util.Pretty
import GHC.Generics

type Exp = PrimExp VName

type LookupTable = M.Map VName Exp

data Sum
  = Sum Overflow [Term]
  deriving (Show, Eq, Generic)

instance Pretty Sum where
  ppr (Sum _ terms) = "Sum " <> brackets (commasep $ fmap ppr terms)

data Term
  = Negated Term
  | Product Overflow [Atom]
  deriving (Show, Eq, Generic)

instance Pretty Term where
  ppr (Negated term) = "-" <> ppr term
  ppr (Product _ atoms) = "Product " <> brackets (commasep $ fmap ppr atoms)

data Atom
  = Exp Exp
  | NestedSum Sum
  deriving (Show, Eq, Generic)

instance Pretty Atom where
  ppr (Exp e) = ppr e
  ppr (NestedSum s) = parens $ ppr s

simplify :: LookupTable -> TPrimExp Int64 VName -> TPrimExp Int64 VName
simplify table e =
  untyped e
    & sumOfProducts
    & simplifySum
    & lookupExps table
    & sumToExp
    & TPrimExp

lookupExps :: LookupTable -> Sum -> Sum
lookupExps table (Sum o terms) =
  Sum o $ fmap lookupExpsInTerm terms
  where
    lookupExpsInTerm (Negated term) = Negated $ lookupExpsInTerm term
    lookupExpsInTerm (Product o' atoms) = Product o' $ fmap lookupExpsInAtom atoms

    lookupExpsInAtom e@(Exp (LeafExp v _)) = maybe e (lookupExpsInAtom . Exp) $ M.lookup v table
    lookupExpsInAtom (NestedSum s) = NestedSum $ lookupExps table s
    lookupExpsInAtom a = a

sumOfProducts :: Exp -> Sum
sumOfProducts e =
  case e of
    BinOpExp (Add Int64 o) e1 e2 ->
      let terms =
            ( case sumOfProducts e1 of
                Sum o' terms' | o == o' -> terms'
                _ -> [Product o [Exp e1]]
            )
              <> ( case sumOfProducts e2 of
                     Sum o' terms' | o == o' -> terms'
                     _ -> [Product o [Exp e2]]
                 )
       in Sum o terms
    BinOpExp (Sub Int64 o) (ValueExp (IntValue (Int64Value 0))) e'
      | Sum o' terms <- sumOfProducts e',
        o == o' ->
        Sum o $ fmap Negated terms
    BinOpExp (Sub Int64 o) e1 e2 ->
      let terms =
            ( case sumOfProducts e1 of
                Sum o' terms' | o == o' -> terms'
                _ -> [Product o [Exp e1]]
            )
              <> ( case sumOfProducts e2 of
                     Sum o' terms' | o == o' -> fmap Negated terms'
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

productOfSums :: Exp -> Term
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
      Product OverflowUndef [NestedSum (sumOfProducts e)]
    _ -> Product OverflowUndef [Exp e]

removeNegations :: [Term] -> [Term]
removeNegations [] = []
removeNegations (t : ts) =
  case simplifyTerm $ Negated t of
    Just t' ->
      case break (== t') ts of
        (start, _ : rest) -> removeNegations $ start <> rest
        _ -> t : removeNegations ts
    Nothing -> t : removeNegations ts

simplifySum :: Sum -> Sum
simplifySum s =
  case simplifySum' s of
    Just s'
      | s' == s -> s
      | otherwise -> simplifySum s'
    Nothing ->
      Sum OverflowUndef []

simplifySum' :: Sum -> Maybe Sum
simplifySum' (Sum o []) = Nothing
simplifySum' (Sum o ts) =
  case removeNegations $ mapMaybe simplifyTerm ts of
    [] -> Nothing
    s -> Just $ Sum o s

simplifyTerm :: Term -> Maybe Term
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

simplifyAtom :: Atom -> Maybe Atom
simplifyAtom (Exp e) = Just $ Exp e
simplifyAtom (NestedSum s) =
  case simplifySum' s of
    Nothing -> Just $ Exp $ ValueExp $ IntValue $ Int64Value 0
    Just s' -> Just $ NestedSum s'

sumToExp :: Sum -> Exp
sumToExp (Sum _ []) =
  ValueExp $ IntValue $ Int64Value 0
sumToExp (Sum o [x]) = termToExp x
sumToExp (Sum o (x : xs)) = BinOpExp (Add Int64 o) (termToExp x) (sumToExp $ Sum o xs)

termToExp :: Term -> Exp
termToExp (Negated t) = BinOpExp (Sub Int64 OverflowUndef) (ValueExp $ IntValue $ Int64Value 0) $ termToExp t
termToExp (Product o []) = ValueExp $ IntValue $ Int64Value 0
termToExp (Product o (atom : atoms)) =
  foldl (BinOpExp (Mul Int64 o)) (atomToExp atom) $ map atomToExp atoms

atomToExp :: Atom -> Exp
atomToExp (Exp e) = e
atomToExp (NestedSum s) = sumToExp s
