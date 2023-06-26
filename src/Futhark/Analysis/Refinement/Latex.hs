module Futhark.Analysis.Refinement.Latex
  ( mkLaTeX,
    LaTeXC (..),
    ToLaTeX (..),
    LaTeX (..),
  )
where

import Data.Bifunctor
import Data.List (intersperse)
import Data.List qualified as L
import Data.Set qualified as S
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Representation
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark qualified as E
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Trees.Qtree
import Prelude hiding (concat)

class ToLaTeX a where
  toLaTeX_ :: (Eq l, LaTeXC l) => Int -> a -> l
  toLaTeX :: (Eq l, LaTeXC l) => a -> l
  toLaTeX = toLaTeX_ 0

concatWith :: (Foldable t, LaTeXC l) => (l -> l -> l) -> t l -> l
concatWith _ ls
  | null ls = mempty
concatWith op ls =
  foldr1 op ls

joinWith :: (Eq l, LaTeXC l) => (l -> l -> l) -> l -> l -> l
joinWith _ l r
  | r == mempty = l
joinWith _ l r
  | l == mempty = r
joinWith op l r = l `op` r

enclose :: (LaTeXC l) => l -> l -> l -> l
enclose l r x = l <> x <> r

surround :: (LaTeXC l) => l -> l -> l -> l
surround x l r = enclose l r x

emptySet :: (LaTeXC l) => l
emptySet = raw "\\varnothing"

neg :: (LaTeXC l) => l -> l
neg = (comm0 "neg" <>)

num :: (Show x, LaTeXC l) => x -> l
num = fromString . show

concat :: (LaTeXC l) => l -> l -> l
concat = surround (raw "\\mathbin{+\\mkern-10mu+}")

substack :: (LaTeXC l) => [l] -> l
substack =
  comm1 "substack" . mconcat . intersperse lnbk

fun :: (LaTeXC l) => l -> [l] -> l
fun f args =
  f <> autoParens (concatWith (surround ", ") args)

(<+>) :: (LaTeXC l) => l -> l -> l
l <+> r = l <> space <> r

bigC :: (LaTeXC l) => l -> l
bigC x = raw "\\makebox{\\huge\\ensuremath{C}}" !: x

autoParensP :: (LaTeXC l) => Int -> Int -> l -> l
autoParensP prec new_prec l
  | prec >= new_prec = autoParens l
  | otherwise = l

instance (ToLaTeX u) => ToLaTeX (SoP.Term u) where
  toLaTeX_ p t =
    autoParensP p 7 $
      concatWith cdot $
        map (toLaTeX_ 7) $
          SoP.termToList t

instance (ToLaTeX u, Ord u) => ToLaTeX (SoP.SoP u) where
  toLaTeX_ p sop
    | Just c <- SoP.justConstant sop = num c
    | null pos_terms =
        (if length neg_neg_terms > 1 then autoParensP p 6 else id)
          ( "-" <> concatWith (surround "-") (map mult neg_neg_terms)
          )
    | otherwise =
        autoParensP p 6 $
          joinWith
            (surround "-")
            (concatWith (surround "+") (map mult pos_terms))
            (concatWith (surround "-") (map mult neg_neg_terms))
    where
      mult (t, n)
        | SoP.isConstTerm t = num n
      mult (t, 1) = toLaTeX_ p t
      mult (t, n) = num n `cdot` toLaTeX_ 7 t
      (pos_terms, neg_terms) = L.partition ((>= 0) . snd) $ SoP.sopToList sop
      neg_neg_terms = map (second negate) neg_terms

instance ToLaTeX E.VName where
  toLaTeX_ _ vn = fromString $ E.baseString vn

instance ToLaTeX Hole where
  toLaTeX_ _ _ = square

instance ToLaTeX Term where
  toLaTeX_ p (Var x) = toLaTeX_ p x
  toLaTeX_ p (THole h) = toLaTeX_ p h
  toLaTeX_ p (SoP sop) = toLaTeX_ p sop
  toLaTeX_ _ (Len x) = mathtt "len" <> autoParens (toLaTeX x)
  toLaTeX_ _ (Elems x) = mathtt "elems" <> autoParens (toLaTeX x)
  toLaTeX_ _ (Set es) =
    autoBraces $ concatWith (surround ", ") $ map toLaTeX $ S.toList es
  toLaTeX_ _ (Array es) =
    autoSquareBrackets $ concatWith (surround ", ") $ map toLaTeX es
  toLaTeX_ _ (Range from step to') =
    autoBraces $ toLaTeX from <> "," <> toLaTeX (from ~+~ step) <> "," <> ldots <> "," <> toLaTeX to'
  toLaTeX_ _ (Idx x y) =
    toLaTeX_ 9 x <> autoSquareBrackets (toLaTeX y)
  toLaTeX_ p (Union x y) =
    autoParensP p 4 $ toLaTeX_ 4 x `cup` toLaTeX_ 4 y
  toLaTeX_ _ (Unions i set conds x) =
    bigcupFromTo (substack [toLaTeX i `in_` toLaTeX set, toLaTeX conds]) mempty <> autoBraces (toLaTeX x)
  toLaTeX_ _ (Sigma i set x) =
    sumFromTo (toLaTeX i `in_` toLaTeX set) mempty <> toLaTeX_ 9 x
  toLaTeX_ _ (x :< y) = toLaTeX_ 3 x <: toLaTeX_ 3 y
  toLaTeX_ _ (x :<= y) = toLaTeX_ 3 x <=: toLaTeX_ 3 y
  toLaTeX_ _ (x :> y) = toLaTeX_ 3 x >: toLaTeX_ 3 y
  toLaTeX_ _ (x :>= y) = toLaTeX_ 3 x >=: toLaTeX_ 3 y
  toLaTeX_ _ (x :== y) = toLaTeX_ 3 x =: toLaTeX_ 3 y
  toLaTeX_ _ (x :/= y) = toLaTeX_ 3 x /=: toLaTeX_ 3 y
  toLaTeX_ _ (PermutationOf x y) =
    fun (mathtt "permutation_of") $ map toLaTeX [x, y]
  toLaTeX_ _ (Bool b) = mathtt $ fromString $ show b
  toLaTeX_ _ (Not x) = neg $ toLaTeX_ 9 x
  toLaTeX_ p (CNFTerm cnf) = toLaTeX_ p cnf
  toLaTeX_ p (If c t f) =
    autoParensP p 4 $
      mathtt "if"
        <+> toLaTeX_ 4 c
        <+> mathtt "then"
        <+> toLaTeX_ 4 t
        <+> mathtt "else"
        <+> toLaTeX_ 4 f
  toLaTeX_ _ (BoolToInt x) =
    mathtt "bool_to_int" <> autoParens (toLaTeX x)
  toLaTeX_ _ (Forall {}) = undefined

instance (ToLaTeX a) => ToLaTeX (Or a) where
  toLaTeX_ p x =
    (if length (ors x) > 1 then autoParensP p 1 else id) $
      concatWith vee $
        map (toLaTeX_ 1) $
          ors x

instance (ToLaTeX a) => ToLaTeX (And a) where
  toLaTeX_ p x =
    (if length (ands x) > 1 then autoParensP p 2 else id) $
      concatWith wedge $
        map (toLaTeX_ 2) $
          ands x

instance (ToLaTeX a) => ToLaTeX (CNF a) where
  toLaTeX_ p = toLaTeX_ p . getCNF

instance (ToLaTeX a) => ToLaTeX [a] where
  toLaTeX_ p = mconcat . intersperse lnbk . map (toLaTeX_ p)

instance ToLaTeX LaTeX where
  toLaTeX_ _ = fromLaTeX

mkLaTeX :: (ToLaTeX a) => FilePath -> [a] -> IO ()
mkLaTeX fp as =
  renderFile fp content
  where
    content :: LaTeX
    content =
      mconcat
        [ documentclass [a0paper] article,
          usepackage [] qtree,
          usepackage [utf8] inputenc,
          usepackage [] amsmath,
          usepackage [] amssymb,
          fit $
            align_ $
              intersperse "\n" $
                map toLaTeX as
        ]
    fit x =
      mconcat $
        intersperse
          "\n"
          [ raw "\\begin{document}",
            raw "\\hoffset=-1in",
            raw "\\voffset=-1in",
            vbox x,
            raw "\\pdfpageheight=\\dimexpr\\ht0+\\dp0\\relax",
            raw "\\pdfpagewidth=\\wd0",
            raw "\\shipout\\box0",
            raw "\\stop"
          ]

    vbox x = raw "\\setbox0" <> liftL (\l -> TeXComm "vbox" [FixArg l]) x
