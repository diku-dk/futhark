module Futhark.Analysis.View.Latex
  ( mkLaTeX,
    LaTeXC (..),
    ToLaTeX (..),
    LaTeX (..),
    prettyLaTeX,
  )
where

import Data.Bifunctor
import Data.List (intersperse)
import Data.List qualified as L
import Futhark.Analysis.View.Representation
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
import qualified Data.List.NonEmpty as NE
import Futhark.Util.Pretty (prettyString, Pretty)

class ToLaTeX a where
  toLaTeX_ :: (Eq l, LaTeXC l) => Int -> a -> l
  toLaTeX :: (Eq l, LaTeXC l) => a -> l
  toLaTeX = toLaTeX_ 0

prettyLaTeX :: (Pretty a, LaTeXC l) => a -> l
prettyLaTeX =
  verbatim . fromString . filter stripId . prettyString
  where
    nums :: [Char]
    nums = "₀₁₂₃₄₅₆₇₈₉"
    stripId c = c `notElem` nums

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

neg :: (LaTeXC l) => l -> l
neg = (comm0 "neg" <>)

num :: (Show x, LaTeXC l) => x -> l
num = fromString . show

concat :: (LaTeXC l) => l -> l -> l
concat = surround (raw "\\mathbin{+\\mkern-10mu+}")

indicator :: (LaTeXC l) => l
indicator = raw "\\mathbb{I}"

substack :: (LaTeXC l) => [l] -> l
substack =
  comm1 "substack" . mconcat . intersperse lnbk

fun :: (LaTeXC l) => l -> [l] -> l
fun f args =
  f <> autoParens (concatWith (surround ", ") args)

(<+>) :: (LaTeXC l) => l -> l -> l
l <+> r = l <> space <> r

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
  toLaTeX_ _ vn = fromString (E.baseString vn) !: num (E.baseTag vn)

instance ToLaTeX Term where
  toLaTeX_ p (Var x) = toLaTeX_ p x
  toLaTeX_ _ (Sum j lb ub e) =
    sumFromTo (toLaTeX j =: toLaTeX lb) (toLaTeX ub) <> autoParens (toLaTeX e)
  toLaTeX_ p (Idx xs i) = toLaTeX_ p xs <> autoSquareBrackets (toLaTeX i)
  toLaTeX_ p (SoP2 sop) = toLaTeX_ p sop
  toLaTeX_ p (Indicator sop) = indicator <> autoParens (toLaTeX_ p sop)
  toLaTeX_ _ (Tuple es) =
    autoParens $ concatWith (surround ", ") $ map toLaTeX es
  toLaTeX_ _ (Bool b) = mathtt $ fromString $ show b
  toLaTeX_ _ (Not x) = neg . autoParens $ toLaTeX_ 9 x
  toLaTeX_ _ (x :== y) = toLaTeX_ 3 x =: toLaTeX_ 3 y
  toLaTeX_ _ (x :< y) = toLaTeX_ 3 x <: toLaTeX_ 3 y
  toLaTeX_ _ (x :> y) = toLaTeX_ 3 x >: toLaTeX_ 3 y
  toLaTeX_ _ (x :/= y) = toLaTeX_ 3 x /=: toLaTeX_ 3 y
  toLaTeX_ _ (x :>= y) = toLaTeX_ 3 x >=: toLaTeX_ 3 y
  toLaTeX_ _ (x :<= y) = toLaTeX_ 3 x <=: toLaTeX_ 3 y
  toLaTeX_ _ (x :&& y) = toLaTeX_ 2 x `wedge` toLaTeX_ 2 y
  toLaTeX_ _ (x :|| y) = toLaTeX_ 1 x `vee` toLaTeX_ 1 y
  toLaTeX_ _ Recurrence = raw "\\circlearrowleft"

instance (ToLaTeX a) => ToLaTeX [a] where
  toLaTeX_ p = mconcat . intersperse lnbk . map (toLaTeX_ p)

instance (ToLaTeX a) => ToLaTeX (Cases a) where
  toLaTeX_ _ (Cases cs) = cases . mconcat . intersperse lnbk . map f $ NE.toList cs
    where
      f (c, v) = toLaTeX c <> implies <> toLaTeX v 

autoOpenEndedInterval :: LaTeXC l => l -> l
autoOpenEndedInterval x = commS "left[" <> x <> commS "right)"


catFromTo :: LaTeXC a => a -> a -> a
catFromTo x y = commS "biguplus" <> raw"_" <> braces x <> raw"^" <> braces y

instance ToLaTeX Domain where
  toLaTeX_ _ (Iota e) = mathtt "iota" <> space <> toLaTeX e
  toLaTeX_ _ (Cat k m b) =
    catFromTo (toLaTeX k =: "0") (toLaTeX (m ~-~ SoP2 (SoP.int2SoP 1)))
      <> autoOpenEndedInterval
           (toLaTeX b
              <> raw ", \\dots, "
              <> toLaTeX (termToSoP $ substituteName k (Var k ~+~ SoP2 (SoP.int2SoP 1)) b))

instance ToLaTeX IndexFn where
  toLaTeX_ _ (IndexFn (Forall i dom@(Iota {})) e) =
    equation_ $ forall <> toLaTeX i `in_` toLaTeX dom <> raw "~.~" <> toLaTeX e
  toLaTeX_ _ (IndexFn (Forall i dom) e) =
    align_ [forall <> toLaTeX i `in_` mempty & toLaTeX dom <> raw "~.~" <> lnbk & toLaTeX e]
  toLaTeX_ _ (IndexFn Empty e) = equation_ $ raw "\\bullet~.~" <> toLaTeX e

instance ToLaTeX (E.VName, IndexFn) where
  toLaTeX_ _ (vn, IndexFn (Forall i dom) e) =
    align_ [
      toLaTeX vn & mempty
        =: forall <> toLaTeX i `in_` toLaTeX dom <> raw "~.~" <> lnbk & toLaTeX e
    ]
  toLaTeX_ _ (vn, IndexFn Empty e) =
    align_ [
      toLaTeX vn & mempty
        =: raw "\\bullet~.~" <> lnbk & toLaTeX e
    ]

instance ToLaTeX (E.VName, Term) where
  toLaTeX_ _ (vn, t) =
    equation_ (toLaTeX vn =: toLaTeX t)

instance ToLaTeX E.Exp where
  toLaTeX_ _ = prettyLaTeX

instance ToLaTeX E.Pat where
  toLaTeX_ _ = texttt . fromString . prettyString

instance ToLaTeX LaTeX where
  toLaTeX_ _ = fromLaTeX

mkLaTeX :: (ToLaTeX a) => FilePath -> [a] -> IO ()
mkLaTeX fp as =
  renderFile fp content
  where
    content :: LaTeX
    content =
      mconcat $
        intersperse
          "\n"
          [ documentclass [a0paper] article,
            usepackage [] qtree,
            usepackage [utf8] inputenc,
            usepackage [] amsmath,
            usepackage [] amssymb,
            fit $
              mconcat $
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
            raw "\\hoffset=0in",
            raw "\\voffset=0in",
            raw "\\pdfpagewidth=\\paperwidth",
            raw "\\pdfpageheight=\\paperheight",
            raw "\\end{document}"
          ]

    vbox x = raw "\\setbox0" <> liftL (\l -> TeXComm "vbox" [FixArg l]) x
