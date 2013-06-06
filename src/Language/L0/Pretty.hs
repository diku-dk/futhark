{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | L0 prettyprinter.  This module defines 'Pretty' instances for the
-- AST defined in "Language.L0.Syntax", but also a number of
-- convenience functions if you don't want to use the interface from
-- 'Pretty'.
module Language.L0.Pretty
  ( ppType
  , ppValue
  , ppExp
  , ppTupId
  , prettyPrint
  )
  where

import Data.Array

import Text.PrettyPrint.Mainland

import Language.L0.Syntax
import Language.L0.Attributes

tildes :: String -> String
tildes = map tilde
  where tilde '-' = '~'
        tilde c   = c

instance Pretty Name where
  ppr (Name t) = fromText t

instance Pretty Value where
  ppr (IntVal x) = text $ tildes $ show x
  ppr (CharVal c) = text $ show c
  ppr (LogVal b) = text $ show b
  ppr (RealVal x) = text $ tildes $ show x
  ppr (TupVal vs) = tuple $ map ppr vs
  ppr v@(ArrayVal a t)
    | Just s <- arrayString v = text $ show s
    | [] <- elems a = text "empty" <> parens (ppr t)
    | otherwise     = braces $ commasep $ map ppr $ elems a

instance Pretty ElemType where
  ppr Int = text "int"
  ppr Char = text "char"
  ppr Bool = text "bool"
  ppr Real = text "real"
  ppr (Tuple ets) = tuple $ map ppr ets

instance Pretty Type where
  ppr (Elem et) = ppr et
  ppr (Array et ds u) = u' <> foldl f (ppr et) ds
    where f s Nothing = brackets s
          f s (Just e) = brackets $ s <> comma <> ppr e
          u' | Unique <- u = star
             | otherwise = empty

instance (Eq vn, Pretty vn) => Pretty (IdentBase ty vn) where
  ppr = ppr . identName

instance (Eq vn, Pretty vn) => Pretty (ExpBase ty vn) where
  ppr = pprPrec 0
  pprPrec _ (Var v) = ppr v
  pprPrec _ (Literal v _) = ppr v
  pprPrec _ (TupLit es _) = tuple $ map ppr es
  pprPrec _ (ArrayLit es _ _) = braces $ commasep $ map ppr es
  pprPrec p (BinOp bop x y _ _) = ppBinOp p bop x y
  pprPrec p (And x y _) = ppBinOp p LogAnd x y
  pprPrec p (Or x y _) = ppBinOp p LogOr x y
  pprPrec _ (Not e _) = text "not" <+> pprPrec 9 e
  pprPrec _ (Negate e _ _) = text "~" <> pprPrec 9 e
  pprPrec _ (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  pprPrec _ (Apply fname args _ _) = text (nameToString fname) <>
                                     tuple (map (align . ppr) args)
  pprPrec _ (LetPat pat e body _) = text "let" <+> align (ppr pat) <+>
                                    equals <+> align (ppr e) <+> text "in" </>
                                    ppr body
  pprPrec _ (LetWith dest src idxs ve body _)
    | dest == src =
      text "let" <+> ppr dest <+> list (map ppr idxs) <+>
      equals <+> ppr ve <+>
      text "in" </> ppr body
    | otherwise =
      text "let" <+> ppr dest <+> equals <+> ppr src <+>
      text "with" <+> list (map ppr idxs) <+> text "<-" <+> ppr ve <+>
      text "in" </> ppr body
  pprPrec _ (Index v idxs _ _) = ppr v <> list (map ppr idxs)
  pprPrec _ (Iota e _) = text "iota" <> parens (ppr e)
  pprPrec _ (Size e _) = text "size" <> parens (ppr e)
  pprPrec _ (Replicate ne ve _) =
    text "replicate" <> tuple [ppr ne, ppr ve]
  pprPrec _ (Reshape shape e _) =
    text "replicate" <> tuple [tuple (map ppr shape), ppr e]
  pprPrec _ (Transpose e _) = text "transpose" <> parens (ppr e)
  pprPrec _ (Map lam a _ _) = ppSOAC "map" [lam] [a]
  pprPrec _ (Mapall lam a _) = ppSOAC "mapall" [lam] [a]
  pprPrec _ (Reduce lam e a _ _) = ppSOAC "reduce" [lam] [e, a]
  pprPrec _ (Redomap redlam maplam e a _ _) =
    ppSOAC "redomap" [redlam, maplam] [e, a]
  pprPrec _ (Scan lam e a _ _) = ppSOAC "scan" [lam] [e, a]
  pprPrec _ (Filter lam a _ _) = ppSOAC "filter" [lam] [a]
  pprPrec _ (Zip es _) = text "zip" <> tuple (map (ppr . fst) es)
  pprPrec _ (Unzip e _ _) = text "unzip" <> parens (ppr e)
  pprPrec _ (Split e a _ _) = text "split" <> tuple [ppr e, ppr a]
  pprPrec _ (Concat x y _) = text "concat" <> tuple [ppr x, ppr y]
  pprPrec _ (Copy e _) = text "copy" <> parens (ppr e)
  pprPrec _ (DoLoop pat initexp i bound loopbody letbody _) =
    text "loop" <+> parens (ppr pat <+> equals <+> ppr initexp) <+>
    equals <+> text "for" <+> ppr i <+> langle <+> ppr bound <+> text "do" </>
    indent 2 (ppr loopbody) <+> text "in" </>
    ppr letbody
  pprPrec _ (Map2 lam as _ _) = ppSOAC "map2" [lam] as
  pprPrec _ (Mapall2 lam as _) = ppSOAC "mapall2" [lam] as
  pprPrec _ (Reduce2 lam e as _ _) = ppSOAC "reduce" [lam] $ e:as
  pprPrec _ (Redomap2 redlam maplam e as _ _) =
    ppSOAC "redomap2" [redlam, maplam] $ e:as
  pprPrec _ (Scan2 lam e as _ _) = ppSOAC "scan2" [lam] $ e:as
  pprPrec _ (Filter2 lam as _) = ppSOAC "filter2" [lam] as

instance (Eq vn, Pretty vn) => Pretty (TupIdentBase ty vn) where
  ppr (Id ident) = ppr ident
  ppr (TupId pats _) = tuple $ map ppr pats

instance (Eq vn, Pretty vn) => Pretty (LambdaBase ty vn) where
  ppr (CurryFun fname [] _ _) = text $ nameToString fname
  ppr (CurryFun fname curryargs _ _) =
    text (nameToString fname) <+> tuple (map ppr curryargs)
  ppr (AnonymFun params body rettype _) =
    text "fn" <+> ppr rettype <+>
    tuple (map ppParam params) <+>
    text "=>" <+> ppr body

instance (Eq vn, Pretty vn) => Pretty (ProgBase ty vn) where
  ppr = stack . punctuate line . map ppFun . progFunctions
    where ppFun (name, rettype, args, body, _) =
            text "fun" <+> ppr rettype <+>
            text (nameToString name) <//>
            tuple (map ppParam args) <+>
            equals </> indent 2 (ppr body)

ppParam :: (Eq vn, Pretty ty, Pretty vn) => IdentBase ty vn -> Doc
ppParam param = ppr (identType param) <+> ppr param

ppBinOp :: (Eq vn, Pretty vn) => Int -> BinOp -> ExpBase ty vn -> ExpBase ty vn -> Doc
ppBinOp p bop x y = parensIf (p > precedence bop) $
                    pprPrec (precedence bop) x <+>
                    text (opStr bop) <+>
                    pprPrec (precedence bop) y
  where precedence LogAnd = 0
        precedence LogOr = 0
        precedence Band = 1
        precedence Bor = 1
        precedence Xor = 1
        precedence Equal = 2
        precedence Less = 2
        precedence Leq = 2
        precedence ShiftL = 3
        precedence ShiftR = 3
        precedence Plus = 4
        precedence Minus = 4
        precedence Times = 5
        precedence Divide = 5
        precedence Mod = 5
        precedence Pow = 6

ppSOAC :: (Eq vn, Pretty vn) => String -> [LambdaBase ty vn] -> [ExpBase ty vn] -> Doc
ppSOAC name [] es = text name <> tuple (map ppr es)
ppSOAC name (fun:funs) es =
  text name <> parens (foldl ppfun (ppr fun) funs <> comma <//> commasep (map ppr es))
  where ppfun s fun' = s <//> comma <> ppr fun'

render80 :: Pretty a => a -> String
render80 = pretty 80 . ppr

-- | Prettyprint a value, wrapped to 80 characters.
ppValue :: Value -> String
ppValue = render80

-- | Prettyprint a type, wrapped to 80 characters.
ppType :: Type -> String
ppType = render80

-- | Prettyprint an expression, wrapped to 80 characters.
ppExp :: (Eq vn, Pretty vn) => ExpBase ty vn -> String
ppExp = render80

-- | Prettyprint a pattern, wrapped to 80 characters.
ppTupId :: (Eq vn, Pretty vn) => TupIdentBase ty vn -> String
ppTupId = render80

-- | Prettyprint an entire L0 program, wrapped to 80 characters.
prettyPrint :: (Eq vn, Pretty vn) => ProgBase ty vn -> String
prettyPrint = render80
