{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances for the
-- AST defined in "Language.Futhark.Syntax", but also a number of
-- convenience functions if you don't want to use the interface from
-- 'Pretty'.
module Language.Futhark.Pretty
  ( ppType
  , ppValue
  , ppExp
  , ppLambda
  , ppTupId
  , prettyPrint
  )
  where

import Data.Array
import Data.Hashable
import qualified Data.HashSet as HS

import Text.PrettyPrint.Mainland

import Language.Futhark.Syntax
import Language.Futhark.Attributes

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = encloseSep lparen rparen comma . map align

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

aliasComment :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => TupIdentBase ty vn -> Doc -> Doc
aliasComment pat d = case aliasComment' pat of
                       []   -> d
                       l:ls -> foldl (</>) l ls </> d
  where aliasComment' (Wildcard {}) = []
        aliasComment' (TupId pats _) = concatMap aliasComment' pats
        aliasComment' (Id ident) =
          case maybe [] (clean . HS.toList . aliases)
                 $ unboxType $ identType ident of
            [] -> []
            als -> [oneline $
                    text "// " <> ppr ident <> text " aliases " <>
                    commasep (map ppr als)]
          where clean = filter (/= identName ident)
                oneline s = text $ displayS (renderCompact s) ""

instance Pretty Value where
  ppr (BasicVal bv) = ppr bv
  ppr (TupVal vs)
    | any (not . basicType . valueType) vs =
      braces $ commastack $ map ppr vs
    | otherwise =
      braces $ commasep $ map ppr vs
  ppr v@(ArrayVal a t)
    | Just s <- arrayString v = text $ show s
    | [] <- elems a = text "empty" <> parens (ppr t)
    | Array {} <- t = brackets $ commastack $ map ppr $ elems a
    | otherwise     = brackets $ commasep $ map ppr $ elems a

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TupleArrayElemTypeBase as vn) where
  ppr (BasicArrayElem bt _) = ppr bt
  ppr (ArrayArrayElem at)   = ppr at
  ppr (TupleArrayElem ts)   = braces $ commasep $ map ppr ts

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ArrayTypeBase as vn) where
  ppr (BasicArray et ds u _) = u' <> foldl f (ppr et) ds
    where f s Nothing = brackets s
          f s (Just e) = brackets $ s <> comma <> ppr e
          u' | Unique <- u = star
             | otherwise = empty
  ppr (TupleArray et ds u) = u' <> foldl f (braces $ commasep $ map ppr et) ds
    where f s Nothing = brackets s
          f s (Just e) = brackets $ s <> comma <> ppr e
          u' | Unique <- u = star
             | otherwise = empty

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TypeBase as vn) where
  ppr (Basic et) = ppr et
  ppr (Array at) = ppr at
  ppr (Tuple ts) = braces $ commasep $ map ppr ts

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (IdentBase ty vn) where
  ppr = ppr . identName

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit (ArrayLit {}) = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit (Literal val _) = hasArrayVal val
hasArrayLit _ = False

hasArrayVal :: Value -> Bool
hasArrayVal (ArrayVal {}) = True
hasArrayVal (TupVal vs) = any hasArrayVal vs
hasArrayVal _ = False

instance (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => Pretty (ExpBase ty vn) where
  ppr = pprPrec (-1)
  pprPrec _ (Var v) = ppr v
  pprPrec _ (Literal v _) = ppr v
  pprPrec _ (TupLit es _)
    | any hasArrayLit es = braces $ commastack $ map ppr es
    | otherwise          = braces $ commasep $ map ppr es
  pprPrec _ (ArrayLit es rt _) =
    case unboxType rt of
      Just (Array {}) -> brackets $ commastack $ map ppr es
      _               -> brackets $ commasep $ map ppr es
  pprPrec p (BinOp bop x y _ _) = ppBinOp p bop x y
  pprPrec _ (Not e _) = text "not" <+> pprPrec 9 e
  pprPrec _ (Negate e _) = text "-" <> pprPrec 9 e
  pprPrec _ (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  pprPrec _ (Apply fname args _ _) = text (nameToString fname) <>
                                     apply (map (align . ppr . fst) args)
  pprPrec p (LetPat pat e body _) =
    aliasComment pat $ mparens $ align $
    text "let" <+> align (ppr pat) <+>
    (if linebreak
     then equals </> indent 2 (ppr e)
     else equals <+> align (ppr e)) <+> text "in" </>
    ppr body
    where mparens = if p == -1 then id else parens
          linebreak = case e of
                        Map {} -> True
                        Reduce {} -> True
                        Filter {} -> True
                        Redomap {} -> True
                        Scan {} -> True
                        DoLoop {} -> True
                        LetPat {} -> True
                        LetWith {} -> True
                        Literal (ArrayVal {}) _ -> False
                        If {} -> True
                        ArrayLit {} -> False
                        _ -> hasArrayLit e
  pprPrec _ (LetWith cs dest src idxcs idxs ve body _)
    | dest == src =
      text "let" <+> ppCertificates cs <> ppr dest <+> list (map ppr idxs) <+>
      equals <+> align (ppr ve) <+>
      text "in" </> ppr body
    | otherwise =
      text "let" <+> ppCertificates cs <> ppr dest <+> equals <+> ppr src <+>
      text "with" <+> brackets (ppcs <> commasep (map ppr idxs)) <+>
      text "<-" <+> align (ppr ve) <+>
      text "in" </> ppr body
    where ppcs = case idxcs of Nothing     -> empty
                               Just []     -> text "<>|"
                               Just csidx' -> ppCertificates csidx' <> text "|"
  pprPrec _ (Index cs v csidx idxs _) =
    ppCertificates cs <> ppr v <>
    brackets (ppcs <> commasep (map ppr idxs))
    where ppcs = case csidx of Nothing     -> empty
                               Just []     -> text "<>|"
                               Just csidx' -> ppCertificates csidx' <> text "|"
  pprPrec _ (Iota e _) = text "iota" <> parens (ppr e)
  pprPrec _ (Size cs i e _) =
    ppCertificates cs <> text "size" <> apply [text $ show i, ppr e]
  pprPrec _ (Replicate ne ve _) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  pprPrec _ (Reshape cs shape e _) =
    ppCertificates cs <> text "reshape" <> apply [apply (map ppr shape), ppr e]
  pprPrec _ (Rearrange cs perm e _) =
    ppCertificates cs <> text "rearrange" <> apply [apply (map ppr perm), ppr e]
  pprPrec _ (Rotate cs n e _) =
    ppCertificates cs <> text "rotate" <> apply [ppr n, ppr e]
  pprPrec _ (Transpose cs 0 1 e _) =
    ppCertificates cs <> text "transpose" <> apply [ppr e]
  pprPrec _ (Transpose cs k n e _) =
        ppCertificates cs <>
        text "transpose" <> apply [text $ show k,
                                   text $ show n,
                                   ppr e]
  pprPrec _ (Map lam a _) = ppSOAC "map" [lam] [a]
  pprPrec _ (ConcatMap lam a as _) = ppSOAC "concatMap" [lam] $ a : as
  pprPrec _ (Reduce lam e a _) = ppSOAC "reduce" [lam] [e, a]
  pprPrec _ (Redomap redlam maplam e a _) =
    ppSOAC "redomap" [redlam, maplam] [e, a]
  pprPrec _ (Scan lam e a _) = ppSOAC "scan" [lam] [e, a]
  pprPrec _ (Filter lam a _) = ppSOAC "filter" [lam] [a]
  pprPrec _ (Zip es _) = text "zip" <> apply (map (ppr . fst) es)
  pprPrec _ (Unzip e _ _) = text "unzip" <> parens (ppr e)
  pprPrec _ (Split cs e a _) =
    ppCertificates cs <> text "split" <> apply [ppr e, ppr a]
  pprPrec _ (Concat cs x y _) =
    ppCertificates cs <> text "concat" <> apply [ppr x, ppr y]
  pprPrec _ (Copy e _) = text "copy" <> parens (ppr e)
  pprPrec _ (Assert e _) = text "assert" <> parens (ppr e)
  pprPrec _ (Conjoin es _) = text "conjoin" <> parens (commasep $ map ppr es)
  pprPrec _ (DoLoop pat initexp i bound loopbody letbody _) =
    aliasComment pat $
    text "loop" <+> parens (ppr pat <+> equals <+> ppr initexp) <+>
    equals <+> text "for" <+> ppr i <+> text "<" <+> align (ppr bound) <+> text "do" </>
    indent 2 (ppr loopbody) <+> text "in" </>
    ppr letbody

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TupIdentBase ty vn) where
  ppr (Id ident)     = ppr ident
  ppr (TupId pats _) = braces $ commasep $ map ppr pats
  ppr (Wildcard _ _) = text "_"

instance (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => Pretty (LambdaBase ty vn) where
  ppr (CurryFun fname [] _ _) = text $ nameToString fname
  ppr (CurryFun fname curryargs _ _) =
    text (nameToString fname) <+> apply (map ppr curryargs)
  ppr (AnonymFun params body rettype _) =
    text "fn" <+> ppr rettype <+>
    apply (map ppParam params) <+>
    text "=>" </> indent 2 (ppr body)

instance (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => Pretty (ProgBase ty vn) where
  ppr = stack . punctuate line . map ppFun . progFunctions
    where ppFun (name, rettype, args, body, _) =
            text "fun" <+> ppr rettype <+>
            text (nameToString name) <//>
            apply (map ppParam args) <+>
            equals </> indent 2 (ppr body)

ppParam :: (Eq vn, Hashable vn, Pretty (ty vn), Pretty vn, TypeBox ty) => IdentBase ty vn -> Doc
ppParam param = ppr (identType param) <+> ppr param

ppBinOp :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => Int -> BinOp -> ExpBase ty vn -> ExpBase ty vn -> Doc
ppBinOp p bop x y = parensIf (p > precedence bop) $
                    pprPrec (precedence bop) x <+/>
                    text (opStr bop) <+>
                    pprPrec (rprecedence bop) y
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
        rprecedence Minus = 10
        rprecedence Divide = 10
        rprecedence op = precedence op

ppSOAC :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty, Pretty fn) =>
          String -> [fn] -> [ExpBase ty vn] -> Doc
ppSOAC name funs es =
  text name <> parens (ppList funs </>
                       commasep (map ppr es))

ppList :: (Pretty a) => [a] -> Doc
ppList as = case map ppr as of
              []     -> empty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'

ppCertificates :: (Eq vn, Hashable vn, TypeBox ty, Pretty vn) => CertificatesBase ty vn -> Doc
ppCertificates [] = empty
ppCertificates cs = text "<" <> commasep (map ppr cs) <> text ">"

render80 :: Pretty a => a -> String
render80 = pretty 80 . ppr

-- | Prettyprint a value, wrapped to 80 characters.
ppValue :: Value -> String
ppValue = render80

-- | Prettyprint a type, wrapped to 80 characters.
ppType :: (Eq vn, Hashable vn, Pretty vn) => TypeBase as vn -> String
ppType = render80

-- | Prettyprint an expression, wrapped to 80 characters.
ppExp :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => ExpBase ty vn -> String
ppExp = render80

-- | Prettyprint a lambda, wrapped to 80 characters.
ppLambda :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => LambdaBase ty vn -> String
ppLambda = render80

-- | Prettyprint a pattern, wrapped to 80 characters.
ppTupId :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => TupIdentBase ty vn -> String
ppTupId = render80

-- | Prettyprint an entire Futhark program, wrapped to 80 characters.
prettyPrint :: (Eq vn, Hashable vn, Pretty vn, TypeBox ty) => ProgBase ty vn -> String
prettyPrint = render80
