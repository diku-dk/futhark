{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.Representation.AST.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.Representation.AST.Pretty
  ( prettyTuple
  , pretty
  , PrettyLore (..)
  )
  where

import Data.Array

import Text.PrettyPrint.Mainland hiding (pretty)
import qualified Text.PrettyPrint.Mainland as PP

import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Lore as Lore
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes

-- | The class of lores whose annotations can be prettyprinted.
class (Lore lore,
       Pretty (RetType lore),
       Pretty (Pattern lore),
       Pretty (Lore.LetBound lore),
       Pretty (Lore.FParam lore)) => PrettyLore lore where
  ppBindingLore :: Binding lore -> Maybe Doc
  ppBindingLore = const Nothing
  ppFunDecLore :: FunDec lore -> Maybe Doc
  ppFunDecLore = const Nothing
  ppExpLore :: Exp lore -> Maybe Doc
  ppExpLore = const Nothing

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = encloseSep lparen rparen comma . map align

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Uniqueness where
  ppr Unique = star
  ppr Nonunique = empty

instance Pretty Value where
  ppr (BasicVal bv) = ppr bv
  ppr v@(ArrayVal a t)
    | Just s <- arrayString v = text $ show s
    | Array {} <- t = brackets $ commastack $ map ppr $ elems a
    | otherwise     = brackets $ commasep $ map ppr $ elems a

instance Pretty (TypeBase Shape) where
  ppr (Basic et) = ppr et
  ppr (Array et (Shape ds) u) = ppr u <> foldr f (ppr et) ds
    where f e s = brackets $ s <> comma <> ppr e
  ppr (Mem s) = text "mem" <> parens (ppr s)

instance Pretty (TypeBase ExtShape) where
  ppr (Basic et) = ppr et
  ppr (Array et (ExtShape ds) u) = ppr u <> foldr f (ppr et) ds
    where f (Free e) s = brackets $ s <> comma <> ppr e
          f (Ext x)  s = brackets $ s <> comma <>
                         text "?" <> text (show x)
  ppr (Mem s) = text "mem" <> parens (ppr s)

instance Pretty (TypeBase Rank) where
  ppr (Basic et) = ppr et
  ppr (Array et (Rank n) u) = u' <> foldl f (ppr et) [1..n]
    where f s _ = brackets s
          u' | Unique <- u = star
             | otherwise = empty
  ppr (Mem s) = text "mem" <> parens (ppr s)

instance Pretty (IdentBase shape) where
  ppr = text . textual . identName

instance Pretty SubExp where
  ppr (Var v)      = ppr v
  ppr (Constant v) = ppr v

instance Pretty Result where
  ppr (Result es) =
    braces (commasep $ map ppr es)

instance PrettyLore lore => Pretty (Body lore) where
  ppr (Body lore (bnd:bnds) res) =
    ppr bnd <+> text "in" </> ppr (Body lore bnds res)
  ppr (Body _ [] res) =
    ppr res

bindingAnnotation :: PrettyLore lore => Binding lore -> Doc -> Doc
bindingAnnotation bnd doc =
  case ppBindingLore bnd of
    Nothing    -> doc
    Just annot -> annot </> doc

instance Pretty (PatternT lore) where
  ppr = ppPattern . patternIdents

instance Pretty annot => Pretty (BindeeT annot) where
  ppr bindee = ppr (bindeeType bindee) <+>
               ppr (bindeeIdent bindee) <+>
               parens (ppr $ bindeeLore bindee)

instance PrettyLore lore => Pretty (Binding lore) where
  ppr bnd@(Let pat _ e) =
    bindingAnnotation bnd $ align $
    text "let" <+> align (ppPattern $ patternIdents pat) <+>
    case (linebreak, ppExpLore e) of
      (True, Nothing) -> equals </>
                         indent 2 e'
      (_, Just annot) -> equals </>
                         indent 2 (annot </>
                                   e')
      (False, Nothing) -> equals <+> align e'
    where e' = ppr e
          linebreak = case e of
                        LoopOp {} -> True
                        If {} -> True
                        PrimOp (ArrayLit {}) -> False
                        _ -> False

instance PrettyLore lore => Pretty (PrimOp lore) where
  ppr (SubExp se) = ppr se
  ppr (ArrayLit [] rt) =
    text "empty" <> parens (ppr rt)
  ppr (ArrayLit es rt) =
    case rt of
      Array {} -> brackets $ commastack $ map ppr es
      _        -> brackets $ commasep   $ map ppr es
  ppr (BinOp bop x y _) = ppBinOp bop x y
  ppr (Not e) = text "not" <+> pprPrec 9 e
  ppr (Negate e) = text "-" <> pprPrec 9 e
  ppr (Index cs v idxs) =
    ppCertificates cs <> ppr v <>
    brackets (commasep (map ppr idxs))
  ppr (Update cs src idxs ve) =
    ppCertificates cs <> ppr src <+>
    text "with" <+> brackets (commasep (map ppr idxs)) <+>
    text "<-" <+> align (ppr ve)
  ppr (Iota e) = text "iota" <> parens (ppr e)
  ppr (Replicate ne ve) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  ppr (Scratch t shape) =
    text "scratch" <> apply (ppr t : map ppr shape)
  ppr (Reshape cs shape e) =
    ppCertificates cs <> text "reshape" <> apply [apply (map ppr shape), ppr e]
  ppr (Rearrange cs perm e) =
    ppCertificates cs <> text "rearrange" <> apply [apply (map ppr perm), ppr e]
  ppr (Rotate cs n e) =
    ppCertificates cs <> text "rotate" <> apply [ppr n, ppr e]
  ppr (Split cs e a _) =
    ppCertificates cs <> text "split" <> apply [ppr e, ppr a]
  ppr (Concat cs x y _) =
    ppCertificates cs <> text "concat" <> apply [ppr x, ppr y]
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Assert e _) = text "assert" <> parens (ppr e)
  ppr (Conjoin es) = text "conjoin" <> parens (commasep $ map ppr es)
  ppr (Alloc e) = text "alloc" <> apply [ppr e]

instance PrettyLore lore => Pretty (LoopOp lore) where
  ppr (DoLoop res mergepat i bound loopbody) =
    text "loop" <+> ppPattern res <+>
    text "<-" <+> ppPattern (map bindeeIdent pat) <+> equals <+> ppTuple' initexp </>
    text "for" <+> ppr i <+> text "<" <+> align (ppr bound) <+> text "do" </>
    indent 2 (ppr loopbody)
    where (pat, initexp) = unzip mergepat
  ppr (Map cs lam as) =
    ppCertificates' cs <> ppSOAC "map" [lam] Nothing as
  ppr (Reduce cs lam inputs) =
    ppCertificates' cs <> ppSOAC "reduce" [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Redomap cs outer inner es as) =
    ppCertificates' cs <> text "redomap" <>
    parens (ppr outer <> comma </> ppr inner <> comma </>
            commasep (braces (commasep $ map ppr es) : map ppr as))
  ppr (Scan cs lam inputs) =
    ppCertificates' cs <> ppSOAC "scan" [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Filter cs lam as) =
    ppCertificates' cs <> ppSOAC "filter" [lam] Nothing as

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f _) = text "if" <+> ppr c </>
                     text "then" <+> align (ppr t) </>
                     text "else" <+> align (ppr f)
  ppr (PrimOp op) = ppr op
  ppr (LoopOp op) = ppr op
  ppr (Apply fname args _) = text (nameToString fname) <>
                             apply (map (align . ppr . fst) args)

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr (Lambda params body rettype) =
    text "fn" <+> ppTuple' rettype <+>
    apply (map ppParam params) <+>
    text "=>" </> indent 2 (ppr body)

instance Pretty ExtRetType where
  ppr = ppTuple' . retTypeValues

instance PrettyLore lore => Pretty (FunDec lore) where
  ppr fundec@(FunDec name rettype args body) =
    maybe id (</>) (ppFunDecLore fundec) $
    text "fun" <+> ppr rettype <+>
    text (nameToString name) <//>
    apply (map (ppParam . bindeeIdent) args) <+>
    equals </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (Prog lore) where
  ppr = stack . punctuate line . map ppr . progFunctions

ppParam :: Param -> Doc
ppParam param = ppr (identType param) <+> ppr param

ppBinOp :: BinOp -> SubExp -> SubExp -> Doc
ppBinOp bop x y = ppr x <+/> text (opStr bop) <+> ppr y

ppSOAC :: Pretty fn => String -> [fn] -> Maybe [SubExp] -> [Ident] -> Doc
ppSOAC name funs es as =
  text name <> parens (ppList funs </>
                       commasep (es' ++ map ppr as))
  where es' = maybe [] ((:[]) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
              []     -> empty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'

ppPattern :: [Ident] -> Doc
ppPattern = braces . commasep . map ppBind

ppBind :: Ident -> Doc
ppBind ident = ppr (identType ident) <+> ppr ident

ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map ppr ets

ppCertificates :: Certificates -> Doc
ppCertificates [] = empty
ppCertificates cs = text "<" <> commasep (map ppr cs) <> text ">"

ppCertificates' :: Certificates -> Doc
ppCertificates' [] = empty
ppCertificates' cs = ppCertificates cs <> line

-- | Prettyprint a list enclosed in curly braces.
prettyTuple :: Pretty a => [a] -> String
prettyTuple = PP.pretty 80 . ppTuple'

-- | Prettyprint a value, wrapped to 80 characters.
pretty :: Pretty a => a -> String
pretty = PP.pretty 80 . ppr
