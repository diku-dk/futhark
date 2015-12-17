{-# LANGUAGE UndecidableInstances #-}
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
  , ppCertificates
  , ppCertificates'
  , ppTuple'
  )
  where

import Data.Array (elems, listArray)
import Data.Monoid

import Futhark.Util.Pretty

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Util

-- | The class of lores whose annotations can be prettyprinted.
class (Lore lore,
       Pretty (RetType lore),
       Pretty (ParamT (Annotations.FParam lore)),
       Pretty (ParamT (Annotations.LParam lore)),
       Pretty (PatElemT (Annotations.LetBound lore)),
       Pretty (Annotations.Op lore)) => PrettyLore lore where
  ppBindingLore :: Binding lore -> Maybe Doc
  ppBindingLore = const Nothing
  ppFunDecLore :: FunDec lore -> Maybe Doc
  ppFunDecLore = const Nothing
  ppLambdaLore :: Lambda lore -> Maybe Doc
  ppLambdaLore = const Nothing
  ppExpLore :: Exp lore -> Maybe Doc
  ppExpLore = const Nothing

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Uniqueness where
  ppr Unique = star
  ppr Nonunique = empty

instance Pretty NoUniqueness where
  ppr _ = mempty

instance Pretty Value where
  ppr (BasicVal bv) = ppr bv
  ppr v
    | Just s <- arrayString v = text $ show s
  ppr (ArrayVal a t _)
    | null $ elems a = text "empty" <> parens (ppr t)
  ppr (ArrayVal a t (_:rowshape@(_:_))) =
    brackets $ commastack
    [ ppr $ ArrayVal (listArray (0, rowsize-1) a') t rowshape
      | a' <- chunk rowsize $ elems a ]
    where rowsize = product rowshape
  ppr (ArrayVal a _ _) =
    brackets $ commasep $ map ppr $ elems a

instance Pretty Shape where
  ppr = brackets . commasep . map ppr . shapeDims

instance Pretty ExtDimSize where
  ppr (Free e) = ppr e
  ppr (Ext x)  = text "?" <> text (show x)

instance Pretty ExtShape where
  ppr = brackets . commasep . map ppr . extShapeDims

instance Pretty Space where
  ppr DefaultSpace = mempty
  ppr (Space s)    = text "@" <> text s

instance Pretty u => Pretty (TypeBase Shape u) where
  ppr (Basic et) = ppr et
  ppr (Array et (Shape ds) u) = ppr u <> foldr f (ppr et) ds
    where f e s = brackets $ s <> comma <> ppr e
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty u => Pretty (TypeBase ExtShape u) where
  ppr (Basic et) = ppr et
  ppr (Array et (ExtShape ds) u) = ppr u <> foldr f (ppr et) ds
    where f dim s = brackets $ s <> comma <> ppr dim
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty u => Pretty (TypeBase Rank u) where
  ppr (Basic et) = ppr et
  ppr (Array et (Rank n) u) = ppr u <> foldl f (ppr et) [1..n]
    where f s _ = brackets s
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty Ident where
  ppr ident = ppr (identType ident) <+> ppr (identName ident)

instance Pretty SubExp where
  ppr (Var v)      = ppr v
  ppr (Constant v) = ppr v

instance PrettyLore lore => Pretty (Body lore) where
  ppr (Body lore (bnd:bnds) res) =
    ppr bnd <+> text "in" </> ppr (Body lore bnds res)
  ppr (Body _ [] res) =
    braces (commasep $ map ppr res)

bindingAnnotation :: PrettyLore lore => Binding lore -> Doc -> Doc
bindingAnnotation bnd doc =
  case ppBindingLore bnd of
    Nothing    -> doc
    Just annot -> annot </> doc

instance Pretty (PatElemT attr) => Pretty (PatternT attr) where
  ppr = braces . commasep . map ppr . patternElements

instance Pretty (PatElemT b) => Pretty (PatElemT (a,b)) where
  ppr = ppr . fmap snd

instance Pretty (PatElemT Type) where
  ppr (PatElem name BindVar t) =
    ppr t <+>
    ppr name

  ppr (PatElem name (BindInPlace cs src is) t) =
    ppCertificates cs <>
    parens (ppr t <+>
            ppr name <+>
            text "<-" <+>
            ppr src) <>
    brackets (commasep $ map ppr is)

instance Pretty (ParamT b) => Pretty (ParamT (a,b)) where
  ppr = ppr . fmap snd

instance Pretty (ParamT DeclType) where
  ppr (Param name t) =
    ppr t <+>
    ppr name

instance Pretty (ParamT Type) where
  ppr (Param name t) =
    ppr t <+>
    ppr name

instance PrettyLore lore => Pretty (Binding lore) where
  ppr bnd@(Let pat _ e) =
    bindingAnnotation bnd $ align $
    text "let" <+> align (ppr pat) <+>
    case (linebreak, ppExpLore e) of
      (True, Nothing) -> equals </>
                         indent 2 e'
      (_, Just annot) -> equals </>
                         indent 2 (annot </>
                                   e')
      (False, Nothing) -> equals <+> align e'
    where e' = ppr e
          linebreak = case e of
                        LoopOp{} -> True
                        Op{} -> True
                        If{} -> True
                        PrimOp ArrayLit{} -> False
                        _ -> False

instance PrettyLore lore => Pretty (PrimOp lore) where
  ppr (SubExp se) = ppr se
  ppr (ArrayLit [] rt) =
    text "empty" <> parens (ppr rt)
  ppr (ArrayLit es rt) =
    case rt of
      Array {} -> brackets $ commastack $ map ppr es
      _        -> brackets $ commasep   $ map ppr es
  ppr (BinOp bop x y _) = ppr x <+/> text (pretty bop) <+> ppr y
  ppr (Not e) = text "!" <+> pprPrec 9 e
  ppr (Negate e) = text "-" <> pprPrec 9 e
  ppr (Abs e) = text "abs" <+> pprPrec 9 e
  ppr (Signum e) = text "signum" <+> pprPrec 9 e
  ppr (Complement e) = text "~" <> pprPrec 9 e
  ppr (Index cs v idxs) =
    ppCertificates cs <> ppr v <>
    brackets (commasep (map ppr idxs))
  ppr (Iota e) = text "iota" <> parens (ppr e)
  ppr (Replicate ne ve) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  ppr (Scratch t shape) =
    text "scratch" <> apply (ppr t : map ppr shape)
  ppr (Reshape cs shape e) =
    ppCertificates cs <> text "reshape" <> apply [apply (map ppr shape), ppr e]
  ppr (Rearrange cs perm e) =
    ppCertificates cs <> text "rearrange" <> apply [apply (map ppr perm), ppr e]
  ppr (Stripe cs stride v) =
    ppCertificates cs <> text "stripe" <> apply [ppr stride, ppr v]
  ppr (Unstripe cs stride v) =
    ppCertificates cs <> text "unstripe" <> apply [ppr stride, ppr v]
  ppr (Split cs sizeexps a) =
    ppCertificates cs <> text "split" <> apply [apply (map ppr sizeexps), ppr a]
  ppr (Concat cs x ys _) =
    ppCertificates cs <> text "concat" <> apply (ppr x : map ppr ys)
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Assert e _) = text "assert" <> parens (ppr e)
  ppr (Partition cs n flags arrs) =
    ppCertificates' cs <>
    text "partition" <>
    parens (commasep $ [ ppr n, ppr flags ] ++ map ppr arrs)

instance PrettyLore lore => Pretty (LoopOp lore) where
  ppr (DoLoop res mergepat form loopbody) =
    text "loop" <+> braces (commasep $ map ppr res) <+>
    text "<-" <+> ppPattern pat <+> equals <+> ppTuple' initexp </>
    (case form of
      ForLoop i bound ->
        text "for" <+> ppr i <+> text "<" <+> align (ppr bound)
      WhileLoop cond ->
        text "while" <+> ppr cond
    ) <+> text "do" </>
    indent 2 (ppr loopbody)
    where (pat, initexp) = unzip mergepat

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f _) = text "if" <+> ppr c </>
                     text "then" <+> align (ppr t) </>
                     text "else" <+> align (ppr f)
  ppr (PrimOp op) = ppr op
  ppr (LoopOp op) = ppr op
  ppr (Apply fname args _) = text (nameToString fname) <>
                             apply (map (align . ppr . fst) args)
  ppr (Op op) = ppr op

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr lambda@(Lambda index params body rettype) =
    maybe id (</>) (ppLambdaLore lambda) $
    text "fn" <+> ppTuple' rettype <+>
    parens (ppr index <> semi <+>
            commasep (map ppr params)) <+>
    text "=>" </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (ExtLambda lore) where
  ppr (ExtLambda index params body rettype) =
    text "fn" <+> ppTuple' rettype <+>
    parens (ppr index <> semi <+>
            commasep (map ppr params)) <+>
    text "=>" </> indent 2 (ppr body)

instance Pretty ExtRetType where
  ppr = ppTuple' . retTypeValues

instance PrettyLore lore => Pretty (FunDec lore) where
  ppr fundec@(FunDec name rettype args body) =
    maybe id (</>) (ppFunDecLore fundec) $
    text "fun" <+> ppr rettype <+>
    text (nameToString name) <//>
    apply (map ppr args) <+>
    equals </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (Prog lore) where
  ppr = stack . punctuate line . map ppr . progFunctions

instance Pretty BinOp where
  ppr Plus = text "+"
  ppr Minus = text "-"
  ppr Pow = text "**"
  ppr Times = text "*"
  ppr FloatDiv = text "/"
  ppr Div = text "div"
  ppr Mod = text "%"
  ppr Quot = text "//"
  ppr Rem = text "%%"
  ppr ShiftR = text ">>"
  ppr ShiftL = text "<<"
  ppr Band = text "&"
  ppr Xor = text "^"
  ppr Bor = text "|"
  ppr LogAnd = text "&&"
  ppr LogOr = text "||"
  ppr Equal = text "=="
  ppr Less = text "<"
  ppr Leq = text "<="

instance Pretty d => Pretty (DimChange d) where
  ppr (DimCoercion se) = text "~" <> ppr se
  ppr (DimNew      se) = ppr se

ppPattern :: Pretty a => [a] -> Doc
ppPattern = braces . commasep . map ppr

ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map ppr ets

ppCertificates :: Certificates -> Doc
ppCertificates [] = empty
ppCertificates cs = text "<" <> commasep (map ppr cs) <> text ">"

ppCertificates' :: Certificates -> Doc
ppCertificates' [] = empty
ppCertificates' cs = ppCertificates cs <> line
