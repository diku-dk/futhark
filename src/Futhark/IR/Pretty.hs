{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.IR.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.IR.Pretty
  ( prettyTuple,
    pretty,
    PrettyAnnot (..),
    PrettyLore (..),
    ppTuple',
  )
where

import Data.Foldable (toList)
import Data.Maybe
import Futhark.IR.Prop.Patterns
import Futhark.IR.Syntax
import Futhark.Util.Pretty

-- | Class for values that may have some prettyprinted annotation.
class PrettyAnnot a where
  ppAnnot :: a -> Maybe Doc

instance PrettyAnnot (PatElemT (TypeBase shape u)) where
  ppAnnot = const Nothing

instance PrettyAnnot (Param (TypeBase shape u)) where
  ppAnnot = const Nothing

instance PrettyAnnot () where
  ppAnnot = const Nothing

-- | The class of lores whose annotations can be prettyprinted.
class
  ( Decorations lore,
    Pretty (RetType lore),
    Pretty (BranchType lore),
    Pretty (Param (FParamInfo lore)),
    Pretty (Param (LParamInfo lore)),
    Pretty (PatElemT (LetDec lore)),
    PrettyAnnot (PatElem lore),
    PrettyAnnot (FParam lore),
    PrettyAnnot (LParam lore),
    Pretty (Op lore)
  ) =>
  PrettyLore lore
  where
  ppExpLore :: ExpDec lore -> Exp lore -> Maybe Doc
  ppExpLore _ _ = Nothing

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty VName where
  ppr (VName vn i) = ppr vn <> text "_" <> text (show i)

instance Pretty NoUniqueness where
  ppr _ = mempty

instance Pretty Commutativity where
  ppr Commutative = text "commutative"
  ppr Noncommutative = text "noncommutative"

instance Pretty Shape where
  ppr = mconcat . map (brackets . ppr) . shapeDims

instance Pretty a => Pretty (Ext a) where
  ppr (Free e) = ppr e
  ppr (Ext x) = text "?" <> text (show x)

instance Pretty ExtShape where
  ppr = mconcat . map (brackets . ppr) . shapeDims

instance Pretty Space where
  ppr DefaultSpace = mempty
  ppr (Space s) = text "@" <> text s
  ppr (ScalarSpace d t) = text "@" <> mconcat (map (brackets . ppr) d) <> ppr t

instance Pretty u => Pretty (TypeBase Shape u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase ExtShape u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase Rank u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Rank n) u) =
    ppr u <> mconcat (replicate n $ brackets mempty) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty Ident where
  ppr ident = ppr (identType ident) <+> ppr (identName ident)

instance Pretty SubExp where
  ppr (Var v) = ppr v
  ppr (Constant v) = ppr v

instance Pretty Certificates where
  ppr (Certificates []) = empty
  ppr (Certificates cs) = text "#" <> braces (commasep (map ppr cs))

instance PrettyLore lore => Pretty (Stms lore) where
  ppr = stack . map ppr . stmsToList

instance PrettyLore lore => Pretty (Body lore) where
  ppr (Body _ stms res)
    | null stms = braces (commasep $ map ppr res)
    | otherwise =
      stack (map ppr $ stmsToList stms)
        </> text "in" <+> braces (commasep $ map ppr res)

instance Pretty Attr where
  ppr (AttrAtom v) = ppr v
  ppr (AttrComp f attrs) = ppr f <> parens (commasep $ map ppr attrs)

attrAnnots :: Attrs -> [Doc]
attrAnnots = map f . toList . unAttrs
  where
    f v = text "#[" <> ppr v <> text "]"

stmAttrAnnots :: Stm lore -> [Doc]
stmAttrAnnots = attrAnnots . stmAuxAttrs . stmAux

certAnnots :: Certificates -> [Doc]
certAnnots cs
  | cs == mempty = []
  | otherwise = [ppr cs]

stmCertAnnots :: Stm lore -> [Doc]
stmCertAnnots = certAnnots . stmAuxCerts . stmAux

instance Pretty (PatElemT dec) => Pretty (PatternT dec) where
  ppr pat = ppPattern (patternContextElements pat) (patternValueElements pat)

instance Pretty (PatElemT b) => Pretty (PatElemT (a, b)) where
  ppr = ppr . fmap snd

instance Pretty (PatElemT Type) where
  ppr (PatElem name t) = ppr name <+> colon <+> ppr t

instance Pretty (Param b) => Pretty (Param (a, b)) where
  ppr = ppr . fmap snd

instance Pretty (Param DeclType) where
  ppr (Param name t) = ppr name <+> colon <+> ppr t

instance Pretty (Param Type) where
  ppr (Param name t) = ppr name <+> colon <+> ppr t

instance PrettyLore lore => Pretty (Stm lore) where
  ppr bnd@(Let pat aux e) =
    stmannot $
      align $
        hang 2 $
          text "let" <+> align (ppr pat)
            <+> case (linebreak, ppExpLore (stmAuxDec aux) e) of
              (True, Nothing) -> equals </> ppr e
              (_, Just ann) -> equals </> (ann </> ppr e)
              (False, Nothing) -> equals <+/> ppr e
    where
      linebreak = case e of
        DoLoop {} -> True
        Op {} -> True
        If {} -> True
        Apply {} -> True
        BasicOp ArrayLit {} -> False
        BasicOp Assert {} -> True
        _ -> False

      stmannot =
        case stmAttrAnnots bnd
          <> stmCertAnnots bnd
          <> mapMaybe ppAnnot (patternElements $ stmPattern bnd) of
          [] -> id
          annots -> (align (stack annots) </>)

instance Pretty BasicOp where
  ppr (SubExp se) = ppr se
  ppr (Opaque e) = text "opaque" <> apply [ppr e]
  ppr (ArrayLit es rt) =
    case rt of
      Array {} -> brackets $ commastack $ map ppr es
      _ -> brackets $ commasep $ map ppr es
      <+> colon
      <+> text "[]" <> ppr rt
  ppr (BinOp bop x y) = ppr bop <> parens (ppr x <> comma <+> ppr y)
  ppr (CmpOp op x y) = ppr op <> parens (ppr x <> comma <+> ppr y)
  ppr (ConvOp conv x) =
    text (convOpFun conv) <+> ppr fromtype <+> ppr x <+> text "to" <+> ppr totype
    where
      (fromtype, totype) = convOpType conv
  ppr (UnOp op e) = ppr op <+> pprPrec 9 e
  ppr (Index v idxs) =
    ppr v <> brackets (commasep (map ppr idxs))
  ppr (Update src idxs se) =
    ppr src <+> text "with" <+> brackets (commasep (map ppr idxs))
      <+> text "="
      <+> ppr se
  ppr (Iota e x s et) = text "iota" <> et' <> apply [ppr e, ppr x, ppr s]
    where
      et' = text $ show $ primBitSize $ IntType et
  ppr (Replicate ne ve) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  ppr (Scratch t shape) =
    text "scratch" <> apply (ppr t : map ppr shape)
  ppr (Reshape shape e) =
    text "reshape" <> apply [apply (map ppr shape), ppr e]
  ppr (Rearrange perm e) =
    text "rearrange" <> apply [apply (map ppr perm), ppr e]
  ppr (Rotate es e) =
    text "rotate" <> apply [apply (map ppr es), ppr e]
  ppr (Concat i x ys w) =
    text "concat" <> text "@" <> ppr i <> apply (ppr w : ppr x : map ppr ys)
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Manifest perm e) = text "manifest" <> apply [apply (map ppr perm), ppr e]
  ppr (Assert e msg (loc, _)) =
    text "assert" <> apply [ppr e, ppr msg, text $ show $ locStr loc]

instance Pretty a => Pretty (ErrorMsg a) where
  ppr (ErrorMsg parts) = braces $ align $ commasep $ map p parts
    where
      p (ErrorString s) = text $ show s
      p (ErrorInt32 x) = ppr x <+> colon <+> text "i32"
      p (ErrorInt64 x) = ppr x <+> colon <+> text "i64"

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f (IfDec ret ifsort)) =
    text "if" <+> info' <+> ppr c
      </> text "then"
      <+> maybeNest t
      <+> text "else"
      <+> maybeNest f
      <+> colon
      <+> braces (commasep $ map ppr ret)
    where
      info' = case ifsort of
        IfNormal -> mempty
        IfFallback -> text "<fallback>"
        IfEquiv -> text "<equiv>"
      maybeNest b
        | null $ bodyStms b = ppr b
        | otherwise = nestedBlock "{" "}" $ ppr b
  ppr (BasicOp op) = ppr op
  ppr (Apply fname args _ (safety, _, _)) =
    text "apply"
      <+> text (nameToString fname) <> safety'
        <> apply (map (align . pprArg) args)
    where
      pprArg (arg, Consume) = text "*" <> ppr arg
      pprArg (arg, _) = ppr arg
      safety' = case safety of
        Unsafe -> text "<unsafe>"
        Safe -> mempty
  ppr (Op op) = ppr op
  ppr (DoLoop ctx val form loopbody) =
    annot (mapMaybe ppAnnot (ctxparams ++ valparams)) $
      text "loop" <+> ppPattern ctxparams valparams
        <+> equals
        <+> ppTuple' (ctxinit ++ valinit)
        </> ( case form of
                ForLoop i it bound [] ->
                  text "for"
                    <+> align
                      ( ppr i <> text ":" <> ppr it
                          <+> text "<"
                          <+> align (ppr bound)
                      )
                ForLoop i it bound loop_vars ->
                  annot (mapMaybe (ppAnnot . fst) loop_vars) $
                    text "for"
                      <+> align
                        ( ppr i <> text ":" <> ppr it
                            <+> text "<"
                            <+> align (ppr bound)
                            </> stack (map pprLoopVar loop_vars)
                        )
                WhileLoop cond ->
                  text "while" <+> ppr cond
            )
        <+> text "do"
        <+> nestedBlock "{" "}" (ppr loopbody)
    where
      (ctxparams, ctxinit) = unzip ctx
      (valparams, valinit) = unzip val
      pprLoopVar (p, a) = ppr p <+> text "in" <+> ppr a

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr (Lambda [] _ []) = text "nilFn"
  ppr (Lambda params body rettype) =
    annot (mapMaybe ppAnnot params) $
      text "fn" <+> ppTuple' rettype
        <+/> align (parens (commasep (map ppr params)))
        <+> text "=>" </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (FunDef lore) where
  ppr (FunDef entry attrs name rettype fparams body) =
    annot (mapMaybe ppAnnot fparams <> attrAnnots attrs) $
      text fun <+> ppTuple' rettype
        <+/> text (nameToString name)
        <+> apply (map ppr fparams)
        <+> equals
        <+> nestedBlock "{" "}" (ppr body)
    where
      fun
        | isJust entry = "entry"
        | otherwise = "fun"

instance PrettyLore lore => Pretty (Prog lore) where
  ppr (Prog consts funs) =
    stack $ punctuate line $ ppr consts : map ppr funs

instance Pretty d => Pretty (DimChange d) where
  ppr (DimCoercion se) = text "~" <> ppr se
  ppr (DimNew se) = ppr se

instance Pretty d => Pretty (DimIndex d) where
  ppr (DimFix i) = ppr i
  ppr (DimSlice i n s) = ppr i <+> text ":+" <+> ppr n <+> text "*" <+> ppr s

ppPattern :: (Pretty a, Pretty b) => [a] -> [b] -> Doc
ppPattern [] bs = braces $ commasep $ map ppr bs
ppPattern as bs = braces $ commasep (map ppr as) <> semi </> commasep (map ppr bs)

-- | Like 'prettyTuple', but produces a 'Doc'.
ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map ppr ets
