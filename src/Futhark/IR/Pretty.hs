{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.IR.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.IR.Pretty
  ( prettyTuple,
    pretty,
    PrettyLore (..),
    ppTuple',
  )
where

import Data.Foldable (toList)
import Data.Maybe
import Futhark.IR.Syntax
import Futhark.Util.Pretty

-- | The class of lores whose annotations can be prettyprinted.
class
  ( Decorations lore,
    Pretty (RetType lore),
    Pretty (BranchType lore),
    Pretty (FParamInfo lore),
    Pretty (LParamInfo lore),
    Pretty (LetDec lore),
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

instance Pretty ElemType where
  ppr (ElemPrim et) = ppr et
  ppr (ElemAcc arrs) = text "acc" <> ppTuple' arrs

instance Pretty u => Pretty (TypeBase Shape u) where
  ppr (Prim t) = ppr $ ElemPrim t
  ppr (Acc ts) = ppr $ ElemAcc ts
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase ExtShape u) where
  ppr (Prim t) = ppr $ ElemPrim t
  ppr (Acc ts) = ppr $ ElemAcc ts
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase Rank u) where
  ppr (Prim t) = ppr $ ElemPrim t
  ppr (Acc ts) = ppr $ ElemAcc ts
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

instance Pretty t => Pretty (PatElemT t) where
  ppr (PatElem name t) = ppr name <+> colon <+> align (ppr t)

instance Pretty t => Pretty (Param t) where
  ppr (Param name t) = ppr name <+> colon <+> align (ppr t)

instance PrettyLore lore => Pretty (Stm lore) where
  ppr bnd@(Let pat aux e) =
    align . hang 2 $
      text "let" <+> align (ppr pat)
        <+> case (linebreak, stmannot) of
          (True, []) -> equals </> ppr e
          (False, []) -> equals <+/> ppr e
          (_, ann) -> equals </> (stack ann </> ppr e)
    where
      linebreak = case e of
        DoLoop {} -> True
        Op {} -> True
        If {} -> True
        WithAcc {} -> True
        Apply {} -> True
        BasicOp ArrayLit {} -> False
        BasicOp Assert {} -> True
        _ -> False

      stmannot =
        concat
          [ maybeToList (ppExpLore (stmAuxDec aux) e),
            stmAttrAnnots bnd,
            stmCertAnnots bnd
          ]

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
  ppr (UpdateAcc acc is v) =
    text "update_acc" <> apply [ppr acc, ppTuple' is, ppTuple' v]
  ppr (JoinAcc acc arrs) =
    text "join_acc" <> apply [ppr acc, ppTuple' arrs]

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
  ppr (Apply fname args ret (safety, _, _)) =
    text "apply"
      <+> text (nameToString fname)
      <> safety'
      <> apply (map (align . pprArg) args)
      </> colon
      <+> braces (commasep $ map ppr ret)
    where
      pprArg (arg, Consume) = text "*" <> ppr arg
      pprArg (arg, _) = ppr arg
      safety' = case safety of
        Unsafe -> text "<unsafe>"
        Safe -> mempty
  ppr (Op op) = ppr op
  ppr (DoLoop ctx val form loopbody) =
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
  ppr (WithAcc shape arrs lam op) =
    text "with_acc"
      <> parens
        ( ppr shape <> comma <+> ppTuple' arrs <> comma </> ppr lam
            <> case op of
              Nothing -> mempty
              Just (op', nes) -> comma </> ppTuple' [ppr op', ppTuple' $ map ppr nes]
        )

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr (Lambda [] _ []) = text "nilFn"
  ppr (Lambda params body rettype) =
    text "\\" <> ppTuple' params
      <+/> colon <+> ppTuple' rettype <+> text "->"
      </> indent 2 (ppr body)

instance Pretty EntryPointType where
  ppr TypeDirect = "direct"
  ppr TypeUnsigned = "unsigned"
  ppr (TypeOpaque desc n) = "opaque" <> apply [ppr (show desc), ppr n]

instance PrettyLore lore => Pretty (FunDef lore) where
  ppr (FunDef entry attrs name rettype fparams body) =
    annot (attrAnnots attrs) $
      fun <+> text (nameToString name)
        <+> apply (map ppr fparams)
        </> indent 2 (colon <+> align (ppTuple' rettype))
        <+> equals
        <+> nestedBlock "{" "}" (ppr body)
    where
      fun = case entry of
        Nothing -> "fun"
        Just (p_entry, ret_entry) ->
          "entry"
            <> nestedBlock "(" ")" (ppTuple' p_entry <> comma </> ppTuple' ret_entry)

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
