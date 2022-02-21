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
    prettyTupleLines,
    pretty,
    PrettyRep (..),
    ppTuple',
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Futhark.IR.Syntax
import Futhark.Util.Pretty

-- | The class of representations whose annotations can be prettyprinted.
class
  ( RepTypes rep,
    Pretty (RetType rep),
    Pretty (BranchType rep),
    Pretty (FParamInfo rep),
    Pretty (LParamInfo rep),
    Pretty (LetDec rep),
    Pretty (Op rep)
  ) =>
  PrettyRep rep
  where
  ppExpDec :: ExpDec rep -> Exp rep -> Maybe Doc
  ppExpDec _ _ = Nothing

instance Pretty VName where
  ppr (VName vn i) = ppr vn <> text "_" <> text (show i)

instance Pretty Commutativity where
  ppr Commutative = text "commutative"
  ppr Noncommutative = text "noncommutative"

instance Pretty NoUniqueness where
  ppr _ = mempty

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
  ppr (Prim t) = ppr t
  ppr (Acc acc ispace ts u) =
    ppr u <> text "acc" <> apply [ppr acc, ppr ispace, ppTuple' ts]
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase ExtShape u) where
  ppr (Prim t) = ppr t
  ppr (Acc acc ispace ts u) =
    ppr u <> text "acc" <> apply [ppr acc, ppr ispace, ppTuple' ts]
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty u => Pretty (TypeBase Rank u) where
  ppr (Prim t) = ppr t
  ppr (Acc acc ispace ts u) =
    ppr u <> text "acc" <> apply [ppr acc, ppr ispace, ppTuple' ts]
  ppr (Array et (Rank n) u) =
    ppr u <> mconcat (replicate n $ brackets mempty) <> ppr et
  ppr (Mem s) = text "mem" <> ppr s

instance Pretty Ident where
  ppr ident = ppr (identType ident) <+> ppr (identName ident)

instance Pretty SubExp where
  ppr (Var v) = ppr v
  ppr (Constant v) = ppr v

instance Pretty Certs where
  ppr (Certs []) = empty
  ppr (Certs cs) = text "#" <> braces (commasep (map ppr cs))

instance PrettyRep rep => Pretty (Stms rep) where
  ppr = stack . map ppr . stmsToList

instance Pretty SubExpRes where
  ppr (SubExpRes cs se) = spread $ certAnnots cs ++ [ppr se]

instance PrettyRep rep => Pretty (Body rep) where
  ppr (Body _ stms res)
    | null stms = braces (commasep $ map ppr res)
    | otherwise =
      stack (map ppr $ stmsToList stms)
        </> text "in" <+> braces (commasep $ map ppr res)

instance Pretty Attr where
  ppr (AttrName v) = ppr v
  ppr (AttrInt x) = ppr x
  ppr (AttrComp f attrs) = ppr f <> parens (commasep $ map ppr attrs)

attrAnnots :: Attrs -> [Doc]
attrAnnots = map f . toList . unAttrs
  where
    f v = text "#[" <> ppr v <> text "]"

stmAttrAnnots :: Stm rep -> [Doc]
stmAttrAnnots = attrAnnots . stmAuxAttrs . stmAux

certAnnots :: Certs -> [Doc]
certAnnots cs
  | cs == mempty = []
  | otherwise = [ppr cs]

stmCertAnnots :: Stm rep -> [Doc]
stmCertAnnots = certAnnots . stmAuxCerts . stmAux

instance Pretty Attrs where
  ppr = spread . attrAnnots

instance Pretty t => Pretty (Pat t) where
  ppr (Pat xs) = braces $ commastack $ map ppr xs

instance Pretty t => Pretty (PatElem t) where
  ppr (PatElem name t) = ppr name <+> colon <+> align (ppr t)

instance Pretty t => Pretty (Param t) where
  ppr (Param attrs name t) =
    annot (attrAnnots attrs) $ ppr name <+> colon <+> align (ppr t)

instance PrettyRep rep => Pretty (Stm rep) where
  ppr stm@(Let pat aux e) =
    align . hang 2 $
      text "let" <+> align (ppr pat)
        <+> case (linebreak, stmannot) of
          (True, []) -> equals </> ppr e
          (False, []) -> equals <+/> ppr e
          (_, ann) -> equals </> (stack ann </> ppr e)
    where
      linebreak = case e of
        BasicOp BinOp {} -> False
        BasicOp CmpOp {} -> False
        BasicOp ConvOp {} -> False
        BasicOp UnOp {} -> False
        BasicOp SubExp {} -> False
        _ -> True

      stmannot =
        concat
          [ maybeToList (ppExpDec (stmAuxDec aux) e),
            stmAttrAnnots stm,
            stmCertAnnots stm
          ]

instance Pretty a => Pretty (Slice a) where
  ppr (Slice xs) = brackets (commasep (map ppr xs))

instance Pretty d => Pretty (FlatDimIndex d) where
  ppr (FlatDimIndex n s) = ppr n <+> text ":" <+> ppr s

instance Pretty a => Pretty (FlatSlice a) where
  ppr (FlatSlice offset xs) = brackets (ppr offset <> text ";" <+> commasep (map ppr xs))

instance Pretty BasicOp where
  ppr (SubExp se) = ppr se
  ppr (Opaque OpaqueNil e) = text "opaque" <> apply [ppr e]
  ppr (Opaque (OpaqueTrace s) e) = text "trace" <> apply [ppr (show s), ppr e]
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
  ppr (Index v slice) = ppr v <> ppr slice
  ppr (Update safety src slice se) =
    ppr src <+> with <+> ppr slice <+> text "=" <+> ppr se
    where
      with = case safety of
        Unsafe -> text "with"
        Safe -> text "with?"
  ppr (FlatIndex v slice) = ppr v <> ppr slice
  ppr (FlatUpdate src slice se) =
    ppr src <+> text "with" <+> ppr slice <+> text "=" <+> ppr se
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
  ppr (Concat i (x :| xs) w) =
    text "concat" <> text "@" <> ppr i <> apply (ppr w : ppr x : map ppr xs)
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Manifest perm e) = text "manifest" <> apply [apply (map ppr perm), ppr e]
  ppr (Assert e msg (loc, _)) =
    text "assert" <> apply [ppr e, ppr msg, text $ show $ locStr loc]
  ppr (UpdateAcc acc is v) =
    text "update_acc" <> apply [ppr acc, ppTuple' is, ppTuple' v]

instance Pretty a => Pretty (ErrorMsg a) where
  ppr (ErrorMsg parts) = braces $ align $ commasep $ map p parts
    where
      p (ErrorString s) = text $ show s
      p (ErrorVal t x) = ppr x <+> colon <+> ppr t

instance PrettyRep rep => Pretty (Exp rep) where
  ppr (If c t f (IfDec ret ifsort)) =
    text "if" <+> info' <+> ppr c
      </> text "then"
      <+> maybeNest t
      <+> text "else"
      <+> maybeNest f
      </> colon
      <+> ppTuple' ret
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
    applykw
      <+> text (nameToString fname)
      <> apply (map (align . pprArg) args)
      </> colon
      <+> braces (commasep $ map ppr ret)
    where
      pprArg (arg, Consume) = text "*" <> ppr arg
      pprArg (arg, _) = ppr arg
      applykw = case safety of
        Unsafe -> text "apply <unsafe>"
        Safe -> text "apply"
  ppr (Op op) = ppr op
  ppr (DoLoop merge form loopbody) =
    text "loop" <+> braces (commastack $ map ppr params)
      <+> equals
      <+> ppTuple' args
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
      (params, args) = unzip merge
      pprLoopVar (p, a) = ppr p <+> text "in" <+> ppr a
  ppr (WithAcc inputs lam) =
    text "with_acc"
      <> parens (braces (commastack $ map ppInput inputs) <> comma </> ppr lam)
    where
      ppInput (shape, arrs, op) =
        parens
          ( ppr shape <> comma <+> ppTuple' arrs
              <> case op of
                Nothing -> mempty
                Just (op', nes) ->
                  comma </> parens (ppr op' <> comma </> ppTuple' (map ppr nes))
          )

instance PrettyRep rep => Pretty (Lambda rep) where
  ppr (Lambda [] (Body _ stms []) []) | stms == mempty = text "nilFn"
  ppr (Lambda params body rettype) =
    text "\\" <+> ppTuple' params
      </> indent 2 (colon <+> ppTupleLines' rettype <+> text "->")
      </> indent 2 (ppr body)

instance Pretty EntryPointType where
  ppr (TypeDirect u) = ppr u <> "direct"
  ppr (TypeUnsigned u) = ppr u <> "unsigned"
  ppr (TypeOpaque u desc n) = ppr u <> "opaque" <> apply [ppr (show desc), ppr n]

instance Pretty EntryParam where
  ppr (EntryParam name t) = ppr name <> colon <+> ppr t

instance PrettyRep rep => Pretty (FunDef rep) where
  ppr (FunDef entry attrs name rettype fparams body) =
    annot (attrAnnots attrs) $
      fun
        </> indent 2 (text (nameToString name))
        <+> apply (map ppr fparams)
        </> indent 2 (colon <+> align (ppTuple' rettype))
        <+> equals
        <+> nestedBlock "{" "}" (ppr body)
    where
      fun = case entry of
        Nothing -> "fun"
        Just (p_name, p_entry, ret_entry) ->
          "entry"
            <> parens
              ( "\"" <> ppr p_name <> "\"" <> comma
                  </> ppTuple' p_entry <> comma
                  </> ppTuple' ret_entry
              )

instance PrettyRep rep => Pretty (Prog rep) where
  ppr (Prog consts funs) =
    stack $ punctuate line $ ppr consts : map ppr funs

instance Pretty d => Pretty (DimChange d) where
  ppr (DimCoercion se) = text "~" <> ppr se
  ppr (DimNew se) = ppr se

instance Pretty d => Pretty (DimIndex d) where
  ppr (DimFix i) = ppr i
  ppr (DimSlice i n s) = ppr i <+> text ":+" <+> ppr n <+> text "*" <+> ppr s

-- | Like 'prettyTuple', but produces a 'Doc'.
ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map (align . ppr) ets

-- | Like 'prettyTupleLines', but produces a 'Doc'.
ppTupleLines' :: Pretty a => [a] -> Doc
ppTupleLines' ets = braces $ stack $ punctuate comma $ map (align . ppr) ets
