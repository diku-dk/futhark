{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.Representation.AST.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.Representation.AST.Pretty
  ( prettyTuple
  , pretty
  , PrettyAnnot (..)
  , PrettyLore (..)
  , ppTuple'
  , bindingAnnotation
  , ppArray
  )
  where

import           Data.Array                                     (elems,
                                                                 listArray)
import           Data.Maybe
import           Data.Monoid                                    ((<>))

import           Futhark.Util.Pretty

import           Futhark.Representation.AST.Attributes.Patterns
import           Futhark.Representation.AST.Syntax
import           Futhark.Util

-- | Class for values that may have some prettyprinted annotation.
class PrettyAnnot a where
  ppAnnot :: a -> Maybe Doc

instance PrettyAnnot (PatElemT (TypeBase shape u)) where
  ppAnnot = const Nothing

instance PrettyAnnot (ParamT (TypeBase shape u)) where
  ppAnnot = const Nothing

instance PrettyAnnot () where
  ppAnnot = const Nothing

-- | The class of lores whose annotations can be prettyprinted.
class (Annotations lore,
       Pretty (RetType lore),
       Pretty (BranchType lore),
       Pretty (ParamT (FParamAttr lore)),
       Pretty (ParamT (LParamAttr lore)),
       Pretty (PatElemT (LetAttr lore)),
       PrettyAnnot (PatElem lore),
       PrettyAnnot (FParam lore),
       PrettyAnnot (LParam lore),
       Pretty (Op lore)) => PrettyLore lore where
  ppExpLore :: ExpAttr lore -> Exp lore -> Maybe Doc
  ppExpLore _ (If _ _ _ (IfAttr ts _)) =
    Just $ stack $ map (text . ("-- "++)) $ lines $ pretty $
    text "Branch returns:" <+> ppTuple' ts
  ppExpLore _ _ = Nothing

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty NoUniqueness where
  ppr _ = mempty

instance Pretty Commutativity where
  ppr Commutative    = text "commutative"
  ppr Noncommutative = text "noncommutative"

-- | Print an array value, using the given function for printing
-- elements and element types.
ppArray :: (TypeBase Rank NoUniqueness -> Doc) -> (PrimValue -> Doc) -> Value -> Doc
ppArray _ pprim (PrimVal v) = pprim v
ppArray pt _ (ArrayVal _ t shape)
  | product shape == 0 = text "empty" <> parens (pt row_t)
  where row_t = Array t (Rank $ length shape - 1) NoUniqueness
ppArray pt pprim (ArrayVal a t (_:rowshape@(_:_))) =
  brackets $ commastack
  [ ppArray pt pprim $ ArrayVal (listArray (0, rowsize-1) a') t rowshape
  | a' <- chunk rowsize $ elems a ]
  where rowsize = product rowshape
ppArray _ pprim (ArrayVal a _ _) =
  brackets $ commasep $ map pprim $ elems a

instance Pretty Value where
  ppr = ppArray ppr ppr

instance Pretty Shape where
  ppr = brackets . commasep . map ppr . shapeDims

instance Pretty a => Pretty (Ext a) where
  ppr (Free e) = ppr e
  ppr (Ext x)  = text "?" <> text (show x)

instance Pretty ExtShape where
  ppr = brackets . commasep . map ppr . shapeDims

instance Pretty Space where
  ppr DefaultSpace = mempty
  ppr (Space s)    = text "@" <> text s

instance Pretty u => Pretty (TypeBase Shape u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty u => Pretty (TypeBase ExtShape u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Shape ds) u) =
    ppr u <> mconcat (map (brackets . ppr) ds) <> ppr et
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty u => Pretty (TypeBase Rank u) where
  ppr (Prim et) = ppr et
  ppr (Array et (Rank n) u) =
    ppr u <> mconcat (replicate n $ brackets mempty) <> ppr et
  ppr (Mem s DefaultSpace) = text "mem" <> parens (ppr s)
  ppr (Mem s (Space sp)) = text "mem" <> parens (ppr s) <> text "@" <> text sp

instance Pretty Ident where
  ppr ident = ppr (identType ident) <+> ppr (identName ident)

instance Pretty SubExp where
  ppr (Var v)      = ppr v
  ppr (Constant v) = ppr v

instance Pretty Certificates where
  ppr (Certificates []) = empty
  ppr (Certificates cs) = text "<" <> commasep (map ppr cs) <> text ">"

instance PrettyLore lore => Pretty (Stms lore) where
  ppr = stack . map ppr . stmsToList

instance PrettyLore lore => Pretty (Body lore) where
  ppr (Body _ stms res)
    | null stms = braces (commasep $ map ppr res)
    | otherwise = stack (map ppr $ stmsToList stms) </>
                  text "in" <+> braces (commasep $ map ppr res)

bindingAnnotation :: PrettyLore lore => Stm lore -> Doc -> Doc
bindingAnnotation bnd =
  case mapMaybe ppAnnot $ patternElements $ stmPattern bnd of
    []     -> id
    annots -> (stack annots </>)

instance Pretty (PatElemT attr) => Pretty (PatternT attr) where
  ppr pat = ppPattern (patternContextElements pat) (patternValueElements pat)

instance Pretty (PatElemT b) => Pretty (PatElemT (a,b)) where
  ppr = ppr . fmap snd

instance Pretty (PatElemT Type) where
  ppr (PatElem name t) = ppr t <+> ppr name

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

instance PrettyLore lore => Pretty (Stm lore) where
  ppr bnd@(Let pat (StmAux cs attr) e) =
    bindingAnnotation bnd $ align $
    text "let" <+> align (ppr pat) <+>
    case (linebreak, ppExpLore attr e) of
      (True, Nothing) -> equals </>
                         indent 2 e'
      (_, Just ann) -> equals </>
                       indent 2 (ann </> e')
      (False, Nothing) -> equals <+> align e'
    where e' = ppr cs <> ppr e
          linebreak = case e of
                        DoLoop{}           -> True
                        Op{}               -> True
                        If{}               -> True
                        BasicOp ArrayLit{} -> False
                        _                  -> False

instance Pretty (BasicOp lore) where
  ppr (SubExp se) = ppr se
  ppr (Opaque e) = text "opaque" <> apply [ppr e]
  ppr (ArrayLit [] rt) =
    text "empty" <> parens (ppr rt)
  ppr (ArrayLit es rt) =
    case rt of
      Array {} -> brackets $ commastack $ map ppr es
      _        -> brackets $ commasep   $ map ppr es
  ppr (BinOp bop x y) = ppr bop <> parens (ppr x <> comma <+> ppr y)
  ppr (CmpOp op x y) = ppr op <> parens (ppr x <> comma <+> ppr y)
  ppr (ConvOp conv x) =
    text (convOpFun conv) <+> ppr fromtype <+> ppr x <+> text "to" <+> ppr totype
    where (fromtype, totype) = convOpType conv
  ppr (UnOp op e) = ppr op <+> pprPrec 9 e
  ppr (Index v idxs) =
    ppr v <> brackets (commasep (map ppr idxs))
  ppr (Update src idxs se) =
    ppr src <+> text "with" <+> brackets (commasep (map ppr idxs)) <+>
    text "<-" <+> ppr se
  ppr (Iota e x s et) = text "iota" <> et' <> apply [ppr e, ppr x, ppr s]
    where et' = text $ show $ primBitSize $ IntType et
  ppr (Replicate ne ve) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  ppr (Repeat shapes innershape v) =
    text "repeat" <> apply [apply $ map ppr $ shapes ++ [innershape], ppr v]
  ppr (Scratch t shape) =
    text "scratch" <> apply (ppr t : map ppr shape)
  ppr (Reshape shape e) =
    text "reshape" <> apply [apply (map ppr shape), ppr e]
  ppr (Rearrange perm e) =
    text "rearrange" <> apply [apply (map ppr perm), ppr e]
  ppr (Rotate es e) =
    text "rotate" <> apply [apply (map ppr es), ppr e]
  ppr (Concat i x ys _) =
    text "concat" <> text "@" <> ppr i <> apply (ppr x : map ppr ys)
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Manifest perm e) = text "manifest" <> apply [apply (map ppr perm), ppr e]
  ppr (Assert e msg (loc, _)) =
    text "assert" <> apply [ppr e, text (show msg), text $ show $ locStr loc]
  ppr (Partition n flags arrs) =
    text "partition" <>
    parens (commasep $ [ ppr n, ppr flags ] ++ map ppr arrs)

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f (IfAttr _ ifsort)) =
    text "if" <+> info' <+> ppr c </>
    text "then" <+> maybeNest t <+>
    text "else" <+> maybeNest f
    where info' = case ifsort of IfNormal -> mempty
                                 IfFallback -> text "<fallback>"
          maybeNest b | null $ bodyStms b = ppr b
                      | otherwise         = nestedBlock "{" "}" $ ppr b
  ppr (BasicOp op) = ppr op
  ppr (Apply fname args _ (safety, _, _)) =
    text (nameToString fname) <> safety' <> apply (map (align . pprArg) args)
    where pprArg (arg, Consume) = text "*" <> ppr arg
          pprArg (arg, Observe) = ppr arg
          safety' = case safety of Unsafe -> text "<unsafe>"
                                   Safe   -> mempty
  ppr (Op op) = ppr op
  ppr (DoLoop ctx val form loopbody) =
    annot (mapMaybe ppAnnot (ctxparams++valparams)) $
    text "loop" <+> ppPattern ctxparams valparams <+>
    equals <+> ppTuple' (ctxinit++valinit) </>
    (case form of
      ForLoop i it bound [] ->
        text "for" <+> align (ppr i <> text ":" <> ppr it <+>
                              text "<" <+> align (ppr bound))
      ForLoop i it bound loop_vars ->
        annot (mapMaybe (ppAnnot . fst) loop_vars) $
        text "for" <+> align (ppr i <> text ":" <> ppr it <+>
                              text "<" <+> align (ppr bound) </>
                             stack (map pprLoopVar loop_vars))
      WhileLoop cond ->
        text "while" <+> ppr cond
    ) <+> text "do" <+> nestedBlock "{" "}" (ppr loopbody)
    where (ctxparams, ctxinit) = unzip ctx
          (valparams, valinit) = unzip val
          pprLoopVar (p,a) = ppr p <+> text "in" <+> ppr a

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr (Lambda [] _ []) = text "nilFn"
  ppr (Lambda params body rettype) =
    annot (mapMaybe ppAnnot params) $
    text "fn" <+> ppTuple' rettype <+>
    parens (commasep (map ppr params)) <+>
    text "=>" </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (FunDef lore) where
  ppr (FunDef entry name rettype fparams body) =
    annot (mapMaybe ppAnnot fparams) $
    text fun <+> ppTuple' rettype <+>
    text (nameToString name) <//>
    apply (map ppr fparams) <+>
    equals <+> nestedBlock "{" "}" (ppr body)
    where fun | isJust entry = "entry"
              | otherwise    = "fun"

instance PrettyLore lore => Pretty (Prog lore) where
  ppr = stack . punctuate line . map ppr . progFunctions

instance Pretty d => Pretty (DimChange d) where
  ppr (DimCoercion se) = text "~" <> ppr se
  ppr (DimNew      se) = ppr se

instance Pretty d => Pretty (DimIndex d) where
  ppr (DimFix i)       = ppr i
  ppr (DimSlice i n s) = ppr i <> text ":+" <> ppr n <> text "*" <> ppr s

ppPattern :: (Pretty a, Pretty b) => [a] -> [b] -> Doc
ppPattern [] bs = braces $ commasep $ map ppr bs
ppPattern as bs = braces $ commasep (map ppr as) <> semi <+> commasep (map ppr bs)

ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map ppr ets
