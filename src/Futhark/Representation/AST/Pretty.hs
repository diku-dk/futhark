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

import Data.Array (elems, listArray)
import Data.Maybe

import Text.PrettyPrint.Mainland hiding (pretty)
import qualified Text.PrettyPrint.Mainland as PP

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes
import Futhark.Util

-- | The class of lores whose annotations can be prettyprinted.
class (Lore lore,
       Pretty (RetType lore),
       Pretty (Pattern lore),
       Pretty (Annotations.LetBound lore),
       Pretty (Annotations.FParam lore),
       Pretty (Annotations.LParam lore)) => PrettyLore lore where
  ppBindingLore :: Binding lore -> Maybe Doc
  ppBindingLore = const Nothing
  ppFunDecLore :: FunDec lore -> Maybe Doc
  ppFunDecLore = const Nothing
  ppLambdaLore :: Lambda lore -> Maybe Doc
  ppLambdaLore = const Nothing
  ppExpLore :: Exp lore -> Maybe Doc
  ppExpLore = const Nothing

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = parens . commasep . map align

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Uniqueness where
  ppr Unique = star
  ppr Nonunique = empty

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

instance Pretty (TypeBase Shape) where
  ppr (Basic et) = ppr et
  ppr (Array et (Shape ds) u) = ppr u <> foldr f (ppr et) ds
    where f e s = brackets $ s <> comma <> ppr e
  ppr (Mem s) = text "mem" <> parens (ppr s)

instance Pretty (TypeBase ExtShape) where
  ppr (Basic et) = ppr et
  ppr (Array et (ExtShape ds) u) = ppr u <> foldr f (ppr et) ds
    where f dim s = brackets $ s <> comma <> ppr dim
  ppr (Mem s) = text "mem" <> parens (ppr s)

instance Pretty (TypeBase Rank) where
  ppr (Basic et) = ppr et
  ppr (Array et (Rank n) u) = u' <> foldl f (ppr et) [1..n]
    where f s _ = brackets s
          u' | Unique <- u = star
             | otherwise = empty
  ppr (Mem s) = text "mem" <> parens (ppr s)

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

instance Pretty (PatternT lore) where
  ppr = braces . commasep . map ppElem . patternElements
    where ppElem (PatElem ident BindVar _) =
            ppr (identType ident) <+>
            ppr (identName ident)
          ppElem (PatElem ident (BindInPlace cs src is) _) =
            ppCertificates cs <>
            parens (ppr (identType ident) <+>
                    ppr (identName ident) <+>
                    text "<-" <+>
                    ppr src) <>
            brackets (commasep $ map ppr is)

instance Pretty attr => Pretty (PatElemT attr) where
  ppr (PatElem ident BindVar attr) =
    ppr (identType ident) <+>
    ppr (identName ident) <+>
    parens (ppr attr)

  ppr (PatElem ident (BindInPlace cs src is) attr) =
    ppCertificates cs <>
    parens (ppr (identType ident) <+>
            ppr (identName ident) <+>
            text "<-" <+>
            ppr src <>
            parens (ppr attr)) <>
    brackets (commasep $ map ppr is)

instance Pretty attr => Pretty (ParamT attr) where
  ppr (Param ident attr) =
    ppr (identType ident) <+>
    ppr (identName ident) <+>
    parens (ppr attr)

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
  ppr (BinOp bop x y _) = ppr x <+/> text (pretty bop) <+> ppr y
  ppr (Not e) = text "!" <+> pprPrec 9 e
  ppr (Negate e) = text "-" <> pprPrec 9 e
  ppr (Abs e) = text "abs" <> pprPrec 9 e
  ppr (Signum e) = text "signum" <> pprPrec 9 e
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
  ppr (Split cs sizeexps a) =
    ppCertificates cs <> text "split" <> apply [apply (map ppr sizeexps), ppr a]
  ppr (Concat cs x ys _) =
    ppCertificates cs <> text "concat" <> apply (ppr x : map ppr ys)
  ppr (Copy e) = text "copy" <> parens (ppr e)
  ppr (Assert e _) = text "assert" <> parens (ppr e)
  ppr (Alloc e) = text "alloc" <> apply [ppr e]
  ppr (Partition cs n flags arrs) =
    ppCertificates' cs <>
    text "partition" <>
    parens (commasep $ [ ppr n, ppr flags ] ++ map ppr arrs)

instance PrettyLore lore => Pretty (LoopOp lore) where
  ppr (DoLoop res mergepat form loopbody) =
    text "loop" <+> braces (commasep $ map ppr res) <+>
    text "<-" <+> ppPattern (map paramIdent pat) <+> equals <+> ppTuple' initexp </>
    (case form of
      ForLoop i bound ->
        text "for" <+> ppr i <+> text "<" <+> align (ppr bound)
      WhileLoop cond ->
        text "while" <+> ppr cond
    ) <+> text "do" </>
    indent 2 (ppr loopbody)
    where (pat, initexp) = unzip mergepat
  ppr (Map cs size lam as) =
    ppCertificates' cs <> ppSOAC "map" size [lam] Nothing as
  ppr (ConcatMap cs size lam as) =
    ppCertificates' cs <> text "concatMap" <>
    parens (ppr size <> comma </>
            pprConcatLam lam <> comma </>
            commasep (map (braces . commasep . map ppr) as))
    where pprConcatLam (Lambda index params body rettype) =
            text "fn" <+>
            braces (commasep $ map (brackets . ppr) rettype) <+>
            parens (ppr index <> semi <+> commasep (map ppr params)) <+>
            text "=>" </> indent 2 (ppr body)
  ppr (Reduce cs size lam inputs) =
    ppCertificates' cs <> ppSOAC "reduce" size [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Redomap cs size outer inner es as) =
    ppCertificates' cs <> text "redomap" <>
    parens (ppr size <> comma </> ppr outer <> comma </> ppr inner <> comma </>
            commasep (braces (commasep $ map ppr es) : map ppr as))
  ppr (Stream cs size form lam arrs ii) =
    let intent_str = if ii==MaxChunk then "Max" else ""
    in ppCertificates' cs <> case form of
          MapLike o ->
            let ord_str = if o == Disorder then "Per" else ""
            in  text ("streamMap"++ord_str++intent_str) <>
                parens (ppr size <> comma </> ppr lam <> comma </>
                        commasep (map ppr arrs) )
          RedLike o lam0 acc ->
            let ord_str = if o == Disorder then "Per" else ""
            in  text ("streamRed"++ord_str++intent_str) <>
                parens (ppr size <> comma </> ppr lam0 </> comma </> ppr lam </>
                        commasep ( braces (commasep $ map ppr acc) : map ppr arrs ))
          Sequential acc ->
                text "streamSeq" <>
                parens (ppr size <> comma </> ppr lam <> comma </>
                        commasep ( braces (commasep $ map ppr acc) : map ppr arrs ))
  ppr (Scan cs size lam inputs) =
    ppCertificates' cs <> ppSOAC "scan" size [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Kernel cs w index ispace inps returns body) =
    ppCertificates' cs <> text "kernel" <+>
    align (parens (text "width:" <+> ppr w) </>
           parens (text "index:" <+> ppr index) </>
           parens (stack $ punctuate semi $ map ppBound ispace) </>
           parens (stack $ punctuate semi $ map ppr inps) </>
           parens (stack $ punctuate semi $ map ppRet returns) </>
           text "do") </>
    indent 2 (ppr body)
    where ppBound (name, bound) =
            ppr name <+> text "<" <+> ppr bound
          ppRet (t, perm) =
            ppr t <+> text "permuted" <+> apply (map ppr perm)

instance PrettyLore lore => Pretty (KernelInput lore) where
  ppr inp = ppr (kernelInputType inp) <+>
            ppr (kernelInputName inp) <+>
            text "<-" <+>
            ppr (kernelInputArray inp) <>
            brackets (commasep (map ppr $ kernelInputIndices inp))

instance PrettyLore lore => Pretty (SegOp lore) where
  ppr (SegReduce cs size lam inputs descp) =
    ppCertificates' cs <> text "segreduce" <>
    parens (ppr size <> comma </> ppr lam <> comma </>
            ppTuple' nes <> comma <+>
            ppTuple' flatarrs <> comma <+>
            ppr descp)
    where
      (nes, flatarrs) = unzip inputs
  ppr (SegScan cs size st lam inputs descp) =
    ppCertificates' cs <> text "segscan" <> ppScanType st <>
    parens (ppr size <> comma </> ppr lam <> comma </>
            ppTuple' nes <> comma <+>
            ppTuple' flatarrs <> comma <+>
            ppr descp)
    where
      (nes, flatarrs) = unzip inputs
      ppScanType ScanInclusive = text "inc"
      ppScanType ScanExclusive = text "exc"
  ppr (SegReplicate cs counts dataarr seg) =
    ppCertificates' cs <> text "segreplicate" <>
    parens (commasep $ map ppr $ catMaybes [Just counts, Just dataarr, seg])

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f _) = text "if" <+> ppr c </>
                     text "then" <+> align (ppr t) </>
                     text "else" <+> align (ppr f)
  ppr (PrimOp op) = ppr op
  ppr (LoopOp op) = ppr op
  ppr (SegOp op) = ppr op
  ppr (Apply fname args _) = text (nameToString fname) <>
                             apply (map (align . ppr . fst) args)

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr lambda@(Lambda index params body rettype) =
    maybe id (</>) (ppLambdaLore lambda) $
    text "fn" <+> ppTuple' rettype <+>
    parens (ppr index <> semi <+>
            commasep (map (ppr . paramIdent) params)) <+>
    text "=>" </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (ExtLambda lore) where
  ppr (ExtLambda index params body rettype) =
    text "fn" <+> ppTuple' rettype <+>
    parens (ppr index <> semi <+>
            commasep (map (ppr . paramIdent) params)) <+>
    text "=>" </> indent 2 (ppr body)

instance Pretty ExtRetType where
  ppr = ppTuple' . retTypeValues

instance PrettyLore lore => Pretty (FunDec lore) where
  ppr fundec@(FunDec name rettype args body) =
    maybe id (</>) (ppFunDecLore fundec) $
    text "fun" <+> ppr rettype <+>
    text (nameToString name) <//>
    apply (map (ppr . paramIdent) args) <+>
    equals </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (Prog lore) where
  ppr = stack . punctuate line . map ppr . progFunctions

instance Pretty BinOp where
  ppr Plus = text "+"
  ppr Minus = text "-"
  ppr Pow = text "pow"
  ppr Times = text "*"
  ppr Divide = text "/"
  ppr IntDivide = text "div"
  ppr Mod = text "%"
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

ppSOAC :: Pretty fn => String -> SubExp -> [fn] -> Maybe [SubExp] -> [VName] -> Doc
ppSOAC name size funs es as =
  text name <> parens (ppr size <> comma </>
                       ppList funs </>
                       commasep (es' ++ map ppr as))
  where es' = maybe [] ((:[]) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
              []     -> empty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'

ppPattern :: [Ident] -> Doc
ppPattern = braces . commasep . map ppr

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
