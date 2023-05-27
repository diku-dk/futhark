{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Language.Futhark.Syntax".
module Language.Futhark.Pretty
  ( prettyString,
    prettyTuple,
    leadingOperator,
    IsName (..),
    Annot (..),
  )
where

import Control.Monad
import Data.Char (chr)
import Data.Functor
import Data.List (intersperse)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Monoid hiding (Sum)
import Data.Ord
import Data.Word
import Futhark.Util
import Futhark.Util.Pretty
import Language.Futhark.Prop
import Language.Futhark.Syntax
import Prelude

-- | A class for types that are variable names in the Futhark source
-- language.  This is used instead of a mere 'Pretty' instance because
-- in the compiler frontend we want to print VNames differently
-- depending on whether the FUTHARK_COMPILER_DEBUGGING environment
-- variable is set, yet in the backend we want to always print VNames
-- with the tag.  To avoid erroneously using the 'Pretty' instance for
-- VNames, we in fact only define it inside the modules for the core
-- language (as an orphan instance).
class IsName v where
  prettyName :: v -> Doc a
  toName :: v -> Name

-- | Depending on the environment variable FUTHARK_COMPILER_DEBUGGING,
-- VNames are printed as either the name with an internal tag, or just
-- the base name.
instance IsName VName where
  prettyName
    | isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1 =
        \(VName vn i) -> pretty vn <> "_" <> pretty (show i)
    | otherwise = pretty . baseName
  toName = baseName

instance IsName Name where
  prettyName = pretty
  toName = id

-- | Class for type constructors that represent annotations.  Used in
-- the prettyprinter to either print the original AST, or the computed
-- decoration.
class Annot f where
  -- | Extract value, if any.
  unAnnot :: f a -> Maybe a

instance Annot NoInfo where
  unAnnot = const Nothing

instance Annot Info where
  unAnnot = Just . unInfo

instance Pretty PrimValue where
  pretty (UnsignedValue (Int8Value v)) =
    pretty (show (fromIntegral v :: Word8)) <> "u8"
  pretty (UnsignedValue (Int16Value v)) =
    pretty (show (fromIntegral v :: Word16)) <> "u16"
  pretty (UnsignedValue (Int32Value v)) =
    pretty (show (fromIntegral v :: Word32)) <> "u32"
  pretty (UnsignedValue (Int64Value v)) =
    pretty (show (fromIntegral v :: Word64)) <> "u64"
  pretty (SignedValue v) = pretty v
  pretty (BoolValue True) = "true"
  pretty (BoolValue False) = "false"
  pretty (FloatValue v) = pretty v

instance Pretty Size where
  pretty (AnySize Nothing) = mempty
  pretty (AnySize (Just v)) = "?" <> prettyName v
  pretty (NamedSize v) = pretty v
  pretty (ConstSize n) = pretty n

instance (Eq vn, IsName vn, Annot f) => Pretty (SizeExp f vn) where
  pretty SizeExpAny {} = brackets mempty
  pretty (SizeExp e _) = brackets $ pretty e

instance Pretty (Shape Size) where
  pretty (Shape ds) = mconcat (map (brackets . pretty) ds)

instance Pretty (Shape ()) where
  pretty (Shape ds) = mconcat $ replicate (length ds) "[]"

instance Pretty (Shape Int64) where
  pretty (Shape ds) = mconcat (map (brackets . pretty) ds)

instance Pretty (Shape Bool) where
  pretty (Shape ds) = mconcat (map (brackets . pretty) ds)

prettyRetType :: Pretty (Shape dim) => Int -> RetTypeBase dim as -> Doc a
prettyRetType p (RetType [] t) =
  prettyType p t
prettyRetType _ (RetType dims t) =
  "?"
    <> mconcat (map (brackets . prettyName) dims)
    <> "."
    <> pretty t

instance Pretty (Shape dim) => Pretty (RetTypeBase dim as) where
  pretty = prettyRetType 0

instance Pretty Diet where
  pretty Consume = "*"
  pretty Observe = ""

prettyScalarType :: Pretty (Shape dim) => Int -> ScalarTypeBase dim as -> Doc a
prettyScalarType _ (Prim et) = pretty et
prettyScalarType p (TypeVar _ u v targs) =
  parensIf (not (null targs) && p > 3) $
    pretty u <> hsep (pretty v : map (prettyTypeArg 3) targs)
prettyScalarType _ (Record fs)
  | Just ts <- areTupleFields fs =
      group $ parens $ align $ mconcat $ punctuate ("," <> line) $ map pretty ts
  | otherwise =
      group $ braces $ align $ mconcat $ punctuate ("," <> line) fs'
  where
    ppField (name, t) = pretty (nameToString name) <> colon <+> align (pretty t)
    fs' = map ppField $ M.toList fs
prettyScalarType p (Arrow _ (Named v) d t1 t2) =
  parensIf (p > 1) $
    parens (prettyName v <> colon <+> pretty d <> align (pretty t1))
      <+> "->"
      <+> prettyRetType 1 t2
prettyScalarType p (Arrow _ Unnamed d t1 t2) =
  parensIf (p > 1) $
    (pretty d <> prettyType 2 t1)
      <+> "->"
      <+> prettyRetType 1 t2
prettyScalarType p (Sum cs) =
  parensIf (p > 0) $
    group (align (mconcat $ punctuate (" |" <> line) cs'))
  where
    ppConstr (name, fs) = sep $ ("#" <> pretty name) : map (prettyType 2) fs
    cs' = map ppConstr $ M.toList cs

instance Pretty (Shape dim) => Pretty (ScalarTypeBase dim as) where
  pretty = prettyScalarType 0

prettyType :: Pretty (Shape dim) => Int -> TypeBase dim as -> Doc a
prettyType _ (Array _ u shape at) =
  pretty u <> pretty shape <> align (prettyScalarType 1 at)
prettyType p (Scalar t) =
  prettyScalarType p t

instance Pretty (Shape dim) => Pretty (TypeBase dim as) where
  pretty = prettyType 0

prettyTypeArg :: Pretty (Shape dim) => Int -> TypeArg dim -> Doc a
prettyTypeArg _ (TypeArgDim d _) = pretty $ Shape [d]
prettyTypeArg p (TypeArgType t _) = prettyType p t

instance Pretty (TypeArg Size) where
  pretty = prettyTypeArg 0

instance (Eq vn, IsName vn, Annot f) => Pretty (TypeExp f vn) where
  pretty (TEUnique t _) = "*" <> pretty t
  pretty (TEArray d at _) = pretty d <> pretty at
  pretty (TETuple ts _) = parens $ commasep $ map pretty ts
  pretty (TERecord fs _) = braces $ commasep $ map ppField fs
    where
      ppField (name, t) = pretty (nameToString name) <> colon <+> pretty t
  pretty (TEVar name _) = pretty name
  pretty (TEApply t arg _) = pretty t <+> pretty arg
  pretty (TEArrow (Just v) t1 t2 _) = parens v' <+> "->" <+> pretty t2
    where
      v' = prettyName v <> colon <+> pretty t1
  pretty (TEArrow Nothing t1 t2 _) = pretty t1 <+> "->" <+> pretty t2
  pretty (TESum cs _) =
    align $ cat $ punctuate (" |" <> softline) $ map ppConstr cs
    where
      ppConstr (name, fs) = "#" <> pretty name <+> sep (map pretty fs)
  pretty (TEDim dims te _) =
    "?" <> mconcat (map (brackets . prettyName) dims) <> "." <> pretty te

instance (Eq vn, IsName vn, Annot f) => Pretty (TypeArgExp f vn) where
  pretty (TypeArgExpSize d) = pretty d
  pretty (TypeArgExpType t) = pretty t

instance IsName vn => Pretty (QualName vn) where
  pretty (QualName names name) =
    mconcat $ punctuate "." $ map prettyName names ++ [prettyName name]

instance IsName vn => Pretty (IdentBase f vn) where
  pretty = prettyName . identName

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit ArrayLit {} = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit _ = False

instance (Eq vn, IsName vn, Annot f) => Pretty (DimIndexBase f vn) where
  pretty (DimFix e) = pretty e
  pretty (DimSlice i j (Just s)) =
    maybe mempty pretty i
      <> ":"
      <> maybe mempty pretty j
      <> ":"
      <> pretty s
  pretty (DimSlice i (Just j) s) =
    maybe mempty pretty i
      <> ":"
      <> pretty j
      <> maybe mempty ((":" <>) . pretty) s
  pretty (DimSlice i Nothing Nothing) =
    maybe mempty pretty i <> ":"

instance IsName vn => Pretty (SizeBinder vn) where
  pretty (SizeBinder v _) = brackets $ prettyName v

letBody :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> Doc a
letBody body@(AppExp LetPat {} _) = pretty body
letBody body@(AppExp LetFun {} _) = pretty body
letBody body = "in" <+> align (pretty body)

prettyAppExp :: (Eq vn, IsName vn, Annot f) => Int -> AppExpBase f vn -> Doc a
prettyAppExp p (Coerce e t _) =
  parensIf (p /= -1) $ prettyExp 0 e <+> ":>" <+> align (pretty t)
prettyAppExp p (BinOp (bop, _) _ (x, _) (y, _) _) = prettyBinOp p bop x y
prettyAppExp _ (Match e cs _) = "match" <+> pretty e </> (stack . map pretty) (NE.toList cs)
prettyAppExp _ (DoLoop sizeparams pat initexp form loopbody _) =
  "loop"
    <+> align
      ( hsep (map (brackets . prettyName) sizeparams ++ [pretty pat])
          <+> equals
          <+> pretty initexp
          <+> pretty form
          <+> "do"
      )
    </> indent 2 (pretty loopbody)
prettyAppExp _ (Index e idxs _) =
  prettyExp 9 e <> brackets (commasep (map pretty idxs))
prettyAppExp p (LetPat sizes pat e body _) =
  parensIf (p /= -1) . align $
    hsep ("let" : map pretty sizes ++ [align (pretty pat)])
      <+> ( if linebreak
              then equals </> indent 2 (pretty e)
              else equals <+> align (pretty e)
          )
      </> letBody body
  where
    linebreak = case e of
      AppExp {} -> True
      Attr {} -> True
      ArrayLit {} -> False
      _ -> hasArrayLit e
prettyAppExp _ (LetFun fname (tparams, params, retdecl, rettype, e) body _) =
  "let"
    <+> hsep (prettyName fname : map pretty tparams ++ map pretty params)
      <> retdecl'
    <+> equals
    </> indent 2 (pretty e)
    </> letBody body
  where
    retdecl' = case (pretty <$> unAnnot rettype) `mplus` (pretty <$> retdecl) of
      Just rettype' -> colon <+> align rettype'
      Nothing -> mempty
prettyAppExp _ (LetWith dest src idxs ve body _)
  | dest == src =
      "let"
        <+> pretty dest <> list (map pretty idxs)
        <+> equals
        <+> align (pretty ve)
        </> letBody body
  | otherwise =
      "let"
        <+> pretty dest
        <+> equals
        <+> pretty src
        <+> "with"
        <+> brackets (commasep (map pretty idxs))
        <+> "="
        <+> align (pretty ve)
        </> letBody body
prettyAppExp p (Range start maybe_step end _) =
  parensIf (p /= -1) $
    pretty start
      <> maybe mempty ((".." <>) . pretty) maybe_step
      <> case end of
        DownToExclusive end' -> "..>" <> pretty end'
        ToInclusive end' -> "..." <> pretty end'
        UpToExclusive end' -> "..<" <> pretty end'
prettyAppExp _ (If c t f _) =
  "if"
    <+> pretty c
    </> "then"
    <+> align (pretty t)
    </> "else"
    <+> align (pretty f)
prettyAppExp p (Apply f args _) =
  parensIf (p >= 10) $
    prettyExp 0 f
      <+> hsep (map (prettyExp 10 . snd) $ NE.toList args)

instance (Eq vn, IsName vn, Annot f) => Pretty (AppExpBase f vn) where
  pretty = prettyAppExp (-1)

prettyInst :: Annot f => f PatType -> Doc a
prettyInst t =
  case unAnnot t of
    Just t'
      | isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 2 ->
          "@" <> parens (align $ pretty t')
    _ -> mempty

prettyAttr :: Pretty a => a -> Doc ann
prettyAttr attr = "#[" <> pretty attr <> "]"

prettyExp :: (Eq vn, IsName vn, Annot f) => Int -> ExpBase f vn -> Doc a
prettyExp _ (Var name t _) = pretty name <> prettyInst t
prettyExp _ (Hole t _) = "???" <> prettyInst t
prettyExp _ (Parens e _) = align $ parens $ pretty e
prettyExp _ (QualParens (v, _) e _) = pretty v <> "." <> align (parens $ pretty e)
prettyExp p (Ascript e t _) =
  parensIf (p /= -1) $ prettyExp 0 e <+> ":" <+> align (pretty t)
prettyExp _ (Literal v _) = pretty v
prettyExp _ (IntLit v t _) = pretty v <> prettyInst t
prettyExp _ (FloatLit v t _) = pretty v <> prettyInst t
prettyExp _ (TupLit es _)
  | any hasArrayLit es = parens $ commastack $ map pretty es
  | otherwise = parens $ commasep $ map pretty es
prettyExp _ (RecordLit fs _)
  | any fieldArray fs = braces $ commastack $ map pretty fs
  | otherwise = braces $ commasep $ map pretty fs
  where
    fieldArray (RecordFieldExplicit _ e _) = hasArrayLit e
    fieldArray RecordFieldImplicit {} = False
prettyExp _ (ArrayLit es t _) =
  brackets (commasep $ map pretty es) <> prettyInst t
prettyExp _ (StringLit s _) =
  pretty $ show $ map (chr . fromIntegral) s
prettyExp _ (Project k e _ _) = pretty e <> "." <> pretty k
prettyExp _ (Negate e _) = "-" <> pretty e
prettyExp _ (Not e _) = "-" <> pretty e
prettyExp _ (Update src idxs ve _) =
  pretty src
    <+> "with"
    <+> brackets (commasep (map pretty idxs))
    <+> "="
    <+> align (pretty ve)
prettyExp _ (RecordUpdate src fs ve _ _) =
  pretty src
    <+> "with"
    <+> mconcat (intersperse "." (map pretty fs))
    <+> "="
    <+> align (pretty ve)
prettyExp _ (Assert e1 e2 _ _) = "assert" <+> prettyExp 10 e1 <+> prettyExp 10 e2
prettyExp p (Lambda params body rettype _ _) =
  parensIf (p /= -1) $
    "\\" <> hsep (map pretty params) <> ppAscription rettype
      <+> "->"
      </> indent 2 (align (pretty body))
prettyExp _ (OpSection binop _ _) =
  parens $ pretty binop
prettyExp _ (OpSectionLeft binop _ x _ _ _) =
  parens $ pretty x <+> ppBinOp binop
prettyExp _ (OpSectionRight binop _ x _ _ _) =
  parens $ ppBinOp binop <+> pretty x
prettyExp _ (ProjectSection fields _ _) =
  parens $ mconcat $ map p fields
  where
    p name = "." <> pretty name
prettyExp _ (IndexSection idxs _ _) =
  parens $ "." <> brackets (commasep (map pretty idxs))
prettyExp _ (Constr n cs t _) =
  "#" <> pretty n <+> sep (map pretty cs) <> prettyInst t
prettyExp _ (Attr attr e _) =
  prettyAttr attr </> prettyExp (-1) e
prettyExp i (AppExp e _) = prettyAppExp i e

instance (Eq vn, IsName vn, Annot f) => Pretty (ExpBase f vn) where
  pretty = prettyExp (-1)

instance IsName vn => Pretty (AttrAtom vn) where
  pretty (AtomName v) = pretty v
  pretty (AtomInt x) = pretty x

instance IsName vn => Pretty (AttrInfo vn) where
  pretty (AttrAtom attr _) = pretty attr
  pretty (AttrComp f attrs _) = pretty f <> parens (commasep $ map pretty attrs)

instance (Eq vn, IsName vn, Annot f) => Pretty (FieldBase f vn) where
  pretty (RecordFieldExplicit name e _) = pretty name <> equals <> pretty e
  pretty (RecordFieldImplicit name _ _) = prettyName name

instance (Eq vn, IsName vn, Annot f) => Pretty (CaseBase f vn) where
  pretty (CasePat p e _) = "case" <+> pretty p <+> "->" </> indent 2 (pretty e)

instance (Eq vn, IsName vn, Annot f) => Pretty (LoopFormBase f vn) where
  pretty (For i ubound) =
    "for" <+> pretty i <+> "<" <+> align (pretty ubound)
  pretty (ForIn x e) =
    "for" <+> pretty x <+> "in" <+> pretty e
  pretty (While cond) =
    "while" <+> pretty cond

instance Pretty PatLit where
  pretty (PatLitInt x) = pretty x
  pretty (PatLitFloat f) = pretty f
  pretty (PatLitPrim v) = pretty v

instance (Eq vn, IsName vn, Annot f) => Pretty (PatBase f vn) where
  pretty (PatAscription p t _) = pretty p <> colon <+> align (pretty t)
  pretty (PatParens p _) = parens $ pretty p
  pretty (Id v t _) = case unAnnot t of
    Just t' -> parens $ prettyName v <> colon <+> align (pretty t')
    Nothing -> prettyName v
  pretty (TuplePat pats _) = parens $ commasep $ map pretty pats
  pretty (RecordPat fs _) = braces $ commasep $ map ppField fs
    where
      ppField (name, t) = pretty (nameToString name) <> equals <> pretty t
  pretty (Wildcard t _) = case unAnnot t of
    Just t' -> parens $ "_" <> colon <+> pretty t'
    Nothing -> "_"
  pretty (PatLit e _ _) = pretty e
  pretty (PatConstr n _ ps _) = "#" <> pretty n <+> sep (map pretty ps)
  pretty (PatAttr attr p _) = "#[" <> pretty attr <> "]" </> pretty p

ppAscription :: Pretty t => Maybe t -> Doc a
ppAscription Nothing = mempty
ppAscription (Just t) = colon <> align (pretty t)

instance (Eq vn, IsName vn, Annot f) => Pretty (ProgBase f vn) where
  pretty = stack . punctuate line . map pretty . progDecs

instance (Eq vn, IsName vn, Annot f) => Pretty (DecBase f vn) where
  pretty (ValDec dec) = pretty dec
  pretty (TypeDec dec) = pretty dec
  pretty (SigDec sig) = pretty sig
  pretty (ModDec sd) = pretty sd
  pretty (OpenDec x _) = "open" <+> pretty x
  pretty (LocalDec dec _) = "local" <+> pretty dec
  pretty (ImportDec x _ _) = "import" <+> pretty x

instance (Eq vn, IsName vn, Annot f) => Pretty (ModExpBase f vn) where
  pretty (ModVar v _) = pretty v
  pretty (ModParens e _) = parens $ pretty e
  pretty (ModImport v _ _) = "import" <+> pretty (show v)
  pretty (ModDecs ds _) = nestedBlock "{" "}" (stack $ punctuate line $ map pretty ds)
  pretty (ModApply f a _ _ _) = parens $ pretty f <+> parens (pretty a)
  pretty (ModAscript me se _ _) = pretty me <> colon <+> pretty se
  pretty (ModLambda param maybe_sig body _) =
    "\\" <> pretty param <> maybe_sig'
      <+> "->"
      </> indent 2 (pretty body)
    where
      maybe_sig' = case maybe_sig of
        Nothing -> mempty
        Just (sig, _) -> colon <+> pretty sig

instance Pretty Liftedness where
  pretty Unlifted = ""
  pretty SizeLifted = "~"
  pretty Lifted = "^"

instance (Eq vn, IsName vn, Annot f) => Pretty (TypeBindBase f vn) where
  pretty (TypeBind name l params te rt _ _) =
    "type" <> pretty l
      <+> hsep (prettyName name : map pretty params)
      <+> equals
      <+> maybe (pretty te) pretty (unAnnot rt)

instance (Eq vn, IsName vn) => Pretty (TypeParamBase vn) where
  pretty (TypeParamDim name _) = brackets $ prettyName name
  pretty (TypeParamType l name _) = "'" <> pretty l <> prettyName name

instance (Eq vn, IsName vn, Annot f) => Pretty (ValBindBase f vn) where
  pretty (ValBind entry name retdecl rettype tparams args body _ attrs _) =
    mconcat (map ((<> line) . prettyAttr) attrs)
      <> fun
      <+> align
        ( sep
            ( prettyName name
                : map pretty tparams
                ++ map pretty args
                ++ retdecl'
                ++ ["="]
            )
        )
      </> indent 2 (pretty body)
    where
      fun
        | isJust entry = "entry"
        | otherwise = "def"
      retdecl' = case (pretty <$> unAnnot rettype) `mplus` (pretty <$> retdecl) of
        Just rettype' -> [colon <+> align rettype']
        Nothing -> mempty

instance (Eq vn, IsName vn, Annot f) => Pretty (SpecBase f vn) where
  pretty (TypeAbbrSpec tpsig) = pretty tpsig
  pretty (TypeSpec l name ps _ _) =
    "type" <> pretty l <+> hsep (prettyName name : map pretty ps)
  pretty (ValSpec name tparams vtype _ _ _) =
    "val" <+> hsep (prettyName name : map pretty tparams) <> colon <+> pretty vtype
  pretty (ModSpec name sig _ _) =
    "module" <+> prettyName name <> colon <+> pretty sig
  pretty (IncludeSpec e _) =
    "include" <+> pretty e

instance (Eq vn, IsName vn, Annot f) => Pretty (SigExpBase f vn) where
  pretty (SigVar v _ _) = pretty v
  pretty (SigParens e _) = parens $ pretty e
  pretty (SigSpecs ss _) = nestedBlock "{" "}" (stack $ punctuate line $ map pretty ss)
  pretty (SigWith s (TypeRef v ps td _) _) =
    pretty s <+> "with" <+> pretty v <+> hsep (map pretty ps) <> " =" <+> pretty td
  pretty (SigArrow (Just v) e1 e2 _) =
    parens (prettyName v <> colon <+> pretty e1) <+> "->" <+> pretty e2
  pretty (SigArrow Nothing e1 e2 _) =
    pretty e1 <+> "->" <+> pretty e2

instance (Eq vn, IsName vn, Annot f) => Pretty (SigBindBase f vn) where
  pretty (SigBind name e _ _) =
    "module type" <+> prettyName name <+> equals <+> pretty e

instance (Eq vn, IsName vn, Annot f) => Pretty (ModParamBase f vn) where
  pretty (ModParam pname psig _ _) =
    parens (prettyName pname <> colon <+> pretty psig)

instance (Eq vn, IsName vn, Annot f) => Pretty (ModBindBase f vn) where
  pretty (ModBind name ps sig e _ _) =
    "module" <+> hsep (prettyName name : map pretty ps) <> sig' <> " =" <+> pretty e
    where
      sig' = case sig of
        Nothing -> mempty
        Just (s, _) -> " " <> colon <+> pretty s <> " "

ppBinOp :: IsName v => QualName v -> Doc a
ppBinOp bop =
  case leading of
    Backtick -> "`" <> pretty bop <> "`"
    _ -> pretty bop
  where
    leading = leadingOperator $ toName $ qualLeaf bop

prettyBinOp ::
  (Eq vn, IsName vn, Annot f) =>
  Int ->
  QualName vn ->
  ExpBase f vn ->
  ExpBase f vn ->
  Doc a
prettyBinOp p bop x y =
  parensIf (p > symPrecedence) $
    prettyExp symPrecedence x
      <+> bop'
      <+> prettyExp symRPrecedence y
  where
    bop' = case leading of
      Backtick -> "`" <> pretty bop <> "`"
      _ -> pretty bop
    leading = leadingOperator $ toName $ qualLeaf bop
    symPrecedence = precedence leading
    symRPrecedence = rprecedence leading
    precedence PipeRight = -1
    precedence PipeLeft = -1
    precedence LogAnd = 0
    precedence LogOr = 0
    precedence Band = 1
    precedence Bor = 1
    precedence Xor = 1
    precedence Equal = 2
    precedence NotEqual = 2
    precedence Bang = 2
    precedence Equ = 2
    precedence Less = 2
    precedence Leq = 2
    precedence Greater = 2
    precedence Geq = 2
    precedence ShiftL = 3
    precedence ShiftR = 3
    precedence Plus = 4
    precedence Minus = 4
    precedence Times = 5
    precedence Divide = 5
    precedence Mod = 5
    precedence Quot = 5
    precedence Rem = 5
    precedence Pow = 6
    precedence Backtick = 9
    rprecedence Minus = 10
    rprecedence Divide = 10
    rprecedence op = precedence op
