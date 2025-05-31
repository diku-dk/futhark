-- | Fundamental facilities for constructing type error messages.
module Language.Futhark.TypeChecker.Error
  ( -- * Breadcrumbs
    BreadCrumbs,
    hasNoBreadCrumbs,
    matchingField,
    matchingConstructor,
    matchingTypes,
    matching,
  )
where

import Futhark.Util.Pretty
import Language.Futhark

-- | A piece of information that describes what process the type
-- checker currently performing.  This is used to give better error
-- messages for unification errors.
data BreadCrumb
  = MatchingTypes StructType StructType
  | MatchingFields [Name]
  | MatchingConstructor Name
  | Matching (Doc ())

instance Pretty BreadCrumb where
  pretty (MatchingTypes t1 t2) =
    "When matching type"
      </> indent 2 (pretty t1)
      </> "with"
      </> indent 2 (pretty t2)
  pretty (MatchingFields fields) =
    "When matching types of record field"
      <+> dquotes (mconcat $ punctuate "." $ map pretty fields)
      <> dot
  pretty (MatchingConstructor c) =
    "When matching types of constructor" <+> dquotes (pretty c) <> dot
  pretty (Matching s) =
    unAnnotate s

-- | Unification failures can occur deep down inside complicated types
-- (consider nested records). We leave breadcrumbs behind us so we can
-- report the path we took to find the mismatch. When combining
-- breadcrumbs with the 'Semigroup' instance, put the innermost
-- breadcrumbs to the left.
newtype BreadCrumbs = BreadCrumbs [BreadCrumb]

instance Semigroup BreadCrumbs where
  BreadCrumbs (MatchingFields xs : bcs1) <> BreadCrumbs (MatchingFields ys : bcs2) =
    BreadCrumbs $ MatchingFields (ys <> xs) : bcs1 <> bcs2
  BreadCrumbs bcs1 <> BreadCrumbs bcs2 =
    BreadCrumbs $ bcs1 <> bcs2

instance Monoid BreadCrumbs where
  mempty = BreadCrumbs []

-- | Is the path empty?
hasNoBreadCrumbs :: BreadCrumbs -> Bool
hasNoBreadCrumbs (BreadCrumbs []) = True
hasNoBreadCrumbs _ = False

-- | Matching a record field.
matchingField :: Name -> BreadCrumbs
matchingField f = BreadCrumbs [MatchingFields [f]]

-- | Matching two types.
matchingTypes :: StructType -> StructType -> BreadCrumbs
matchingTypes t1 t2 = BreadCrumbs [MatchingTypes t1 t2]

-- | Matching a constructor.
matchingConstructor :: Name -> BreadCrumbs
matchingConstructor c = BreadCrumbs [MatchingConstructor c]

-- | Matching anything.
matching :: Doc () -> BreadCrumbs
matching d = BreadCrumbs [Matching d]

instance Pretty BreadCrumbs where
  pretty (BreadCrumbs []) = mempty
  pretty (BreadCrumbs bcs) = line <> stack (map pretty bcs)
