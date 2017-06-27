
module Documentation.Util where

import Data.Monoid

import Language.Futhark
import Futhark.Util.Pretty (Doc,ppr)

import qualified Text.PrettyPrint.Mainland as PP (pretty)
import Text.Blaze.Html5 (toHtml, Html)

docToHtml :: Doc -> Html
docToHtml = toHtml . PP.pretty 80

primTypeHtml :: PrimType -> Html
primTypeHtml = docToHtml . ppr

prettyTypeName :: TypeName -> Html
prettyTypeName et = (docToHtml . ppr) (baseName <$> qualNameFromTypeName et)

prettyU :: Uniqueness -> Html
prettyU = docToHtml . ppr

renderName :: Name -> Html
renderName name = docToHtml (ppr name)

joinBy :: Html -> [Html] -> Html
joinBy _ [] = mempty
joinBy _ [x] = x
joinBy sep (x:xs) = x <> foldMap (sep <>) xs

commas :: [Html] -> Html
commas = joinBy (toHtml ",")

parens :: Html -> Html
parens x = toHtml "(" <> x <> toHtml ")"
braces :: Html -> Html
braces x = toHtml "{" <> x <> toHtml "}"
brackets :: Html -> Html
brackets x = toHtml "[" <> x <> toHtml "]"

prettyQualName :: QualName VName -> Html
prettyQualName v = docToHtml (ppr v')
  where v' = QualName names name
        QualName names (VName name _) = v
