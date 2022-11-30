{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Compatibility shims for mainland-pretty; the prettyprinting
-- library used by language-c-quote.
module Futhark.CodeGen.Backends.GenericC.Pretty
  ( expText,
    definitionsText,
    typeText,
    idText,
    funcText,
    funcsText,
  )
where

import Data.Text qualified as T
import Language.C.Pretty ()
import Language.C.Syntax qualified as C
import Text.PrettyPrint.Mainland qualified as MPP
import Text.PrettyPrint.Mainland.Class qualified as MPP

expText :: C.Exp -> T.Text
expText = T.pack . MPP.pretty 8000 . MPP.ppr

definitionsText :: [C.Definition] -> T.Text
definitionsText = T.unlines . map (T.pack . MPP.pretty 8000 . MPP.ppr)

typeText :: C.Type -> T.Text
typeText = T.pack . MPP.pretty 8000 . MPP.ppr

idText :: C.Id -> T.Text
idText = T.pack . MPP.pretty 8000 . MPP.ppr

funcText :: C.Func -> T.Text
funcText = T.pack . MPP.pretty 8000 . MPP.ppr

funcsText :: [C.Func] -> T.Text
funcsText = T.unlines . map funcText
