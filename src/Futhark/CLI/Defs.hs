-- | @futhark defs@
module Futhark.CLI.Defs (main) where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Compiler
import Futhark.Util.Loc
import Futhark.Util.Options
import Language.Futhark

data DefKind = Value | Module | ModuleType | Type

data Def = Def DefKind Name Loc

kindText :: DefKind -> T.Text
kindText Value = "value"
kindText Module = "module"
kindText ModuleType = "module type"
kindText Type = "type"

printDef :: Def -> IO ()
printDef (Def k name loc) = do
  T.putStrLn $ T.unwords [kindText k, nameToText name, T.pack (locStr loc)]

defsInProg :: UncheckedProg -> Seq.Seq Def
defsInProg = foldMap defsInDec . progDecs
  where
    defsInDec (ValDec vb) =
      Seq.singleton $ Def Value (valBindName vb) (locOf vb)
    defsInDec (TypeDec tb) =
      Seq.singleton $ Def Type (typeAlias tb) (locOf tb)
    defsInDec (LocalDec d _) = defsInDec d
    defsInDec (OpenDec me _) = defsInModExp me
    defsInDec (ModDec mb) = defsInModExp $ modExp mb
    defsInDec ModTypeDec {} = mempty
    defsInDec ImportDec {} = mempty

    defsInModExp ModVar {} = mempty
    defsInModExp (ModParens me _) = defsInModExp me
    defsInModExp ModImport {} = mempty
    defsInModExp (ModDecs ds _) = foldMap defsInDec ds
    defsInModExp (ModApply me1 me2 _ _ _) = defsInModExp me1 <> defsInModExp me2
    defsInModExp (ModAscript me _ _ _) = defsInModExp me
    defsInModExp (ModLambda _ _ me _) = defsInModExp me

-- | Run @futhark defs@.
main :: String -> [String] -> IO ()
main = mainWithOptions () [] "program" $ \args () ->
  case args of
    [file] -> Just $ do
      prog <- readUntypedProgramOrDie file
      mapM_ printDef . foldMap (defsInProg . snd) $
        filter (not . isBuiltin . fst) prog
    _ -> Nothing
