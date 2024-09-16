module Futhark.Analysis.Proofs.Convert where

import qualified Language.Futhark as E
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP)
import qualified Data.List.NonEmpty as NE
import Futhark.Util.Pretty (prettyString)
import Data.Maybe (fromMaybe)
import Futhark.MonadFreshNames (VNameSource)
import Language.Futhark.Semantic (Imports, ImportName, FileModule (fileProg))
import Futhark.Analysis.Proofs.IndexFn (IndexFn, IndexFnM, runIndexFnM, clearAlgEnv)
import qualified Data.Map as M

--------------------------------------------------------------
-- Extracting information from E.Exp.
--------------------------------------------------------------
getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getSize :: E.Exp -> Maybe (SoP Symbol)
getSize (E.Var _ (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize (E.ArrayLit [] (E.Info {E.unInfo = ty}) _) = sizeOfTypeBase ty
getSize e = error $ "getSize: " <> prettyString e <> "\n" <> show e

sizeOfTypeBase :: E.TypeBase E.Exp as -> Maybe (SoP Symbol)
-- sizeOfTypeBase (E.Scalar (E.Refinement ty _)) =
--   -- TODO why are all refinements scalar?
--   sizeOfTypeBase ty
sizeOfTypeBase (E.Array _ shape _)
  | dim:_ <- E.shapeDims shape =
    Just $ convertSize dim
  where
    convertSize (E.Var (E.QualName _ x) _ _) = sym2SoP $ Var x
    convertSize (E.Parens e _) = convertSize e
    convertSize (E.Attr _ e _) = convertSize e
    convertSize (E.IntLit x _ _) = int2SoP x
    convertSize e = error ("convertSize not implemented for: " <> show e)
sizeOfTypeBase _ = Nothing


-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList
  where
    stripExp x = fromMaybe x (E.stripExp x)

--------------------------------------------------------------
-- Construct index function for source program
--------------------------------------------------------------
mkIndexFnProg :: VNameSource -> Imports -> M.Map E.VName IndexFn
mkIndexFnProg vns prog = snd $ runIndexFnM (mkIndexFnImports prog) vns

mkIndexFnImports :: [(ImportName, FileModule)] -> IndexFnM ()
mkIndexFnImports = mapM_ (mkIndexFnDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkIndexFnDecs :: [E.Dec] -> IndexFnM ()
mkIndexFnDecs [] = pure ()
mkIndexFnDecs (E.ValDec vb : rest) = do
  clearAlgEnv
  mkIndexFnValBind vb
  mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds
