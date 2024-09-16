module Futhark.Analysis.Proofs.Convert where

import qualified Language.Futhark as E
import Futhark.Analysis.Proofs.Symbol (Symbol (..))
import Futhark.SoP.SoP (SoP, int2SoP, sym2SoP, negSoP)
import qualified Data.List.NonEmpty as NE
import Futhark.Util.Pretty (prettyString, Pretty)
import Data.Maybe (fromMaybe)
import Futhark.MonadFreshNames (VNameSource, newNameFromString)
import Language.Futhark.Semantic (Imports, ImportName, FileModule (fileProg))
import Futhark.Analysis.Proofs.IndexFn (IndexFn (..), IndexFnM, runIndexFnM, clearAlgEnv, insertIndexFn, VEnv (..), Iterator (..), cases, Domain (..), Cases)
import qualified Data.Map as M
import Debug.Trace (traceM)
import Control.Monad.RWS
import Futhark.Analysis.Proofs.Rewrite (rewrite)
import Control.Monad (void, forM_)

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
-- Utilities
--------------------------------------------------------------
debugM :: Applicative f => String -> f ()
debugM x = traceM $ "🪲 " <> x

tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM = traceM . prettyString

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
  _ <- mkIndexFnValBind vb
  mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds


-- toplevel_indexfns
mkIndexFnValBind :: E.ValBind -> IndexFnM (Maybe IndexFn)
mkIndexFnValBind val@(E.ValBind _ vn ret _ _ params body _ _ _) = do
  clearAlgEnv
  traceM ("\n====\nmkIndexFnValBind:\n\n" <> prettyString val)
  indexfn <- forward body >>= refineAndBind vn
  -- insertTopLevel vn (params, indexfn)
  algenv <- gets algenv
  debugM ("AlgEnv\n" <> prettyString algenv)
  pure (Just indexfn)

refineAndBind :: E.VName -> IndexFn -> IndexFnM IndexFn
refineAndBind vn indexfn = do
  indexfn' <- rewrite indexfn
  insertIndexFn vn indexfn'
  tracePrettyM indexfn'
  traceM "\n"
  -- tell ["resulting in", toLaTeX (vn, indexfn')]
  pure indexfn'

singleCase :: a -> Cases Symbol a
singleCase e = cases [(Bool True, e)]

fromScalar :: SoP Symbol -> IndexFn
fromScalar e = IndexFn Empty (singleCase e)

forward :: E.Exp -> IndexFnM IndexFn
forward (E.Parens e _) = forward e
forward (E.Attr _ e _) = forward e
-- Let-bindings.
forward (E.AppExp (E.LetPat _ p@(E.Id vn _ _) x body _) _) = do
  traceM (prettyString p <> " = " <> prettyString x)
  -- tell [textbf "Forward on " <> Math.math (toLaTeX vn) <> toLaTeX x]
  _ <- refineAndBind vn =<< forward x
  forward body
-- Tuples left unhandled for now.
-- forward (E.AppExp (E.LetPat _ p@(E.TuplePat patterns _) x body _) _) = do
--     traceM (prettyString patterns <> " = " <> prettyString x)
--     -- tell [textbf "Forward on " <> Math.math (toLaTeX (S.toList $ E.patNames p)) <> toLaTeX x]
--     xs <- unzipT <$> forward x
--     forM_ (zip patterns xs) refineAndBind'
--     forward body
--     where
--       -- Wrap refineAndBind to discard results otherwise bound to wildcards.
--       refineAndBind' (E.Wildcard {}, _) = pure ()
--       refineAndBind' (E.Id vn _ _, indexfn) =
--         void (refineAndBind vn indexfn)
--       refineAndBind' e = error ("not implemented for " <> show e)
-- Leaves.
forward (E.Literal (E.BoolValue x) _) =
  pure . fromScalar . sym2SoP $ Bool x
forward (E.Literal (E.SignedValue (E.Int64Value x)) _) =
  pure . fromScalar . int2SoP $ toInteger x
forward (E.IntLit x _ _) =
  pure . fromScalar $ int2SoP x
forward (E.Negate (E.IntLit x _ _) _) =
  pure . fromScalar . negSoP $ int2SoP x
forward e@(E.Var (E.QualName _ vn) _ _) = do
  indexfns <- gets indexfns
  case M.lookup vn indexfns of
    Just indexfn -> do
      traceM ("🌪️🎭 sub " <> prettyString vn <> " for " <> prettyString indexfn)
      pure indexfn
    _ -> do
      debugM ("creating index function for " <> prettyString vn)
      -- TODO handle refinement types
      -- handleRefinementTypes e
      case getSize e of
        Just sz -> do
          -- Canonical array representation.
          i <- newNameFromString "i"
          rewrite $
            IndexFn (Forall i (Iota sz))
                    (singleCase . sym2SoP $ Idx (Var vn) (sym2SoP $ Var i))
        Nothing ->
          -- Canonical scalar representation.
          rewrite $ IndexFn Empty (singleCase . sym2SoP $ Var vn)
-- Nodes.
-- TODO handle tuples later.
-- forward (E.TupLit es _) = do
--   xs <- mapM forward es
--   vns <- mapM (\_ -> newNameFromString "xs") xs
--   let IndexFn iter1 _ = head xs
--   foldM (\acc (vn, x) -> sub vn x acc)
--         (IndexFn iter1 (toCases . Tuple $ map Var vns))
--         (zip vns xs)
--     >>= rewrite
-- forward (E.AppExp (E.Index xs' slice _) _)
--   | [E.DimFix idx'] <- slice = do -- XXX support only simple indexing for now
--       IndexFn iter_idx idx <- forward idx'
--       IndexFn iter_xs xs <- forward xs'
--       case iteratorName iter_xs of
--         Just j -> do
--           sub j (IndexFn iter_idx idx) (IndexFn iter_idx xs)
--         Nothing ->
--           error "indexing into a scalar"
forward e = error $ "forward on " <> show e
