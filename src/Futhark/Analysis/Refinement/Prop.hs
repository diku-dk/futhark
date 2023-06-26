module Futhark.Analysis.Refinement.Prop where

import Control.Monad.RWS
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Relations
import Futhark.Analysis.Refinement.Representation
import Futhark.SoP.Util
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Prop qualified as E

-- isNonNeg :: Monad m => E.Exp -> RefineT m Bool
-- isNonNeg (E.AppExp (E.Apply f args _) _)
--   | Just "iota" <- getFun f = pure True
--   | Just fname <- getFun f,
--     "map" `L.isPrefixOf` fname,
--     lam : args' <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args =
--       isNonNeg lam
--   | Just "scan" <- getFun f,
--     [E.OpSection (E.QualName [] vn) _ _, _, xs] <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args,
--     "+" <- E.baseString vn =
--       isNonNeg xs
-- isNonNeg e@(E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
--   | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
--     name <- E.baseString $ E.qualLeaf op,
--     Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] =
--       case bop of
--         E.Plus -> isNonNeg e_x ^&& isNonNeg e_y
--         _ -> pure False
-- isNonNeg (E.AppExp (E.If c t f _) _) =
--   (&&) <$> isNonNeg t <*> isNonNeg f
-- isNonNeg (E.IntLit x _ _) =
--   pure $ x >= 0
-- isNonNeg (E.Lambda _ body _ _ _) =
--   isNonNeg body
-- isNonNeg (E.AppExp (E.Index xs _ _) _) =
--   isNonNeg xs
-- isNonNeg (E.Var (E.QualName qs x) _ _) = do
--   ifM
--     (Var x ^>=^ intToExp 0)
--     (pure True)
--     ( do
--         km <- gets known_map
--         case km M.!? x of
--           Just ps ->
--             anyM isNonNegProp ps
--           Nothing -> pure False
--     )
--   where
--     isNonNegProp (ForAll i xs (x :>= y)) =
--       ifM
--         ((i ^==^ x) ^&& (intToExp 0 ^==^ y))
--         (pure True)
--         (pure False)
--     isNonNegProp _ = pure False
-- isNonNeg _ = pure False
--
-- -- addInfo :: E.Exp -> RefineT m ()
-- -- addInfo (E.AppExp (E.Apply f args _) _)
-- --     | Just "iota" <- getFun f = do
-- --         i <- newVName "i"
-- --         ForEach (Var i) (intToExp 0 ...
-- --
-- --
-- --
-- --     | Just fname <- getFun f,
-- --       "map" `L.isPrefixOf` fname,
-- --       E.Lambda params body _ _ _ : args' <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args = do
--
simplify :: (Monad m) => a -> RefineT m a
simplify = pure

-- simplify = astMap m
--   where
--     m =
--       ASTMapper
--         { mapOnLit = astMap m,
--           mapOnExp =
--             \e ->
--               case e of
--                 (Var x) -> do
--                   km <- gets known_map
--                   case km M.!? x of
--                     Just ps ->
--                       let isFwdEq [] = pure $ Var x
--                           isFwdEq ((z :== y) : rest)
--                             | z == Var x = pure y
--                             | otherwise = isFwdEq rest
--                           isFwdEq (_ : rest) = isFwdEq rest
--                        in isFwdEq ps
--                     _ -> pure $ Var x
--                 _ -> astMap m e,
--           mapOnProp =
--             \p ->
--               ifM
--                 (checkProp p)
--                 (pure $ Bool True)
--                 (astMap m p)
--         }
