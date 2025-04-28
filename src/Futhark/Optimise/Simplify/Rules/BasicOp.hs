{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-incomplete-record-updates #-}

-- | Some simplification rules for t'BasicOp'.
module Futhark.Optimise.Simplify.Rules.BasicOp
  ( basicOpRules,
  )
where

import Control.Monad
import Data.List (find, foldl', isSuffixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Futhark.Analysis.PrimExp.Convert
import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Construct
import Futhark.IR
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Rules.Loop
import Futhark.Optimise.Simplify.Rules.Simple

isCt1 :: SubExp -> Bool
isCt1 (Constant v) = oneIsh v
isCt1 _ = False

isCt0 :: SubExp -> Bool
isCt0 (Constant v) = zeroIsh v
isCt0 _ = False

data ConcatArg
  = ArgArrayLit [SubExp]
  | ArgReplicate [SubExp] SubExp
  | ArgVar VName

toConcatArg :: ST.SymbolTable rep -> VName -> (ConcatArg, Certs)
toConcatArg vtable v =
  case ST.lookupBasicOp v vtable of
    Just (ArrayLit ses _, cs) ->
      (ArgArrayLit ses, cs)
    Just (Replicate (Shape [d]) se, cs) ->
      (ArgReplicate [d] se, cs)
    _ ->
      (ArgVar v, mempty)

fromConcatArg ::
  (MonadBuilder m) =>
  Type ->
  (ConcatArg, Certs) ->
  m VName
fromConcatArg t (ArgArrayLit ses, cs) =
  certifying cs $ letExp "concat_lit" $ BasicOp $ ArrayLit ses $ rowType t
fromConcatArg _ (ArgReplicate ws se, cs) = certifying cs $ do
  w <- letSubExp "concat_rep_w" =<< toExp (sum $ map pe64 ws)
  letExp "concat_rep" $ BasicOp $ Replicate (Shape [w]) se
fromConcatArg _ (ArgVar v, _) =
  pure v

fuseConcatArg ::
  [(ConcatArg, Certs)] ->
  (ConcatArg, Certs) ->
  [(ConcatArg, Certs)]
fuseConcatArg xs (ArgArrayLit [], _) =
  xs
fuseConcatArg xs (ArgReplicate [w] se, cs)
  | isCt0 w =
      xs
  | isCt1 w =
      fuseConcatArg xs (ArgArrayLit [se], cs)
fuseConcatArg ((ArgArrayLit x_ses, x_cs) : xs) (ArgArrayLit y_ses, y_cs) =
  (ArgArrayLit (x_ses ++ y_ses), x_cs <> y_cs) : xs
fuseConcatArg ((ArgReplicate x_ws x_se, x_cs) : xs) (ArgReplicate y_ws y_se, y_cs)
  | x_se == y_se =
      (ArgReplicate (x_ws ++ y_ws) x_se, x_cs <> y_cs) : xs
fuseConcatArg xs y =
  y : xs

simplifyConcat :: (BuilderOps rep) => BottomUpRuleBasicOp rep
-- concat@1(transpose(x),transpose(y)) == transpose(concat@0(x,y))
simplifyConcat (vtable, _) pat _ (Concat i (x :| xs) new_d)
  | Just r <- arrayRank <$> ST.lookupType x vtable,
    let perm = [i] ++ [0 .. i - 1] ++ [i + 1 .. r - 1],
    Just (x', x_cs) <- transposedBy perm x,
    Just (xs', xs_cs) <- mapAndUnzipM (transposedBy perm) xs = Simplify $ do
      concat_rearrange <-
        certifying (x_cs <> mconcat xs_cs) $
          letExp "concat_rearrange" $
            BasicOp $
              Concat 0 (x' :| xs') new_d
      letBind pat $ BasicOp $ Rearrange perm concat_rearrange
  where
    transposedBy perm1 v =
      case ST.lookupExp v vtable of
        Just (BasicOp (Rearrange perm2 v'), vcs)
          | perm1 == perm2 -> Just (v', vcs)
        _ -> Nothing

-- Removing a concatenation that involves only a single array.  This
-- may be produced as a result of other simplification rules.
simplifyConcat (vtable, _) pat aux (Concat _ (x :| []) w)
  | Just x_t <- ST.lookupType x vtable,
    arraySize 0 x_t == w =
      -- Still need a copy because Concat produces a fresh array.
      Simplify $ auxing aux $ letBind pat $ BasicOp $ Replicate mempty $ Var x
-- concat xs (concat ys zs) == concat xs ys zs
simplifyConcat (vtable, _) pat (StmAux cs attrs _) (Concat i (x :| xs) new_d)
  | x' /= x || concat xs' /= xs =
      Simplify $
        certifying (cs <> x_cs <> mconcat xs_cs) $
          attributing attrs $
            letBind pat $
              BasicOp $
                Concat i (x' :| zs ++ concat xs') new_d
  where
    (x' : zs, x_cs) = isConcat x
    (xs', xs_cs) = unzip $ map isConcat xs
    isConcat v = case ST.lookupBasicOp v vtable of
      Just (Concat j (y :| ys) _, v_cs) | j == i -> (y : ys, v_cs)
      _ -> ([v], mempty)

-- Removing empty arrays from concatenations.
simplifyConcat (vtable, _) pat aux (Concat i (x :| xs) new_d)
  | Just ts <- mapM (`ST.lookupType` vtable) $ x : xs,
    x' : xs' <- map fst $ filter (isNothing . isEmptyArray . snd) $ zip (x : xs) ts,
    xs' /= xs =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ Concat i (x' :| xs') new_d
-- Fusing arguments to the concat when possible.  Only done when
-- concatenating along the outer dimension for now.
simplifyConcat (vtable, _) pat aux (Concat 0 (x :| xs) outer_w)
  | -- We produce the to-be-concatenated arrays in reverse order, so
    -- reverse them back.
    y : ys <-
      forSingleArray . reverse . foldl' fuseConcatArg mempty $
        map (toConcatArg vtable) (x : xs),
    length xs /= length ys =
      Simplify $ do
        elem_type <- lookupType x
        y' <- fromConcatArg elem_type y
        ys' <- mapM (fromConcatArg elem_type) ys
        auxing aux $ letBind pat $ BasicOp $ Concat 0 (y' :| ys') outer_w
  where
    -- If we fuse so much that there is only a single input left, then
    -- it must have the right size.
    forSingleArray [(ArgReplicate _ v, cs)] =
      [(ArgReplicate [outer_w] v, cs)]
    forSingleArray ys = ys
simplifyConcat _ _ _ _ = Skip

ruleBasicOp :: (BuilderOps rep) => TopDownRuleBasicOp rep
ruleBasicOp vtable pat aux op
  | Just (op', cs) <- applySimpleRules defOf seType op =
      Simplify $ certifying (cs <> stmAuxCerts aux) $ letBind pat $ BasicOp op'
  where
    defOf = (`ST.lookupExp` vtable)
    seType (Var v) = ST.lookupType v vtable
    seType (Constant v) = Just $ Prim $ primValueType v
ruleBasicOp vtable pat aux (Update _ src _ (Var v))
  | Just (BasicOp Scratch {}, _) <- ST.lookupExp v vtable =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ Var src
-- If we are writing a single-element slice from some array, and the
-- element of that array can be computed as a PrimExp based on the
-- index, let's just write that instead.
ruleBasicOp vtable pat aux (Update safety src (Slice [DimSlice i n s]) (Var v))
  | isCt1 n,
    isCt1 s,
    Just (ST.Indexed cs e) <- ST.index v [intConst Int64 0] vtable =
      Simplify $ do
        e' <- toSubExp "update_elem" e
        auxing aux . certifying cs $
          letBind pat $
            BasicOp $
              Update safety src (Slice [DimFix i]) e'
ruleBasicOp vtable pat aux (Update _ dest destis (Var v))
  | Just (e, _) <- ST.lookupExp v vtable,
    arrayFrom e =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ Var dest
  where
    arrayFrom (BasicOp (Replicate (Shape []) (Var copy_v)))
      | Just (e', _) <- ST.lookupExp copy_v vtable =
          arrayFrom e'
    arrayFrom (BasicOp (Index src srcis)) =
      src == dest && destis == srcis
    arrayFrom (BasicOp (Replicate v_shape v_se))
      | Just (Replicate dest_shape dest_se, _) <- ST.lookupBasicOp dest vtable,
        v_se == dest_se,
        shapeDims v_shape `isSuffixOf` shapeDims dest_shape =
          True
    arrayFrom _ =
      False
ruleBasicOp vtable pat aux (Update Unsafe dest is se)
  | Just dest_t <- ST.lookupType dest vtable,
    isFullSlice (arrayShape dest_t) is = Simplify . auxing aux $
      case se of
        Var v | not $ null $ sliceDims is -> do
          v_t <- lookupType v
          v_reshaped <-
            letSubExp (baseString v ++ "_reshaped") . BasicOp $
              Reshape (reshapeAll (arrayShape v_t) (arrayShape dest_t)) v
          letBind pat $ BasicOp $ Replicate mempty v_reshaped
        _ -> letBind pat $ BasicOp $ ArrayLit [se] $ rowType dest_t
ruleBasicOp vtable pat (StmAux cs1 attrs _) (Update safety1 dest1 is1 (Var v1))
  | Just (Update safety2 dest2 is2 se2, cs2) <- ST.lookupBasicOp v1 vtable,
    Just (Replicate (Shape []) (Var v3), cs3) <- ST.lookupBasicOp dest2 vtable,
    Just (Index v4 is4, cs4) <- ST.lookupBasicOp v3 vtable,
    is4 == is1,
    v4 == dest1 =
      Simplify $
        certifying (cs1 <> cs2 <> cs3 <> cs4) $ do
          is5 <- subExpSlice $ sliceSlice (primExpSlice is1) (primExpSlice is2)
          attributing attrs $ letBind pat $ BasicOp $ Update (max safety1 safety2) dest1 is5 se2
ruleBasicOp vtable pat _ (CmpOp (CmpEq t) se1 se2)
  | Just m <- simplifyWith se1 se2 = Simplify m
  | Just m <- simplifyWith se2 se1 = Simplify m
  where
    simplifyWith (Var v) x
      | Just stm <- ST.lookupStm v vtable,
        Match [p] [Case [Just (BoolValue True)] tbranch] fbranch _ <- stmExp stm,
        Just (y, z) <-
          returns v (stmPat stm) tbranch fbranch,
        not $ boundInBody tbranch `namesIntersect` freeIn y,
        not $ boundInBody fbranch `namesIntersect` freeIn z = Just $ do
          eq_x_y <-
            letSubExp "eq_x_y" $ BasicOp $ CmpOp (CmpEq t) x y
          eq_x_z <-
            letSubExp "eq_x_z" $ BasicOp $ CmpOp (CmpEq t) x z
          p_and_eq_x_y <-
            letSubExp "p_and_eq_x_y" $ BasicOp $ BinOp LogAnd p eq_x_y
          not_p <-
            letSubExp "not_p" $ BasicOp $ UnOp (Neg Bool) p
          not_p_and_eq_x_z <-
            letSubExp "p_and_eq_x_y" $ BasicOp $ BinOp LogAnd not_p eq_x_z
          letBind pat $
            BasicOp $
              BinOp LogOr p_and_eq_x_y not_p_and_eq_x_z
    simplifyWith _ _ =
      Nothing

    returns v ifpat tbranch fbranch =
      fmap snd . find ((== v) . patElemName . fst) $
        zip (patElems ifpat) $
          zip (map resSubExp (bodyResult tbranch)) (map resSubExp (bodyResult fbranch))
ruleBasicOp _ pat _ (Replicate _ se)
  | [Acc {}] <- patTypes pat =
      Simplify $ letBind pat $ BasicOp $ SubExp se
ruleBasicOp _ pat _ (Replicate (Shape []) se)
  | [Prim _] <- patTypes pat =
      Simplify $ letBind pat $ BasicOp $ SubExp se
ruleBasicOp vtable pat _ (Replicate shape (Var v))
  | Just (BasicOp (Replicate shape2 se), cs) <- ST.lookupExp v vtable,
    ST.subExpAvailable se vtable =
      Simplify $ certifying cs $ letBind pat $ BasicOp $ Replicate (shape <> shape2) se
ruleBasicOp _ pat _ (ArrayLit (se : ses) _)
  | all (== se) ses =
      Simplify $
        let n = constant (fromIntegral (length ses) + 1 :: Int64)
         in letBind pat $ BasicOp $ Replicate (Shape [n]) se
ruleBasicOp vtable pat aux (Index idd slice)
  | Just inds <- sliceIndices slice,
    Just (BasicOp (Reshape newshape idd2), idd_cs) <- ST.lookupExp idd vtable,
    length newshape == length inds =
      Simplify $
        case reshapeKind newshape of
          ReshapeCoerce ->
            certifying idd_cs . auxing aux . letBind pat . BasicOp $
              Index idd2 slice
          ReshapeArbitrary -> do
            -- Linearise indices and map to old index space.
            oldshape <- arrayDims <$> lookupType idd2
            let new_inds =
                  reshapeIndex
                    (map pe64 oldshape)
                    (map pe64 $ shapeDims $ newShape newshape)
                    (map pe64 inds)
            new_inds' <-
              mapM (toSubExp "new_index") new_inds
            certifying idd_cs . auxing aux $
              letBind pat $
                BasicOp $
                  Index idd2 $
                    Slice $
                      map DimFix new_inds'

-- Copying an iota is pointless; just make it an iota instead.
ruleBasicOp vtable pat aux (Replicate (Shape []) (Var v))
  | Just (Iota n x s it, v_cs) <- ST.lookupBasicOp v vtable =
      Simplify . certifying v_cs . auxing aux $
        letBind pat $
          BasicOp $
            Iota n x s it
-- Handle identity permutation.
ruleBasicOp _ pat _ (Rearrange perm v)
  | sort perm == perm =
      Simplify $ letBind pat $ BasicOp $ SubExp $ Var v
ruleBasicOp vtable pat aux (Rearrange perm v)
  | Just (BasicOp (Rearrange perm2 e), v_cs) <- ST.lookupExp v vtable =
      -- Rearranging a rearranging: compose the permutations.
      Simplify . certifying v_cs . auxing aux $
        letBind pat $
          BasicOp $
            Rearrange (perm `rearrangeCompose` perm2) e
-- Rearranging a replicate where the outer dimension is left untouched.
ruleBasicOp vtable pat aux (Rearrange perm v1)
  | Just (BasicOp (Replicate dims (Var v2)), v1_cs) <- ST.lookupExp v1 vtable,
    num_dims <- shapeRank dims,
    (rep_perm, rest_perm) <- splitAt num_dims perm,
    not $ null rest_perm,
    rep_perm == [0 .. length rep_perm - 1] =
      Simplify $
        certifying v1_cs $
          auxing aux $ do
            v <-
              letSubExp "rearrange_replicate" $
                BasicOp $
                  Rearrange (map (subtract num_dims) rest_perm) v2
            letBind pat $ BasicOp $ Replicate dims v

-- Simplify away 0<=i when 'i' is from a loop of form 'for i < n'.
ruleBasicOp vtable pat aux (CmpOp CmpSle {} x y)
  | Constant (IntValue (Int64Value 0)) <- x,
    Var v <- y,
    Just _ <- ST.lookupLoopVar v vtable =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ constant True
-- Simplify away i<n when 'i' is from a loop of form 'for i < n'.
ruleBasicOp vtable pat aux (CmpOp CmpSlt {} x y)
  | Var v <- x,
    Just n <- ST.lookupLoopVar v vtable,
    n == y =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ constant True
-- Simplify away x<0 when 'x' has been used as array size.
ruleBasicOp vtable pat aux (CmpOp CmpSlt {} (Var x) y)
  | isCt0 y,
    maybe False ST.entryIsSize $ ST.lookup x vtable =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ constant False
-- Simplify away 0<=y when 'y' has been used as array size.
ruleBasicOp vtable pat aux (CmpOp CmpSle {} x (Var y))
  | isCt0 x,
    maybe False ST.entryIsSize $ ST.lookup y vtable =
      Simplify $ auxing aux $ letBind pat $ BasicOp $ SubExp $ constant True
-- Remove certificates for variables whose definition already contain
-- that certificate.
ruleBasicOp vtable pat aux (SubExp (Var v))
  | cs <- unCerts $ stmAuxCerts aux,
    not $ null cs,
    Just v_cs <- unCerts . stmCerts <$> ST.lookupStm v vtable,
    cs' <- filter (`notElem` v_cs) cs,
    cs' /= cs =
      Simplify . certifying (Certs cs') $
        letBind pat $
          BasicOp $
            SubExp $
              Var v
-- Remove UpdateAccs that contribute the neutral value, which is
-- always a no-op.
ruleBasicOp vtable pat aux (UpdateAcc _ acc _ vs)
  | Pat [pe] <- pat,
    Acc token _ _ _ <- patElemType pe,
    Just (_, _, Just (_, ne)) <- ST.entryAccInput =<< ST.lookup token vtable,
    vs == ne =
      Simplify . auxing aux $ letBind pat $ BasicOp $ SubExp $ Var acc
-- Manifest of a a copy (or another Manifest) can be simplified to
-- manifesting the original array, if it is still available.
ruleBasicOp vtable pat aux (Manifest perm v1)
  | Just (Replicate (Shape []) (Var v2), cs) <- ST.lookupBasicOp v1 vtable,
    ST.available v2 vtable =
      Simplify . auxing aux . certifying cs . letBind pat . BasicOp $
        Manifest perm v2
  | Just (Manifest _ v2, cs) <- ST.lookupBasicOp v1 vtable,
    ST.available v2 vtable =
      Simplify . auxing aux . certifying cs . letBind pat . BasicOp $
        Manifest perm v2
ruleBasicOp vtable pat aux (Reshape v3_shape v2)
  | ReshapeArbitrary <- reshapeKind v3_shape,
    Just (Rearrange perm v1, v2_cs) <- ST.lookupBasicOp v2 vtable,
    Just (Reshape v1_shape v0, v1_cs) <- ST.lookupBasicOp v1 vtable,
    ReshapeArbitrary <- reshapeKind v1_shape,
    Just v0_shape <- arrayShape <$> ST.lookupType v0 vtable,
    Just perm' <-
      flipReshapeRearrange (shapeDims v0_shape) (shapeDims (newShape v1_shape)) perm =
      Simplify $ do
        v1' <- letExp (baseString v1) $ BasicOp $ Rearrange perm' v0
        auxing aux . certifying (v1_cs <> v2_cs) . letBind pat $
          BasicOp (Reshape v3_shape v1')
ruleBasicOp _ _ _ _ =
  Skip

topDownRules :: (BuilderOps rep) => [TopDownRule rep]
topDownRules =
  [ RuleBasicOp ruleBasicOp
  ]

bottomUpRules :: (BuilderOps rep) => [BottomUpRule rep]
bottomUpRules =
  [ RuleBasicOp simplifyConcat
  ]

-- | A set of simplification rules for t'BasicOp's.  Includes rules
-- from "Futhark.Optimise.Simplify.Rules.Simple".
basicOpRules :: (BuilderOps rep) => RuleBook rep
basicOpRules = ruleBook topDownRules bottomUpRules <> loopRules
