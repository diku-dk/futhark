-- | This module implements simple simplification rules for bindings.
-- The intent is that you pass a symbol-lookup function and a binding,
-- and is given back a sequence of bindings, that are more efficient
-- than the original binding, yet compute the same result.
--
-- These rewrite rules are "local", in that they do not maintain any
-- state or look at the program as a whole.  Compare this to the
-- fusion algorithm in @L0C.HOTrans.Fusion@, which must be implemented
-- as its own pass.
module L0C.EnablingOpts.Simplification
  ( simplifyBinding
  , VarLookup
  )

where

import Control.Applicative
import Control.Monad

import Data.Array
import Data.Bits
import Data.Maybe
import Data.Loc

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS
import qualified Data.Set          as S

import L0C.InternalRep
import L0C.Tools

-- | A function that, given a variable name, returns its definition.
type VarLookup = VName -> Maybe Exp

-- | @simplifyBinding lookup bnd@ performs simplification of the
-- binding @bnd@.  If simplification is possible, a replacement list
-- of bindings is returned, that bind at least the same banes as the
-- original binding (and possibly more, for intermediate results).
simplifyBinding :: VarLookup -> Binding -> Maybe [Binding]

simplifyBinding = applyRules simplificationRules

applyRules :: [SimplificationRule]
           -> VarLookup -> Binding -> Maybe [Binding]
applyRules []           _    _   = Nothing
applyRules (rule:rules) look bnd =
  (concatMap subApply <$> rule look bnd) <|>
  applyRules rules look bnd
  where subApply bnd' =
          fromMaybe [bnd'] $ applyRules (rule:rules) look bnd'

type SimplificationRule = VarLookup -> Binding -> Maybe [Binding]

simplificationRules :: [SimplificationRule]
simplificationRules = [ liftIdentityMapping
                      , removeReplicateMapping
                      , hoistLoopInvariantMergeVariables
                      , letRule simplifyConstantRedomap
                      , letRule simplifyConstantReduce
                      , letRule simplifyRotate
                      , letRule simplifyBinOp
                      , letRule simplifyNot
                      , letRule simplifyNegate
                      , letRule simplifyAssert
                      , letRule simplifyConjoin
                      , letRule simplifyIndexing
                      ]

liftIdentityMapping :: SimplificationRule
liftIdentityMapping _ (LetBind pat (Map cs fun arrs loc)) =
  case foldr checkInvariance ([], [], []) $ zip3 pat resultSubExps rettype of
    ([], _, _) -> Nothing
    (invariant, mapresult, rettype') ->
      let (pat', resultSubExps') = unzip mapresult
          lambdaRes = Result rescs resultSubExps' resloc
          fun' = fun { lambdaBody = lambdaRes `setBodyResult` lambdaBody fun
                     , lambdaReturnType = rettype'
                     }
      in Just $ LetBind pat' (Map cs fun' arrs loc) : invariant
  where inputMap = HM.fromList $ zip (map identName $ lambdaParams fun) arrs
        free = freeInBody $ lambdaBody fun
        rettype = lambdaReturnType fun
        (rescs, resultSubExps, resloc) = bodyResult $ lambdaBody fun
        outersize = arraysSize 0 $ map subExpType arrs

        freeOrConst (Var v)       = v `HS.member` free
        freeOrConst (Constant {}) = True

        checkInvariance (outId, Var v, _) (invariant, mapresult, rettype')
          | Just inp <- HM.lookup (identName v) inputMap =
            (LetBind [outId] (SubExp inp) : invariant,
             mapresult,
             rettype')
        checkInvariance (outId, e, t) (invariant, mapresult, rettype')
          | freeOrConst e = (LetBind [outId] (Replicate outersize e loc) : invariant,
                             mapresult,
                             rettype')
          | otherwise = (invariant,
                         (outId, e) : mapresult,
                         t : rettype')
liftIdentityMapping _ _ = Nothing

removeReplicateMapping :: SimplificationRule
removeReplicateMapping look (LetBind pat (Map _ fun arrs _)) =
  case mapM isReplicate arrs of
    Just arrs'@((n,_):_) ->
      -- 'n' is the size of the destination array.
      let parameterBnds = [ LetBind [fromParam par] $ SubExp e
                            | (par, (_, e)) <- zip (lambdaParams fun) arrs' ]
          (_, resultSubExps, resloc) = bodyResult $ lambdaBody fun
          resultBnds = [ LetBind [v] $ Replicate n e resloc
                           | (v, e) <- zip pat resultSubExps ]
      in -- XXX: Throwing away certificates.
         Just $ parameterBnds ++
                bodyBindings (lambdaBody fun) ++
                resultBnds
    _ -> Nothing
  where isReplicate (Var v)
          | Just (Replicate n e _) <- look $ identName v = Just (n,e)
        isReplicate _                                    = Nothing
removeReplicateMapping _ _ = Nothing

hoistLoopInvariantMergeVariables :: SimplificationRule
hoistLoopInvariantMergeVariables _ (LoopBind merge idd n loopbody) =
    -- Figure out which of the elemens of loopresult are loop-invariant,
  -- and hoist them out.
  case foldr checkInvariance ([], [], []) $ zip merge resultSubExps of
    ([], _, _) ->
      -- Nothing is invariant.
      Nothing
    (invariant, merge', resultSubExps') ->
      -- We have moved something invariant out of the loop - re-run
      -- the operation with the new enclosing bindings, because
      -- opportunities for copy propagation will have cropped up.
      let loopbody' = Result cs resultSubExps' resloc `setBodyResult` loopbody
      in Just $ invariant ++ [LoopBind merge' idd n loopbody']
  where (cs, resultSubExps, resloc) = bodyResult loopbody

        checkInvariance ((v1,initExp), Var v2) (invariant, merge', resExps)
          | identName v1 == identName v2 =
            (LetBind [v1] (SubExp initExp):invariant, merge', resExps)
        checkInvariance ((v1,initExp), resExp) (invariant, merge', resExps) =
          (invariant, (v1,initExp):merge', resExp:resExps)
hoistLoopInvariantMergeVariables _ _ = Nothing

type LetSimplificationRule = VarLookup -> Exp -> Maybe Exp

letRule :: LetSimplificationRule -> SimplificationRule
letRule rule look (LetBind pat e) = (:[]) . LetBind pat <$> rule look e
letRule _    _    _               = Nothing

simplifyConstantRedomap :: LetSimplificationRule
simplifyConstantRedomap _ (Redomap _ _ innerfun acc _ loc) = do
  es <- simplifyConstantFoldFun innerfun acc
  return $ TupLit es loc
simplifyConstantRedomap _ _ =
  Nothing

simplifyConstantReduce :: LetSimplificationRule
simplifyConstantReduce _ (Reduce _ fun input loc) = do
  es <- simplifyConstantFoldFun fun $ map fst input
  return $ TupLit es loc
simplifyConstantReduce _ _ =
  Nothing

simplifyConstantFoldFun :: Lambda -> [SubExp] -> Maybe [SubExp]
simplifyConstantFoldFun lam accs =
  zipWithM isConstResult resultSubExps $ zip (lambdaParams lam) accs
  where (_, resultSubExps, _) = bodyResult $ lambdaBody lam
        free = freeNamesInBody (lambdaBody lam) `HS.difference`
               HS.fromList (map identName $ lambdaParams lam)
        isConstResult res (p, acc) =
          case res of Constant {}                          -> Just res
                      Var v | identName v == identName p   -> Just acc
                            | identName v `HS.member` free -> Just res
                      _                                    -> Nothing

simplifyRotate :: LetSimplificationRule
-- A zero-rotation is identity.
simplifyRotate _ (Rotate _ 0 e _) =
  Just $ SubExp e

-- If asked to rotate a constant, just do it.
simplifyRotate _ (Rotate _ i (Constant val _) loc) =
  Just $ SubExp $ Constant (rotateArray i val) loc

simplifyRotate look (Rotate _ i (Var v) loc) = do
  bnd <- look $ identName v
  case bnd of
    -- Rotating a replicate is identity.
    Replicate {} ->
      Just $ SubExp $ Var v
    -- Rotating a constant, just do it.
    SubExp (Constant val _) ->
      Just $ SubExp $ Constant (rotateArray i val) loc
    _ ->
      Nothing

simplifyRotate _ _ = Nothing

simplifyBinOp :: LetSimplificationRule

simplifyBinOp _ (BinOp Plus e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _,  Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1+v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1+v2
      _ -> Nothing

simplifyBinOp _ (BinOp Minus e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1-v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1-v2
      _ -> Nothing

simplifyBinOp _ (BinOp Times e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1*v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1*v2
      _ -> Nothing

simplifyBinOp _ (BinOp Divide e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `div` v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1 / v2
      _ -> Nothing

simplifyBinOp _ (BinOp Mod e1 e2 _ pos) =
  case (e1, e2) of
    (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
      binOpRes pos $ IntVal $ v1 `mod` v2
    _ -> Nothing

simplifyBinOp _ (BinOp Pow e1 e2 _ pos)
  | isCt0 e2 =
    case subExpType e1 of
      Basic Int  -> binOpRes pos $ IntVal 1
      Basic Real -> binOpRes pos $ RealVal 1.0
      _          -> Nothing
  | isCt0 e1 || isCt1 e1 || isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 ^ v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ RealVal $ v1**v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftL e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftL` v2
      _ -> Nothing

simplifyBinOp _ (BinOp ShiftR e1 e2 _ pos)
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `shiftR` v2
      _ -> Nothing

simplifyBinOp _ (BinOp Band e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .&. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Bor e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e2
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 .|. v2
      _ -> Nothing

simplifyBinOp _ (BinOp Xor e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ IntVal $ v1 `xor` v2
      _ -> Nothing

simplifyBinOp _ (BinOp LogAnd e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e1
  | isCt0 e2 = Just $ SubExp e2
  | isCt1 e1 = Just $ SubExp e2
  | isCt1 e2 = Just $ SubExp e1
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 && v2
      _ -> Nothing

simplifyBinOp _ (BinOp LogOr e1 e2 _ pos)
  | isCt0 e1 = Just $ SubExp e2
  | isCt0 e2 = Just $ SubExp e1
  | isCt1 e1 = Just $ SubExp e1
  | isCt1 e2 = Just $ SubExp e2
  | otherwise =
    case (e1, e2) of
      (Constant (BasicVal (LogVal v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1 || v2
      _ -> Nothing

simplifyBinOp _ (BinOp Equal e1 e2 _ pos)
  | otherwise =
    case (e1, e2) of
      -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
      (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
        binOpRes pos $ LogVal $ v1==v2
      _ -> Nothing
  | e1 == e2 = binOpRes pos $ LogVal True

simplifyBinOp _ (BinOp Less e1 e2 _ pos) =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (BasicVal (IntVal v1)) _, Constant (BasicVal (IntVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<v2
    _ -> Nothing

simplifyBinOp _ (BinOp Leq e1 e2 _ pos) =
  case (e1, e2) of
    -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
    (Constant (BasicVal (IntVal  v1)) _, Constant (BasicVal (IntVal  v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (RealVal v1)) _, Constant (BasicVal (RealVal v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (LogVal  v1)) _, Constant (BasicVal (LogVal  v2)) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    (Constant (BasicVal (CharVal v1)) _, Constant (BasicVal (CharVal v2 )) _) ->
      binOpRes pos $ LogVal $ v1<=v2
    _ -> Nothing

simplifyBinOp _ _ = Nothing

binOpRes :: SrcLoc -> BasicValue -> Maybe Exp
binOpRes loc v = Just $ SubExp $ Constant (BasicVal v) loc

isCt1 :: SubExp -> Bool
isCt1 (Constant (BasicVal (IntVal x))  _) = x == 1
isCt1 (Constant (BasicVal (RealVal x)) _) = x == 1
isCt1 (Constant (BasicVal (LogVal x))  _) = x
isCt1 _                                   = False

isCt0 :: SubExp -> Bool
isCt0 (Constant (BasicVal (IntVal x))  _) = x == 0
isCt0 (Constant (BasicVal (RealVal x)) _) = x == 0
isCt0 (Constant (BasicVal (LogVal x))  _) = not x
isCt0 _                                   = False

simplifyNot :: LetSimplificationRule
simplifyNot _ (Not (Constant (BasicVal (LogVal v)) _) loc) =
  Just $ SubExp $ Constant (BasicVal $ LogVal (not v)) loc
simplifyNot _ _ = Nothing

simplifyNegate :: LetSimplificationRule
simplifyNegate _ (Negate (Constant (BasicVal (IntVal  v)) _) pos) =
  Just $ SubExp $ Constant (BasicVal $ IntVal (-v)) pos
simplifyNegate _ (Negate (Constant (BasicVal (RealVal  v)) _) pos) =
  Just $ SubExp $ Constant (BasicVal $ RealVal (0.0-v)) pos
simplifyNegate _ _ =
  Nothing

-- If expression is true then just replace assertion.
simplifyAssert :: LetSimplificationRule
simplifyAssert _ (Assert (Constant (BasicVal (LogVal True)) _) loc) =
  Just $ SubExp $ Constant (BasicVal Checked) loc
simplifyAssert _ _ =
  Nothing

simplifyConjoin :: LetSimplificationRule
simplifyConjoin look (Conjoin es loc) =
  -- Remove trivial certificates.
  let check seen (Constant (BasicVal Checked) _) = seen
      check seen (Var idd) =
        case look $ identName idd of
          Just (Conjoin es2 _) -> seen `S.union` S.fromList es2
          _                    -> Var idd `S.insert` seen
      check seen e = e `S.insert` seen
      origset = S.fromList es
      newset = foldl check S.empty es
      es' = S.toList newset
  in case es' of
       []                    -> Just $ SubExp $ Constant (BasicVal Checked) loc
       [c]                   -> Just $ SubExp c
       _ | origset /= newset -> Just $ Conjoin es' loc
         | otherwise         -> Nothing
simplifyConjoin _ _ = Nothing

simplifyIndexing :: LetSimplificationRule
simplifyIndexing look (Index cs idd inds loc) =
  case look $ identName idd of
    Nothing -> Nothing

    Just (SubExp (Constant v _))
      | Just iis <- ctIndex inds,
        length iis == length (valueShape v),
        Just el <- getArrValInd v iis -> Just $ SubExp $ Constant el loc

    Just (Iota _ _)
      | [ii] <- inds -> Just $ SubExp ii

    Just (Index cs2 aa ais _) ->
      Just $ Index (cs++cs2) aa (ais ++ inds) loc

    Just (e@ArrayLit {})
       | Just iis <- ctIndex inds,
         Just el <- getArrLitInd e iis -> Just el

    Just (Replicate _ (Var vv) _)
      | [_]   <- inds -> Just $ SubExp $ Var vv
      | _:is' <- inds -> Just $ Index cs vv is' loc

    Just (Replicate _ (Constant arr@(ArrayVal _ _) _) _)
       | _:is' <- inds,
         Just iis <- ctIndex is',
         Just el <- getArrValInd arr iis ->
           Just $ SubExp $ Constant el loc

    Just (Replicate _ val@(Constant _ _) _)
      | [_] <- inds -> Just $ SubExp val

    Just (Rearrange cs2 perm (Var src) _)
       | permuteReach perm < length inds ->
         let inds' = permuteShape (take (length inds) perm) inds
         in Just $ Index (cs++cs2) src inds' loc

    _ -> Nothing

simplifyIndexing _ _ = Nothing

ctIndex :: [SubExp] -> Maybe [Int]
ctIndex [] = Just []
ctIndex (Constant (BasicVal (IntVal ii)) _:is) =
  case ctIndex is of
    Nothing -> Nothing
    Just y  -> Just (ii:y)
ctIndex _ = Nothing

getArrValInd :: Value -> [Int] -> Maybe Value
getArrValInd v [] = Just v
getArrValInd (ArrayVal arr _) (i:is) = getArrValInd (arr ! i) is
getArrValInd _ _ = Nothing

getArrLitInd :: Exp -> [Int] -> Maybe Exp
getArrLitInd e [] = Just e
getArrLitInd (ArrayLit els _ _) (i:is) = getArrLitInd (SubExp $ els !! i) is
getArrLitInd (SubExp (Constant arr@(ArrayVal _ _) loc)) (i:is) =
  case getArrValInd arr (i:is) of
    Nothing -> Nothing
    Just v  -> Just $ SubExp $ Constant v loc
getArrLitInd _ _ = Nothing
