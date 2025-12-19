-- Utilities for using the Algebra layer from the IndexFn layer.
module Futhark.Analysis.Properties.AlgebraBridge.Util
  ( Answer (..),
    assume,
    addRelIterator,
    addRelSymbol,
    answerFromBool,
    ($<),
    ($<=),
    ($>),
    ($>=),
    ($==),
    ($/=),
    andF,
    andM,
    orM,
    allM,
    isUnknown,
    isYes,
    addRelShape,
    addRelDim,
    printAlgebra,
  )
where

import Control.Monad (forM_, (<=<))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Set qualified as S
import Futhark.Analysis.Properties.AlgebraBridge.Translate (getDisjoint, toAlgebra, addProperty_)
import Futhark.Analysis.Properties.AlgebraPC.Algebra qualified as Algebra
import Futhark.Analysis.Properties.IndexFn (Domain (..), Iterator, Quantified (..))
import Futhark.Analysis.Properties.IndexFnPlus (domainEnd, domainStart, intervalEnd)
import Futhark.Analysis.Properties.Monad
import Futhark.Analysis.Properties.Property (Property (..))
import Futhark.Analysis.Properties.Symbol (Symbol (..), toCNF, neg)
import Futhark.SoP.FourierMotzkin (($/=$), ($<$), ($<=$), ($==$), ($>$), ($>=$))
import Futhark.SoP.Refine (addRel, addRels)
import Futhark.SoP.SoP (Rel (..), SoP, int2SoP, sym2SoP, (.+.), (.-.))
import Futhark.Util.Pretty (Pretty (pretty), viaShow)

-- Fourer Motzkin Elimination solver may return True or False.
-- True means the query holds. False means "I don't know".
data Answer = Unknown | Yes
  deriving (Show, Eq, Ord)

instance Pretty Answer where
  pretty = viaShow

-- Short-circuit evaluation `and`. (Unless debugging is on.)
andF :: Answer -> IndexFnM Answer -> IndexFnM Answer
andF Yes m = m
andF Unknown _ = pure Unknown

andM :: IndexFnM Answer -> IndexFnM Answer -> IndexFnM Answer
andM m1 m2 = do
  ans <- m1
  ans `andF` m2

orM :: (Monad m) => m Answer -> m Answer -> m Answer
orM m1 m2 = do
  a1 <- m1
  case a1 of
    Yes -> pure Yes
    Unknown -> m2

allM :: [IndexFnM Answer] -> IndexFnM Answer
allM [] = pure Yes
allM xs = foldl1 andM xs

isYes :: Answer -> Bool
isYes Yes = True
isYes _ = False

isUnknown :: Answer -> Bool
isUnknown Unknown = True
isUnknown _ = False

assume :: Symbol -> IndexFnM ()
assume sym = do
  -- FIXME I'd like to have the below safety check, which is only really
  -- relevant for me accidentally calling assume on something non-bool.
  -- The source program has been type checked, so things that are in
  -- boolean places (such as predicates in cases) are really boolean.
  --
  -- Can't have it right now due to assume on c in E.If c t f, in Convert.hs.
  -- (Try to uncomment the below and run tests.)
  --
  -- booltype <- isBooleanM p
  -- unless booltype (error $ "Assume on non-boolean: " <> prettyString p)
  addRelSymbol sym
  assume_ sym
  where
    assume_ p = do
      addEq 1 p
      -- Add that pairwise disjoint symbols are false.
      mapM_ (addEq 0) =<< getDisjoint p
      case p of
        p1 :&& p2 -> do
          assume_ p1
          assume_ p2
          addEq 0 (neg p1)
          addEq 0 (neg p2)
        _ -> pure ()

    addEq n e = do
      x <- toAlgebra (sym2SoP e)
      addRel (x :==: int2SoP n)

-- | Adds a relation on symbols to the algebraic environment.
-- No-op if `p` is not a relation.
addRelSymbol :: Symbol -> IndexFnM ()
addRelSymbol (Bool _) = pure ()
addRelSymbol p = do
  rel <- toRel p
  maybe (pure ()) addRel rel
  props <- toProps p
  forM_ props addProperty_
  where
    toRel :: Symbol -> IndexFnM (Maybe (Rel Algebra.Symbol))
    toRel = runMaybeT . toRel_

    convOp transf op x y = do
      a <- transf x
      b <- transf y
      a `op` b

    liftOp op a b = pure $ a `op` b
    convCmp op = convOp (lift . toAlgebra) (liftOp op)

    toRel_ :: Symbol -> MaybeT IndexFnM (Rel Algebra.Symbol)
    toRel_ (x :< y) = convCmp (:<:) x y
    toRel_ (x :<= y) = convCmp (:<=:) x y
    toRel_ (x :> y) = convCmp (:>:) x y
    toRel_ (x :>= y) = convCmp (:>=:) x y
    toRel_ (x :== y) = convCmp (:==:) x y
    toRel_ (x :/= y) = do
      -- Inequality can only be expressed indirectly.
      gte <- lift $ x $>= y
      case gte of
        Yes -> toRel_ (x :> y)
        _ -> do
          lte <- lift $ x $<= y
          case lte of
            Yes -> toRel_ (x :< y)
            _ -> fail ""
    toRel_ (x :&& y) = convOp toRel_ (liftOp (:&&:)) x y
    toRel_ (_ :|| _) = fail "toRel on :||" -- convOp toRel_ (liftOp (:||:)) x y
    toRel_ _ = fail ""

    -- Convert to CNF, then get all conjuncts that are properties.
    -- (Any properties nested inside disjunctions are ignored.)
    toProps :: Symbol -> IndexFnM [Property Algebra.Symbol]
    toProps sym = mapM toAlgebra (getProps $ toCNF sym)
      where
        getProps :: Symbol -> [Property Symbol]
        getProps (Assume prop) = getProps prop
        getProps (Prop prop) = [prop]
        getProps (a :&& b) = getProps a <> getProps b
        getProps _ = []

-- | Add relations derived from the iterator to the algebraic environment.
addRelShape :: [[Iterator]] -> IndexFnM ()
addRelShape = mapM_ addRelIterator . mconcat

addRelDim :: [Iterator] -> IndexFnM ()
addRelDim = mapM_ addRelIterator

addRelIterator :: Iterator -> IndexFnM ()
addRelIterator (Forall i dom) = case dom of
  Iota n -> do
    n' <- toAlgebra n
    dom_end <- Algebra.simplify =<< toAlgebra (domainEnd dom)
    addRels $
      S.fromList
        [ int2SoP 1 :<=: n',
          sVar i :<=: dom_end
        ]
    -- TODO why do I have to add the range in two steps?
    -- Want to enforce range on i without causing cycles---not move lb/ub around
    -- using heuristics.
    addRels $ S.fromList [int2SoP 0 :<=: sVar i]
  Cat k m b -> do
    m' <- toAlgebra m
    dom_start <- Algebra.simplify =<< toAlgebra (domainStart dom)
    dom_end <- Algebra.simplify =<< toAlgebra (domainEnd dom)
    interval_start <- Algebra.simplify =<< toAlgebra b
    interval_end <- Algebra.simplify =<< toAlgebra (intervalEnd dom)
    addRels $
      S.fromList
        [ interval_start :<=: sVar i,
          sVar i :<=: interval_end,
          int2SoP 1 :<=: m',
          int2SoP 0 :<=: sVar k,
          sVar k :<=: m' .-. int2SoP 1
        ]
    addRels $
      S.fromList
        [ dom_start :<=: sVar i,
          sVar i :<=: dom_end
        ]
    interval_size <-
      Algebra.simplify $ (interval_end .+. int2SoP 1) .-. interval_start
    addRel (interval_size :>: int2SoP 0)
  where
    sVar = sym2SoP . Algebra.Var

answerFromBool :: Bool -> Answer
answerFromBool True = Yes
answerFromBool False = Unknown

convFME :: (SoP Algebra.Symbol -> SoP Algebra.Symbol -> IndexFnM Bool) -> SoP Symbol -> SoP Symbol -> IndexFnM Answer
convFME op x y = rollbackAlgEnv $ do
  a <- toAlgebra x
  b <- toAlgebra y
  ans <- a `op` b
  pure $ if ans then Yes else Unknown

($<) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($<) = convFME ($<$)

($<=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($<=) = convFME ($<=$)

($>) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($>) = convFME ($>$)

($>=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($>=) = convFME ($>=$)

($==) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($==) = convFME ($==$)

($/=) :: SoP Symbol -> SoP Symbol -> IndexFnM Answer
($/=) = convFME ($/=$)

infixr 4 $<

infixr 4 $<=

infixr 4 $>

infixr 4 $>=

infixr 4 $==

infixr 4 $/=

printAlgebra :: Int -> Symbol -> IndexFnM ()
printAlgebra level = printM level <=< printAlg

printAlg :: Symbol -> IndexFnM String
printAlg (a :&& b) = (\x y -> x <> " ^ " <> y) <$> printAlg a <*> printAlg b
printAlg (a :|| b) = (\x y -> x <> " v " <> y) <$> printAlg a <*> printAlg b
printAlg (a :< b) = (\x y -> x <> " < " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg (a :> b) = (\x y -> x <> " > " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg (a :<= b) = (\x y -> x <> " <= " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg (a :>= b) = (\x y -> x <> " >= " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg (a :== b) = (\x y -> x <> " == " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg (a :/= b) = (\x y -> x <> " /= " <> y) <$> toStrAlg a <*> toStrAlg b
printAlg e = toStrAlg (sym2SoP e)

toStrAlg :: SoP Symbol -> IndexFnM String
toStrAlg e = do
  alg_e :: SoP Algebra.Symbol <- toAlgebra e
  pure $ prettyStr alg_e
