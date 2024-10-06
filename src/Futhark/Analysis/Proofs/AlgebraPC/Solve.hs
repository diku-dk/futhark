module Futhark.Analysis.Proofs.AlgebraPC.Solve
  ( simplify,
    simplifyLevel,
    findSymbol,
  )
where

-- import Data.Maybe
-- import Data.Foldable
-- import Control.Monad
import qualified Data.Set as S
import qualified Data.MultiSet as MS
-- import Futhark.SoP.Monad
import Futhark.SoP.Expression
import Futhark.SoP.SoP
import Futhark.Analysis.Proofs.AlgebraPC.Algebra
import Futhark.Analysis.Proofs.AlgebraPC.All2AllDriver
import Futhark.Analysis.Proofs.AlgebraPC.UnaryRules
-- import Language.Futhark (VName)
import qualified Futhark.SoP.FourierMotzkin as FM
import Futhark.SoP.Monad
import Futhark.Analysis.Proofs.Traversals (ASTMapper (..), astMap)
-- import Futhark.SoP.Monad (AlgEnv (..), MonadSoP (..), Nameable (mkName))

simplify :: (Expression e, Ord e) =>
  SoP Symbol -> AlgM e (SoP Symbol)
simplify = astMap m
  where
    m =
      ASTMapper
        { mapOnSymbol = pure,
          mapOnSoP = simplifyLevel
        }

simplifyLevel :: (Expression e, Ord e) => SoP Symbol -> AlgM e (SoP Symbol)
simplifyLevel sop0 = do
  let fvs = free sop0
  -- pow simplifications
  sop1 <-
    if S.null $ S.filter hasPow fvs
    then pure sop0
    else simplifyPows simplifyLevel sop0
  -- index & sum-expansion & sum-sum simplifications
  (s2, sop2) <-
    if S.null $ S.filter hasIdxOrSum fvs
    then pure (False, sop1)
    else simplifyAll2All sop1
  -- peel off known indices by looking in the equality table
  equivs <- getEquivs
  let (s3, sop3) = peelOffSumsFP equivs sop2
  -- do we need to run to a fix point ?
  if s2 || s3 then simplifyLevel sop3 else pure sop3

---------------------------------------------------
--- Some Thinking but the code below is garbage ---
---------------------------------------------------

-- | finds a symbol
findSymbolLEq0 :: (Expression e, Ord e) =>
    SoP Symbol -> AlgM e (SoP Symbol, Maybe (Symbol, Range Symbol))
findSymbolLEq0 sop = do
  sop' <- simplify sop
  sop''<- if S.null $ S.filter hasPow (free sop')
          then pure sop'
          else powEquiv sop'
  pure (sop'', Nothing)

powEquiv :: (Expression e, Ord e) => SoP Symbol -> AlgM e (SoP Symbol)
powEquiv sop
  -- Single term: a sufficient condition for `t * 2^expo <= 0`
  --              is `t <= 0`, since 2^expo is always >= 1
  | Just (Term mset, k) <- justSingleTerm sop,
    (mset_pows, mset_others) <- MS.partition hasPow mset,
    length (MS.toOccurList mset_pows) >= 1 =
  pure $ term2SoP (Term mset_others) k
  -- Two terms involving same pow with different exponents.
  -- Sufficient conditions for
  --   `t * 2^e1 - t * 2^e2 <= 0`, which is equivalent with:
  --   `t * 2^e1 <= t * 2^e2`
  -- are: 
  --    (t >= 0) && (e1 <= e2) || (t <= 0 && (e2 <= e1))  
  | [(Term ms1, k1), (Term ms2, k2)] <- sopToList sop,
    (mset1_pow, mset1_others) <- MS.partition hasPow ms1,
    (mset2_pow, mset2_others) <- MS.partition hasPow ms2,
    [(pow1, m1)] <- MS.toOccurList mset1_pow,
    [(pow2, m2)] <- MS.toOccurList mset2_pow,
    (Pow (b1,expo1), Pow (b2, expo2)) <- (pow1, pow2),
    b1 == b2 && k1 == 0-k2 && m1 == m2 && mset1_others == mset2_others = do
  let (e1, e2, k) = if k1 > 0 then (expo1, expo2, k1) else (expo2, expo1, k2)
      t_sop = term2SoP (Term mset1_others) k
  t_gt0 <- (int2SoP 0) FM.$<=$ t_sop
  if t_gt0
  then simplify $ e1 .-. e2
  else do
       t_lt0 <- (int2SoP 0) FM.$>=$ t_sop
       if t_lt0
       then simplify $ e2 .-. e1
       else pure sop
-- need cases for two terms where only one is a constant.
powEquiv sop = pure sop
    

-- | this function is inteneded to do most of the work:
--   1. it simplifies the input SoP based on high-level rules
--   2. it finds a symbol to eleminate (via Fourier Motzking)
--      and it adjusts the environment.
--   3. it returns the symbol to eliminate, together with its
--      range, new/simplified SoP, and the new environment 
findSymbol :: SoP Symbol -> 
              AlgM e (SoP Symbol, Maybe (Symbol, Range Symbol))
findSymbol sop
  | fvs <- free sop,
    (s:_) <- S.toList fvs = do
  let r = Range { lowerBound = S.singleton zeroSoP
                , rangeMult = 1
                , upperBound = S.singleton (int2SoP 1)
                }
  pure (sop, Just (s, r))
-- the default case conservatively fails
findSymbol sop =
  pure (sop, Nothing)



{--
PLAN:

1. find `syms` the symbols appearing in SoP
2. investigate the opportunities for applying the simplification
   rules that do not lose precision:
     -- normalizing sums of slices
     -- pealing the first/last symbol (of known range) from a sum slice
3. invesitgate the opportunities for creating new ranges from properties
     such as sum-slices or monotonicity. 
   3.1 For example, if we have a symbol such as `Sum(a[ind_1: ind_2])` then
   	    this can be replaced with a new VName symbol `SA_slc` with bounds:
   	        (a) In case one can prove `ind_1 > ind_2`:
		 		{ LB = 0, UB = 0 }
		 	(b) In case one can prove `ind_1 <= ind_2
		 		{ LB = (ind_2 - ind_1) * lowerBound(a)
		 		, UB = (ind_2 - ind_1) * upperBound(a)
		 		}
   3.2 For example, if `a` is known to be strictly increasing monotonically,
   	    and `ind_1 > ind_2` is provable, then SoP:
     		`t1 * a[ind_1] - t1 * a[ind_2] + r`
     	can be transformed to:
     		`t1 * a_diff + r`
     	where the range of `a_diff` is:
        	{ LB = ind_1 - ind_2, UB = upperBound(a) - lowerBound(a) }
4. choose the most-dependent variable to eliminate according to the range table,
   i.e., here it should be safe to use the generic functionality of SoP
   (`transClosInRanges`)
--}


-- f :: (SoP Symbol >= 0) -> AlgM e Bool
-- f sop = do
--   modifyEnv $ undefined
--   undefined

-- runF :: (SoP Symbol >= 0) -> AlgEnv Symbol e Property -> VNameSource -> (Bool, VEnv e)
-- runF sop env vns= runAlgM (f sop) env vns

-- rules :: RuleBook (SoP Symbol) Symbol (AlgM e)
-- rules = []

-- (&<) ::  SoP Symbol -> SoP Symbol ->  AlgM e Bool
-- sop1 &< sop2 = do
--  prop  <- getProperties
--  sop1 F.$<$ sop2 -- fourier mo

