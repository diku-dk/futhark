{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Futhark.Analysis.AlgSimplify
  ( ScalExp
  , Error
  , simplify
  , mkSuffConds
  , RangesRep
  , ppRangesRep
  , linFormScalE
  , pickSymToElim
  )
  where

import Control.Applicative

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid

import Prelude

import Futhark.Representation.AST hiding (SDiv, SMod, SQuot, SRem, SSignum)
import Futhark.Analysis.ScalExp
import qualified Futhark.Representation.Primitive as P

-- | Ranges are inclusive.
type RangesRep = M.Map VName (Int, Maybe ScalExp, Maybe ScalExp)

-- | Prettyprint a 'RangesRep'.  Do not rely on the format of this
-- string.  Does not include the loop nesting depth information.
ppRangesRep :: RangesRep -> String
ppRangesRep = unlines . sort . map ppRange . M.toList
  where ppRange (name, (_, lower, upper)) =
          pretty name ++ ": " ++
          if lower == upper
          then "== " ++ ppBound lower
          else "[" ++ ppBound lower ++ ", " ++
               ppBound upper ++ "]"
        ppBound Nothing = "?"
        ppBound (Just se) = pretty se

-- | environment recording the position and
--   a list of variable-to-range bindings.
data AlgSimplifyEnv = AlgSimplifyEnv { inSolveLTH0 :: Bool
                                     , ranges :: RangesRep
                                     , maxSteps :: Int
                                     -- ^ The number of
                                     -- simplifications to do before
                                     -- bailing out, to avoid spending
                                     -- too much time.
                                     }

data Error = StepsExceeded | Error String

type AlgSimplifyM = StateT Int (ReaderT AlgSimplifyEnv (Either Error))

runAlgSimplifier :: Bool -> AlgSimplifyM a -> RangesRep -> Either Error a
runAlgSimplifier s x r = runReaderT (evalStateT x 0) env
  where env = AlgSimplifyEnv { inSolveLTH0 = s
                             , ranges = r
                             , maxSteps = 100 -- heuristically chosen
                             }

step :: AlgSimplifyM ()
step = do modify (1+)
          exceeded <- pure (>) <*> get <*> asks maxSteps
          when exceeded stepsExceeded

stepsExceeded :: AlgSimplifyM a
stepsExceeded = lift $ lift $ Left StepsExceeded

badAlgSimplifyM :: String -> AlgSimplifyM a
badAlgSimplifyM = lift . lift . Left . Error

-- | Binds an array name to the set of used-array vars
markInSolve :: AlgSimplifyEnv -> AlgSimplifyEnv
markInSolve env =
  env { inSolveLTH0 = True }

markGaussLTH0 :: AlgSimplifyM a -> AlgSimplifyM a
markGaussLTH0 = local markInSolve

-----------------------------------------------------------
-- A Scalar Expression, i.e., ScalExp, is simplified to: --
--   1. if numeric: to a normalized sum-of-products form,--
--      in which on the outermost level there are N-ary  --
--      Min/Max nodes, and the next two levels are a sum --
--      of products.                                     --
--   2. if boolean: to disjunctive normal form           --
--                                                       --
-- Corresponding Helper Representations are:             --
--   1. NNumExp, i.e., NSum of NProd of ScalExp          --
--   2. DNF                                              --
-----------------------------------------------------------

data NNumExp = NSum   [NNumExp]  PrimType
             | NProd  [ScalExp]  PrimType
               deriving (Eq, Ord, Show)

data BTerm   = NRelExp RelOp0 NNumExp
             | LogCt  !Bool
             | PosId   VName
             | NegId   VName
               deriving (Eq, Ord, Show)
type NAnd    = [BTerm]
type DNF     = [NAnd ]
--type NOr     = [BTerm]
--type CNF     = [NOr  ]

-- | Applies Simplification at Expression level:
simplify :: ScalExp -> RangesRep -> ScalExp
simplify e rangesrep = case runAlgSimplifier False (simplifyScal e) rangesrep of
  Left (Error err) ->
    error $ "Error during algebraic simplification of: " ++ pretty e ++
    "\n"  ++ err
  Left StepsExceeded -> e
  Right e' -> e'

-- | Given a symbol i and a scalar expression e, it decomposes
--   e = a*i + b and returns (a,b) if possible, otherwise Nothing.
linFormScalE :: VName -> ScalExp -> RangesRep -> Either Error (Maybe (ScalExp,ScalExp))
linFormScalE i e = runAlgSimplifier False (linearFormScalExp i e)

-- | Extracts sufficient conditions for a LTH0 relation to hold
mkSuffConds :: ScalExp -> RangesRep -> Either Error [[ScalExp]]
mkSuffConds e = runAlgSimplifier True (gaussElimRel e)

{-
-- | Test if Simplification engine can handle this kind of expression
canSimplify :: Int -> Either Error ScalExp --[[ScalExp]]
canSimplify i = do
    let (h,_,e2) = mkRelExp i
    case e2 of
        (RelExp LTH0 _) -> do
              -- let e1' = trace (pretty e1) e1
              simplify e2 noLoc h
--            runAlgSimplifier False (gaussAllLTH0 False S.empty =<< toNumSofP =<< simplifyScal e) noLoc h
        _ -> simplify e2 noLoc h-- badAlgSimplifyM "canSimplify: unimplemented!"
-}
-------------------------------------------------------
--- Assumes the relational expression is simplified  --
--- All uses of gaussiam elimination from simplify   --
---  must use simplifyNRel, which calls markGaussLTH0--
---  to set the inSolveLTH0 environment var, so that --
---  we do not enter an infinite recurssion!         --
--- Returns True or False or the input replation,i.e.--
---    static-only simplification!                   --
-------------------------------------------------------
simplifyNRel :: BTerm -> AlgSimplifyM BTerm
simplifyNRel inp_term@(NRelExp LTH0 inp_sofp) = do
    term <- cheapSimplifyNRel inp_term
    in_gauss <- asks inSolveLTH0
    let tp = typeOfNAlg inp_sofp

    if in_gauss || isTrivialNRel term || tp `notElem` map IntType allIntTypes
    then return term
    else do ednf <- markGaussLTH0 $ gaussAllLTH0 True S.empty inp_sofp
            return $ case ednf of
              Val (BoolValue c) -> LogCt c
              _              -> term
    where
        isTrivialNRel (NRelExp _ (NProd [Val _] _)) = True
        isTrivialNRel NRelExp{}                     = False
        isTrivialNRel  _                            = False

        cheapSimplifyNRel :: BTerm -> AlgSimplifyM BTerm
        cheapSimplifyNRel (NRelExp rel (NProd [Val v] _)) = do
            succ' <- valLTHEQ0 rel v; return $ LogCt succ'
        cheapSimplifyNRel e = return e
simplifyNRel inp_term =
    return inp_term --- TODO: handle more cases.

--gaussEliminateNRel :: BTerm -> AlgSimplifyM DNF
--gaussEliminateNRel _ =
--    badAlgSimplifyM "gaussElimNRel: unimplemented!"


gaussElimRel :: ScalExp -> AlgSimplifyM [[ScalExp]] -- ScalExp
gaussElimRel (RelExp LTH0 e) = do
    e_sofp <- toNumSofP =<< simplifyScal e
    e_scal<- simplifyScal =<< gaussAllLTH0 False S.empty e_sofp
    e_dnf <- toDNF e_scal
    mapM (mapM (\f ->
                  case f of
                    LogCt c   -> return $ Val (BoolValue c)
                    PosId i   -> return $ Id  i $ scalExpType e
                    NegId i   -> return $ Id  i $ scalExpType e
                    NRelExp rel ee -> do
                      e_scal' <- fromNumSofP ee
                      return $ RelExp rel e_scal'
               )) e_dnf

gaussElimRel _ =
    badAlgSimplifyM "gaussElimRel: only LTH0 Int relations please!"

--ppSyms :: S.Set VName -> String
--ppSyms ss = foldl (\s x -> s ++ " " ++ (baseString x)) "ElimSyms: " (S.toList ss)


primScalExpLTH0 :: ScalExp -> Bool
primScalExpLTH0 (Val (IntValue v)) = P.intToInt64 v < 0
primScalExpLTH0 _ = False
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
---`gaussAllLTH'                                        ---
---  `static_only':whether only a True/False answer is  ---
---                 required or actual a sufficient cond---
---     `el_syms': the list of already eliminated       ---
---                 symbols, initialy empty             ---
---     `sofp':    the expression e in sum-of-product   ---
---                 form that is compared to 0,         ---
---                 i.e., e < 0. sofp assumed simplified---
---     Result:    is a ScalExp expression, which is    ---
---                 actually a predicate in DNF form,   ---
---                 that is a sufficient condition      ---
---                 for e < 0!                          ---
---                                                     ---
--- gaussAllLTH0 is implementing the tracking of Min/Max---
---              terms, and uses `gaussOneDefaultLTH0'  ---
---              to implement gaussian-like elimination ---
---              to solve the a*i + b < 0 problem.      ---
---                                                     ---
--- IMPORTANT: before calling gaussAllLTH0 from outside ---
---            make sure to set insideSolveLTH0 env     ---
---            member to True, via markGaussLTH0;       ---
---            otherwise infinite recursion might happen---
---            w.r.t. `simplifyScal'                    ---
-----------------------------------------------------------
-----------------------------------------------------------
-----------------------------------------------------------
type Prod = [ScalExp]
gaussAllLTH0 :: Bool -> S.Set VName -> NNumExp -> AlgSimplifyM ScalExp
gaussAllLTH0 static_only el_syms sofp = do
    step
    let tp  = typeOfNAlg sofp
    rangesrep <- asks ranges
    e_scal <- fromNumSofP sofp
    let mi  = pickSymToElim rangesrep el_syms e_scal

    case mi of
      Nothing -> return $ if primScalExpLTH0 e_scal
                          then Val (BoolValue True)
                          else RelExp LTH0 e_scal
      Just i  -> do
        (jmm, fs0, terms) <- findMinMaxTerm i sofp
        -- i.e., sofp == fs0 * jmm + terms, where
        --       i appears in jmm and jmm = MinMax ...

        fs <- if not (null fs0) then return fs0
              else do one <- getPos1 tp; return [Val one]

        case jmm of
          ------------------------------------------------------------------------
          -- A MinMax expression which uses to-be-eliminated symbol i was found --
          ------------------------------------------------------------------------
          Just (MaxMin _     []  ) ->
                badAlgSimplifyM "gaussAllLTH0: Empty MinMax Node!"
          Just (MaxMin ismin mmts) -> do
            mone <- getNeg1 tp

            -- fs_lth0 => fs < 0
--            fs_lth0 <- if null fs then return $ Val (BoolValue False)
--                       else gaussAllLTH0 static_only el_syms (NProd fs tp)
            fsm1    <- toNumSofP =<< simplifyScal =<< fromNumSofP
                         ( NSum [NProd fs tp, NProd [Val mone] tp] tp )
            fs_leq0 <- gaussAllLTH0 static_only el_syms fsm1  -- fs <= 0
            -- mfsm1 = - fs - 1, fs_geq0 => (fs >= 0),
            --             i.e., fs_geq0 => (-fs - 1 < 0)
            mfsm1   <- toNumSofP =<< simplifyScal =<< fromNumSofP
                         ( NSum [NProd (Val mone:fs) tp,NProd [Val mone] tp] tp )
            fs_geq0 <- gaussAllLTH0 static_only el_syms mfsm1

            -- mm_terms are the simplified terms of the MinMax obtained
            -- after inlining everything inside the MinMax, i.e., intially
            -- terms + fs * MinMax ismin [t1,..,tn] -> [fs*t1+terms, ..., fs*tn+terms]
            mm_terms<- mapM (\t -> toNumSofP =<< simplifyScal =<< fromNumSofP
                                   (NSum ( NProd (t:fs) tp:terms ) tp) ) mmts

            -- for every (simplified) `term_i' of the inline MinMax exp,
            --  get the sufficient conditions for `term_i < 0'
            mms     <- mapM (gaussAllLTH0 static_only el_syms) mm_terms

            if static_only
            --------------------------------------------------------------------
            -- returns either Val (BoolValue True) or the original ScalExp relat --
            --------------------------------------------------------------------
            then if ( fs_geq0 == Val (BoolValue True) &&     ismin) ||
                    ( fs_leq0 == Val (BoolValue True) && not ismin)
                 -- at least one term should be < 0!
                 then do let  is_one_true  = Val (BoolValue True ) `elem` mms
                         let are_all_false = all  (== Val (BoolValue False)) mms
                         return $ if       is_one_true  then Val (BoolValue True)
                                  else if are_all_false then Val (BoolValue False)
                                  else RelExp LTH0 e_scal
                 -- otherwise all terms should be all true!
                 else do let are_all_true = all  (== Val (BoolValue True )) mms
                         let is_one_false = Val (BoolValue False) `elem` mms
                         return $ if      are_all_true then Val (BoolValue True )
                                  else if is_one_false then Val (BoolValue False)
                                  else RelExp LTH0 e_scal
            --------------------------------------------------------------------
            -- returns sufficient conditions for the ScalExp relation to hold --
            --------------------------------------------------------------------
            else do
                let mm_fsgeq0 = foldl (if ismin then SLogOr else SLogAnd)
                                      (Val (BoolValue (not ismin))) mms
                let mm_fslth0 = foldl (if ismin then SLogAnd else SLogOr)
                                      (Val (BoolValue      ismin )) mms
                -- the sufficient condition for the original expression, e.g.,
                -- terms + fs * Min [t1,..,tn] < 0 is
                -- (fs >= 0 && (fs*t_1+terms < 0 || ... || fs*t_n+terms < 0) ) ||
                -- (fs <  0 && (fs*t_1+terms < 0 && ... && fs*t_n+terms < 0) )
                return $ SLogOr (SLogAnd fs_geq0 mm_fsgeq0) (SLogAnd fs_leq0 mm_fslth0)

          Just _ -> badAlgSimplifyM "gaussOneLTH0: (Just MinMax) invariant violated!"
          ------------------------------------------------------------------------
          -- A MinMax expression which uses (to-be-elim) symbol i was NOT found --
          ------------------------------------------------------------------------
          Nothing-> do
            m_sofp <- gaussOneDefaultLTH0 static_only i el_syms sofp
            case m_sofp of
                Nothing -> gaussAllLTH0 static_only (S.insert i el_syms) sofp
                Just res_eofp -> return res_eofp
    where
        findMinMaxTerm :: VName -> NNumExp -> AlgSimplifyM (Maybe ScalExp, Prod, [NNumExp])
        findMinMaxTerm _  (NSum  [] _) = return (Nothing, [], [])
        findMinMaxTerm _  (NSum  [NProd [MaxMin ismin e] _] _) =
            return (Just (MaxMin ismin e), [], [])
        findMinMaxTerm _  (NProd [MaxMin ismin e] _) =
            return (Just (MaxMin ismin e), [], [])

        findMinMaxTerm ii t@NProd{} = do (mm, fs) <- findMinMaxFact ii t
                                         return (mm, fs, [])
        findMinMaxTerm ii (NSum (t:ts) tp)= do
            rangesrep <- asks ranges
            case M.lookup ii rangesrep of
                Just (_, Just _, Just _) -> do
                    f <- findMinMaxFact ii t
                    case f of
                        (Just mm, fs) -> return (Just mm, fs, ts)
                        (Nothing, _ ) -> do (mm, fs', ts') <- findMinMaxTerm ii (NSum ts tp)
                                            return (mm, fs', t:ts')
                _ -> return (Nothing, [], t:ts)

        findMinMaxFact :: VName -> NNumExp -> AlgSimplifyM (Maybe ScalExp, Prod)
        findMinMaxFact _  (NProd []     _ ) = return (Nothing, [])
        findMinMaxFact ii (NProd (f:fs) tp) =
            case f of
                MaxMin ismin ts -> do
                        let id_set = mconcat $ map freeIn ts
                        if S.member ii id_set
                        then return (Just (MaxMin ismin ts), fs)
                        else do (mm, fs') <- findMinMaxFact ii (NProd fs tp)
                                return (mm, f:fs')

                _ -> do (mm, fs') <- findMinMaxFact ii (NProd fs tp)
                        return (mm, f:fs')
        findMinMaxFact ii (NSum [f] _) = findMinMaxFact ii f
        findMinMaxFact _  (NSum _ _) =
            badAlgSimplifyM "findMinMaxFact: NSum argument illegal!"



gaussOneDefaultLTH0 :: Bool -> VName -> S.Set VName -> NNumExp -> AlgSimplifyM (Maybe ScalExp)
gaussOneDefaultLTH0  static_only i elsyms e = do
    aipb <- linearForm i e
    case aipb of
        Nothing     -> return Nothing
        Just (a, b) -> do
            rangesrep <- asks ranges
            one    <- getPos1 (typeOfNAlg e)
            ascal  <- fromNumSofP a
            mam1   <- toNumSofP =<< simplifyScal (SNeg (SPlus ascal (Val one)))
            am1    <- toNumSofP =<< simplifyScal (SMinus ascal (Val one))
            ma     <- toNumSofP =<< simplifyScal (SNeg ascal)

            b_scal<- fromNumSofP b
            mbm1  <- toNumSofP =<< simplifyScal (SNeg (SPlus b_scal (Val one)))

            aleq0 <- simplifyScal =<< gaussAllLTH0 static_only elsyms am1
            ageq0 <- simplifyScal =<< gaussAllLTH0 static_only elsyms mam1

            case M.lookup i rangesrep of
                Nothing ->
                    badAlgSimplifyM "gaussOneDefaultLTH0: sym not in ranges!"
                Just (_, Nothing, Nothing) ->
                    badAlgSimplifyM "gaussOneDefaultLTH0: both bounds are undefined!"

                -- only the lower-bound of i is known!
                Just (_, Just lb, Nothing) -> do
                    alpblth0 <- gaussElimHalf static_only elsyms lb a b
                    and_half <- simplifyScal alpblth0
                    case (and_half, aleq0) of
                        (Val (BoolValue True), Val (BoolValue True)) ->
                                return $ Just and_half
                        _ -> do malmbm1lth0 <- gaussElimHalf static_only elsyms lb ma mbm1
                                other_half  <- simplifyScal malmbm1lth0
                                case (other_half, ageq0) of
                                    (Val (BoolValue True), Val (BoolValue True)) ->
                                            return $ Just (Val (BoolValue False))
                                    _  ->   return Nothing

                Just (_, Nothing, Just ub) -> do
                    aupblth0 <- gaussElimHalf static_only elsyms ub a b
                    and_half <- simplifyScal aupblth0
                    case (and_half, ageq0) of
                        (Val (BoolValue True), Val (BoolValue True)) ->
                                return $ Just and_half
                        _ -> do
                                maumbm1    <- gaussElimHalf static_only elsyms ub ma mbm1
                                other_half <- simplifyScal maumbm1
                                case (other_half, aleq0) of
                                    (Val (BoolValue True), Val (BoolValue True)) ->
                                            return $ Just (Val (BoolValue False))
                                    _  ->   return Nothing

                Just (_, Just lb, Just ub) ->
                    if static_only
                    then if aleq0 == Val (BoolValue True)
                         then do alpblth0 <- simplifyScal =<< gaussElimHalf static_only elsyms lb a b
                                 if alpblth0 == Val (BoolValue True)
                                   then return $ Just (Val (BoolValue True))
                                   else do maubmbm1 <- simplifyScal =<< gaussElimHalf static_only elsyms ub ma mbm1
                                           return $ if maubmbm1 == Val (BoolValue True)
                                                    then Just (Val (BoolValue False))
                                                    else Nothing
                      else if ageq0 == Val (BoolValue True)
                      then do aupblth0 <- simplifyScal =<< gaussElimHalf static_only elsyms ub a b
                              if aupblth0 == Val (BoolValue True)
                              then return $ Just (Val (BoolValue True))
                              else do malbmbm1 <- simplifyScal =<< gaussElimHalf static_only elsyms lb ma mbm1
                                      return $ if malbmbm1 == Val (BoolValue True)
                                               then Just (Val (BoolValue False))
                                               else Nothing
                      else return Nothing
                    else do
                      alpblth0 <- gaussElimHalf static_only elsyms lb a b
                      aupblth0 <- gaussElimHalf static_only elsyms ub a b
                      res <- simplifyScal $ SLogOr (SLogAnd aleq0 alpblth0) (SLogAnd ageq0 aupblth0)
                      return $ Just res

    where
        gaussElimHalf :: Bool -> S.Set VName -> ScalExp -> NNumExp -> NNumExp -> AlgSimplifyM ScalExp
        gaussElimHalf only_static elsyms0 q a b = do
            a_scal <- fromNumSofP a
            b_scal <- fromNumSofP b
            e_num_scal <- simplifyScal (SPlus (STimes a_scal q) b_scal)
            e_num <- toNumSofP e_num_scal
            gaussAllLTH0 only_static elsyms0 e_num

--    pos <- asks pos
--    badAlgSimplifyM "gaussOneDefaultLTH0: unimplemented!"

----------------------------------------------------------
--- Pick a Symbol to Eliminate & Bring To Linear Form  ---
----------------------------------------------------------

pickSymToElim :: RangesRep -> S.Set VName -> ScalExp -> Maybe VName
pickSymToElim rangesrep elsyms0 e_scal =
--    ranges <- asks ranges
--    e_scal <- fromNumSofP e0
    let ids0= S.toList $ freeIn e_scal
        ids1= filter (\s -> not (S.member s elsyms0)) ids0
        ids2= filter (\s -> case M.lookup s rangesrep of
                                Nothing -> False
                                Just _  -> True
                     ) ids1
        ids = sortBy (\n1 n2 -> let n1p = M.lookup n1 rangesrep
                                    n2p = M.lookup n2 rangesrep
                                in case (n1p, n2p) of
                                     (Just (p1,_,_), Just (p2,_,_)) -> compare (-p1) (-p2)
                                     (_            , _            ) -> compare (1::Int) (1::Int)
                     ) ids2
    in  case ids of
            []  -> Nothing
            v:_ -> Just v


linearFormScalExp :: VName -> ScalExp -> AlgSimplifyM (Maybe (ScalExp, ScalExp))
linearFormScalExp sym scl_exp = do
    sofp <- toNumSofP =<< simplifyScal scl_exp
    ab   <- linearForm sym sofp
    case ab of
        Just (a_sofp, b_sofp) -> do
            a <- fromNumSofP a_sofp
            b <- fromNumSofP b_sofp
            a'<- simplifyScal a
            b'<- simplifyScal b
            return $ Just (a', b')
        Nothing ->
            return Nothing

linearForm :: VName -> NNumExp -> AlgSimplifyM (Maybe (NNumExp, NNumExp))
linearForm _ (NProd [] _) =
    badAlgSimplifyM "linearForm: empty Prod!"
linearForm idd ee@NProd{} = linearForm idd (NSum [ee] (typeOfNAlg ee))
linearForm _ (NSum [] _) =
    badAlgSimplifyM "linearForm: empty Sum!"
linearForm idd (NSum terms tp) = do
    terms_d_idd <- mapM  (\t -> do t0 <- case t of
                                            NProd (_:_) _ -> return t
                                            _ -> badAlgSimplifyM "linearForm: ILLEGAL111!!!!"
                                   t_scal <- fromNumSofP t0
                                   simplifyScal $ SDiv t_scal (Id idd (scalExpType t_scal))
                         ) terms
    let myiota  = [1..(length terms)]
    let ia_terms= filter (\(_,t)-> case t of
                                     SDiv _ _ -> False
                                     _           -> True
                         ) (zip myiota terms_d_idd)
    let (a_inds, a_terms) = unzip ia_terms

    let (_, b_terms) = unzip $ filter (\(iii,_) -> iii `notElem` a_inds)
                                      (zip myiota terms)
    -- check that b_terms do not contain idd
    b_succ <- foldM (\acc x ->
                        case x of
                           NProd fs _ -> do let fs_scal = case fs of
                                                            [] -> Val $ IntValue $ Int32Value 1
                                                            f:fs' -> foldl STimes f fs'
                                            let b_ids = freeIn fs_scal
                                            return $ acc && not (idd `S.member` b_ids)
                           _          -> badAlgSimplifyM "linearForm: ILLEGAL222!!!!"
                    ) True b_terms

    case a_terms of
        t:ts | b_succ -> do
            let a_scal = foldl SPlus t ts
            a_terms_sofp <- toNumSofP =<< simplifyScal a_scal
            b_terms_sofp <- if null b_terms
                            then do zero <- getZero tp; return $ NProd [Val zero] tp
                            else return $ NSum b_terms tp
            return $ Just (a_terms_sofp, b_terms_sofp)
        _ -> return Nothing

------------------------------------------------
------------------------------------------------
-- Main Helper Function: takes a scalar exp,  --
-- normalizes and simplifies it               --
------------------------------------------------
------------------------------------------------

simplifyScal :: ScalExp -> AlgSimplifyM ScalExp

simplifyScal (Val v) = return $ Val v
simplifyScal (Id  x t) = return $ Id  x t

simplifyScal e@SNot{} = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@SLogAnd{} = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@SLogOr{} = fromDNF =<< simplifyDNF =<< toDNF e
simplifyScal e@RelExp{} = fromDNF =<< simplifyDNF =<< toDNF e

--------------------------------------
--- MaxMin related simplifications ---
--------------------------------------
simplifyScal (MaxMin _ []) =
    badAlgSimplifyM "Scalar MaxMin expression with empty arglist."
simplifyScal (MaxMin _ [e]) = simplifyScal e
simplifyScal (MaxMin ismin es) = do -- helperMinMax ismin  es pos
    -- pos <- asks pos
    es0 <- mapM simplifyScal es
    let evals = filter isValue         es0
        es'   = filter (not . isValue) es0
        mvv = case evals of
                []   -> Nothing
                v:vs -> let myop = if ismin then min else max
                            myval= getValue v
                            oneval = (foldl myop myval . map getValue) vs
                        in  Just $ Val oneval
    -- flatten the result and remove duplicates,
    -- e.g., Max(Max(e1,e2), e1) -> Max(e1,e2,e3)
    case (es', mvv) of
        ([], Just vv) -> return vv
        (_,  Just vv) -> return $ MaxMin ismin $ remDups $ foldl flatop [] $ vv:es'
        (_,  Nothing) -> return $ MaxMin ismin $ remDups $ foldl flatop [] es'
    -- ToDo: This can prove very expensive as compile time
    --       but, IF e2-e1 <= 0 simplifies to True THEN
    --       Min(e1,e2) = e2.   Code example:
    -- e1me2 <- if isMin
    --          then simplifyScal $ AlgOp MINUS e1 e2 pos
    --          else simplifyScal $ AlgOp MINUS e2 e1 pos
    -- e1me2leq0 <- simplifyNRel $ NRelExp LEQ0 e1me2 pos
    -- case e1me2leq0 of
    --    NAnd [LogCt True  _] _ -> simplifyAlgN e1
    --    NAnd [LogCt False _] _ -> simplifyAlgN e2
    where
        isValue :: ScalExp -> Bool
        isValue e = case e of
                      Val _ -> True
                      _     -> False
        getValue :: ScalExp -> PrimValue
        getValue se = case se of
                        Val v -> v
                        _     -> value (0::Int32)
        flatop :: [ScalExp] -> ScalExp -> [ScalExp]
        flatop a e@(MaxMin ismin' ses) =
            a ++ if ismin == ismin' then ses else [e]
        flatop a e = a++[e]
        remDups :: [ScalExp] -> [ScalExp]
        remDups l = S.toList (S.fromList l)

---------------------------------------------------
--- Plus/Minus related simplifications          ---
--- BUG: the MinMax pattern matching should     ---
---      be performed on the simplified subexps ---
---------------------------------------------------
simplifyScal (SPlus e1o e2o) = do
    e1' <- simplifyScal e1o
    e2' <- simplifyScal e2o
    if isMaxMin e1' || isMaxMin e2'
    then helperPlusMinMax $ SPlus e1' e2'
    else normalPlus e1' e2'

    where
      normalPlus :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalPlus e1 e2 = do
        e1' <- toNumSofP e1
        e2' <- toNumSofP e2
        let tp = scalExpType e1
        let terms = getTerms e1' ++ getTerms e2'
        splittedTerms <- mapM splitTerm terms
        let sortedTerms = sortBy (\(n1,_) (n2,_) -> compare n1 n2) splittedTerms
        -- foldM discriminate: adds together identical terms, and
        -- we reverse the list, to keep it in a ascending order.
        merged <- reverse <$> foldM discriminate [] sortedTerms
        let filtered = filter (\(_,v) -> not $ zeroIsh v ) merged
        if null filtered
        then do
            zero <- getZero tp
            fromNumSofP $ NProd [Val zero] tp
        else do
            terms' <- mapM joinTerm filtered
            fromNumSofP $ NSum terms' tp

simplifyScal (SMinus e1 e2) = do
  let tp = scalExpType e1
  if e1 == e2
    then do zero <- getZero tp
            return $ Val zero
    else do min_1 <- getNeg1 $ scalExpType e1
            simplifyScal $ SPlus e1 $ STimes (Val min_1) e2

simplifyScal (SNeg e) = do
    negOne <- getNeg1 $ scalExpType e
    simplifyScal $ STimes (Val negOne) e

simplifyScal (SAbs e) = return $ SAbs e

simplifyScal (SSignum e) = return $ SSignum e

---------------------------------------------------
--- Times        related simplifications        ---
--- BUG: the MinMax pattern matching should     ---
---      be performed on the simplified subexps ---
---------------------------------------------------
simplifyScal (STimes e1o e2o) = do
    e1'' <- simplifyScal e1o
    e2'' <- simplifyScal e2o
    if isMaxMin e1'' || isMaxMin e2''
    then helperMultMinMax $ STimes e1'' e2''
    else normalTimes e1'' e2''

    where
      normalTimes :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalTimes e1 e2 = do
        let tp = scalExpType e1
        e1' <- toNumSofP e1
        e2' <- toNumSofP e2
        case (e1', e2') of
            (NProd xs _, y@NProd{}) -> fromNumSofP =<< makeProds xs y
            (NProd xs _, y) -> do
                    prods <- mapM (makeProds xs) $ getTerms y
                    fromNumSofP $ NSum (sort prods) tp
            (x, NProd ys _) -> do
                    prods <- mapM (makeProds ys) $ getTerms x
                    fromNumSofP $ NSum (sort prods) tp
            (NSum xs _, NSum ys _) -> do
                    xsMultChildren <- mapM getMultChildren xs
                    prods <- mapM (\x -> mapM (makeProds x) ys) xsMultChildren
                    fromNumSofP $ NSum (sort $ concat prods) tp

      makeProds :: [ScalExp] -> NNumExp -> AlgSimplifyM NNumExp
      makeProds [] _ =
           badAlgSimplifyM " In simplifyAlgN, makeProds: 1st arg is the empty list! "
      makeProds _ (NProd [] _) =
          badAlgSimplifyM " In simplifyAlgN, makeProds: 2nd arg is the empty list! "
      makeProds _ NSum{} =
          badAlgSimplifyM " In simplifyAlgN, makeProds: e1 * e2: e2 is a sum of sums! "
      makeProds (Val v1:exs) (NProd (Val v2:ys) tp1) = do
          v <- mulVals v1 v2
          return $ NProd (Val v : sort (ys++exs) ) tp1
      makeProds (Val v:exs) (NProd ys tp1) =
          return $ NProd (Val v : sort (ys++exs) ) tp1
      makeProds exs (NProd (Val v : ys) tp1) =
          return $ NProd (Val v : sort (ys++exs) ) tp1
      makeProds exs (NProd ys tp1) =
          return $ NProd (sort (ys++exs)) tp1

---------------------------------------------------
---------------------------------------------------
--- DIvide        related simplifications       ---
---------------------------------------------------
---------------------------------------------------

simplifyScal (SDiv e1o e2o) = do
    e1' <- simplifyScal e1o
    e2' <- simplifyScal e2o

    if isMaxMin e1' || isMaxMin e2'
    then helperMultMinMax $ SDiv e1' e2'
    else normalFloatDiv e1' e2'

    where
      normalFloatDiv :: ScalExp -> ScalExp -> AlgSimplifyM ScalExp
      normalFloatDiv e1 e2
        | e1 == e2                  = do one <- getPos1 $ scalExpType e1
                                         return $ Val one
--        | e1 == (negateSimplified e2) = do mone<- getNeg1 $ scalExpType e1
--                                           return $ Val mone
        | otherwise = do
            e1' <- toNumSofP e1
            e2' <- toNumSofP e2
            case e2' of
              NProd fs tp -> do
                e1Split <- mapM splitTerm (getTerms e1')
                case e1Split of
                  []  -> do zero <- getZero tp
                            return $ Val zero
                  _   -> do (fs', e1Split')  <- trySimplifyDivRec fs [] e1Split
                            if length fs' == length fs
                            then turnBackAndDiv e1' e2' -- insuccess
                            else do terms_e1' <- mapM joinTerm e1Split'
                                    e1'' <- fromNumSofP $ NSum terms_e1' tp
                                    case fs' of
                                      [] -> return e1''
                                      _  -> do e2'' <- fromNumSofP $ NProd fs' tp
                                               return $ SDiv e1'' e2''

              _ -> turnBackAndDiv e1' e2'

      turnBackAndDiv :: NNumExp -> NNumExp -> AlgSimplifyM ScalExp
      turnBackAndDiv ee1 ee2 = do
        ee1' <- fromNumSofP ee1
        ee2' <- fromNumSofP ee2
        return $ SDiv ee1' ee2'

simplifyScal (SMod e1o e2o) =
    SMod <$> simplifyScal e1o <*> simplifyScal e2o

simplifyScal (SQuot e1o e2o) =
    SQuot <$> simplifyScal e1o <*> simplifyScal e2o

simplifyScal (SRem e1o e2o) =
    SRem <$> simplifyScal e1o <*> simplifyScal e2o

---------------------------------------------------
---------------------------------------------------
--- Power        related simplifications        ---
---------------------------------------------------
---------------------------------------------------

-- cannot handle 0^a, because if a < 0 then it's an error.
-- Could be extented to handle negative exponents better, if needed
simplifyScal (SPow e1 e2) = do
    let tp = scalExpType e1
    e1' <- simplifyScal e1
    e2' <- simplifyScal e2

    if isCt1 e1' || isCt0 e2'
    then do one <- getPos1 tp
            return $ Val one
    else if isCt1 e2'
    then return e1'
    else case (e1', e2') of
            (Val v1, Val v2)
              | Just v <- powVals v1 v2 -> return $ Val v
            (_, Val (IntValue n)) ->
                if P.intToInt64 n >= 1
                then -- simplifyScal =<< fromNumSofP $ NProd (replicate n e1') tp
                        do new_e <- fromNumSofP $ NProd (genericReplicate (P.intToInt64 n) e1') tp
                           simplifyScal new_e
                else return $ SPow e1' e2'
            (_, _) -> return $ SPow e1' e2'

    where
        powVals :: PrimValue -> PrimValue -> Maybe PrimValue
        powVals (IntValue v1) (IntValue v2) = IntValue <$> P.doPow v1 v2
        powVals _ _ = Nothing

-----------------------------------------------------
--- Helpers for simplifyScal: MinMax related, etc ---
-----------------------------------------------------

isMaxMin :: ScalExp -> Bool
isMaxMin MaxMin{} = True
isMaxMin _        = False

helperPlusMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperPlusMinMax (SPlus (MaxMin ismin es) e) =
    simplifyScal $ MaxMin ismin $ map (`SPlus` e) es
helperPlusMinMax (SPlus e (MaxMin ismin es)) =
    simplifyScal $ MaxMin ismin $ map (SPlus e) es
helperPlusMinMax _ = badAlgSimplifyM "helperPlusMinMax: Reached unreachable case!"

{-
helperMinusMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperMinusMinMax (SMinus (MaxMin ismin es) e) =
    simplifyScal $ MaxMin ismin $ map (\x -> SMinus x e) es
helperMinusMinMax (SMinus e (MaxMin ismin es)) =
    simplifyScal $ MaxMin ismin $ map (\x -> SMinus e x) es
helperMinusMinMax _ = do
    pos <- asks pos
    badAlgSimplifyM "helperMinusMinMax: Reached unreachable case!"
-}
helperMultMinMax :: ScalExp -> AlgSimplifyM ScalExp
helperMultMinMax (STimes  e em@MaxMin{}) = helperTimesDivMinMax True  True  em e
helperMultMinMax (STimes  em@MaxMin{} e) = helperTimesDivMinMax True  False em e
helperMultMinMax (SDiv e em@MaxMin{}) = helperTimesDivMinMax False True  em e
helperMultMinMax (SDiv em@MaxMin{} e) = helperTimesDivMinMax False False em e
helperMultMinMax _ = badAlgSimplifyM "helperMultMinMax: Reached unreachable case!"

helperTimesDivMinMax :: Bool -> Bool -> ScalExp -> ScalExp -> AlgSimplifyM ScalExp
helperTimesDivMinMax isTimes isRev emo@MaxMin{} e = do
    em <- simplifyScal emo
    case em of
        MaxMin ismin es -> do
            e' <- simplifyScal e
            e'_sop <- toNumSofP e'
            p' <- simplifyNRel $ NRelExp LTH0 e'_sop
            case p' of
                LogCt ctbool -> do
--                    let cond = not isTimes && isRev
--                    let cond'= if ctbool then cond  else not cond
--                    let ismin'= if cond' then ismin else not ismin

                    let cond =  (     isTimes              && not ctbool ) ||
                                ( not isTimes && not isRev && not ctbool ) ||
                                ( not isTimes &&     isRev &&     ctbool  )
                    let ismin' = if cond then ismin else not ismin
                    simplifyScal $ MaxMin ismin' $ map (`mkTimesDiv` e') es

                _  -> if not isTimes then return $ mkTimesDiv em e'
                      else -- e' * MaxMin{...}
                        case e'_sop of
                            NProd _  _  -> return $ mkTimesDiv em e' -- simplifyScal =<< fromNumSofP (NProd (em:fs) tp)
                            NSum  ts tp -> do
                                new_ts <-
                                    mapM (\x -> case x of
                                                  NProd fs _ -> return $ NProd (em:fs) tp
                                                  _          -> badAlgSimplifyM
                                                                "helperTimesDivMinMax: SofP invariant violated!"
                                         ) ts
                                simplifyScal =<< fromNumSofP ( NSum new_ts tp )
        _ -> simplifyScal $ mkTimesDiv em e
    where
        mkTimesDiv :: ScalExp -> ScalExp -> ScalExp
        mkTimesDiv e1 e2
          | not isTimes = if isRev then SDiv e2 e1 else SDiv e1 e2
          | isRev       = STimes e2 e1
          | otherwise   = STimes e1 e2

helperTimesDivMinMax _ _ _ _ =
  badAlgSimplifyM "helperTimesDivMinMax: Reached unreachable case!"


---------------------------------------------------
---------------------------------------------------
--- translating to and simplifying the          ---
--- disjunctive normal form: toDNF, simplifyDNF ---
---------------------------------------------------
---------------------------------------------------
--isTrueDNF :: DNF -> Bool
--isTrueDNF [[LogCt True]] = True
--isTrueDNF _              = False
--
--getValueDNF :: DNF -> Maybe Bool
--getValueDNF [[LogCt True]]  = Just True
--getValueDNF [[LogCt False]] = Just True
--getValueDNF _               = Nothing


negateBTerm :: BTerm -> AlgSimplifyM BTerm
negateBTerm (LogCt v) = return $ LogCt (not v)
negateBTerm (PosId i) = return $ NegId i
negateBTerm (NegId i) = return $ PosId i
negateBTerm (NRelExp rel e) = do
    let tp = typeOfNAlg e
    case (tp, rel) of
        (IntType it, LTH0) -> do
            se <- fromNumSofP e
            ne <- toNumSofP =<< simplifyScal (SNeg $ SPlus se (Val (value (P.intValue it (1::Int)))))
            return $ NRelExp LTH0 ne
        _ -> do
            e' <- toNumSofP =<< negateSimplified =<< fromNumSofP e;
            return $ NRelExp (if rel == LEQ0 then LTH0 else LEQ0) e'

bterm2ScalExp :: BTerm -> AlgSimplifyM ScalExp
bterm2ScalExp (LogCt v) = return $ Val $ BoolValue v
bterm2ScalExp (PosId i) = return $ Id i int32
bterm2ScalExp (NegId i) = return $ SNot $ Id i int32
bterm2ScalExp (NRelExp rel e) = do e' <- fromNumSofP e; return $ RelExp rel e'

-- translates from DNF to ScalExp
fromDNF :: DNF -> AlgSimplifyM ScalExp
fromDNF [] = badAlgSimplifyM "fromDNF: empty DNF!"
fromDNF (t:ts) = do
    t' <- translFact t
    foldM (\acc x -> do x' <- translFact x; return $ SLogOr x' acc) t' ts
    where
        translFact [] = badAlgSimplifyM "fromDNF, translFact empty DNF factor!"
        translFact (f:fs) = do
            f' <- bterm2ScalExp f
            foldM (\acc x -> do x' <- bterm2ScalExp x; return $ SLogAnd x' acc) f' fs

-- translates (and simplifies numeric expressions?) to DNF form.
toDNF :: ScalExp -> AlgSimplifyM DNF
toDNF (Val  (BoolValue v)) = return [[LogCt v]]
toDNF (Id      idd  _ ) = return [[PosId idd]]
toDNF (RelExp  rel  e ) = do
  let t = scalExpType e
  case t of
    IntType it -> do
      e' <- if rel == LEQ0
            then do m1 <- getNeg1 $ IntType it
                    return $ SPlus e $ Val m1
            else return e
      ne   <- toNumSofP =<< simplifyScal e'
      nrel <- simplifyNRel $ NRelExp LTH0 ne  -- False
      return [[nrel]]

    _   -> do ne   <- toNumSofP =<< simplifyScal e
              nrel <- markGaussLTH0 $ simplifyNRel $ NRelExp rel ne
              return [[nrel]]
--
toDNF (SNot (SNot     e)) = toDNF e
toDNF (SNot (Val (BoolValue v))) = return [[LogCt $ not v]]
toDNF (SNot (Id idd _)) = return [[NegId idd]]
toDNF (SNot (RelExp rel e)) = do
    let not_rel = if rel == LEQ0 then LTH0 else LEQ0
    neg_e <- simplifyScal (SNeg e)
    toDNF $ RelExp not_rel neg_e
--
toDNF (SLogOr  e1 e2  ) = do
    e1s <- toDNF e1
    e2s <- toDNF e2
    return $ sort $ e1s ++ e2s
toDNF (SLogAnd e1 e2  ) = do
    -- [t1 ++ t2 | t1 <- toDNF e1, t2 <- toDNF e2]
    e1s <- toDNF e1
    e2s <- toDNF e2
    let lll = map (\t2-> map (++t2) e1s) e2s
    return $ sort $ concat lll
toDNF (SNot (SLogAnd e1 e2)) = do
    e1s <- toDNF (SNot e1)
    e2s <- toDNF (SNot e2)
    return $ sort $ e1s ++ e2s
toDNF (SNot (SLogOr e1 e2)) = do
    -- [t1 ++ t2 | t1 <- dnf $ SNot e1, t2 <- dnf $ SNot e2]
    e1s <- toDNF $ SNot e1
    e2s <- toDNF $ SNot e2
    let lll = map (\t2-> map (++t2) e1s) e2s
    return $ sort $ concat lll
toDNF _            = badAlgSimplifyM "toDNF: not a boolean expression!"

------------------------------------------------------
--- Simplifying Boolean Expressions:               ---
---  0. p     AND p == p;       p     OR p == p    ---
---  1. False AND p == False;   True  OR p == True ---
---  2. True  AND p == p;       False OR p == p    ---
---  3.(not p)AND p == FALSE;  (not p)OR p == True ---
---  4. ToDo: p1 AND p2 == p1 if p1 => p2          ---
---           p1 AND p2 == False if p1 => not p2 or---
---                                 p2 => not p1   ---
---     Also: p1 OR p2 == p2 if p1 => p2           ---
---           p1 OR p2 == True if not p1 => p2 or  ---
---                               not p2 => p1     ---
---     This boils down to relations:              ---
---      e1 < 0 => e2 < 0 if e2 <= e1              ---
------------------------------------------------------
simplifyDNF :: DNF -> AlgSimplifyM DNF
simplifyDNF terms0 = do
    terms1 <- mapM (simplifyAndOr True) terms0
    let terms' = if [LogCt True] `elem` terms1 then [[LogCt True]]
                 else S.toList $ S.fromList $
                        filter (/= [LogCt False]) terms1
    if null terms' then return [[LogCt False]]
    else do
        let len1terms = all ((1==) . length) terms'
        if not len1terms then return terms'
        else do let terms_flat = concat terms'
                terms'' <- simplifyAndOr False terms_flat
                return $ map (:[]) terms''

-- big helper function for simplifyDNF
simplifyAndOr :: Bool -> [BTerm] -> AlgSimplifyM [BTerm]
simplifyAndOr _ [] = badAlgSimplifyM "simplifyAndOr: not a boolean expression!"
simplifyAndOr is_and fs =
    if LogCt (not is_and) `elem` fs
         -- False AND p == False
    then return [LogCt $ not is_and]
                    -- (i) p AND p == p,        (ii) True AND p == p
    else do let fs' = S.toList . S.fromList . filter (/=LogCt is_and) $ fs
            if null fs'
            then return [LogCt is_and]
            else do    -- IF p1 => p2 THEN   p1 AND p2 --> p1
                fs''<- foldM (\l x-> do (addx, l') <- trimImplies is_and x l
                                        return $ if addx then x:l' else l'
                             ) [] fs'
                       -- IF p1 => not p2 THEN p1 AND p2 == False
                isF <- foldM (\b x -> if b then return b
                                      else do notx <- negateBTerm x
                                              impliesAny is_and x notx fs''
                             ) False fs''
                return $ if not isF then fs''
                         else if is_and then [LogCt False]
                                        else [LogCt True ]
    where
        -- e1 => e2 ?
        impliesRel :: BTerm -> BTerm -> AlgSimplifyM Bool
        impliesRel (LogCt False) _ = return True
        impliesRel _ (LogCt  True) = return True
        impliesRel (LogCt True)  e = do
            let e' = e -- simplifyNRel e
            return $ e' == LogCt True
        impliesRel e (LogCt False) = do
            e' <- negateBTerm e -- simplifyNRel =<< negateBTerm e
            return $ e' == LogCt True
        impliesRel (NRelExp rel1 e1) (NRelExp rel2 e2) = do
        -- ToDo: implement implies relation!
            --not_aggr <- asks cheap
            let btp = typeOfNAlg e1
            if btp /= typeOfNAlg e2
            then return False
            else do
                one <- getPos1    btp
                e1' <- fromNumSofP e1
                e2' <- fromNumSofP e2
                case (rel1, rel2, btp) of
                    (LTH0, LTH0, IntType _) -> do
                        e2me1m1 <- toNumSofP =<< simplifyScal (SMinus e2' $ SPlus e1' $ Val one)
                        diffrel <- simplifyNRel $ NRelExp LTH0 e2me1m1
                        return $ diffrel == LogCt True
                    (_, _, IntType _) -> badAlgSimplifyM "impliesRel: LEQ0 for Int!"
                    (_, _, _) -> badAlgSimplifyM "impliesRel: exp of illegal type!"
        impliesRel p1 p2
            | p1 == p2  = return True
            | otherwise = return False

        -- trimImplies(true,  x, l) performs: p1 AND p2 == p1 if p1 => p2,
        --   i.e., removes any p in l such that:
        --    (i) x => p orelse if
        --   (ii) p => x then indicates that p should not be added to the reuslt
        -- trimImplies(false, x, l) performs: p1 OR p2 == p2 if p1 => p2,
        --   i.e., removes any p from l such that:
        --    (i) p => x orelse if
        --   (ii) x => p then indicates that p should not be added to the result
        trimImplies :: Bool -> BTerm -> [BTerm] -> AlgSimplifyM (Bool, [BTerm])
        trimImplies _        _ []     = return (True, [])
        trimImplies and_case x (p:ps) = do
            succc <- impliesRel x p
            if succc
            then if and_case then trimImplies and_case x ps else return (False, p:ps)
            else do suc <- impliesRel p x
                    if suc then if and_case then return (False, p:ps) else trimImplies and_case x ps
                    else do (addx, newps) <- trimImplies and_case x ps
                            return (addx, p:newps)

        -- impliesAny(true,  x, notx, l) performs:
        --   x AND p == False if p => not x, where p in l,
        -- impliesAny(true,  x, notx, l) performs:
        --   x OR p == True if not x => p
        -- BUT only when p != x, i.e., avoids comparing x with notx
        impliesAny :: Bool -> BTerm -> BTerm -> [BTerm] -> AlgSimplifyM Bool
        impliesAny _        _ _    []     = return False
        impliesAny and_case x notx (p:ps)
            | x == p = impliesAny and_case x notx ps
            | otherwise = do
                succ' <- if and_case then impliesRel p notx else impliesRel notx p
                if succ' then return True
                else impliesAny and_case x notx ps

------------------------------------------------
--- Syntax-Directed (Brainless) Translators  ---
---    scalExp <-> NNumExp                   ---
--- and negating a scalar expression         ---
------------------------------------------------

-- negates an already simplified scalar expression,
--   presumably more efficient than negating and
--   then simplifying it.
negateSimplified :: ScalExp -> AlgSimplifyM ScalExp
negateSimplified (SNeg e) = return e
negateSimplified (SNot e) = return e
negateSimplified (SAbs e) = return $ SAbs e
negateSimplified (SSignum e) =
  SSignum <$> negateSimplified e
negateSimplified e@(Val v) = do
    m1 <- getNeg1 $ scalExpType e
    v' <- mulVals m1 v; return $ Val v'
negateSimplified e@Id{} = do
    m1 <- getNeg1 $ scalExpType e
    return $ STimes (Val m1) e
negateSimplified (SMinus e1 e2) = do -- return $ SMinus e2 e1
    e1' <- negateSimplified e1
    return $ SPlus e1' e2
negateSimplified (SPlus e1 e2) = do
    e1' <- negateSimplified e1
    e2' <- negateSimplified e2
    return $ SPlus e1' e2'
negateSimplified e@(SPow _ _) = do
    m1 <- getNeg1 $ scalExpType e
    return $ STimes (Val m1) e
negateSimplified (STimes  e1 e2) = do
    (e1', e2') <- helperNegateMult e1 e2; return $ STimes  e1' e2'
negateSimplified (SDiv e1 e2) = do
    (e1', e2') <- helperNegateMult e1 e2; return $ SDiv e1' e2'
negateSimplified (SMod e1 e2) =
    return $ SMod e1 e2
negateSimplified (SQuot e1 e2) = do
    (e1', e2') <- helperNegateMult e1 e2; return $ SQuot e1' e2'
negateSimplified (SRem e1 e2) =
    return $ SRem e1 e2
negateSimplified (MaxMin ismin ts) = do
    ts' <- mapM negateSimplified ts; return $ MaxMin (not ismin) ts'
negateSimplified (RelExp LEQ0 e) = do
    me <- negateSimplified e
    return $ RelExp LTH0 me
negateSimplified (RelExp LTH0 e) = do
    me <- negateSimplified e
    return $ RelExp LEQ0 me
negateSimplified SLogAnd{} = badAlgSimplifyM "negateSimplified: SLogAnd unimplemented!"
negateSimplified SLogOr{} = badAlgSimplifyM "negateSimplified: SLogOr  unimplemented!"

helperNegateMult :: ScalExp -> ScalExp -> AlgSimplifyM (ScalExp, ScalExp)
helperNegateMult e1 e2 =
    case (e1, e2) of
        (Val _,              _) -> do e1'<- negateSimplified e1;       return (e1', e2)
        (STimes (Val v) e1r, _) -> do ev <- negateSimplified (Val v);  return (STimes ev e1r, e2)
        (_,              Val _) -> do e2'<- negateSimplified e2;       return (e1, e2')
        (_, STimes (Val v) e2r) -> do ev <- negateSimplified (Val v);  return (e1, STimes ev e2r)
        (_,                  _) -> do e1'<- negateSimplified e1;       return (e1', e2)


toNumSofP :: ScalExp -> AlgSimplifyM NNumExp
toNumSofP e@(Val  _) = return $ NProd [e] $ scalExpType e
toNumSofP e@(Id _ _) = return $ NProd [e] $ scalExpType e
toNumSofP e@SDiv{} = return $ NProd [e] $ scalExpType e
toNumSofP e@SPow{} = return $ NProd [e] $ scalExpType e
toNumSofP (SMinus _ _) = badAlgSimplifyM "toNumSofP: SMinus is not in SofP form!"
toNumSofP (SNeg _) = badAlgSimplifyM "toNumSofP: SNeg is not in SofP form!"
toNumSofP (STimes e1 e2) = do
    e2' <- toNumSofP e2
    case e2' of
        NProd es2 t -> return $ NProd (e1:es2) t
        _ -> badAlgSimplifyM "toNumSofP: STimes nor in SofP form!"
toNumSofP (SPlus  e1 e2)   = do
    let t = scalExpType e1
    e1' <- toNumSofP  e1
    e2' <- toNumSofP  e2
    case (e1', e2') of
        (NSum es1 _, NSum es2 _) -> return $ NSum (es1++es2) t
        (NSum es1 _, NProd{}) -> return $ NSum (es1++[e2']) t
        (NProd{}, NSum es2 _) -> return $ NSum (e1':es2)    t
        (NProd{}, NProd{}   ) -> return $ NSum [e1', e2']   t
toNumSofP me@MaxMin{} =
  return $ NProd [me] $ scalExpType me
toNumSofP s_e = return $ NProd [s_e] $ scalExpType s_e


fromNumSofP :: NNumExp -> AlgSimplifyM ScalExp
fromNumSofP (NSum [ ] t) = do
    zero <- getZero t
    return $ Val zero
fromNumSofP (NSum [f] _) = fromNumSofP f
fromNumSofP (NSum (f:fs) t) = do
    fs_e <- fromNumSofP $ NSum fs t
    f_e  <- fromNumSofP f
    return $ SPlus f_e fs_e
fromNumSofP (NProd [] _) =
  badAlgSimplifyM " In fromNumSofP, empty NProd expression! "
fromNumSofP (NProd [f] _)    = return f
fromNumSofP (NProd (f:fs) t) = do
    fs_e <- fromNumSofP $ NProd fs t
    return $ STimes f fs_e
--fromNumSofP _ = do
--    pos <- asks pos
--    badAlgSimplifyM "fromNumSofP: unimplemented!"
------------------------------------------------------------
--- Helpers for simplifyScal: getTerms, getMultChildren, ---
---   splitTerm, joinTerm, discriminate
------------------------------------------------------------


-- get the list of terms of an expression
-- BUG for NMinMax -> should convert it back to a ScalExp
getTerms :: NNumExp -> [NNumExp]
getTerms (NSum es _) = es
getTerms e@NProd{} = [e]

-- get the factors of a term
getMultChildren :: NNumExp -> AlgSimplifyM [ScalExp]
getMultChildren (NSum _ _) = badAlgSimplifyM "getMultChildren, NaryPlus should not be nested 2 levels deep "
getMultChildren (NProd xs _) = return xs

-- split a term into a (multiplicative) value and the rest of the factors.
splitTerm :: NNumExp -> AlgSimplifyM (NNumExp, PrimValue)
splitTerm (NProd [ ] _) = badAlgSimplifyM "splitTerm: Empty n-ary list of factors."
splitTerm (NProd [f] tp) = do
  one <- getPos1 tp
  case f of
      (Val v) -> return (NProd [Val one] tp, v  )
      e       -> return (NProd [e]       tp, one)
splitTerm ne@(NProd (f:fs) tp) =
  case f of
      (Val v) -> return (NProd fs tp, v)
      _       -> do one <- getPos1 tp
                    return (ne, one)
splitTerm e = do
  one <- getPos1 (typeOfNAlg e)
  return (e, one)

-- join a value with a list of factors into a term.
joinTerm :: (NNumExp, PrimValue) -> AlgSimplifyM NNumExp
joinTerm ( NSum _ _, _) = badAlgSimplifyM "joinTerm: NaryPlus two levels deep."
joinTerm ( NProd [] _, _) = badAlgSimplifyM "joinTerm: Empty NaryProd."
joinTerm ( NProd (Val l:fs) tp, v) = do
    v' <- mulVals v l
    let v'Lit = Val v'
    return $ NProd (v'Lit:sort fs) tp
joinTerm ( e@(NProd fs tp), v)
  | P.oneIsh v   = return e
  | otherwise = let vExp = Val v
                in return $ NProd (vExp:sort fs) tp

-- adds up the values corresponding to identical factors!
discriminate :: [(NNumExp, PrimValue)] -> (NNumExp, PrimValue) -> AlgSimplifyM [(NNumExp, PrimValue)]
discriminate []          e        = return [e]
discriminate e@((k,v):t) (k', v') =
  if k == k'
  then do v'' <- addVals v v'
          return ( (k, v'') : t )
  else return ( (k', v') : e )

------------------------------------------------------
--- Trivial Utility Functions                      ---
------------------------------------------------------

getZero :: PrimType -> AlgSimplifyM PrimValue
getZero (IntType t)     = return $ value $ intValue t (0::Int)
getZero tp      = badAlgSimplifyM ("getZero for type: "++pretty tp)

getPos1 :: PrimType -> AlgSimplifyM PrimValue
getPos1 (IntType t)     = return $ value $ intValue t (1::Int)
getPos1 tp      = badAlgSimplifyM ("getOne for type: "++pretty tp)

getNeg1 :: PrimType -> AlgSimplifyM PrimValue
getNeg1 (IntType t)     = return $ value $ intValue t (-1::Int)
getNeg1 tp      = badAlgSimplifyM ("getOne for type: "++pretty tp)

valLTHEQ0 :: RelOp0 -> PrimValue -> AlgSimplifyM Bool
valLTHEQ0 LEQ0 (IntValue iv) = return $ P.intToInt64 iv <= 0
valLTHEQ0 LTH0 (IntValue iv) = return $ P.intToInt64 iv < 0
valLTHEQ0 _ _ = badAlgSimplifyM "valLTHEQ0 for non-numeric type!"

isCt1 :: ScalExp -> Bool
isCt1 (Val v) = P.oneIsh v
isCt1 _ = False

isCt0 :: ScalExp -> Bool
isCt0 (Val v) = P.zeroIsh v
isCt0 _       = False


addVals :: PrimValue -> PrimValue -> AlgSimplifyM PrimValue
addVals (IntValue v1) (IntValue v2) =
  return $ IntValue $ P.doAdd v1 v2
addVals _ _ =
  badAlgSimplifyM "addVals: operands not of (the same) numeral type! "

mulVals :: PrimValue -> PrimValue -> AlgSimplifyM PrimValue
mulVals (IntValue v1) (IntValue v2) =
  return $ IntValue $ P.doMul v1 v2
mulVals v1 v2 =
  badAlgSimplifyM $ "mulVals: operands not of (the same) numeral type! "++
  pretty (PrimVal v1)++" "++pretty (PrimVal v2)

divVals :: PrimValue -> PrimValue -> AlgSimplifyM PrimValue
divVals (IntValue v1) (IntValue v2) =
  case P.doSDiv v1 v2 of
    Just v -> return $ IntValue v
    Nothing -> badAlgSimplifyM "Division by zero"
divVals _ _ =
  badAlgSimplifyM "divVals: operands not of (the same) numeral type! "

canDivValsEvenly :: PrimValue -> PrimValue -> AlgSimplifyM Bool
canDivValsEvenly (IntValue v1) (IntValue v2) =
  case P.doSMod v1 v2 of
    Just v -> return $ P.zeroIsh $ IntValue v
    Nothing -> return False
canDivValsEvenly _ _ =
  badAlgSimplifyM "canDivValsEvenly: operands not of (the same) numeral type!"

-------------------------------------------------------------
-------------------------------------------------------------
---- Helpers for the ScalExp and NNumRelLogExp Datatypes ----
-------------------------------------------------------------
-------------------------------------------------------------

typeOfNAlg :: NNumExp -> PrimType
typeOfNAlg (NSum   _   t) = t
typeOfNAlg (NProd  _   t) = t

----------------------------------------
---- Helpers for Division and Power ----
----------------------------------------
trySimplifyDivRec :: [ScalExp] -> [ScalExp] -> [(NNumExp, PrimValue)] ->
                     AlgSimplifyM ([ScalExp], [(NNumExp, PrimValue)])
trySimplifyDivRec [] fs' spl_terms =
    return (fs', spl_terms)
trySimplifyDivRec (f:fs) fs' spl_terms = do
    res_tmp <- mapM (tryDivProdByOneFact f) spl_terms
    let (succs, spl_terms') = unzip res_tmp
    if all (==True) succs
    then trySimplifyDivRec fs fs' spl_terms'
    else trySimplifyDivRec fs (fs'++[f]) spl_terms


tryDivProdByOneFact :: ScalExp -> (NNumExp, PrimValue) -> AlgSimplifyM (Bool, (NNumExp, PrimValue))
tryDivProdByOneFact (Val f) (e, v) = do
    succc <- canDivValsEvenly v f
    if succc then do vres <- divVals v f
                     return (True, (e, vres))
             else return (False,(e, v) )

tryDivProdByOneFact _ pev@(NProd [] _, _) = return (False, pev)
tryDivProdByOneFact f pev@(NProd (t:tfs) tp, v) = do
    (succc, newt) <- tryDivTriv t f
    one <- getPos1 tp
    if not succc
    then do (succ', (tfs', v')) <- tryDivProdByOneFact f (NProd tfs tp, v)
            case (succ', tfs') of
                (True,  NProd (Val vv:tfs'') _) -> do
                                    vres <- mulVals v' vv
                                    return (True, (NProd (t:tfs'') tp, vres))
                (True,  NProd tfs'' _) -> return (True, (NProd (t:tfs'') tp, v'))
                (_, _) -> return (False, pev)
    else case (newt, tfs) of
           (Val vv, _) -> do vres <- mulVals vv v
                             return $ if null tfs
                                      then (True, (NProd [Val one] tp, vres))
                                      else (True, (NProd tfs tp, vres))
           (_,      _) -> return (True, (NProd (newt:tfs) tp, v))

tryDivProdByOneFact _ (NSum _ _, _) =
  badAlgSimplifyM "tryDivProdByOneFact: unreachable case NSum reached!"


tryDivTriv :: ScalExp -> ScalExp -> AlgSimplifyM (Bool, ScalExp)
tryDivTriv (SPow a e1) (SPow d e2)
    | a == d && e1 == e2 = do one <- getPos1 $ scalExpType a
                              return (True, Val one)
    | a == d = do
          let tp = scalExpType a
          one <- getPos1 tp
          e1me2 <- simplifyScal $ SMinus e1 e2
          case (tp, e1me2) of
            (IntType _, Val v) | P.zeroIsh v ->
              return (True, Val one)
            (IntType _, Val v) | P.oneIsh v ->
              return (True, a)
            (IntType _, _) -> do
              e2me1 <- negateSimplified e1me2
              e2me1_sop <- toNumSofP e2me1
              p' <- simplifyNRel $ NRelExp LTH0 e2me1_sop
              return $ if p' == LogCt True
                       then (True,  SPow a e1me2)
                       else (False, SDiv (SPow a e1) (SPow d e2))

            (_, _) -> return (False, SDiv (SPow a e1) (SPow d e2))

    | otherwise = return (False, SDiv (SPow a e1) (SPow d e2))

tryDivTriv (SPow a e1) b
    | a == b = do one <- getPos1 $ scalExpType a
                  tryDivTriv (SPow a e1) (SPow a (Val one))
    | otherwise = return (False, SDiv (SPow a e1) b)

tryDivTriv b (SPow a e1)
    | a == b = do one <- getPos1 $ scalExpType a
                  tryDivTriv (SPow a (Val one)) (SPow a e1)
    | otherwise = return (False, SDiv b (SPow a e1))

tryDivTriv t f
    | t == f    = do one <- getPos1 $ scalExpType t
                     return (True,  Val one)
    | otherwise = return (False, SDiv t f)


{-
mkRelExp :: Int -> (RangesRep, ScalExp, ScalExp)
mkRelExp 1 =
    let (i',j',n',p',m') = (tident "int i", tident "int j", tident "int n", tident "int p", tident "int m")
        (i,j,n,p,m) = (Id i', Id j', Id n', Id p', Id m')
        one = Val (IntVal 1)
        min_p_nm1 = MaxMin True [p, SMinus n one]
        hash = M.fromList $ [ (identName n', ( 1::Int, Just (Val (IntVal 1)), Nothing ) ),
                               (identName p', ( 1::Int, Just (Val (IntVal 0)), Nothing ) ),
                               (identName i', ( 5::Int, Just (Val (IntVal 0)), Just min_p_nm1 ) )
                             , (identName j', ( 9::Int, Just (Val (IntVal 0)), Just i ) )
                             ] -- M.Map VName (Int, Maybe ScalExp, Maybe ScalExp)
        ij_p_j_p_1_m_m = SMinus (SPlus (STimes i j) (SPlus j one)) m
        rel1 = RelExp LTH0 ij_p_j_p_1_m_m
        m_ij_m_j_m_2 = SNeg ( SPlus (STimes i j) (SPlus j (Val (IntVal 2))) )
        rel2 = RelExp LTH0 m_ij_m_j_m_2

    in (hash, rel1, rel2)
mkRelExp 2 =
    let (i',a',b',l',u') = (tident "int i", tident "int a", tident "int b", tident "int l", tident "int u")
        (i,a,b,l,u) = (Id i', Id a', Id b', Id l', Id u')
        hash = M.fromList $ [ (identName i', ( 5::Int, Just l, Just u ) ) ]
        ai_p_b = SPlus (STimes a i) b
        rel1 = RelExp LTH0 ai_p_b

    in (hash, rel1, rel1)
mkRelExp 3 =
    let (i',j',n',m') = (tident "int i", tident "int j", tident "int n", tident "int m")
        (i,j,n,m) = (Id i', Id j', Id n', Id m')
        one = Val (IntVal 1)
        two = Val (IntVal 2)
        min_j_nm1 = MaxMin True [MaxMin False [Val (IntVal 0), SMinus i (STimes two n)], SMinus n one]
        hash = M.fromList $ [ (identName n', ( 1::Int, Just (Val (IntVal 1)), Nothing ) ),
                               (identName m', ( 2::Int, Just (Val (IntVal 1)), Nothing ) ),
                               (identName i', ( 5::Int, Just (Val (IntVal 0)), Just (SMinus m one) ) )
                             , (identName j', ( 9::Int, Just (Val (IntVal 0)), Just min_j_nm1 ) )
                             ] -- M.Map VName (Int, Maybe ScalExp, Maybe ScalExp)
        ij_m_m = SMinus (STimes i j) m
        rel1 = RelExp LTH0 ij_m_m
--        rel3 = RelExp LTH0 (SMinus i (SPlus (STimes two n) j))
        m_ij_m_1 = SMinus (Val (IntVal (-1))) (STimes i j)
        rel2 = RelExp LTH0 m_ij_m_1

--        simpl_exp = SDiv (MaxMin True [SMinus (Val (IntVal 0)) (STimes i j), SNeg (STimes i n) ])
--                         (STimes i j)
--        rel4 = RelExp LTH0 simpl_exp

    in (hash, rel1, rel2)

mkRelExp _ = let hash = M.empty
                 rel = RelExp LTH0 (Val (IntVal (-1)))
             in (hash, rel, rel)
-}
