{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module implements an optimisation that moves in-place
-- updates into/before loops where possible, with the end goal of
-- minimising memory copies.  As an example, consider this program:
--
-- @
--   let r =
--     loop (r1 = r0) = for i < n do
--       let a = r1[i]
--       let r1[i] = a * i
--       in r1
--   ...
--   let x = y with [k] <- r in
--   ...
-- @
--
-- We want to turn this into the following:
--
-- @
--   let x0 = y with [k] <- r0
--   loop (x = x0) = for i < n do
--     let a = a[k,i]
--     let x[k,i] = a * i
--     in x
--   let r = x[k] in
--   ...
-- @
--
-- The intent is that we are also going to optimise the new data
-- movement (in the @x0@-binding), possibly by changing how @r0@ is
-- defined.  For the above transformation to be valid, a number of
-- conditions must be fulfilled:
--
--    (1) @r@ must not be consumed after the original in-place update.
--
--    (2) @k@ and @y@ must be available at the beginning of the loop.
--
--    (3) @x@ must be visible whenever @r@ is visible.  (This means
--    that both @x@ and @r@ must be bound in the same t'Body'.)
--
--    (4) If @x@ is consumed at a point after the loop, @r@ must not
--    be used after that point.
--
--    (5) The size of @r1@ is invariant inside the loop.
--
--    (6) The value @r@ must come from something that we can actually
--    optimise (e.g. not a function parameter).
--
--    (7) @y@ (or its aliases) may not be used inside the body of the
--    loop.
--
--    (8) The result of the loop may not alias the merge parameter
--    @r1@.
--
--    (9) @y@ or its aliases may not be used after the loop.
--
-- FIXME: the implementation is not finished yet.  Specifically, not
-- all of the above conditions are checked.
module Futhark.Optimise.InPlaceLowering
  ( inPlaceLoweringGPU,
    inPlaceLoweringSeq,
    inPlaceLoweringMC,
  )
where

import Control.Monad.RWS
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Futhark.Analysis.Alias
import Futhark.Builder
import Futhark.IR.Aliases
import Futhark.IR.GPU
import Futhark.IR.MC
import Futhark.IR.Seq (Seq)
import Futhark.Optimise.InPlaceLowering.LowerIntoStm
import Futhark.Pass
import Futhark.Util (nubByOrd)

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLoweringGPU :: Pass GPU GPU
inPlaceLoweringGPU = inPlaceLowering onKernelOp lowerUpdateGPU

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLoweringSeq :: Pass Seq Seq
inPlaceLoweringSeq = inPlaceLowering pure lowerUpdate

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLoweringMC :: Pass MC MC
inPlaceLoweringMC = inPlaceLowering onMCOp lowerUpdate

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLowering ::
  Constraints rep =>
  OnOp rep ->
  LowerUpdate rep (ForwardingM rep) ->
  Pass rep rep
inPlaceLowering onOp lower =
  Pass "In-place lowering" "Lower in-place updates into loops" $
    fmap removeProgAliases
      . intraproceduralTransformationWithConsts optimiseConsts optimiseFunDef
      . aliasAnalysis
  where
    optimiseConsts stms =
      modifyNameSource $
        runForwardingM lower onOp $
          stmsFromList <$> optimiseStms (stmsToList stms) (pure ())

    optimiseFunDef consts fundec =
      modifyNameSource $
        runForwardingM lower onOp $
          descend (stmsToList consts) $
            bindingFParams (funDefParams fundec) $ do
              body <- optimiseBody $ funDefBody fundec
              pure $ fundec {funDefBody = body}

    descend [] m = m
    descend (stm : stms) m = bindingStm stm $ descend stms m

type Constraints rep = (Buildable rep, CanBeAliased (Op rep))

optimiseBody ::
  Constraints rep =>
  Body (Aliases rep) ->
  ForwardingM rep (Body (Aliases rep))
optimiseBody (Body als stms res) = do
  stms' <- deepen $ optimiseStms (stmsToList stms) $ mapM_ (seen . resSubExp) res
  pure $ Body als (stmsFromList stms') res
  where
    seen Constant {} = pure ()
    seen (Var v) = seenVar v

optimiseStms ::
  Constraints rep =>
  [Stm (Aliases rep)] ->
  ForwardingM rep () ->
  ForwardingM rep [Stm (Aliases rep)]
optimiseStms [] m = m >> pure []
optimiseStms (stm : stms) m = do
  (stms', bup) <- tapBottomUp $ bindingStm stm $ optimiseStms stms m
  stm' <- optimiseInStm stm
  -- XXX: unfortunate that we cannot handle duplicate update values.
  -- Would be good to improve this.  See inplacelowering6.fut.
  case nubByOrd (comparing updateValue)
    . filter (not . (`nameIn` bottomUpSeen bup) . updateSource) -- (9)
    . filter ((`elem` boundHere) . updateValue)
    $ forwardThese bup of
    [] -> do
      checkIfForwardableUpdate stm'
      pure $ stm' : stms'
    updates -> do
      lower <- asks topLowerUpdate
      scope <- askScope

      -- If we forward any updates, we need to remove them from stms'.
      let updated_names =
            map updateName updates
          notUpdated =
            not . any (`elem` updated_names) . patNames . stmPat

      -- Condition (5) and (7) are assumed to be checked by
      -- lowerUpdate.
      case lower scope stm' updates of
        Just lowering -> do
          new_stms <- lowering
          new_stms' <- optimiseStms new_stms $ tell bup {forwardThese = []}
          pure $ new_stms' ++ filter notUpdated stms'
        Nothing -> do
          checkIfForwardableUpdate stm'
          pure $ stm' : stms'
  where
    boundHere = patNames $ stmPat stm

    checkIfForwardableUpdate (Let pat (StmAux cs _ _) e)
      | Pat [PatElem v dec] <- pat,
        BasicOp (Update Unsafe src slice (Var ve)) <- e =
          maybeForward ve v dec cs src slice
    checkIfForwardableUpdate stm' =
      mapM_ seenVar $ namesToList $ freeIn $ stmExp stm'

optimiseInStm :: Constraints rep => Stm (Aliases rep) -> ForwardingM rep (Stm (Aliases rep))
optimiseInStm (Let pat dec e) =
  Let pat dec <$> optimiseExp e

optimiseExp :: Constraints rep => Exp (Aliases rep) -> ForwardingM rep (Exp (Aliases rep))
optimiseExp (DoLoop merge form body) =
  bindingScope (scopeOf form) . bindingFParams (map fst merge) $
    DoLoop merge form <$> optimiseBody body
optimiseExp (Op op) = do
  f <- asks topOnOp
  Op <$> f op
optimiseExp e = mapExpM optimise e
  where
    optimise =
      identityMapper
        { mapOnBody = const optimiseBody
        }

onSegOp ::
  (Buildable rep, CanBeAliased (Op rep)) =>
  SegOp lvl (Aliases rep) ->
  ForwardingM rep (SegOp lvl (Aliases rep))
onSegOp op =
  bindingScope (scopeOfSegSpace (segSpace op)) $ do
    let mapper = identitySegOpMapper {mapOnSegOpBody = onKernelBody}
        onKernelBody kbody = do
          stms <-
            deepen $
              optimiseStms (stmsToList (kernelBodyStms kbody)) $
                mapM_ seenVar $ namesToList $ freeIn $ kernelBodyResult kbody
          pure kbody {kernelBodyStms = stmsFromList stms}
    mapSegOpM mapper op

onMCOp :: OnOp MC
onMCOp (ParOp par_op op) = ParOp <$> traverse onSegOp par_op <*> onSegOp op
onMCOp op = pure op

onKernelOp :: OnOp GPU
onKernelOp (SegOp op) = SegOp <$> onSegOp op
onKernelOp op = pure op

data Entry rep = Entry
  { entryNumber :: Int,
    entryAliases :: Names,
    entryDepth :: Int,
    entryOptimisable :: Bool,
    entryType :: NameInfo (Aliases rep)
  }

type VTable rep = M.Map VName (Entry rep)

type OnOp rep = Op (Aliases rep) -> ForwardingM rep (Op (Aliases rep))

data TopDown rep = TopDown
  { topDownCounter :: Int,
    topDownTable :: VTable rep,
    topDownDepth :: Int,
    topLowerUpdate :: LowerUpdate rep (ForwardingM rep),
    topOnOp :: OnOp rep
  }

data BottomUp rep = BottomUp
  { bottomUpSeen :: Names,
    forwardThese :: [DesiredUpdate (LetDec (Aliases rep))]
  }

instance Semigroup (BottomUp rep) where
  BottomUp seen1 forward1 <> BottomUp seen2 forward2 =
    BottomUp (seen1 <> seen2) (forward1 <> forward2)

instance Monoid (BottomUp rep) where
  mempty = BottomUp mempty mempty

newtype ForwardingM rep a = ForwardingM (RWS (TopDown rep) (BottomUp rep) VNameSource a)
  deriving
    ( Monad,
      Applicative,
      Functor,
      MonadReader (TopDown rep),
      MonadWriter (BottomUp rep),
      MonadState VNameSource
    )

instance MonadFreshNames (ForwardingM rep) where
  getNameSource = get
  putNameSource = put

instance Constraints rep => HasScope (Aliases rep) (ForwardingM rep) where
  askScope = M.map entryType <$> asks topDownTable

runForwardingM ::
  LowerUpdate rep (ForwardingM rep) ->
  OnOp rep ->
  ForwardingM rep a ->
  VNameSource ->
  (a, VNameSource)
runForwardingM f g (ForwardingM m) src =
  let (x, src', _) = runRWS m emptyTopDown src
   in (x, src')
  where
    emptyTopDown =
      TopDown
        { topDownCounter = 0,
          topDownTable = M.empty,
          topDownDepth = 0,
          topLowerUpdate = f,
          topOnOp = g
        }

bindingParams ::
  (dec -> NameInfo (Aliases rep)) ->
  [Param dec] ->
  ForwardingM rep a ->
  ForwardingM rep a
bindingParams f params = local $ \(TopDown n vtable d x y) ->
  let entry fparam =
        ( paramName fparam,
          Entry n mempty d False $ f $ paramDec fparam
        )
      entries = M.fromList $ map entry params
   in TopDown (n + 1) (M.union entries vtable) d x y

bindingFParams ::
  [FParam (Aliases rep)] ->
  ForwardingM rep a ->
  ForwardingM rep a
bindingFParams = bindingParams FParamName

bindingScope ::
  Scope (Aliases rep) ->
  ForwardingM rep a ->
  ForwardingM rep a
bindingScope scope = local $ \(TopDown n vtable d x y) ->
  let entries = M.map entry scope
      infoAliases (LetName (aliases, _)) = unAliases aliases
      infoAliases _ = mempty
      entry info = Entry n (infoAliases info) d False info
   in TopDown (n + 1) (entries <> vtable) d x y

bindingStm ::
  Stm (Aliases rep) ->
  ForwardingM rep a ->
  ForwardingM rep a
bindingStm (Let pat _ _) = local $ \(TopDown n vtable d x y) ->
  let entries = M.fromList $ map entry $ patElems pat
      entry patElem =
        let (aliases, _) = patElemDec patElem
         in ( patElemName patElem,
              Entry n (unAliases aliases) d True $ LetName $ patElemDec patElem
            )
   in TopDown (n + 1) (M.union entries vtable) d x y

bindingNumber :: VName -> ForwardingM rep Int
bindingNumber name = do
  res <- asks $ fmap entryNumber . M.lookup name . topDownTable
  case res of
    Just n -> pure n
    Nothing ->
      error $
        "bindingNumber: variable "
          ++ pretty name
          ++ " not found."

deepen :: ForwardingM rep a -> ForwardingM rep a
deepen = local $ \env -> env {topDownDepth = topDownDepth env + 1}

areAvailableBefore :: Names -> VName -> ForwardingM rep Bool
areAvailableBefore names point = do
  pointN <- bindingNumber point
  nameNs <- mapM bindingNumber $ namesToList names
  pure $ all (< pointN) nameNs

isInCurrentBody :: VName -> ForwardingM rep Bool
isInCurrentBody name = do
  current <- asks topDownDepth
  res <- asks $ fmap entryDepth . M.lookup name . topDownTable
  case res of
    Just d -> pure $ d == current
    Nothing ->
      error $
        "isInCurrentBody: variable "
          ++ pretty name
          ++ " not found."

isOptimisable :: VName -> ForwardingM rep Bool
isOptimisable name = do
  res <- asks $ fmap entryOptimisable . M.lookup name . topDownTable
  case res of
    Just b -> pure b
    Nothing ->
      error $
        "isOptimisable: variable "
          ++ pretty name
          ++ " not found."

seenVar :: VName -> ForwardingM rep ()
seenVar name = do
  aliases <-
    asks $
      maybe mempty entryAliases
        . M.lookup name
        . topDownTable
  tell $ mempty {bottomUpSeen = oneName name <> aliases}

tapBottomUp :: ForwardingM rep a -> ForwardingM rep (a, BottomUp rep)
tapBottomUp m = do
  (x, bup) <- listen m
  pure (x, bup)

maybeForward ::
  Constraints rep =>
  VName ->
  VName ->
  LetDec (Aliases rep) ->
  Certs ->
  VName ->
  Slice SubExp ->
  ForwardingM rep ()
maybeForward v dest_nm dest_dec cs src slice = do
  -- Checks condition (2)
  available <-
    (freeIn src <> freeIn slice <> freeIn cs)
      `areAvailableBefore` v
  -- Check condition (3)
  samebody <- isInCurrentBody v
  -- Check condition (6)
  optimisable <- isOptimisable v
  not_prim <- not . primType <$> lookupType v
  when (available && samebody && optimisable && not_prim) $ do
    let fwd = DesiredUpdate dest_nm dest_dec cs src slice v
    tell mempty {forwardThese = [fwd]}
