{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module implements an optimisation that moves in-place
-- updates into/before loops where possible, with the end goal of
-- minimising memory copies.  As an example, consider this program:
--
-- @
--   let r =
--     loop (r1 = r0) = for i < n do
--       let a = r1[i] in
--       let r1[i] = a * i in
--       r1
--       in
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
--     let a = a[k,i] in
--     let x[k,i] = a * i in
--     x
--     in
--   let r = x[y] in
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
-- FIXME: the implementation is not finished yet.  Specifically, not
-- all of the above conditions are checked.
module Futhark.Optimise.InPlaceLowering
       ( inPlaceLoweringKernels
       , inPlaceLoweringSeq
       )
where

import Control.Monad.RWS
import qualified Data.Map.Strict as M

import Futhark.Analysis.Alias
import Futhark.Representation.Aliases
import Futhark.Representation.Kernels
import Futhark.Representation.Seq (Seq)
import Futhark.Optimise.InPlaceLowering.LowerIntoStm
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Pass

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLoweringKernels :: Pass Kernels Kernels
inPlaceLoweringKernels = inPlaceLowering onKernelOp lowerUpdateKernels

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLoweringSeq :: Pass Seq Seq
inPlaceLoweringSeq = inPlaceLowering pure lowerUpdate

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLowering :: Constraints lore =>
                   OnOp lore -> LowerUpdate lore (ForwardingM lore)
                -> Pass lore lore
inPlaceLowering onOp lower =
  Pass "In-place lowering" "Lower in-place updates into loops" $
  fmap removeProgAliases .
  intraproceduralTransformationWithConsts optimiseConsts optimiseFunDef .
  aliasAnalysis
  where optimiseConsts stms =
          modifyNameSource $ runForwardingM lower onOp $
          stmsFromList <$> optimiseStms (stmsToList stms) (pure ())

        optimiseFunDef consts fundec =
          modifyNameSource $ runForwardingM lower onOp $
          descend (stmsToList consts) $ bindingFParams (funDefParams fundec) $ do
          body <- optimiseBody $ funDefBody fundec
          return $ fundec { funDefBody = body }

        descend [] m = m
        descend (stm:stms) m = bindingStm stm $ descend stms m

type Constraints lore = (Bindable lore, CanBeAliased (Op lore))

optimiseBody :: Constraints lore =>
                Body (Aliases lore) -> ForwardingM lore (Body (Aliases lore))
optimiseBody (Body als bnds res) = do
  bnds' <- deepen $ optimiseStms (stmsToList bnds) $
    mapM_ seen res
  return $ Body als (stmsFromList bnds') res
  where seen Constant{} = return ()
        seen (Var v)    = seenVar v

optimiseStms :: Constraints lore =>
                [Stm (Aliases lore)] -> ForwardingM lore ()
             -> ForwardingM lore [Stm (Aliases lore)]
optimiseStms [] m = m >> return []

optimiseStms (bnd:bnds) m = do
  (bnds', bup) <- tapBottomUp $ bindingStm bnd $ optimiseStms bnds m
  bnd' <- optimiseInStm bnd
  case filter ((`elem` boundHere) . updateValue) $ forwardThese bup of
    [] -> do checkIfForwardableUpdate bnd'
             return $ bnd':bnds'
    updates -> do
      lower <- asks topLowerUpdate
      scope <- askScope

      -- If we forward any updates, we need to remove them from bnds'.
      let updated_names =
            map updateName updates
          notUpdated =
            not . any (`elem` updated_names) . patternNames . stmPattern

      -- Condition (5) and (7) are assumed to be checked by
      -- lowerUpdate.
      case lower scope bnd' updates of
        Just lowering -> do new_bnds <- lowering
                            new_bnds' <- optimiseStms new_bnds $
                                         tell bup { forwardThese = [] }
                            return $ new_bnds' ++ filter notUpdated bnds'
        Nothing       -> do checkIfForwardableUpdate bnd'
                            return $ bnd':bnds'

  where boundHere = patternNames $ stmPattern bnd

        checkIfForwardableUpdate (Let pat (StmAux cs _) e)
            | Pattern [] [PatElem v dec] <- pat,
              BasicOp (Update src slice (Var ve)) <- e =
                maybeForward ve v dec cs src slice
        checkIfForwardableUpdate _ = return ()

optimiseInStm :: Constraints lore => Stm (Aliases lore) -> ForwardingM lore (Stm (Aliases lore))
optimiseInStm (Let pat dec e) =
  Let pat dec <$> optimiseExp e

optimiseExp :: Constraints lore => Exp (Aliases lore) -> ForwardingM lore (Exp (Aliases lore))
optimiseExp (DoLoop ctx val form body) =
  bindingScope (scopeOf form) $
  bindingFParams (map fst $ ctx ++ val) $
  DoLoop ctx val form <$> optimiseBody body
optimiseExp (Op op) = do
  f <- asks topOnOp
  Op <$> f op
optimiseExp e = mapExpM optimise e
  where optimise = identityMapper { mapOnBody = const optimiseBody
                                  }
onKernelOp :: OnOp Kernels
onKernelOp (SegOp op) =
  bindingScope (scopeOfSegSpace (segSpace op)) $ do
    let mapper = identitySegOpMapper { mapOnSegOpBody = onKernelBody }
        onKernelBody kbody = do
          stms <- deepen $ optimiseStms (stmsToList (kernelBodyStms kbody)) $
                  mapM_ seenVar $ namesToList $ freeIn $ kernelBodyResult kbody
          return kbody { kernelBodyStms = stmsFromList stms }
    SegOp <$> mapSegOpM mapper op
onKernelOp op = return op

data Entry lore = Entry { entryNumber :: Int
                        , entryAliases :: Names
                        , entryDepth :: Int
                        , entryOptimisable :: Bool
                        , entryType :: NameInfo (Aliases lore)
                        }

type VTable lore = M.Map VName (Entry lore)

type OnOp lore = Op (Aliases lore) -> ForwardingM lore (Op (Aliases lore))

data TopDown lore = TopDown { topDownCounter :: Int
                            , topDownTable :: VTable lore
                            , topDownDepth :: Int
                            , topLowerUpdate :: LowerUpdate lore (ForwardingM lore)
                            , topOnOp :: OnOp lore
                            }

data BottomUp lore = BottomUp { bottomUpSeen :: Names
                              , forwardThese :: [DesiredUpdate (LetDec (Aliases lore))]
                              }

instance Semigroup (BottomUp lore) where
  BottomUp seen1 forward1 <> BottomUp seen2 forward2 =
    BottomUp (seen1 <> seen2) (forward1 <> forward2)

instance Monoid (BottomUp lore) where
  mempty = BottomUp mempty mempty

newtype ForwardingM lore a = ForwardingM (RWS (TopDown lore) (BottomUp lore) VNameSource a)
                      deriving (Monad, Applicative, Functor,
                                MonadReader (TopDown lore),
                                MonadWriter (BottomUp lore),
                                MonadState VNameSource)

instance MonadFreshNames (ForwardingM lore) where
  getNameSource = get
  putNameSource = put

instance Constraints lore => HasScope (Aliases lore) (ForwardingM lore) where
  askScope = M.map entryType <$> asks topDownTable

runForwardingM :: LowerUpdate lore (ForwardingM lore) -> OnOp lore -> ForwardingM lore a
               -> VNameSource -> (a, VNameSource)
runForwardingM f g (ForwardingM m) src = let (x, src', _) = runRWS m emptyTopDown src
                                         in (x, src')
  where emptyTopDown = TopDown { topDownCounter = 0
                               , topDownTable = M.empty
                               , topDownDepth = 0
                               , topLowerUpdate = f
                               , topOnOp = g
                               }

bindingParams :: (dec -> NameInfo (Aliases lore))
              -> [Param dec]
               -> ForwardingM lore a
               -> ForwardingM lore a
bindingParams f params = local $ \(TopDown n vtable d x y) ->
  let entry fparam =
        (paramName fparam,
         Entry n mempty d False $ f $ paramDec fparam)
      entries = M.fromList $ map entry params
  in TopDown (n+1) (M.union entries vtable) d x y

bindingFParams :: [FParam (Aliases lore)]
               -> ForwardingM lore a
               -> ForwardingM lore a
bindingFParams = bindingParams FParamName

bindingScope :: Scope (Aliases lore)
             -> ForwardingM lore a
             -> ForwardingM lore a
bindingScope scope = local $ \(TopDown n vtable d x y) ->
  let entries = M.map entry scope
      infoAliases (LetName (aliases, _)) = unNames aliases
      infoAliases _ = mempty
      entry info = Entry n (infoAliases info) d False info
  in TopDown (n+1) (entries<>vtable) d x y

bindingStm :: Stm (Aliases lore)
           -> ForwardingM lore a
           -> ForwardingM lore a
bindingStm (Let pat _ _) = local $ \(TopDown n vtable d x y) ->
  let entries = M.fromList $ map entry $ patternElements pat
      entry patElem =
        let (aliases, _) = patElemDec patElem
        in (patElemName patElem,
            Entry n (unNames aliases) d True $ LetName $ patElemDec patElem)
  in TopDown (n+1) (M.union entries vtable) d x y

bindingNumber :: VName -> ForwardingM lore Int
bindingNumber name = do
  res <- asks $ fmap entryNumber . M.lookup name . topDownTable
  case res of Just n  -> return n
              Nothing -> error $ "bindingNumber: variable " ++
                         pretty name ++ " not found."

deepen :: ForwardingM lore a -> ForwardingM lore a
deepen = local $ \env -> env { topDownDepth = topDownDepth env + 1 }

areAvailableBefore :: Names -> VName -> ForwardingM lore Bool
areAvailableBefore names point = do
  pointN <- bindingNumber point
  nameNs <- mapM bindingNumber $ namesToList names
  return $ all (< pointN) nameNs

isInCurrentBody :: VName -> ForwardingM lore Bool
isInCurrentBody name = do
  current <- asks topDownDepth
  res <- asks $ fmap entryDepth . M.lookup name . topDownTable
  case res of Just d  -> return $ d == current
              Nothing -> error $ "isInCurrentBody: variable " ++
                         pretty name ++ " not found."

isOptimisable :: VName -> ForwardingM lore Bool
isOptimisable name = do
  res <- asks $ fmap entryOptimisable . M.lookup name . topDownTable
  case res of Just b  -> return b
              Nothing -> error $ "isOptimisable: variable " ++
                         pretty name ++ " not found."

seenVar :: VName -> ForwardingM lore ()
seenVar name = do
  aliases <- asks $
             maybe mempty entryAliases .
             M.lookup name . topDownTable
  tell $ mempty { bottomUpSeen = oneName name <> aliases }

tapBottomUp :: ForwardingM lore a -> ForwardingM lore (a, BottomUp lore)
tapBottomUp m = do (x,bup) <- listen m
                   return (x, bup)

maybeForward :: Constraints lore =>
                VName
             -> VName -> LetDec (Aliases lore)
             -> Certificates -> VName -> Slice SubExp
             -> ForwardingM lore ()
maybeForward v dest_nm dest_dec cs src slice = do
  -- Checks condition (2)
  available <- (freeIn src <> freeIn slice <> freeIn cs)
               `areAvailableBefore` v
  -- Check condition (3)
  samebody <- isInCurrentBody v
  -- Check condition (6)
  optimisable <- isOptimisable v
  not_prim <- not . primType <$> lookupType v
  when (available && samebody && optimisable && not_prim) $ do
    let fwd = DesiredUpdate dest_nm dest_dec cs src slice v
    tell mempty { forwardThese = [fwd] }
