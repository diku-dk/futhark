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
--    that both @x@ and @r@ must be bound in the same 'Body'.)
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
       (
         inPlaceLowering
       ) where

import Control.Monad.RWS
import qualified Data.Map.Strict as M

import Futhark.Analysis.Alias
import Futhark.Representation.Aliases
import Futhark.Representation.Kernels
import Futhark.Optimise.InPlaceLowering.LowerIntoStm
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Pass
import Futhark.Tools (fullSlice)

-- | Apply the in-place lowering optimisation to the given program.
inPlaceLowering :: Pass Kernels Kernels
inPlaceLowering =
  Pass "In-place lowering" "Lower in-place updates into loops" $
  fmap removeProgAliases .
  intraproceduralTransformation optimiseFunDef .
  aliasAnalysis

optimiseFunDef :: MonadFreshNames m => FunDef (Aliases Kernels)
               -> m (FunDef (Aliases Kernels))
optimiseFunDef fundec =
  modifyNameSource $ runForwardingM lowerUpdateKernels onKernelOp $
  bindingFParams (funDefParams fundec) $ do
    body <- optimiseBody $ funDefBody fundec
    return $ fundec { funDefBody = body }

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
  case filter ((`elem` boundHere) . updateValue) $
       forwardThese bup of
    [] -> checkIfForwardableUpdate bnd' bnds'
    updates -> do
      let updateStms = map updateStm updates
      lower <- asks lowerUpdate
      scope <- askScope
      -- Condition (5) and (7) are assumed to be checked by
      -- lowerUpdate.
      case lower scope bnd' updates of
        Just lowering -> do new_bnds <- lowering
                            new_bnds' <- optimiseStms new_bnds $
                                         tell bup { forwardThese = [] }
                            return $ new_bnds' ++ bnds'
        Nothing       -> checkIfForwardableUpdate bnd' $
                         updateStms ++ bnds'

  where boundHere = patternNames $ stmPattern bnd

        checkIfForwardableUpdate bnd'@(Let (Pattern [] [PatElem v attr])
                                       (StmAux cs _) e) bnds'
            | BasicOp (Update src (DimFix i:slice) (Var ve)) <- e,
              slice == drop 1 (fullSlice (typeOf attr) [DimFix i]) = do
                forwarded <- maybeForward ve v attr cs src i
                return $ if forwarded
                         then bnds'
                         else bnd' : bnds'
        checkIfForwardableUpdate bnd' bnds' =
          return $ bnd' : bnds'

optimiseInStm :: Constraints lore => Stm (Aliases lore) -> ForwardingM lore (Stm (Aliases lore))
optimiseInStm (Let pat attr e) =
  Let pat attr <$> optimiseExp e

optimiseExp :: Constraints lore => Exp (Aliases lore) -> ForwardingM lore (Exp (Aliases lore))
optimiseExp (DoLoop ctx val form body) =
  bindingScope (scopeOf form) $
  bindingFParams (map fst $ ctx ++ val) $
  DoLoop ctx val form <$> optimiseBody body
optimiseExp (Op op) = do
  f <- asks onOp
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
                            , lowerUpdate :: LowerUpdate lore (ForwardingM lore)
                            , onOp :: OnOp lore
                            }

data BottomUp lore = BottomUp { bottomUpSeen :: Names
                              , forwardThese :: [DesiredUpdate (LetAttr (Aliases lore))]
                              }

instance Semigroup (BottomUp lore) where
  BottomUp seen1 forward1 <> BottomUp seen2 forward2 =
    BottomUp (seen1 <> seen2) (forward1 <> forward2)

instance Monoid (BottomUp lore) where
  mempty = BottomUp mempty mempty

updateStm :: Constraints lore => DesiredUpdate (LetAttr (Aliases lore)) -> Stm (Aliases lore)
updateStm fwd =
  mkLet [] [Ident (updateName fwd) $ typeOf $ updateType fwd] $
  BasicOp $ Update (updateSource fwd)
  (fullSlice (typeOf $ updateType fwd) $ updateIndices fwd) $
  Var $ updateValue fwd

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
                               , lowerUpdate = f
                               , onOp = g
                               }

bindingParams :: (attr -> NameInfo (Aliases lore))
              -> [Param attr]
               -> ForwardingM lore a
               -> ForwardingM lore a
bindingParams f params = local $ \(TopDown n vtable d x y) ->
  let entry fparam =
        (paramName fparam,
         Entry n mempty d False $ f $ paramAttr fparam)
      entries = M.fromList $ map entry params
  in TopDown (n+1) (M.union entries vtable) d x y

bindingFParams :: [FParam (Aliases lore)]
               -> ForwardingM lore a
               -> ForwardingM lore a
bindingFParams = bindingParams FParamInfo

bindingScope :: Scope (Aliases lore)
             -> ForwardingM lore a
             -> ForwardingM lore a
bindingScope scope = local $ \(TopDown n vtable d x y) ->
  let entries = M.map entry scope
      infoAliases (LetInfo (aliases, _)) = unNames aliases
      infoAliases _ = mempty
      entry info = Entry n (infoAliases info) d False info
  in TopDown (n+1) (entries<>vtable) d x y

bindingStm :: Stm (Aliases lore)
           -> ForwardingM lore a
           -> ForwardingM lore a
bindingStm (Let pat _ _) = local $ \(TopDown n vtable d x y) ->
  let entries = M.fromList $ map entry $ patternElements pat
      entry patElem =
        let (aliases, _) = patElemAttr patElem
        in (patElemName patElem,
            Entry n (unNames aliases) d True $ LetInfo $ patElemAttr patElem)
  in TopDown (n+1) (M.union entries vtable) d x y

bindingNumber :: VName -> ForwardingM lore Int
bindingNumber name = do
  res <- asks $ fmap entryNumber . M.lookup name . topDownTable
  case res of Just n  -> return n
              Nothing -> error $ "bindingNumber: variable " ++
                         pretty name ++ " not found."

deepen :: ForwardingM lore a -> ForwardingM lore a
deepen = local $ \env -> env { topDownDepth = topDownDepth env + 1 }

areAvailableBefore :: [SubExp] -> VName -> ForwardingM lore Bool
areAvailableBefore ses point = do
  pointN <- bindingNumber point
  nameNs <- mapM bindingNumber $ subExpVars ses
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
             -> VName -> LetAttr (Aliases lore) -> Certificates -> VName -> SubExp
             -> ForwardingM lore Bool
maybeForward v dest_nm dest_attr cs src i = do
  -- Checks condition (2)
  available <- [i,Var src] `areAvailableBefore` v
  -- ...subcondition, the certificates must also.
  certs_available <- map Var (namesToList $ freeIn cs) `areAvailableBefore` v
  -- Check condition (3)
  samebody <- isInCurrentBody v
  -- Check condition (6)
  optimisable <- isOptimisable v
  not_prim <- not . primType <$> lookupType v
  if available && certs_available && samebody && optimisable && not_prim then do
    let fwd = DesiredUpdate dest_nm dest_attr cs src [DimFix i] v
    tell mempty { forwardThese = [fwd] }
    return True
    else return False
