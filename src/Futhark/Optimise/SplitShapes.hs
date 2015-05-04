{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
-- | For every function with an existential return shape, try to see
-- if we can extract an efficient shape slice.  If so, replace every
-- call of the original function with a function to the shape and
-- value slices.
module Futhark.Optimise.SplitShapes
       (splitShapes)
where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM
import Data.Maybe

import Prelude

import Futhark.Representation.Basic
import Futhark.Tools
import Futhark.MonadFreshNames
import Futhark.Renamer
import Futhark.Substitute
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.DeadVarElim

-- | Perform the transformation on a program.
splitShapes :: Prog -> Prog
splitShapes prog =
  Prog { progFunctions = runSplitM m HM.empty $ newNameSourceForProg prog }
  where m = do let origfuns = progFunctions prog
               (substs, newfuns) <-
                 unzip <$> map extract <$>
                 makeFunSubsts origfuns
               mapM (substCalls substs) $ origfuns ++ concat newfuns
        extract (fname, (shapefun, valfun)) =
          ((fname, (funDecName shapefun, funDecRetType shapefun,
                    funDecName valfun, funDecRetType valfun)),
           [shapefun, valfun])

newtype SplitM a = SplitM (ReaderT TypeEnv
                           (State VNameSource)
                           a)
                 deriving (Applicative, Functor, Monad,
                           MonadReader TypeEnv,
                           MonadState VNameSource,
                           MonadFreshNames,
                           HasTypeEnv)

runSplitM :: SplitM a -> TypeEnv -> VNameSource -> a
runSplitM (SplitM m) =
  evalState . runReaderT m

makeFunSubsts :: (MonadFreshNames m, HasTypeEnv m) =>
                 [FunDec] -> m [(Name, (FunDec, FunDec))]
makeFunSubsts fundecs =
  cheapSubsts <$>
  zip (map funDecName fundecs) <$>
  mapM (simplifyShapeFun' <=< functionSlices) fundecs
  where simplifyShapeFun' (shapefun, valfun) = do
          shapefun' <- simplifyShapeFun shapefun
          return (shapefun', valfun)

-- | Returns shape slice and value slice.  The shape slice duplicates
-- the entire value slice - you should try to simplify it, and see if
-- it's "cheap", in some sense.
functionSlices :: (MonadFreshNames m, HasTypeEnv m) =>
                  FunDec -> m (FunDec, FunDec)
functionSlices (FunDec fname rettype params body@(Body _ bodybnds bodyres)) = do
  -- The shape function should not consume its arguments - if it wants
  -- to do in-place stuff, it needs to copy them first.  In most
  -- cases, these copies will be removed by the simplifier.
  ((shapeParams, cpybnds),_) <- runBinderEmptyEnv $ nonuniqueParams params

  -- Give names to the existentially quantified sizes of the return
  -- type.  These will be passed as parameters to the value function.
  (staticRettype, shapeidents) <-
    runWriterT $
    instantiateShapes instantiate $ retTypeValues rettype

  shapes <- subExpShapeContext (retTypeValues rettype) bodyres
  shapetypes <- mapM subExpType shapes

  valueBody <- substituteExtResultShapes staticRettype body

  let valueRettype = ExtRetType $ staticShapes staticRettype
      valueParams = shapeidents ++ map paramIdent params
      shapeBody = mkBody (cpybnds <> bodybnds) shapes
      mkFParam = flip Param ()
      fShape = FunDec shapeFname (ExtRetType $ staticShapes shapetypes)
               shapeParams
               shapeBody
      fValue = FunDec valueFname valueRettype
               (map mkFParam valueParams)
               valueBody
  return (fShape, fValue)
  where shapeFname = fname <> nameFromString "_shape"
        valueFname = fname <> nameFromString "_value"

        instantiate _ = do v <- lift $ newIdent "precomp_shape" (Basic Int)
                           tell [v]
                           return $ Var $ identName v

substituteExtResultShapes :: (MonadFreshNames m, HasTypeEnv m) =>
                             [Type] -> Body -> m Body
substituteExtResultShapes rettype (Body _ bnds res) = do
  compshapes <- typesShapes <$> mapM subExpType res
  let subst = HM.fromList $ mapMaybe isSubst $ zip compshapes $ typesShapes rettype
  bnds' <- mapM (substInBnd subst) bnds
  let res' = map (substituteNames subst) res
  return $ mkBody bnds' res'
  where typesShapes = concatMap (shapeDims . arrayShape)
        isSubst (Var v1, Var v2) = Just (v1, v2)
        isSubst _                = Nothing

        substInBnd subst (Let pat _ e) =
          mkLet' [] <$> mapM (substInBnd' subst) (patternIdents pat) <*>
          pure (substituteNames subst e)
        substInBnd' subst v
          | identName v' `HM.member` subst = newIdent' (<>"unused") v'
          | otherwise                      = return v'
          where v' = v { identType = substituteNames subst $ identType v }

simplifyShapeFun :: MonadFreshNames m => FunDec -> m FunDec
simplifyShapeFun shapef = return . deadCodeElimFun =<< simplifyFun' =<<
                          return . deadCodeElimFun =<< simplifyFun' =<<
                          return . deadCodeElimFun =<< simplifyFun' =<<
                          return . deadCodeElimFun =<< simplifyFun' =<<
                          return . deadCodeElimFun =<< simplifyFun' =<<
                          return . deadCodeElimFun =<< simplifyFun' =<<
                          renameFun shapef
  where simplifyFun' = simplifyFunWithRules bindableSimpleOps basicRules

cheapFun :: FunDec -> Bool
cheapFun  = cheapBody . funDecBody
  where cheapBody (Body _ bnds _) = all cheapBinding bnds
        cheapBinding (Let _ _ e) = cheap e
        cheap (LoopOp {}) = False
        cheap (Apply {}) = False
        cheap (If _ tbranch fbranch _) = cheapBody tbranch && cheapBody fbranch
        cheap _ = True

cheapSubsts :: [(Name, (FunDec, FunDec))] -> [(Name, (FunDec, FunDec))]
cheapSubsts = filter (cheapFun . fst . snd)
              -- Probably too simple.  We might want to inline first.

substCalls :: MonadFreshNames m =>
              [(Name, (Name, RetType, Name, RetType))]
           -> FunDec -> m FunDec
substCalls subst fundec = do
  fbody' <- treatBody $ funDecBody fundec
  return fundec { funDecBody = fbody' }
  where treatBody (Body _ bnds res) = do
          bnds' <- mapM treatBinding bnds
          return $ mkBody (concat bnds') res
        treatLambda lam = do
          body <- treatBody $ lambdaBody lam
          return $ lam { lambdaBody = body }

        treatBinding (Let pat _ (Apply fname args _))
          | Just (shapefun,shapetype,valfun,_) <- lookup fname subst =
            liftM snd . runBinderEmptyEnv $ do
              let (vs,vals) =
                    splitAt (length $ retTypeValues shapetype) $
                    patternElements pat
              letBind_ (Pattern [] vs) $
                Apply shapefun args shapetype
              letBind_ (Pattern [] vals) $
                Apply valfun ([(Var $ patElemName v,Observe) | v <- vs]++args)
                (ExtRetType $ staticShapes $ map patElemType vals)

        treatBinding (Let pat _ e) = do
          e' <- mapExpM mapper e
          return [mkLet' [] (patternIdents pat) e']
          where mapper = identityMapper { mapOnBody = treatBody
                                        , mapOnLambda = treatLambda
                                        }
