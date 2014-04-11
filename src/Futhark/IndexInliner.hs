{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A transformation pass that inlines instances of @a[i]@, when @a@
-- is directly the result of a 'mapT'.  Beware: The resulting program
-- is not uniquely named.
module Futhark.IndexInliner
  ( transformProg
  , transformBody
  , transformLambda
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Loc
import qualified Data.HashMap.Strict as HM

import Futhark.InternalRep
import Futhark.InternalRep.Renamer
import Futhark.MonadFreshNames
import Futhark.NeedNames
import Futhark.Tools

-- | Perform the transformation.  Never fails.
transformProg :: Prog -> Prog
transformProg prog =
  let src = newNameSourceForProg prog
  in renameProg $ Prog $ fst $ runInlinerM src $ mapM transformFun $ progFunctions prog

-- | Transform just a single expression.
transformBody :: NameSource VName -> Body -> (Body, NameSource VName)
transformBody src = runInlinerM src . transformBodyM

-- | Transform just a single lambda.
transformLambda :: NameSource VName -> Lambda -> (Lambda, NameSource VName)
transformLambda src l =
  let (e, src') = transformBody src $ lambdaBody l
  in (l { lambdaBody = e }, src')

transformFun :: FunDec -> InlinerM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- transformBodyM body
  return (fname, rettype, params, body', loc)

transformBodyM :: Body -> InlinerM Body
transformBodyM (Body (Let pat e@(Map cs fun es mloc):bnds) res) = do
  fun' <- transformLambdaM fun
  dels <- performArrayDelays pat e
  Body bnds' res' <- local (HM.union dels) $ transformBodyM $ Body bnds res
  return $ Body (Let pat (Map cs fun' es mloc):bnds') res'
transformBodyM (Body (Let pat e:bnds) res) = do
  env <- ask
  (e',f) <- runBinder' $ do
              es <- transformExp env e
              case es of
                Left e'   -> return e'
                Right es' -> return $ SubExps es' $ srclocOf e
  f <$> ((Let pat e' `insertBinding`) <$>
         transformBodyM (Body bnds res))
transformBodyM body = mapBodyM transform body
  where transform = identityMapper {
                      mapOnBody   = mapBodyM transform
                    }

transformExp :: ArrayIndexMap -> Exp -> Binder (Either Exp [SubExp])

transformExp m e@(Index cs v (idx:idxs) _) =
  case me of
    Nothing -> return $ Left e
    Just e' -> do
     b <- provideNames e'
     case idxs of
       _ | cheapBody b -> do e'' <- bodyBind b
                             return $ Right e''
       _               -> return $ Left e
  where me = lookupDelayed v cs idx m

transformExp _ e = Left <$> mapExpM transform e
  where transform = identityMapper {
                      mapOnLambda   = runInlinerM' . transformLambdaM
                    }

transformLambdaM :: Lambda -> InlinerM Lambda
transformLambdaM l = do
  e <- transformBodyM $ lambdaBody l
  return l { lambdaBody = e }

type ArrayIndexMap = HM.HashMap Ident
                     (Certificates -> SubExp -> NeedNames Body)

emptyArrayIndexMap :: ArrayIndexMap
emptyArrayIndexMap = HM.empty

lookupDelayed :: Ident -> Certificates -> SubExp
              -> ArrayIndexMap
              -> Maybe (NeedNames Body)
lookupDelayed var cs idx m = do
  f <- HM.lookup var m
  return $ f cs idx

performArrayDelay :: Exp -> Maybe (Certificates -> SubExp -> NeedNames Body)
performArrayDelay (Map cs fun es _) = do
  vs <- mapM varExp es
  Just $ performArrayDelay' cs fun vs
  where varExp (Var v) = Just v
        varExp _       = Nothing
performArrayDelay _ = Nothing

performArrayDelay' :: Certificates -> Lambda -> [Ident]
                   -> Certificates -> SubExp -> NeedNames Body
performArrayDelay' mcs fun vs cs idx =
  return $ inlineMapFun (lambdaParams fun)
                        [Index (mcs++cs) v [idx] $ srclocOf v
                           | v <- vs ]
  where inlineMapFun (p:ps) (ie:ies) =
          Let [fromParam p] ie `insertBinding` inlineMapFun ps ies
        inlineMapFun _ _ =
          lambdaBody fun

performArrayDelays :: MonadFreshNames m => [Ident] -> Exp -> m ArrayIndexMap
performArrayDelays pat e = do
  bnds <- forM pat $ \name ->
            newIdent (baseString $ identName name)
                     (stripArray 1 $ identType name)
                     (srclocOf name)

  let dels f bnd cs is = do
        fb <- f cs is
        let loc = srclocOf fb
        runBinder $ do
          es <- bodyBind fb
          return $ Body [Let bnds $ SubExps es loc] $
                   Result [] [Var bnd] loc
  case performArrayDelay e of
    Nothing -> return emptyArrayIndexMap
    Just f ->
      return $ HM.fromList [ (name, dels f bnd)
                             | (name,bnd) <- zip pat bnds ]

cheapBody :: Body -> Bool
cheapBody (Body bnds _) = all cheapBind bnds
  where cheapBind (Let _ e) = cheapExp e

cheapExp :: Exp -> Bool
cheapExp (BinOp {}) = True
cheapExp (Not {}) = True
cheapExp (Negate {}) = True
cheapExp (SubExps {}) = True
cheapExp (Index {}) = True
cheapExp _ = False

newtype InlinerM a = InlinerM (StateT (NameSource VName) (Reader ArrayIndexMap) a)
  deriving (Functor, Applicative, Monad,
            MonadReader ArrayIndexMap, MonadState VNameSource)

instance MonadFreshNames InlinerM where
  getNameSource = get
  putNameSource = put

runInlinerM :: NameSource VName -> InlinerM a -> (a, NameSource VName)
runInlinerM src (InlinerM m) = runReader (runStateT m src) emptyArrayIndexMap

runInlinerM' :: MonadFreshNames m => InlinerM a -> m a
runInlinerM' = modifyNameSource . flip runInlinerM
