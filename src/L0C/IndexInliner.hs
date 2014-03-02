{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | A transformation pass that inlines instances of @a[i]@, when @a@
-- is directly the result of a 'mapT'.  Beware: The resulting program
-- is not uniquely named.
module L0C.IndexInliner
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

import L0C.InternalRep
import L0C.MonadFreshNames
import L0C.NeedNames
import L0C.Tools

-- | Perform the transformation.  Never fails.
transformProg :: Prog -> Prog
transformProg prog =
  let src = newNameSourceForProg prog
  in Prog $ fst $ runInlinerM src $ mapM transformFun $ progFunctions prog

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
transformBodyM (LetPat pat e@(Map cs fun es mloc) body loc) = do
  fun' <- transformLambdaM fun
  dels <- performArrayDelays pat e
  body' <- local (HM.union dels) $ transformBodyM body
  return $ LetPat pat (Map cs fun' es mloc) body' loc
transformBodyM (LetPat pat e body loc) = do
  env <- ask
  (e',f) <- runBinder' $ do
              es <- transformExp env e
              case es of
                Left e'   -> return e'
                Right es' -> return $ TupLit es' loc
  f <$> (LetPat pat e' <$> transformBodyM body <*> pure loc)
transformBodyM body = mapBodyM transform body
  where transform = identityMapper {
                      mapOnBody   = mapBodyM transform
                    }

transformExp :: ArrayIndexMap -> Exp -> Binder (Either Exp [SubExp])

transformExp m e@(Index cs v idxcs (idx:idxs) _) =
  case me of
    Nothing -> return $ Left e
    Just e' -> do
     b <- provideNames e'
     case idxs of
       _ | cheapBody b -> do e'' <- bodyBind b
                             return $ Right e''
       _               -> return $ Left e
  where me = lookupDelayed v cs idxcs idx m

transformExp _ e = Left <$> mapExpM transform e
  where transform = identityMapper {
                      mapOnLambda   = runInlinerM' . transformLambdaM
                    }

transformLambdaM :: Lambda -> InlinerM Lambda
transformLambdaM l = do
  e <- transformBodyM $ lambdaBody l
  return l { lambdaBody = e }

type ArrayIndexMap = HM.HashMap Ident
                     (Certificates -> Certificates -> SubExp -> NeedNames Body)

emptyArrayIndexMap :: ArrayIndexMap
emptyArrayIndexMap = HM.empty

lookupDelayed :: Ident -> Certificates -> Certificates -> SubExp
              -> ArrayIndexMap
              -> Maybe (NeedNames Body)
lookupDelayed var cs idxcs idx m = do
  f <- HM.lookup var m
  return $ f cs idxcs idx

performArrayDelay :: Exp -> Maybe (Certificates -> Certificates -> SubExp -> NeedNames Body)
performArrayDelay (Map cs fun es _) = do
  vs <- mapM varExp es
  Just $ performArrayDelay' cs fun vs
  where varExp (Var v) = Just v
        varExp _       = Nothing
performArrayDelay _ = Nothing

performArrayDelay' :: Certificates -> Lambda -> [Ident]
                   -> Certificates -> Certificates -> SubExp -> NeedNames Body
performArrayDelay' mcs fun vs cs idxcs idx =
  return $ inlineMapFun (lambdaParams fun)
                        [Index (mcs++cs) v idxcs [idx] $ srclocOf v
                           | v <- vs ]
  where inlineMapFun (p:ps) (ie:ies) =
          LetPat [fromParam p] ie (inlineMapFun ps ies) $ srclocOf p
        inlineMapFun _ _ =
          lambdaBody fun

performArrayDelays :: MonadFreshNames m => [Ident] -> Exp -> m ArrayIndexMap
performArrayDelays pat e = do
  bnds <- forM pat $ \name ->
            newIdent (baseString $ identName name)
                     (stripArray 1 $ identType name)
                     (srclocOf name)

  let dels f bnd cs ics is = do
        fb <- f cs ics is
        let loc = srclocOf fb
        runBinder $ do
          es <- bodyBind fb
          return $ LetPat bnds (TupLit es loc) (Result [Var bnd] loc) loc
  case performArrayDelay e of
    Nothing -> return emptyArrayIndexMap
    Just f ->
      return $ HM.fromList [ (name, dels f bnd)
                             | (name,bnd) <- zip pat bnds ]

cheapBody :: Body -> Bool
cheapBody (LetPat _ e body _) = cheapExp e && cheapBody body
cheapBody (Result _ _)        = True
cheapBody _ = False

cheapExp :: Exp -> Bool
cheapExp (BinOp {}) = True
cheapExp (Not {}) = True
cheapExp (Negate {}) = True
cheapExp (TupLit {}) = True
cheapExp (Index {}) = True
cheapExp (SubExp {}) = True
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
runInlinerM' m = do
  src <- getNameSource
  let (x, src') = runInlinerM src m
  putNameSource src'
  return x
