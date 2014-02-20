{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | A transformation pass that inlines instances of @a[i]@, when @a@
-- is directly the result of a 'mapT'.  Beware: The resulting program
-- is not uniquely named.
module L0C.IndexInliner
  ( transformProg
  , transformExp
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

-- | Perform the transformation.  Never fails.
transformProg :: Prog -> Prog
transformProg prog =
  let src = newNameSourceForProg prog
  in Prog $ fst $ runInlinerM src $ mapM transformFun $ progFunctions prog

-- | Transform just a single expression.
transformExp :: NameSource VName -> Exp -> (Exp, NameSource VName)
transformExp src = runInlinerM src . transformExpM

-- | Transform just a single lambda.
transformLambda :: NameSource VName -> Lambda -> (Lambda, NameSource VName)
transformLambda src l =
  let (e, src') = transformExp src $ lambdaBody l
  in (l { lambdaBody = e }, src')

transformFun :: FunDec -> InlinerM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- transformExpM body
  return (fname, rettype, params, body', loc)

transformExpM :: Exp -> InlinerM Exp

transformExpM (LetPat pat e@(Map cs fun es mloc) body loc) = do
  fun' <- transformLambdaM fun
  dels <- performArrayDelays pat e
  body' <- local (HM.union dels) $ transformExpM body
  return $ LetPat pat (Map cs fun' es mloc) body' loc

transformExpM e@(Index cs v idxcs (idx:idxs) loc) = do
  me <- asks $ lookupDelayed v cs idxcs idx
  case me of
    Nothing -> return e
    Just e' -> do
     e'' <- provideNames e'
     idents <- mapM (\t -> newIdent elname t loc) $ typeOf e''
     let res = LetPat idents e'' (TupLit (map Var idents) loc) loc
     case idxs of
       [] | cheapExp e'' -> return e''
       _  | cheapExp res -> return res
       _                 -> return e
  where elname = baseString (identName v) ++ "_elem"

transformExpM e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp    = transformExpM
                    , mapOnLambda = transformLambdaM
                    }

transformLambdaM :: Lambda -> InlinerM Lambda
transformLambdaM l = do
  e <- transformExpM $ lambdaBody l
  return l { lambdaBody = e }

type ArrayIndexMap = HM.HashMap Ident
                     (Certificates -> Maybe Certificates -> SubExp -> NeedNames Exp)

emptyArrayIndexMap :: ArrayIndexMap
emptyArrayIndexMap = HM.empty

lookupDelayed :: Ident -> Certificates -> Maybe Certificates -> SubExp
              -> ArrayIndexMap
              -> Maybe (NeedNames Exp)
lookupDelayed var cs idxcs idx m = do
  f <- HM.lookup var m
  return $ f cs idxcs idx

performArrayDelay :: Exp -> Maybe (Certificates -> Maybe Certificates -> SubExp -> NeedNames Exp)
performArrayDelay (Map cs fun es _) = do
  vs <- mapM varExp es
  Just $ performArrayDelay' cs fun vs
  where varExp (Var v) = Just v
        varExp _       = Nothing
performArrayDelay _ = Nothing

performArrayDelay' :: Certificates -> Lambda -> [Ident]
                   -> Certificates -> Maybe Certificates -> SubExp -> NeedNames Exp
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
        fe <- f cs ics is
        let loc = srclocOf fe
        return $ LetPat bnds fe (SubExp $ Var bnd) loc
  case performArrayDelay e of
    Nothing -> return emptyArrayIndexMap
    Just f ->
      return $ HM.fromList [ (name, dels f bnd)
                             | (name,bnd) <- zip pat bnds ]

cheapExp :: Exp -> Bool
cheapExp (LetPat _ _ body _) = cheapExp body
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
