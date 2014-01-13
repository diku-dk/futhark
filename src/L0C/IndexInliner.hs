{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-- | A transformation pass that inlines instances of @a[i]@, when @a@
-- is directly the result of a 'mapT'.  Beware: The resulting program
-- is not uniquely named.
module L0C.IndexInliner
  (
  transformProg
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Loc
import qualified Data.HashMap.Strict as HM

import L0C.L0
import L0C.MonadFreshNames
import L0C.NeedNames

-- | Perform the transformation.  Never fails.
transformProg :: Prog -> Prog
transformProg prog =
  let src = newNameSourceForProg prog
  in Prog $ runInlinerM src $ mapM transformFun $ progFunctions prog

transformFun :: FunDec -> InlinerM FunDec
transformFun (fname, rettype, params, body, loc) = do
  body' <- transformExp body
  return (fname, rettype, params, body', loc)

transformExp :: Exp -> InlinerM Exp

transformExp (LetPat pat e@(MapT cs fun es mloc) body loc) = do
  es'  <- mapM transformExp es
  fun' <- transformTupleLambda fun
  dels <- performArrayDelays pat e
  body' <- local (HM.union dels) $ transformExp body
  return $ LetPat pat (MapT cs fun' es' mloc) body' loc

transformExp e@(Index cs v idxcs (idx:idxs) loc) = do
  me <- asks $ lookupDelayed v cs idxcs idx
  case me of
    Nothing -> return e
    Just e' -> do
     e'' <- provideNames e'
     ident <- newIdent elname (typeOf e'') loc
     idxmore <- transformExp $ Index cs ident idxcs idxs loc
     let res = LetPat (Id ident) e'' idxmore loc
     case idxs of
       [] | cheapExp e'' -> return e''
       _  | cheapExp res -> return res
       _                 -> return e
  where elname = baseString (identName v) ++ "_elem"

transformExp e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp         = transformExp
                    , mapOnTupleLambda = transformTupleLambda
                    }

transformTupleLambda :: TupleLambda -> InlinerM TupleLambda
transformTupleLambda l = do
  e <- transformExp $ tupleLambdaBody l
  return l { tupleLambdaBody = e }

type ArrayIndexMap = HM.HashMap Ident
                     (Certificates -> Maybe Certificates -> Exp -> NeedNames Exp)

emptyArrayIndexMap :: ArrayIndexMap
emptyArrayIndexMap = HM.empty

lookupDelayed :: Ident -> Certificates -> Maybe Certificates -> Exp
              -> ArrayIndexMap
              -> Maybe (NeedNames Exp)
lookupDelayed var cs idxcs idx m = do
  f <- HM.lookup var m
  return $ f cs idxcs idx

performArrayDelay :: Exp -> Maybe (Certificates -> Maybe Certificates -> Exp -> NeedNames Exp)
performArrayDelay (MapT cs fun es _) = do
  vs <- mapM varExp es
  Just $ performArrayDelay' cs fun vs
  where varExp (Var v) = Just v
        varExp _       = Nothing
performArrayDelay _ = Nothing

performArrayDelay' :: Certificates -> TupleLambda -> [Ident]
                   -> Certificates -> Maybe Certificates -> Exp -> NeedNames Exp
performArrayDelay' mcs fun vs cs idxcs idx =
  return $ inlineMapFun (tupleLambdaParams fun)
                        [Index (mcs++cs) v idxcs [idx] $ srclocOf v
                           | v <- vs ]
  where inlineMapFun (p:ps) (ie:ies) =
          LetPat (Id $ fromParam p) ie (inlineMapFun ps ies) $ srclocOf p
        inlineMapFun _ _ =
          tupleLambdaBody fun

performArrayDelays :: MonadFreshNames VName m => TupIdent -> Exp -> m ArrayIndexMap
performArrayDelays pat e = do
  bnds <- forM names $ \name ->
            newIdent (baseString $ identName name)
                     (stripArray 1 $ identType name)
                     (srclocOf name)

  let dels f bnd cs ics is = do
        fe <- f cs ics is
        let loc = srclocOf fe
        return $ LetPat (TupId (map Id bnds) loc) fe (Var bnd) loc
  case performArrayDelay e of
    Nothing -> return emptyArrayIndexMap
    Just f ->
      return $ HM.fromList [ (name, dels f bnd) | (name,bnd) <- zip names bnds ]
  where names = patIdents pat

cheapExp :: Exp -> Bool
cheapExp (LetPat _ e body _) = cheapExp e && cheapExp body
cheapExp (BinOp _ x y _ _) = cheapExp x && cheapExp y
cheapExp (Not x _) = cheapExp x
cheapExp (Negate x _ _) = cheapExp x
cheapExp (Var {}) = True
cheapExp (Literal {}) = True
cheapExp (TupLit es _) = all cheapExp es
cheapExp (Index _ _ _ idx _) = all cheapExp idx
cheapExp _ = False

newtype InlinerM a = InlinerM (StateT (NameSource VName) (Reader ArrayIndexMap) a)
  deriving (Functor, Applicative, Monad,
            MonadReader ArrayIndexMap, MonadState (NameSource VName))

instance MonadFreshNames VName InlinerM where
  getNameSource = get
  putNameSource = put

runInlinerM :: NameSource VName -> InlinerM a -> a
runInlinerM src (InlinerM m) = runReader (evalStateT m src) emptyArrayIndexMap
