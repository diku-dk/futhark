{-# LANGUAGE FlexibleContexts #-}
-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive
-- - it's certainly worth considering when we can express such
-- transformations in-place.  This module should be run very late in
-- the compilation pipeline, ideally just before the code generator.
module L0C.FirstOrderTransform
  ( transformable
  , transformProg
  , transformExp
  , RecDepth
  , noDepthLimit
  , noRecursion
  )
  where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import L0C.InternalRep
import L0C.MonadFreshNames
import L0C.Tools

-- | Return 'True' if the given expression is a SOAC that can be
-- first-order transformed.
transformable :: Exp -> Bool
transformable (Map {}) = True
transformable (Reduce {}) = True
transformable (Redomap {}) = True
transformable (Scan {}) = True
transformable (Filter {}) = True
transformable _ = False

-- | Perform the first-order transformation on an L0 program.  The
-- resulting program is not uniquely named, so make sure to run the
-- renamer!
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runTransformM $ mapM transformFunDec $ progFunctions prog
  where runTransformM m = evalState m $ newNameSourceForProg prog

transformFunDec :: MonadFreshNames VName m => FunDec -> m FunDec
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- transformExp noDepthLimit body
  return (fname, rettype, params, body', loc)

-- | How deeply to recurse the transformation.
newtype RecDepth = RecDepth (Maybe Int)

-- | Recurse forever - this means transforming the entire input.
noDepthLimit :: RecDepth
noDepthLimit = RecDepth Nothing

-- | Only transform the first level/immediate expression.
noRecursion :: RecDepth
noRecursion = RecDepth $ Just 1

stopRecursion :: RecDepth -> Bool
stopRecursion (RecDepth (Just x)) = x <= 0
stopRecursion (RecDepth Nothing)  = False

dec :: RecDepth -> RecDepth
dec (RecDepth x) = RecDepth $ liftM (\x' -> x'-1) x

-- | Transform a single expression, using the @RecDepth@ argument to
-- control how deeply to go.
transformExp :: MonadFreshNames VName m => RecDepth -> Exp -> m Exp

transformExp rec e | stopRecursion rec = return e

-- We sometimes have to allocate new arrays up front.  This is a bit
-- tricky, as in case the new array is multidimensional, we need to
-- compute the first element in order to get the proper size.  For
-- example, in the case for map, we evaluate the function on the first
-- element of the array and use that to construct the array for the
-- rest.  If the input array is empty, we simply return an empty
-- output array.
transformExp rec mape@(Map cs fun arrs loc) = do
  let zero = Constant (IntVal 0) loc
  newLets (dec rec) "inarr" arrs $ \inarrs -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    newLet (dec rec) "n" (size cs inarrs) $ \_ nv -> do
      funcall0 <- transformLambda (dec rec) fun (index cs inarrs zero)
      funcall <- transformLambda (dec rec) fun (index cs inarrs iv)
      newResultArray (dec rec) nv funcall0 $ \outarr outarrv -> do
        loopbody <- letwith cs outarr iv funcall outarrv
        let branch = If (BinOp Less zero nv (Elem Bool) loc)
                     letbody
                     (maybeCopy $ Constant (blankValue $ typeOf mape) loc)
                     (typeOf letbody) loc
            letbody = DoLoop outarr outarrv i nv loopbody
                      outarrv loc
        return branch

transformExp rec (Reduce cs fun args loc) =
  newFold (dec rec) loc arrexps accexp $ \arr (acc, accv) (i, iv) -> do
    funcall <- transformLambda (dec rec) fun (map Var acc ++ index cs arr iv)
    return $ DoLoop acc accv i (size cs arr)
             funcall accv loc
  where (accexp, arrexps) = unzip args

transformExp rec (Scan cs fun args loc) =
  newFold (dec rec) loc arrexps accexp $ \arr (acc, _) (i, iv) -> do
    funcall <- transformLambda (dec rec) fun $ map Var acc ++ index cs arr iv
    loopbody <- letwith cs arr iv funcall $
                TupLit (index cs arr iv++map Var arr) loc
    let loop = DoLoop (acc ++ arr)
               (TupLit (map Var $ acc ++ arr) loc)
               i (size cs arr) loopbody (TupLit (map Var arr) loc) loc
    return loop
  where (accexp, arrexps) = unzip args

transformExp rec filtere@(Filter cs fun arrexps loc) =
  newLets (dec rec) "arr" arrexps $ \arr ->
    asSubExp (transformExp (dec rec) (size cs arr)) $ \nv -> do
      let checkempty nonempty =
            If (BinOp Equal nv (intval 0) (Elem Bool) loc)
            (Constant (blankValue $ typeOf filtere) loc) nonempty
            (typeOf nonempty) loc
          rowtypes = map (rowType . identType) arr
      (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
      (i, iv) <- newVar loc "i" $ Elem Int
      fun' <- transformLambda (dec rec) fun $ map Var xs
      (check, checkv) <- newVar loc "check" $ Elem Bool
      let test = LetPat [check] fun' branch loc
          branch = If checkv (intval 1) (intval 0) (Elem Int) loc
          indexin0 = index cs arr $ intval 0
          indexin = index cs arr iv
      mape <- transformExp (dec rec) $
              Map cs (Lambda (map toParam xs) test [Elem Int] loc) (map Var arr) loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ Lambda [toParam a, toParam b] (BinOp Plus av bv (Elem Int) loc) [Elem Int] loc
      scan <- newLet (dec rec) "mape" mape $ \_ mape' ->
                transformExp (dec rec) $ Scan cs plus [(intval 0,mape')] loc
      newLet (dec rec) "ia" scan $ \ia _ -> do
        nv' <- sub1 nv
        let indexia ind = Index cs ia Nothing [ind] loc
            indexi = indexia iv
            indexiaend = indexia nv'
            indexim1 = indexia nv'
        newResultArray (dec rec) indexiaend (TupLit indexin0 loc) $ \res -> do
          let resv = TupLit (map Var res) loc
          update <- letwith cs res (sub1 indexi) indexin resv
          loopbody <-
            eIf (eBinOp LogOr
                 (eBinOp Equal (pexp iv) (pexp $ intval 0) (Elem Bool) loc)
                 (eIf (eBinOp Less (pexp $ intval 0) (pexp iv) (Elem Bool) loc)
                      (eBinOp Equal (pure indexi) (pure indexim1) (Elem Bool) loc)
                      (pexp $ Constant (BasicValue $ LogVal False) loc)
                      [Elem Bool] loc)
                 (Elem Bool) loc)
                (pure resv) (pure update) (typeOf resv) loc
          return $ checkempty $ DoLoop (zip res $ map Var res) i nv loopbody resv loc -- XXX, name shadowing
  where intval x = Constant (BasicValue $ IntVal x) loc
        pexp = pure . SubExp
        sub1 e = eBinOp Minus e (pexp $ intval 1) (Elem Int) loc

transformExp rec (Redomap cs _ innerfun accexps arrexps loc) =
  newFold (dec rec) loc (map SubExp arrexps) accexps $ \arr (acc, accv) (i, iv) -> do
    funcall <- transformLambda (dec rec) innerfun
               (map (SubExp . Var) acc ++ index cs arr iv)
    asSubExp (pure $ size cs arr) $ \sze ->
      return $ DoLoop (zip acc accv) i sze funcall (TupLit accv loc) loc

transformExp rec e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp (dec rec)
                    }

newFold :: MonadFreshNames VName m =>
           RecDepth -> SrcLoc -> [Exp] -> [SubExp]
        -> ([Ident] -> ([Ident], [SubExp]) -> (Ident, SubExp) -> m Exp) -> m Exp
newFold rec loc arrexps accexps body = do
  (i, iv) <- newVar loc "i" (Elem Int)
  newLets rec "arr" arrexps $ \arr -> do
    let ets = map subExpType accexps
    (names, namevs) <- unzip <$> mapM (newVar loc "acc") ets
    let binder inner = LetPat names (TupLit accexps loc) inner loc
    binder <$> body arr (names, namevs) (i, iv)

newLet :: MonadFreshNames VName m =>
          RecDepth -> String -> Exp -> ([Ident] -> Exp -> m Exp) -> m Exp
newLet rec name e body = do
  (names, namevs) <- unzip <$> mapM (newVar loc name) ts
  e' <- transformExp rec e
  let bndOuter inner = LetPat names e' inner loc
  bndOuter <$> body names (TupLit namevs loc)
  where loc = srclocOf e
        ts  = typeOf e

newLets :: MonadFreshNames VName m =>
           RecDepth -> String -> [Exp] -> ([Ident] -> m Exp) -> m Exp
newLets rec k es body = newLets' es []
  where newLets' [] names       =
          body (reverse names)
        newLets' (e:rest) names =
          newLet rec k e $ \name _ -> newLets' rest (name++names)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = SubExp e
  | otherwise = Copy e $ srclocOf e

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
                  Index cs arr Nothing [i] $ srclocOf i

newResultArray :: MonadFreshNames VName m =>
                  RecDepth -> SubExp -> Exp -> ([Ident] -> m Exp) -> m Exp
newResultArray rec sizeexp valueexp body = do
  (names, namevs) <- unzip <$> mapM (newVar loc "outarr_v") vts
  let bnd inner = LetPat names valueexp inner loc
      arrexp = [ Replicate sizeexp namev loc | namev <- namevs ]
  bnd <$> newLets rec "outarr" arrexp body
  where loc = srclocOf valueexp
        vts = typeOf valueexp

letwith :: MonadFreshNames VName m =>
           Certificates -> [Ident] -> m Exp -> [Exp] -> Exp -> m Exp
letwith cs ks i vs body =
  asSubExps (pure vs) $ \vs' ->
  asSubExp i $ \i' -> do
    names <- mapM (liftM fst . newVar loc "tup" . subExpType) vs'
    let comb inner (k, name) = LetWith cs k k Nothing [i'] (Var name) inner loc
    return $ LetPat names (TupLit vs' loc) (foldl comb body $ zip ks names) loc
    where loc = srclocOf body

size :: Certificates -> [Ident] -> Exp
size _ []     = SubExp $ Constant (BasicValue $ IntVal 0) noLoc
size cs (k:_) = Size cs 0 (Var k) $ srclocOf k

transformLambda :: MonadFreshNames VName m =>
                   RecDepth -> Lambda -> [Exp] -> m Exp
transformLambda rec (Lambda params body _ loc) args = do
  body' <- transformExp rec body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) =
          LetPat [Ident pname ptype loc] arg e loc
