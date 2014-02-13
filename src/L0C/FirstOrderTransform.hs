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
  let zero = Constant (BasicValue $ IntVal 0) loc
  letExps "inarr" (pure $ map SubExp arrs) $ \inarrs -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    letExp "n" (pure $ size cs inarrs) $ \n -> do
      let funcall0 = transformLambda (dec rec) fun (index cs inarrs zero)
      let funcall = transformLambda (dec rec) fun (index cs inarrs iv)
      letTupExp "fun0" funcall0 $ \y _ ->
        letTupExp "fun" funcall $ \x _ ->
        newResultArray (SubExp $ Var n) (map (SubExp . Var) y) $ \outarr -> do
          let outarrv = TupLit (map Var outarr) loc
          loopbody <- letwith cs outarr (pexp iv) (map (SubExp . Var) x) outarrv
          letbody <- eDoLoop (zip outarr $ map (pexp . Var) outarr)
                     i (pexp $ Var n) loopbody outarrv loc
          eIf (eBinOp Less (pexp zero) (pexp $ Var n) (Elem Bool) loc)
              (pure letbody)
              (blankArray (typeOf mape) loc)
              (typeOf mape) loc

transformExp rec (Reduce cs fun args loc) =
  newFold loc arrexps accexp $ \arr (acc, accv) (i, iv) -> do
    funcall <- transformLambda (dec rec) fun $
               map (SubExp . Var) acc ++ index cs arr iv
    eDoLoop (zip acc $ map pexp accv) i (pure $ size cs arr)
            funcall (TupLit (map Var acc) loc) loc
  where (accexp, arrexps) = unzip args

transformExp rec (Scan cs fun args loc) =
  newFold loc arrexps accexp $ \arr (acc, _) (i, iv) -> do
    let funcall = transformLambda (dec rec) fun $ map (SubExp . Var) acc ++ index cs arr iv
    letTupExp "fun" funcall $ \x _ -> do
      loopbody <- letwith cs arr (pexp iv) (map (SubExp . Var) x) =<<
                  eTupLit (map pure $ index cs arr iv++map (SubExp . Var) arr) loc
      eDoLoop (zip (acc ++ arr) (map (pexp . Var) $ acc ++ arr)) -- XXX Shadowing
              i (pure $ size cs arr) loopbody (TupLit (map Var arr) loc) loc
  where (accexp, arrexps) = unzip args

transformExp rec filtere@(Filter cs fun arrexps loc) =
  letExps "arr" (pure $ map SubExp arrexps) $ \arr ->
    letSubExp "size" (transformExp (dec rec) (size cs arr)) $ \nv -> do
      let checkempty nonempty =
            eIf (eBinOp Equal (pexp nv) (pexp $ intval 0) (Elem Bool) loc)
                (blankArray (typeOf filtere) loc) (pure nonempty)
                (typeOf nonempty) loc
          rowtypes = map (rowType . identType) arr
      (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
      (i, iv) <- newVar loc "i" $ Elem Int
      fun' <- transformLambda (dec rec) fun $ map (SubExp . Var) xs
      (check, checkv) <- newVar loc "check" $ Elem Bool
      let test = LetPat [check] fun' branch loc
          branch = If checkv (SubExp $ intval 1) (SubExp $ intval 0) [Elem Int] loc
          indexin0 = index cs arr $ intval 0
          indexin = index cs arr iv
      mape <- transformExp (dec rec) $
              Map cs (Lambda (map toParam xs) test [Elem Int] loc) (map Var arr) loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ Lambda [toParam a, toParam b] (BinOp Plus av bv (Elem Int) loc) [Elem Int] loc
      scan <- letExp "mape" (pure mape) $ \mape' ->
                transformExp (dec rec) $ Scan cs plus [(intval 0,Var mape')] loc
      letExp "ia" (pure scan) $ \ia -> do
        let indexia ind = eIndex cs ia Nothing [ind] loc
            sub1 e = eBinOp Minus e (pexp $ intval 1) (Elem Int) loc
            indexi = indexia $ pexp iv
            indexim1 = indexia $ sub1 $ pexp nv
        indexiaend <- indexia $ sub1 $ pexp nv
        newResultArray indexiaend indexin0 $ \res -> do
          let resv = TupLit (map Var res) loc
          update <- letwith cs res (sub1 indexi) indexin resv
          loopbody <-
            eIf (eBinOp LogOr
                 (eBinOp Equal (pexp iv) (pexp $ intval 0) (Elem Bool) loc)
                 (eIf (eBinOp Less (pexp $ intval 0) (pexp iv) (Elem Bool) loc)
                      (eBinOp Equal indexi indexim1 (Elem Bool) loc)
                      (pexp $ Constant (BasicValue $ LogVal False) loc)
                      [Elem Bool] loc)
                 (Elem Bool) loc)
                (pure resv) (pure update) (typeOf resv) loc
          checkempty $ DoLoop (zip res $ map Var res) i nv loopbody resv loc -- XXX, name shadowing
  where intval x = Constant (BasicValue $ IntVal x) loc

transformExp rec (Redomap cs _ innerfun accexps arrexps loc) =
  newFold loc arrexps accexps $ \arr (acc, accv) (i, iv) -> do
    funcall <- transformLambda (dec rec) innerfun
               (map (SubExp . Var) acc ++ index cs arr iv)
    letSubExp "size" (pure $ size cs arr) $ \sze ->
      return $ DoLoop (zip acc accv) i sze funcall (TupLit accv loc) loc

transformExp rec e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp (dec rec)
                    }

newFold :: MonadFreshNames VName m =>
           SrcLoc -> [SubExp] -> [SubExp]
        -> ([Ident] -> ([Ident], [SubExp]) -> (Ident, SubExp) -> m Exp) -> m Exp
newFold loc arrexps accexps body = do
  (i, iv) <- newVar loc "i" (Elem Int)
  letExps "arr" (pure $ map SubExp arrexps) $ \arr -> do
    let ets = map subExpType accexps
    (names, namevs) <- unzip <$> mapM (newVar loc "acc") ets
    let binder inner = LetPat names (TupLit accexps loc) inner loc
    binder <$> body arr (names, namevs) (i, iv)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = SubExp e
  | otherwise = Copy e $ srclocOf e

blankArray :: MonadFreshNames VName m => [Type] -> SrcLoc -> m Exp
blankArray [t] loc = return $ ArrayLit [] (rowType t) loc
blankArray ts  loc =
  letSubExps "blank" (pure $ map blank ts) $ \blanks ->
    letSubExps "copy_arg" (pure $ map maybeCopy blanks) $ \blanks' ->
      pure $ TupLit blanks' loc
  where blank t = ArrayLit [] (rowType t) loc

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
                  Index cs arr Nothing [i] $ srclocOf i

newResultArray :: MonadFreshNames VName m =>
                  Exp -> [Exp] -> ([Ident] -> m Exp) -> m Exp
newResultArray sizeexp valueexps body =
  letSubExp "size" (pure sizeexp) $ \sizeexp' ->
  letSubExps "value" (pure valueexps) $ \valueexps' -> do
    (names, namevs) <- unzip <$> mapM (newVar loc "outarr_v" . subExpType) valueexps'
    let bnd inner = LetPat names (TupLit valueexps' loc) inner loc
        arrexp = [ Replicate sizeexp' namev loc | namev <- namevs ]
    bnd <$> letExps "outarr" (pure arrexp) body
    where loc = srclocOf sizeexp

letwith :: MonadFreshNames VName m =>
           Certificates -> [Ident] -> m Exp -> [Exp] -> Exp -> m Exp
letwith cs ks i vs body =
  letSubExps "values" (pure vs) $ \vs' ->
  letSubExp "i" i $ \i' -> do
    names <- mapM (liftM fst . newVar loc "tup" . subExpType) vs'
    let comb inner (k, name) = LetWith cs k k Nothing [i'] (Var name) inner loc
    return $ LetPat names (TupLit vs' loc) (foldl comb body $ zip ks names) loc
    where loc = srclocOf body

size :: Certificates -> [Ident] -> Exp
size _ []     = SubExp $ Constant (BasicValue $ IntVal 0) noLoc
size cs (k:_) = Size cs 0 (Var k) $ srclocOf k

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . SubExp

transformLambda :: MonadFreshNames VName m =>
                   RecDepth -> Lambda -> [Exp] -> m Exp
transformLambda rec (Lambda params body _ loc) args = do
  body' <- transformExp rec body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) =
          LetPat [Ident pname ptype loc] arg e loc
