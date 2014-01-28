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

import L0C.L0
import L0C.MonadFreshNames

-- | Return 'True' if the given expression is a SOAC that can be
-- first-order transformed.
transformable :: Exp -> Bool
transformable (Map {}) = True
transformable (Reduce {}) = True
transformable (Redomap {}) = True
transformable (Scan {}) = True
transformable (Filter {}) = True
transformable (MapT {}) = True
transformable (ReduceT {}) = True
transformable (RedomapT {}) = True
transformable (ScanT {}) = True
transformable (FilterT {}) = True
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
transformExp rec mape@(Map fun e _ loc) =
  newLet (dec rec) "inarr" e $ \inarr inarrv -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    newLet (dec rec) "n" (Size [] 0 inarrv loc) $ \_ nv -> do
      let zero = Literal (IntVal 0) loc
          index0 = Index [] inarr Nothing [zero] loc
          indexi = Index [] inarr Nothing [iv] loc
      funcall0 <- transformLambda (dec rec) fun [index0]
      funcall <- transformLambda (dec rec) fun [indexi]
      newLet (dec rec) "outarr" (Replicate nv funcall0 loc) $ \outarr outarrv -> do
        let branch = If (BinOp Less zero nv (Elem Bool) loc) letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     (typeOf letbody) loc
            letbody = DoLoop (Id outarr) outarrv i nv loopbody outarrv loc
            loopbody = LetWith [] outarr outarr Nothing [iv] funcall outarrv loc
        return branch

transformExp rec (Reduce fun accexp arrexp _ loc) =
  newFold (dec rec) loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index [] arr Nothing [iv] loc
    funcall <- transformLambda (dec rec) fun [accv, indexi]
    let loop = DoLoop (Id acc) accv i (Size [] 0 arrv loc) loopbody accv loc
        loopbody = LetPat (Id acc) funcall accv loc
    return loop

transformExp rec (Scan fun accexp arrexp _ loc) =
  newFold (dec rec) loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index [] arr Nothing [iv] loc
    funcall <- transformLambda (dec rec) fun [accv, indexi]
    let loop = DoLoop (TupId [Id acc, Id arr] loc)
               (TupLit [accv, arrv] loc) i (Size [] 0 arrv loc) loopbody arrv loc
        loopbody = LetWith [] arr arr Nothing [iv] funcall (TupLit [indexi, arrv] loc) loc
    return loop

transformExp rec (Filter fun arrexp rowtype loc) =
  newLet (dec rec) "arr" arrexp $ \arr arrv ->
    newLet (dec rec) "n" (Size [] 0 arrv loc) $ \_ nv -> do
      let checkempty nonempty = If (BinOp Equal nv (intval 0) (Elem Bool) loc)
                                (Literal (emptyArray rowtype) loc) nonempty
                                (typeOf nonempty) loc
      (x, xv) <- newVar loc "x" rowtype
      (i, iv) <- newVar loc "i" (Elem Int)
      fun' <- transformLambda (dec rec) fun [xv]
      let branch = If fun' (intval 1) (intval 0) (Elem Int) loc
          indexin0 = Index [] arr Nothing [intval 0] loc
          indexin = Index [] arr Nothing [iv] loc
      mape <- transformExp rec $ Map (AnonymFun [toParam x] branch (Elem Int) loc) arrv rowtype loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ AnonymFun [toParam a, toParam b]
                 (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
      scan <- transformExp (dec rec) $ Scan plus (intval 0) mape (Elem Int) loc
      newLet (dec rec) "ia" scan $ \ia _ -> do
        let indexia ind = Index [] ia Nothing [ind] loc
            indexiaend = indexia (BinOp Minus nv (intval 1) (Elem Int) loc)
            indexi = indexia iv
            indexim1 = indexia (BinOp Minus iv (intval 1) (Elem Int) loc)
        newLet (dec rec) "res" (Replicate indexiaend indexin0 loc) $ \res resv -> do
          let loop = DoLoop (Id res) resv i nv loopbody resv loc
              loopbody = If (BinOp LogOr
                                   (BinOp Equal iv (intval 0) (Elem Bool) loc)
                                   (BinOp LogAnd
                                          (BinOp Less (intval 0) iv (Elem Bool) loc)
                                          (BinOp Equal indexi indexim1 (Elem Bool) loc)
                                          (Elem Bool) loc)
                             (Elem Bool) loc)
                         resv update (typeOf resv) loc
              update = LetWith [] res res Nothing [BinOp Minus indexi (intval 1) (Elem Int) loc]
                       indexin resv loc
          return $ checkempty loop
        where intval x = Literal (IntVal x) loc

transformExp rec (Redomap _ innerfun accexp arrexp ets loc) =
  transformExp rec $ Reduce innerfun accexp arrexp ets loc

transformExp rec mape@(MapT cs fun arrs loc) = do
  let zero = Literal (IntVal 0) loc
  newLets (dec rec) "inarr" arrs $ \inarrs _ -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    newLet (dec rec) "n" (size cs inarrs) $ \_ nv -> do
      funcall0 <- tuple <$> transformTupleLambda (dec rec) fun (index cs inarrs zero)
      funcall <- tuple <$> transformTupleLambda (dec rec) fun (index cs inarrs iv)
      newResultArray (dec rec) nv funcall0 $ \outarr outarrv -> do
        loopbody <- letwith cs outarr iv funcall outarrv
        let branch = If (BinOp Less zero nv (Elem Bool) loc)
                     letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     (typeOf letbody) loc
            letbody = DoLoop (TupId (map Id outarr) loc) outarrv i nv loopbody
                      outarrv loc
        return branch

transformExp rec (ReduceT cs fun args loc) =
  newFold2 (dec rec) loc arrexps accexp $ \(arr, _) (acc, accv) (i, iv) -> do
    funcall <- transformTupleLambda (dec rec) fun (map Var acc ++ index cs arr iv)
    return $ DoLoop (TupId (map Id acc) loc) accv i (size cs arr)
             funcall accv loc
  where (accexp, arrexps) = unzip args

transformExp rec (ScanT cs fun args loc) =
  newFold2 (dec rec) loc arrexps accexp $ \(arr, arrv) (acc, _) (i, iv) -> do
    funcall <- transformTupleLambda (dec rec) fun $ map Var acc ++ index cs arr iv
    loopbody <- letwith cs arr iv funcall $
                TupLit (index cs arr iv++map Var arr) loc
    let loop = DoLoop (TupId (map Id $ acc ++ arr) loc)
               (TupLit (map Var $ acc ++ arr) loc)
               i (size cs arr) loopbody arrv loc
    return loop
  where (accexp, arrexps) = unzip args

transformExp rec filtere@(FilterT cs fun arrexps loc) =
  newLets (dec rec) "arr" arrexps $ \arr _ ->
    newLet (dec rec) "n" (size cs arr) $ \_ nv -> do
      let checkempty nonempty =
            If (BinOp Equal nv (intval 0) (Elem Bool) loc)
            (Literal (blankValue $ typeOf filtere) loc) nonempty
            (typeOf nonempty) loc
          rowtypes = map (rowType . identType) arr
      (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
      (i, iv) <- newVar loc "i" $ Elem Int
      fun' <- transformTupleLambda (dec rec) fun $ map Var xs
      (check, checkv) <- newVar loc "check" $ Elem Bool
      let test = LetPat (TupId [Id check] loc) fun' branch loc
          branch = If checkv (intval 1) (intval 0) (Elem Int) loc
          indexin0 = index cs arr $ intval 0
          indexin = index cs arr iv
      mape <- transformExp (dec rec) $
              MapT cs (TupleLambda (map toParam xs) test [Elem Int] loc) (map Var arr) loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ TupleLambda [toParam a, toParam b] (BinOp Plus av bv (Elem Int) loc) [Elem Int] loc
      scan <- newTupLet (dec rec) "mape" mape $ \_ mape' ->
                transformExp (dec rec) $ ScanT cs plus [(intval 0,mape')] loc
      newTupLet (dec rec) "ia" scan $ \ia _ -> do
        let indexia ind = Index cs ia Nothing [ind] loc
            indexiaend = indexia (sub1 nv)
            indexi = indexia iv
            indexim1 = indexia (sub1 iv)
        newResultArray (dec rec) indexiaend (TupLit indexin0 loc) $ \res resv -> do
          update <- letwith cs res (sub1 indexi) (TupLit indexin loc) resv
          let loop = DoLoop (TupId (map Id res) loc) resv i nv loopbody resv loc
              loopbody = If (BinOp LogOr
                               (BinOp Equal iv (intval 0) (Elem Bool) loc)
                               (BinOp LogAnd
                                  (BinOp Less (intval 0) iv (Elem Bool) loc)
                                  (BinOp Equal indexi indexim1 (Elem Bool) loc)
                                  (Elem Bool) loc)
                             (Elem Bool) loc)
                         resv update (typeOf resv) loc
          return $ checkempty loop
  where intval x = Literal (IntVal x) loc
        sub1 e = BinOp Minus e (intval 1) (Elem Int) loc

transformExp rec (RedomapT cs _ innerfun accexps arrexps loc) =
  newFold2 (dec rec) loc arrexps accexps $ \(arr, _) (acc, accv) (i, iv) -> do
    funcall <- transformTupleLambda (dec rec) innerfun (map Var acc ++ index cs arr iv)
    return $ DoLoop (TupId (map Id acc) loc) accv i (size cs arr)
             funcall accv loc

transformExp rec e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp (dec rec)
                    }

newFold :: MonadFreshNames VName m =>
                RecDepth -> SrcLoc -> Exp -> Exp ->
                ((Ident, Exp) -> (Ident, Exp) -> (Ident, Exp) -> m Exp) -> m Exp
newFold rec loc arrexp accexp body =
  newLet rec "arr" arrexp $ \arr arrv ->
    newLet rec "acc" accexp $ \acc accv -> do
      (i, iv) <- newVar loc "i" (Elem Int)
      body (arr, arrv) (acc, accv) (i, iv)

newFold2 :: MonadFreshNames VName m =>
                 RecDepth -> SrcLoc -> [Exp] -> [Exp]
              -> (([Ident], Exp) -> ([Ident], Exp) -> (Ident, Exp) -> m Exp) -> m Exp
newFold2 rec loc arrexps accexps body = do
  (i, iv) <- newVar loc "i" (Elem Int)
  newLets rec "arr" arrexps $ \arr arrv -> do
    let ets = map typeOf accexps
    (names, namevs) <- unzip <$> mapM (newVar loc "acc") ets
    let binder inner = LetPat (TupId (map Id names) loc) (TupLit accexps loc) inner loc
    binder <$> body (arr, arrv) (names, TupLit namevs loc) (i, iv)

newLet :: MonadFreshNames VName m =>
          RecDepth -> String -> Exp -> (Ident -> Exp -> m Exp) -> m  Exp
newLet rec name e body = do
  e' <- liftM maybeCopy $ transformExp rec e
  (x,xv) <- newVar loc name (typeOf e')
  let xlet inner = LetPat (Id x) e' inner loc
  xlet <$> body x xv
  where loc = srclocOf e

newTupLet :: MonadFreshNames VName m =>
             RecDepth -> String -> Exp -> (Ident -> Exp -> m Exp) -> m Exp
newTupLet rec name e body = do
  e' <- liftM maybeCopy $ transformExp rec e
  case typeOf e' of
    Elem (Tuple [t]) -> do
      (x,xv) <- newVar loc name t
      let xlet inner = LetPat (TupId [Id x] loc) e' inner loc
      xlet <$> body x xv
        where loc = srclocOf e
    _ -> newLet rec name e body

newLets :: MonadFreshNames VName m =>
           RecDepth -> String -> [Exp] -> ([Ident] -> Exp -> m Exp) -> m Exp
newLets rec k es body = newLets' es []
  where newLets' [] names       =
          body (reverse names) $ TupLit (map Var $ reverse names) noLoc
        newLets' (e:rest) names =
          newLet rec k e $ \name _ -> newLets' rest (name:names)

newVar :: MonadFreshNames VName m => SrcLoc -> String -> Type -> m (Ident, Exp)
newVar loc name tp = do
  x <- newVName name
  return (Ident x tp loc, Var $ Ident x tp loc)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: Exp -> Exp
maybeCopy e
  | unique (typeOf e) || basicType (typeOf e)  = e
  | otherwise = Copy e $ srclocOf e

index :: Certificates -> [Ident] -> Exp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
                  Index cs arr Nothing [i] $ srclocOf i

newResultArray :: MonadFreshNames VName m =>
                  RecDepth -> Exp -> Exp -> ([Ident] -> Exp -> m Exp) -> m Exp
newResultArray rec sizeexp valueexp body =
  case typeOf valueexp of
    Elem (Tuple ets) -> do
      (names, namevs) <- unzip <$> mapM (newVar loc "outarr_v") ets
      let bnd inner = LetPat (TupId (map Id names) loc ) valueexp inner loc
          arrexp = [ maybeCopy $ Replicate sizeexp namev loc
                     | namev <- namevs ]
      bnd <$> newLets rec "outarr" arrexp body
    _ -> do let rep = maybeCopy $ Replicate sizeexp valueexp loc
            (name, namev) <- newVar loc "outarr" $ typeOf rep
            let bnd inner = LetPat (Id name) rep inner loc
            bnd <$> body [name] namev
  where loc = srclocOf valueexp

letwith :: MonadFreshNames VName m => Certificates -> [Ident] -> Exp -> Exp -> Exp -> m Exp
letwith cs ks i v body =
  case typeOf v of
    Elem (Tuple ets) -> do
      names <- mapM (liftM fst . newVar loc "tup") ets
      let comb inner (k, name) = LetWith cs k k Nothing [i] (Var name) inner loc
      return $ LetPat (TupId (map Id names) loc) v
               (foldl comb body $ zip ks names) loc
    _ -> do let comb inner k = LetWith cs k k Nothing [i] v inner loc
            return $ foldl comb body ks
  where loc = srclocOf body

tuple :: Exp -> Exp
tuple e = case typeOf e of Elem (Tuple _) -> e
                           _              -> TupLit [e] $ srclocOf e

size :: Certificates -> [Ident] -> Exp
size _ [] = Literal (IntVal 0) noLoc
size cs (k:_) = Size cs 0 (Var k) $ srclocOf k

transformLambda :: MonadFreshNames VName m => RecDepth -> Lambda -> [Exp] -> m Exp
transformLambda rec (AnonymFun params body _ loc) args = do
  body' <- transformExp rec body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) = LetPat (Id $ Ident pname ptype loc) arg e loc
transformLambda rec (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM (transformExp rec) curryargs
  return $ Apply fname [(arg, Observe) | arg <- curryargs'++args] rettype loc

transformTupleLambda :: MonadFreshNames VName m => RecDepth -> TupleLambda -> [Exp] -> m Exp
transformTupleLambda rec (TupleLambda params body _ loc) args = do
  body' <- transformExp rec body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) =
          LetPat (Id $ Ident pname ptype loc) arg e loc
