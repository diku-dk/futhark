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
import L0C.InternalRep.Renamer
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
  renameProg $ Prog $ evalState (mapM transformFunDec $ progFunctions prog) src
  where src = newNameSourceForProg prog

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- runBinder $ transformExp noDepthLimit body
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
transformExp :: RecDepth -> Exp -> Binder Exp

transformExp rec e | stopRecursion rec = return e

-- We sometimes have to allocate new arrays up front.  This is a bit
-- tricky, as in case the new array is multidimensional, we need to
-- compute the first element in order to get the proper size.  For
-- example, in the case for map, we evaluate the function on the first
-- element of the array and use that to construct the array for the
-- rest.  If the input array is empty, we simply return an empty
-- output array.
transformExp rec mape@(Map cs fun arrs loc) = do
  let zero = Constant (BasicVal $ IntVal 0) loc
  inarrs <- letExps "inarr" $ map SubExp arrs
  (i, iv) <- newVar loc "i" $ Basic Int
  n <- letExp "n" $ size cs inarrs
  let letbody = do
        funcall0 <- transformLambda (dec rec) fun (index cs inarrs zero)
        y <- letTupExp "fun0" funcall0
        outarr <- newResultArray (SubExp $ Var n) (map (SubExp . Var) y)
        let outarrv = TupLit (map Var outarr) loc
            loopbody = do
              funcall <- transformLambda (dec rec) fun (index cs inarrs iv)
              x <- letTupExp "fun" funcall
              dests <- letwith cs outarr (pexp iv) (map (SubExp . Var) x)
              return $ TupLit (map Var dests) loc
        eDoLoop (zip outarr $ map (pexp . Var) outarr)
                i (pexp $ Var n) loopbody (pure outarrv) loc
  eIf (eBinOp Less (pexp zero) (pexp $ Var n) (Basic Bool) loc)
      letbody
      (blankArray (typeOf mape) loc)
      (typeOf mape) loc

transformExp rec (Reduce cs fun args loc) = do
  (arr, (acc, accv), (i, iv)) <- newFold loc arrexps accexp
  let funcall = transformLambda (dec rec) fun $
                map (SubExp . Var) acc ++ index cs arr iv
  eDoLoop (zip acc $ map pexp accv) i (pure $ size cs arr)
          funcall (pure $ TupLit (map Var acc) loc) loc
  where (accexp, arrexps) = unzip args

transformExp rec (Scan cs fun args loc) = do
  (arr, (acc, _), (i, iv)) <- newFold loc arrexps accexp
  let loopbody = do
        funcall <- transformLambda (dec rec) fun $ map (SubExp . Var) acc ++ index cs arr iv
        x <- letTupExp "fun" funcall
        dests <- letwith cs arr (pexp iv) (map (SubExp . Var) x)
        irows <- letSubExps "row" $ index cs dests iv
        let rowcopies = [ Copy irow loc | irow <- irows ]
        eTupLit (map pure $ rowcopies ++ map (SubExp . Var) dests) loc
  eDoLoop (zip (acc ++ arr) (map (pexp . Var) $ acc ++ arr)) -- XXX Shadowing
          i (pure $ size cs arr) loopbody (pure $ TupLit (map Var arr) loc) loc
  where (accexp, arrexps) = unzip args

transformExp rec filtere@(Filter cs fun arrexps loc) = do
  arr <- letExps "arr" $ map SubExp arrexps
  nv <- letSubExp "size" =<< transformExp (dec rec) (size cs arr)
  let checkempty nonempty =
        eIf (eBinOp Equal (pexp nv) (pexp $ intval 0) (Basic Bool) loc)
            (blankArray (typeOf filtere) loc) (pure nonempty)
            (typeOf nonempty) loc
      rowtypes = map (rowType . identType) arr
  (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
  (i, iv) <- newVar loc "i" $ Basic Int
  (check, checkv) <- newVar loc "check" $ Basic Bool
  fun' <- insertBindings $ transformLambda (dec rec) fun $ map (SubExp . Var) xs
  let test = LetPat [check] fun' branch loc
      branch = If checkv (SubExp $ intval 1) (SubExp $ intval 0) [Basic Int] loc
      indexin0 = index cs arr $ intval 0
  mape <- transformExp (dec rec) $
          Map cs (Lambda (map toParam xs) test [Basic Int] loc) (map Var arr) loc
  plus <- do
    (a,av) <- newVar loc "a" (Basic Int)
    (b,bv) <- newVar loc "b" (Basic Int)
    return $ Lambda [toParam a, toParam b] (BinOp Plus av bv (Basic Int) loc) [Basic Int] loc
  scan <- do mape' <- letExp "mape" mape
             transformExp (dec rec) $ Scan cs plus [(intval 0,Var mape')] loc
  ia <- letExp "ia" scan
  let indexia ind = eIndex cs ia Nothing [ind] loc
      indexim1 = indexia $ sub1 $ pexp nv
      sub1 e = eBinOp Minus e (pexp $ intval 1) (Basic Int) loc
  indexiaend <- indexia $ sub1 $ pexp nv
  res <- newResultArray indexiaend indexin0
  let resv = TupLit (map Var res) loc
      loopbody = do
        let indexi = indexia $ pexp iv
            indexin = index cs arr iv
            update = do
              dest <- letwith cs res (sub1 indexi) indexin
              return $ TupLit (map Var dest) loc
        eIf (eBinOp LogOr
             (eBinOp Equal (pexp iv) (pexp $ intval 0) (Basic Bool) loc)
             (eIf (eBinOp Less (pexp $ intval 0) (pexp iv) (Basic Bool) loc)
              (eBinOp Equal indexi indexim1 (Basic Bool) loc)
              (pexp $ Constant (BasicVal $ LogVal False) loc)
              [Basic Bool] loc)
             (Basic Bool) loc)
            (pure resv) update (typeOf resv) loc
  checkempty =<< eDoLoop (zip res $ map (pexp . Var) res) i (pexp nv) loopbody (pure resv) loc -- XXX, name shadowing
  where intval x = Constant (BasicVal $ IntVal x) loc

transformExp rec (Redomap cs _ innerfun accexps arrexps loc) = do
  (arr, (acc, accv), (i, iv)) <- newFold loc arrexps accexps
  let funcall = transformLambda (dec rec) innerfun
                (map (SubExp . Var) acc ++ index cs arr iv)
  sze <- letSubExp "size" $ size cs arr
  eDoLoop (zip acc $ map pexp accv) i (pexp sze) funcall (pure $ TupLit accv loc) loc

transformExp rec e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = insertBindings . transformExp (dec rec)
                    }

newFold :: SrcLoc -> [SubExp] -> [SubExp]
        -> Binder ([Ident], ([Ident], [SubExp]), (Ident, SubExp))
newFold loc arrexps accexps = do
  (i, iv) <- newVar loc "i" $ Basic Int
  arr <- letExps "arr" $ map maybeCopy arrexps
  acc <- letExps "acc" $ map maybeCopy accexps
  return (arr, (acc, map Var acc), (i, iv))

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = SubExp e
  | otherwise = Copy e $ srclocOf e

blankArray :: [Type] -> SrcLoc -> Binder Exp
blankArray [t] loc = return $ ArrayLit [] (rowType t) loc
blankArray ts  loc = do
  blanks  <- letSubExps "blank" $ map blank ts
  blanks' <- letSubExps "copy_arg" $ map maybeCopy blanks
  pure $ TupLit blanks' loc
  where blank t = ArrayLit [] (rowType t) loc

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
                  Index cs arr Nothing [i] $ srclocOf i

newResultArray :: Exp -> [Exp] -> Binder [Ident]
newResultArray sizeexp valueexps = do
  sizeexp' <- letSubExp "size" sizeexp
  valueexps' <- letSubExps "value" valueexps
  outarrs <- letExps "outarr" [ Replicate sizeexp' vexp loc | vexp <- valueexps' ]
  letExps "outarr" $ map (maybeCopy . Var) outarrs
  where loc = srclocOf sizeexp

letwith :: Certificates -> [Ident] -> Binder Exp -> [Exp] -> Binder [Ident]
letwith cs ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  dests <- mapM (newIdent' (const "letwith_dest")) ks
  let update (dest, k, v) = letWithBind cs dest k Nothing [i'] v
  mapM_ update $ zip3 dests ks vs'
  return dests

size :: Certificates -> [Ident] -> Exp
size _ []     = SubExp $ Constant (BasicVal $ IntVal 0) noLoc
size cs (k:_) = Size cs 0 (Var k) $ srclocOf k

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . SubExp

transformLambda :: RecDepth -> Lambda -> [Exp] -> Binder Exp
transformLambda rec (Lambda params body _ _) args = do
  zipWithM_ letBind (map ((:[]) . fromParam) params) args
  transformExp rec body
