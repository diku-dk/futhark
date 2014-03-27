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
  )
  where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import L0C.InternalRep
import L0C.InternalRep.Renamer
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
  renameProg $ Prog $ evalState (mapM transformFunDec $ progFunctions prog) src
  where src = newNameSourceForProg prog

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- runBinder $ transformBody body
  return (fname, rettype, params, body', loc)

transformBody :: Body -> Binder Body
transformBody = mapBodyM transform

-- | Transform a single expression.
transformExp :: Exp -> Binder Exp

-- We sometimes have to allocate new arrays up front.  This is a bit
-- tricky, as in case the new array is multidimensional, we need to
-- compute the first element in order to get the proper size.  For
-- example, in the case for map, we evaluate the function on the first
-- element of the array and use that to construct the array for the
-- rest.  If the input array is empty, we simply return an empty
-- output array.
transformExp mape@(Map cs fun arrs loc) = do
  inarrs <- letExps "inarr" $ map subExp arrs
  (i, iv) <- newVar loc "i" $ Basic Int
  resarr <- resultArray (typeOf mape) loc
  outarr <- forM (map subExpType resarr) $ \t ->
            newIdent "map_outarr" t loc
  loopbody <- runBinder $ do
    x <- bodyBind =<< transformLambda fun (index cs inarrs iv)
    dests <- letwith cs outarr (pexp iv) $ map subExp x
    return $ resultBody [] (map Var dests) loc
  loopBind (zip outarr resarr) i (isize inarrs) loopbody
  return $ SubExps (map Var outarr) loc

transformExp (Reduce cs fun args loc) = do
  (_, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  arrvs <- mapM (letExp "reduce_arr" . subExp) arrexps
  loopbody <- insertBindingsM $ transformLambda fun $
              map (subExp . Var) acc ++ index cs arrvs iv
  loopBind (zip acc initacc) i (isize arrvs) loopbody
  return $ SubExps (map Var acc) loc
  where (accexps, arrexps) = unzip args

transformExp (Scan cs fun args loc) = do
  ((arr, initarr), (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  loopbody <- insertBindingsM $ do
    x <- bodyBind =<<
         transformLambda fun (map (subExp . Var) acc ++ index cs arr iv)
    dests <- letwith cs arr (pexp iv) $ map subExp x
    irows <- letSubExps "row" $ index cs dests iv
    rowcopies <- letExps "copy" [ Copy irow loc | irow <- irows ]
    return $ resultBody [] (map Var $ rowcopies ++ dests) loc
  loopBind (zip (acc ++ arr) (initacc ++ initarr)) i (isize arr) loopbody
  return $ SubExps (map Var arr) loc
  where (accexps, arrexps) = unzip args

transformExp (Filter cs fun arrexps outersize loc) = do
  arr <- letExps "arr" $ map subExp arrexps
  let nv = size arrexps
      rowtypes = map (rowType . subExpType) arrexps
  (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
  (i, iv) <- newVar loc "i" $ Basic Int
  test <- insertBindingsM $ do
   [check] <- bodyBind =<< transformLambda fun (map (subExp . Var) xs) -- XXX
   res <- letSubExp "res" $
          If check
             (resultBody [] [intval 1] loc)
             (resultBody [] [intval 0] loc)
             [Basic Int] loc
   return $ resultBody [] [res] loc
  mape <- letExp "mape" <=< transformExp $
          Map cs (Lambda (map toParam xs) test [Basic Int] loc) (map Var arr) loc
  plus <- do
    (a,av) <- newVar loc "a" (Basic Int)
    (b,bv) <- newVar loc "b" (Basic Int)
    body <- insertBindingsM $ do
      res <- letSubExp "sum" $ BinOp Plus av bv (Basic Int) loc
      return $ resultBody [] [res] loc
    return $ Lambda [toParam a, toParam b] body [Basic Int] loc
  scan <- transformExp $ Scan cs plus [(intval 0,Var mape)] loc
  ia <- letExp "ia" scan
  let indexia ind = eIndex cs ia [ind] loc
      sub1 e = eBinOp Minus e (pexp $ intval 1) (Basic Int) loc
  resinit <- resultArray (map ((`setOuterSize` outersize) . subExpType) arrexps) loc
  res <- forM (map subExpType resinit) $ \t -> newIdent "filter_result" t loc
  let resv = resultBody [] (map Var res) loc
  loopbody <- insertBindingsM $ do
    let indexi = indexia $ pexp iv
        indexin = index cs arr iv
        indexinm1 = indexia $ sub1 $ pexp iv
        update = do
          dest <- letwith cs res (sub1 indexi) indexin
          return $ SubExps (map Var dest) loc
    eBody $
      eIf (eIf (pure $ BinOp Equal iv (intval 0) (Basic Bool) loc)
               (eBody $ eBinOp Equal indexi (pexp $ intval 0) (Basic Bool) loc)
               (eBody $ eBinOp Equal indexi indexinm1 (Basic Bool) loc)
               [Basic Bool] loc)
          (pure resv) (eBody update) (bodyType resv) loc
  loopBind (zip res resinit) i nv loopbody
  return $ SubExps (map Var res) loc
  where intval x = Constant (BasicVal $ IntVal x) loc

transformExp (Redomap cs _ innerfun accexps arrexps loc) = do
  (_, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  arrvs <- mapM (letExp "redomap_arr" . subExp) arrexps
  loopbody <- insertBindingsM $ transformLambda innerfun $
              map (subExp . Var) acc ++ index cs arrvs iv
  loopBind (zip acc initacc) i (isize arrvs) loopbody
  return $ SubExps (map Var acc) loc

transformExp e = mapExpM transform e

transform :: Mapper Binder
transform = identityMapper {
              mapOnExp = transformExp
            , mapOnBody = insertBindingsM . transformBody
            }

newFold :: SrcLoc -> [SubExp] -> [SubExp]
        -> Binder (([Ident], [SubExp]), ([Ident], [SubExp]), (Ident, SubExp))
newFold loc arrexps accexps = do
  (i, iv) <- newVar loc "i" $ Basic Int
  initacc <- letSubExps "acc" $ map maybeCopy accexps
  arrinit <- letSubExps "arr" $ map maybeCopy arrexps
  arr <- forM (map subExpType arrinit) $ \t -> newIdent "fold_arr" t noLoc
  acc <- forM initacc $ \e -> newIdent "acc" (subExpType e) $ srclocOf e
  return ((arr, arrinit), (acc, initacc), (i, iv))

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = subExp e
  | otherwise = Copy e $ srclocOf e

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
                  Index cs arr [i] $ srclocOf i

resultArray :: [Type] -> SrcLoc -> Binder [SubExp]
resultArray ts loc = mapM arrayOfShape ts
  where arrayOfShape t = arrayOfShape' $ arrayDims t
          where arrayOfShape' [] =
                  return $ blankConstant t
                arrayOfShape' (d:ds) = do
                  elm <- arrayOfShape' ds
                  letSubExp "result" $ Replicate d elm loc

        blankConstant t = Constant (blankValue $ basicDecl $ elemType t) loc

letwith :: Certificates -> [Ident] -> Binder Exp -> [Exp] -> Binder [Ident]
letwith cs ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  dests <- mapM (newIdent' (const "letwith_dest")) ks
  let update (dest, k, v) = letWithBind cs dest k [i'] v
  mapM_ update $ zip3 dests ks vs'
  return dests

isize :: [Ident] -> SubExp
isize = size . map Var

size :: [SubExp] -> SubExp
size (v:_)
  | se : _ <- shapeDims $ arrayShape $ subExpType v = se
size _ = Constant (BasicVal $ IntVal 0) noLoc

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . subExp

transformLambda :: Lambda -> [Exp] -> Binder Body
transformLambda (Lambda params body _ _) args = do
  zipWithM_ letBind (map ((:[]) . fromParam) params) args
  transformBody body
