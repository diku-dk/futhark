-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive
-- - it's certainly worth considering when we can express such
-- transformations in-place.  This module should be run very late in
-- the compilation pipeline, ideally just before the code generator.
module Futhark.FirstOrderTransform
  ( transformable
  , transformProg
  , transformExp
  )
  where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import Futhark.Representation.Basic
import Futhark.Renamer
import Futhark.MonadFreshNames
import Futhark.Tools

-- | Return 'True' if the given expression is a SOAC that can be
-- first-order transformed.
transformable :: Exp -> Bool
transformable (LoopOp (Map {})) = True
transformable (LoopOp (Reduce {})) = True
transformable (LoopOp (Redomap {})) = True
transformable (LoopOp (Scan {})) = True
transformable (LoopOp (Filter {})) = True
transformable _ = False

-- | Perform the first-order transformation on an Futhark program.  The
-- resulting program is not uniquely named, so make sure to run the
-- renamer!
transformProg :: Prog -> Prog
transformProg prog =
  renameProg $ Prog $ evalState (mapM transformFunDec $ progFunctions prog) src
  where src = newNameSourceForProg prog

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec (FunDec fname rettype params body loc) = do
  body' <- runBinder $ transformBody body
  return $ FunDec fname rettype params body' loc

transformBody :: Body -> Binder Basic Body
transformBody = mapBodyM transform

-- | Transform a single expression.
transformExp :: Exp -> Binder Basic Exp

transformExp (LoopOp (Map cs fun arrs loc)) = do
  inarrs <- letExps "inarr" $ map (PrimOp . SubExp) arrs
  (i, iv) <- newVar loc "i" $ Basic Int
  resarr <- resultArray (mapType fun $ map subExpType arrs) loc
  outarr <- forM (map subExpType resarr) $ \t ->
            newIdent "map_outarr" t loc
  loopbody <- runBinder $ do
    x <- bodyBind =<< transformLambda fun (index cs inarrs iv)
    dests <- letwith cs outarr (pexp iv) $ map (PrimOp . SubExp) x
    return $ resultBody [] (map Var dests) loc
  return $ LoopOp $ DoLoop outarr (loopMerge outarr resarr) i (isize inarrs) loopbody loc

transformExp (LoopOp (Reduce cs fun args loc)) = do
  (_, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  arrvs <- mapM (letExp "reduce_arr" . PrimOp . SubExp) arrexps
  loopbody <- insertBindingsM $ transformLambda fun $
              map (PrimOp . SubExp . Var) acc ++ index cs arrvs iv
  return $ LoopOp $ DoLoop acc (loopMerge acc initacc) i (isize arrvs) loopbody loc
  where (accexps, arrexps) = unzip args

transformExp (LoopOp (Scan cs fun args loc)) = do
  ((arr, initarr), (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  loopbody <- insertBindingsM $ do
    x <- bodyBind =<<
         transformLambda fun (map (PrimOp . SubExp . Var) acc ++ index cs arr iv)
    dests <- letwith cs arr (pexp iv) $ map (PrimOp . SubExp) x
    irows <- letSubExps "row" $ index cs dests iv
    rowcopies <- letExps "copy" [ PrimOp $ Copy irow loc | irow <- irows ]
    return $ resultBody [] (map Var $ rowcopies ++ dests) loc
  return $ LoopOp $ DoLoop arr (loopMerge (acc ++ arr) (initacc ++ initarr)) i (isize arr) loopbody loc
  where (accexps, arrexps) = unzip args

transformExp (LoopOp (Filter cs fun arrexps loc)) = do
  arr <- letExps "arr" $ map (PrimOp . SubExp) arrexps
  let nv = size arrexps
      rowtypes = map (rowType . subExpType) arrexps
  (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
  (i, iv) <- newVar loc "i" $ Basic Int
  test <- insertBindingsM $ do
   [check] <- bodyBind =<< transformLambda fun (map (PrimOp . SubExp . Var) xs) -- XXX
   res <- letSubExp "res" $
          If check
             (resultBody [] [intval 1] loc)
             (resultBody [] [intval 0] loc)
             (staticResType [Basic Int]) loc
   return $ resultBody [] [res] loc
  mape <- letExp "mape" <=< transformExp $
          LoopOp $ Map cs (Lambda xs test [Basic Int] loc) (map Var arr) loc
  plus <- do
    (a,av) <- newVar loc "a" (Basic Int)
    (b,bv) <- newVar loc "b" (Basic Int)
    body <- insertBindingsM $ do
      res <- letSubExp "sum" $ PrimOp $ BinOp Plus av bv (Basic Int) loc
      return $ resultBody [] [res] loc
    return $ Lambda [a, b] body [Basic Int] loc
  scan <- transformExp $ LoopOp $ Scan cs plus [(intval 0,Var mape)] loc
  ia <- letExp "ia" scan
  let indexia ind = eIndex cs ia [ind] loc
      sub1 e = eBinOp Minus e (pexp $ intval 1) (Basic Int) loc
      indexi = indexia $ pexp iv
      indexin = index cs arr iv
      indexinm1 = indexia $ sub1 $ pexp iv
  outersize <- letSubExp "filter_result_size" =<< indexia (sub1 $ pexp nv)
  resinit <- resultArray (map ((`setOuterSize` outersize) . subExpType) arrexps) loc
  res <- forM (map subExpType resinit) $ \t -> newIdent "filter_result" t loc
  mergesize <- newIdent "mergesize" (Basic Int) loc
  let resv = resultBody [] (map Var $ mergesize : res) loc
  loopbody <- insertBindingsM $ do
    let update = insertBindingsM $ do
          dest <- letwith cs res (sub1 indexi) indexin
          return $ resultBody [] (map Var $ mergesize:dest) loc
    eBody [eIf (eIf (pure $ PrimOp $ BinOp Equal iv (intval 0) (Basic Bool) loc)
               (eBody [eBinOp Equal indexi (pexp $ intval 0) (Basic Bool) loc])
               (eBody [eBinOp Equal indexi indexinm1 (Basic Bool) loc])
               (staticResType [Basic Bool]) loc)
           (pure resv) update (bodyType resv) loc]
  return $ LoopOp $ DoLoop (mergesize:res)
    (loopMerge (mergesize:res) (outersize:resinit))
    i nv loopbody loc
  where intval x = intconst x loc

transformExp (LoopOp (Redomap cs _ innerfun accexps arrexps loc)) = do
  (_, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  arrvs <- mapM (letExp "redomap_arr" . PrimOp . SubExp) arrexps
  loopbody <- insertBindingsM $ transformLambda innerfun $
              map (PrimOp . SubExp . Var) acc ++ index cs arrvs iv
  return $ LoopOp $ DoLoop acc (loopMerge acc initacc) i (isize arrvs) loopbody loc

transformExp e = mapExpM transform e

transformBinding :: Binding -> Binder Basic Binding
transformBinding (Let pat annot e) = Let pat annot <$> transformExp e

transform :: Mapper Basic Basic (Binder Basic)
transform = identityMapper {
              mapOnBinding = transformBinding
            , mapOnBody = insertBindingsM . transformBody
            }

newFold :: SrcLoc -> [SubExp] -> [SubExp]
        -> Binder Basic (([Ident], [SubExp]), ([Ident], [SubExp]), (Ident, SubExp))
newFold loc arrexps accexps = do
  (i, iv) <- newVar loc "i" $ Basic Int
  initacc <- letSubExps "acc" $ map maybeCopy accexps
  arrinit <- letSubExps "arr" $ map maybeCopy arrexps
  arr <- forM arrinit $ \e -> newIdent "fold_arr" (subExpType e) $ srclocOf e
  acc <- forM accexps $ \e -> newIdent "acc" (subExpType e) $ srclocOf e
  return ((arr, arrinit), (acc, initacc), (i, iv))

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = PrimOp $ SubExp e
  | otherwise = PrimOp $ Copy e $ srclocOf e

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
  PrimOp $ Index cs arr [i] $ srclocOf i

resultArray :: [TypeBase Shape] -> SrcLoc -> Binder Basic [SubExp]
resultArray ts loc = mapM arrayOfShape ts
  where arrayOfShape t = arrayOfShape' $ arrayDims t
          where arrayOfShape' [] =
                  return $ blankConstant t
                arrayOfShape' (d:ds) = do
                  elm <- arrayOfShape' ds
                  letSubExp "result" =<< eCopy (pure $ PrimOp $ Replicate d elm loc)

        blankConstant t = Constant (blankBasicValue $ elemType t) loc

letwith :: Certificates -> [Ident] -> Binder Basic Exp -> [Exp] -> Binder Basic [Ident]
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
size _ = intconst 0 noLoc

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . PrimOp . SubExp

transformLambda :: Lambda -> [Exp] -> Binder Basic Body
transformLambda (Lambda params body _ _) args = do
  zipWithM_ letBind (map pure params) args
  transformBody body

loopMerge :: [Ident] -> [SubExp] -> [(FParam, SubExp)]
loopMerge vars vals = [ (Bindee var (), val) | (var,val) <- zip vars vals ]
