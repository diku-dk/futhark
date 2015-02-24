-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive
-- - it's certainly worth considering when we can express such
-- transformations in-place.  This module should be run very late in
-- the compilation pipeline, ideally just before the code generator.
module Futhark.FirstOrderTransform
  ( transformProg
  )
  where

import Control.Applicative
import Control.Monad.State

import Futhark.Representation.Basic
import Futhark.Renamer
import Futhark.MonadFreshNames
import Futhark.Tools

-- | Perform the first-order transformation on an Futhark program.  The
-- resulting program is not uniquely named, so make sure to run the
-- renamer!
transformProg :: Prog -> Prog
transformProg prog =
  renameProg $ Prog $ evalState (mapM transformFunDec $ progFunctions prog) src
  where src = newNameSourceForProg prog

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec (FunDec fname rettype params body) = do
  body' <- runBinder $ transformBody body
  return $ FunDec fname rettype params body'

transformBody :: Body -> Binder Basic Body
transformBody = mapBodyM transform

-- | Transform a single expression.
transformExp :: Exp -> Binder Basic Exp

transformExp (LoopOp (Map cs fun arrs)) = do
  inarrs <- forM (zip
                  (map identType arrs)
                  (map (uniqueness . identType) $ lambdaParams fun)) $ \(t, u) ->
            newIdent "map_inarr" (setUniqueness t u)
  (i, iv) <- newVar "i" $ Basic Int
  resarr <- resultArray $ mapType fun $ map identType arrs
  outarrs <- forM (map identType resarr) $ \t ->
             newIdent "map_outarr" $ t `setUniqueness` Unique
  loopbody <- runBinder $ do
    x <- bodyBind =<< transformLambda fun (index cs inarrs iv)
    dests <- letwith cs outarrs (pexp iv) $ map (PrimOp . SubExp) x
    return $ resultBody $ map Var $ inarrs ++ dests
  return $ LoopOp $
    DoLoop outarrs (loopMerge (inarrs++outarrs) (map Var $ arrs++resarr))
    i (isize inarrs) loopbody

transformExp (LoopOp (Reduce cs fun args)) = do
  (_, (acc, initacc), (i, iv)) <- newFold arrexps accexps
  inarrs <- forM (zip
                  (map identType arrexps)
                  (map (uniqueness . identType) $
                   snd $ splitAt (length args) $ lambdaParams fun)) $ \(t,u) ->
            newIdent "reduce_inarr" (setUniqueness t u)
  loopbody <- runBinder $ do
    acc' <- bodyBind =<< transformLambda fun
            (map (PrimOp . SubExp . Var) acc ++ index cs inarrs iv)
    return $ resultBody (map Var inarrs ++ acc')
  return $ LoopOp $
    DoLoop acc (loopMerge (inarrs++acc) (map Var arrexps++initacc))
    i (isize inarrs) loopbody
  where (accexps, arrexps) = unzip args

transformExp (LoopOp (Scan cs fun args)) = do
  ((arr, initarr), (acc, initacc), (i, iv)) <- newFold arrexps accexps
  loopbody <- insertBindingsM $ do
    x <- bodyBind =<<
         transformLambda fun (map (PrimOp . SubExp . Var) acc ++ index cs arr iv)
    dests <- letwith cs arr (pexp iv) $ map (PrimOp . SubExp) x
    irows <- letSubExps "row" $ index cs dests iv
    rowcopies <- letExps "copy" [ PrimOp $ Copy irow | irow <- irows ]
    return $ resultBody $ map Var $ rowcopies ++ dests
  return $ LoopOp $
    DoLoop arr (loopMerge (acc ++ arr) (initacc ++ initarr)) i (isize arr) loopbody
  where (accexps, arrexps) = unzip args

transformExp (LoopOp (Filter cs fun arrexps)) = do
  arr <- letExps "arr" $ map (PrimOp . SubExp . Var) arrexps
  let nv = isize arrexps
      rowtypes = map (rowType . identType) arrexps
  (xs, _) <- unzip <$> mapM (newVar "x") rowtypes
  (i, iv) <- newVar "i" $ Basic Int
  test <- insertBindingsM $ do
   [check] <- bodyBind =<< transformLambda fun (map (PrimOp . SubExp . Var) xs) -- XXX
   res <- letSubExp "res" $
          If check
             (resultBody [intconst 1])
             (resultBody [intconst 0])
             [Basic Int]
   return $ resultBody [res]
  mape <- letExp "mape" <=< transformExp $
          LoopOp $ Map cs (Lambda xs test [Basic Int]) arr
  plus <- do
    (a,av) <- newVar "a" (Basic Int)
    (b,bv) <- newVar "b" (Basic Int)
    body <- insertBindingsM $ do
      res <- letSubExp "sum" $ PrimOp $ BinOp Plus av bv Int
      return $ resultBody [res]
    return $ Lambda [a, b] body [Basic Int]
  scan <- transformExp $ LoopOp $ Scan cs plus [(intconst 0,mape)]
  ia <- (`setIdentUniqueness` Nonunique) <$> letExp "ia" scan
  let indexia ind = eIndex cs ia [ind]
      sub1 e = eBinOp Minus e (pexp $ intconst 1) Int
      indexi = indexia $ pexp iv
      indexin = index cs arr iv
      indexinm1 = indexia $ sub1 $ pexp iv
  outersize <- letSubExp "filter_result_size" =<< indexia (sub1 $ pexp nv)

  resinit_presplit <- resultArray $ map identType arrexps
  resinit <- forM resinit_presplit $ \v -> do
    let vt = identType v
    leftover <- letSubExp "split_leftover" $ PrimOp $
                BinOp Minus (arraySize 0 vt) outersize Int
    splitres <- letTupExp "filter_split_result" $
      PrimOp $ Split cs outersize v leftover
    case splitres of
      [x,_] -> return x
      _     -> fail "FirstOrderTransform filter: weird split result"

  res <- forM (map identType resinit) $ \t -> newIdent "filter_result" t
  mergesize <- newIdent "mergesize" (Basic Int)
  let resv = resultBody (map Var $ mergesize : res)
  loopbody <- insertBindingsM $ do
    let update = insertBindingsM $ do
          dest <- letwith cs res (sub1 indexi) indexin
          return $ resultBody (map Var $ mergesize:dest)
    eBody [eIf (eIf (pure $ PrimOp $ BinOp Equal iv (intconst 0) Bool)
               (eBody [eBinOp Equal indexi (pexp $ intconst 0) Bool])
               (eBody [eBinOp Equal indexi indexinm1 Bool]))
           (pure resv) update]
  return $ LoopOp $ DoLoop (mergesize:res)
    (loopMerge (mergesize:res) (outersize:map Var resinit))
    i nv loopbody

transformExp (LoopOp (Redomap cs _ innerfun accexps arrexps)) = do
  (_, (acc, initacc), (i, iv)) <- newFold arrexps accexps
  inarrs <- forM (zip
                  (map identType arrexps)
                  (map (uniqueness . identType) $
                   snd $ splitAt (length accexps) $ lambdaParams innerfun)) $ \(t,u) ->
            newIdent "redomap_inarr" (setUniqueness t u)
  loopbody <- runBinder $ do
    acc' <- bodyBind =<< transformLambda innerfun
            (map (PrimOp . SubExp . Var) acc ++ index cs inarrs iv)
    return $ resultBody (map Var inarrs ++ acc')
  return $ LoopOp $
    DoLoop acc (loopMerge (inarrs++acc) (map Var arrexps++initacc))
    i (isize inarrs) loopbody

transformExp e = mapExpM transform e

transformBinding :: Binding -> Binder Basic Binding
transformBinding (Let pat () e@(LoopOp (Filter {})))
  | size : rest <- patternIdents pat = do
  -- FIXME: we need to fix the shape of the type for filter, which is
  -- done in a hacky way here.  The better solution is to change how
  -- filter works.
  size':rest' <- letTupExp "filter_for" =<< transformExp e
  addBinding $ mkLet' [size] $ PrimOp $ SubExp $ Var size'
  let reshapeResult dest orig =
        addBinding $ mkLet' [dest] $
        PrimOp $ Reshape [] (arrayDims $ identType dest) orig
  zipWithM_ reshapeResult rest rest'
  dummy <- newVName "dummy"
  mkLetNames' [dummy] $ PrimOp $ SubExp $ intconst 0
transformBinding (Let pat annot e) =
  Let pat annot <$> transformExp e

transform :: Mapper Basic Basic (Binder Basic)
transform = identityMapper {
              mapOnBinding = transformBinding
            , mapOnBody = insertBindingsM . transformBody
            }

newFold :: [Ident] -> [SubExp]
        -> Binder Basic (([Ident], [SubExp]), ([Ident], [SubExp]), (Ident, SubExp))
newFold arrexps accexps = do
  (i, iv) <- newVar "i" $ Basic Int
  initacc <- letSubExps "acc" $ map maybeCopy accexps
  arrinit <- letSubExps "arr" $ map (maybeCopy . Var) arrexps
  arr <- forM arrinit $ \e -> newIdent "fold_arr" (subExpType e)
  acc <- forM accexps $ \e -> newIdent "acc" (subExpType e)
  return ((arr, arrinit), (acc, initacc), (i, iv))

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: SubExp -> Exp
maybeCopy e
  | unique (subExpType e) || basicType (subExpType e) = PrimOp $ SubExp e
  | otherwise = PrimOp $ Copy e

index :: Certificates -> [Ident] -> SubExp -> [Exp]
index cs arrs i = flip map arrs $ \arr ->
  PrimOp $ Index cs arr [i]

resultArray :: [TypeBase Shape] -> Binder Basic [Ident]
resultArray = mapM arrayOfShape
  where arrayOfShape t = letExp "result" $ PrimOp $ Scratch (elemType t) (arrayDims t)

letwith :: Certificates -> [Ident] -> Binder Basic Exp -> [Exp] -> Binder Basic [Ident]
letwith cs ks i vs = do
  vs' <- letSubExps "values" vs
  i' <- letSubExp "i" =<< i
  let update k v =
        letInPlace "lw_dest" cs k [i'] $ PrimOp $ SubExp v
  zipWithM update ks vs'

isize :: [Ident] -> SubExp
isize = arraysSize 0 . map identType

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . PrimOp . SubExp

transformLambda :: Lambda -> [Exp] -> Binder Basic Body
transformLambda (Lambda params body _) args = do
  zipWithM_ letBindNames' (map (pure . identName) params) args
  transformBody body

loopMerge :: [Ident] -> [SubExp] -> [(FParam, SubExp)]
loopMerge vars vals = [ (FParam var (), val) | (var,val) <- zip vars vals ]
