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
import qualified Data.HashMap.Lazy as HM

import Futhark.Representation.Basic
import Futhark.Renamer
import Futhark.MonadFreshNames
import Futhark.Tools

-- | Perform the first-order transformation on an Futhark program.  The
-- resulting program is not uniquely named, so make sure to run the
-- renamer!
transformProg :: Prog -> Prog
transformProg prog =
  {-renameProg $-} Prog $ evalState (mapM transformFunDec $ progFunctions prog) src
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
  (i, iv) <- newVar "i" $ Basic Int
  resarr <- resultArray $ mapType fun $ map identType arrs
  outarrs <- forM (map identType resarr) $ \t ->
             newIdent "map_outarr" $ t `setUniqueness` Unique
  loopbody <- runBinder $ do
    x <- bodyBind =<< transformLambda fun (index cs arrs_nonunique iv)
    dests <- letwith cs outarrs (pexp iv) $ map (PrimOp . SubExp) x
    return $ resultBody $ map Var dests
  return $ LoopOp $
    DoLoop outarrs (loopMerge outarrs (map Var resarr))
    (ForLoop i (isize arrs)) loopbody
  where arrs_nonunique = [ v { identType = identType v `setUniqueness` Nonunique }
                         | v <- arrs ]

transformExp (LoopOp op@(ConcatMap cs fun inputs)) = do
  arrs <- forM inputs $ \input -> do
    fun' <- renameLambda fun
    let funparams = lambdaParams fun'
        (ctxparams, valparams) =
          splitAt (length funparams-length input) funparams
        shapemap = shapeMapping (map identType valparams) $
                   map identType input
        fun'' = fun' { lambdaParams = valparams }
    forM_ (HM.toList shapemap) $ \(size,se) ->
      when (size `elem` map identName ctxparams) $
        letBindNames'_ [size] $ PrimOp $ SubExp se
    input' <- forM (zip valparams input) $ \(p,v) ->
      letExp "concatMap_reshaped_input" $
      PrimOp $ Reshape [] (arrayDims $ identType p) v
    vs <- bodyBind =<< transformLambda fun'' (map (PrimOp . SubExp . Var) input')
    mapM (letExp "concatMap_fun_res" . PrimOp . SubExp) vs
  emptyarrs <- mapM (letExp "empty")
               [ PrimOp $ ArrayLit [] t | t <- lambdaReturnType fun ]
  let hackbody = Body () [] $ Result $ map Var emptyarrs
      concatArrays arrs1 arrs2 = do
        arrs1' <- mapM (letExp "concatMap_concatted_intermediate_result") arrs1
        let ns = map (arraySize 0 . identType) arrs1'
            ms = map (arraySize 0 . identType) arrs2
        ks <- mapM (letSubExp "concatMap_concat_size")
              [ PrimOp $ BinOp Plus n m Int | (n,m) <- zip ns ms ]
        return [ PrimOp $ Concat cs arr1 [arr2] k
               | (arr1,arr2,k) <- zip3 arrs1' arrs2 ks ]
  realbody <- runBinder $ do
    res <- mapM (letSubExp "concatMap_concatted_result") =<<
           foldM concatArrays (map (PrimOp . SubExp . Var) emptyarrs) arrs
    resultBody <$> mapM (letSubExp "concatMap_copy_result" . PrimOp . Copy) res
  return $ If (Constant $ LogVal False) hackbody realbody (loopOpExtType op)

transformExp (LoopOp (Reduce cs fun args)) = do
  ((acc, initacc), (i, iv)) <- newFold accexps
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
    (ForLoop i (isize inarrs)) loopbody
  where (accexps, arrexps) = unzip args

transformExp (LoopOp (Scan cs fun args)) = do
  ((acc, initacc), (i, iv)) <- newFold accexps
  initarr <- resultArray $ map identType arrexps
  arr <- forM initarr $ \v -> newIdent "fold_arr" $ identType v `setUniqueness` Unique
  loopbody <- insertBindingsM $ do
    x <- bodyBind =<<
         transformLambda fun (map (PrimOp . SubExp . Var) acc ++
                              index cs arrexps_nonunique iv)
    dests <- letwith cs arr (pexp iv) $ map (PrimOp . SubExp) x
    irows <- letSubExps "row" $ index cs dests iv
    rowcopies <- letExps "copy" $ map (PrimOp . Copy) irows
    return $ resultBody $ map Var $ rowcopies ++ dests
  return $ LoopOp $
    DoLoop arr (loopMerge (acc ++ arr) (initacc ++ map Var initarr))
    (ForLoop i (isize arr)) loopbody
  where (accexps, arrexps) = unzip args
        arrexps_nonunique = [ v { identType = identType v `setUniqueness` Nonunique }
                            | v <- arrexps ]

transformExp (LoopOp (Redomap cs _ innerfun accexps arrexps)) = do
  let outersize = arraysSize 0 (map identType arrexps)
  -- for the MAP    part
  let acc_num     = length accexps
  let res_tps     = lambdaReturnType innerfun
  let map_arr_tps = drop (length accexps) res_tps
  maparrs <- resultArray $
               [ arrayOf t (Shape [outersize]) (uniqueness t)
                 | t <- map_arr_tps ]
  outarrs <- forM (map identType maparrs) $ \t ->
             newIdent "redomap_outarr" $ t `setUniqueness` Unique
  -- for the REDUCE part
  ((acc, initacc), (i, iv)) <- newFold accexps
  inarrs <- forM (zip
                  (map identType arrexps)
                  (map (uniqueness . identType) $
                   snd $ splitAt (length accexps) $ lambdaParams innerfun)) $ \(t,u) ->
            newIdent "redomap_inarr" (setUniqueness t u)
  loopbody <- runBinder $ do
    accxis<- bodyBind =<< transformLambda innerfun
             (map (PrimOp . SubExp . Var) acc ++ index cs inarrs iv)
    let (acc', xis) = splitAt acc_num accxis
    dests <- letwith cs outarrs (pexp iv) $ map (PrimOp . SubExp) xis
    return $ resultBody (map Var inarrs ++ acc' ++ map Var dests)
  return $ LoopOp $
    DoLoop (acc++outarrs) (loopMerge (inarrs++acc++outarrs) 
    (map Var arrexps++initacc++map Var maparrs))
    (ForLoop i (isize inarrs)) loopbody

transformExp e = mapExpM transform e

transformBinding :: Binding -> Binder Basic Binding
transformBinding (Let pat annot e) =
  Let pat annot <$> transformExp e

transform :: Mapper Basic Basic (Binder Basic)
transform = identityMapper {
              mapOnBinding = transformBinding
            , mapOnBody = insertBindingsM . transformBody
            }

newFold :: [SubExp]
        -> Binder Basic (([Ident], [SubExp]), (Ident, SubExp))
newFold accexps = do
  (i, iv) <- newVar "i" $ Basic Int
  initacc <- letSubExps "acc" $ map maybeCopy accexps
  acc <- forM accexps $ \e -> newIdent "acc" (subExpType e)
  return ((acc, initacc), (i, iv))

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
  forM_ (zip params args) $ \(param, arg) ->
    if unique (identType param) then
      letBindNames' [identName param] =<< eCopy (pure arg)
    else
      letBindNames' [identName param] arg
  transformBody body

loopMerge :: [Ident] -> [SubExp] -> [(FParam, SubExp)]
loopMerge vars vals = [ (FParam var (), val) | (var,val) <- zip vars vals ]
