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
  let zero = Constant (BasicVal $ IntVal 0) loc
  inarrs <- letExps "inarr" $ map SubExp arrs
  (i, iv) <- newVar loc "i" $ Basic Int
  let n = size inarrs
  letbody <- insertBindings $ do
    y <- bodyBind =<< transformLambda fun (index cs inarrs zero)
    outarr <- newResultArray (SubExp n) $ map SubExp y
    let outarrv = Result [] (map Var outarr) loc
        loopbody = runBinder $ do
          x <- bodyBind =<< transformLambda fun (index cs inarrs iv)
          dests <- letwith cs outarr (pexp iv) $ map SubExp x
          return $ Result [] (map Var dests) loc
    eDoLoop (zip outarr $ map (pexp . Var) outarr)
            i (pexp n) loopbody (pure outarrv) loc
  nonempty <- letExp "nonempty" =<<
              eBinOp Less (pexp zero) (pexp n) (Basic Bool) loc
  blank <- letTupExp "blank" =<< blankArray (typeOf mape) loc
  return $ If (Var nonempty)
              letbody
              (Result [] (map Var blank) loc)
              (typeOf mape) loc

transformExp (Reduce cs fun args loc) = do
  (arr, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  loopbody <- insertBindings $ transformLambda fun $
              map (SubExp . Var) acc ++ index cs arr iv
  loopBind (zip acc initacc) i (size arr) loopbody
  return $ TupLit (map Var acc) loc
  where (accexps, arrexps) = unzip args

transformExp (Scan cs fun args loc) = do
  (arr, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  loopbody <- insertBindings $ do
    x <- bodyBind =<<
         transformLambda fun (map (SubExp . Var) acc ++ index cs arr iv)
    dests <- letwith cs arr (pexp iv) $ map SubExp x
    irows <- letSubExps "row" $ index cs dests iv
    rowcopies <- letExps "copy" [ Copy irow loc | irow <- irows ]
    return $ Result [] (map Var $ rowcopies ++ dests) loc
  loopBind (zip (acc ++ arr) (initacc ++ map Var arr)) -- XXX Shadowing
           i (size arr) loopbody
  return $ TupLit (map Var arr) loc
  where (accexps, arrexps) = unzip args

transformExp filtere@(Filter cs fun arrexps _ loc) = do
  arr <- letExps "arr" $ map SubExp arrexps
  let nv = size arr
      rowtypes = map (rowType . identType) arr
  (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
  (i, iv) <- newVar loc "i" $ Basic Int
  test <- insertBindings $ do
   [check] <- bodyBind =<< transformLambda fun (map (SubExp . Var) xs) -- XXX
   res <- letSubExp "res" $
          If check (Result [] [intval 1] loc) (Result [] [intval 0] loc) [Basic Int] loc
   return $ Result [] [res] loc
  mape <- letExp "mape" <=< transformExp $
          Map cs (Lambda (map toParam xs) test [Basic Int] loc) (map Var arr) loc
  let indexin0 = index cs arr $ intval 0
  plus <- do
    (a,av) <- newVar loc "a" (Basic Int)
    (b,bv) <- newVar loc "b" (Basic Int)
    body <- insertBindings $ do
      res <- letSubExp "sum" $ BinOp Plus av bv (Basic Int) loc
      return $ Result [] [res] loc
    return $ Lambda [toParam a, toParam b] body [Basic Int] loc
  scan <- transformExp $ Scan cs plus [(intval 0,Var mape)] loc
  ia <- letExp "ia" scan
  let indexia ind = eIndex cs ia [ind] loc
      sub1 e = eBinOp Minus e (pexp $ intval 1) (Basic Int) loc
  indexiaend <- indexia $ sub1 $ pexp nv
  res <- newResultArray indexiaend indexin0
  let resv = Result [] (map Var res) loc
  loopbody <- insertBindings $ do
    let indexi = indexia $ pexp iv
        indexin = index cs arr iv
        indexinm1 = indexia $ sub1 $ pexp iv
        update = do
          dest <- letwith cs res (sub1 indexi) indexin
          return $ TupLit (map Var dest) loc
    eBody $
      eIf (eIf (pure $ BinOp Equal iv (intval 0) (Basic Bool) loc)
               (eBody $ eBinOp Equal indexi (pexp $ intval 0) (Basic Bool) loc)
               (eBody $ eBinOp Equal indexi indexinm1 (Basic Bool) loc)
               [Basic Bool] loc)
          (pure resv) (eBody update) (bodyType resv) loc
  nonempty <- eDoLoop (zip res $ map (pexp . Var) res)
              i (pexp nv) (pure loopbody) (pure resv) loc -- XXX, name shadowing
  isempty <- letSubExp "isempty" =<<
             eBinOp Equal (pexp nv) (pexp $ intval 0) (Basic Bool) loc
  blank <- letTupExp "blank" =<< blankArray (typeOf filtere) loc
  return $ If isempty (Result [] (map Var blank) loc)
           nonempty (bodyType nonempty) loc
  where intval x = Constant (BasicVal $ IntVal x) loc

transformExp (Redomap cs _ innerfun accexps arrexps loc) = do
  (arr, (acc, initacc), (i, iv)) <- newFold loc arrexps accexps
  loopbody <- insertBindings $ transformLambda innerfun
                               (map (SubExp . Var) acc ++ index cs arr iv)
  loopBind (zip acc initacc) i (size arr) loopbody
  return $ TupLit (map Var acc) loc

transformExp e = mapExpM transform e

transform :: Mapper Binder
transform = identityMapper {
              mapOnExp = transformExp
            , mapOnBody = insertBindings . transformBody
            }

newFold :: SrcLoc -> [SubExp] -> [SubExp]
        -> Binder ([Ident], ([Ident], [SubExp]), (Ident, SubExp))
newFold loc arrexps accexps = do
  (i, iv) <- newVar loc "i" $ Basic Int
  arr <- letExps "arr" $ map maybeCopy arrexps
  initacc <- letSubExps "acc" $ map maybeCopy accexps
  acc <- forM initacc $ \e -> newIdent "acc" (subExpType e) $ srclocOf e
  return (arr, (acc, initacc), (i, iv))

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
                  Index cs arr [i] $ srclocOf i

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
  let update (dest, k, v) = letWithBind cs dest k [i'] v
  mapM_ update $ zip3 dests ks vs'
  return dests

size :: [Ident] -> SubExp
size (v:_)
  | se : _ <- shapeDims $ arrayShape $ identType v = se
size _ = Constant (BasicVal $ IntVal 0) noLoc

pexp :: Applicative f => SubExp -> f Exp
pexp = pure . SubExp

transformLambda :: Lambda -> [Exp] -> Binder Body
transformLambda (Lambda params body _ _) args = do
  zipWithM_ letBind (map ((:[]) . fromParam) params) args
  transformBody body
