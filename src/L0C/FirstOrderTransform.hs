-- | The code generator cannot handle the array combinators (@map@ and
-- friends), so this module was written to transform them into the
-- equivalent do-loops.  The transformation is currently rather naive
-- - it's certainly worth considering when we can express such
-- transformations in-place.  This module should be run very late in
-- the compilation pipeline, ideally just before the code generator.
module L0C.FirstOrderTransform
  ( transformProg )
  where

import Control.Applicative
import Control.Monad.State

import Data.Loc

import L0C.L0
import L0C.FreshNames

type TransformM = State VNameSource

-- | Return a new, fresh name, with the given string being part of the
-- name.
new :: String -> TransformM VName
new = state . newVName

-- | Perform the first-order transformation on an L0 program.
transformProg :: Prog -> Prog
transformProg prog =
  Prog $ runTransformM $ mapM transformFunDec $ progFunctions prog
  where runTransformM m = evalState m $ newNameSourceForProg prog

transformFunDec :: FunDec -> TransformM FunDec
transformFunDec (fname, rettype, params, body, loc) = do
  body' <- transformExp body
  return (fname, rettype, params, body', loc)

-- We sometimes have to allocate new arrays up front.  This is a bit
-- tricky, as in case the new array is multidimensional, we need to
-- compute the first element in order to get the proper size.  For
-- example, in the case for map, we evaluate the function on the first
-- element of the array and use that to construct the array for the
-- rest.  If the input array is empty, we simply return an empty
-- output array.
transformExp :: Exp -> TransformM Exp

transformExp mape@(Map fun e intype loc) =
  newLet "inarr" e $ \inarr inarrv -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    rs <- rows inarrv loc
    newLet "n" rs $ \_ nv -> do
      let zero = Literal (IntVal 0) loc
          index0 = Index inarr [zero] intype loc
          indexi = Index inarr [iv] intype loc
      funcall0 <- transformLambda fun [index0]
      funcall <- transformLambda fun [indexi]
      newLet "outarr" (Replicate nv funcall0 loc) $ \outarr outarrv -> do
        let branch = If (BinOp Less zero nv (Elem Bool) loc)
                     letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     (typeOf mape) loc
            letbody = DoLoop (Id outarr) outarrv i nv loopbody outarrv loc
            loopbody = LetWith outarr outarr [iv] funcall outarrv loc
        return branch

transformExp (Reduce fun accexp arrexp intype loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] intype loc
    funcall <- transformLambda fun [accv, indexi]
    rs <- rows arrv loc
    let loop = DoLoop (Id acc) accv i rs loopbody accv loc
        loopbody = LetPat (Id acc) funcall accv loc
    return loop

transformExp (Scan fun accexp arrexp intype loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] intype loc
    funcall <- transformLambda fun [accv, indexi]
    rs <- rows arrv loc
    let loop = DoLoop (TupId [Id acc, Id arr] loc) (TupLit [accv, arrv] loc) i rs loopbody arrv loc
        loopbody = LetWith arr arr [iv] funcall (TupLit [indexi, arrv] loc) loc
    return loop

transformExp (Filter fun arrexp rowtype loc) =
  newLet "arr" arrexp $ \arr arrv -> do
    rs <- rows arrv loc
    newLet "n" rs $ \_ nv -> do
      let checkempty nonempty = If (BinOp Equal nv (intval 0) (Elem Bool) loc)
                                (Literal (emptyArray rowtype) loc) nonempty
                                (typeOf arrexp) loc
      (x, xv) <- newVar loc "x" rowtype
      (i, iv) <- newVar loc "i" (Elem Int)
      fun' <- transformLambda fun [xv]
      let branch = If fun' (intval 1) (intval 0) (Elem Int) loc
          indexin0 = Index arr [intval 0] rowtype loc
          indexin = Index arr [iv] rowtype loc
      mape <- transformExp $
              Map (AnonymFun [toParam x] branch (Elem Int) loc) arrv rowtype loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ AnonymFun [toParam a, toParam b]
                 (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
      scan <- transformExp $ Scan plus (intval 0) mape (Elem Int) loc
      newLet "ia" scan $ \ia _ -> do
        let indexia ind = Index ia [ind] (Elem Int) loc
            indexiaend = indexia (BinOp Minus nv (intval 1) (Elem Int) loc)
            indexi = indexia iv
            indexim1 = indexia (BinOp Minus iv (intval 1) (Elem Int) loc)
        newLet "res" (Replicate indexiaend indexin0 loc) $ \res resv -> do
          let loop = DoLoop (Id res) resv i nv loopbody resv loc
              loopbody = If (Or (BinOp Equal iv (intval 0) (Elem Bool) loc)
                                (And (BinOp Less (intval 0) iv (Elem Bool) loc)
                                     (BinOp Equal indexi indexim1 (Elem Bool) loc) loc)
                             loc)
                         resv update (typeOf resv) loc
              update = LetWith res res [BinOp Minus indexi (intval 1) (Elem Int) loc]
                       indexin resv loc
          return $ checkempty loop
        where intval x = Literal (IntVal x) loc

transformExp (Mapall fun arrexp loc) = transformExp =<< toMap arrexp
  where toMap e = case peelArray 1 $ typeOf e of
                    Just et -> do
                      (x,xv) <- newVar loc "x" et
                      body <- toMap xv
                      let ot = arrayType (arrayDims et) (lambdaReturnType fun) Nonunique
                      return $ Map (AnonymFun [toParam x] body ot loc) e et loc
                    _ -> transformLambda fun [e]

transformExp (Redomap redfun mapfun accexp arrexp _ loc) =
  newReduction loc arrexp accexp $ \(arr, arrv) (acc, accv) (i, iv) -> do
    let indexi = Index arr [iv] (stripArray 1 $ typeOf arrexp) loc
    mapfuncall <- transformLambda mapfun [indexi]
    redfuncall <- transformLambda redfun [accv, mapfuncall]
    rs <- rows arrv loc
    let loop = DoLoop (Id acc) accv i rs loopbody accv loc
        loopbody = LetWith acc acc [] redfuncall accv loc
    return loop

transformExp mape@(Map2 fun arrs _ loc) = do
  let zero = Literal (IntVal 0) loc
  newLets "inarr" arrs $ \inarrs _ -> do
    (i, iv) <- newVar loc "i" (Elem Int)
    sz <- size inarrs
    newLet "n" sz $ \_ nv -> do
      funcall0 <- transformLambda fun $ index inarrs zero
      funcall <- transformLambda fun $ index inarrs iv
      newResultArray nv funcall0 $ \outarr outarrv -> do
        loopbody <- letwith outarr iv funcall outarrv
        let branch = If (BinOp Less zero nv (Elem Bool) loc)
                     letbody
                     (maybeCopy $ Literal (blankValue $ typeOf mape) loc)
                     (typeOf mape) loc
            letbody = DoLoop (pattern outarr loc) outarrv i nv loopbody
                      outarrv loc
        return branch

transformExp (Reduce2 fun accexp arrexps _ loc) =
  newReduction2 loc arrexps accexp $ \(arr, _) (acc, accv) (i, iv) -> do
    funcall <- transformLambda fun $ map Var acc ++ index arr iv
    sz <- size arr
    let loop = DoLoop (pattern acc loc) accv i sz
               funcall accv loc
    return loop

transformExp (Scan2 fun accexp arrexps _ loc) =
  newReduction2 loc arrexps accexp $ \(arr, arrv) (acc, _) (i, iv) -> do
    funcall <- transformLambda fun $ map Var acc ++ index arr iv
    loopbody <- letwith arr iv funcall $
                TupLit (index arr iv++map Var arr) loc
    sz <- size arr
    let looppat = case pattern acc loc of
                    Id k         -> TupId (Id k : map Id arr) loc
                    TupId pats _ -> TupId (pats ++ map Id arr) loc
        loop = DoLoop looppat (TupLit (map Var $ acc ++ arr) loc)
               i sz loopbody arrv loc
    return loop

transformExp filtere@(Filter2 fun arrexps loc) =
  newLets "arr" arrexps $ \arr _ -> do
    sz <- size arr
    newLet "n" sz $ \_ nv -> do
      let checkempty nonempty =
            If (BinOp Equal nv (intval 0) (Elem Bool) loc)
            (Literal (blankValue $ typeOf filtere) loc) nonempty
            (typeOf filtere) loc
          rowtypes = map (rowType . identType) arr
      (xs, _) <- unzip <$> mapM (newVar loc "x") rowtypes
      (i, iv) <- newVar loc "i" $ Elem Int
      fun' <- transformLambda fun $ map Var xs
      let branch = If fun' (intval 1) (intval 0) (Elem Int) loc
          indexin0 = index arr $ intval 0
          indexin = index arr iv
          rowtype = case rowtypes of [t] -> t
                                     _   -> Elem $ Tuple rowtypes
      mape <- transformExp $
              Map2 (AnonymFun (map toParam xs) branch (Elem Int) loc) (map Var arr)
              rowtype loc
      plus <- do
        (a,av) <- newVar loc "a" (Elem Int)
        (b,bv) <- newVar loc "b" (Elem Int)
        return $ AnonymFun [toParam a, toParam b] (BinOp Plus av bv (Elem Int) loc) (Elem Int) loc
      scan <- transformExp $ Scan2 plus [intval 0] [mape] (Elem Int) loc
      newLet "ia" scan $ \ia _ -> do
        let indexia ind = Index ia [ind] (Elem Int) loc
            indexiaend = indexia (sub1 nv)
            indexi = indexia iv
            indexim1 = indexia (sub1 iv)
            tup es = case es of [e] -> e
                                _   -> TupLit es loc
        newResultArray indexiaend (tup indexin0) $ \res resv -> do
          update <- letwith res (sub1 indexi) (tup indexin) resv
          let loop = DoLoop (pattern res loc) resv i nv loopbody resv loc
              loopbody = If (Or (BinOp Equal iv (intval 0) (Elem Bool) loc)
                                (And (BinOp Less (intval 0) iv (Elem Bool) loc)
                                     (BinOp Equal indexi indexim1 (Elem Bool) loc) loc)
                             loc)
                         resv update (typeOf resv) loc
          return $ checkempty loop
  where intval x = Literal (IntVal x) loc
        sub1 e = BinOp Minus e (intval 1) (Elem Int) loc

transformExp (Mapall2 fun arrexps loc) = transformExp =<< toMap arrexps
  where toMap es =
          case mapM (peelArray 1 . typeOf) es of
            Just rts -> do
              (xs,_) <- unzip <$> mapM (newVar loc "x") rts
              body <- toMap $ map Var xs
              return $ Map2 (AnonymFun (map toParam xs) body (toDecl $ typeOf body) loc)
                       es (Elem $ Tuple rts) loc
            _ -> transformLambda fun es

transformExp (Redomap2 redfun mapfun accexps arrexps _ loc) =
  newReduction2 loc arrexps accexps $ \(arr, _) (acc, accv) (i, iv) -> do
    mapfuncall <- transformLambda mapfun $ index arr iv
    sz <- size arr
    let loop loopbody = DoLoop (pattern acc loc) accv i sz loopbody accv loc
    case typeOf mapfuncall of
      Elem (Tuple ts) -> do
        names <- mapM (liftM fst . newVar loc "mapres") ts
        redfuncall <- transformLambda redfun $ map Var $ acc ++ names
        return $ loop $ LetPat (TupId (map Id names) loc) mapfuncall redfuncall loc
      t -> do
        name <- fst <$> newVar loc "mapres" t
        redfuncall <- transformLambda redfun $ map Var $ acc ++ [name]
        return $ loop $ LetPat (Id name) mapfuncall redfuncall loc

transformExp e = mapExpM transform e
  where transform = identityMapper {
                      mapOnExp = transformExp
                    }

newReduction :: SrcLoc -> Exp -> Exp
             -> ((Ident, Exp) -> (Ident, Exp) -> (Ident, Exp) -> TransformM Exp)
             -> TransformM Exp
newReduction loc arrexp accexp body =
  newLet "arr" arrexp $ \arr arrv ->
    newLet "acc" accexp $ \acc accv -> do
      (i, iv) <- newVar loc "i" (Elem Int)
      body (arr, arrv) (acc, accv) (i, iv)

newReduction2 :: SrcLoc -> [Exp] -> [Exp]
             -> (([Ident], Exp) -> ([Ident], Exp) -> (Ident, Exp) -> TransformM Exp)
             -> TransformM Exp
newReduction2 loc arrexps accexps body = do
  (i, iv) <- newVar loc "i" (Elem Int)
  newLets "arr" arrexps $ \arr arrv ->
    case accexps of
      [e] -> do
        let t = typeOf e
        (acc, accv) <- newVar loc "acc" t
        let binder inner = LetPat (Id acc) e inner loc
        binder <$> body (arr, arrv) ([acc], accv) (i, iv)
      es -> do
        let ets = map typeOf es
        (names, namevs) <- unzip <$> mapM (newVar loc "acc") ets
        let binder inner = LetPat (TupId (map Id names) loc) (TupLit accexps loc) inner loc
        binder <$> body (arr, arrv) (names, TupLit namevs loc) (i, iv)

newLet :: String -> Exp -> (Ident -> Exp -> TransformM Exp)
       -> TransformM Exp
newLet name e body = do
  e' <- liftM maybeCopy $ transformExp e
  (x,xv) <- newVar loc name (typeOf e')
  let xlet inner = LetPat (Id x) e' inner loc
  xlet <$> body x xv
  where loc = srclocOf e

newLets :: String -> [Exp] -> ([Ident] -> Exp -> TransformM Exp)
        -> TransformM Exp
newLets k es body = newLets' es []
  where newLets' [] [name]      = body [name] $ Var name
        newLets' [] names       =
          body (reverse names) $ TupLit (map Var $ reverse names) noLoc
        newLets' (e:rest) names =
          newLet k e $ \name _ -> newLets' rest (name:names)

newVar :: SrcLoc -> String -> Type -> TransformM (Ident, Exp)
newVar loc name tp = do
  x <- new name
  return (Ident x tp loc, Var $ Ident x tp loc)

-- | @maybeCopy e@ returns a copy expression containing @e@ if @e@ is
-- not unique or a basic type, otherwise just returns @e@ itself.
maybeCopy :: Exp -> Exp
maybeCopy e
  | unique (typeOf e) || basicType (typeOf e)  = e
  | otherwise = Copy e $ srclocOf e

index :: [Ident] -> Exp -> [Exp]
index arrs i = flip map arrs $ \arr ->
               Index arr [i] (stripArray 1 $ identType arr) $ srclocOf i

newResultArray :: Exp -> Exp -> ([Ident] -> Exp -> TransformM Exp) -> TransformM Exp
newResultArray sizeexp valueexp body =
  case typeOf valueexp of
    Elem (Tuple ets) -> do
      (names, namevs) <- unzip <$> mapM (newVar loc "outarr_v") ets
      let bnd inner = LetPat (TupId (map Id names) loc ) valueexp inner loc
          arrexp = [ maybeCopy $ Replicate sizeexp namev loc
                     | namev <- namevs ]
      bnd <$> newLets "outarr" arrexp body
    _ -> do let rep = maybeCopy $ Replicate sizeexp valueexp loc
            (name, namev) <- newVar loc "outarr" $ typeOf rep
            let bnd inner = LetPat (Id name) rep inner loc
            bnd <$> body [name] namev
  where loc = srclocOf valueexp

letwith :: [Ident] -> Exp -> Exp -> Exp -> TransformM Exp
letwith ks i v body =
  case typeOf v of
    Elem (Tuple ets) -> do
      names <- mapM (liftM fst . newVar loc "tup") ets
      let comb inner (k, name) = LetWith k k [i] (Var name) inner loc
      return $ LetPat (TupId (map Id names) loc) v
               (foldl comb body $ zip ks names) loc
    _ -> do let comb inner k = LetWith k k [i] v inner loc
            return $ foldl comb body ks
  where loc = srclocOf body

pattern :: [Ident] -> SrcLoc -> TupIdent
pattern [k] _ = Id k
pattern ks loc = TupId (map Id ks) loc

rows :: Exp -> SrcLoc -> TransformM Exp
rows e loc = do
  (rs, rsv) <- newVar loc "rows" $ Elem Int
  case arrayDims $ typeOf e of
    1 -> return $ LetPat (Id rs) (Shape e loc) rsv loc
    n -> do
      names <- replicateM (n-1) $
               liftM fst . newVar loc "dim" $ Elem Int
      return $ LetPat (TupId (map Id $ rs:names) loc) (Shape e loc) rsv loc

size :: [Ident] -> TransformM Exp
size [] = return $ Literal (IntVal 0) noLoc
size (k:_) = rows (Var k) $ srclocOf k

transformLambda :: Lambda -> [Exp] -> TransformM Exp
transformLambda (AnonymFun params body _ loc) args = do
  body' <- transformExp body
  return $ foldl bind body' $ zip (map fromParam params) args
  where bind e (Ident pname ptype _, arg) = LetPat (Id $ Ident pname ptype loc) arg e loc
transformLambda (CurryFun fname curryargs rettype loc) args = do
  curryargs' <- mapM transformExp curryargs
  return $ Apply fname (curryargs'++args) rettype loc
