{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.AccurateSizes
  (
   addSizeInformation
  )
  where

import Debug.Trace

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Loc
import Data.Maybe

import L0C.InternalRep
import L0C.MonadFreshNames
import L0C.Tools

import qualified Data.HashMap.Lazy as HM

addSizeInformation :: Prog -> Prog
addSizeInformation prog = runSizeM prog $
  Prog <$> mapM functionSizes (progFunctions prog)

type SizeTable = HM.HashMap VName [SubExp]

data SizeEnv = SizeEnv {
    envVtable :: SizeTable
  }

newtype SizeM a = SizeM (ReaderT SizeEnv (State VNameSource) a)
  deriving (Applicative, Functor, Monad,
            MonadReader SizeEnv, MonadState VNameSource)

instance MonadFreshNames SizeM where
  getNameSource = get
  putNameSource = put

bindShapes :: SizeTable -> SizeM a -> SizeM a
bindShapes shapes = local bind
  where bind env = env { envVtable = shapes `HM.union` envVtable env }

lookupShape :: Ident -> SizeM [SubExp]
lookupShape v =  do
  shape <- asks $ HM.lookup (identName v) . envVtable
  case shape of
    Nothing     -> error $ "Unknown variable " ++ textual (identName v)
    Just shape' -> return shape'

runSizeM :: Prog -> SizeM a -> a
runSizeM prog (SizeM m) = evalState (runReaderT m env) src
  where src = newNameSourceForProg prog
        env = SizeEnv {
                envVtable = HM.empty
              }

functionSizes :: FunDec -> SizeM FunDec
functionSizes (fname, rettype, params, body, loc) = do
  (shapes,params') <- patternSizes $ map fromParam params
  let rettype' = typeSizes rettype
  body' <- bindShapes shapes $ bodySizes body
  return (fname, rettype', map toParam params', body', loc)

bodySizes :: Body -> SizeM Body

bodySizes (LetPat pat e body loc) = do
  (shapes, bnds) <- expSizes pat e
  let bind [] = bindShapes shapes $ bodySizes body
      bind ((pat',e'):rest) =
        LetPat pat' e' <$> bind rest <*> pure loc
  bind bnds

bodySizes (LetWith cs dest src idx iv body loc) = do
  shape <- lookupShape src
  LetWith cs dest src idx iv <$>
          bindShapes (hasShape [dest] shape) (bodySizes body) <*> pure loc

bodySizes (DoLoop merge i bound loopbody letbody loc) = do
  (shapes,mergepat') <- patternSizes mergepat
  mergeexp' <- subExpsWithShapes mergeexp
  DoLoop (zip mergepat' mergeexp') i bound <$>
         bindShapes shapes (bodySizes loopbody) <*>
         bindShapes shapes (bodySizes letbody) <*> pure loc
  where (mergepat, mergeexp) = unzip merge

bodySizes (Result cs es loc) =
  Result cs <$> subExpsWithShapes es <*> pure loc

expSizes :: [Ident] -> Exp -> SizeM (SizeTable, [([Ident], Exp)])

expSizes pat (Map _ fun [] loc) =
  -- Empty in, empty out
  return (HM.unions [ hasShape pat $ replicate (n+1) zero
                      | n <- map arrayRank $ lambdaReturnType fun ],
          [ ([v], ArrayLit [] (rowType $ identType v) loc)
            | v <- pat ])
  where zero = Constant (BasicVal $ IntVal 0) loc

expSizes pat (Map cs fun args@(arg:_) loc) = do
  fun' <- lambdaSizes fun [] args
  let (sizefun, valuefun) = splitLambda fun'
  (inner_shapes, all_comp_shapes, checks) <-
    liftM unzip3 $ forM pat $ \v -> do
      let n = arrayRank (identType v) - 1
      comp_shapes <- replicateM n $ newIdent "map_computed_shape"
                                    (arrayType 1 (Basic Int) Unique) loc
      certs  <- replicateM n $ newIdent "map_cert" (Basic Cert) loc
      shapes <- replicateM n $ newIdent "map_shape" (Basic Int) loc
      return ((identName v, shapes),
              comp_shapes,
              (certs,
               [([cert, shape],
                 Apply (nameFromString "all_equal")
                 [(Var comp_shape, Observe)]
                 [Basic Cert, Basic Int] loc)
                | (cert, shape, comp_shape) <- zip3 certs shapes comp_shapes ]))
  let (certs, shape_checks) = unzip checks
      sizecomp = if null $ concat all_comp_shapes
                 then []
                 else [(concat all_comp_shapes, Map cs sizefun args loc)]
  outer_shape : _ <- subExpShape arg
  let shapes = HM.fromList [ (v, outer_shape : map Var inner_shape)
                             | (v, inner_shape) <- inner_shapes ]
  return (shapes,
          sizecomp ++
          concat shape_checks ++
          [(pat, Map (cs++concat certs) valuefun args loc)])

expSizes pat (Reduce cs fun args loc) = do
  -- XXX Shouldn't we use a modified function that checks the size...?
  let (accargs, arrargs) = unzip args
  (_, valuefun) <- splitLambda <$> lambdaSizes fun accargs arrargs
  shapes <- mapM subExpShape accargs
  return (HM.unions $ map (hasShape pat) shapes,
          [(pat, Reduce cs valuefun args loc)])

expSizes pat (Scan cs fun args loc) = do
  -- XXX Shouldn't we use a modified function that checks the size...?
  let (accargs, arrargs) = unzip args
  (_, valuefun) <- splitLambda <$> lambdaSizes fun accargs arrargs
  shapes <- mapM subExpShape arrargs
  return (HM.unions $ map (hasShape pat) shapes,
          [(pat, Scan cs valuefun args loc)])

expSizes pat (Filter cs fun args loc) = do
  (_, valuefun) <- splitLambda <$> lambdaSizes fun [] args
  argshapes <- mapM (liftM (drop 1) . subExpShape) args
  outershape <- newIdent "filter_size" (Basic Int) loc
  markarray  <- newIdent "filter_mark" (arrayType 1 (Basic Int) Nonunique) loc
  markfunBody <- flip mapTailM (lambdaBody fun) $ \funcs es -> do
                   let [ok] = es -- XXX
                       result e = Result funcs [e] loc
                   ok_int <- newIdent "ok" (Basic Int) loc
                   return $ LetPat [ok_int] (If ok (result one) (result zero) [Basic Int] loc)
                            (Result funcs [Var ok_int] loc) loc
  countfun <- binOpLambda Plus (Basic Int) loc
  let markfun = fun { lambdaBody = markfunBody
                    , lambdaReturnType = [Basic Int]
                    }
      markmap = Map cs markfun args loc
      countcomp = Reduce cs countfun [(zero, Var markarray)] loc
  return (HM.unions $ map (hasShape pat . (Var outershape:)) argshapes,
          [([markarray], markmap),
           ([outershape], countcomp),
           (pat, Filter cs valuefun args loc)])
  where zero = Constant (BasicVal $ IntVal 0) loc
        one  = Constant (BasicVal $ IntVal 1) loc

expSizes pat (Redomap cs outerfun innerfun accargs arrargs loc) = do
-- XXX Shouldn't we use a modified function that checks the size...?
  (_, outervaluefun) <- splitLambda <$> lambdaSizes outerfun accargs arrargs
  (_, innervaluefun) <- splitLambda <$> lambdaSizes innerfun accargs arrargs
  shapes <- mapM subExpShape accargs
  return (HM.unions $ map (hasShape pat) shapes,
          [(pat, Redomap cs outervaluefun innervaluefun accargs arrargs loc)])

expSizes pat (SubExp se) = do
  shape <- subExpShape se
  return (hasShape pat shape, [(pat, SubExp se)])

expSizes pat e@(Iota ne _) =
  return (hasShape pat [ne], [(pat, e)])

expSizes pat e@(Replicate ne ve _) = do
  inner_shape <- subExpShape ve
  return (hasShape pat $ ne : inner_shape, [(pat, e)])

expSizes pat (Size _ i se _) = do
  shape <- subExpShape se
  return (HM.empty, [(pat, SubExp $ shape !! i)])

expSizes pat e@(TupLit es _) = do
  shapes <- mapM subExpShape es
  return (HM.unions $ zipWith hasShape (map single pat) shapes,
          [(pat, e)])
  where single x = [x]

expSizes pat (If ce tb fb rettype loc) = do
  tb' <- bodySizes tb
  fb' <- bodySizes fb
  (shapes, pat') <- patternSizes pat
  return (shapes, [(pat', If ce tb' fb' rettype' loc)])
  where rettype' = typeSizes rettype

expSizes pat (Apply fname args rettype loc) = do
  (shapes, pat') <- patternSizes pat
  args' <- liftM concat $ forM args $ \(e,d) -> do
             sizes <- subExpShape e
             return $ (e,d) : [ (size, Observe) | size <- sizes ]
  return (shapes, [(pat', Apply fname args' (typeSizes rettype) loc)])

expSizes pat e@(BinOp {}) =
  return (HM.empty, [(pat, e)])

expSizes pat e@(Not {}) =
  return (HM.empty, [(pat, e)])

expSizes pat e@(Negate {}) =
  return (HM.empty, [(pat, e)])

expSizes pat e@(Assert {}) =
  return (HM.empty, [(pat, e)])

expSizes pat e@(Conjoin {}) =
  return (HM.empty, [(pat, e)])

expSizes pat e@(Copy se _) = do
  shape <- subExpShape se
  return (hasShape pat shape, [(pat, e)])

expSizes [v1,v2] e@(Split _ ne ae loc) = do
  outer : inner <- subExpShape ae
  v2size <- newIdent "v2_size" (Basic Int) loc
  return (HM.fromList [(identName v1, ne : inner),
                       (identName v2, Var v2size : inner)],
          [([v2size], BinOp Minus outer ne (Basic Int) loc),
           ([v1,v2],  e)])

expSizes _ (Split {}) = error "Invalid split expression"

expSizes pat e@(Concat _ se1 se2 loc) = do
  v1n : inner <- subExpShape se1
  v2n : _     <- subExpShape se2
  concat_size <- newIdent "concat_size" (Basic Int) loc
  return (hasShape pat $ Var concat_size : inner,
          [([concat_size], BinOp Plus v1n v2n (Basic Int) loc),
            (pat, e)])

expSizes pat e@(ArrayLit [] rt loc) =
  return (hasShape pat $ replicate (arrayRank rt+1) zero,
          [(pat, e)])
  where zero = Constant (BasicVal $ IntVal 0) loc

expSizes pat e@(ArrayLit elems@(se:_) _ loc) = do
  shape <- subExpShape se
  -- XXX: Check that they are all equal?
  return (hasShape pat $ outer : shape, [(pat, e)])
  where outer = Constant (BasicVal $ IntVal $ length elems) loc

expSizes pat e@(Index _ v idxs _) = do
  shape <- subExpShape $ Var v
  return (hasShape pat (drop (length idxs) shape),
          [(pat, e)])

expSizes pat e@(Reshape _ shape _ _) =
  return (hasShape pat shape, [(pat, e)])

expSizes pat e@(Rearrange _ perm ae _) = do
  shape <- subExpShape ae
  return (hasShape pat (permuteDims perm shape), [(pat, e)])

subExpShape :: SubExp -> SizeM [SubExp]
subExpShape (Var v)
  | basicType $ identType v = return []
  | otherwise               = lookupShape v
subExpShape (Constant v loc) =
  return [ Constant (BasicVal $ IntVal d) loc | d <- arrayShape v ]

subExpsWithShapes :: [SubExp] -> SizeM [SubExp]
subExpsWithShapes es = concat <$> mapM addShapes es
  where addShapes se = do
          shape <- subExpShape se
          return $ se : shape

hasShape :: [Ident] -> [SubExp] -> SizeTable
hasShape vs es = HM.fromList [(identName v, es) | v <- vs]

lambdaSizes :: Lambda -> [SubExp] -> [SubExp] -> SizeM Lambda
lambdaSizes (Lambda params body rettype loc) accargs arrargs = do
  accshapes <- mapM subExpShape accargs
  arrshapes <- mapM (liftM (drop 1) . subExpShape) arrargs
  let (accparams, arrparams) = splitAt (length accargs) params
      shapemap = HM.fromList $ zip (map identName arrparams) arrshapes ++
                               zip (map identName accparams) accshapes
  body' <- bindShapes shapemap $ bodySizes body
  return $ Lambda params body' rettype' loc
  where rettype' = typeSizes rettype

splitLambda :: Lambda -> (Lambda, Lambda)
splitLambda (Lambda params body rettype loc) =
  (Lambda params sizeBody  sizeRettype  loc,
   Lambda params valueBody valueRettype loc)
    where sizeBody = flip mapTail body $ \cs es ->
                     Result cs (fst $ splitTyped subExpType es) loc
          valueBody = flip mapTail body $ \cs es ->
                      Result cs (snd $ splitTyped subExpType es) loc
          (sizeRettype, valueRettype) = splitTyped id rettype

          splitTyped _ []     = ([],[])
          splitTyped f (x:xs) =
            let (sizes, values) = splitTyped f (drop n xs)
            in (take n xs ++ sizes, x : values)
            where n = arrayRank $ f x

patternSizes :: [Ident] -> SizeM (SizeTable, [Ident])
patternSizes pat = do
  (shapes, pat') <- liftM unzip $ forM pat $ \v -> do
    let rank = arrayRank $ identType v
    names <- replicateM rank $ newIdent "param_size" (Basic Int) $ srclocOf v
    return ((identName v, map Var names),
            v : names)
  return (HM.fromList shapes, concat pat')

typeSizes :: [TypeBase als] -> [TypeBase als]
typeSizes = concatMap addShapeTypes
  where addShapeTypes t = t : replicate (arrayRank t) (Basic Int)
