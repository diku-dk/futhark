{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module L0C.AccurateSizes
  (
   addSizeInformation
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Loc
import Data.Maybe

import L0C.InternalRep
import L0C.MonadFreshNames

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

sameShapes :: Ident -> Ident -> SizeM a -> SizeM a
sameShapes dest src = local bind
  where bind env =
          case HM.lookup (identName src) $ envVtable env of
            Just srcShape ->
              env { envVtable = HM.insert (identName dest) srcShape $
                                envVtable env }
            Nothing -> error $ "Shape for " ++ textual (identName src) ++ " not found - should never happen."

lookupShape :: Ident -> SizeM [SubExp]
lookupShape v =  do
  shape <- asks $ HM.lookup (identName v) . envVtable
  case shape of Nothing     -> error "Unknown variable"
                Just shape' -> return shape'

lookupSize :: Int -> Ident -> SizeM SubExp
lookupSize i v = do
  shape <- lookupShape v
  return $ shape !! i

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

bodySizes (LetWith cs dest src idx iv body loc) =
  LetWith cs dest src idx iv <$>
          sameShapes dest src (bodySizes body) <*> pure loc

bodySizes (DoLoop merge i bound loopbody letbody loc) =
  DoLoop merge i bound <$>
  bodySizes loopbody <*> bodySizes letbody <*> pure loc -- XXX need shape bindings

bodySizes (Result cs es loc) = do
  es' <- concat <$> mapM addShapes es
  return $ Result cs es' loc
  where addShapes (Constant v cloc) =
          return [Constant v cloc]
        addShapes (Var v) = do
          shape <- asks $ HM.lookup (identName v) . envVtable
          case shape of
            Just shape' -> return $ Var v : shape'
            Nothing     -> return [Var v]

expSizes :: [Ident] -> Exp -> SizeM (SizeTable, [([Ident], Exp)])

expSizes pat e
  | all (basicType . identType) pat =
    return (HM.empty, [(pat, e)])

expSizes pat (Map cs fun args loc) = do
  fun' <- lambdaSizes fun args
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
                 else [(concat all_comp_shapes, Map cs sizefun  args loc)]
  shapes <-
    liftM HM.fromList $ forM (zip inner_shapes args) $ \((v, inner_shape), arg) -> do
      let Var argv = arg
      outer_shape <- lookupSize 0 argv
      return (v, outer_shape : map Var inner_shape)
  return (shapes,
          sizecomp ++
          concat shape_checks ++
          [(pat, Map (cs++concat certs) valuefun args loc)])

expSizes pat (SubExp se) = do
  shape <- subExpShape se
  return (hasShape pat shape, [(pat, SubExp se)])

expSizes pat (Iota e loc) =
  return (hasShape pat [e], [(pat, Iota e loc)])

expSizes pat (Size _ i (Var v) _) = do
  se <- lookupSize i v
  return (HM.empty, [(pat, SubExp se)])

expSizes pat (TupLit es loc) = do
  shapes <- mapM subExpShape es
  return (HM.unions $ map (hasShape pat) shapes, [(pat, TupLit es loc)])

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

expSizes [v1,v2] e@(Split _ ne (Var av) loc) = do
  outer : inner <- lookupShape av
  v2size <- newIdent "v2_size" (Basic Int) loc
  return (HM.fromList [(identName v1, ne : inner),
                       (identName v2, Var v2size : inner)],
          [([v2size], BinOp Minus outer ne (Basic Int) loc),
           ([v1,v2],  e)])

expSizes pat e@(Concat _ (Var v1) (Var v2) loc) = do
  v1n : inner <- lookupShape v1
  v2n : _     <- lookupShape v2
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

subExpShape :: SubExp -> SizeM [SubExp]
subExpShape (Var v)        = lookupShape v
subExpShape (Constant v loc) =
  return [ Constant (BasicVal $ IntVal d) loc | d <- arrayShape v ]

hasShape :: [Ident] -> [SubExp] -> SizeTable
hasShape vs es = HM.fromList [(identName v, es) | v <- vs]

lambdaSizes :: Lambda -> [SubExp] -> SizeM Lambda
lambdaSizes (Lambda params body rettype loc) args = do
  shapes <- mapM (liftM (drop 1) . subExpShape) args
  let arrparams = drop (length params - length args) params
      shapemap = HM.fromList $ zip (map identName arrparams) shapes
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
