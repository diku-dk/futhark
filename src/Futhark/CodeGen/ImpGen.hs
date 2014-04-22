{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.ImpGen
  ( compileProg
  )
  where

import Control.Applicative
import Control.Monad.RWS

import qualified Data.Array as A
import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Loc

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.InternalRep
import Futhark.MonadFreshNames

data Env = Env {
    envVtable :: HM.HashMap VName Type
  }

newEnv :: Env
newEnv = Env { envVtable = HM.empty }

newtype ImpM a = ImpM (RWS Env Imp.Code VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadState VNameSource, MonadReader Env, MonadWriter Imp.Code)

instance MonadFreshNames ImpM where
  getNameSource = get
  putNameSource = put

runImpM :: ImpM a -> VNameSource -> (a, VNameSource, Imp.Code)
runImpM (ImpM m) = runRWS m newEnv

collect :: ImpM () -> ImpM Imp.Code
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

compileProg :: Prog -> Imp.Program
compileProg prog = snd $ mapAccumL compileFunDec src $ progFunctions prog
  where src = newNameSourceForProg prog

compileType :: TypeBase als Shape -> Imp.Type
compileType t = Imp.Type (elemType t) $ map asImpSize $ arrayDims t
  where asImpSize (Constant (BasicVal (IntVal x)) _) =
          Imp.ConstSize x
        asImpSize (Constant _ _) =
          error "Futhark.CodeGen.ImpGen.compileType: dimension size is a non-integer constant"
        asImpSize (Var v) =
          Imp.VarSize $ identName v

compileParam :: Param -> Imp.Param
compileParam p = Imp.Param (identName p) $ compileType $ identType p

compileParams :: [Param] -> [Imp.Param]
compileParams = map compileParam

compileFunDec :: VNameSource -> FunDec -> (VNameSource, (Name, Imp.Function))
compileFunDec src (fname, rettype, params, body, _) =
  let (outs, src', body') = runImpM compile src
  in (src', (fname, Imp.Function outs (compileParams params) body'))
  where compile = do
          outs <- replicateM (length rettype) $ newVName "out"
          compileBody outs body
          return $ zipWith Imp.Param outs $ map compileType $ bodyType body

compileBody :: [VName] -> Body -> ImpM ()
compileBody targets (Body bnds (Result _ ses _)) = do
  mapM_ compileBinding bnds
  zipWithM_ compileSubExpTo targets ses

compileBinding :: Binding -> ImpM ()
compileBinding (Let pat e) = do
  makeVars pat
  compileExp (map identName pat) e

compileExp :: [VName] -> Exp -> ImpM ()

compileExp targets (SubExps ses _) =
  zipWithM_ compileSubExpTo targets ses

compileExp targets (If cond tbranch fbranch _ _) = do
  tcode <- collect $ compileBody targets tbranch
  fcode <- collect $ compileBody targets fbranch
  tell $ Imp.If (compileSubExp cond) tcode fcode

compileExp targets (Apply fname args _ _) =
  tell $ Imp.Call targets fname $ map (compileSubExp . fst) args

compileExp targets (DoLoop res merge i bound body _) = do
  makeVars mergepat
  zipWithM_ compileSubExpTo mergenames mergeinit
  body' <- collect $ compileBody mergenames body
  tell $ Imp.For (identName i) (compileSubExp bound) body'
  zipWithM_ compileSubExpTo targets $ map Var res
  where (mergepat, mergeinit) = unzip merge
        mergenames = map identName mergepat

compileExp [target] (Not e _) =
  writeExp target $ Imp.UnOp Imp.Not $ compileSubExp e

compileExp [target] (Negate e _) =
  writeExp target $ Imp.UnOp Imp.Negate $ compileSubExp e

compileExp [target] (BinOp bop x y _ _) =
  writeExp target $ Imp.BinOp bop (compileSubExp x) (compileSubExp y)

compileExp [_] (Assert e loc) =
  tell $ Imp.Assert (compileSubExp e) loc

compileExp [target] (Index _ src idxs _) =
  writeExp target $ Imp.Read (identName src) $ map compileSubExp idxs

compileExp [_] (Conjoin {}) =
  return ()

compileExp [target] (ArrayLit es _ _) = do
  allocate target
  forM_ (zip [0..] es) $ \(i,e) ->
    tell $ Imp.Write target [Imp.Constant $ Imp.BasicVal $ IntVal i] $ compileSubExp e

compileExp [target] (Update _ src idxs val _) = do
  writeExp target $ var $ identName src
  tell $ Imp.Write target (map compileSubExp idxs) $ compileSubExp val

compileExp [target] (Iota n _) = do
  i <- newVName "i"
  allocate target
  tell $ Imp.For i (compileSubExp n) $ Imp.Write target [var i] $ var i

compileExp [target] (Replicate n v _) = do
  i <- newVName "i"
  allocate target
  tell $ Imp.For i (compileSubExp n) $ Imp.Write target [var i] $ compileSubExp v

compileExp [target] (Copy e _)
  | arrayRank (subExpType e) == 0 = return ()
  | otherwise =
  writeExp target =<< Imp.Copy <$> expAsName (compileSubExp e)

compileExp [target] (Reshape _ shape src _) = do
  allocate target
  src' <- expAsName $ compileSubExp src
  let shape' = map compileSubExp shape
      srcshape' = map compileSubExp $ arrayDims $ subExpType src
  n <- newVName "n"
  makeVar $ Ident n (Basic Int) noLoc
  writeExp n $ foldl (Imp.BinOp Imp.Times) one shape'
  i <- newVName "i"
  let mult    = Imp.BinOp Imp.Times
      impProd = foldl mult one
  targetsizes <- mapM (expAsName . impProd) $ drop 1 $ tails shape'
  srcsizes <- mapM (expAsName . impProd) $ drop 1 $ tails srcshape'
  -- Some of these index calculations may duplicate computation a
  -- little bit.
  let idxs asizes ashape =
        [ Imp.BinOp Imp.Mod (Imp.BinOp Imp.Divide (var i) (var slicesize)) dimsize
          | (slicesize,dimsize) <- zip asizes ashape ]
      targetidxs = idxs targetsizes shape'
      srcidxs    = idxs srcsizes srcshape'
  tell $ Imp.For i (var n) $ Imp.Write target targetidxs $ Imp.Read src' srcidxs
  where one = Imp.Constant $ Imp.BasicVal $ IntVal 1

compileExp [target] (Concat _ x y _ _) = do
  allocate target
  x' <- expAsName $ compileSubExp x
  y' <- expAsName $ compileSubExp y
  let xsize = compileSubExp $ arraySize 0 $ subExpType x
      ysize = compileSubExp $ arraySize 0 $ subExpType y
  i <- newVName "i"
  tell $ Imp.For i xsize $ Imp.Write target [var i] $
         Imp.Read x' [var i]
  j <- newVName "j"
  tell $ Imp.For j ysize $ Imp.Write target [Imp.BinOp Imp.Plus xsize $ var j] $
         Imp.Read y' [var j]

compileExp [target1, target2] (Split _ n x restsize _) = do
  allocate target1
  allocate target2
  x' <- expAsName $ compileSubExp x
  let n' = compileSubExp n
      restsize' = compileSubExp restsize
  i <- newVName "i"
  tell $ Imp.For i n' $ Imp.Write target1 [var i] $
         Imp.Read x' [var i]
  j <- newVName "i"
  tell $ Imp.For j restsize' $ Imp.Write target2 [var j] $
         Imp.Read x' [Imp.BinOp Imp.Plus n' $ var j]

compileExp _ (Split {}) = fail "ImpGen.compileExp: Incorrect number of targets to split"

compileExp [target] (Rearrange _ perm e _) = do
  allocate target
  e' <- expAsName $ compileSubExp e
  is <- replicateM (length perm) $ newVName "i"
  let sizes = map compileSubExp $ arrayDims $ subExpType e
  tell $ foldl (.) id (zipWith Imp.For is sizes) $
         Imp.Write target (permuteShape perm $ map var is) $
         Imp.Read e' $ map var is

compileExp [target] (Rotate _ n e _) = do
  allocate target
  e' <- expAsName $ compileSubExp e
  let size = compileSubExp $ arraySize 0 $ subExpType e
      n'   = Imp.Constant $ Imp.BasicVal $ IntVal n
  i <- newVName "i"
  tell $ Imp.For i size $
         Imp.Write target [Imp.BinOp Mod (Imp.BinOp Plus n' $ var i) size] $
         Imp.Read e' [var i]

compileExp [_] (Map {}) = soacError

compileExp [_] (Filter {}) = soacError

compileExp [_] (Reduce {}) = soacError

compileExp [_] (Scan {}) = soacError

compileExp [_] (Redomap {}) = soacError

compileExp [] _ = return () -- No arms, no cake.

compileExp (_:_:_) _ = fail "ImpGen.compileExp: Incorrect number of targets"

soacError :: ImpM a
soacError = fail "SOAC encountered in code generator; should have been removed by first-order transform."

writeExp :: VName -> Imp.Exp -> ImpM ()
writeExp target = tell . Imp.Write target []

allocate :: VName -> ImpM ()
allocate target = tell $ Imp.Allocate target

var :: VName -> Imp.Exp
var v = Imp.Read v []

makeVars :: [Ident] -> ImpM ()
makeVars = mapM_ makeVar

makeVar :: Ident -> ImpM ()
makeVar v = do
  shape <- mapM (expAsDimSize . compileSubExp) $ arrayDims t
  tell $ Imp.Declare (identName v) (elemType t) shape
  where t = identType v

compileSubExpTo :: VName -> SubExp -> ImpM ()
compileSubExpTo target se =
  tell $ Imp.Write target [] $ compileSubExp se

compileSubExp :: SubExp -> Imp.Exp
compileSubExp (Constant (BasicVal v) _) =
  Imp.Constant $ Imp.BasicVal v
compileSubExp (Constant val@(ArrayVal arr rt) _) =
  let vs = concatMap flatten $ A.elems arr
      arr' = A.listArray (0,length vs-1) vs
  in Imp.Constant $ Imp.ArrayVal (valueShape val) arr' $ elemType rt
  where flatten (ArrayVal arr' _) = concatMap flatten $ A.elems arr'
        flatten (BasicVal v) = [v]
compileSubExp (Var v) =
  Imp.Read (identName v) []

expAsDimSize :: Imp.Exp -> ImpM Imp.DimSize
expAsDimSize (Imp.Read v []) =
  return $ Imp.VarSize v
expAsDimSize (Imp.Constant (Imp.BasicVal (IntVal x))) =
  return $ Imp.ConstSize x
expAsDimSize e = do
  size <- expAsName e
  return $ Imp.VarSize size

expAsName :: Imp.Exp -> ImpM VName
expAsName (Imp.Read v []) =
  return v
expAsName e = do
  size <- newIdent "size" (Basic Int) noLoc
  makeVar size
  writeExp (identName size) e
  return $ identName size
