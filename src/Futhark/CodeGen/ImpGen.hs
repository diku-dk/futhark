{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.CodeGen.ImpGen
  ( compileProg
  -- * Pluggable compiler
  , ExpCompiler
  , ExpCompilerResult (..)
  -- * Monadic compiler interface
  , ImpM
  , compileType
  )
  where

import Control.Applicative
import Control.Monad.RWS

import qualified Data.HashMap.Lazy as HM
import Data.List
import Data.Loc

import qualified Futhark.CodeGen.ImpCode as Imp
import Futhark.Representation.ExplicitMemory
import qualified Futhark.Representation.ExplicitMemory.IndexFunction.Unsafe as IxFun
import Futhark.MonadFreshNames

-- | A substitute expression compiler, tried before the main
-- expression compilation function.
type ExpCompiler op = [VName] -> Exp -> ImpM op (ExpCompilerResult op)

-- | The result of the substitute expression compiler.
data ExpCompilerResult op =
      CompileBindings [Binding]
    -- ^ New bindings.  Note that the bound expressions will
    -- themselves be compiled using the expression compiler.
    | CompileExp Exp
    -- ^ A new expression (or possibly the same as the input) - this
    -- will not be passed back to the expression compiler, but instead
    -- processed with the default action.
    | Done
    -- ^ Some code was added via the monadic interface.

data ArrayEntry = ArrayEntry {
    entryArrayLocation :: Imp.MemLocation
  , entryArrayElemType :: BasicType
  , entryArrayShape :: [Imp.DimSize]
  }

data Env op = Env {
    envVtable :: HM.HashMap VName ArrayEntry
  , envExpCompiler :: ExpCompiler op
  }

newEnv :: ExpCompiler op -> Env op
newEnv ec = Env { envVtable = HM.empty
                , envExpCompiler = ec
                }

newtype ImpM op a = ImpM (RWS (Env op) (Imp.Code op) VNameSource a)
  deriving (Functor, Applicative, Monad,
            MonadState VNameSource,
            MonadReader (Env op),
            MonadWriter (Imp.Code op))

instance MonadFreshNames (ImpM op) where
  getNameSource = get
  putNameSource = put

runImpM :: ImpM op a -> ExpCompiler op -> VNameSource -> (a, VNameSource, Imp.Code op)
runImpM (ImpM m) = runRWS m . newEnv

collect :: ImpM op () -> ImpM op (Imp.Code op)
collect m = pass $ do
  ((), code) <- listen m
  return (code, const mempty)

compileProg :: ExpCompiler op -> Prog -> Imp.Program op
compileProg ec prog =
  Imp.Program $ snd $ mapAccumL (compileFunDec ec) src $ progFunctions prog
  where src = newNameSourceForProg prog

compileType :: TypeBase Shape -> ImpM op Imp.Type
compileType (Mem size) =
  Imp.Mem <$> subExpToDimSize size
compileType t =
  return $ Imp.Value $ Imp.Type (elemType t) $ map asImpSize $ arrayDims t
  where asImpSize (Constant (IntVal x) _) =
          Imp.ConstSize x
        asImpSize (Constant _ _) =
          error "Futhark.CodeGen.ImpGen.compileType: dimension size is a non-integer constant"
        asImpSize (Var v) =
          Imp.VarSize $ identName v

compileParam :: Param -> ImpM op Imp.Param
compileParam p = Imp.Param (identName p) <$> compileType (identType p)

compileParams :: [Param] -> ImpM op [Imp.Param]
compileParams = mapM compileParam

compileFunDec :: ExpCompiler op -> VNameSource -> FunDec
              -> (VNameSource, (Name, Imp.Function op))
compileFunDec ec src (FunDec fname rettype params body _) =
  let ((params', outs), src', body') = runImpM compile ec src
  in (src',
      (fname,
       Imp.Function outs params' body'))
  where compile
          | fname == defaultEntryPoint = do
            outs <- replicateM (length ses) $ newVName "out"
            compileBody outs body
            params' <- compileParams $ map bindeeIdent params
            outs' <- zipWith Imp.Param outs <$>
                     mapM (compileType . subExpType) ses
            return (params', outs')
          | otherwise = do
            outs <- replicateM (length ses') $ newVName "out"
            compileExtBody rettype outs body
            params' <- compileParams $ map bindeeIdent params
            outs' <- zipWith Imp.Param outs <$>
                     mapM (compileType . subExpType) ses'
            return (params', outs')
        ses = resultSubExps $ bodyResult body
        ses' = subExpShapeContext (resTypeValues rettype) ses ++ ses

compileBody :: [VName] -> Body -> ImpM op ()
compileBody targets body = compileExtBody rettype targets body
  where rettype = staticResType $ map subExpType $
                  resultSubExps $ bodyResult body

compileExtBody :: ResType -> [VName] -> Body -> ImpM op ()
compileExtBody rettype targets (Body _ bnds (Result _ ses _)) =
  compileBindings bnds $
  zipWithM_ compileSubExpTo targets $
  subExpShapeContext (resTypeValues rettype) ses ++ ses

compileBindings :: [Binding] -> ImpM op a -> ImpM op a
compileBindings []     m = m
compileBindings (b:bs) m = do
  let pat = bindingPattern b
  declaringVars (patternBindees pat) $
    compileExp (patternNames pat) (bindingExp b) $
    compileBindings bs m

compileExp :: [VName] -> Exp -> ImpM op a -> ImpM op a
compileExp targets e m = do
  ec <- asks envExpCompiler
  res <- ec targets e
  case res of
    CompileBindings bnds -> compileBindings bnds m
    CompileExp e'        -> do defCompileExp targets e'
                               m
    Done                 -> m

defCompileExp :: [VName] -> Exp -> ImpM op ()

defCompileExp targets (If cond tbranch fbranch rettype _) = do
  tcode <- collect $ compileExtBody rettype targets tbranch
  fcode <- collect $ compileExtBody rettype targets fbranch
  tell $ Imp.If (compileSubExp cond) tcode fcode

defCompileExp targets (Apply fname args _ _) =
  tell $ Imp.Call targets fname $ map (compileSubExp . fst) args

defCompileExp targets (PrimOp op) = defCompilePrimOp targets op

defCompileExp targets (LoopOp op) = defCompileLoopOp targets op

defCompilePrimOp :: [VName] -> PrimOp -> ImpM op ()

defCompilePrimOp [target] (SubExp se) =
  compileSubExpTo target se

defCompilePrimOp [target] (Not e _) =
  writeExp target $ Imp.UnOp Imp.Not $ compileSubExp e

defCompilePrimOp [target] (Negate e _) =
  writeExp target $ Imp.UnOp Imp.Negate $ compileSubExp e

defCompilePrimOp [target] (BinOp bop x y _ _) =
  writeExp target $ Imp.BinOp bop (compileSubExp x) (compileSubExp y)

defCompilePrimOp [_] (Assert e loc) =
  tell $ Imp.Assert (compileSubExp e) loc

defCompilePrimOp [target] (Index _ src idxs _)
  | length idxs == arrayRank (identType src) =
    tell $ Imp.Write target [] $
      Imp.Read (identName src) $ map compileSubExp idxs

defCompilePrimOp [target] (Index _ src idxs _) = do
  Imp.MemLocation destmem destoffset <- arrayLocation target
  (srcmem, srcoffset, size) <- indexArray (identName src) $ map compileSubExp idxs
  tell $ Imp.Copy destmem (dimSizeToExp destoffset) srcmem srcoffset size

{-
defCompilePrimOp [_] (Conjoin {}) =
  return ()

defCompilePrimOp [target] (ArrayLit es _ _) = do
  allocate target
  forM_ (zip [0..] es) $ \(i,e) ->
    tell $ Imp.Write target [Imp.Constant $ Imp.BasicVal $ IntVal i] $ compileSubExp e

defCompilePrimOp [target] (Update _ src idxs val _) = do
  writeExp target $ var $ identName src
  tell $ Imp.Write target (map compileSubExp idxs) $ compileSubExp val

defCompilePrimOp [target] (Iota n _) = do
  i <- newVName "i"
  allocate target
  tell $ Imp.For i (compileSubExp n) $ Imp.Write target [var i] $ var i

defCompilePrimOp [target] (Replicate n v _) = do
  i <- newVName "i"
  allocate target
  tell $ Imp.For i (compileSubExp n) $ Imp.Write target [var i] $ compileSubExp v

defCompilePrimOp [target] (Copy e _)
  | arrayRank (subExpType e) == 0 =
  writeExp target $ compileSubExp e
  | otherwise = do
    mem <- arraySubExpLocation e
    let elsize = basicSize $ elemType $ subExpType e
        arraysize = foldl (Imp.BinOp Times)
                    (Imp.Constant $ Imp.BasicVal $ IntVal elsize)
                    $ map compileSubExp $ arrayDims $ subExpType e
    tell $ Imp.Copy target mem arraysize

defCompilePrimOp [target] (Reshape _ shape src _) = do
  allocate target
  src' <- expAsName srct $ compileSubExp src
  let shape' = map compileSubExp shape
      srcshape' = map compileSubExp $ arrayDims srct
  n <- newVName "n"
  declareBasicVar n Int
  writeExp n $ foldl (Imp.BinOp Imp.Times) one shape'
  i <- newVName "i"
  let mult    = Imp.BinOp Imp.Times
      impProd = foldl mult one
  targetsizes <- mapM (expAsName (Basic Int) . impProd) $ drop 1 $ tails shape'
  srcsizes <- mapM (expAsName (Basic Int) . impProd) $ drop 1 $ tails srcshape'
  -- Some of these index calculations may duplicate computation a
  -- little bit.
  let idxs asizes ashape =
        [ Imp.BinOp Imp.Mod (Imp.BinOp Imp.Divide (var i) (var slicesize)) dimsize
          | (slicesize,dimsize) <- zip asizes ashape ]
      targetidxs = idxs targetsizes shape'
      srcidxs    = idxs srcsizes srcshape'
  tell $ Imp.For i (var n) $ Imp.Write target targetidxs $ Imp.Read src' srcidxs
  where one = Imp.Constant $ Imp.BasicVal $ IntVal 1
        srct = subExpType src

defCompilePrimOp [target] (Concat _ x y _ _) = do
  allocate target
  x' <- expAsName xt $ compileSubExp x
  y' <- expAsName yt $ compileSubExp y
  let xsize = compileSubExp $ arraySize 0 xt
      ysize = compileSubExp $ arraySize 0 yt
  i <- newVName "i"
  tell $ Imp.For i xsize $ Imp.Write target [var i] $
         Imp.Read x' [var i]
  j <- newVName "j"
  tell $ Imp.For j ysize $ Imp.Write target [Imp.BinOp Imp.Plus xsize $ var j] $
         Imp.Read y' [var j]
  where xt = subExpType x
        yt = subExpType y

defCompilePrimOp [target1, target2] (Split _ n x restsize _) = do
  allocate target1
  allocate target2
  x' <- expAsName xt $ compileSubExp x
  let n' = compileSubExp n
      restsize' = compileSubExp restsize
  i <- newVName "i"
  tell $ Imp.For i n' $ Imp.Write target1 [var i] $
         Imp.Read x' [var i]
  j <- newVName "i"
  tell $ Imp.For j restsize' $ Imp.Write target2 [var j] $
         Imp.Read x' [Imp.BinOp Imp.Plus n' $ var j]
  where xt = subExpType x

defCompilePrimOp _ (Split {}) = fail "ImpGen.compileExp: Incorrect number of targets to split"

defCompilePrimOp [target] (Rearrange _ perm e _) = do
  allocate target
  e' <- expAsName et $ compileSubExp e
  is <- replicateM (length perm) $ newVName "i"
  let sizes = map compileSubExp $ arrayDims et
  tell $ foldl (.) id (zipWith Imp.For is sizes) $
         Imp.Write target (permuteShape perm $ map var is) $
         Imp.Read e' $ map var is
  where et = subExpType e

defCompilePrimOp [target] (Rotate _ n e _) = do
  allocate target
  e' <- expAsName et $ compileSubExp e
  let size = compileSubExp $ arraySize 0 et
      n'   = Imp.Constant $ Imp.BasicVal $ IntVal n
  i <- newVName "i"
  tell $ Imp.For i size $
         Imp.Write target [Imp.BinOp Mod (Imp.BinOp Plus n' $ var i) size] $
         Imp.Read e' [var i]
  where et = subExpType e

defCompilePrimOp [] _ = return () -- No arms, no cake.

defCompilePrimOp (_:_:_) _ = fail "ImpGen.compilePrimOp: Incorrect number of targets"
-}
defCompileLoopOp :: [VName] -> LoopOp -> ImpM op ()

{-
defCompileLoopOp targets (DoLoop res merge i bound body _) = do
  declareVars mergepat
  zipWithM_ compileSubExpTo mergenames mergeinit
  body' <- collect $ compileBody mergenames body
  tell $ Imp.For (identName i) (compileSubExp bound) body'
  let writes = loopResultWrites targets res (map fst merge)
  forM_ writes $ \(from, to) ->
    tell $ Imp.Write to [] $ Imp.Read from []
  where (mergepat, mergeinit) = unzip merge
        mergenames = map bindeeName mergepat
-}

defCompileLoopOp [_] (Map {}) = soacError

defCompileLoopOp [_] (Filter {}) = soacError

defCompileLoopOp [_] (Reduce {}) = soacError

defCompileLoopOp [_] (Scan {}) = soacError

defCompileLoopOp [_] (Redomap {}) = soacError

defCompileLoopOp [] _ = return () -- No arms, no cake.

defCompileLoopOp (_:_:_) _ = fail "ImpGen.compileLoopOp: Incorrect number of targets"

soacError :: ImpM op a
soacError = fail "SOAC encountered in code generator; should have been removed by first-order transform."

writeExp :: VName -> Imp.Exp -> ImpM op ()
writeExp target = tell . Imp.Write target []

allocate :: VName -> ImpM op ()
allocate target = tell $ Imp.Allocate target

var :: VName -> Imp.Exp
var v = Imp.Read v []

declaringVars :: [PatBindee] -> ImpM op a -> ImpM op a
declaringVars bs m = foldr declaringVar m bs

declaringVar :: PatBindee -> ImpM op a -> ImpM op a
declaringVar bindee m =
  case bindeeType bindee of
    Basic bt -> do
      tell $ Imp.DeclareScalar name bt
      m
    Mem size -> do
      tell . Imp.DeclareMem name =<< subExpToDimSize size
      m
    Array bt shape _ -> do
      shape' <- mapM subExpToDimSize $ shapeDims shape
      let MemSummary mem ixfun = bindeeLore bindee
          location = Imp.MemLocation (identName mem) $ Imp.ConstSize 0
          entry = ArrayEntry {
              entryArrayLocation = location
            , entryArrayElemType = bt
            , entryArrayShape    = shape'
            }
          bind env =
            env { envVtable = HM.insert name entry $ envVtable env }
      unless (IxFun.isLinear ixfun) $
        fail "Can only handle linear (simple) allocation for now."
      tell $ Imp.DeclareArray name bt shape'
      local bind m
  where name = bindeeName bindee

declareVars :: [PatBindee] -> ImpM op ()
declareVars = mapM_ declareVar

declareVar :: PatBindee -> ImpM op ()
declareVar v =
  case bindeeType v of
    Mem size ->
      tell =<< (Imp.DeclareMem (bindeeName v) <$> subExpToDimSize size)

declareBasicVar :: VName -> BasicType -> ImpM op ()
declareBasicVar name bt = tell $ Imp.DeclareScalar name bt

subExpToDimSize :: SubExp -> ImpM op Imp.DimSize
subExpToDimSize (Var v) =
  return $ Imp.VarSize $ identName v
subExpToDimSize (Constant (IntVal i) _) =
  return $ Imp.ConstSize i
subExpToDimSize (Constant {}) =
  fail "Size subexp is not a non-integer constant."

dimSizeToExp :: Imp.DimSize -> Imp.Exp
dimSizeToExp (Imp.VarSize v)   = Imp.Read v []
dimSizeToExp (Imp.ConstSize x) = Imp.Constant $ IntVal x

compileSubExpTo :: VName -> SubExp -> ImpM op ()
compileSubExpTo target se =
  tell $ Imp.Write target [] $ compileSubExp se

compileSubExp :: SubExp -> Imp.Exp
compileSubExp (Constant v _) =
  Imp.Constant v
compileSubExp (Var v) =
  Imp.Read (identName v) []

{-
expAsName :: Type -> Imp.Exp -> ImpM op VName
expAsName _ (Imp.Read v []) =
  return v
expAsName t e = do
  size <- newIdent "size" t noLoc
  declareVar size
  writeExp (identName size) e
  return $ identName size
-}

lookupArray :: VName -> ImpM op ArrayEntry
lookupArray name = do
  res <- asks $ HM.lookup name . envVtable
  case res of
    Nothing    -> fail $ "Unknown array: " ++ textual name
    Just entry -> return entry

arrayLocation :: VName -> ImpM op Imp.MemLocation
arrayLocation name = entryArrayLocation <$> lookupArray name

indexArray :: VName -> [Imp.Exp] -> ImpM op (VName, Imp.Exp, Imp.Exp)
indexArray name indices = do
  entry <- lookupArray name
  let elemSizeExp = Imp.Constant $ IntVal $
                    basicSize $ entryArrayElemType entry
      arrshape = map dimSizeToExp $ entryArrayShape entry
      expProduct = foldl (Imp.BinOp Times) elemSizeExp
      expSum = foldl (Imp.BinOp Plus) $ Imp.Constant $ IntVal 0
      dimsizes = map expProduct $ drop 1 $ tails arrshape
      ixoffset = expSum $ zipWith (Imp.BinOp Times) indices dimsizes
      Imp.MemLocation arrmem arroffset = entryArrayLocation entry
  return (arrmem,
          Imp.BinOp Plus (dimSizeToExp arroffset) ixoffset,
          expProduct $ drop (length indices) arrshape)

basicSize :: BasicType -> Int
basicSize Int = 4
basicSize Bool = 1
basicSize Char = 1
basicSize Real = 8
basicSize Cert = 1

loopResultWrites :: [VName] -> [Ident] -> [FParam]
                 -> [(VName, VName)]
loopResultWrites = undefined
