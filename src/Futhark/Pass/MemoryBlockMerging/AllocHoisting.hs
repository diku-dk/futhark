-- | Move certain allocations up in the program to enable more array
-- coalescings.
--
-- This should be run *before* memory block merging.  Otherwise it might get
-- confused?
module Futhark.Pass.MemoryBlockMerging.AllocHoisting
  ( hoistAllocsFunDef
  ) where

import System.IO.Unsafe (unsafePerformIO) -- Just for debugging!

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

import Futhark.MonadFreshNames
import Futhark.Tools
import Futhark.Representation.AST
import qualified Futhark.Representation.ExplicitMemory as ExpMem
import Futhark.Pass.ExplicitAllocations()

type Line = Int
data Origin = FromFParam
            | FromLine Line
  deriving (Eq, Ord, Show)

data Allocation = Allocation { _allocMem :: VName
                             , _allocSize :: VName
                             , _allocSpace :: Space
                             }
  deriving (Show)

data PrimBinding = PrimBinding { _pbSimpleExp :: Maybe (Exp ExpMem.ExplicitMemory)
                               , _pbType :: Maybe PrimType
                               , pbOrigin :: Origin
                               }
  deriving (Show)

-- For a variable name, this map contains both its type (if it is a scalar
-- type), its assigned expression (if it is an expression that can be expressed
-- as a PrimExp), and its location in the body.  This is used to expand certain
-- allocation size expressions, so that they can be hoisted upwards and used in
-- more cases.
type BindingMap = M.Map VName PrimBinding

hoistAllocsFunDef :: MonadFreshNames m
                  => FunDef ExpMem.ExplicitMemory
                  -> m (FunDef ExpMem.ExplicitMemory)
hoistAllocsFunDef fundef = do
  let scope_new = scopeOf fundef
      scope_old = M.empty
      bindingmap_cur = M.empty
      m = hoistAllocsBody scope_new scope_old bindingmap_cur $ funDefBody fundef
  body' <- modifyNameSource (runState m)
  return fundef { funDefBody = body' }

type HoistAllocsM = State VNameSource

-- This is a bit messy.
hoistAllocsBody :: Scope ExpMem.ExplicitMemory
                -> Scope ExpMem.ExplicitMemory
                -> BindingMap
                -> Body ExpMem.ExplicitMemory
                -> HoistAllocsM (Body ExpMem.ExplicitMemory)
hoistAllocsBody scope_new scope_old bindingmap_old body = do
  let allocs = findAllocations body
      allocs' = filter (isSimpleMemUse body) allocs

  let scope_cur = scope_old <> scope_new
      bindingmap_new = M.fromList $ mapMaybe scopeBindingMap $ M.toList scope_new
      bindingmap_cur = bindingmap_old <> bindingmap_new

  Body () bnds res <-
    foldM (hoistAlloc scope_cur bindingmap_cur) body allocs'

  let scope_bnds = scopeOf bnds
      scope = scope_cur <> scope_bnds

      bindingmap_bnds = bodyBindingMap bnds
      bindingmap = bindingmap_cur <> bindingmap_bnds

      debug = unsafePerformIO $ do
        putStrLn $ replicate 10 '*' ++ " Allocations found in body "  ++ replicate 10 '*'
        forM_ allocs' print
        putStrLn $ replicate 70 '-'

  bnds' <- mapM (hoistRecursivelyStm scope bindingmap) bnds
  debug `seq` return $ Body () bnds' res

  where scopeBindingMap :: (VName, NameInfo ExpMem.ExplicitMemory)
                        -> Maybe (VName, PrimBinding)
        scopeBindingMap (x, ni) = case typeOf ni of
          Prim pt -> Just (x, PrimBinding Nothing (Just pt) FromFParam)
          _ -> Just (x, PrimBinding Nothing Nothing FromFParam)

findAllocations :: Body ExpMem.ExplicitMemory
                -> [Allocation]
findAllocations body =
  let stms = bodyStms body
  in mapMaybe findAllocation stms

  where findAllocation :: Stm ExpMem.ExplicitMemory
                       -> Maybe Allocation
        findAllocation (Let (Pattern _ [PatElem xmem _ _])
                            () (Op (ExpMem.Alloc (Var xsize) space))) =
          Just $ Allocation xmem xsize space
        findAllocation _ = Nothing

-- Is the allocated memory used by either Copy or Concat in the function body?
-- Those are the only kinds of memory we care about, since those are the cases
-- handled in ArrayCoalescing.
isSimpleMemUse :: Body ExpMem.ExplicitMemory -> Allocation -> Bool
isSimpleMemUse body alloc =
  let stms = bodyStms body
  in any (isSimple alloc) stms

  where isSimple :: Allocation -> Stm ExpMem.ExplicitMemory -> Bool
        isSimple (Allocation xmem_alloc _ _)
                 (Let
                  (Pattern _
                   [PatElem _dst _bindage
                    (ExpMem.ArrayMem _pt _shape _u xmem_pat _xixfun)])
                  ()
                  (BasicOp bop)) =
          xmem_pat == xmem_alloc && case bop of
                                      Copy {} -> True
                                      Concat {} -> True
                                      _ -> False
        isSimple _ _ = False

hoistAlloc :: Scope ExpMem.ExplicitMemory
           -> BindingMap
           -> Body ExpMem.ExplicitMemory
           -> Allocation
           -> HoistAllocsM (Body ExpMem.ExplicitMemory)
hoistAlloc scope_cur bindingmap_cur body alloc = do
  let scope_new = scopeOf $ bodyStms body
      scope = M.union scope_cur scope_new

      bindingmap_new = bodyBindingMap $ bodyStms body
      bindingmap = M.union bindingmap_cur bindingmap_new

      size = runReader (sizePrimExpFromAlloc alloc) bindingmap
      origin = runReader (furthestOrigin size) bindingmap
      body1 = removeMatchingAllocation alloc body
  body2 <- insertAllocationAfter origin alloc size body1 scope

  let debug = unsafePerformIO $ do
        putStrLn $ replicate 10 '*' ++ " Allocation hoisting "  ++ replicate 10 '*'
        putStrLn $ "Allocation: " ++ show alloc
        putStrLn $ "Size expression: " ++ pretty size
        putStrLn $ "Origin: " ++ show origin
        putStrLn $ replicate 70 '-'

  debug `seq` return body2

hoistRecursivelyStm :: Scope ExpMem.ExplicitMemory
                    -> BindingMap
                    -> Stm ExpMem.ExplicitMemory
                    -> HoistAllocsM (Stm ExpMem.ExplicitMemory)
hoistRecursivelyStm scope bindingmap (Let pat () e) =
  Let pat () <$> mapExpM transform e

  where transform = identityMapper { mapOnBody = mapper }
        mapper scope_new = hoistAllocsBody scope_new scope bindingmap

lookupPrimBinding :: VName
                   -> Reader BindingMap PrimBinding
lookupPrimBinding vname = do
  m <- M.lookup vname <$> ask
  case m of
    Just b -> return b
    Nothing -> error (pretty vname ++ " was not found in BindingMap.  This should not happen! No non-PrimType should ever be in this use.")

bodyBindingMap :: [Stm ExpMem.ExplicitMemory] -> BindingMap
bodyBindingMap stms =
  M.fromList $ concatMap createBindingStmt $ zip [0..] stms

  where createBindingStmt :: (Line, Stm ExpMem.ExplicitMemory)
                          -> [(VName, PrimBinding)]
        createBindingStmt (line,
                           Let (Pattern _ patelems) () e) =
          let checkScalar t = case t of
                ExpMem.Scalar pt -> Just pt
                _ -> Nothing
          in case patelems of
            -- In this case we know that 'x' is directly related to 'e'
            [PatElem x _ t] ->
              let e' = case e of
                    -- Only accept expressions that can be expressed as PrimExps.
                    -- This is used in 'corePrimExp' to make sure we don't try to
                    -- expand an expression that cannot be expressed in terms of
                    -- scalar variables.
                    BasicOp BinOp{} -> Just e
                    BasicOp CmpOp{} -> Just e
                    BasicOp UnOp{} -> Just e
                    BasicOp ConvOp{} -> Just e
                    BasicOp SubExp{} -> Just e
                    _ -> Nothing
              in [(x, PrimBinding e' (checkScalar t) (FromLine line))]

            -- If there is more than one pattern element, we cannot say that
            -- "'x' is 'e'", but we can still store its type (admittedly, we
            -- could just use the scope for that).
            _ -> map (\(PatElem x _ t) ->
                        (x, PrimBinding Nothing (checkScalar t) (FromLine line)))
                 patelems

sizePrimExpFromAlloc :: Allocation
                     -> Reader BindingMap (ExpMem.PrimExp VName)
sizePrimExpFromAlloc (Allocation _ xsize _) =
  let e = BasicOp $ SubExp $ Var xsize
  in primExpFromExp corePrimExp e

corePrimExp :: Exp ExpMem.ExplicitMemory
            -> Reader BindingMap (ExpMem.PrimExp VName)
corePrimExp (BasicOp (SubExp (Var vname))) = do
  b <- lookupPrimBinding vname
  case b of
    PrimBinding (Just e) _ _ -> primExpFromExp corePrimExp e
    PrimBinding Nothing (Just pt) _ -> return $ LeafExp vname pt
    _ -> error "should not happen; not enabled by BindingMap"
corePrimExp _ = error "should not happen; not enabled by BindingMap"
-- FIXME: Improve the above guarantees by writing the code nicer.

furthestOrigin :: ExpMem.PrimExp VName -> Reader BindingMap Origin
furthestOrigin pe = do
  let vars = execWriter $ traverse (tell . (: [])) pe
  origins <- map pbOrigin <$> mapM lookupPrimBinding vars
  return $ fst $ maximum $ zip origins vars

removeMatchingAllocation :: Allocation
                         -> Body ExpMem.ExplicitMemory
                         -> Body ExpMem.ExplicitMemory
removeMatchingAllocation (Allocation xmem_alloc _ _) body =
  body { bodyStms = filter (not . isTheAlloc) $ bodyStms body }

  where isTheAlloc :: Stm ExpMem.ExplicitMemory -> Bool
        isTheAlloc (Let (Pattern _ [PatElem xmem_pat _ _]) () _) =
          xmem_pat == xmem_alloc
        isTheAlloc _ = False

insertAllocationAfter :: Origin
                      -> Allocation
                      -> ExpMem.PrimExp VName
                      -> Body ExpMem.ExplicitMemory
                      -> Scope ExpMem.ExplicitMemory
                      -> HoistAllocsM (Body ExpMem.ExplicitMemory)
insertAllocationAfter origin alloc size body scope = do
  stms <- snd <$> runBinderT (newSizeExp alloc size) scope
  return $ insertStmsAfter origin stms body

insertStmsAfter :: Origin -> [Stm ExpMem.ExplicitMemory]
                -> Body ExpMem.ExplicitMemory
                -> Body ExpMem.ExplicitMemory
insertStmsAfter origin stms body =
  let line = case origin of
        FromFParam -> 0
        FromLine n -> n + 1
      bodystms = bodyStms body
      bodystms' = take line bodystms
                  ++ stms
                  ++ drop line bodystms
  in body { bodyStms = bodystms' }

newSizeExp :: Allocation
           -> ExpMem.PrimExp VName
           -> BinderT ExpMem.ExplicitMemory HoistAllocsM ()
newSizeExp (Allocation xmem _ space) pe = do
  xsize_e <- primExpToExp (return . BasicOp . SubExp . Var) pe
  xsize <- newVName "alloc_size"
  letBindNames'_ [xsize] xsize_e

  let xmem_e = Op (ExpMem.Alloc (Var xsize) space)
  letBindNames'_ [xmem] xmem_e
