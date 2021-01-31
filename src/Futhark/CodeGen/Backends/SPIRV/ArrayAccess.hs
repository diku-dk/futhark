module Futhark.CodeGen.Backends.SPIRV.ArrayAccess
  (ArrayAccesses, getArrayAccesses)
where
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Futhark.CodeGen.ImpCode
import Futhark.CodeGen.ImpCode.Kernels hiding (Code)

type ArrayAccesses = [(S.Set VName, S.Set PrimType)]

aaAdd :: VName -> ArrayAccesses -> ArrayAccesses
aaAdd v aa =
  case findIndex (\(s, _) -> v `S.member` s) aa of
    Just _ -> aa
    Nothing -> vn : aa
  where
    vn = (S.singleton v, S.empty)

aaAddType :: (VName, PrimType) -> ArrayAccesses -> ArrayAccesses
aaAddType (v, t) aa =
  case findIndex (\(s, _) -> v `S.member` s) aa of
    Just idx -> let (pfx, e:sfx) = splitAt idx aa in pfx ++ (e <> vt) : sfx
    Nothing -> vt : aa
  where
    vt = (S.singleton v, S.singleton t)

aaJoin :: (VName, VName) -> ArrayAccesses -> ArrayAccesses
aaJoin (v1, v2) aa =
  let idx1 = findIndex (\(s, _) -> v1 `S.member` s) aa
      idx2 = findIndex (\(s, _) -> v2 `S.member` s) aa
  in case (idx1, idx2) of
      (Nothing, Just i) ->
        let (pfx, e:sfx) = splitAt i aa
        in pfx ++ (e <> (S.singleton v1, S.empty)) : sfx
      (Just i, Nothing) ->
        let (pfx, e:sfx) = splitAt i aa
        in pfx ++ (e <> (S.singleton v2, S.empty)) : sfx
      (Nothing, Nothing) ->
        (S.fromList [v1, v2], S.empty) : aa
      (Just i1, Just i2) | i1 /= i2 ->
        let a1@(vs1, ts1) = aa !! i1
            a2@(vs2, ts2) = aa !! i2
            aa' = delete a2 (delete a1 aa)
        in (vs1 <> vs2, ts1 <> ts2) : aa'
      (Just _, Just _) -> aa

findAccessesExp :: Exp -> State ArrayAccesses ()
findAccessesExp (LeafExp (Index v (Count i) t _ _) _) = do
  findAccessesExp $ untyped i
  modify (aaAddType (v, t))
findAccessesExp _ = return ()

findAccesses :: Code KernelOp -> State ArrayAccesses ()
findAccesses (lc :>>: rc) = mapM_ findAccesses [lc, rc]
findAccesses (For _ cond body) = do
  findAccessesExp cond
  findAccesses body
findAccesses (While cond body) = do
  findAccessesExp $ untyped cond
  findAccesses body
findAccesses (DeclareMem v _) = modify (aaAdd v)
findAccesses (DeclareArray v _ t _) =
  modify (aaAddType (v, t))
findAccesses (Allocate {}) = error "Allocate unsupported"
findAccesses (Copy {}) = error "Copy unsupported"
findAccesses (Write v (Count i) t _ _ e) = do
  findAccessesExp $ untyped i
  findAccessesExp e
  modify (aaAddType (v, t))
findAccesses (SetScalar _ e) =  findAccessesExp e
findAccesses (SetMem v1 v2 _) = modify (aaJoin (v1, v2))
findAccesses (Call _ _ args) =
  mapM_ findAccessesExp $ mapMaybe onArg args
  where
    onArg (ExpArg e) = Just e
    onArg _ = Nothing
findAccesses (If cond b1 b2) = do
  findAccessesExp (untyped cond)
  mapM_ findAccesses [b1, b2]
findAccesses (Assert e _ _) = findAccessesExp e
findAccesses (Comment _ c) = findAccesses c
findAccesses (Op (LocalAlloc v _)) = modify (aaAdd v)
findAccesses (Op (Atomic _ aop)) = do
  let (t, arr, es) = onAtomicOp aop
  modify (aaAddType (arr, t))
  mapM_ findAccessesExp es
  where
    onAtomicOp (AtomicAdd t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicFAdd t _ arr (Count i) x) =
      (FloatType t, arr, [untyped i, x])
    onAtomicOp (AtomicSMax t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicSMin t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicUMax t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicUMin t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicAnd t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicOr t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicXor t _ arr (Count i) x) =
      (IntType t, arr, [untyped i, x])
    onAtomicOp (AtomicCmpXchg t _ arr (Count i) x y) =
      (t, arr, [untyped i, x, y])
    onAtomicOp (AtomicXchg t _ arr (Count i) x) =
      (t, arr, [untyped i, x])
findAccesses _ = return ()

getArrayAccesses :: Code KernelOp -> ArrayAccesses
getArrayAccesses c = snd $ runState (findAccesses c) []
