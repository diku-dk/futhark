{-# LANGUAGE QuasiQuotes #-}

-- | The parts of the translation in "Futhark.CodeGen.ImpGen.GPU.ToOpenCL"
-- that are specific to Metal.  The kernels are generated as OpenCL C,
-- and then adjusted to be valid Metal Shading Language: the macros in
-- @prelude.metal@ take care of most differences, but a few require
-- rewriting the code itself.
module Futhark.CodeGen.ImpGen.GPU.ToOpenCL.Metal
  ( metalKernel,
    metalDeviceFun,
  )
where

import Data.Maybe
import Data.Text qualified as T
import Futhark.CodeGen.Backends.GenericC.Pretty
import Futhark.CodeGen.Backends.SimpleRep ()
import Futhark.CodeGen.ImpCode.OpenCL (Name)
import Futhark.Error (compilerLimitationS)
import Language.C.Quote.OpenCL qualified as C
import Language.C.Syntax qualified as C

-- | Metal kernels take their arguments as a struct in buffer 0 (see
-- @FUTHARK_KERNEL_PARAMS@ in @prelude.metal@), which we unpack into
-- local variables of the same names as the OpenCL kernel parameters.
-- The MSL attributes on the kernel parameters cannot be expressed
-- with language-c-quote, so we print the function header ourselves.
-- The body is the OpenCL kernel body, from which we remove the jumps
-- with 'eliminateGotos'.
metalKernel :: Name -> T.Text -> [C.Param] -> [C.BlockItem] -> T.Text
metalKernel name attribute params body =
  definitionsText [[C.cedecl|struct $id:args_struct { $sdecls:fields };|]]
    <> attribute
    <> ("void " <> idText (C.toIdent name mempty))
    <> ("(FUTHARK_KERNEL_PARAMS(" <> args_struct <> "))\n")
    <> stmText [C.cstm|{ $items:unpack $items:(eliminateGotos body) }|]
  where
    args_struct = idText (C.toIdent name mempty) <> "_args"
    (fields, unpack) = unzip $ map field params
    field (C.Param (Just v) spec decl loc) =
      let t = C.Type spec decl loc
       in ( [C.csdecl|$ty:t $id:v;|],
            [C.citem|$ty:t $id:v = futhark_args->$id:v;|]
          )
    field p = error $ "metalKernel: unnamed parameter " <> show p

-- | Metal requires an address space on every pointer, and does not
-- support @goto@ (and hence labels).  The only unqualified pointers
-- in device functions are the parameters for return values, which
-- point to private memory.
metalDeviceFun :: (C.Definition, C.Func) -> (C.Definition, C.Func)
metalDeviceFun
  ( C.DecDef (C.InitGroup dspec attrs [C.Init v (C.Proto d ps ploc) asm ini iattrs iloc] gloc) dloc,
    C.Func fspec fv fd fps body floc
    )
    | any (itemAny isGoto) body =
        compilerLimitationS "Metal does not support failing GPU functions with jumps."
    | otherwise =
        ( C.DecDef (C.InitGroup dspec attrs [C.Init v (C.Proto d (onParams ps) ploc) asm ini iattrs iloc] gloc) dloc,
          C.Func fspec fv fd (onParams fps) (map unlabel body) floc
        )
    where
      onParams (C.Params ps' variadic loc) = C.Params (map onParam ps') variadic loc
      onParam (C.Param p (C.DeclSpec st quals ts sloc) decl@C.Ptr {} loc)
        | not $ any isAddressSpace quals =
            C.Param p (C.DeclSpec st (C.TCLprivate mempty : quals) ts sloc) decl loc
      onParam p = p
      isAddressSpace C.TCLprivate {} = True
      isAddressSpace C.TCLlocal {} = True
      isAddressSpace C.TCLglobal {} = True
      isAddressSpace C.TCLconstant {} = True
      isAddressSpace _ = False
      unlabel (C.BlockStm (C.Label _ _ s _)) = C.BlockStm s
      unlabel item = item
metalDeviceFun _ = error "metalDeviceFun: unexpected function definition."

-- | A failing thread jumps to the next 'ErrorSync', where all threads
-- of the block check whether one of them failed, or to the end of the
-- kernel if there is none (see 'errorLabel').  Metal has no @goto@, so we
-- eliminate the jumps from the kernel body: a failing thread instead
-- sets the private flag @futhark_failed@, which guards all following
-- statements up to the next synchronisation point (the statement with
-- a label), and which breaks out of any loop that does not contain a
-- synchronisation point.  The synchronisation points themselves are
-- always executed, so the failing thread still takes part in the
-- barrier, after which all threads of the block return.
eliminateGotos :: [C.BlockItem] -> [C.BlockItem]
eliminateGotos body =
  [[C.citem|bool futhark_failed = false;|] | any (itemAny isGoto) body]
    ++ onItems False False body
  where
    -- The first argument is whether these items are the body of a
    -- loop that contains no synchronisation point, such that a
    -- failure must break out of it.  The second is whether a failure
    -- may have happened since the last synchronisation point.
    onItems _ _ [] = []
    onItems loop pending items@(x : xs)
      | C.BlockStm (C.Label _ _ s _) <- x =
          C.BlockStm (onStm pending s) : onItems loop False xs
      | itemAny isLabel x =
          onItem pending x : onItems loop (pending || itemAny isGoto x) xs
      | pending =
          let (guarded, rest) = break (itemAny isLabel) items
              -- Declarations must remain visible after the guard,
              -- unless nothing executes after it.
              (decls, guarded')
                | returns rest = ([], guarded)
                | otherwise = mconcat $ map hoistDecl guarded
           in decls
                ++ [C.citem|if (!futhark_failed) { $items:(onItems False False guarded') }|]
                : onItems loop True rest
      | itemAny isGoto x =
          onItem False x
            : [[C.citem|if (futhark_failed) { break; }|] | loop]
            ++ onItems loop (not loop) xs
      | otherwise = x : onItems loop False xs

    returns [] = True
    returns (C.BlockStm (C.Label _ _ C.Return {} _) : _) = True
    returns _ = False

    onItem pending (C.BlockStm s) = C.BlockStm $ onStm pending s
    onItem _ item = item

    onStm _ (C.Goto _ _) =
      [C.cstm|futhark_failed = true;|]
    onStm pending (C.Label _ _ s _) =
      onStm pending s
    onStm pending (C.Block items loc) =
      C.Block (onItems False pending items) loc
    onStm pending (C.If e t f loc) =
      C.If e (onStm pending t) (onStm pending <$> f) loc
    onStm pending (C.Comment c s loc) =
      C.Comment c (onStm pending s) loc
    onStm pending (C.For ini cond inc s loc) =
      C.For ini cond inc (onLoopBody pending s) loc
    onStm pending (C.While e s loc) =
      C.While e (onLoopBody pending s) loc
    onStm pending (C.DoWhile s e loc) =
      C.DoWhile (onLoopBody pending s) e loc
    onStm _ s = s

    -- A synchronisation point in a loop body may be reached from a
    -- failure in a previous iteration.
    onLoopBody pending (C.Block items loc)
      | any (itemAny isLabel) items =
          C.Block (onItems False (pending || any (itemAny isGoto) items) items) loc
      | otherwise =
          C.Block (onItems True False items) loc
    onLoopBody pending s =
      onLoopBody pending $ C.Block [C.BlockStm s] mempty

    -- Split a declaration into an uninitialised declaration and
    -- assignments.
    hoistDecl (C.BlockDecl (C.InitGroup (C.DeclSpec st quals ts sloc) attrs inits loc)) =
      ( [ C.BlockDecl $
            C.InitGroup
              (C.DeclSpec st (filter (not . isConst) quals) ts sloc)
              attrs
              [C.Init v d asm Nothing iattrs iloc | C.Init v d asm _ iattrs iloc <- inits]
              loc
        ],
        mapMaybe assign inits
      )
    hoistDecl item = ([], [item])
    assign (C.Init v _ _ (Just (C.ExpInitializer e _)) _ _) =
      Just [C.citem|$id:v = $exp:e;|]
    assign (C.Init _ _ _ Nothing _ _) =
      Nothing
    assign _ =
      compilerLimitationS "eliminateGotos: cannot hoist initialiser."
    isConst C.Tconst {} = True
    isConst _ = False

isGoto, isLabel :: C.Stm -> Bool
isGoto C.Goto {} = True
isGoto _ = False
isLabel C.Label {} = True
isLabel _ = False

-- | Does any statement in this item satisfy the predicate?
itemAny :: (C.Stm -> Bool) -> C.BlockItem -> Bool
itemAny p (C.BlockStm s) = stmAny p s
itemAny _ _ = False

-- | Does this statement, or any statement nested in it, satisfy the
-- predicate?
stmAny :: (C.Stm -> Bool) -> C.Stm -> Bool
stmAny p s = p s || any (stmAny p) (children s)
  where
    children (C.Label _ _ s' _) = [s']
    children (C.Case _ s' _) = [s']
    children (C.Default s' _) = [s']
    children (C.Block items _) = [s' | C.BlockStm s' <- items]
    children (C.If _ t f _) = t : maybeToList f
    children (C.Switch _ s' _) = [s']
    children (C.While _ s' _) = [s']
    children (C.DoWhile s' _ _) = [s']
    children (C.For _ _ _ s' _) = [s']
    children (C.Comment _ s' _) = [s']
    children _ = []
