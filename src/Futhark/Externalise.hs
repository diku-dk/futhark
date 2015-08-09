{-# OPTIONS_GHC -w #-}
-- | Convert an Futhark program in internal representation to a
-- corresponding program in external representation.  No effort is
-- made to make the external program look pretty, but correctness and
-- performance should be preserved.
--
-- Single-element tuples are converted to their element type, not a
-- tuple.
--
-- SOACs are currently converted to the tupleless SOACs of the
-- external language, although this should probably be changed.
module Futhark.Externalise
  ( externaliseProg
  )
  where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Reader
import Data.Loc
import Data.Monoid
import qualified Data.HashMap.Lazy as HM

import Prelude

import Futhark.Representation.External as E
import Futhark.Representation.Basic as I

-- | Convert a program in internal representation to the corresponding
-- program in the external representation.  The number and names of
-- functions is preserved.
externaliseProg :: I.Prog -> E.Prog
externaliseProg (I.Prog funs) =
  E.Prog $ map externaliseFunction funs

externaliseFunction :: I.FunDec -> E.FunDec
externaliseFunction (FunDec fname ret params body) =
  (fname,
   externaliseDeclTypes $ retTypeValues ret,
   map externaliseParam params,
   runExternaliseM $ bindFParams params $ externaliseBody body,
   noLoc)

type ExternaliseM = Reader TypeEnv

runExternaliseM :: ExternaliseM a -> a
runExternaliseM = flip runReader HM.empty

bindFParams :: [I.FParam] -> ExternaliseM a
            -> ExternaliseM a
bindFParams = bindIdents . map paramIdent

bindPattern :: I.Pattern -> ExternaliseM a
            -> ExternaliseM a
bindPattern = bindIdents . patternIdents

bindIdents :: [I.Ident] -> ExternaliseM a
            -> ExternaliseM a
bindIdents = local . HM.union . HM.fromList . map (I.identName &&& I.identType)

externaliseBody :: I.Body -> ExternaliseM E.Exp
externaliseBody (I.Body _ [] es) =
  externaliseSubExps es
externaliseBody (I.Body lore (I.Let pat _ e:bnds) res) =
  E.LetPat (externalisePat pat) <$>
  externaliseExp e <*>
  bindPattern pat (externaliseBody $ I.Body lore bnds res) <*>
  pure noLoc

externaliseExp :: I.Exp -> ExternaliseM E.Exp
externaliseExp (I.Apply fname args ts) =
  E.Apply fname <$>
  mapM externaliseArg args <*>
  pure (externaliseTypes $ retTypeValues ts) <*>
  pure noLoc
    where externaliseArg (e,d) = do
            e' <- externaliseSubExp e
            return (e', externaliseDiet d)
externaliseExp (I.If ce tb fb t) =
  E.If <$>
  externaliseSubExp ce <*>
  externaliseBody tb <*>
  externaliseBody fb <*>
  pure (externaliseTypes t) <*>
  pure noLoc
externaliseExp (PrimOp op) =
  externalisePrimOp op
externaliseExp (LoopOp op) =
  externaliseLoopOp op

externalisePrimOp :: I.PrimOp -> ExternaliseM E.Exp
externalisePrimOp (I.SubExp se) = externaliseSubExp se
externalisePrimOp (I.ArrayLit [] et) =
  pure $ E.Copy (E.Literal (E.arrayVal [] $ E.toDecl $ externaliseType et) noLoc) noLoc
externalisePrimOp (I.ArrayLit es et) =
  E.ArrayLit <$>
  mapM externaliseSubExp es <*>
  pure (externaliseType et) <*>
  pure noLoc
externalisePrimOp (I.BinOp bop x y t) =
  E.BinOp (externaliseBinOp bop) <$>
  externaliseSubExp x <*>
  externaliseSubExp y <*>
  pure (E.Basic t) <*>
  pure noLoc
externalisePrimOp (I.Not x) =
  E.UnOp E.Not <$> externaliseSubExp x <*> pure noLoc
externalisePrimOp (I.Negate x) =
  E.UnOp E.Negate <$> externaliseSubExp x <*> pure noLoc
externalisePrimOp (I.Index _ src idxs) =
  E.Index <$> externaliseVar src <*>
  mapM externaliseSubExp idxs <*>
  pure noLoc
  {-
externalisePrimOp (I.Update cs src idxs ve) =
  E.LetWith (externaliseCerts cs) (externaliseIdent src) (externaliseIdent src)
            (Just []) (map externaliseSubExp idxs)
            (externaliseSubExp ve) (E.Var $ externaliseIdent src)
            noLoc
-}
{- TODO: Externalise Split
externalisePrimOp (I.Split cs sizeexps ae) =
  E.Split (externaliseCerts cs)
          (externaliseSubExp ne)
          (E.Var $ externaliseIdent ae)
          noLoc
-}
externalisePrimOp (I.Concat _ x ys _) =
  E.Concat <$>
  (E.Var <$> externaliseVar x) <*>
  mapM (liftM E.Var . externaliseVar) ys <*>
  pure noLoc
externalisePrimOp (I.Copy e) =
  E.Copy <$> (E.Var <$> externaliseVar e) <*> pure noLoc
externalisePrimOp (I.Iota ne) =
  E.Iota <$> externaliseSubExp ne <*> pure noLoc
externalisePrimOp (I.Replicate ne ve) =
  E.Replicate <$>
  externaliseSubExp ne <*>
  externaliseSubExp ve <*>
  pure noLoc
externalisePrimOp (I.Reshape _ shape e) =
  E.Reshape <$>
  mapM (externaliseSubExp . newDim) shape <*>
  (E.Var <$> externaliseVar e) <*>
  pure noLoc
externalisePrimOp (I.Rearrange _ perm e) =
  E.Rearrange perm <$> (E.Var <$> externaliseVar e) <*> pure noLoc

externaliseLoopOp :: I.LoopOp -> ExternaliseM E.Exp

externaliseLoopOp (I.DoLoop respat merge form loopbody) =
  bindFParams mergepat $
  E.DoLoop (externaliseBinders (map paramIdent mergepat)) <$>
  externaliseSubExps mergeexp <*>
  form' <*>
  externaliseBody loopbody <*>
  (E.TupLit <$> mapM (liftM E.Var . externaliseVar) respat <*> pure noLoc) <*>
  pure noLoc
  where (mergepat, mergeexp) = unzip merge
        form' = case form of
          I.ForLoop i bound ->
            E.ForLoop (externaliseIdent $ I.Ident i $ I.Basic Int) <$>
            externaliseSubExp bound
          I.WhileLoop cond ->
            E.WhileLoop <$> E.Var <$> externaliseVar cond
externaliseLoopOp (I.Map _ _ fun es) =
  maybeUnzip <$>
  (E.Map <$>
   externaliseMapLambda fun <*>
   externaliseSOACArrayArgs es <*>
   pure noLoc)
externaliseLoopOp (I.Reduce _ _ fun inputs) =
  E.Reduce <$>
  externaliseFoldLambda fun (length inputs) <*>
  (maybeTupLit <$> mapM externaliseSubExp accinputs) <*>
  externaliseSOACArrayArgs arrinputs <*>
  pure noLoc
  where (accinputs, arrinputs) = unzip inputs
externaliseLoopOp (I.Scan _ _ fun inputs) =
  maybeUnzip <$>
  (E.Scan <$>
   externaliseFoldLambda fun (length inputs) <*>
   (maybeTupLit <$> mapM externaliseSubExp accinputs) <*>
   externaliseSOACArrayArgs arrinputs <*>
   pure noLoc)
  where (accinputs, arrinputs) = unzip inputs
externaliseLoopOp (I.Redomap _ _ outerfun innerfun vs as) =
  E.Redomap <$>
  externaliseFoldLambda outerfun (length vs) <*>
  externaliseFoldLambda innerfun (length vs) <*>
  (maybeTupLit <$> mapM externaliseSubExp vs) <*>
  externaliseSOACArrayArgs as <*>
  pure noLoc

externaliseMapLambda :: I.Lambda -> ExternaliseM E.Lambda
externaliseMapLambda (Lambda _ params body ret) =
  E.AnonymFun params' <$>
  (wrap <$> externaliseBody body) <*>
  pure (externaliseDeclTypes ret) <*>
  pure noLoc
  where (params', wrap) =
          let ps = map externaliseParam params
          in case makeTupleParam ps of
               Nothing     -> (ps, id)
               Just (p, f) -> ([p], f)

externaliseFoldLambda :: I.Lambda -> Int -> ExternaliseM E.Lambda
externaliseFoldLambda (Lambda _ params body ret) n =
  E.AnonymFun (accparams'++arrparams') <$>
  (wraparr <$> wrapacc <$> externaliseBody body) <*>
  pure (externaliseDeclTypes ret) <*>
  pure noLoc
  where (accparams, arrparams) = splitAt n $ map externaliseParam params
        (accparams', wrapacc) = case makeTupleParam accparams of
                                  Nothing     -> (accparams, id)
                                  Just (p, f) -> ([p], f)
        (arrparams', wraparr) = case makeTupleParam arrparams of
                                  Nothing     -> (arrparams, id)
                                  Just (p, f) -> ([p], f)

makeTupleParam :: [E.Parameter] -> Maybe (E.Parameter, E.Exp -> E.Exp)
makeTupleParam ps@(p:_:_) =
  -- Since an external Futhark map-lambda takes only a single
  -- argument, we have to create a new parameter, which is
  -- then unpacked inside the function.
  let pname = ID (nameFromString "ext_param",
                  baseTag $ E.identName p)
      ptype = E.Tuple $ map E.identType ps
      p'    = E.Ident pname ptype loc
      loc   = srclocOf p
  in Just (p',
           \inner -> E.LetPat
           (E.TupId (map (E.Id . E.fromParam) ps) noLoc)
           (E.Var $ E.fromParam p') inner noLoc)
makeTupleParam _ = Nothing

externaliseDiet :: I.Diet -> E.Diet
externaliseDiet I.Consume = E.Consume
externaliseDiet I.Observe = E.Observe

externalisePat :: I.Pattern -> E.TupIdent
externalisePat = externaliseBinders . patternIdents

externaliseBinders :: [I.Ident] -> E.TupIdent
externaliseBinders [v] = Id $ externaliseIdent v
externaliseBinders vs  = TupId (map (Id . externaliseIdent) vs) noLoc

externaliseBinOp :: I.BinOp -> E.BinOp
externaliseBinOp I.Plus = E.Plus
externaliseBinOp I.Minus = E.Minus
externaliseBinOp I.Pow = E.Pow
externaliseBinOp I.Times = E.Times
externaliseBinOp I.Divide = E.Divide
externaliseBinOp I.Mod = E.Mod
externaliseBinOp I.ShiftR = E.ShiftR
externaliseBinOp I.ShiftL = E.ShiftL
externaliseBinOp I.Band = E.Band
externaliseBinOp I.Xor = E.Xor
externaliseBinOp I.Bor = E.Bor
externaliseBinOp I.LogAnd = E.LogAnd
externaliseBinOp I.LogOr = E.LogOr
externaliseBinOp I.Less = E.Less
externaliseBinOp I.Leq = E.Leq
externaliseBinOp I.Equal = E.Equal

externaliseDeclTypes :: I.ArrayShape shape =>
                        [I.TypeBase shape] -> E.DeclType
externaliseDeclTypes ts =
  case map externaliseDeclType ts of
    [t]  -> t
    ts'  -> E.Tuple ts'

externaliseTypes :: I.ArrayShape shape =>
                    [I.TypeBase shape] -> E.Type
externaliseTypes ts =
  case map externaliseType ts of
    [t]  -> t
    ts'  -> E.Tuple ts'

externaliseDeclType :: I.ArrayShape shape =>
                       I.TypeBase shape -> E.DeclType
externaliseDeclType (I.Basic t) = E.Basic t
externaliseDeclType (I.Array et shape u) =
  E.Array $ E.BasicArray et (E.ShapeDecl $ replicate (I.shapeRank shape) E.AnyDim) u NoInfo

externaliseType :: I.ArrayShape shape =>
                   I.TypeBase shape -> E.Type
externaliseType (I.Basic t) = E.Basic t
externaliseType (I.Array et shape u) =
  E.Array $ E.BasicArray et (E.Rank (I.shapeRank shape)) u mempty

externaliseSOACArrayArgs :: [I.VName] -> ExternaliseM E.Exp
externaliseSOACArrayArgs [e] = externaliseSubExp $ I.Var e
externaliseSOACArrayArgs es  = do
  es' <- mapM (externaliseSubExp . I.Var) es
  return $ Zip [ (e, E.typeOf e) | e <- es' ] noLoc

externaliseSubExps :: [I.SubExp] -> ExternaliseM E.Exp
externaliseSubExps [e] = externaliseSubExp e
externaliseSubExps es  = E.TupLit <$> mapM externaliseSubExp es <*> pure noLoc

externaliseSubExp :: I.SubExp -> ExternaliseM E.Exp
externaliseSubExp (I.Var v) =
  E.Var <$> externaliseVar v
externaliseSubExp (I.Constant v) =
  return $ E.Literal (E.BasicVal v) noLoc

externaliseParam :: I.ParamT attr -> E.Parameter
externaliseParam (I.Param (I.Ident name t) _) =
  E.Ident name (externaliseDeclType t) noLoc

externaliseIdent :: I.Ident -> E.Ident
externaliseIdent (I.Ident name t) =
  E.Ident name (externaliseType t) noLoc

externaliseVar :: I.VName -> ExternaliseM E.Ident
externaliseVar name = do
  t <- lookupType name
  return $ E.Ident name (externaliseType t) noLoc

maybeUnzip :: E.Exp -> E.Exp
maybeUnzip e
  | (E.Tuple ts@(_:_:_))
      <- E.rowType $ E.typeOf e = Unzip e ts noLoc
  | otherwise                   = e

maybeTupLit :: [E.Exp] -> E.Exp
maybeTupLit [e] = e
maybeTupLit es  = E.TupLit es noLoc
