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

import Data.Loc
import Data.Monoid

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
   externaliseDeclTypes $ map I.toDecl $ retTypeValues ret,
   map (externaliseParam . fparamIdent) params,
   externaliseBody body,
   noLoc)

externaliseBody :: I.Body -> E.Exp
externaliseBody (I.Body _ [] (I.Result es)) =
  externaliseSubExps es
externaliseBody (I.Body lore (I.Let pat _ e:bnds) res) =
  E.LetPat (externalisePat pat) (externaliseExp e)
           (externaliseBody $ I.Body lore bnds res) noLoc

externaliseExp :: I.Exp -> E.Exp
externaliseExp (I.Apply fname args ts) =
  E.Apply fname (map externaliseArg args)
  (externaliseTypes $ retTypeValues ts) noLoc
    where externaliseArg (e,d) =
            (externaliseSubExp e,
             externaliseDiet d)
externaliseExp (I.If ce tb fb t) =
  E.If (externaliseSubExp ce)
       (externaliseBody tb)
       (externaliseBody fb)
       (externaliseTypes t)
       noLoc
externaliseExp (PrimOp op) =
  externalisePrimOp op
externaliseExp (LoopOp op) =
  externaliseLoopOp op

externalisePrimOp :: I.PrimOp -> E.Exp
externalisePrimOp (I.SubExp se) = externaliseSubExp se
externalisePrimOp (I.ArrayLit [] et) =
  E.Copy (E.Literal (E.arrayVal [] $ E.toDecl $ externaliseType et) noLoc) noLoc
externalisePrimOp (I.ArrayLit es et) =
  E.ArrayLit (map externaliseSubExp es) (externaliseType et) noLoc
externalisePrimOp (I.BinOp bop x y t) =
  E.BinOp bop (externaliseSubExp x) (externaliseSubExp y)
              (E.fromDecl $ externaliseDeclType $ I.Basic t) noLoc
externalisePrimOp (I.Not x) =
  E.Not (externaliseSubExp x) noLoc
externalisePrimOp (I.Negate x) =
  E.Negate (externaliseSubExp x) noLoc
externalisePrimOp (I.Index _ src idxs) =
  E.Index (externaliseIdent src)
          (map externaliseSubExp idxs)
          noLoc
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
  E.Concat (E.Var $ externaliseIdent x)
           (map (E.Var . externaliseIdent) ys)
           noLoc
externalisePrimOp (I.Copy e) =
  E.Copy (externaliseSubExp e) noLoc
externalisePrimOp (I.Iota ne) =
  E.Iota (externaliseSubExp ne) noLoc
externalisePrimOp (I.Replicate ne ve) =
  E.Replicate (externaliseSubExp ne)
              (externaliseSubExp ve)
              noLoc
externalisePrimOp (I.Reshape _ shape e) =
  E.Reshape (map externaliseSubExp shape)
            (E.Var $ externaliseIdent e)
            noLoc
externalisePrimOp (I.Rearrange _ perm e) =
  E.Rearrange perm
              (E.Var $ externaliseIdent e)
              noLoc

externaliseLoopOp :: I.LoopOp -> E.Exp

externaliseLoopOp (I.DoLoop respat merge form loopbody) =
  E.DoLoop (externaliseBinders (map fparamIdent mergepat))
           (externaliseSubExps mergeexp)
           form'
           (externaliseBody loopbody)
           (E.TupLit (map (E.Var . externaliseIdent) respat) noLoc) noLoc
  where (mergepat, mergeexp) = unzip merge
        form' = case form of
          I.ForLoop i bound ->
            E.ForLoop (externaliseIdent i) (externaliseSubExp bound)
          I.WhileLoop cond ->
            E.WhileLoop $ E.Var $ externaliseIdent cond
externaliseLoopOp (I.Map _ fun es) =
  maybeUnzip $ E.Map (externaliseMapLambda fun)
               (externaliseSOACArrayArgs es)
               noLoc
externaliseLoopOp (I.Reduce _ fun inputs) =
  E.Reduce (externaliseFoldLambda fun $ length inputs)
           (maybeTupLit (map externaliseSubExp accinputs))
           (externaliseSOACArrayArgs arrinputs)
           noLoc
  where (accinputs, arrinputs) = unzip inputs
externaliseLoopOp (I.Scan _ fun inputs) =
  maybeUnzip $ E.Scan (externaliseFoldLambda fun $ length inputs)
               (maybeTupLit (map externaliseSubExp accinputs))
               (externaliseSOACArrayArgs arrinputs)
               noLoc
  where (accinputs, arrinputs) = unzip inputs
externaliseLoopOp (I.Filter _ fun es) =
  maybeUnzip $ E.Filter (externaliseMapLambda fun)
                        (externaliseSOACArrayArgs es)
                        noLoc
externaliseLoopOp (I.Redomap _ outerfun innerfun vs as) =
  E.Redomap (externaliseFoldLambda outerfun $ length vs)
            (externaliseFoldLambda innerfun $ length vs)
            (maybeTupLit (map externaliseSubExp vs))
            (externaliseSOACArrayArgs as)
            noLoc

externaliseMapLambda :: I.Lambda -> E.Lambda
externaliseMapLambda (Lambda params body ret) =
  E.AnonymFun params'
              (wrap $ externaliseBody body)
              (externaliseDeclTypes $ map I.toDecl ret)
              noLoc
  where (params', wrap) =
          let ps = map externaliseParam params
          in case makeTupleParam ps of
               Nothing     -> (ps, id)
               Just (p, f) -> ([p], f)

externaliseFoldLambda :: I.Lambda -> Int -> E.Lambda
externaliseFoldLambda (Lambda params body ret) n =
  E.AnonymFun (accparams'++arrparams')
              (wraparr $ wrapacc $ externaliseBody body)
              (externaliseDeclTypes $ map I.toDecl ret)
              noLoc
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

externaliseDeclTypes :: [I.DeclType] -> E.DeclType
externaliseDeclTypes ts =
  case map externaliseDeclType ts of
    [t]  -> t
    ts'  -> E.Tuple ts'

externaliseTypes :: ArrayShape shape => [I.TypeBase shape] -> E.Type
externaliseTypes ts =
  case map externaliseType ts of
    [t]  -> t
    ts'  -> E.Tuple ts'

externaliseDeclType :: I.DeclType -> E.DeclType
externaliseDeclType (I.Basic t) = E.Basic t
externaliseDeclType (I.Array et shape u) =
  E.Array $ E.BasicArray et (replicate (shapeRank shape) Nothing) u NoInfo

externaliseType :: ArrayShape shape =>
                   I.TypeBase shape -> E.Type
externaliseType (I.Basic t) = E.Basic t
externaliseType (I.Array et shape u) =
  E.Array $ E.BasicArray et (replicate (shapeRank shape) Nothing) u mempty

externaliseSOACArrayArgs :: [I.Ident] -> E.Exp
externaliseSOACArrayArgs [e] = externaliseSubExp $ I.Var e
externaliseSOACArrayArgs es  =
  Zip [ (e, E.typeOf e) | e <- map (externaliseSubExp . I.Var) es ] noLoc

externaliseSubExps :: [I.SubExp] -> E.Exp
externaliseSubExps [e] = externaliseSubExp e
externaliseSubExps es  = E.TupLit (map externaliseSubExp es) noLoc

externaliseSubExp :: I.SubExp -> E.Exp
externaliseSubExp (I.Var v) =
  E.Var $ externaliseIdent v
externaliseSubExp (I.Constant v) =
  E.Literal (E.BasicVal v) noLoc

externaliseParam :: I.Param -> E.Parameter
externaliseParam (I.Ident name t) =
  E.Ident name (externaliseDeclType $ I.toDecl t) noLoc

externaliseIdent :: I.Ident -> E.Ident
externaliseIdent (I.Ident name t) =
  E.Ident name (externaliseType t) noLoc

maybeUnzip :: E.Exp -> E.Exp
maybeUnzip e
  | (E.Tuple ts@(_:_:_))
      <- E.rowType $ E.typeOf e = Unzip e ts noLoc
  | otherwise                   = e

maybeTupLit :: [E.Exp] -> E.Exp
maybeTupLit [e] = e
maybeTupLit es  = E.TupLit es noLoc
