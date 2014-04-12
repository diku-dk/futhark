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

import qualified Data.Array as A
import Data.Loc

import Futhark.ExternalRep as E
import Futhark.InternalRep as I

-- | Convert a program in internal representation to the corresponding
-- program in the external representation.  The number and names of
-- functions is preserved.
externaliseProg :: I.Prog -> E.Prog
externaliseProg (I.Prog funs) =
  E.Prog $ map externaliseFunction funs

externaliseFunction :: I.FunDec -> E.FunDec
externaliseFunction (fname, ret, params, body, loc) =
  (fname,
   externaliseDeclTypes ret,
   map externaliseParam params,
   externaliseBody body,
   loc)

externaliseBody :: I.Body -> E.Exp
externaliseBody (I.Body [] (I.Result _ es loc)) =
  externaliseSubExps es loc
externaliseBody (I.Body (I.Let pat e:bnds) res) =
  E.LetPat (externalisePat pat loc) (externaliseExp e)
           (externaliseBody $ I.Body bnds res) loc
  where loc = srclocOf e

externaliseExp :: I.Exp -> E.Exp
externaliseExp (I.SubExps es loc) = externaliseSubExps es loc
externaliseExp (I.ArrayLit [] et loc) =
  E.Copy (E.Literal (E.arrayVal [] $ E.toDecl $ externaliseType et) loc) loc
externaliseExp (I.ArrayLit es et loc) =
  E.ArrayLit (map externaliseSubExp es) (externaliseType et) loc
externaliseExp (I.Apply fname args ts loc) =
  E.Apply fname (map externaliseArg args) (externaliseTypes ts) loc
    where externaliseArg (e,d) =
            (externaliseSubExp e,
             externaliseDiet d)
externaliseExp (I.If ce tb fb t loc) =
  E.If (externaliseSubExp ce)
       (externaliseBody tb)
       (externaliseBody fb)
       (externaliseTypes t)
       loc
externaliseExp (I.BinOp bop x y t loc) =
  E.BinOp bop (externaliseSubExp x) (externaliseSubExp y)
              (externaliseType t) loc
externaliseExp (I.Not x loc) =
  E.Not (externaliseSubExp x) loc
externaliseExp (I.Negate x loc) =
  E.Negate (externaliseSubExp x) loc
externaliseExp (I.Assert x loc) =
  E.Assert (externaliseSubExp x) loc
externaliseExp (I.Conjoin es loc) =
  E.Conjoin (map externaliseSubExp es) loc
externaliseExp (I.Index cs src idxs loc) =
  E.Index (externaliseCerts cs)
          (externaliseIdent src)
          (Just [])
          (map externaliseSubExp idxs)
          loc
externaliseExp (I.Update cs src idxs ve loc) =
  E.LetWith (externaliseCerts cs) (externaliseIdent src) (externaliseIdent src)
            (Just []) (map externaliseSubExp idxs)
            (externaliseSubExp ve) (E.Var $ externaliseIdent src)
            loc
externaliseExp (I.Split cs ne ae _ loc) =
  E.Split (externaliseCerts cs)
          (externaliseSubExp ne)
          (externaliseSubExp ae)
          loc
externaliseExp (I.Concat cs x y _ loc) =
  E.Concat (externaliseCerts cs)
           (externaliseSubExp x)
           (externaliseSubExp y)
           loc
externaliseExp (I.Copy e loc) =
  E.Copy (externaliseSubExp e) loc
externaliseExp (I.Iota ne loc) =
  E.Iota (externaliseSubExp ne) loc
externaliseExp (I.Replicate ne ve loc) =
  E.Replicate (externaliseSubExp ne)
              (externaliseSubExp ve)
              loc
externaliseExp (I.Reshape cs shape e loc) =
  E.Reshape (externaliseCerts cs)
            (map externaliseSubExp shape)
            (externaliseSubExp e)
            loc
externaliseExp (I.Rearrange cs perm e loc) =
  E.Rearrange (externaliseCerts cs)
              perm
              (externaliseSubExp e)
              loc
externaliseExp (I.Rotate cs n e loc) =
  E.Rotate (externaliseCerts cs)
           n
           (externaliseSubExp e)
           loc
externaliseExp (I.DoLoop respat merge i bound loopbody loc) =
  E.DoLoop (externalisePat mergepat loc) (externaliseSubExps mergeexp loc)
           (externaliseIdent i) (externaliseSubExp bound)
           (externaliseBody loopbody)
           (E.TupLit (map (E.Var . externaliseIdent) respat) loc) loc
  where (mergepat, mergeexp) = unzip merge
externaliseExp (I.Map _ fun es loc) =
  maybeUnzip $ E.Map (externaliseMapLambda fun)
               (externaliseSOACArrayArgs es loc)
               loc
externaliseExp (I.Reduce _ fun inputs loc) =
  E.Reduce (externaliseFoldLambda fun $ length inputs)
           (maybeTupLit (map externaliseSubExp accinputs) loc)
           (externaliseSOACArrayArgs arrinputs loc)
           loc
  where (accinputs, arrinputs) = unzip inputs
externaliseExp (I.Scan _ fun inputs loc) =
  maybeUnzip $ E.Scan (externaliseFoldLambda fun $ length inputs)
               (maybeTupLit (map externaliseSubExp accinputs) loc)
               (externaliseSOACArrayArgs arrinputs loc)
               loc
  where (accinputs, arrinputs) = unzip inputs
externaliseExp (I.Filter _ fun es _ loc) =
  maybeUnzip $ E.Filter (externaliseMapLambda fun)
                        (externaliseSOACArrayArgs es loc)
                        loc
externaliseExp (I.Redomap _ outerfun innerfun vs as loc) =
  E.Redomap (externaliseFoldLambda outerfun $ length vs)
            (externaliseFoldLambda innerfun $ length vs)
            (maybeTupLit (map externaliseSubExp vs) loc)
            (externaliseSOACArrayArgs as loc)
            loc

externaliseMapLambda :: I.Lambda -> E.Lambda
externaliseMapLambda (Lambda params body ret loc) =
  E.AnonymFun params'
              (wrap $ externaliseBody body)
              (externaliseDeclTypes $ map I.toDecl ret)
              loc
  where (params', wrap) =
          let ps = map externaliseParam params
          in case makeTupleParam ps of
               Nothing     -> (ps, id)
               Just (p, f) -> ([p], f)

externaliseFoldLambda :: I.Lambda -> Int -> E.Lambda
externaliseFoldLambda (Lambda params body ret loc) n =
  E.AnonymFun (accparams'++arrparams')
              (wraparr $ wrapacc $ externaliseBody body)
              (externaliseDeclTypes $ map I.toDecl ret)
              loc
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
      ptype = E.Elem $ E.Tuple $ map E.identType ps
      p'    = E.Ident pname ptype loc
      loc   = srclocOf p
  in Just (p',
           \inner -> E.LetPat
           (E.TupId (map (E.Id . E.fromParam) ps) loc)
           (E.Var $ E.fromParam p') inner loc)
makeTupleParam _ = Nothing

externaliseDiet :: I.Diet -> E.Diet
externaliseDiet I.Consume = E.Consume
externaliseDiet I.Observe = E.Observe

externaliseCerts :: I.Certificates -> E.Certificates
externaliseCerts = map externaliseIdent

externalisePat :: [I.Ident] -> SrcLoc -> E.TupIdent
externalisePat [v] _  = Id $ externaliseIdent v
externalisePat vs loc = TupId (map (Id . externaliseIdent) vs) loc

externaliseDeclTypes :: [I.DeclType] -> E.DeclType
externaliseDeclTypes ts =
  case map externaliseDeclType ts of
    [t]  -> t
    ts'  -> E.Elem $ E.Tuple ts'

externaliseTypes :: [I.Type] -> E.Type
externaliseTypes ts =
  case map externaliseType ts of
    [t]  -> t
    ts'  -> E.Elem $ E.Tuple ts'

externaliseDeclType :: I.DeclType -> E.DeclType
externaliseDeclType (I.Basic t) = E.Elem $ E.Basic t
externaliseDeclType (I.Array et shape u ()) =
  E.Array (E.Basic et) (replicate (shapeRank shape) Nothing) u NoInfo

externaliseType :: I.Type -> E.Type
externaliseType (I.Basic t) = E.Elem $ E.Basic t
externaliseType (I.Array et shape u als) =
  E.Array (E.Basic et) (replicate (shapeRank shape) Nothing)
          u als

externaliseSOACArrayArgs :: [I.SubExp] -> SrcLoc -> E.Exp
externaliseSOACArrayArgs [e] _   = externaliseSubExp e
externaliseSOACArrayArgs es  loc =
  Zip [ (e, E.typeOf e) | e <- map externaliseSubExp es ] loc

externaliseSubExps :: [I.SubExp] -> SrcLoc -> E.Exp
externaliseSubExps [e] _  = externaliseSubExp e
externaliseSubExps es loc = E.TupLit (map externaliseSubExp es) loc

externaliseSubExp :: I.SubExp -> E.Exp
externaliseSubExp (I.Var v) =
  E.Var $ externaliseIdent v
externaliseSubExp (I.Constant v loc) =
  E.Literal (externaliseValue v) loc

externaliseParam :: I.Param -> E.Parameter
externaliseParam (I.Ident name t loc) =
  E.Ident name (externaliseDeclType $ I.toDecl t) loc

externaliseIdent :: I.Ident -> E.Ident
externaliseIdent (I.Ident name t loc) =
  E.Ident name (externaliseType t) loc

externaliseValue :: I.Value -> E.Value
externaliseValue (I.BasicVal bv) = E.BasicVal bv
externaliseValue (I.ArrayVal a dt) =
  E.arrayVal (map externaliseValue $ A.elems a) $
  externaliseDeclType dt

maybeUnzip :: E.Exp -> E.Exp
maybeUnzip e
  | E.Elem (E.Tuple ts@(_:_:_))
      <- E.rowType $ E.typeOf e = Unzip e ts loc
  | otherwise                   = e
  where loc = srclocOf e

maybeTupLit :: [E.Exp] -> SrcLoc -> E.Exp
maybeTupLit [e] _   = e
maybeTupLit es  loc = E.TupLit es loc
