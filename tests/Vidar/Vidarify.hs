module Vidarify (vidarify) where

import qualified Vidar as V

import qualified Futhark.Representation.Basic as I
import qualified Futhark.Representation.AST.Syntax as S

import Language.Futhark.Core

vidarify :: I.Prog -> V.Element
vidarify (I.Prog decs) = V.SubBlock $ V.StrictBlock $ map vidarifyDec decs

vidarifyDec :: I.FunDec -> V.Element
vidarifyDec (I.FunDec n _retType params body) =
  V.Block (V.ExactName $ nameToString n)
    $ V.StrictBlock [V.SubBlock $ V.StrictBlock $ vidarifyParams params  -- parameters
                    ,V.SubBlock $ V.StrictBlock $ vidarifyFuncBody body] -- body

vidarifyParams :: [S.FParam I.Basic] -> [V.Element]
vidarifyParams = map vidarifyParam

vidarifyParam :: S.FParam I.Basic -> V.Element
vidarifyParam (S.FParam ident _lore) = V.Name $ vidarifyIdent ident

vidarifyIdents :: [I.Param] -> [V.Element]
vidarifyIdents = map $ V.Name . vidarifyIdent

showName :: VName -> String
showName n = baseString n ++ show (baseTag n)

vidarifyFuncBody :: I.Body -> [V.Element]
vidarifyFuncBody (I.Body _lore bs res) =
    map vidarifyBinding bs ++ [vidarifyRes res]

vidarifyBinding :: I.Binding -> V.Element
vidarifyBinding (I.Let p _lore exp) = V.Binding (vidarifyPattern p) (vidarifyExp exp)

vidarifyExp :: I.Exp -> V.Element
vidarifyExp (I.PrimOp p)       = vidarifyPrimOp p
vidarifyExp (I.LoopOp l)       = vidarifyLoopOp l
vidarifyExp (S.If cond a b _t) =
    V.Block (V.ExactName "If") $ V.StrictBlock $ [
        vidarifySubExp cond,
        V.SubBlock $ V.StrictBlock $ vidarifyFuncBody a,
        V.SubBlock $ V.StrictBlock $ vidarifyFuncBody b
    ]
vidarifyExp e = V.Anything

vidarifyLambda :: I.Lambda -> V.Element
vidarifyLambda (S.Lambda params body _rtype) =
    V.Block (V.ExactName "FN") $ V.StrictBlock [
        V.SubBlock $ V.StrictBlock $ vidarifyIdents params,
        V.SubBlock $ V.StrictBlock $ vidarifyFuncBody body
    ]

vidarifyLoopOp :: I.LoopOp -> V.Element
vidarifyLoopOp (S.Map certs f idents) =
    V.Block (V.ExactName "Map") $ V.StrictBlock [
        vidarifyLambda f,
        V.SubBlock $ V.StrictBlock $ vidarifyIdents idents
    ]
vidarifyLoopOp _ = V.Anything

vidarifyPrimOp :: I.PrimOp -> V.Element
vidarifyPrimOp (S.SubExp subexp) = vidarifySubExp subexp
vidarifyPrimOp (S.ArrayLit subexps _type) =
    V.Block (V.ExactName "Array") $ V.StrictBlock $
        map vidarifySubExp subexps
vidarifyPrimOp (S.BinOp binop a b _tp) =
    V.Block (V.ExactName $ show binop) $ V.StrictBlock $ [
        vidarifySubExp a,
        vidarifySubExp b
    ]
vidarifyPrimOp (S.Not subexp) =
    V.Block (V.ExactName "Not") $ V.StrictBlock [
        vidarifySubExp subexp
    ]
vidarifyPrimOp (S.Negate subexp) =
    V.Block (V.ExactName "Negate") $ V.StrictBlock [
        vidarifySubExp subexp
    ]
vidarifyPrimOp (S.Assert subexp _loc) =
    V.Block (V.ExactName "Assert") $ V.StrictBlock [
        vidarifySubExp subexp
    ]
vidarifyPrimOp (S.Index certs ident subexps) =
    V.Block (V.ExactName "Index") $ V.StrictBlock $ [
        V.SubBlock $ V.StrictBlock $ map (V.Name . vidarifyIdent) certs,
        V.Name $ vidarifyIdent ident,
        V.SubBlock $ V.StrictBlock $ map vidarifySubExp subexps
    ]
vidarifyPrimOp (S.Iota subexp) =
    V.Block (V.ExactName "Iota") $ V.StrictBlock $ [
        vidarifySubExp subexp
    ]
vidarifyPrimOp (S.Reshape _certs subexps ident) =
    V.Block (V.ExactName "Reshape") $ V.StrictBlock [
        V.SubBlock $ V.StrictBlock $ map vidarifySubExp subexps,
        V.Name $ vidarifyIdent ident
    ]
vidarifyPrimOp _ = V.Anything

vidarifyPattern :: I.Pattern -> V.Name
vidarifyPattern (S.Pattern [b]) = vidarifyPatElem b
vidarifyPattern p = V.AnyName

vidarifyPatElem :: I.PatElem -> V.Name
vidarifyPatElem (S.PatElem ident _bindage _lore) = vidarifyIdent ident

vidarifyIdent :: I.Ident -> V.Name
vidarifyIdent (S.Ident n _idType) = V.ExactName $ showName n

vidarifyRes :: I.Result -> V.Element
vidarifyRes (S.Result subexps) = V.SubBlock $ V.StrictBlock $ map vidarifySubExp subexps

vidarifySubExp :: I.SubExp -> V.Element
vidarifySubExp (S.Constant bv)             = vidarifyBasicVal bv
vidarifySubExp (S.Var ident) = V.Name $ vidarifyIdent ident

vidarifyBasicVal :: BasicValue -> V.Element
vidarifyBasicVal (IntVal x) = V.Name $ V.ExactName $ show x
vidarifyBasicVal _ = V.Anything

