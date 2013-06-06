{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | L0 prettyprinter.
module Language.L0.Pretty
  ( ppType
  , ppValue
  , ppExp
  , ppTupId
  , prettyPrint
  )
  where

import Data.Array

import Text.PrettyPrint.Mainland

import Language.L0.Syntax
import Language.L0.Attributes

tildes :: String -> String
tildes = map tilde
  where tilde '-' = '~'
        tilde c   = c

instance Pretty Name where
  ppr (Name t) = fromText t

instance Pretty Value where
  ppr (IntVal x) = text $ tildes $ show x
  ppr (CharVal c) = text $ show c
  ppr (LogVal b) = text $ show b
  ppr (RealVal x) = text $ tildes $ show x
  ppr (TupVal vs) = tuple $ map ppr vs
  ppr v@(ArrayVal a t)
    | Just s <- arrayString v = text $ show s
    | [] <- elems a = text "empty" <> parens (ppr t)
    | otherwise     = braces $ commasep $ map ppr $ elems a

instance Pretty ElemType where
  ppr Int = text "int"
  ppr Char = text "char"
  ppr Bool = text "bool"
  ppr Real = text "real"
  ppr (Tuple ets) = tuple $ map ppr ets

instance Pretty Type where
  ppr (Elem et) = ppr et
  ppr (Array et ds u) = u' <> foldl f (ppr et) ds
    where f s Nothing = brackets s
          f s (Just e) = brackets $ s <> comma <> ppr e
          u' | Unique <- u = star
             | otherwise = empty

instance (Eq vn, Pretty vn) => Pretty (IdentBase ty vn) where
  ppr = ppr . identName

instance (Eq vn, Pretty vn) => Pretty (ExpBase ty vn) where
  ppr = pprPrec 0
  pprPrec _ (Var v) = ppr v
  pprPrec _ (Literal v _) = ppr v
  pprPrec _ (TupLit es _) = tuple $ map ppr es
  pprPrec _ (ArrayLit es _ _) = braces $ commasep $ map ppr es
  pprPrec p (BinOp bop x y _ _) = ppBinOp p bop x y
  pprPrec p (And x y _) = ppBinOp p LogAnd x y
  pprPrec p (Or x y _) = ppBinOp p LogOr x y
  pprPrec _ (Not e _) = text "not" <+> pprPrec 9 e
  pprPrec _ (Negate e _ _) = text "~" <> pprPrec 9 e
  pprPrec _ (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  pprPrec _ (Apply fname args _ _) = text (nameToString fname) <>
                                     tuple (map (align . ppr) args)
  pprPrec _ (LetPat pat e body _) = text "let" <+> align (ppr pat) <+>
                                    equals <+> align (ppr e) <+> text "in" </>
                                    ppr body
  pprPrec _ (LetWith dest src idxs ve body _)
    | dest == src =
      text "let" <+> ppr dest <+> list (map ppr idxs) <+>
      equals <+> ppr ve <+>
      text "in" </> ppr body
    | otherwise =
      text "let" <+> ppr dest <+> equals <+> ppr src <+>
      text "with" <+> list (map ppr idxs) <+> text "<-" <+> ppr ve <+>
      text "in" </> ppr body
  pprPrec _ (Index v idxs _ _) = ppr v <> list (map ppr idxs)
  pprPrec _ (Iota e _) = text "iota" <> parens (ppr e)
  pprPrec _ (Size e _) = text "size" <> parens (ppr e)
  pprPrec _ (Replicate ne ve _) =
    text "replicate" <> tuple [ppr ne, ppr ve]
  pprPrec _ (Reshape shape e _) =
    text "replicate" <> tuple [tuple (map ppr shape), ppr e]
  pprPrec _ (Transpose e _) = text "transpose" <> parens (ppr e)
  pprPrec _ (Map lam a _ _) = ppSOAC "map" [lam] [a]
  pprPrec _ (Mapall lam a _) = ppSOAC "mapall" [lam] [a]
  pprPrec _ (Reduce lam e a _ _) = ppSOAC "reduce" [lam] [e, a]
  pprPrec _ (Redomap redlam maplam e a _ _) =
    ppSOAC "redomap" [redlam, maplam] [e, a]
  pprPrec _ (Scan lam e a _ _) = ppSOAC "scan" [lam] [e, a]
  pprPrec _ (Filter lam a _ _) = ppSOAC "filter" [lam] [a]
  pprPrec _ (Zip es _) = text "zip" <> tuple (map (ppr . fst) es)
  pprPrec _ (Unzip e _ _) = text "unzip" <> parens (ppr e)
  pprPrec _ (Split e a _ _) = text "split" <> tuple [ppr e, ppr a]
  pprPrec _ (Concat x y _) = text "concat" <> tuple [ppr x, ppr y]
  pprPrec _ (Copy e _) = text "copy" <> parens (ppr e)
  pprPrec _ (DoLoop pat initexp i bound loopbody letbody _) =
    text "loop" <+> parens (ppr pat <+> equals <+> ppr initexp) <+>
    equals <+> text "for" <+> ppr i <+> langle <+> ppr bound <+> text "do" </>
    indent 2 (ppr loopbody) <+> text "in" </>
    ppr letbody
  pprPrec _ (Map2 lam as _ _) = ppSOAC "map2" [lam] as
  pprPrec _ (Mapall2 lam as _) = ppSOAC "mapall2" [lam] as
  pprPrec _ (Reduce2 lam e as _ _) = ppSOAC "reduce" [lam] $ e:as
  pprPrec _ (Redomap2 redlam maplam e as _ _) =
    ppSOAC "redomap2" [redlam, maplam] $ e:as
  pprPrec _ (Scan2 lam e as _ _) = ppSOAC "scan2" [lam] $ e:as
  pprPrec _ (Filter2 lam as _) = ppSOAC "filter2" [lam] as

instance (Eq vn, Pretty vn) => Pretty (TupIdentBase ty vn) where
  ppr (Id ident) = ppr ident
  ppr (TupId pats _) = tuple $ map ppr pats

instance (Eq vn, Pretty vn) => Pretty (LambdaBase ty vn) where
  ppr (CurryFun fname [] _ _) = text $ nameToString fname
  ppr (CurryFun fname curryargs _ _) =
    text (nameToString fname) <+> tuple (map ppr curryargs)
  ppr (AnonymFun params body rettype _) =
    text "fn" <+> ppr rettype <+>
    tuple (map ppParam params) <+>
    text "=>" <+> ppr body

instance (Eq vn, Pretty vn) => Pretty (ProgBase ty vn) where
  ppr = stack . punctuate line . map ppFun . progFunctions
    where ppFun (name, rettype, args, body, _) =
            text "fun" <+> ppr rettype <+>
            text (nameToString name) <//>
            tuple (map ppParam args) <+>
            equals </> indent 2 (ppr body)

ppParam :: (Eq vn, Pretty ty, Pretty vn) => IdentBase ty vn -> Doc
ppParam param = ppr (identType param) <+> ppr param

ppBinOp :: (Eq vn, Pretty vn) => Int -> BinOp -> ExpBase ty vn -> ExpBase ty vn -> Doc
ppBinOp p bop x y = parensIf (p > precedence bop) $
                    pprPrec (precedence bop) x <+>
                    text (opStr bop) <+>
                    pprPrec (precedence bop) y
  where precedence LogAnd = 0
        precedence LogOr = 0
        precedence Band = 1
        precedence Bor = 1
        precedence Xor = 1
        precedence Equal = 2
        precedence Less = 2
        precedence Leq = 2
        precedence ShiftL = 3
        precedence ShiftR = 3
        precedence Plus = 4
        precedence Minus = 4
        precedence Times = 5
        precedence Divide = 5
        precedence Mod = 5
        precedence Pow = 6

ppSOAC :: (Eq vn, Pretty vn) => String -> [LambdaBase ty vn] -> [ExpBase ty vn] -> Doc
ppSOAC name [] es = text name <> tuple (map ppr es)
ppSOAC name (fun:funs) es =
  text name <> parens (foldl ppfun (ppr fun) funs <> comma <//> commasep (map ppr es))
  where ppfun s fun' = s <//> comma <> ppr fun'

render80 :: Pretty a => a -> String
render80 = pretty 80 . ppr

ppValue :: Value -> String
ppValue = render80

ppType :: Type -> String
ppType = render80

ppExp :: (Eq vn, Pretty vn) => ExpBase ty vn -> String
ppExp = render80

ppTupId :: (Eq vn, Pretty vn) => TupIdentBase ty vn -> String
ppTupId = render80

prettyPrint :: (Eq vn, Pretty vn) => ProgBase ty vn -> String
prettyPrint = render80


{-
spaces :: Int -> String
spaces n = replicate (n*2) ' '

tildes :: String -> String
tildes = map tilde
  where tilde '-' = '~'
        tilde c   = c

-- | Pretty printing a value.
ppValue :: Value -> String
ppValue (IntVal n)  = tildes (show n) ++ " "
ppValue (RealVal n) = tildes (show n) ++ " "
ppValue (LogVal b)  = show b ++ " "
ppValue (CharVal c) = show c ++ " "
ppValue v@(ArrayVal arr _)
  | [] <- elems arr = " empty (" ++ ppType (stripArray 1 $ typeOf v) ++ " ) "
  | Just s <- arrayString v = show s
  | otherwise = " { " ++ intercalate ", " (map ppValue $ elems arr) ++ " } "
ppValue (TupVal vs)   =
  " ( " ++ intercalate ", " (map ppValue vs) ++ " ) "

-- | Pretty printing an expression
ppExp :: TypeBox ty => Int -> Exp ty -> String
ppExp _ (Literal val _)     = ppValue val
ppExp d (ArrayLit es _ _) =
  " { " ++ intercalate ", " (map (ppExp d) es) ++ " } "
ppExp d (TupLit es _) =
  " ( " ++ intercalate ", " (map (ppExp d) es) ++ " ) "
ppExp _ (Var ident) = nameToString (identName ident) ++ " "

ppExp d (BinOp op e1 e2 _ _) = " ( " ++ ppExp d e1 ++ ppBinOp op ++ ppExp d e2 ++ " ) "
ppExp d (And   e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " && " ++ ppExp d e2 ++ " ) "
ppExp d (Or    e1 e2 _  ) = " ( " ++ ppExp d e1 ++ " || " ++ ppExp d e2 ++ " ) "

ppExp d (Not   e _      ) = " ( " ++ "not " ++ ppExp d e ++ " ) "
ppExp d (Negate e _ _   ) = " ( " ++ "~ " ++ ppExp d e ++   " ) "

ppExp d (If    e1 e2 e3 _ _)  =
  "\n" ++
  spaces (d+1) ++ "if( "  ++ ppExp (d+2) e1 ++ " )\n" ++
  spaces (d+1) ++ "then " ++ ppExp (d+2) e2 ++ "\n" ++
  spaces (d+1) ++ "else " ++ ppExp (d+2) e3 ++ "\n" ++
  spaces d

ppExp _ (Apply f [] _ _)    = nameToString f ++ "() "
ppExp d (Apply f args _ _)  =
  nameToString f ++ "( " ++ intercalate ", " (map (ppExp d) args) ++ " ) "

ppExp d (LetPat tupid e1 body _) =
        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp (d+2) e1 ++
        "in  " ++ ppExp d body
ppExp d (LetWith (Ident dest _ _) (Ident src _ _) es el e2 _)
  | dest == src =
    "\n" ++ spaces d ++ "let " ++ nameToString dest ++ "[ " ++
    intercalate ", " (map (ppExp d) es) ++
    "] = " ++ ppExp (d+2) el ++ "in  " ++ ppExp d e2
  | otherwise =
    "\n" ++ spaces d ++ "let " ++ nameToString dest ++ " = " ++ nameToString src ++
    " with [ " ++ intercalate ", " (map (ppExp d) es) ++
    "] <- " ++ ppExp d el ++ "in  " ++ ppExp d e2

ppExp d (Index (Ident name _ _) es _ _) =
  nameToString name ++ "[ " ++ intercalate ", " (map (ppExp d) es) ++ " ] "

-- | Array Constructs
ppExp d (Iota e _)         = "iota ( " ++ ppExp d e ++ " ) "
ppExp d (Size e _)         = "size ( " ++ ppExp d e ++ " ) "
ppExp d (Replicate e el _) = "replicate ( " ++ ppExp d e ++ ", " ++ ppExp d el ++ " ) "

ppExp d (Transpose e _) = " transpose ( " ++ ppExp d e ++ " ) "

ppExp d (Reshape es arr _) =
  " reshape ( ( " ++ intercalate ", " (map (ppExp d) es) ++ " ), "  ++
  ppExp d arr ++ " ) "

ppExp d (Map fun e _ _) = " map ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) e ++ " ) "

ppExp d (Zip es _) =
  " zip ( " ++ intercalate ", " (map (ppExp d . fst) es) ++ " ) "

ppExp d (Unzip e _ _) = " unzip ( " ++ ppExp d e ++ " ) "

ppExp d (Reduce fun el lst _ _) =
  " reduce ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) lst ++ " ) "
ppExp d (Scan  fun el lst _ _) =
  " scan ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) lst ++ " ) "
ppExp d (Filter fun a _ _) =
  " filter ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Mapall fun a _)
          = " mapall ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) a ++ " ) "
ppExp d (Redomap id1 id2 el a _ _)
          = " redomap ( " ++ ppLambda (d+1) id1 ++ ", " ++ ppLambda (d+1) id2 ++ 
            ", " ++ ppExp (d+1) el ++ ", " ++ ppExp (d+1) a ++ " ) "

ppExp d (Split  idx arr _ _) = " split ( " ++ ppExp d idx ++ ", " ++ ppExp d arr ++ " ) "
ppExp d (Concat a1 a2 _) = " concat ( " ++ ppExp d a1 ++ ", " ++ ppExp d a2 ++ " ) "
ppExp d (Copy e _) = " copy ( " ++ ppExp d e ++ " ) "

ppExp d (DoLoop mvpat mvexp i n loopbody letbody _) =
  "\n" ++ spaces d ++ "loop (" ++ ppTupId mvpat ++ " = " ++ ppExp d mvexp ++
    ") = " ++ "for " ++ nameToString (identName i) ++ " < " ++ ppExp d n ++ " do\n" ++
    spaces (d+1) ++ ppExp (d+1) loopbody ++ "\n" ++ spaces d ++
    "in " ++ ppExp d letbody
--- Cosmin added ppExp for soac2
ppExp d (Map2 fun lst _ _) =
    " map2 ( " ++ ppLambda (d+1) fun ++ ", " ++
    intercalate ", " (map (ppExp (d+1)) lst) ++ " ) "

--
ppExp d (Reduce2 fun el arrs _ _) =
    " reduce2 ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++
    intercalate ", " (map (ppExp (d+1)) arrs) ++ " ) "
--
ppExp d (Scan2  fun el lst _ _) =
    " scan2 ( " ++ ppLambda (d+1) fun ++ ", " ++ ppExp (d+1) el ++ ", " ++
    intercalate ", " (map (ppExp (d+1)) lst) ++ " ) "
--
ppExp d (Filter2 fun els _) =
    " filter2 ( " ++ ppLambda (d+1) fun ++ ", " ++
    intercalate ", " (map (ppExp (d+1)) els) ++ " ) "
--
ppExp d (Redomap2 id1 id2 el els _ _)
          = " redomap2 ( " ++ ppLambda (d+1) id1 ++ ", " ++ ppLambda (d+1) id2 ++ 
            ", " ++ ppExp (d+1) el ++ "," ++ intercalate ", " (map (ppExp (d+1)) els) ++ " ) "
--
ppExp d (Mapall2 fun lst _) = 
    " mapall2 ( " ++ ppLambda (d+1) fun ++ ", " ++
    intercalate ", " (map (ppExp (d+1)) lst) ++ " ) "
--- Cosmin end ppExp for soac2

ppBinOp :: BinOp -> String
ppBinOp op = " " ++ opStr op ++ " "

ppUniqueness :: Uniqueness -> String
ppUniqueness Unique = "*"
ppUniqueness Nonunique = ""

-- | Pretty printing a type
ppType :: Type -> String
ppType (Elem Int) = " int "
ppType (Elem Bool) = " bool "
ppType (Elem Char) = " char "
ppType (Elem Real) = " real "
ppType (Array tp ds u) = ppUniqueness u ++ foldl f pptp ds
  where f s Nothing  = "[ " ++ s ++ " ]"
        f s (Just e) = "[ " ++ s ++ ", " ++ ppExp 0 e ++ " ]"
        pptp = ppType $ Elem tp
ppType (Elem (Tuple tps)) = "( " ++ intercalate ", " (map ppType tps) ++ " ) "

-- | Pretty printing a tuple id
ppTupId :: TupIdent ty -> String
ppTupId (Id ident) = " " ++ nameToString (identName ident) ++ " "
ppTupId (TupId pats _) = " ( " ++ intercalate ", " (map ppTupId pats) ++ " ) "

--        "\n" ++ spaces d ++ "let " ++ ppTupId tupid ++ " = " ++ ppExp d e1 ++
--        " in  " ++ ppExp d body



-- pretty printing Lambda, i.e., curried and unnamed functions *)
ppLambda :: TypeBox ty => Int -> Lambda ty -> String
ppLambda d ( AnonymFun params body rtp _) =
      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ nameToString arg
          strargs = intercalate ", " $ map pp_bd params
      in "\n" ++ spaces d ++ "fn " ++ ppType rtp ++ " ( " ++ strargs ++ 
         " ) " ++ " => " ++ ppExp (d+1) body ++ "\n" ++ spaces d
--      let pp_bd (Ident arg tp _) = ppType tp ++ " " ++ arg
--          strargs = intercalate ", " $ map pp_bd params
--      in " fn " ++ ppType rtp ++ " ( " ++ strargs ++ " ) " ++ " => " ++ ppExp 0 body
ppLambda _ ( CurryFun fid []   _  _  ) = nameToString fid
ppLambda _ ( CurryFun fid args ty pos) =
      ppExp 0 (Apply fid args ty pos)

-- | pretty printing a function declaration
ppFun ::  TypeBox ty => Int -> FunDec ty -> String
ppFun d (name, ret_tp, args, body, _) =
  let -- pretty printing a list of bindings separated by commas
      ppBd (Ident argname tp _) = ppType tp ++ " " ++ nameToString argname
      pp_bindings = intercalate "," . map ppBd

  in "\n\nfun " ++ ppType ret_tp ++ nameToString name ++
     "( " ++ pp_bindings args ++ ") = \n" ++
     spaces (d+1) ++ ppExp (d+1) body

-- | Pretty printing a program.
prettyPrint ::  TypeBox ty => Prog ty -> String
prettyPrint p = concatMap (ppFun 0) p ++ "\n"
-}
