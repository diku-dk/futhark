-- | L0 prettyprinter.
module Language.L0.Pretty
  ( ppType
  , flattenArray
  , ppValue
  , ppExp
  , ppBinOp
  , ppTupId
  , prettyPrint
  )
  where

import Data.Array
import Data.List

import Language.L0.Syntax
import Language.L0.Attributes

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
            ", " ++ ppExp (d+1) el ++ intercalate ", " (map (ppExp (d+1)) els) ++ " ) "
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
