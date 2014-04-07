{
module Language.Futhark.Parser.Parser
  ( prog
  , expression
  , pattern
  , lambda
  , futharktype
  , intValue
  , realValue
  , boolValue
  , charValue
  , stringValue
  , certValue
  , arrayValue
  , tupleValue
  , anyValue
  , anyValues
  , ParserMonad
  , ReadLineMonad(..)
  , getLinesFromIO
  , getLinesFromStrings
  , getNoLines
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Applicative ((<$>), (<*>))
import Data.Array
import Data.Loc hiding (L, unLoc) -- Lexer has replacements.

import Language.Futhark.Syntax hiding (ID)
import Language.Futhark.Attributes
import Language.Futhark.Parser.Lexer

}

%name prog Prog
%name expression Exp
%name pattern TupId
%name lambda FunAbstr
%name futharktype Type
%name intValue IntValue
%name realValue RealValue
%name boolValue LogValue
%name charValue CharValue
%name certValue CertValue
%name stringValue StringValue
%name arrayValue ArrayValue
%name tupleValue TupleValue
%name anyValue Value
%name anyValues CatValues

%tokentype { L Token }
%error { parseError }
%monad { ParserMonad }
%lexer { lexer } { L _ EOF }

%token
      if              { L $$ IF }
      then            { L $$ THEN }
      else            { L $$ ELSE }
      let             { L $$ LET }
      loop            { L $$ LOOP }
      in              { L $$ IN }
      int             { L $$ INT }
      bool            { L $$ BOOL }
      cert            { L $$ CERT }
      char            { L $$ CHAR }
      real            { L $$ REAL }

      id              { L _ (ID _) }

      intlit          { L _ (INTLIT _) }
      reallit         { L _ (REALLIT _) }
      stringlit       { L _ (STRINGLIT _) }
      charlit         { L _ (CHARLIT _) }

      '+'             { L $$ PLUS }
      '-'             { L $$ MINUS }
      '*'             { L $$ TIMES }
      '/'             { L $$ DIVIDE }
      '%'             { L $$ MOD }
      '='             { L $$ EQU }
      '<'             { L $$ LTH }
      '>'             { L $$ GTH }
      '<='            { L $$ LEQ }
      pow             { L $$ POW }
      '<<'            { L $$ SHIFTL }
      '>>'            { L $$ SHIFTR }
      '|'             { L $$ BOR }
      '&'             { L $$ BAND }
      '^'             { L $$ XOR }
      '('             { L $$ LPAR }
      ')'             { L $$ RPAR }
      '['             { L $$ LBRACKET }
      ']'             { L $$ RBRACKET }
      '{'             { L $$ LCURLY }
      '}'             { L $$ RCURLY }
      ','             { L $$ COMMA }
      '_'             { L $$ UNDERSCORE }
      fun             { L $$ FUN }
      fn              { L $$ FN }
      '=>'            { L $$ ARROW }
      '<-'            { L $$ SETTO }
      for             { L $$ FOR }
      do              { L $$ DO }
      with            { L $$ WITH }
      iota            { L $$ IOTA }
      size            { L $$ SIZE }
      replicate       { L $$ REPLICATE }
      map             { L $$ MAP }
      reduce          { L $$ REDUCE }
      reshape         { L $$ RESHAPE }
      rearrange       { L $$ REARRANGE }
      rotate          { L $$ ROTATE }
      transpose       { L $$ TRANSPOSE }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      scan            { L $$ SCAN }
      split           { L $$ SPLIT }
      concat          { L $$ CONCAT }
      filter          { L $$ FILTER }
      redomap         { L $$ REDOMAP }
      true            { L $$ TRUE }
      false           { L $$ FALSE }
      checked         { L $$ CHECKED }
      not             { L $$ NOT }
      '~'             { L $$ NEGATE }
      '&&'            { L $$ AND }
      '||'            { L $$ OR }
      op              { L $$ OP }
      empty           { L $$ EMPTY }
      copy            { L $$ COPY }
      assert          { L $$ ASSERT }
      conjoin         { L $$ CONJOIN }

%nonassoc ifprec letprec
%left '||'
%left '&&'
%left '&' '^' '|'
%left '<=' '<' '='

%left '<<' '>>'
%left '+' '-'

%left '*' '/' '%'
%left pow
%nonassoc not '~'

%%

Prog :: { UncheckedProg }
     :   FunDecs {- EOF -}   { Prog $1 }
;

Ops : op '+'     { (nameFromString "op +", $1) }
    | op '*'     { (nameFromString "op *", $1) }
    | op '-'     { (nameFromString "op -", $1) }
    | op '/'     { (nameFromString "op /", $1) }
    | op '%'     { (nameFromString "op %", $1) }
    | op '='     { (nameFromString "op =", $1) }
    | op '<'     { (nameFromString "op <", $1) }
    | op '<='    { (nameFromString "op <=", $1) }
    | op '&&'    { (nameFromString "op &&", $1) }
    | op '||'    { (nameFromString "op ||", $1) }
    | op not     { (nameFromString "op not", $1) }
    | op '~'     { (nameFromString "op ~",$1) }
    | op pow     { (nameFromString "op pow", $1) }
    | op '^'     { (nameFromString "op ^", $1) }
    | op '&'     { (nameFromString "op &", $1) }
    | op '|'     { (nameFromString "op |", $1) }
    | op '>>'    { (nameFromString "op >>", $1) }
    | op '<<'    { (nameFromString "op <<", $1) }

FunDecs : fun Fun FunDecs   { $2 : $3 }
        | fun Fun           { [$2] }
;

Fun :     Type id '(' TypeIds ')' '=' Exp
                        { let L pos (ID name) = $2 in (name, $1, $4, $7, pos) }
        | Type id '(' ')' '=' Exp
                        { let L pos (ID name) = $2 in (name, $1, [], $6, pos) }
;

Uniqueness : '*' { Unique }
           |     { Nonunique }

Type :: { UncheckedType }
        :    ElemType { Elem $1 }
        | Uniqueness '[' InnerType ']'
          { let (ds, et) = $3
            in Array et (Nothing:ds) $1 NoInfo }
--        | Uniqueness '[' InnerType ',' Exp ']'
--          { let (ds, et) = $3
--            in Array et (Just $5:ds) $1 NoInfo }
;

InnerType : ElemType { ([], $1) }
          | '[' InnerType ']' { let (ds, et) = $2
                                in (Nothing:ds, et) }
--          | '[' InnerType ',' Exp ']' { let (ds, et) = $2
--                                        in (Just $4:ds, et) }

ElemType : int           { Basic Int }
         | real          { Basic Real }
         | bool          { Basic Bool }
         | cert          { Basic Cert }
         | char          { Basic Char }
         | TupleType     { Tuple $1 }

TupleType : '{' Types '}' { $2 }

Types : Type ',' Types { $1 : $3 }
      | Type           { [$1] }
      |                { [] }
;

TypeIds : Type id ',' TypeIds
                        { let L pos (ID name) = $2 in Ident name $1 pos : $4 }
        | Type id       { let L pos (ID name) = $2 in [Ident name $1 pos] }
;

Exp  :: { UncheckedExp }
     : intlit         { let L pos (INTLIT num) = $1 in Literal (BasicVal $ IntVal num) pos }
     | reallit        { let L pos (REALLIT num) = $1 in Literal (BasicVal $ RealVal num) pos }
     | charlit        { let L pos (CHARLIT char) = $1 in Literal (BasicVal $ CharVal char) pos }
     | stringlit      { let L pos (STRINGLIT s) = $1
                        in Literal (ArrayVal (arrayFromList $ map (BasicVal . CharVal) s) $ Elem $ Basic Char) pos }
     | true           { Literal (BasicVal $ LogVal True) $1 }
     | false          { Literal (BasicVal $ LogVal False) $1 }
     | checked        { Literal (BasicVal Checked) $1 }
     | Id             { Var $1 }
     | empty '(' Type ')' { Literal (emptyArray $3) $1 }
     | '[' Exps ']'   { ArrayLit $2 NoInfo $1 }
     | TupleExp       { let (exps, pos) = $1 in TupLit exps pos }

     | Exp '+' Exp    { BinOp Plus $1 $3 NoInfo $2 }
     | Exp '-' Exp    { BinOp Minus $1 $3 NoInfo $2 }
     | Exp '*' Exp    { BinOp Times $1 $3 NoInfo $2 }
     | Exp '/' Exp    { BinOp Divide $1 $3 NoInfo $2 }
     | Exp '%' Exp    { BinOp Mod $1 $3 NoInfo $2 }
     | '-' Exp        { Negate $2 $1 }
     | not Exp        { Not $2 $1 }
     | Exp pow Exp    { BinOp Pow $1 $3 NoInfo $2 }
     | Exp '>>' Exp   { BinOp ShiftR $1 $3 NoInfo $2 }
     | Exp '<<' Exp   { BinOp ShiftL $1 $3 NoInfo $2 }
     | Exp '&&' Exp   { BinOp LogAnd $1 $3 NoInfo $2 }
     | Exp '||' Exp   { BinOp LogOr $1 $3 NoInfo $2 }
     | Exp '&' Exp    { BinOp Band $1 $3 NoInfo $2 }
     | Exp '|' Exp    { BinOp Bor $1 $3 NoInfo $2 }
     | Exp '^' Exp    { BinOp Xor $1 $3 NoInfo $2 }

     | Exp '=' Exp    { BinOp Equal $1 $3 NoInfo $2 }
     | Exp '<' Exp    { BinOp Less $1 $3 NoInfo $2 }
     | Exp '<=' Exp   { BinOp Leq  $1 $3 NoInfo $2 }

     | if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 NoInfo $1 }

     | id '(' Exps ')'
                      { let L pos (ID name) = $1
                        in Apply name [ (arg, Observe) | arg <- $3 ] NoInfo pos }
     | id '(' ')'     { let L pos (ID name) = $1
                        in Apply name [] NoInfo pos }

     | iota '(' Exp ')' { Iota $3 $1 }

     | Certificates size '(' NaturalInt ',' Exp ')'
                      { Size $1 $4 $6 $2 }

     | size '(' NaturalInt ',' Exp ')'
                      { Size [] $3 $5 $1 }

     | replicate '(' Exp ',' Exp ')' { Replicate $3 $5 $1 }

     | Certificates reshape '(' '(' Exps ')' ',' Exp ')'
                      { Reshape $1 $5 $8 $2 }

     | reshape '(' '(' Exps ')' ',' Exp ')'
                      { Reshape [] $4 $7 $1 }

     | Certificates rearrange '(' '(' NaturalInts ')' ',' Exp ')'
                      { Rearrange $1 $5 $8 $2 }

     | rearrange '(' '(' NaturalInts ')' ',' Exp ')'
                      { Rearrange [] $4 $7 $1 }

     | Certificates transpose '(' Exp ')' { Transpose $1 0 1 $4 $2 }

     | transpose '(' Exp ')' { Transpose [] 0 1 $3 $1 }

     | Certificates transpose '(' NaturalInt ',' SignedInt ',' Exp ')'
                      { Transpose $1 $4 $6 $8 $2 }

     | transpose '(' NaturalInt ',' SignedInt ',' Exp ')'
                      { Transpose [] $3 $5 $7 $1 }

     | rotate '(' SignedInt ',' Exp ')'
                      { Rotate [] $3 $5 $1 }

     | Certificates split '(' Exp ',' Exp ')'
                      { Split $1 $4 $6 $2 }

     | split '(' Exp ',' Exp ')'
                      { Split [] $3 $5 $1 }

     | Certificates concat '(' Exp ',' Exp ')'
                      { Concat $1 $4 $6 $2 }

     | concat '(' Exp ',' Exp ')'
                      { Concat [] $3 $5 $1 }

     | reduce '(' FunAbstr ',' Exp ',' Exp ')'
                      { Reduce $3 $5 $7 $1 }

     | map '(' FunAbstr ',' Exp ')'
                      { Map $3 $5 $1 }

     | scan '(' FunAbstr ',' Exp ',' Exp ')'
                      { Scan $3 $5 $7 $1 }

     | zip '(' Exps2 ')'
                      { Zip (map (\x -> (x, NoInfo)) $3) $1 }

     | unzip '(' Exp ')'
                      { Unzip $3 [] $1 }

     | filter '(' FunAbstr ',' Exp ')'
                      { Filter $3 $5 $1 }

     | redomap '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                      { Redomap $3 $5 $7 $9 $1 }

     | copy '(' Exp ')' { Copy $3 $1 }

     | assert '(' Exp ')' { Assert $3 $1 }

     | conjoin '(' Exps ')' { Conjoin $3 $1 }

     | conjoin '(' ')' { Conjoin [] $1 }

     | '(' Exp ')' { $2 }

     | let Id '=' Exp in Exp %prec letprec
                      { LetPat (Id $2) $4 $6 $1 }

     | let '_' '=' Exp in Exp %prec letprec
                      { LetPat (Wildcard NoInfo $2) $4 $6 $1 }

     | let '{' TupIds '}' '=' Exp in Exp %prec letprec
                      { LetPat (TupId $3 $1) $6 $8 $1 }

     | let Certificates Id '=' Id with Index '<-' Exp in Exp %prec letprec
                      { LetWith $2 $3 $5 (fst $7) (snd $7) $9 $11 $1 }
     | let Id '=' Id with Index '<-' Exp in Exp %prec letprec
                      { LetWith [] $2 $4 (fst $6) (snd $6) $8 $10 $1 }
     | let Certificates Id Index '=' Exp in Exp %prec letprec
                      { LetWith $2 $3 $3 (fst $4) (snd $4) $6 $8 $1 }
     | let Id Index '=' Exp in Exp %prec letprec
                      { LetWith [] $2 $2 (fst $3) (snd $3) $5 $7 $1 }
     | let Certificates Id '[' ']' '=' Exp in Exp %prec letprec
                      { LetWith $2 $3 $3 Nothing [] $7 $9 $1 }
     | let Id '[' ']' '=' Exp in Exp %prec letprec
                      { LetWith [] $2 $2 Nothing [] $6 $8 $1 }

     | Id Index
                      { Index [] $1 (fst $2) (snd $2) (srclocOf $1) }

     | Certificates Id Index
                      { Index $1 $2 (fst $3) (snd $3) (srclocOf $2) }

     | loop '(' TupId ')' '=' for Id '<' Exp do Exp in Exp %prec letprec
                      {% liftM (\t -> DoLoop $3 t $7 $9 $11 $13 $1) (tupIdExp $3) }
     | loop '(' TupId '=' Exp ')' '=' for Id '<' Exp do Exp in Exp %prec letprec
                      { DoLoop $3 $5 $9 $11 $13 $15 $1 }

Index : '[' Certificates '|' Exps ']' { (Just $2, $4) }
      | '[' Exps ']'                  { (Nothing, $2) }

Exps : Exp ',' Exps { $1 : $3 }
     | Exp          { [$1] }

Exps2 : Exp ',' Exps2 { $1 : $3 }
      | Exp ',' Exp   { [$1, $3] }


TupleExp : '{' Exps '}' { ($2, $1) }
         | '{'      '}' { ([], $1) }

Id : id { let L loc (ID name) = $1 in Ident name NoInfo loc }

Ids : Id         { [$1] }
    | Id ',' Ids { $1 : $3 }

Certificates : '<' '>'                   { [] }
             | '<' Ids '>' { $2 }

TupIds : TupId ',' TupIds  { $1 : $3 }
       | TupId             { [$1] }
       |                   { [] }
;

TupId : id { let L pos (ID name) = $1 in Id $ Ident name NoInfo pos }
      | '_' { Wildcard NoInfo $1 }
      | '{' TupIds '}' { TupId $2 $1 }

FunAbstr : id { let L pos (ID name) = $1 in CurryFun name [] NoInfo pos }
         | Ops { let (name,pos) = $1 in CurryFun name [] NoInfo pos }
         | id '(' ')' { let L pos (ID name) = $1 in CurryFun name [] NoInfo pos }
         | Ops '(' ')' { let (name,pos) = $1 in CurryFun name [] NoInfo pos }
         | id '(' Exps ')'
               { let L pos (ID name) = $1 in CurryFun name $3 NoInfo pos }
         | Ops '(' Exps ')'
               { let (name,pos) = $1 in CurryFun name $3 NoInfo pos }
         | fn Type '(' TypeIds ')' '=>' Exp { AnonymFun $4 $7 $2 $1 }

Value : IntValue { $1 }
      | RealValue { $1 }
      | CharValue { $1 }
      | StringValue { $1 }
      | LogValue { $1 }
      | ArrayValue { $1 }
      | TupleValue { $1 }

CatValues : Value CatValues { $1 : $2 }
          |                 { [] }

SignedInt :     intlit { let L _ (INTLIT num) = $1 in num  }
          | '-' intlit { let L _ (INTLIT num) = $2 in -num }

NaturalInt :: { Int }
           :  intlit { let L _ (INTLIT num) = $1 in num  }

NaturalInts :: { [Int] }
           : intlit                 { let L _ (INTLIT num) = $1 in [num] }
           | intlit ',' NaturalInts { let L _ (INTLIT num) = $1 in num : $3  }

IntValue : intlit        { let L pos (INTLIT num) = $1 in BasicVal $ IntVal num }
         | '-' intlit    { let L pos (INTLIT num) = $2 in BasicVal $ IntVal (-num) }
RealValue : reallit      { let L pos (REALLIT num) = $1 in BasicVal $ RealVal num }
          | '-' reallit      { let L pos (REALLIT num) = $2 in BasicVal $ RealVal (-num) }
CharValue : charlit      { let L pos (CHARLIT char) = $1 in BasicVal $ CharVal char }
StringValue : stringlit  { let L pos (STRINGLIT s) = $1 in ArrayVal (arrayFromList $ map (BasicVal . CharVal) s) $ Elem $ Basic Char }
LogValue : true          { BasicVal $ LogVal True }
        | false          { BasicVal $ LogVal False }
CertValue : checked      { BasicVal Checked }
ArrayValue :  '[' Values ']'
             {% case combArrayTypes $ map (toDecl . valueType) $2 of
                  Nothing -> throwError "Invalid array value"
                  Just ts -> return $ ArrayVal (arrayFromList $2) $ removeNames ts
             }
TupleValue : '(' Values2 ')' { TupVal $2 }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }

Values2 : Value ',' Values { $1 : $3 }

{

type ParserMonad a =
  ErrorT String (
    ReaderT FilePath (
       StateT [L Token] ReadLineMonad)) a

data ReadLineMonad a = Value a
                     | GetLine (String -> ReadLineMonad a)

readLineFromMonad :: ReadLineMonad String
readLineFromMonad = GetLine Value

instance Monad ReadLineMonad where
  return = Value
  Value x >>= f = f x
  GetLine g >>= f = GetLine $ \s -> g s >>= f

instance Functor ReadLineMonad where
  f `fmap` m = do x <- m
                  return $ f x

instance Applicative ReadLineMonad where
  (<*>) = ap

getLinesFromIO :: ReadLineMonad a -> IO a
getLinesFromIO (Value x) = return x
getLinesFromIO (GetLine f) = do
  s <- getLine
  getLinesFromIO $ f s

getLinesFromStrings :: [String] -> ReadLineMonad a -> Either String a
getLinesFromStrings _ (Value x) = Right x
getLinesFromStrings (x : xs) (GetLine f) = getLinesFromStrings xs $ f x
getLinesFromStrings [] (GetLine _) = Left "Ran out of input"

getNoLines :: ReadLineMonad a -> Either String a
getNoLines (Value x) = Right x
getNoLines (GetLine _) = Left "No extra lines"

combArrayTypes :: [UncheckedType] -> Maybe UncheckedType
combArrayTypes []     = Nothing
combArrayTypes (v:vs) = foldM comb v vs
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

tupIdExp :: UncheckedTupIdent -> ParserMonad UncheckedExp
tupIdExp (Id ident) = return $ Var ident
tupIdExp (TupId pats loc) = TupLit <$> (mapM tupIdExp pats) <*> return loc
tupIdExp (Wildcard _ loc) = throwError $ "Cannot have wildcard at " ++ locStr loc

eof :: L Token
eof = L (SrcLoc $ Loc (Pos "" 0 0 0) (Pos "" 0 0 0)) EOF

getTokens :: ParserMonad [L Token]
getTokens = lift $ lift get

putTokens :: [L Token] -> ParserMonad ()
putTokens ts = lift $ lift $ put ts

getFilename :: ParserMonad FilePath
getFilename = lift ask

readLine :: ParserMonad String
readLine = lift $ lift $ lift readLineFromMonad

lexer :: (L Token -> ParserMonad a) -> ParserMonad a
lexer cont = do
  ts <- getTokens
  case ts of
    [] -> do
      ended <- lift $ runErrorT $ cont eof
      case ended of
        Right x -> return x
        Left _ -> do
          ts' <- alexScanTokens <$> getFilename <*> readLine
          ts'' <- case ts' of Right x -> return x
                              Left e  -> throwError e
          case ts'' of
            [] -> cont eof
            xs -> do
              putTokens xs
              lexer cont
    (x : xs) -> do
      putTokens xs
      cont x

parseError :: L Token -> ParserMonad a
parseError (L _ EOF) = throwError "Parse error: End of file"
parseError tok       = throwError $ "Parse error at " ++ locStr (srclocOf tok)
}
