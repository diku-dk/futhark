{
module L0.Parser.Parser
  ( prog
  , intValue
  , realValue
  , boolValue
  , charValue
  , stringValue
  , arrayValue
  , tupleValue)
  where

import Control.Monad (foldM)
import Data.Array
import Data.Loc hiding (L, unLoc) -- Lexer has replacements.

import L0.AbSyn
import L0.Parser.Lexer

}

%name prog Prog
%name intValue IntValue
%name realValue RealValue
%name boolValue LogValue
%name charValue CharValue
%name stringValue StringValue
%name arrayValue ArrayValue
%name tupleValue TupleValue

%tokentype { L Token }
%error { parseError }
%monad { Either String } { >>= } { return }

%token 
      if              { L $$ IF }
      then            { L $$ THEN }
      else            { L $$ ELSE }
      let             { L $$ LET }
      loop            { L $$ LOOP }
      in              { L $$ IN }
      int             { L $$ INT }
      bool            { L $$ BOOL }
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
      transpose       { L $$ TRANSPOSE }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      scan            { L $$ SCAN }
      split           { L $$ SPLIT }
      concat          { L $$ CONCAT }
      filter          { L $$ FILTER }
      mapall          { L $$ MAPALL }
      redomap         { L $$ REDOMAP }
      true            { L $$ TRUE }
      false           { L $$ FALSE }
      not             { L $$ NOT }
      '~'             { L $$ NEGATE }
      '&&'            { L $$ AND }
      '||'            { L $$ OR }
      op              { L $$ OP }
      empty           { L $$ EMPTY }
      copy            { L $$ COPY }

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

Prog :	  FunDecs {- EOF -}   { $1 }
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

Type :	  int                    { Int      }
        | real                   { Real     }
        | bool                   { Bool     }
        | char                   { Char     }
        | '(' Types ')'          { Tuple $2 }
        | Uniqueness '[' Type ']' { Array $3 Nothing $1 }
        | Uniqueness '[' Type ',' Exp ']' { Array $3 (Just $5) $1 }
;

Types : Type ',' Types { $1 : $3 }
      | Type ',' Type  { [$1, $3] }
;

TypeIds : Type id ',' TypeIds
                        { let L pos (ID name) = $2 in Ident name $1 pos : $4 }
        | Type id       { let L pos (ID name) = $2 in [Ident name $1 pos] }
;

Exp  : intlit         { let L pos (INTLIT num) = $1 in Literal (IntVal num) pos }
     | reallit        { let L pos (REALLIT num) = $1 in Literal (RealVal num) pos }
     | charlit        { let L pos (CHARLIT char) = $1 in Literal (CharVal char) pos }
     | stringlit      { let L pos (STRINGLIT s) = $1
                        in Literal (ArrayVal (arrayFromList $ map CharVal s) Char) pos }
     | true           { Literal (LogVal True) $1 }
     | false          { Literal (LogVal False) $1 }
     | Id             { Var $1 }
     | empty '(' Type ')' { Literal (ArrayVal (arrayFromList []) $3) $1 }
     | '{' Exps '}'   { ArrayLit $2 Nothing $1 }
     | TupleExp       { let (exps, pos) = $1 in TupLit exps pos }

     | Exp '+' Exp    { BinOp Plus $1 $3 Nothing $2 }
     | Exp '-' Exp    { BinOp Minus $1 $3 Nothing $2 }
     | Exp '*' Exp    { BinOp Times $1 $3 Nothing $2 }
     | Exp '/' Exp    { BinOp Divide $1 $3 Nothing $2 }
     | Exp '%' Exp    { BinOp Mod $1 $3 Nothing $2 }
     | '~' Exp        { Negate $2 Nothing $1 }
     | not Exp        { Not $2 $1 }
     | Exp '&&' Exp   { And $1 $3 $2 }
     | Exp '||' Exp   { Or $1 $3 $2 }
     | Exp pow Exp    { BinOp Pow $1 $3 Nothing $2 }
     | Exp '>>' Exp   { BinOp ShiftR $1 $3 Nothing $2 }
     | Exp '<<' Exp   { BinOp ShiftL $1 $3 Nothing $2 }
     | Exp '&' Exp    { BinOp Band $1 $3 Nothing $2 }
     | Exp '|' Exp    { BinOp Bor $1 $3 Nothing $2 }
     | Exp '^' Exp    { BinOp Xor $1 $3 Nothing $2 }

     | Exp '=' Exp    { BinOp Equal $1 $3 Nothing $2 }
     | Exp '<' Exp    { BinOp Less $1 $3 Nothing $2 }
     | Exp '<=' Exp   { BinOp Leq  $1 $3 Nothing $2 }

     | if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 Nothing $1 }

     | id '(' Exps ')'
                      { let L pos (ID name) = $1
                        in Apply name $3 Nothing pos }
     | id '(' ')'     { let L pos (ID name) = $1
                        in Apply name [] Nothing pos }

     | iota '(' Exp ')' { Iota $3 $1 }

     | size '(' Exp ')' { Size $3 $1 }

     | replicate '(' Exp ',' Exp ')' { Replicate $3 $5 $1 }

     | reshape '(' '(' Exps ')' ',' Exp ')'
                      { Reshape $4 $7 $1 }

     | transpose '(' Exp ')'
                      { Transpose $3 $1 }

     | split '(' Exp ',' Exp ')'
                      { Split $3 $5 Nothing $1 }

     | concat '(' Exp ',' Exp ')'
                      { Concat $3 $5 Nothing $1 }

     | reduce '(' FunAbstr ',' Exp ',' Exp ')'
                      { Reduce $3 $5 $7 Nothing $1 }

     | map '(' FunAbstr ',' Exp ')'
                      { Map $3 $5 Nothing Nothing $1 }

     | scan '(' FunAbstr ',' Exp ',' Exp ')'
                      { Scan $3 $5 $7 Nothing $1 }

     | zip '(' Exps2 ')'
                      { Zip (map (\x -> (x, Nothing)) $3) $1 }

     | unzip '(' Exp ')'
                      { Unzip $3 [] $1 }

     | filter '(' FunAbstr ',' Exp ')'
                      { Filter $3 $5 Nothing $1 }

     | mapall '(' FunAbstr ',' Exp ')'
                      { Mapall $3 $5 Nothing Nothing $1 }

     | redomap '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                      { Redomap $3 $5 $7 $9 Nothing Nothing $1 }

     | copy '(' Exp ')' { Copy $3 $1 }

     | '(' Exp ')' { $2 }

     | let Id '=' Exp in Exp %prec letprec
                      { LetPat (Id $2) $4 $6 $1 }

     | let '(' TupIds ')' '=' Exp in Exp %prec letprec
                      { LetPat (TupId $3 $1) $6 $8 $1 }

     | let Id '=' Id with '[' Exps ']' '<-' Exp in Exp %prec letprec
                      { LetWith $2 $4 $7 $10 $12 $1 }
     | let Id '[' Exps ']' '=' Exp in Exp %prec letprec
                      { LetWith $2 $2 $4 $7 $9 $1 }
     | let Id '[' ']' '=' Exp in Exp %prec letprec
                      { LetWith $2 $2 [] $6 $8 $1 }

     | Id '[' Exps ']'
                      { Index $1 $3 Nothing Nothing (srclocOf $1) }

     | loop '(' TupId ')' '=' for Id '<' Exp do Exp in Exp %prec letprec
                      { DoLoop $3 (tupIdExp $3) $7 $9 $11 $13 $1 }
     | loop '(' TupId '=' Exp ')' '=' for Id '<' Exp do Exp in Exp %prec letprec
                      { DoLoop $3 $5 $9 $11 $13 $15 $1 }

Exps : Exp ',' Exps { $1 : $3 }
     | Exp          { [$1] }

Exps2 : Exp ',' Exps2 { $1 : $3 }
      | Exp ',' Exp   { [$1, $3] }

TupleExp : '(' Exps2 ')' { ($2, $1) }

Id : id { let L loc (ID name) = $1 in Ident name Nothing loc }

TupIds : TupId ',' TupId   { [$1, $3] }
       | TupId ',' TupIds  { $1 : $3 }
;

TupId : id { let L pos (ID name) = $1 in Id $ Ident name Nothing pos }
      | '(' TupIds ')' { TupId $2 $1 }

FunAbstr : id { let L pos (ID name) = $1 in CurryFun name [] Nothing pos }
         | Ops { let (name,pos) = $1 in CurryFun name [] Nothing pos }
         | id '(' ')' { let L pos (ID name) = $1 in CurryFun name [] Nothing pos }
         | Ops '(' ')' { let (name,pos) = $1 in CurryFun name [] Nothing pos }
         | id '(' Exps ')'
               { let L pos (ID name) = $1 in CurryFun name $3 Nothing pos }
         | Ops '(' Exps ')'
               { let (name,pos) = $1 in CurryFun name $3 Nothing pos }
         | fn Type '(' TypeIds ')' '=>' Exp { AnonymFun $4 $7 $2 $1 }

Value : IntValue { $1 }
      | RealValue { $1 }
      | CharValue { $1 }
      | StringValue { $1 }
      | LogValue { $1 }
      | ArrayValue { $1 }


IntValue : intlit        { let L pos (INTLIT num) = $1 in IntVal num }
RealValue : reallit      { let L pos (REALLIT num) = $1 in RealVal num }
CharValue : charlit      { let L pos (CHARLIT char) = $1 in CharVal char }
StringValue : stringlit  { let L pos (STRINGLIT s) = $1 in ArrayVal (arrayFromList $ map CharVal s) Char }
LogValue : true          { LogVal True }
        | false          { LogVal False }
ArrayValue :  '{' Values '}' { case combArrayTypes $ map typeOf $2 of
                                 Nothing -> error "Invalid array value"
                                 Just ts -> ArrayVal (arrayFromList $2) ts }
TupleValue : '(' Values2 ')'        { TupVal $2 }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }

Values2 : Value ',' Values { $1 : $3 }

{
combArrayTypes :: [Type] -> Maybe Type
combArrayTypes []     = Nothing
combArrayTypes (v:vs) = foldM comb v vs
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

tupIdExp :: TupIdent ty -> Exp ty
tupIdExp (Id ident) = Var ident
tupIdExp (TupId pats loc) = TupLit (map tupIdExp pats) loc

parseError :: [L Token] -> Either String a
parseError [] = Left "Parse error: End of file"
parseError (tok:_) = Left $ "Parse error at " ++ locStr (srclocOf tok)
}
