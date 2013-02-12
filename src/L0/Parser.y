{
module L0.Parser( parseL0
                , parseInt
                , parseReal
                , parseBool
                , parseChar
                , parseString
                , parseArray
                , parseTuple)
  where

import Control.Monad (foldM)

import L0.AbSyn
import L0.Lexer

}

%name prog Prog
%name intValue IntValue
%name realValue RealValue
%name logValue LogValue
%name charValue CharValue
%name stringValue StringValue
%name arrayValue ArrayValue
%name tupleValue TupleValue

%tokentype { Token }
%error { parseError }

%token 
      if              { IF $$ }
      then            { THEN $$ }
      else            { ELSE $$ }
      let             { LET $$ }
      in              { IN $$ }
      int             { INT $$ }
      bool            { BOOL $$ }
      char            { CHAR $$ }
      real            { REAL $$ }

      id              { ID _ _ }

      intlit          { INTLIT _ _ }
      reallit         { REALLIT _ _ }
      stringlit       { STRINGLIT _ _ }
      charlit         { CHARLIT _ _ }

      '+'             { PLUS $$ }
      '-'             { MINUS $$ }
      '*'             { TIMES $$ }
      '/'             { DIVIDE $$ }
      '='             { EQU $$ }
      '<'             { LTH $$ }
      '<='            { LEQ $$ }
      pow             { POW $$ }
      '<<'            { SHIFTL $$ }
      '>>'            { SHIFTR $$ }
      '|'             { BOR $$ }
      '&'             { BAND $$ }
      '^'             { XOR $$ }
      '('             { LPAR $$ }
      ')'             { RPAR $$ }
      '['             { LBRACKET $$ }
      ']'             { RBRACKET $$ }
      '{'             { LCURLY $$ }
      '}'             { RCURLY $$ }
      ','             { COMMA $$ }
      fun             { FUN $$ }
      fn              { FN $$ }
      '=>'            { ARROW $$ }
      '<-'            { SETTO $$ }
      for             { FOR $$ }
      do              { DO $$ }
      with            { WITH $$ }
      merge           { MERGE $$ }
      iota            { IOTA $$ }
      size            { SIZE $$ }
      replicate       { REPLICATE $$ }
      map             { MAP $$ }
      reduce          { REDUCE $$ }
      reshape         { RESHAPE $$ }
      transpose       { TRANSPOSE $$ }
      read            { READ $$ }
      write           { WRITE $$ }
      zipWith         { ZIPWITH $$ }
      scan            { SCAN $$ }
      split           { SPLIT $$ }
      concat          { CONCAT $$ }
      filter          { FILTER $$ }
      mapall          { MAPALL $$ }
      redomap         { REDOMAP $$ }
      true            { TRUE $$ }
      false           { FALSE $$ }
      not             { NOT $$ }
      '~'             { NEGATE $$ }
      '&&'            { AND $$ }
      '||'            { OR $$ }
      op              { OP $$ }

%nonassoc ifprec letprec
%left '||'
%left '&&'
%left '&' '^' '|'
%left '<=' '<' '=' 

%left '<<' '>>'
%left '+' '-'

%left '*' '/'
%left pow
%nonassoc not '~'

%%

Prog :	  FunDecs {- EOF -}   { $1 }
;

Ops : op '+'     { ("op +", $1) }
    | op '*'     { ("op *", $1) }
    | op '-'     { ("op -", $1) }
    | op '/'     { ("op /", $1) }
    | op '='     { ("op =", $1) }
    | op '<'     { ("op <", $1) }
    | op '<='    { ("op <=", $1) }
    | op '&&'    { ("op &&", $1) }
    | op '||'    { ("op ||", $1) }
    | op not     { ("op not", $1) }
    | op '~'     { ("op ~",$1) }
    | op pow     { ("op pow", $1) }
    | op '^'     { ("op ^", $1) }
    | op '&'     { ("op &", $1) }
    | op '|'     { ("op |", $1) }
    | op '>>'    { ("op >>", $1) }
    | op '<<'    { ("op <<", $1) }

FunDecs : fun Fun FunDecs   { $2 : $3 }
        | fun Fun           { [$2] }
;

Fun :     Type id '(' TypeIds ')' '=' Exp 
			{ let ID name pos = $2 in (name, $1, $4, $7, pos) }
        | Type id '(' ')' '=' Exp 
			{ let ID name pos = $2 in (name, $1, [], $6, pos) }
;

Type :	  int                    { Int   $1             }
        | real                   { Real  $1             }
        | bool                   { Bool  $1             }
        | char                   { Char  $1             }
        | '(' Types ')'        { Tuple $2 $1       }
        | '[' Type ']' { Array $2 Nothing $1 }
        | '[' Type ',' Exp ']' { Array $2 (Just $4) $1 }
;

Types : Type '*' Types { $1 : $3 }
      | Type '*' Type  { [$1, $3] }
;

TypeIds : Type id ',' TypeIds
                        { let ID name _ = $2 in (name, $1) : $4 }
        | Type id       { let ID name _ = $2 in [(name, $1)] }
;

Exp  : intlit         { let INTLIT num pos = $1 in Literal $ IntVal num pos }
     | reallit        { let REALLIT num pos = $1 in Literal $ RealVal num pos }
     | charlit        { let CHARLIT char pos = $1 in Literal $ CharVal char pos }
     | stringlit      { let STRINGLIT s pos = $1
                        in Literal $ ArrayVal (map (`CharVal` pos) s) (Array (Char pos) Nothing pos) pos }
     | true           { Literal $ LogVal True $1 }
     | false          { Literal $ LogVal False $1 }
     | id             { let ID name pos = $1 in Var name Nothing pos }
     | '{' Exps '}'   { ArrayLit $2 Nothing $1 }
     | TupleExp       { let (exps, pos) = $1 in TupLit exps Nothing pos }

     | Exp '+' Exp    { BinOp Plus $1 $3 Nothing $2 }
     | Exp '-' Exp    { BinOp Minus $1 $3 Nothing $2 }
     | Exp '*' Exp    { BinOp Times $1 $3 Nothing $2 }
     | Exp '/' Exp    { BinOp Divide $1 $3 Nothing $2 }
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
                      { let ID name pos = $1
                        in Apply name $3 Nothing pos }
     | id '(' ')'     { let ID name pos = $1
                        in Apply name [] Nothing pos }

     | read '(' Type ')' { Read $3 $1 }
     | write '(' Exp ')' { Write $3 Nothing $1 }

     | iota '(' Exp ')' { Iota $3 $1 }

     | size '(' Exp ')' { Size $3 $1 }

     | replicate '(' Exp ',' Exp ')' { Replicate $3 $5 Nothing $1 }

     | reshape '(' '(' Exps ')' ',' Exp ')'
                      { Reshape $4 $7 Nothing Nothing $1 }

     | transpose '(' Exp ')'
                      { Transpose $3 Nothing Nothing $1 }

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

     | zipWith '(' FunAbstr ',' Exps ')'
                      { ZipWith $3 $5 Nothing Nothing $1 }

     | filter '(' FunAbstr ',' Exp ')'
                      { Filter $3 $5 Nothing $1 }

     | mapall '(' FunAbstr ',' Exp ')'
                      { Mapall $3 $5 Nothing Nothing $1 }

     | redomap '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                      { Redomap $3 $5 $7 $9 Nothing Nothing $1 }

     | '(' Exp ')' { $2 }

     | let id '=' Exp in Exp %prec letprec
                      { let ID name pos = $2
                        in LetPat (Id name Nothing pos) $4 $6 $1 }

     | let '(' TupIds ')' '=' Exp in Exp %prec letprec
                      { LetPat (TupId $3 $1) $6 $8 $1 }

     | let id '=' Exp with '[' Exps ']' '<-' Exp in Exp %prec letprec
                      { let ID name _ = $2
                        in LetWith name $4 $7 $10 $12 $1 }
     | let id '[' Exps ']' '=' Exp in Exp %prec letprec
                      { let ID name pos = $2
                        in LetWith name (Var name Nothing pos) $4 $7 $9 $1 }

     | id '[' Exps ']'
                      { let ID name pos = $1
                        in Index name $3 Nothing Nothing pos }

     | for id '<' Exp do Exp merge '(' Ids ')'
                      { let ID name _ = $2
                        in DoLoop name $4 $6 $9 $1 }
     | for id '<' Exp do Exp merge id
                      {case ($2, $8) of
                          (ID name _, ID mergename _) -> DoLoop name $4 $6 [mergename] $1 }

Exps : Exp ',' Exps { $1 : $3 }
     | Exp          { [$1] }

Exps2 : Exp ',' Exps2 { $1 : $3 }
      | Exp ',' Exp   { [$1, $3] }

TupleExp : '(' Exps2 ')' { ($2, $1) }

Ids : id { let ID name pos = $1 in [name] }
    | id ',' Ids { let ID name pos = $1 in name : $3 }

TupIds : TupId ',' TupId   { [$1, $3] }
       | TupId ',' TupIds  { $1 : $3 }
;

TupId : id { let ID name pos = $1 in Id name Nothing pos }
      | '(' TupIds ')' { TupId $2 $1 }

FunAbstr : id { let ID name pos = $1 in CurryFun name [] Nothing Nothing pos }
         | Ops { let (name,pos) = $1 in CurryFun name [] Nothing Nothing pos }
         | id '(' ')' { let ID name pos = $1 in CurryFun name [] Nothing Nothing pos }
         | Ops '(' ')' { let (name,pos) = $1 in CurryFun name [] Nothing Nothing pos }
         | id '(' Exps ')'
               { let ID name pos = $1 in CurryFun name $3 Nothing Nothing pos }
         | Ops '(' Exps ')'
               { let (name,pos) = $1 in CurryFun name $3 Nothing Nothing pos }
         | fn Type '(' TypeIds ')' '=>' Exp { AnonymFun $4 $7 $2 $1 }

Value : IntValue { $1 }
      | RealValue { $1 }
      | CharValue { $1 }
      | StringValue { $1 }
      | LogValue { $1 }
      | ArrayValue { $1 }


IntValue : intlit        { let INTLIT num pos = $1 in IntVal num pos }
RealValue : reallit      { let REALLIT num pos = $1 in RealVal num pos }
CharValue : charlit      { let CHARLIT char pos = $1 in CharVal char pos }
StringValue : stringlit  { let STRINGLIT s pos = $1 in ArrayVal (map (`CharVal` pos) s) (Char pos) pos }
LogValue : true          { LogVal True $1 }
        | false          { LogVal False $1 }
ArrayValue :  '{' Values '}' { case combArrayTypes $ map valueType $2 of
                                 Nothing -> error "Invalid array value"
                                 Just ts -> ArrayVal $2 ts $1 }
TupleValue : TupleVal        { let (vals, pos) = $1 in TupVal vals pos }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }

Values2 : Value ',' Values { $1 : $3 }

TupleVal : '(' Values2 ')' { ($2, $1) }

{
combArrayTypes :: [Type] -> Maybe Type
combArrayTypes []     = Nothing
combArrayTypes (v:vs) = foldM comb v vs
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

parseError :: [Token] -> a
parseError [] = error "Parse error: End of file"
parseError (tok:_) = error $ "Parse error at " ++ show (tokPos tok)

parseL0 :: String -> Prog Maybe
parseL0 = prog . alexScanTokens

parseInt :: String -> Value
parseInt = intValue . alexScanTokens

parseReal :: String -> Value
parseReal = realValue . alexScanTokens

parseBool :: String -> Value
parseBool = logValue . alexScanTokens

parseChar :: String -> Value
parseChar = charValue . alexScanTokens

parseString :: String -> Value
parseString = stringValue . alexScanTokens

parseTuple :: String -> Value
parseTuple = tupleValue . alexScanTokens

parseArray :: String -> Value
parseArray = arrayValue . alexScanTokens
}
