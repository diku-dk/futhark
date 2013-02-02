{
module L0.Parser(parseL0) where

import L0.AbSyn
import L0.Lexer


}

%name l0
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

Exp  : intlit         { let INTLIT num pos = $1 in NumInt num pos }
     | reallit        { let REALLIT num pos = $1 in NumReal num pos }
     | charlit        { let CHARLIT char pos = $1 in CharLit char pos }
     | stringlit      { let STRINGLIT s pos = $1 in StringLit s pos }
     | true           { Log True $1 }
     | false          { Log False $1 }
     | id             { let ID name pos = $1 in Var name Nothing pos }
     | '{' Exps '}'   { ArrayLit $2 Nothing $1 }
     | TupleExp       { let (exps, pos) = $1 in TupLit exps Nothing pos }

     | Exp '+' Exp    { Plus  $1 $3 Nothing $2 }
     | Exp '-' Exp    { Minus $1 $3 Nothing $2 }
     | Exp '*' Exp    { Times $1 $3 Nothing $2 }
     | Exp '/' Exp    { Divide $1 $3 Nothing $2 }
     | '~' Exp        { Negate $2 Nothing $1 }
     | not Exp        { Not $2 $1 }
     | Exp '&&' Exp   { And $1 $3 $2 }
     | Exp '||' Exp   { Or $1 $3 $2 }
     | Exp pow Exp    { Pow $1 $3 Nothing $2 }
     | Exp '>>' Exp   { ShiftR $1 $3 $2 }
     | Exp '<<' Exp   { ShiftL $1 $3 $2 }
     | Exp '&' Exp    { Band $1 $3 $2 }
     | Exp '|' Exp    { Bor $1 $3 $2 }
     | Exp '^' Exp    { Xor $1 $3 $2 }

     | Exp '=' Exp    { Equal $1 $3 $2 }
     | Exp '<' Exp    { Less $1 $3 $2 }
     | Exp '<=' Exp   { Leq  $1 $3 $2 }

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

     | let id '=' Exp in Exp
                      { let ID name pos = $2
                        in Let (Id name pos) $4 Nothing Nothing $6 $1 }

     | let '(' TupIds ')' '=' Exp in Exp
                      { Let (TupId $3 $1) $6 Nothing Nothing $8 $1 }

     | let id '=' Exp with '[' Exps ']' '<-' Exp in Exp
                      { let ID name pos = $2
                        in Let (Id name pos) $4 (Just $7) (Just $10) $12 $1 }
     | let id '[' Exps ']' '=' Exp in Exp
                      { let ID name pos = $2
                        in Let (Id name pos) (Var name Nothing pos) (Just $4) (Just $7) $9 $1 }

     | id '[' Exps ']'
                      { let ID name pos = $1
                        in Index name $3 Nothing Nothing pos }

     | for id '<' Exp do Exp merge Ids
                      { let ID name _ = $2
                        in DoLoop name $4 $6 $8 $1 }

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

TupId : id { let ID name pos = $1 in Id name pos }
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

{
parseError :: [Token] -> a
parseError [] = error "Parse error: End of file"
parseError (tok:_) = error $ "Parse error at " ++ show (tokPos tok)

parseL0 :: String -> Prog Maybe
parseL0 = l0 . alexScanTokens
}
