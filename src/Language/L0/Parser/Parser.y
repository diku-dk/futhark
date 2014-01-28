{
module Language.L0.Parser.Parser
  ( prog
  , expression
  , pattern
  , lambda
  , tupleLambda
  , l0type
  , intValue
  , realValue
  , boolValue
  , charValue
  , stringValue
  , certValue
  , arrayValue
  , tupleValue)
  where

import Control.Monad (foldM)
import Data.Array
import Data.Loc hiding (L, unLoc) -- Lexer has replacements.

import Language.L0.Syntax
import Language.L0.Attributes
import Language.L0.Parser.Lexer

}

%name prog Prog
%name expression Exp
%name pattern TupId
%name lambda FunAbstr
%name tupleLambda TupleFunAbstr
%name l0type Type
%name intValue IntValue
%name realValue RealValue
%name boolValue LogValue
%name charValue CharValue
%name certValue CertValue
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
      mapT            { L $$ MAP2 }
      reduce          { L $$ REDUCE }
      reduceT         { L $$ REDUCE2 }
      reshape         { L $$ RESHAPE }
      transpose       { L $$ TRANSPOSE }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      scan            { L $$ SCAN }
      scanT           { L $$ SCAN2 }
      split           { L $$ SPLIT }
      concat          { L $$ CONCAT }
      filter          { L $$ FILTER }
      filterT         { L $$ FILTER2 }
      redomap         { L $$ REDOMAP }
      redomapT        { L $$ REDOMAP2 }
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

ElemType : int           { Int }
         | real          { Real }
         | bool          { Bool }
         | cert          { Cert }
         | char          { Char }
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
     : intlit         { let L pos (INTLIT num) = $1 in Literal (IntVal num) pos }
     | reallit        { let L pos (REALLIT num) = $1 in Literal (RealVal num) pos }
     | charlit        { let L pos (CHARLIT char) = $1 in Literal (CharVal char) pos }
     | stringlit      { let L pos (STRINGLIT s) = $1
                        in Literal (ArrayVal (arrayFromList $ map CharVal s) $ Elem Char) pos }
     | true           { Literal (LogVal True) $1 }
     | false          { Literal (LogVal False) $1 }
     | checked        { Literal Checked $1 }
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

     | Certificates transpose '(' Exp ')' { Transpose $1 0 1 $4 $2 }

     | transpose '(' Exp ')' { Transpose [] 0 1 $3 $1 }

     | Certificates transpose '(' NaturalInt ',' SignedInt ',' Exp ')'
                      { Transpose $1 $4 $6 $8 $2 }

     | transpose '(' NaturalInt ',' SignedInt ',' Exp ')'
                      { Transpose [] $3 $5 $7 $1 }

     | Certificates split '(' Exp ',' Exp ')'
                      { Split $1 $4 $6 $2 }

     | split '(' Exp ',' Exp ')'
                      { Split [] $3 $5 $1 }

     | Certificates concat '(' Exp ',' Exp ')'
                      { Concat $1 $4 $6 $2 }

     | concat '(' Exp ',' Exp ')'
                      { Concat [] $3 $5 $1 }

     | reduce '(' FunAbstr ',' Exp ',' Exp ')'
                      { Reduce $3 $5 $7 NoInfo $1 }

     | Certificates reduceT '(' TupleFunAbstr ',' DExps ')'
                      { ReduceT $1 $4 $6 $2 }

     | reduceT '(' TupleFunAbstr ',' DExps ')'
                      { ReduceT [] $3 $5 $1 }

     | map '(' FunAbstr ',' Exp ')'
                      { Map $3 $5 NoInfo $1 }

     | Certificates mapT '(' TupleFunAbstr ',' Exps ')'
                      { MapT $1 $4 $6 $2 }

     | mapT '(' TupleFunAbstr ',' Exps ')'
                      { MapT [] $3 $5 $1 }

     | scan '(' FunAbstr ',' Exp ',' Exp ')'
                      { Scan $3 $5 $7 NoInfo $1 }

     | Certificates scanT '(' TupleFunAbstr ',' DExps ')'
                      { ScanT $1 $4 $6 $2 }

     | scanT '(' TupleFunAbstr ',' DExps ')'
                      { ScanT [] $3 $5 $1 }

     | zip '(' Exps2 ')'
                      { Zip (map (\x -> (x, NoInfo)) $3) $1 }

     | unzip '(' Exp ')'
                      { Unzip $3 [] $1 }

     | filter '(' FunAbstr ',' Exp ')'
                      { Filter $3 $5 NoInfo $1 }

     | Certificates filterT '(' TupleFunAbstr ',' Exps ')'
                      { FilterT $1 $4 $6 $2 }

     | filterT '(' TupleFunAbstr ',' Exps ')'
                      { FilterT [] $3 $5 $1 }

     | redomap '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                      { Redomap $3 $5 $7 $9 NoInfo $1 }

     | Certificates redomapT '(' TupleFunAbstr ',' TupleFunAbstr ',' '{' Exps '}' ',' Exps ')'
                      { RedomapT $1 $4 $6 $9 $12 $2 }

     | redomapT '(' TupleFunAbstr ',' TupleFunAbstr ',' '{' Exps '}' ',' Exps ')'
                      { RedomapT [] $3 $5 $8 $11 $1 }

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
                      { DoLoop $3 (tupIdExp $3) $7 $9 $11 $13 $1 }
     | loop '(' TupId '=' Exp ')' '=' for Id '<' Exp do Exp in Exp %prec letprec
                      { DoLoop $3 $5 $9 $11 $13 $15 $1 }

Index : '[' Certificates '|' Exps ']' { (Just $2, $4) }
      | '[' Exps ']'                  { (Nothing, $2) }

Exps : Exp ',' Exps { $1 : $3 }
     | Exp          { [$1] }

Exps2 : Exp ',' Exps2 { $1 : $3 }
      | Exp ',' Exp   { [$1, $3] }


DExps   :: { [(UncheckedExp, UncheckedExp)] }
        : '{' DExps_ { let (es, as) = $2
                       in zip es $ reverse as }
DExps_ :: { ([UncheckedExp], [UncheckedExp]) }
       : Exp '}' ',' Exp         { ([$1], [$4]) }
       | Exp ',' DExps_ ',' Exp { let (es, as) = $3
                                  in ($1:es, $5:as) }

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

TupleFunAbstr : fn TupleType '(' TypeIds ')' '=>' Exp { TupleLambda $4 $7 $2 $1 }

Value : IntValue { $1 }
      | RealValue { $1 }
      | CharValue { $1 }
      | StringValue { $1 }
      | LogValue { $1 }
      | ArrayValue { $1 }
      | TupleValue { $1 }

SignedInt :     intlit { let L _ (INTLIT num) = $1 in num  }
          | '-' intlit { let L _ (INTLIT num) = $2 in -num }

NaturalInt :     intlit { let L _ (INTLIT num) = $1 in num  }

IntValue : intlit        { let L pos (INTLIT num) = $1 in IntVal num }
RealValue : reallit      { let L pos (REALLIT num) = $1 in RealVal num }
CharValue : charlit      { let L pos (CHARLIT char) = $1 in CharVal char }
StringValue : stringlit  { let L pos (STRINGLIT s) = $1 in ArrayVal (arrayFromList $ map CharVal s) $ Elem Char }
LogValue : true          { LogVal True }
        | false          { LogVal False }
CertValue : checked      { Checked }
ArrayValue :  '[' Values ']'
             { case combArrayTypes $ map (toDecl . valueType) $2 of
                 Nothing -> error "Invalid array value"
                 Just ts -> ArrayVal (arrayFromList $2) $ removeNames ts
             }
TupleValue : '(' Values2 ')' { TupVal $2 }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }

Values2 : Value ',' Values { $1 : $3 }

{
combArrayTypes :: [UncheckedType] -> Maybe UncheckedType
combArrayTypes []     = Nothing
combArrayTypes (v:vs) = foldM comb v vs
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

tupIdExp :: UncheckedTupIdent -> UncheckedExp
tupIdExp (Id ident) = Var ident
tupIdExp (TupId pats loc) = TupLit (map tupIdExp pats) loc
tupIdExp (Wildcard _ loc) = error $ "Cannot have wildcard at " ++ locStr loc

parseError :: [L Token] -> Either String a
parseError [] = Left "Parse error: End of file"
parseError (tok:_) = Left $ "Parse error at " ++ locStr (srclocOf tok)
}
