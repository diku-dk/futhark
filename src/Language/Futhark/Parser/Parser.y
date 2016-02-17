{
-- | Futhark parser written with Happy.
module Language.Futhark.Parser.Parser
  ( prog
  , expression
  , lambda
  , futharktype
  , anyValue
  , anyValues
  , ParserEnv (..)
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
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Applicative ((<$>), (<*>))
import Data.Array
import Data.Maybe (fromMaybe)
import Data.Loc hiding (L) -- Lexer has replacements.
import qualified Data.HashMap.Lazy as HM

import Language.Futhark.Syntax hiding (ID)
import Language.Futhark.Attributes hiding (arrayValue)
import Language.Futhark.Parser.Lexer

}

%name prog Prog
%name expression Exp
%name lambda FunAbstr
%name futharktype Type
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
      i8              { L $$ I8 }
      i16             { L $$ I16 }
      i32             { L $$ I32 }
      i64             { L $$ I64 }
      u8              { L $$ U8 }
      u16             { L $$ U16 }
      u32             { L $$ U32 }
      u64             { L $$ U64 }
      bool            { L $$ BOOL }
      char            { L $$ CHAR }
      real            { L $$ REAL }
      f32             { L $$ F32 }
      f64             { L $$ F64 }

      id              { L _ (ID _) }

      intlit          { L _ (INTLIT _) }
      i8lit           { L _ (I8LIT _) }
      i16lit          { L _ (I16LIT _) }
      i32lit          { L _ (I32LIT _) }
      i64lit          { L _ (I64LIT _) }
      u8lit           { L _ (U8LIT _) }
      u16lit          { L _ (U16LIT _) }
      u32lit          { L _ (U32LIT _) }
      u64lit          { L _ (U64LIT _) }
      reallit         { L _ (REALLIT _) }
      f32lit          { L _ (F32LIT _) }
      f64lit          { L _ (F64LIT _) }
      stringlit       { L _ (STRINGLIT _) }
      charlit         { L _ (CHARLIT _) }

      '+'             { L $$ PLUS }
      '-'             { L $$ MINUS }
      '*'             { L $$ TIMES }
      '/'             { L $$ DIVIDE }
      '%'             { L $$ MOD }
      '//'            { L $$ QUOT }
      '%%'            { L $$ REM }
      '='             { L $$ EQU }
      '=='            { L $$ EQU2 }
      '<'             { L $$ LTH }
      '>'             { L $$ GTH }
      '<='            { L $$ LEQ }
      '>='            { L $$ GEQ }
      pow             { L $$ POW }
      '<<'            { L $$ SHIFTL }
      '>>'            { L $$ SHIFTR }
      '>>>'           { L $$ ZSHIFTR }
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
      '!'             { L $$ BANG }
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
      reduceComm      { L $$ REDUCECOMM }
      reshape         { L $$ RESHAPE }
      rearrange       { L $$ REARRANGE }
      stripe          { L $$ STRIPE }
      unstripe          { L $$ UNSTRIPE }
      transpose       { L $$ TRANSPOSE }
      zipWith         { L $$ ZIPWITH }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      scan            { L $$ SCAN }
      split           { L $$ SPLIT }
      concat          { L $$ CONCAT }
      filter          { L $$ FILTER }
      partition       { L $$ PARTITION }
      redomap         { L $$ REDOMAP }
      true            { L $$ TRUE }
      false           { L $$ FALSE }
      '~'             { L $$ TILDE }
      abs             { L $$ ABS }
      signum          { L $$ SIGNUM }
      '&&'            { L $$ AND }
      '||'            { L $$ OR }
      empty           { L $$ EMPTY }
      copy            { L $$ COPY }
      while           { L $$ WHILE }
      streamMap       { L $$ STREAM_MAP }
      streamMapPer    { L $$ STREAM_MAPPER }
      streamRed       { L $$ STREAM_RED }
      streamRedPer    { L $$ STREAM_REDPER }
      streamSeq       { L $$ STREAM_SEQ }

%nonassoc ifprec letprec
%left '||'
%left '&&'
%left '&' '^' '|'
%left '<=' '>=' '>' '<' '=='

%left '<<' '>>' '>>>'
%left '+' '-'

%left '*' '/' '%' '//' '%%'
%left pow
%nonassoc '~' '!' signum abs real f32 f64 int i8 i16 i32 i64

%%

Prog :: { UncheckedProg }
     :   FunDecs { Prog $1 }
;

-- Note that this production does not include Minus.
BinOp :: { (BinOp, SrcLoc) }
      : '+'     { (Plus, $1) }
      | '*'     { (Times, $1) }
      | '/'     { (Divide, $1) }
      | '%'     { (Mod, $1) }
      | '//'    { (Quot, $1) }
      | '%%'    { (Rem, $1) }
      | '=='    { (Equal, $1) }
      | '<'     { (Less, $1) }
      | '<='    { (Leq, $1) }
      | '>'     { (Greater, $1) }
      | '>='    { (Geq, $1) }
      | '&&'    { (LogAnd, $1) }
      | '||'    { (LogOr, $1) }
      | pow     { (Pow, $1) }
      | '^'     { (Xor, $1) }
      | '&'     { (Band, $1) }
      | '|'     { (Bor, $1) }
      | '>>'    { (ShiftR, $1) }
      | '>>>'   { (ZShiftR, $1) }
      | '<<'    { (ShiftL, $1) }

UnOp :: { (UnOp, SrcLoc) }
     : '~' { (Complement, $1) }
     | '!' { (Not, $1) }
     | abs { (Abs, $1) }
     | signum { (Signum, $1) }
     | SignedType { (ToSigned (fst $1), snd $1) }
     | UnsignedType { (ToUnsigned (fst $1), snd $1) }
     | FloatType { (ToFloat (fst $1), snd $1) }

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
        : PrimType     { Prim $1 }
        | ArrayType     { Array $1 }
        | '{' Types '}' { Tuple $2 }
;

DimDecl :: { DimDecl Name }
        : ',' id
          { let L _ (ID name) = $2
            in NamedDim name }
        | ',' intlit
          { let L _ (INTLIT n) = $2
            in ConstDim (fromIntegral n) }
        | { AnyDim }

ArrayType :: { UncheckedArrayType }
          : Uniqueness '[' PrimArrayRowType DimDecl ']'
            { let (ds, et) = $3
              in PrimArray et (ShapeDecl ($4:ds)) $1 NoInfo }
          | Uniqueness '[' TupleArrayRowType DimDecl ']'
            { let (ds, et) = $3
              in TupleArray et (ShapeDecl ($4:ds)) $1 }

PrimArrayRowType : PrimType
                    { ([], $1) }
                  | '[' PrimArrayRowType DimDecl ']'
                    { let (ds, et) = $2
                      in ($3:ds, et) }

TupleArrayRowType : '{' TupleArrayElemTypes '}'
                     { ([], $2) }
                  | '[' TupleArrayRowType DimDecl ']'
                     { let (ds, et) = $2
                       in ($3:ds, et) }

TupleArrayElemTypes : { [] }
                    | TupleArrayElemType
                      { [$1] }
                    | TupleArrayElemType ',' TupleArrayElemTypes
                      { $1 : $3 }

TupleArrayElemType : PrimType                   { PrimArrayElem $1 NoInfo }
                   | ArrayType                   { ArrayArrayElem $1 }
                   | '{' TupleArrayElemTypes '}' { TupleArrayElem $2 }

PrimType : UnsignedType { Unsigned (fst $1) }
         | SignedType   { Signed (fst $1) }
         | FloatType    { FloatType (fst $1) }
         | bool         { Bool }
         | char         { Char }

SignedType :: { (IntType, SrcLoc) }
           : int { (Int32, $1) }
           | i8  { (Int8, $1) }
           | i16 { (Int16, $1) }
           | i32 { (Int32, $1) }
           | i64 { (Int64, $1) }

UnsignedType :: { (IntType, SrcLoc) }
             : u8  { (Int8, $1) }
             | u16 { (Int16, $1) }
             | u32 { (Int32, $1) }
             | u64 { (Int64, $1) }

FloatType :: { (FloatType, SrcLoc) }
          : real {% do t <- getRealType; return (t, $1) }
          | f32  { (Float32, $1) }
          | f64  { (Float64, $1) }

Types : Type ',' Types { $1 : $3 }
      | Type           { [$1] }
      |                { [] }
;

TypeIds : Type id ',' TypeIds
                        { let L pos (ID name) = $2 in Ident name $1 pos : $4 }
        | Type id       { let L pos (ID name) = $2 in [Ident name $1 pos] }
;

Exp  :: { UncheckedExp }
     : PrimLit        { Literal (PrimValue (fst $1)) (snd $1) }
     | stringlit      { let L pos (STRINGLIT s) = $1
                        in Literal (ArrayValue (arrayFromList $ map (PrimValue . CharValue) s) $ Prim Char) pos }
     | Id %prec letprec { Var $1 }
     | empty '(' Type ')' { Literal (emptyArray $3) $1 }
     | '[' Exps ']'   { ArrayLit $2 NoInfo $1 }
     | TupleExp       { let (exps, pos) = $1 in TupLit exps pos }

     | Exp '+' Exp    { BinOp Plus $1 $3 NoInfo $2 }
     | Exp '-' Exp    { BinOp Minus $1 $3 NoInfo $2 }
     | Exp '*' Exp    { BinOp Times $1 $3 NoInfo $2 }
     | Exp '/' Exp    { BinOp Divide $1 $3 NoInfo $2 }
     | Exp '%' Exp    { BinOp Mod $1 $3 NoInfo $2 }
     | Exp '//' Exp   { BinOp Quot $1 $3 NoInfo $2 }
     | Exp '%%' Exp   { BinOp Rem $1 $3 NoInfo $2 }
     | '-' Exp %prec '~' { UnOp Negate $2 $1 }
     | '!' Exp        { UnOp Not $2 $1 }
     | '~' Exp        { UnOp Complement $2 $1 }
     | abs Exp        { UnOp Abs $2 $1 }
     | signum Exp     { UnOp Signum $2 $1 }
     | SignedType '(' Exp ')' { UnOp (ToSigned (fst $1)) $3 (snd $1) }
     | UnsignedType '(' Exp ')' { UnOp (ToUnsigned (fst $1)) $3 (snd $1) }
     | FloatType '(' Exp ')' { UnOp (ToFloat (fst $1)) $3 (snd $1) }
     | Exp pow Exp    { BinOp Pow $1 $3 NoInfo $2 }
     | Exp '>>' Exp   { BinOp ShiftR $1 $3 NoInfo $2 }
     | Exp '>>>' Exp  { BinOp ZShiftR $1 $3 NoInfo $2 }
     | Exp '<<' Exp   { BinOp ShiftL $1 $3 NoInfo $2 }
     | Exp '&&' Exp   { BinOp LogAnd $1 $3 NoInfo $2 }
     | Exp '||' Exp   { BinOp LogOr $1 $3 NoInfo $2 }
     | Exp '&' Exp    { BinOp Band $1 $3 NoInfo $2 }
     | Exp '|' Exp    { BinOp Bor $1 $3 NoInfo $2 }
     | Exp '^' Exp    { BinOp Xor $1 $3 NoInfo $2 }

     | Exp '==' Exp   { BinOp Equal $1 $3 NoInfo $2 }
     | Exp '<' Exp    { BinOp Less $1 $3 NoInfo $2 }
     | Exp '<=' Exp   { BinOp Leq  $1 $3 NoInfo $2 }
     | Exp '>' Exp    { BinOp Greater $1 $3 NoInfo $2 }
     | Exp '>=' Exp   { BinOp Geq  $1 $3 NoInfo $2 }

     | if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 NoInfo $1 }

     | id '(' Exps ')'
                      {% let L pos (ID name) = $1 in do{
                            name' <- getFunName name;
                            return (Apply name' [ (arg, Observe) | arg <- $3 ] NoInfo pos)}
                      }
     | id '(' ')'     {% let L pos (ID name) = $1
                        in do { name' <- getFunName name; return (Apply name' [] NoInfo pos) } }

     | iota '(' Exp ')' { Iota $3 $1 }

     | size '(' NaturalInt ',' Exp ')'
                      { Size $3 $5 $1 }

     | replicate '(' Exp ',' Exp ')' { Replicate $3 $5 $1 }

     | reshape '(' '(' Exps ')' ',' Exp ')'
                      { Reshape $4 $7 $1 }

     | rearrange '(' '(' NaturalInts ')' ',' Exp ')'
                      { Rearrange $4 $7 $1 }

     | stripe '(' Exp ',' Exp ')'
                      { Stripe $3 $5 $1 }
     | unstripe '(' Exp ',' Exp ')'
                      { Unstripe $3 $5 $1 }

     | transpose '(' Exp ')' { Transpose $3 $1 }

     | split '(' '(' Exps ')' ',' Exp ')'
                      { Split $4 $7 $1 }

     | concat '(' Exp ',' Exps ')'
                      { Concat $3 $5 $1 }

     | reduce '(' FunAbstr ',' Exp ',' Exp ')'
                      { Reduce (commutativity $3) $3 $5 $7 $1 }

     | reduceComm '(' FunAbstr ',' Exp ',' Exp ')'
                      { Reduce Commutative $3 $5 $7 $1 }


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

     | partition '(' FunAbstrsThenExp ')'
                      { Partition (fst $3) (snd $3) $1 }

     | redomap '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                      { Redomap (commutativity $3) $3 $5 $7 $9 $1 }

     | zipWith '(' FunAbstr ',' Exps2 ')'
                      { Map $3 (Zip (map (\x -> (x, NoInfo)) $5) $1) $1 }

     | copy '(' Exp ')' { Copy $3 $1 }

     | '(' Exp ')' { $2 }

     | let Id '=' Exp in Exp %prec letprec
                      { LetPat (Id $2) $4 $6 $1 }

     | let '_' '=' Exp in Exp %prec letprec
                      { LetPat (Wildcard NoInfo $2) $4 $6 $1 }

     | let '{' Patterns '}' '=' Exp in Exp %prec letprec
                      { LetPat (TuplePattern $3 $1) $6 $8 $1 }

     | let Id '=' Id with Index '<-' Exp in Exp %prec letprec
                      { LetWith $2 $4 $6 $8 $10 $1 }

     | let Id Index '=' Exp in Exp %prec letprec
                      { LetWith $2 $2 $3 $5 $7 $1 }

     | let Id '[' ']' '=' Exp in Exp %prec letprec
                      { LetWith $2 $2 [] $6 $8 $1 }

     | Id Index
                      { Index $1 $2 (srclocOf $1) }

     | loop '(' Pattern ')' '=' LoopForm do Exp in Exp %prec letprec
                      {% liftM (\t -> DoLoop $3 t $6 $8 $10 $1)
                               (patternExp $3) }
     | loop '(' Pattern '=' Exp ')' '=' LoopForm do Exp in Exp %prec letprec
                      { DoLoop $3 $5 $8 $10 $12 $1 }

     | streamMap       '(' FunAbstr ',' Exp ')'
                         { Stream (MapLike InOrder)  $3 $5 $1 }
     | streamMapPer    '(' FunAbstr ',' Exp ')'
                         { Stream (MapLike Disorder) $3 $5 $1 }
     | streamRed       '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                         { Stream (RedLike InOrder (commutativity $3) $3 $7) $5 $9 $1 }
     | streamRedPer    '(' FunAbstr ',' FunAbstr ',' Exp ',' Exp ')'
                         { Stream (RedLike Disorder (commutativity $3) $3 $7) $5 $9 $1 }
     | streamSeq       '(' FunAbstr ',' Exp ',' Exp ')'
                         { Stream (Sequential $5) $3 $7 $1 }

LoopForm : for Id '<' Exp
           { For FromUpTo (zeroExpression (srclocOf $1)) $2 $4 }
         | for Exp '<=' Id '<' Exp
           { For FromUpTo $2 $4 $6 }
         | for Exp '>' Id '>=' Exp
           { For FromDownTo $6 $4 $2 }
         | for Exp '>' Id
           { For FromDownTo (zeroExpression (srclocOf $1)) $4 $2 }
         | while Exp      { While $2 }

Index : '[' Exps ']'                  { $2 }

Exps : Exp ',' Exps { $1 : $3 }
     | Exp          { [$1] }

Exps2 : Exp ',' Exps2 { $1 : $3 }
      | Exp ',' Exp   { [$1, $3] }


TupleExp : '{' Exps '}' { ($2, $1) }
         | '{'      '}' { ([], $1) }

Id : id { let L loc (ID name) = $1 in Ident name NoInfo loc }

Patterns : Pattern ',' Patterns  { $1 : $3 }
         | Pattern               { [$1] }
         |                       { [] }
;

Pattern : id { let L pos (ID name) = $1 in Id $ Ident name NoInfo pos }
      | '_' { Wildcard NoInfo $1 }
      | '{' Patterns '}' { TuplePattern $2 $1 }

FunAbstr :: { UncheckedLambda }
         : fn Type '(' TypeIds ')' '=>' Exp
           { AnonymFun $4 $7 $2 $1 }
         | id '(' Exps ')'
           {% let L pos (ID name) = $1 in do {
             name' <- getFunName name; return (CurryFun name' $3 NoInfo pos) } }
         | id '(' ')'
           {% let L pos (ID name) = $1 in do {
             name' <- getFunName name; return (CurryFun name' [] NoInfo pos) } }
         | id
           {% let L pos (ID name) = $1 in do {
             name' <- getFunName name; return (CurryFun name' [] NoInfo pos ) } }
           -- Minus is handed explicitly here because I could figure
           -- out how to resolve the ambiguity with negation.
         | '-' Exp
           { CurryBinOpRight Minus $2 NoInfo NoInfo $1 }
         | '-'
           { BinOpFun Minus NoInfo NoInfo NoInfo $1 }
         | Exp '-'
           { CurryBinOpLeft Minus $1 NoInfo NoInfo (srclocOf $1) }
         | BinOp Exp
           { CurryBinOpRight (fst $1) $2 NoInfo NoInfo (snd $1) }
         | Exp BinOp
           { CurryBinOpLeft (fst $2) $1 NoInfo NoInfo (snd $2) }
         | BinOp
           { BinOpFun (fst $1) NoInfo NoInfo NoInfo (snd $1) }
         | UnOp
           { UnOpFun (fst $1) NoInfo NoInfo (snd $1) }

FunAbstrsThenExp : FunAbstr ',' Exp              { ([$1], $3) }
                 | FunAbstr ',' FunAbstrsThenExp { ($1 : fst $3, snd $3) }

Value : IntValue { $1 }
      | FloatValue { $1 }
      | CharValue { $1 }
      | StringValue { $1 }
      | BoolValue { $1 }
      | ArrayValue { $1 }
      | TupleValue { $1 }

CatValues : Value CatValues { $1 : $2 }
          |                 { [] }

NaturalInt :: { Int }
           :  intlit { let L _ (INTLIT num) = $1 in fromIntegral num  }

NaturalInts :: { [Int] }
           : intlit                 { let L _ (INTLIT num) = $1 in [fromIntegral num] }
           | intlit ',' NaturalInts { let L _ (INTLIT num) = $1 in fromIntegral num : $3  }

IntValue :: { Value }
         : SignedLit { PrimValue (SignedValue (fst $1)) }
         | '-' SignedLit { PrimValue (SignedValue (intNegate (fst $2))) }
         | UnsignedLit { PrimValue (UnsignedValue (fst $1)) }

FloatValue :: { Value }
         : FloatLit { PrimValue (FloatValue (fst $1)) }
         | '-' FloatLit { PrimValue (FloatValue (floatNegate (fst $2))) }

CharValue : charlit      { let L pos (CHARLIT char) = $1 in PrimValue $ CharValue char }
StringValue : stringlit  { let L pos (STRINGLIT s) = $1 in ArrayValue (arrayFromList $ map (PrimValue . CharValue) s) $ Prim Char }
BoolValue : true          { PrimValue $ BoolValue True }
         | false          { PrimValue $ BoolValue False }

SignedLit :: { (IntValue, SrcLoc) }
          : i8lit  { let L pos (I8LIT num)  = $1 in (Int8Value num, pos) }
          | i16lit { let L pos (I16LIT num) = $1 in (Int16Value num, pos) }
          | i32lit { let L pos (I32LIT num) = $1 in (Int32Value num, pos) }
          | i64lit { let L pos (I64LIT num) = $1 in (Int64Value num, pos) }
          | intlit { let L pos (INTLIT num) = $1 in (Int32Value num, pos) }

UnsignedLit :: { (IntValue, SrcLoc) }
            : u8lit  { let L pos (U8LIT num)  = $1 in (Int8Value num, pos) }
            | u16lit { let L pos (U16LIT num) = $1 in (Int16Value num, pos) }
            | u32lit { let L pos (U32LIT num) = $1 in (Int32Value num, pos) }
            | u64lit { let L pos (U64LIT num) = $1 in (Int64Value num, pos) }

FloatLit :: { (FloatValue, SrcLoc) }
         : f32lit { let L pos (F32LIT num) = $1 in (Float32Value num, pos) }
         | f64lit { let L pos (F64LIT num) = $1 in (Float64Value num, pos) }
         | reallit {% let L pos (REALLIT num) = $1 in do num' <- getRealValue num; return (num', pos) }

PrimLit :: { (PrimValue, SrcLoc) }
        : SignedLit { let (x,loc) = $1 in (SignedValue x, loc) }
        | UnsignedLit { let (x,loc) = $1 in (UnsignedValue x, loc) }
        | FloatLit { let (x,loc) = $1 in (FloatValue x, loc) }

        | true   { (BoolValue True, $1) }
        | false  { (BoolValue False, $1) }

        | charlit { let L pos (CHARLIT char) = $1 in (CharValue char, pos) }

ArrayValue :  '[' Value ']'
             {% return $ ArrayValue (arrayFromList [$2]) $ removeNames $ toDecl $ valueType $2
             }
           |  '[' Value ',' Values ']'
             {% case combArrayTypes (valueType $2) $ map valueType $4 of
                  Nothing -> throwError "Invalid array value"
                  Just ts -> return $ ArrayValue (arrayFromList $ $2:$4) $ removeNames ts
             }
           | empty '(' Type ')'
             { emptyArray $3 }
TupleValue : '{' Values '}' { TupValue $2 }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }
       |                  { [] }

{

data ParserEnv = ParserEnv {
                 parserFile :: FilePath
               , parserRealType :: FloatType
               , parserRealFun :: Double -> FloatValue
               , parserFunMap :: HM.HashMap Name Name
               }

type ParserMonad a =
  ExceptT String (
    ReaderT ParserEnv (
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

combArrayTypes :: TypeBase Rank NoInfo Name
               -> [TypeBase Rank NoInfo Name]
               -> Maybe (TypeBase Rank NoInfo Name)
combArrayTypes t ts = foldM comb t ts
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

patternExp :: UncheckedPattern -> ParserMonad UncheckedExp
patternExp (Id ident) = return $ Var ident
patternExp (TuplePattern pats loc) = TupLit <$> (mapM patternExp pats) <*> return loc
patternExp (Wildcard _ loc) = throwError $ "Cannot have wildcard at " ++ locStr loc

zeroExpression :: SrcLoc -> UncheckedExp
zeroExpression = Literal $ PrimValue $ SignedValue $ Int32Value 0

commutativity :: LambdaBase ty vn -> Commutativity
commutativity (BinOpFun binop _ _ _ _)
  | commutative binop = Commutative
commutativity _ = Noncommutative

eof :: L Token
eof = L (SrcLoc $ Loc (Pos "" 0 0 0) (Pos "" 0 0 0)) EOF

getTokens :: ParserMonad [L Token]
getTokens = lift $ lift get

putTokens :: [L Token] -> ParserMonad ()
putTokens ts = lift $ lift $ put ts

getFilename :: ParserMonad FilePath
getFilename = lift $ asks parserFile

getRealType :: ParserMonad FloatType
getRealType = lift $ asks parserRealType

getRealValue :: Double -> ParserMonad FloatValue
getRealValue x = do f <- lift $ asks parserRealFun
                    return $ f x

getFunName :: Name -> ParserMonad Name
getFunName name = do substs <- lift $ asks parserFunMap
                     return $ HM.lookupDefault name name substs

intNegate :: IntValue -> IntValue
intNegate (Int8Value v) = Int8Value (-v)
intNegate (Int16Value v) = Int16Value (-v)
intNegate (Int32Value v) = Int32Value (-v)
intNegate (Int64Value v) = Int64Value (-v)

floatNegate :: FloatValue -> FloatValue
floatNegate (Float32Value v) = Float32Value (-v)
floatNegate (Float64Value v) = Float64Value (-v)

readLine :: ParserMonad String
readLine = lift $ lift $ lift readLineFromMonad

lexer :: (L Token -> ParserMonad a) -> ParserMonad a
lexer cont = do
  ts <- getTokens
  case ts of
    [] -> do
      ended <- lift $ runExceptT $ cont eof
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
