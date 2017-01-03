{
-- | Futhark parser written with Happy.
module Language.Futhark.Parser.Parser
  ( prog
  , expression
  , lambda
  , futharkType
  , anyValue
  , anyValues
  , ParserEnv (..)
  , ParserMonad
  , ReadLineMonad(..)
  , getLinesFromIO
  , getLinesFromTexts
  , getNoLines
  , newParserEnv
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Applicative ((<$>), (<*>))
import Control.Arrow
import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Loc hiding (L) -- Lexer has replacements.
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Language.Futhark.Syntax hiding (ID)
import Language.Futhark.Attributes
import Language.Futhark.Parser.Lexer

}

%name prog Prog
%name futharkType UserType
%name expression Exp
%name lambda FunAbstr
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
      default         { L $$ DEFAULT }
      int             { L $$ INT }
      float           { L $$ FLOAT }
      i8              { L $$ I8 }
      i16             { L $$ I16 }
      i32             { L $$ I32 }
      i64             { L $$ I64 }
      u8              { L $$ U8 }
      u16             { L $$ U16 }
      u32             { L $$ U32 }
      u64             { L $$ U64 }
      bool            { L $$ BOOL }
      f32             { L $$ F32 }
      f64             { L $$ F64 }

      id              { L _ (ID _) }
      'id['           { L _ (INDEXING _) }

      qid             { L _ (QUALID _ _) }
      'qid['          { L _ (QUALINDEXING _ _) }


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
      '!='            { L $$ NEQU }
      '<'             { L $$ LTH }
      '>'             { L $$ GTH }
      '<='            { L $$ LEQ }
      '>='            { L $$ GEQ }
      '**'            { L $$ POW }
      '<<'            { L $$ SHIFTL }
      '>>'            { L $$ SHIFTR }
      '>>>'           { L $$ ZSHIFTR }
      '|'             { L $$ BOR }
      '&'             { L $$ BAND }
      '^'             { L $$ XOR }
      '('             { L $$ LPAR }
      ')'             { L $$ RPAR }
      ')['            { L $$ RPAR_THEN_LBRACKET }
      '{'             { L $$ LCURLY }
      '}'             { L $$ RCURLY }
      '['             { L $$ LBRACKET }
      ']'             { L $$ RBRACKET }
      ','             { L $$ COMMA }
      '_'             { L $$ UNDERSCORE }
      '!'             { L $$ BANG }
      '@'             { L $$ AT }
      '#'             { L $$ HASH }
      fun             { L $$ FUN }
      entry           { L $$ ENTRY }
      fn              { L $$ FN }
      '=>'            { L $$ ARROW }
      '->'            { L $$ TYPE_ARROW }
      ':'             { L $$ COLON }
      for             { L $$ FOR }
      do              { L $$ DO }
      iota            { L $$ IOTA }
      shape           { L $$ SHAPE }
      replicate       { L $$ REPLICATE }
      map             { L $$ MAP }
      reduce          { L $$ REDUCE }
      reduceComm      { L $$ REDUCECOMM }
      reshape         { L $$ RESHAPE }
      rearrange       { L $$ REARRANGE }
      transpose       { L $$ TRANSPOSE }
      rotate          { L $$ ROTATE }
      zipWith         { L $$ ZIPWITH }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      unsafe          { L $$ UNSAFE }
      scan            { L $$ SCAN }
      split           { L $$ SPLIT }
      concat          { L $$ CONCAT }
      filter          { L $$ FILTER }
      partition       { L $$ PARTITION }
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
      include         { L $$ INCLUDE }
      write           { L $$ WRITE }
      type            { L $$ TYPE }
      module          { L $$ MODULE }
      val             { L $$ VAL }

%left bottom
%left ifprec letprec
%left ','
%left '||'
%left '&&'
%left '<=' '>=' '>' '<' '==' '!='
%left '&' '^' '|'

%left '<<' '>>' '>>>'
%left '+' '-'

%left '*' '/' '%' '//' '%%'
%left '**'
%left ':'
%nonassoc '~' '!' signum abs float f32 f64 int i8 i16 i32 i64 unsafe default
%nonassoc '['
%nonassoc Id
%left juxtprec
%left indexprec iota shape copy transpose rotate rearrange split shape reduce map scan filter partition zipWith streamRed streamRedPer streamMap streamMapPer streamSeq
%%


Prog :: { UncheckedProgWithHeaders }
     :   Headers DecStart { ProgWithHeaders $1 $2 }
     |   DecStart { ProgWithHeaders [] $1 }
;


DecStart :: { [DecBase f vn] }
         :  DefaultDec Decs { $2 }
         |  Decs { $1 }
;

Decs :: { [DecBase f vn] }
     : Dec Decs { $1 ++ $2 }
     | Dec { $1 }
;

Dec :: { [DecBase f vn] }
    : Fun           { [ValDec $ FunDec $1] }
    | Const         { [ValDec $ ConstDec $1] }
    | UserTypeAlias { map TypeDec $1 }
    | SigBind       { [SigDec $1 ] }
    | StructBind    { [StructDec $1 ] }
;


Aliases : id ',' Aliases
            { let L loc (ID name) = $1
                in (name,loc) : $3 }
        | id { let L loc (ID name) = $1
               in [(name,loc)] }
;

SigExp :: { SigExpBase f vn }
        : QualName      { let (v, loc) = $1 in SigVar v loc }
        | '{' Specs '}' { SigSpecs $2 $1 }

SigBind :: { SigBindBase f vn }
         : module type id '=' SigExp
          { let L pos (ID name) = $3
            in SigBind name $5 pos }

        -- Shortcut form
        | module type id '{' Specs '}'
          { let L pos (ID name) = $3
            in SigBind name (SigSpecs $5 pos) pos }

ModExp :: { ModExpBase f vn }
        : QualName     { let (v, loc) = $1 in ModVar v loc }
        | '{' Decs '}' { ModDecs $2 $1 }

StructBind :: { StructBindBase f vn }
           : module id '=' ModExp
             { let L pos (ID name) = $2
               in StructBind name Nothing $4 pos }
           | module id ':' SigExp '=' ModExp
             { let L pos (ID name) = $2
               in StructBind name (Just $4) $6 pos }

           -- Shortcut forms
           | module id '{' Decs '}'
             { let L pos (ID name) = $2
               in StructBind name Nothing (ModDecs $4 pos) pos }
           | module id ':' SigExp '{' Decs '}'
             { let L pos (ID name) = $2
               in StructBind name (Just $4) (ModDecs $6 pos) pos }

Specs : Spec Specs { $1 : $2 }
      |            { [] }

Spec :: { SpecBase NoInfo Name }
      : val id ':' SigTypeDecl
        { let L loc (ID name) = $2; (ps, r) = $4
          in ValSpec name ps r loc  }
      | type id '=' UserTypeDecl
        { let L loc (ID name) = $2
          in TypeAbbrSpec (TypeBind name $4 loc) }
      | type id
        { let L loc (ID name) = $2
          in TypeSpec name loc }
;

DefaultDec :: { () }
           :  default '(' SignedType ')' {% defaultIntType (fst $3)  }
           |  default '(' FloatType ')' {% defaultRealType (fst $3) }
           |  default '(' SignedType ',' FloatType ')'
                {% defaultIntType (fst $3) >> defaultRealType (fst $5) }
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
      | '!='    { (NotEqual, $1) }
      | '<'     { (Less, $1) }
      | '<='    { (Leq, $1) }
      | '>'     { (Greater, $1) }
      | '>='    { (Geq, $1) }
      | '&&'    { (LogAnd, $1) }
      | '||'    { (LogOr, $1) }
      | '**'    { (Pow, $1) }
      | '^'     { (Xor, $1) }
      | '&'     { (Band, $1) }
      | '|'     { (Bor, $1) }
      | '>>'    { (ShiftR, $1) }
      | '>>>'   { (ZShiftR, $1) }
      | '<<'    { (ShiftL, $1) }

UnOp :: { (UnOp, SrcLoc) }
     : OpLikeUnOp { $1 }
     | FuncLikeUnOp { $1 }

OpLikeUnOp :: { (UnOp, SrcLoc) }
     : '~' { (Complement, $1) }
     | '!' { (Not, $1) }

FuncLikeUnOp :: { (UnOp, SrcLoc) }
     : signum { (Signum, $1) }
     | abs { (Abs, $1) }
     | SignedType { (ToSigned (fst $1), snd $1) }
     | UnsignedType { (ToUnsigned (fst $1), snd $1) }
     | FloatType { (ToFloat (fst $1), snd $1) }
     | '#' NaturalInt { (TupleProject $2, $1) }

Headers :: { [ProgHeader] }
        : Header Headers { $1 : $2 }
        | Header { [$1] }
;

Header :: { ProgHeader }
Header : include QualName { let (QualName (qs, v), _) = $2 in Include (map nameToString (qs++[v])) }
;

Fun     : fun id Params MaybeAscription '=' Exp
                        { let L pos (ID name) = $2
                          in FunBind (name==defaultEntryPoint) name (fmap declaredType $4) NoInfo $3 $6 pos }
        | entry id Params MaybeAscription '=' Exp
                        { let L pos (ID name) = $2
                          in FunBind True name (fmap declaredType $4) NoInfo $3 $6 pos }
;

Const : val id ':' UserTypeDecl '=' Exp
      { let L loc (ID name) = $2
        in ConstBind name $4 $6 loc }

SigTypeDecl :: { ([TypeDeclBase NoInfo Name], TypeDeclBase NoInfo Name) }
             : UserTypeDecl
               { ([], $1) }
             | UserTypeDecl '->' SigTypeDecl
               { let (ts, t) = $3 in ($1 : ts, t) }

UserTypeDecl :: { TypeDeclBase NoInfo Name }
             : UserType { TypeDecl $1 NoInfo }

UserTypeAlias :: { [TypeBindBase f vn] }
UserTypeAlias : type Aliases '=' UserTypeDecl
                  { let aliases = $2
                      in map (\(name, loc) -> TypeBind name $4 loc) aliases }
;

UserType :: { UncheckedUserType }
         : PrimType      { let (t,loc) = $1 in UserPrim t loc }
         | '*' UserType  { UserUnique $2 $1 }
         | '[' DimDecl ']' UserType { UserArray $4 $2 $1 }
         | '(' ')'           { UserTuple [] $1 }
         | '(' UserTypes ')' { UserTuple $2 $1 }
         | QualName { UserTypeAlias (fst $1) (snd $1) }
;

UserTypes :: { [UncheckedUserType] }
UserTypes : UserType ',' UserTypes { $1 : $3 }
          | UserType               { [$1] }
DimDecl :: { DimDecl Name }
        : id
          { let L _ (ID name) = $1
            in NamedDim name }
        | intlit
          { let L _ (INTLIT n) = $1
            in ConstDim (fromIntegral n) }
        | { AnyDim }

PrimType :: { (PrimType, SrcLoc) }
         : UnsignedType { first Unsigned $1 }
         | SignedType   { first Signed $1 }
         | FloatType    { first FloatType $1 }
         | bool         { (Bool, $1) }

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
          : float { (Float64, $1) }
          | f32  { (Float32, $1) }
          | f64  { (Float64, $1) }

Params :: { [PatternBase NoInfo Name] }
       : Param        { [$1] }
       | Param Params { $1 : $2 }

Param :: { PatternBase NoInfo Name }
Param : VarId                        { Id $1 }
      | '_'                          { Wildcard NoInfo $1 }
      | '(' ')'                      { TuplePattern [] $1 }
      | '(' Pattern ')'              { $2 }
      | '(' Pattern ',' Patterns ')' { TuplePattern ($2:$4) $1 }

QualName :: { (QualName Name, SrcLoc) }
          : qid { let L loc (QUALID qs v) = $1 in (QualName (qs, v), loc) }
          | id  { let L loc (ID v) = $1 in (QualName ([], v), loc) }

Exp  :: { UncheckedExp }
     : if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 NoInfo $1 }

     | LetExp %prec letprec { $1 }

     | iota Atom { Iota $2 $1 }

     | shape Atom { Shape $2 $1 }

     | replicate Atom Atom { Replicate $2 $3 $1 }

     | reshape Atom Atom
                      { Reshape $2 $3 $1 }

     | rearrange '(' NaturalInts ')' Atom
                      { Rearrange $3 $5 $1 }

     | transpose Atom
                      { Transpose $2 $1 }

     | rotate '@' NaturalInt Atom Atom { Rotate $3 $4 $5 $1 }

     | rotate Atom Atom
                      { Rotate 0 $2 $3 $1 }

     | split Atom Atom
                      { Split 0 $2 $3 $1 }

     | split '@' NaturalInt Atom Atom
                      { Split $3 $4 $5 $1 }

     | concat Atoms
                      { Concat 0 (fst $2) (snd $2) $1 }

     | concat '@' NaturalInt Atoms
                      { Concat $3 (fst $4) (snd $4) $1 }


     | reduce FunAbstr Atom Atom
                      { Reduce (commutativity $2) $2 $3 $4 $1 }

     | reduceComm FunAbstr Atom Atom
                      { Reduce Commutative $2 $3 $4 $1 }


     | map FunAbstr Atoms
                      { Map $2 (fst $3:snd $3) $1 }

     | zipWith FunAbstr Atoms
                      { Map $2 (fst $3:snd $3) $1 }

     | scan FunAbstr Atom Atom
                      { Scan $2 $3 $4 $1 }

     | zip Atoms
                      { Zip 0 (fst $2) (snd $2) $1 }

     | zip '@' NaturalInt Atoms
                      { Zip $3 (fst $4) (snd $4) $1 }

     | unzip Atom  { Unzip $2 [] $1 }

     | unsafe Exp     { Unsafe $2 $1 }

     | filter FunAbstr Atom
                      { Filter $2 $3 $1 }

     | partition '(' FunAbstrs ')' Atom
                      { Partition $3 $5 $1 }

     | copy Atom   { Copy $2 $1 }

     | streamMap       FunAbstr Atom
                         { Stream (MapLike InOrder)  $2 $3 $1 }
     | streamMapPer    FunAbstr Atom
                         { Stream (MapLike Disorder) $2 $3 $1 }
     | streamRed       FunAbstr FunAbstr Atom
                         { Stream (RedLike InOrder (commutativity $2) $2) $3 $4 $1 }
     | streamRedPer    FunAbstr FunAbstr Atom
                         { Stream (RedLike Disorder Commutative $2) $3 $4 $1 }
     | streamSeq       FunAbstr Atom Atom
                         { Stream (Sequential $3) $2 $4 $1 }
     | write Atom Atom Atom
                         { Write $2 $3 $4 $1 }

     | Exp '+' Exp    { BinOp Plus $1 $3 NoInfo $2 }
     | Exp '-' Exp    { BinOp Minus $1 $3 NoInfo $2 }
     | Exp '*' Exp    { BinOp Times $1 $3 NoInfo $2 }
     | Exp '/' Exp    { BinOp Divide $1 $3 NoInfo $2 }
     | Exp '%' Exp    { BinOp Mod $1 $3 NoInfo $2 }
     | Exp '//' Exp   { BinOp Quot $1 $3 NoInfo $2 }
     | Exp '%%' Exp   { BinOp Rem $1 $3 NoInfo $2 }
     | '-' Exp %prec juxtprec
       { UnOp Negate $2 NoInfo $1 }
     | UnOp Exp %prec juxtprec
       { UnOp (fst $1) $2 NoInfo (snd $1) }
     | Exp '**' Exp   { BinOp Pow $1 $3 NoInfo $2 }
     | Exp '>>' Exp   { BinOp ShiftR $1 $3 NoInfo $2 }
     | Exp '>>>' Exp  { BinOp ZShiftR $1 $3 NoInfo $2 }
     | Exp '<<' Exp   { BinOp ShiftL $1 $3 NoInfo $2 }
     | Exp '&&' Exp   { BinOp LogAnd $1 $3 NoInfo $2 }
     | Exp '||' Exp   { BinOp LogOr $1 $3 NoInfo $2 }
     | Exp '&' Exp    { BinOp Band $1 $3 NoInfo $2 }
     | Exp '|' Exp    { BinOp Bor $1 $3 NoInfo $2 }
     | Exp '^' Exp    { BinOp Xor $1 $3 NoInfo $2 }

     | Exp '==' Exp   { BinOp Equal $1 $3 NoInfo $2 }
     | Exp '!=' Exp   { BinOp NotEqual $1 $3 NoInfo $2 }
     | Exp '<' Exp    { BinOp Less $1 $3 NoInfo $2 }
     | Exp '<=' Exp   { BinOp Leq  $1 $3 NoInfo $2 }
     | Exp '>' Exp    { BinOp Greater $1 $3 NoInfo $2 }
     | Exp '>=' Exp   { BinOp Geq  $1 $3 NoInfo $2 }
     | Apply
       { let (fname, args, loc) = $1 in Apply fname [ (arg, Observe) | arg <- args ] NoInfo loc }
     | Atom %prec juxtprec { $1 }

Apply : Apply Atom %prec juxtprec
        { let (fname, args, loc) = $1 in (fname, args ++ [$2], loc) }
      | QualName Atom %prec juxtprec
        { (fst $1, [$2], snd $1) }

Atoms :: { (UncheckedExp, [UncheckedExp]) }
Atoms : Atom       { ($1, []) }
      | Atom Atoms { ($1, fst $2 : snd $2) }

Atom :: { UncheckedExp }
Atom : PrimLit        { Literal (fst $1) (snd $1) }
     | stringlit      {% let L pos (STRINGLIT s) = $1 in do
                             s' <- mapM (getIntValue . fromIntegral . ord) s
                             t <- lift $ gets parserIntType
                             return $ ArrayLit (map (flip Literal pos . SignedValue) s') NoInfo pos }
     | empty '(' UserTypeDecl ')' { Empty $3 $1 }
     | '(' Exp ')'                { $2 }
     | '(' Exp ')[' DimIndices ']' { Index $2 $4 $1 }
     | '(' Exp ',' Exps ')'       { TupLit ($2:$4) $1 }
     | '('      ')'               { TupLit [] $1 }
     | '[' Exps ']'   { ArrayLit $2 NoInfo $1 }
     | QualVarSlice  { let (v,slice,loc) = $1
                       in Index (Var v NoInfo loc) slice loc }
     | QualName { Var (fst $1) NoInfo (snd $1) }

LetExp :: { UncheckedExp }
     : let Pattern '=' Exp LetBody
                      { LetPat $2 $4 $5 $1 }

     | let VarSlice '=' Exp LetBody
                      { let (v,slice,loc) = $2; ident = Ident v NoInfo loc
                        in LetWith ident ident slice $4 $5 loc }

     | loop '(' Pattern ')' '=' LoopForm do Exp LetBody
                      {% liftM (\t -> DoLoop $3 t $6 $8 $9 $1)
                               (patternExp $3) }
     | loop '(' Pattern '=' Exp ')' '=' LoopForm do Exp LetBody
                  { DoLoop $3 $5 $8 $10 $11 $1 }

LetBody :: { UncheckedExp }
    : in Exp %prec letprec { $2 }
    | LetExp %prec letprec { $1 }

LoopForm : for VarId '<' Exp
           { For FromUpTo ZeroBound $2 $4 }
         | for Atom '<=' VarId '<' Exp
           { For FromUpTo (ExpBound $2) $4 $6 }
         | for Atom '>' VarId '>=' Exp
           { For FromDownTo (ExpBound $6) $4 $2 }
         | for Atom '>' VarId
           { For FromDownTo ZeroBound $4 $2 }
         | while Exp      { While $2 }

VarSlice :: { (Name, [UncheckedDimIndex], SrcLoc) }
          : 'id[' DimIndices ']'
              { let L loc (INDEXING v) = $1
                in (v, $2, loc) }

QualVarSlice :: { (QualName Name, [UncheckedDimIndex], SrcLoc) }
              : VarSlice
                { let (x, y, z) = $1 in (QualName ([], x), y, z) }
              | 'qid[' DimIndices ']'
                { let L loc (QUALINDEXING qs v) = $1 in (QualName (qs, v), $2, loc) }

DimIndices : DimIndex ',' SomeDimIndices { $1 : $3 }
           | DimIndex                    { [$1] }
           |                             { [] }

SomeDimIndices : DimIndex ',' SomeDimIndices { $1 : $3 }
               | DimIndex                    { [$1] }

DimIndex :: { UncheckedDimIndex }
         : Exp         { DimFix $1 }
         | Exp ':' Exp { DimSlice (Just $1) (Just $3) }
         | Exp ':'     { DimSlice (Just $1) Nothing }
         |     ':' Exp { DimSlice Nothing (Just $2) }
         |     ':'     { DimSlice Nothing Nothing }

Exps : Exp ',' Exps %prec bottom { $1 : $3 }
     | Exp          %prec bottom { [$1] }

VarId : id { let L pos (ID name) = $1 in Ident name NoInfo pos }

Patterns : Pattern ',' Patterns  { $1 : $3 }
         | Pattern               { [$1] }

Pattern : VarId { Id $1 }
      | '_' { Wildcard NoInfo $1 }
      | '(' ')' { TuplePattern [] $1 }
      | '(' Pattern ')' { $2 }
      | '(' Pattern ',' Patterns ')' { TuplePattern ($2:$4) $1 }
      | Pattern ':' UserTypeDecl { PatternAscription $1 $3 }

MaybeAscription :: { Maybe (TypeDeclBase NoInfo Name) }
MaybeAscription : ':' UserTypeDecl { Just $2 }
                |                  { Nothing }

Curry : Curry Atom
        { let (fname, args, loc) = $1 in (fname, args ++ [$2], loc) }
      | QualName Atom %prec indexprec
        { (fst $1, [$2], snd $1) }

FunAbstr :: { UncheckedLambda }
         : '(' fn Params MaybeAscription '=>' Exp ')'
           { AnonymFun $3 $6 $4 NoInfo $1 }
         | QualName
           { CurryFun (fst $1) [] NoInfo (snd $1) }
         | '(' Curry ')'
           { let (fname, args, loc) = $2 in CurryFun fname args NoInfo loc }
           -- Minus is handed explicitly here because I could not
           -- figure out how to resolve the ambiguity with negation.
         | '(' '-' Exp ')'
           { CurryBinOpRight Minus $3 NoInfo NoInfo $1 }
         | '(' '-' ')'
           { BinOpFun Minus NoInfo NoInfo NoInfo $1 }
         | '(' Exp '-' ')'
           { CurryBinOpLeft Minus $2 NoInfo NoInfo (srclocOf $1) }
         | '(' BinOp Exp ')'
           { CurryBinOpRight (fst $2) $3 NoInfo NoInfo $1 }
         | '(' Exp BinOp ')'
           { CurryBinOpLeft (fst $3) $2 NoInfo NoInfo $1 }
         | '(' BinOp ')'
           { BinOpFun (fst $2) NoInfo NoInfo NoInfo $1 }
         | '(' OpLikeUnOp ')'
           { UnOpFun (fst $2) NoInfo NoInfo $1 }
         | FuncLikeUnOp
           { UnOpFun (fst $1) NoInfo NoInfo (snd $1) }

FunAbstrs : FunAbstr ',' FunAbstrs { $1 : $3 }
          | FunAbstr               { [$1] }

Value : IntValue { $1 }
      | FloatValue { $1 }
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

StringValue : stringlit  {% let L pos (STRINGLIT s) = $1 in do
                             s' <- mapM (getIntValue . fromIntegral . ord) s
                             t <- lift $ gets parserIntType
                             return $ ArrayValue (arrayFromList $ map (PrimValue . SignedValue) s') $ Prim $ Signed t }
BoolValue : true           { PrimValue $ BoolValue True }
          | false          { PrimValue $ BoolValue False }

SignedLit :: { (IntValue, SrcLoc) }
          : i8lit   { let L pos (I8LIT num)  = $1 in (Int8Value num, pos) }
          | i16lit  { let L pos (I16LIT num) = $1 in (Int16Value num, pos) }
          | i32lit  { let L pos (I32LIT num) = $1 in (Int32Value num, pos) }
          | i64lit  { let L pos (I64LIT num) = $1 in (Int64Value num, pos) }
          | intlit  {% let L pos (INTLIT num) = $1 in do num' <- getIntValue num; return (num', pos) }
          | charlit {% let L pos (CHARLIT char) = $1 in do
                       num <- getIntValue $ fromIntegral $ ord char
                       return (num, pos) }

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

ArrayValue :  '[' Value ']'
             {% return $ ArrayValue (arrayFromList [$2]) $ toStruct $ valueType $2
             }
           |  '[' Value ',' Values ']'
             {% case combArrayTypes (valueType $2) $ map valueType $4 of
                  Nothing -> throwError "Invalid array value"
                  Just ts -> return $ ArrayValue (arrayFromList $ $2:$4) ts
             }
           | empty '(' PrimType ')'
             { ArrayValue (listArray (0,-1) []) (Prim (fst $3)) }
TupleValue : '(' Values ')' { TupValue $2 }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }
       |                  { [] }

{

data ParserEnv = ParserEnv {
                 parserFile :: FilePath
               , parserIntType :: IntType
               , parserRealType :: FloatType
               , parserRealFun :: Double -> FloatValue
               }

newParserEnv :: FilePath -> IntType -> FloatType -> ParserEnv
newParserEnv path intType realType =
  let s = ParserEnv path intType realType Float64Value
  in modParserEnv s realType

modParserEnv :: ParserEnv -> FloatType -> ParserEnv
modParserEnv s realType =
  case realType of
    Float32 -> s {
        parserRealType = Float32,
        parserRealFun = float32RealFun
      }
    Float64 -> s {
        parserRealType = Float64,
        parserRealFun = float64RealFun
      }
  where

    float32RealFun x =
      let (m,n) = decodeFloat x
      in Float32Value $ encodeFloat m n
    float64RealFun = Float64Value

type ParserMonad a =
  ExceptT String (
    StateT ParserEnv (
       StateT [L Token] ReadLineMonad)) a

data ReadLineMonad a = Value a
                     | GetLine (T.Text -> ReadLineMonad a)

readLineFromMonad :: ReadLineMonad T.Text
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
  s <- T.getLine
  getLinesFromIO $ f s

getLinesFromTexts :: [T.Text] -> ReadLineMonad a -> Either String a
getLinesFromTexts _ (Value x) = Right x
getLinesFromTexts (x : xs) (GetLine f) = getLinesFromTexts xs $ f x
getLinesFromTexts [] (GetLine _) = Left "Ran out of input"

getNoLines :: ReadLineMonad a -> Either String a
getNoLines (Value x) = Right x
getNoLines (GetLine _) = Left "Unexpected end of input"

combArrayTypes :: TypeBase Rank ()
               -> [TypeBase Rank ()]
               -> Maybe (TypeBase Rank ())
combArrayTypes t ts = foldM comb t ts
  where comb x y
          | x == y    = Just x
          | otherwise = Nothing

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

patternExp :: UncheckedPattern -> ParserMonad UncheckedExp
patternExp (Id ident) = return $ Var (QualName ([],identName ident)) NoInfo $ srclocOf ident
patternExp (TuplePattern pats loc) = TupLit <$> (mapM patternExp pats) <*> return loc
patternExp (Wildcard _ loc) = throwError $ "Cannot have wildcard at " ++ locStr loc

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

defaultIntType :: IntType -> ParserMonad ()
defaultIntType intType = do
  s <- lift $ get
  lift $ put $ s { parserIntType = intType }

defaultRealType :: FloatType -> ParserMonad ()
defaultRealType realType = do
  s <- lift $ get
  lift $ put $ modParserEnv s realType

getFilename :: ParserMonad FilePath
getFilename = lift $ gets parserFile

getIntValue :: Int64 -> ParserMonad IntValue
getIntValue x = do
  t <- lift $ gets parserIntType
  return $ (getIntFun t) (toInteger x)

getIntFun :: IntType -> (Integer -> IntValue)
getIntFun Int8  = Int8Value . fromInteger
getIntFun Int16 = Int16Value . fromInteger
getIntFun Int32 = Int32Value . fromInteger
getIntFun Int64 = Int64Value . fromInteger

getRealValue :: Double -> ParserMonad FloatValue
getRealValue x = do f <- lift $ gets parserRealFun
                    return $ f x

intNegate :: IntValue -> IntValue
intNegate (Int8Value v) = Int8Value (-v)
intNegate (Int16Value v) = Int16Value (-v)
intNegate (Int32Value v) = Int32Value (-v)
intNegate (Int64Value v) = Int64Value (-v)

floatNegate :: FloatValue -> FloatValue
floatNegate (Float32Value v) = Float32Value (-v)
floatNegate (Float64Value v) = Float64Value (-v)

readLine :: ParserMonad T.Text
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
          ts' <- scanTokens <$> getFilename <*> readLine
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
