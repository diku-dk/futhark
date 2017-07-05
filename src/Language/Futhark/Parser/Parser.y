{
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections #-}
-- | Futhark parser written with Happy.
module Language.Futhark.Parser.Parser
  ( prog
  , expression
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

  , parse
  , ParseError(..)
  , parseExpIncr
  , parseExpIncrIO
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
import qualified Data.Map.Strict as M
import Data.Monoid

import Language.Futhark.Syntax hiding (ID)
import Language.Futhark.Attributes
import Language.Futhark.Pretty
import Language.Futhark.Parser.Lexer

}

%name prog Prog
%name futharkType TypeExp
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

      id              { L _ (ID _) }
      'id['           { L _ (INDEXING _) }

      qid             { L _ (QUALID _ _) }
      'qid['          { L _ (QUALINDEXING _ _) }

      'qid.('         { L _ (QUALPAREN _ _) }

      unop            { L _ (UNOP _) }
      qunop           { L _ (QUALUNOP _ _) }

      declit          { L _ (DECLIT _) }
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

      '#'             { L $$ HASH }
      '='             { L $$ EQU }

      '*'             { L $$ ASTERISK }
      '-'             { L $$ NEGATE }
      '<'             { L $$ LTH }
      '>'             { L $$ GTH }
      '<='            { L $$ LEQ }
      '>='            { L $$ GEQ }

      '+...'          { L _ (SYMBOL Plus _ _) }
      '-...'          { L _ (SYMBOL Minus _ _) }
      '*...'          { L _ (SYMBOL Times _ _) }
      '/...'          { L _ (SYMBOL Divide _ _) }
      '%...'          { L _ (SYMBOL Mod _ _) }
      '//...'         { L _ (SYMBOL Quot _ _) }
      '%%...'         { L _ (SYMBOL Rem _ _) }
      '==...'         { L _ (SYMBOL Equal _ _) }
      '!=...'         { L _ (SYMBOL NotEqual _ _) }
      '<...'          { L _ (SYMBOL Less _ _) }
      '>...'          { L _ (SYMBOL Greater _ _) }
      '<=...'         { L _ (SYMBOL Leq _ _) }
      '>=...'         { L _ (SYMBOL Geq _ _) }
      '**...'         { L _ (SYMBOL Pow _ _) }
      '<<...'         { L _ (SYMBOL ShiftL _ _) }
      '>>...'         { L _ (SYMBOL ShiftR _ _) }
      '>>>...'        { L _ (SYMBOL ZShiftR _ _) }
      '|...'          { L _ (SYMBOL Bor _ _) }
      '&...'          { L _ (SYMBOL Band _ _) }
      '^...'          { L _ (SYMBOL Xor _ _) }
      '||...'         { L _ (SYMBOL LogOr _ _) }
      '&&...'         { L _ (SYMBOL LogAnd _ _) }

      '('             { L $$ LPAR }
      ')'             { L $$ RPAR }
      ')['            { L $$ RPAR_THEN_LBRACKET }
      '{'             { L $$ LCURLY }
      '}'             { L $$ RCURLY }
      '['             { L $$ LBRACKET }
      ']'             { L $$ RBRACKET }
      ','             { L $$ COMMA }
      '_'             { L $$ UNDERSCORE }
      '@'             { L $$ AT }
      '\\'            { L $$ BACKSLASH }
      '\''            { L $$ APOSTROPHE }
      entry           { L $$ ENTRY }
      '->'            { L $$ RIGHT_ARROW }
      '<-'            { L $$ LEFT_ARROW }
      ':'             { L $$ COLON }
      for             { L $$ FOR }
      do              { L $$ DO }
      with            { L $$ WITH }
      map             { L $$ MAP }
      reduce          { L $$ REDUCE }
      reduceComm      { L $$ REDUCECOMM }
      reshape         { L $$ RESHAPE }
      rearrange       { L $$ REARRANGE }
      rotate          { L $$ ROTATE }
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
      empty           { L $$ EMPTY }
      while           { L $$ WHILE }
      stream_map      { L $$ STREAM_MAP }
      stream_map_per  { L $$ STREAM_MAPPER }
      stream_red      { L $$ STREAM_RED }
      stream_red_per  { L $$ STREAM_REDPER }
      include         { L $$ INCLUDE }
      import          { L $$ IMPORT }
      type            { L $$ TYPE }
      module          { L $$ MODULE }
      val             { L $$ VAL }
      open            { L $$ OPEN }
      local           { L $$ LOCAL }
      doc             { L _  (DOC _) }

%left bottom
%left ifprec letprec
%left ','
%left ':'
%left '<-'
%left '||...'
%left '&&...'
%left '<=' '<=...' '>=' '>=...' '>' '>...' '<' '<...' '==...' '!=...'
%left '&...' '^...' '|...'

%left '<<...' '>>...' '>>>...'
%left '+...' '-...' '-'

%left '*...' '*' '/...' '%...' '//...' '%%...'
%left '**...'
%right '->'
nonassoc with
%nonassoc '~' '!' f32 f64 int i8 i16 i32 i64 unsafe default
%nonassoc '['
%nonassoc Id
%left juxtprec
%left indexprec iota copy rotate rearrange split shape reduce map scan filter partition stream_red stream_red_per stream_map stream_map_per streamSeq
%%

-- Some parameterized productions.  Left-recursive, as this is faster
-- in Happy, but does require us to reverse lists at the end.  This
-- allows us to use a more EBNF-style grammar.

many(p) :          { [] }
        | many1(p) { fst $1 : snd $1 }

many1(p) : many1_(p) { reverseNonempty $1 }
many1_(p) : p           { ($1, []) }
          | many1_(p) p { ($2, fst $1 : snd $1) }

sepBy(p,s) :             { [] }
           | sepBy1(p,s) { fst $1 : snd $1 }

sepBy1(p,s) : sepBy1_(p,s) { reverseNonempty $1 }
sepBy1_(p,s) : p                { ($1, []) }
             | sepBy1_(p,s) s p { ($3, fst $1 : snd $1) }

sepBy2(p,s) : p s sepBy1(p,s) { $1 : fst $3 : snd $3 }

-- The main parser.


Prog :: { UncheckedProg }
      : many(Dec) { Prog (concat $1) }
;


Dec :: { [UncheckedDec] }
    : Fun               { [FunDec $1] }
    | Val               { [ValDec $1] }
    | TypeAbbr          { [TypeDec $1] }
    | SigBind           { [SigDec $1 ] }
    | ModBind           { [ModDec $1 ] }
    | DefaultDec        { [] }
    | import stringlit
      { let L loc (STRINGLIT s) = $2 in [LocalDec (OpenDec (ModImport s loc) [] NoInfo $1) $1] }
    | open many1(ModExpAtom)
      { [OpenDec (fst $2) (snd $2) NoInfo $1] }
    | local Dec         { map (`LocalDec` $1) $2 }
    | doc Dec           { let L _ (DOC s) = $1 in map (addDoc s) $2 }
;

SigExp :: { UncheckedSigExp }
        : QualName            { let (v, loc) = $1 in SigVar v loc }
        | '{' many(Spec) '}'  { SigSpecs $2 $1 }
        | SigExp with TypeRef { SigWith $1 $3 $2 }
        | '(' SigExp ')'      { SigParens $2 $1 }
        | '(' id ':' SigExp ')' '->' SigExp
                              { let L _ (ID name) = $2
                                in SigArrow (Just name) $4 $7 $1 }
        | SigExp '->' SigExp  { SigArrow Nothing $1 $3 (srclocOf $1) }

TypeRef :: { TypeRefBase NoInfo Name }
         : QualName '=' TypeExpDecl { TypeRef (fst $1) $3 }

SigBind :: { SigBindBase NoInfo Name }
         : module type id '=' SigExp
          { let L pos (ID name) = $3
            in SigBind name $5 Nothing pos }

ModExp :: { UncheckedModExp }
        : import stringlit
          { let L _ (STRINGLIT s) = $2 in ModImport s $1 }
        | ModExp ':' SigExp
          { ModAscript $1 $3 NoInfo (srclocOf $1) }
        | '\\' ModParam maybeAscription(SimpleSigExp) '->' ModExp
          { ModLambda $2 (fmap (,NoInfo) $3) $5 $1 }
        | ModExpApply
          { $1 }
        | ModExpAtom
          { $1 }

ModExpApply :: { UncheckedModExp }
             : ModExpAtom ModExpAtom %prec juxtprec
               { ModApply $1 $2 NoInfo NoInfo (srclocOf $1) }
             | ModExpApply ModExpAtom %prec juxtprec
               { ModApply $1 $2 NoInfo NoInfo (srclocOf $1) }

ModExpAtom :: { UncheckedModExp }
            : '(' ModExp ')'
              { ModParens $2 $1 }
            | QualName
              { let (v, loc) = $1 in ModVar v loc }
            | '{' many(Dec) '}' { ModDecs (concat $2) $1 }

SimpleSigExp :: { UncheckedSigExp }
             : QualName            { let (v, loc) = $1 in SigVar v loc }
             | '(' SigExp ')'      { $2 }

ModBind :: { ModBindBase NoInfo Name }
         : module id many(ModParam) maybeAscription(SigExp) '=' ModExp
           { let L floc (ID fname) = $2;
             in ModBind fname $3 (fmap (,NoInfo) $4) $6 $1
           }

ModParam :: { ModParamBase NoInfo Name }
          : '(' id ':' SigExp ')' { let L _ (ID name) = $2 in ModParam name $4 $1 }

Spec :: { SpecBase NoInfo Name }
      : val id many(TypeParam) ':' SigTypeDecl
        { let L loc (ID name) = $2; (ps, r) = $5
          in ValSpec name $3 ps r Nothing loc }
      | val BindingBinOp ':' SigTypeDecl
        { let (ps, r) = $4
          in ValSpec $2 [] ps r Nothing $1 }
      | TypeAbbr
        { TypeAbbrSpec $1 }
      | type id many(TypeParam)
        { let L loc (ID name) = $2
          in TypeSpec name $3 Nothing loc }
      | type 'id[' id ']' many(TypeParam)
        { let L loc (INDEXING name) = $2; L ploc (ID pname) = $3
          in TypeSpec name (TypeParamDim pname ploc : $5) Nothing loc }
      | module id ':' SigExp
        { let L _ (ID name) = $2
          in ModSpec name $4 $1 }
      | include SigExp
        { IncludeSpec $2 $1 }
      | doc Spec
        { let L _ (DOC s) = $1 in addDocSpec s $2 }
;

TypeParam :: { TypeParamBase Name }
           : '[' id ']' { let L _ (ID name) = $2 in TypeParamDim name $1 }
           | '\'' id { let L _ (ID name) = $2 in TypeParamType name $1 }

DefaultDec :: { () }
           :  default '(' id ')' {% let L _ (ID s) = $3 in defaultType s  }
           |  default '(' id ',' id ')'
                {% let L _ (ID s1) = $3; L _ (ID s2) = $5 in defaultType s1 >> defaultType s2 }
;


-- Note that this production does not include Minus.
BinOp :: { QualName Name }
      : '+...'     { binOpName $1 }
      | '-...'     { binOpName $1 }
      | '*...'     { binOpName $1 }
      | '*'        { qualName (nameFromString "*") }
      | '/...'     { binOpName $1 }
      | '%...'     { binOpName $1 }
      | '//...'    { binOpName $1 }
      | '%%...'    { binOpName $1 }
      | '==...'    { binOpName $1 }
      | '!=...'    { binOpName $1 }
      | '<...'     { binOpName $1 }
      | '<=...'    { binOpName $1 }
      | '>...'     { binOpName $1 }
      | '>=...'    { binOpName $1 }
      | '&&...'    { binOpName $1 }
      | '||...'    { binOpName $1 }
      | '**...'    { binOpName $1 }
      | '^...'     { binOpName $1 }
      | '&...'     { binOpName $1 }
      | '|...'     { binOpName $1 }
      | '>>...'    { binOpName $1 }
      | '>>>...'   { binOpName $1 }
      | '<<...'    { binOpName $1 }

      | '<'     { QualName [] (nameFromString "<") }
      | '<='    { QualName [] (nameFromString "<=") }
      | '>'     { QualName [] (nameFromString ">") }
      | '>='    { QualName [] (nameFromString ">=") }

BindingBinOp :: { Name }
      : BinOp {% let QualName qs name = $1 in do
                   unless (null qs) $ fail "Cannot use a qualified name in binding position."
                   return name }
      | '-'   { nameFromString "-" }

Fun     : let id many(TypeParam) many1(FunParam) maybeAscription(TypeExpDecl) '=' Exp
          { let L loc (ID name) = $2
            in FunBind (name==defaultEntryPoint) name (fmap declaredType $5) NoInfo
               $3 (fst $4 : snd $4) $7 Nothing loc
          }

        | entry id many(TypeParam) many1(FunParam) maybeAscription(TypeExpDecl) '=' Exp
          { let L loc (ID name) = $2
            in FunBind True name (fmap declaredType $5) NoInfo
               $3 (fst $4 : snd $4) $7 Nothing loc }

        | let FunParam BindingBinOp FunParam maybeAscription(TypeExpDecl) '=' Exp
          { FunBind False $3 (fmap declaredType $5) NoInfo [] [$2,$4] $7 Nothing $1
          }
;

Val : let id maybeAscription(TypeExpDecl) '=' Exp
      { let L _ (ID name) = $2
        in ValBind (name==defaultEntryPoint) name (fmap declaredType $3) NoInfo $5 Nothing $1 }
    | entry id maybeAscription(TypeExpDecl) '=' Exp
      { let L _ (ID name) = $2
        in ValBind True name (fmap declaredType $3) NoInfo $5 Nothing $1 }

Param :: { ParamBase NoInfo Name }
       : '(' id ':' TypeExpDecl ')' { let L _ (ID v) = $2 in NamedParam v $4 $1 }
       | TypeExpDecl                { UnnamedParam $1 }

SigTypeDecl :: { ([ParamBase NoInfo Name], TypeDeclBase NoInfo Name) }
             : TypeExpDecl
               { ([], $1) }
             | Param '->' SigTypeDecl
               { let (ts, t) = $3 in ($1 : ts, t) }

TypeExpDecl :: { TypeDeclBase NoInfo Name }
             : TypeExp { TypeDecl $1 NoInfo }

TypeAbbr :: { TypeBindBase NoInfo Name }
TypeAbbr : type id many(TypeParam) '=' TypeExpDecl
           { let L loc (ID name) = $2
              in TypeBind name $3 $5 Nothing loc }
         | type 'id[' id ']' many(TypeParam) '=' TypeExpDecl
           { let L loc (INDEXING name) = $2; L ploc (ID pname) = $3
             in TypeBind name (TypeParamDim pname ploc:$5) $7 Nothing loc }

TypeExp :: { UncheckedTypeExp }
         : TypeExpApply { TEApply (fst (fst $1)) (snd $1) (snd (fst $1)) }
         | '*' TypeExp  { TEUnique $2 $1 }
         | '[' DimDecl ']' TypeExp   { TEArray $4 (fst $2) $1 }
         | '['  ']' TypeExp          { TEArray $3 AnyDim $1 }
         | TypeExpAtom  { $1 }

TypeExpApply :: { ((QualName Name, SrcLoc), [TypeArgExp Name]) }
              : TypeExpApply TypeArg { (fst $1, snd $1 ++ [$2]) }
              | QualName TypeArg     { ($1, [$2]) }
              | 'id[' DimDecl ']'    { let L loc (INDEXING v) = $1
                                       in ((QualName [] v, loc), [TypeArgExpDim (fst $2) loc]) }
              | 'qid[' DimDecl ']'   { let L loc (QUALINDEXING qs v) = $1
                                       in ((QualName qs v, loc), [TypeArgExpDim (fst $2) loc]) }

TypeExpAtom :: { UncheckedTypeExp }
             : '(' TypeExp ')'               { $2 }
             | '(' ')'                       { TETuple [] $1 }
             | '{' sepBy(FieldType, ',') '}' { TERecord $2 $1 }
             | '(' sepBy2(TypeExp, ',') ')'  { TETuple $2 $1 }
             | QualName                      { TEVar (fst $1) (snd $1) }

TypeArg :: { TypeArgExp Name }
         : '[' DimDecl ']' { TypeArgExpDim (fst $2) $1 }
         | '[' ']'         { TypeArgExpDim AnyDim $1 }
         | TypeExpAtom     { TypeArgExpType $1 }

FieldType : FieldId ':' TypeExp { (fst $1, $3) }

DimDecl :: { (DimDecl Name, SrcLoc) }
        : QualName
          { (NamedDim (fst $1), snd $1) }
        | '#' id
          { let L _ (ID name) = $2
            in (BoundDim name, $1) }
        | declit
          { let L loc (DECLIT n) = $1
            in (ConstDim (fromIntegral n), loc) }
        | intlit
          { let L loc (INTLIT n) = $1
            in (ConstDim (fromIntegral n), loc) }

FunParam :: { PatternBase NoInfo Name }
FunParam : InnerPattern { $1 }

QualName :: { (QualName Name, SrcLoc) }
          : qid { let L loc (QUALID qs v) = $1 in (QualName qs v, loc) }
          | id  { let L loc (ID v) = $1 in (QualName [] v, loc) }

QualUnOpName :: { (QualName Name, SrcLoc) }
          : qunop { let L loc (QUALUNOP qs v) = $1 in (QualName qs v, loc) }
          | unop  { let L loc (UNOP v) = $1 in (QualName [] v, loc) }

-- Expressions are divided into several layers.  The first distinction
-- (between Exp and Exp2) is to factor out ascription, which we do not
-- permit inside array indices operations (there is an ambiguity with
-- array slices).
Exp :: { UncheckedExp }
     : Exp ':' TypeExpDecl { Ascript $1 $3 $2 }
     | Exp2 %prec ':'      { $1 }

Exp2 :: { UncheckedExp }
     : if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 NoInfo $1 }

     | loop '(' many(TypeParam) Pattern ')' LoopForm do Exp %prec ifprec
         {% fmap (\t -> DoLoop $3 $4 t $6 $8 $1) (patternExp $4) }

     | loop '(' many(TypeParam) Pattern '=' Exp ')' LoopForm do Exp %prec ifprec
         { DoLoop $3 $4 $6 $8 $10 $1 }

     | LetExp %prec letprec { $1 }

     | reshape Atom Atom
                      { Reshape $2 $3 $1 }

     | rearrange '(' NaturalInts ')' Atom
                      { Rearrange $3 $5 $1 }

     | rotate '@' NaturalInt Atom Atom { Rotate $3 $4 $5 $1 }

     | rotate Atom Atom
                      { Rotate 0 $2 $3 $1 }

     | split Atom Atom
                      { Split 0 $2 $3 $1 }

     | split '@' NaturalInt Atom Atom
                      { Split $3 $4 $5 $1 }

     | concat many1(Atom)
                      { Concat 0 (fst $2) (snd $2) $1 }

     | concat '@' NaturalInt many1(Atom)
                      { Concat $3 (fst $4) (snd $4) $1 }


     | reduce FunAbstr Atom Atom
                      { Reduce Noncommutative $2 $3 $4 $1 }

     | reduceComm FunAbstr Atom Atom
                      { Reduce Commutative $2 $3 $4 $1 }


     | map FunAbstr many1(Atom)
                      { Map $2 (fst $3:snd $3) $1 }

     | scan FunAbstr Atom Atom
                      { Scan $2 $3 $4 $1 }

     | zip many1(Atom)
                      { Zip 0 (fst $2) (snd $2) $1 }

     | zip '@' NaturalInt many1(Atom)
                      { Zip $3 (fst $4) (snd $4) $1 }

     | unzip Atom  { Unzip $2 [] $1 }

     | unsafe Exp2     { Unsafe $2 $1 }

     | filter FunAbstr Atom
                      { Filter $2 $3 $1 }

     | partition '(' sepBy1(FunAbstr, ',') ')' Atom
                      { Partition (fst $3 : snd $3) $5 $1 }

     | stream_map       FunAbstr Atom
                         { Stream (MapLike InOrder)  $2 $3 $1 }
     | stream_map_per    FunAbstr Atom
                         { Stream (MapLike Disorder) $2 $3 $1 }
     | stream_red       FunAbstr FunAbstr Atom
                         { Stream (RedLike InOrder Noncommutative $2) $3 $4 $1 }
     | stream_red_per    FunAbstr FunAbstr Atom
                         { Stream (RedLike Disorder Commutative $2) $3 $4 $1 }

     | Exp2 '+...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '-...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '-' Exp2       { binOp $1 (L $2 (SYMBOL Minus [] (nameFromString "-"))) $3 }
     | Exp2 '*...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '*' Exp2       { binOp $1 (L $2 (SYMBOL Times [] (nameFromString "*"))) $3 }
     | Exp2 '/...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '%...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '//...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '%%...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '**...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '>>...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '>>>...' Exp2  { binOp $1 $2 $3 }
     | Exp2 '<<...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '&...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '|...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '&&...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '||...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '^...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '==...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '!=...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '<...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '<=...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '>...' Exp2    { binOp $1 $2 $3 }
     | Exp2 '>=...' Exp2   { binOp $1 $2 $3 }

     | Exp2 '>=' Exp2      { binOp $1 (L $2 (SYMBOL Geq [] (nameFromString ">="))) $3 }
     | Exp2 '>' Exp2       { binOp $1 (L $2 (SYMBOL Greater [] (nameFromString ">"))) $3 }
     | Exp2 '<=' Exp2      { binOp $1 (L $2 (SYMBOL Leq [] (nameFromString "<="))) $3 }
     | Exp2 '<' Exp2       { binOp $1 (L $2 (SYMBOL Less [] (nameFromString "<"))) $3 }

     | Apply
       { let (fname, args, loc) = $1 in Apply fname [ (arg, Observe) | arg <- args ] NoInfo loc }
     | Atom %prec juxtprec { $1 }

     | '-' Exp2
       { Negate $2 $1 }

     | Exp2 with '[' sepBy(DimIndex, ',') ']' '<-' Exp2
       { Update $1 $4 $7 (srclocOf $1) }


Apply :: { (QualName Name, [UncheckedExp], SrcLoc) }
      : Apply Atom %prec juxtprec
        { let (fname, args, loc) = $1 in (fname, args ++ [$2], loc) }
      | QualName Atom %prec juxtprec
        { (fst $1, [$2], snd $1) }
      | QualUnOpName Atom %prec juxtprec
        { (fst $1, [$2], snd $1) }

Atom :: { UncheckedExp }
Atom : PrimLit        { Literal (fst $1) (snd $1) }
     | stringlit      {% let L pos (STRINGLIT s) = $1 in do
                             s' <- mapM (getIntValue . fromIntegral . ord) s
                             t <- lift $ gets parserIntType
                             return $ ArrayLit (map (flip Literal pos . SignedValue) s') NoInfo pos }
     | empty '(' TypeExpDecl ')'   { Empty $3 $1 }
     | '(' Exp ')'                 { Parens $2 $1 }
     | '(' Exp ')[' sepBy(DimIndex, ',') ']' { Index (Parens $2 $1) $4 $1 }
     | '(' sepBy2(Exp, ',') ')'    { TupLit $2 $1 }
     | '('      ')'                { TupLit [] $1 }
     | '[' sepBy1(Exp, ',') ']'    { ArrayLit (fst $2:snd $2) NoInfo $1 }
     | QualVarSlice  { let (v,slice,loc) = $1
                       in Index (Var v NoInfo loc) slice loc }
     | QualName { Var (fst $1) NoInfo (snd $1) }
     | '#' FieldId Atom { Project (fst $2) $3 NoInfo $1 }
     | '{' sepBy(Field, ',') '}' { RecordLit $2 $1 }
     | 'qid.(' Exp ')'
       { let L loc (QUALPAREN qs name) = $1 in QualParens (QualName qs name) $2 loc }

Field :: { FieldBase NoInfo Name }
       : FieldId '=' Exp { RecordField (fst $1) $3 (snd $1) }
       | Exp             { RecordRecord $1 }

-- The two productions for LetPat are split instead of using many() to
-- avoid a shift/reduce-conflict.
LetExp :: { UncheckedExp }
     : let Pattern '=' Exp LetBody
                      { LetPat [] $2 $4 $5 $1 }
     | let many1(TypeParam) Pattern '=' Exp LetBody
                      { LetPat (fst $2 : snd $2) $3 $5 $6 $1 }

     | let id many(TypeParam) many1(FunParam) maybeAscription(TypeExpDecl) '=' Exp LetBody
       { let L _ (ID name) = $2
         in LetFun name ($3, fst $4 : snd $4, (fmap declaredType $5), NoInfo, $7) $8 $1 }

     | let VarSlice '=' Exp LetBody
                      { let (v,slice,loc) = $2; ident = Ident v NoInfo loc
                        in LetWith ident ident slice $4 $5 loc }

LetBody :: { UncheckedExp }
    : in Exp %prec letprec { $2 }
    | LetExp %prec letprec { $1 }

LoopForm : for VarId '<' Exp
           { For $2 $4 }
         | for Pattern in Exp
           { ForIn $2 $4 }
         | while Exp
           { While $2 }

VarSlice :: { (Name, [UncheckedDimIndex], SrcLoc) }
          : 'id[' sepBy(DimIndex, ',') ']'
              { let L loc (INDEXING v) = $1
                in (v, $2, loc) }

QualVarSlice :: { (QualName Name, [UncheckedDimIndex], SrcLoc) }
              : VarSlice
                { let (x, y, z) = $1 in (QualName [] x, y, z) }
              | 'qid[' sepBy(DimIndex, ',') ']'
                { let L loc (QUALINDEXING qs v) = $1 in (QualName qs v, $2, loc) }

DimIndex :: { UncheckedDimIndex }
         : Exp2                   { DimFix $1 }
         | Exp2 ':' Exp2          { DimSlice (Just $1) (Just $3) Nothing }
         | Exp2 ':'               { DimSlice (Just $1) Nothing Nothing }
         |      ':' Exp2          { DimSlice Nothing (Just $2) Nothing }
         |      ':'               { DimSlice Nothing Nothing Nothing }
         | Exp2 ':' Exp2 ':' Exp2 { DimSlice (Just $1) (Just $3) (Just $5) }
         |      ':' Exp2 ':' Exp2 { DimSlice Nothing (Just $2) (Just $4) }
         | Exp2 ':'      ':' Exp2 { DimSlice (Just $1) Nothing (Just $4) }
         |      ':'      ':' Exp2 {  DimSlice Nothing Nothing (Just $3) }

VarId : id { let L pos (ID name) = $1 in Ident name NoInfo pos }

FieldId :: { (Name, SrcLoc) }
         : id     { let L loc (ID name) = $1 in (name, loc) }
         | declit { let L loc (DECLIT n) = $1 in (nameFromString (show n), loc) }

Pattern :: { PatternBase NoInfo Name }
Pattern : InnerPattern ':' TypeExpDecl { PatternAscription $1 $3 }
        | InnerPattern                 { $1 }

InnerPattern :: { PatternBase NoInfo Name }
InnerPattern : VarId                            { Id $1 }
             | '_'                              { Wildcard NoInfo $1 }
             | '(' ')'                          { TuplePattern [] $1 }
             | '(' Pattern ')'                  { PatternParens $2 $1 }
             | '(' sepBy2(Pattern, ',') ')'     { TuplePattern $2 $1 }
             | '{' sepBy(FieldPattern, ',') '}' { RecordPattern $2 $1 }

FieldPattern :: { (Name, PatternBase NoInfo Name) }
              : FieldId '=' Pattern     { (fst $1, $3) }
              | FieldId ':' TypeExpDecl { (fst $1,
                                           PatternAscription (Id $ Ident (fst $1) NoInfo (snd $1))
                                                             $3) }
              | FieldId                 { (fst $1, Id $ Ident (fst $1) NoInfo (snd $1)) }

maybeAscription(p) : ':' p { Just $2 }
                   |       { Nothing }

Curry : Curry Atom
        { let (fname, args, loc) = $1 in (fname, args ++ [$2], loc) }
      | QualName Atom %prec indexprec
        { (fst $1, [$2], snd $1) }

FunAbstr :: { UncheckedLambda }
         : '(' '\\' many(TypeParam) many1(FunParam) maybeAscription(TypeExpDecl) '->' Exp ')'
           { AnonymFun $3 (fst $4 : snd $4) $7 $5 NoInfo $1 }
         | QualName
           { CurryFun (fst $1) [] NoInfo (snd $1) }
         | '(' QualUnOpName ')'
           { CurryFun (fst $2) [] NoInfo (snd $2) }
         | '(' Curry ')'
           { let (fname, args, loc) = $2 in CurryFun fname args NoInfo loc }
           -- Minus is handed explicitly here because I could not
           -- figure out how to resolve the ambiguity with negation.
         | '(' '-' Exp2 ')'
           { CurryBinOpRight (QualName [] (nameFromString "-")) $3 (NoInfo, NoInfo) NoInfo $1 }
         | '(' '-' ')'
           { BinOpFun (QualName [] (nameFromString "-")) NoInfo NoInfo NoInfo $1 }
         | '(' Exp2 '-' ')'
           { CurryBinOpLeft (QualName [] (nameFromString "-"))
             $2 (NoInfo, NoInfo) NoInfo (srclocOf $1) }
         | '(' BinOp Exp2 ')'
           { CurryBinOpRight $2 $3 (NoInfo, NoInfo) NoInfo $1 }
         | '(' Exp2 BinOp ')'
           { CurryBinOpLeft $3 $2 (NoInfo, NoInfo) NoInfo $1 }
         | '(' BinOp ')'
           { BinOpFun $2 NoInfo NoInfo NoInfo $1 }

Value : IntValue { $1 }
      | FloatValue { $1 }
      | StringValue { $1 }
      | BoolValue { $1 }
      | ArrayValue { $1 }

CatValues : many(Value) { $1 }

NaturalInt :: { Int }
           : declit { let L _ (DECLIT num) = $1 in fromIntegral num  }
           | intlit { let L _ (INTLIT num) = $1 in fromIntegral num  }

NaturalInts :: { [Int] }
           : NaturalInt                 { [$1] }
           | NaturalInt ',' NaturalInts { $1 : $3  }

PrimType :: { PrimType }
         : id {% let L _ (ID s) = $1 in primTypeFromName s }

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
          | declit  {% let L pos (DECLIT num) = $1 in do num' <- getIntValue (toInteger num); return (num', pos) }
          | intlit  {% let L pos (INTLIT num) = $1 in do num' <- getIntValue (toInteger num); return (num', pos) }
          | charlit {% let L pos (CHARLIT char) = $1 in do
                       num <- getIntValue $ fromIntegral $ ord char
                       return (num, pos) }

UnsignedLit :: { (IntValue, SrcLoc) }
            : u8lit  { let L pos (U8LIT num)  = $1 in (Int8Value $ fromIntegral num, pos) }
            | u16lit { let L pos (U16LIT num) = $1 in (Int16Value $ fromIntegral num, pos) }
            | u32lit { let L pos (U32LIT num) = $1 in (Int32Value $ fromIntegral num, pos) }
            | u64lit { let L pos (U64LIT num) = $1 in (Int64Value $ fromIntegral num, pos) }

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
             {% case combArrayElements $2 $4 of
                  Left e -> throwError e
                  Right v -> return $ ArrayValue (arrayFromList $ $2:$4) $ valueType v
             }
           | empty '(' PrimType ')'
             { ArrayValue (listArray (0,-1) []) (Prim $3) }
           | empty '(' RowType ')'
             { ArrayValue (listArray (0,-1) []) $3 }

RowType : '[' ']' RowType   { arrayOf $3 (Rank 1) Nonunique }
        | '[' ']' PrimType  { arrayOf (Prim $3) (Rank 1) Nonunique }

Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }
       |                  { [] }

{

addDoc :: String -> UncheckedDec -> UncheckedDec
addDoc doc (ValDec val) = ValDec (val { constDoc = Just doc })
addDoc doc (FunDec fun) = FunDec (fun { funBindDoc = Just doc })
addDoc doc (TypeDec tp) = TypeDec (tp { typeDoc = Just doc })
addDoc doc (SigDec sig) = SigDec (sig { sigDoc = Just doc })
addDoc _ dec = dec

addDocSpec :: String -> SpecBase NoInfo Name -> SpecBase NoInfo Name
addDocSpec doc (TypeAbbrSpec tpsig) = TypeAbbrSpec (tpsig { typeDoc = Just doc })
addDocSpec doc val@(ValSpec {}) = val { specDoc = Just doc }
addDocSpec doc (TypeSpec name ps _ loc) = TypeSpec name ps (Just doc) loc
addDocSpec _ spec = spec

reverseNonempty :: (a, [a]) -> (a, [a])
reverseNonempty (x, l) =
  case reverse (x:l) of
    x':rest -> (x', rest)
    []      -> (x, [])

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

combArrayElements :: Value
                  -> [Value]
                  -> Either String Value
combArrayElements t ts = foldM comb t ts
  where comb x y
          | valueType x == valueType y = Right x
          | otherwise                  = Left $ "Elements " ++ pretty x ++ " and " ++
                                         pretty y ++ " cannot exist in same array."

arrayFromList :: [a] -> Array Int a
arrayFromList l = listArray (0, length l-1) l

patternExp :: UncheckedPattern -> ParserMonad UncheckedExp
patternExp (Id ident) = return $ Var (QualName [] (identName ident)) NoInfo $ srclocOf ident
patternExp (TuplePattern pats loc) = TupLit <$> (mapM patternExp pats) <*> return loc
patternExp (Wildcard _ loc) = throwError $ "Cannot have wildcard at " ++ locStr loc

eof :: L Token
eof = L (SrcLoc $ Loc (Pos "" 0 0 0) (Pos "" 0 0 0)) EOF

binOpName (L _ (SYMBOL _ qs op)) = QualName qs op

binOp x (L loc (SYMBOL _ qs op)) y =
  BinOp (QualName qs op) (x, Observe) (y, Observe) NoInfo loc

getTokens :: ParserMonad [L Token]
getTokens = lift $ lift get

putTokens :: [L Token] -> ParserMonad ()
putTokens ts = lift $ lift $ put ts

primTypeFromName :: Name -> ParserMonad PrimType
primTypeFromName s = maybe boom return $ M.lookup s namesToPrimTypes
  where boom = fail $ "No type named " ++ nameToString s

defaultType :: Name -> ParserMonad ()
defaultType name = do
  t <- primTypeFromName name
  s <- lift get
  case t of
    Signed t'    -> lift $ put s { parserIntType = t' }
    Unsigned t'  -> lift $ put s { parserIntType = t' }
    FloatType t' -> lift $ put $ modParserEnv s t'
    _            -> fail $ "Cannot default literals to type " ++ pretty name

getFilename :: ParserMonad FilePath
getFilename = lift $ gets parserFile

getIntValue :: Integer -> ParserMonad IntValue
getIntValue x = do
  t <- lift $ gets parserIntType
  return $ getIntFun t x

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
          ts' <- scanTokensText <$> getFilename <*> readLine
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

--- Now for the parser interface.

-- | A parse error.  Use 'show' to get a human-readable description.
data ParseError = ParseError String

instance Show ParseError where
  show (ParseError s) = s

parseInMonad :: ParserMonad a -> FilePath -> T.Text
             -> ReadLineMonad (Either ParseError a)
parseInMonad p file program =
  either (Left . ParseError) Right <$> either (return . Left)
  (evalStateT (evalStateT (runExceptT p) env))
  (scanTokensText file program)
  where env = newParserEnv file Int32 Float64

parseIncrementalIO :: ParserMonad a -> FilePath -> T.Text
                   -> IO (Either ParseError a)
parseIncrementalIO p file program =
  getLinesFromIO $ parseInMonad p file program

parseIncremental :: ParserMonad a -> FilePath -> T.Text
                 -> Either ParseError a
parseIncremental p file program =
  either (Left . ParseError) id
  $ getLinesFromTexts (T.lines program)
  $ parseInMonad p file mempty

parse :: ParserMonad a -> FilePath -> T.Text
      -> Either ParseError a
parse p file program =
  either (Left . ParseError) id
  $ getNoLines $ parseInMonad p file program

-- | Parse an Futhark expression greedily from the given 'String', only parsing
-- enough lines to get a correct expression, using the 'FilePath' as the source
-- name for error messages.
parseExpIncr :: FilePath -> T.Text
             -> Either ParseError UncheckedExp
parseExpIncr = parseIncremental expression

-- | Parse an Futhark expression incrementally from IO 'getLine' calls, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrIO :: FilePath -> T.Text
               -> IO (Either ParseError UncheckedExp)
parseExpIncrIO = parseIncrementalIO expression

}
