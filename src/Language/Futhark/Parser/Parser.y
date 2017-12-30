{
{-# LANGUAGE TupleSections #-}
-- | Futhark parser written with Happy.
module Language.Futhark.Parser.Parser
  ( prog
  , expression
  , futharkType
  , anyValue
  , anyValues

  , ParserMonad
  , parse
  , ParseError(..)
  , parseExpIncrM
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
      hexreallit      { L _ (REALLIT _) }
      f32lit          { L _ (F32LIT _) }
      f64lit          { L _ (F64LIT _) }
      stringlit       { L _ (STRINGLIT _) }
      charlit         { L _ (CHARLIT _) }

      '#'             { L $$ HASH }
      '..'            { L $$ TWO_DOTS }
      '...'           { L $$ THREE_DOTS }
      '..<'           { L $$ TWO_DOTS_LT }
      '..>'           { L $$ TWO_DOTS_GT }
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
      '.'             { L $$ DOT }
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
%left ifprec letprec unsafe
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
%left juxtprec
%nonassoc with
%left indexprec iota copy rotate rearrange split shape reduce map scan filter partition stream_red stream_red_per stream_map stream_map_per streamSeq
%%

-- The main parser.

Doc :: { String }
     : doc { let L _ (DOC s) = $1 in s }

-- Three cases to avoid ambiguities.
Prog :: { UncheckedProg }
      -- File begins with a file comment, followed by a Dec with a comment.
      : Doc Doc Dec_ Decs { Prog (Just $1) (map (addDoc $2) $3 ++ $4) }
      -- File begins with a file comment, followed by a Dec with no comment.
      | Doc Dec_ Decs     { Prog (Just $1) ($2 ++ $3) }
      -- File begins with a dec with no comment.
      | Dec_ Decs         { Prog Nothing ($1 ++ $2) }
;

Dec :: { [UncheckedDec] }
    : Dec_              { $1 }
    | Doc Dec_          { map (addDoc $1) $2 }

Decs :: { [UncheckedDec] }
      :          { [] }
      | Dec Decs { $1 ++ $2 }

Dec_ :: { [UncheckedDec] }
    : Val               { [ValDec $1] }
    | TypeAbbr          { [TypeDec $1] }
    | SigBind           { [SigDec $1 ] }
    | ModBind           { [ModDec $1 ] }
    | open ModExp
      { [OpenDec $2 [] NoInfo $1] }
    | import stringlit
      { let L loc (STRINGLIT s) = $2 in [LocalDec (OpenDec (ModImport s NoInfo loc) [] NoInfo $1) $1] }
    | local Dec         { map (`LocalDec` $1) $2 }
    | DefaultDec        { [] }
;

SigExp :: { UncheckedSigExp }
        : QualName            { let (v, loc) = $1 in SigVar v loc }
        | '{' Specs '}'  { SigSpecs $2 $1 }
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
        : ModExp ':' SigExp
          { ModAscript $1 $3 NoInfo (srclocOf $1) }
        | '\\' ModParam maybeAscription(SimpleSigExp) '->' ModExp
          { ModLambda $2 (fmap (,NoInfo) $3) $5 $1 }
        | import stringlit
          { let L _ (STRINGLIT s) = $2 in ModImport s NoInfo $1 }
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
            | '{' Decs '}' { ModDecs $2 $1 }

SimpleSigExp :: { UncheckedSigExp }
             : QualName            { let (v, loc) = $1 in SigVar v loc }
             | '(' SigExp ')'      { $2 }

ModBind :: { ModBindBase NoInfo Name }
         : module id ModParams maybeAscription(SigExp) '=' ModExp
           { let L floc (ID fname) = $2;
             in ModBind fname $3 (fmap (,NoInfo) $4) $6 Nothing $1
           }

ModParam :: { ModParamBase NoInfo Name }
          : '(' id ':' SigExp ')' { let L _ (ID name) = $2 in ModParam name $4 NoInfo $1 }

ModParams :: { [ModParamBase NoInfo Name] }
           : ModParam ModParams { $1 : $2 }
           |                    { [] }

Spec :: { SpecBase NoInfo Name }
      : val id TypeParams ':' SigTypeDecl
        { let L loc (ID name) = $2; (ps, r) = $5
          in ValSpec name $3 ps r Nothing loc }
      | val BindingBinOp ':' SigTypeDecl
        { let (ps, r) = $4
          in ValSpec $2 [] ps r Nothing $1 }
      | val BindingUnOp ':' SigTypeDecl
        { let (ps, r) = $4
          in ValSpec $2 [] ps r Nothing $1 }
      | TypeAbbr
        { TypeAbbrSpec $1 }
      | type id TypeParams
        { let L loc (ID name) = $2
          in TypeSpec name $3 Nothing loc }
      | type 'id[' id ']' TypeParams
        { let L loc (INDEXING name) = $2; L ploc (ID pname) = $3
          in TypeSpec name (TypeParamDim pname ploc : $5) Nothing loc }
      | module id ':' SigExp
        { let L _ (ID name) = $2
          in ModSpec name $4 $1 }
      | include SigExp
        { IncludeSpec $2 $1 }
      | Doc Spec
        { addDocSpec $1 $2 }

Specs :: { [SpecBase NoInfo Name] }
       : Spec Specs { $1 : $2 }
       |            { [] }

TypeParam :: { TypeParamBase Name }
           : '[' id ']' { let L _ (ID name) = $2 in TypeParamDim name $1 }
           | '\'' id { let L _ (ID name) = $2 in TypeParamType name $1 }

TypeParams :: { [TypeParamBase Name] }
            : TypeParam TypeParams { $1 : $2 }
            |                      { [] }

TypeParams1 :: { (TypeParamBase Name, [TypeParamBase Name]) }
            : TypeParam TypeParams { ($1, $2) }

DefaultDec :: { () }
           :  default '(' id ')' {% let L _ (ID s) = $3 in defaultType s  }
           |  default '(' id ',' id ')'
                {% let L _ (ID s1) = $3; L _ (ID s2) = $5 in defaultType s1 >> defaultType s2 }
;

UnOp :: { (QualName Name, SrcLoc) }
      : qunop { let L loc (QUALUNOP qs v) = $1 in (QualName qs v, loc) }
      | unop  { let L loc (UNOP v) = $1 in (QualName [] v, loc) }

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

BindingUnOp :: { Name }
      : UnOp {% let (QualName qs name, _) = $1 in do
                   unless (null qs) $ fail "Cannot use a qualified name in binding position."
                   return name }

BindingBinOp :: { Name }
      : BinOp {% let QualName qs name = $1 in do
                   unless (null qs) $ fail "Cannot use a qualified name in binding position."
                   return name }
      | '-'   { nameFromString "-" }


Val    :: { ValBindBase NoInfo Name }
Val     : let id TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let L loc (ID name) = $2
            in ValBind (name==defaultEntryPoint) name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing loc
          }

        | entry id TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let L loc (ID name) = $2
            in ValBind True name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing loc }

        | let FunParam BindingBinOp FunParam maybeAscription(TypeExpDecl) '=' Exp
          { ValBind False $3 (fmap declaredType $5) NoInfo [] [$2,$4] $7 Nothing $1
          }

        | let BindingUnOp TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let name = $2
            in ValBind (name==defaultEntryPoint) name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing $1
          }

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
TypeAbbr : type id TypeParams '=' TypeExpDecl
           { let L loc (ID name) = $2
              in TypeBind name $3 $5 Nothing loc }
         | type 'id[' id ']' TypeParams '=' TypeExpDecl
           { let L loc (INDEXING name) = $2; L ploc (ID pname) = $3
             in TypeBind name (TypeParamDim pname ploc:$5) $7 Nothing loc }

TypeExp :: { UncheckedTypeExp }
         : TypeExpApply { TEApply (fst (fst $1)) (snd $1) (snd (fst $1)) }
         | '*' TypeExp  { TEUnique $2 $1 }
         | '[' DimDecl ']' TypeExp   { TEArray $4 (fst $2) $1 }
         | '['  ']' TypeExp          { TEArray $3 AnyDim $1 }
         | TypeExpAtom  { $1 }

         -- Errors
         | '[' DimDecl ']' %prec bottom
           {% parseErrorAt (srclocOf ($3)) $ Just $
                unlines ["missing array row type.",
                         "Did you mean []"  ++ pretty (fst $2) ++ "?"]
           }

TypeExpApply :: { ((QualName Name, SrcLoc), [TypeArgExp Name]) }
              : TypeExpApply TypeArg { (fst $1, snd $1 ++ [$2]) }
              | QualName TypeArg     { ($1, [$2]) }
              | 'id[' DimDecl ']'    { let L loc (INDEXING v) = $1
                                       in ((QualName [] v, loc), [TypeArgExpDim (fst $2) loc]) }
              | 'qid[' DimDecl ']'   { let L loc (QUALINDEXING qs v) = $1
                                       in ((QualName qs v, loc), [TypeArgExpDim (fst $2) loc]) }

TypeExpAtom :: { UncheckedTypeExp }
             : '(' TypeExp ')'                { $2 }
             | '(' ')'                        { TETuple [] $1 }
             | '(' TypeExp ',' TupleTypes ')' { TETuple ($2:$4) $1 }
             | '{' '}'                        { TERecord [] $1 }
             | '{' FieldTypes1 '}'            { TERecord $2 $1 }
             | QualName                       { TEVar (fst $1) (snd $1) }

TypeArg :: { TypeArgExp Name }
         : '[' DimDecl ']' { TypeArgExpDim (fst $2) $1 }
         | '[' ']'         { TypeArgExpDim AnyDim $1 }
         | TypeExpAtom     { TypeArgExpType $1 }

FieldType :: { (Name, UncheckedTypeExp) }
FieldType : FieldId ':' TypeExp { (fst $1, $3) }

FieldTypes1 :: { [(Name, UncheckedTypeExp)] }
FieldTypes1 : FieldType                 { [$1] }
            | FieldType ',' FieldTypes1 { $1 : $3 }

TupleTypes :: { [UncheckedTypeExp] }
            : TypeExp                { [$1] }
            | TypeExp ',' TupleTypes { $1 : $3 }

DimDecl :: { (DimDecl Name, SrcLoc) }
        : QualName
          { (NamedDim (fst $1), snd $1) }
        | declit
          { let L loc (DECLIT n) = $1
            in (ConstDim (fromIntegral n), loc) }
        | intlit
          { let L loc (INTLIT n) = $1
            in (ConstDim (fromIntegral n), loc) }

        -- Errors
        | '#' {% parseErrorAt (srclocOf $1) $ Just $
                unlines ["found implicit size quantification.",
                         "This is no longer supported.  Use explicit size parameters."]
              }


FunParam :: { PatternBase NoInfo Name }
FunParam : InnerPattern { $1 }

FunParams1 :: { (PatternBase NoInfo Name, [PatternBase NoInfo Name]) }
FunParams1 : FunParam            { ($1, []) }
           | FunParam FunParams1 { ($1, fst $2 : snd $2) }

FunParams :: { [PatternBase NoInfo Name] }
FunParams :                     { [] }
           | FunParam FunParams { $1 : $2 }

QualName :: { (QualName Name, SrcLoc) }
          : qid { let L loc (QUALID qs v) = $1 in (QualName qs v, loc) }
          | id  { let L loc (ID v) = $1 in (QualName [] v, loc) }

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

     | loop TypeParams Pattern LoopForm do Exp %prec ifprec
         {% fmap (\t -> DoLoop $2 $3 t $4 $6 $1) (patternExp $3) }

     | loop TypeParams Pattern '=' Exp LoopForm do Exp %prec ifprec
         { DoLoop $2 $3 $5 $6 $8 $1 }

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

     | concat Atoms1
                      { Concat 0 (fst $2) (snd $2) $1 }

     | concat '@' NaturalInt Atoms1
                      { Concat $3 (fst $4) (snd $4) $1 }


     | reduce Atom Atom Atom
                      { Reduce Noncommutative $2 $3 $4 $1 }

     | reduceComm Atom Atom Atom
                      { Reduce Commutative $2 $3 $4 $1 }


     | map Atom Atoms1
                      { Map $2 (fst $3:snd $3) $1 }

     | scan Atom Atom Atom
                      { Scan $2 $3 $4 $1 }

     | zip Atoms1
                      { Zip 0 (fst $2) (snd $2) $1 }

     | zip '@' NaturalInt Atoms1
                      { Zip $3 (fst $4) (snd $4) $1 }

     | unzip Atom  { Unzip $2 [] $1 }

     | unsafe Exp2     { Unsafe $2 $1 }

     | filter Atom Atom
                      { Filter $2 $3 $1 }

     | partition '(' CommaAtoms1 ')' Atom
                      { Partition (fst $3 : snd $3) $5 $1 }

     | stream_map       Atom Atom
                         { Stream (MapLike InOrder)  $2 $3 $1 }
     | stream_map_per    Atom Atom
                         { Stream (MapLike Disorder) $2 $3 $1 }
     | stream_red       Atom Atom Atom
                         { Stream (RedLike InOrder Noncommutative $2) $3 $4 $1 }
     | stream_red_per    Atom Atom Atom
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

     | '-' Exp2
       { Negate $2 $1 }

     | Exp2 with '[' DimIndices ']' '<-' Exp2
       { Update $1 $4 $7 (srclocOf $1) }

     | '\\' TypeParams FunParams1 maybeAscription(TypeExpDecl) '->' Exp
       { Lambda $2 (fst $3 : snd $3) $6 $4 NoInfo $1 }

     | Apply { $1 }

Apply :: { UncheckedExp }
      : Apply Atom %prec juxtprec
        { Apply $1 $2 NoInfo NoInfo (srclocOf $1) }
      | UnOp Atom %prec juxtprec
        { Apply (Var (fst $1) NoInfo (snd $1)) $2 NoInfo NoInfo (snd $1) }
      | Atom %prec juxtprec
        { $1 }

Atom :: { UncheckedExp }
Atom : PrimLit        { Literal (fst $1) (snd $1) }
     | stringlit      {% let L pos (STRINGLIT s) = $1 in do
                             s' <- mapM (getIntValue . fromIntegral . ord) s
                             t <- lift $ gets parserIntType
                             return $ ArrayLit (map (flip Literal pos . SignedValue) s') NoInfo pos }
     | empty '(' TypeExpDecl ')'   { Empty $3 $1 }
     | '(' Exp ')' FieldAccesses
       { foldl (\x (y, _) -> Project y x NoInfo (srclocOf x))
               (Parens $2 $1)
               $4 }
     | '(' Exp ')[' DimIndices ']'    { Index (Parens $2 $1) $4 $1 }
     | '(' Exp ',' Exps1 ')'          { TupLit ($2 : fst $4 : snd $4) $1 }
     | '('      ')'                   { TupLit [] $1 }
     | '[' Exps1 ']'                  { ArrayLit (fst $2:snd $2) NoInfo $1 }

     | '[' Exp '...' Exp ']'          { Range $2 Nothing (ToInclusive $4) $1 }
     | '[' Exp '..<' Exp ']'          { Range $2 Nothing (UpToExclusive $4) $1 }
     | '[' Exp '..>' Exp ']'          { Range $2 Nothing (DownToExclusive $4) $1 }
     | '[' Exp '..' Exp '...' Exp ']' { Range $2 (Just $4) (ToInclusive $6) $1 }
     | '[' Exp '..' Exp '..<' Exp ']' { Range $2 (Just $4) (UpToExclusive $6) $1 }
     | '[' Exp '..' Exp '..>' Exp ']' { Range $2 (Just $4) (DownToExclusive $6) $1 }

     | QualVarSlice  { let (v,slice,loc) = $1
                       in Index (Var v NoInfo loc) slice loc }
     | QualName FieldAccesses
       { foldl (\x (y, _) -> Project y x NoInfo (srclocOf x))
               (Var (fst $1) NoInfo (snd $1))
               $2 }
     | '{' Fields '}' { RecordLit $2 $1 }
     | 'qid.(' Exp ')'
       { let L loc (QUALPAREN qs name) = $1 in QualParens (QualName qs name) $2 loc }

     -- Operator sections.
     | '(' UnOp ')'
        { Var (fst $2) NoInfo (snd $2) }
     | '(' '-' ')'
        { OpSection (QualName [] (nameFromString "-")) NoInfo NoInfo NoInfo $1 }
     | '(' Exp2 '-' ')'
        { OpSectionLeft (QualName [] (nameFromString "-"))
          $2 (NoInfo, NoInfo) NoInfo (srclocOf $1) }
     | '(' BinOp Exp2 ')'
       { OpSectionRight $2 $3 (NoInfo, NoInfo) NoInfo $1 }
     | '(' Exp2 BinOp ')'
       { OpSectionLeft $3 $2 (NoInfo, NoInfo) NoInfo $1 }
     | '(' BinOp ')'
       { OpSection $2 NoInfo NoInfo NoInfo $1 }

     -- Errors
     | '[' ']'
       {% emptyArrayError $1 }

     | '[' Exp '..' Exp ']' {% twoDotsRange $1 }

Atoms1 :: { (UncheckedExp, [UncheckedExp]) }
        : Atom Atoms1 { ($1, fst $2 : snd $2) }
        | Atom        { ($1, []) }

CommaAtoms1 :: { (UncheckedExp, [UncheckedExp]) }
             : Atom ',' CommaAtoms1 { ($1, fst $3 : snd $3) }
             | Atom                 { ($1, []) }

Exps1 :: { (UncheckedExp, [UncheckedExp]) }
        : Exp ',' Exps1 { ($1, fst $3 : snd $3) }
        | Exp           { ($1, []) }

FieldAccess :: { (Name, SrcLoc) }
             : '.' FieldId { (fst $2, $1) }

FieldAccesses :: { [(Name, SrcLoc)] }
               : FieldAccess FieldAccesses { $1 : $2 }
               |                           { [] }

Field :: { FieldBase NoInfo Name }
       : FieldId '=' Exp { RecordFieldExplicit (fst $1) $3 (snd $1) }
       | id              { let L loc (ID s) = $1 in RecordFieldImplicit s NoInfo loc }

Fields :: { [FieldBase NoInfo Name] }
        : Fields1 { $1 }
        |         { [] }

Fields1 :: { [FieldBase NoInfo Name] }
        : Field ',' Fields1 { $1 : $3 }
        | Field             { [$1] }

LetExp :: { UncheckedExp }
     : let Pattern '=' Exp LetBody
                      { LetPat [] $2 $4 $5 $1 }
     | let TypeParams1 Pattern '=' Exp LetBody
                      { LetPat (fst $2 : snd $2) $3 $5 $6 $1 }

     | let id TypeParams FunParams1 maybeAscription(TypeExpDecl) '=' Exp LetBody
       { let L _ (ID name) = $2
         in LetFun name ($3, fst $4 : snd $4, (fmap declaredType $5), NoInfo, $7) $8 $1 }

     | let VarSlice '=' Exp LetBody
                      { let (v,slice,loc) = $2; ident = Ident v NoInfo loc
                        in LetWith ident ident slice $4 $5 loc }

LetBody :: { UncheckedExp }
    : in Exp %prec letprec { $2 }
    | LetExp %prec letprec { $1 }

LoopForm :: { LoopFormBase NoInfo Name }
LoopForm : for VarId '<' Exp
           { For $2 $4 }
         | for Pattern in Exp
           { ForIn $2 $4 }
         | while Exp
           { While $2 }

VarSlice :: { (Name, [UncheckedDimIndex], SrcLoc) }
          : 'id[' DimIndices ']'
              { let L loc (INDEXING v) = $1
                in (v, $2, loc) }

QualVarSlice :: { (QualName Name, [UncheckedDimIndex], SrcLoc) }
              : VarSlice
                { let (x, y, z) = $1 in (QualName [] x, y, z) }
              | 'qid[' DimIndices ']'
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
         |      ':'      ':' Exp2 { DimSlice Nothing Nothing (Just $3) }

DimIndices :: { [UncheckedDimIndex] }
            :             { [] }
            | DimIndices1 { fst $1 : snd $1 }

DimIndices1 :: { (UncheckedDimIndex, [UncheckedDimIndex]) }
             : DimIndex                 { ($1, []) }
             | DimIndex ',' DimIndices1 { ($1, fst $3 : snd $3) }

VarId :: { IdentBase NoInfo Name }
VarId : id { let L pos (ID name) = $1 in Ident name NoInfo pos }

FieldId :: { (Name, SrcLoc) }
         : id     { let L loc (ID name) = $1 in (name, loc) }
         | declit { let L loc (DECLIT n) = $1 in (nameFromString (show n), loc) }

Pattern :: { PatternBase NoInfo Name }
Pattern : InnerPattern ':' TypeExpDecl { PatternAscription $1 $3 }
        | InnerPattern                 { $1 }

Patterns1 :: { [PatternBase NoInfo Name] }
           : Pattern               { [$1] }
           | Pattern ',' Patterns1 { $1 : $3 }

InnerPattern :: { PatternBase NoInfo Name }
InnerPattern : id                               { let L loc (ID name) = $1 in Id name NoInfo loc }
             | '_'                              { Wildcard NoInfo $1 }
             | '(' ')'                          { TuplePattern [] $1 }
             | '(' Pattern ')'                  { PatternParens $2 $1 }
             | '(' Pattern ',' Patterns1 ')'    { TuplePattern ($2:$4) $1 }
             | '{' FieldPatterns '}'            { RecordPattern $2 $1 }

FieldPattern :: { (Name, PatternBase NoInfo Name) }
              : FieldId '=' Pattern
                { (fst $1, $3) }
              | FieldId ':' TypeExpDecl
                { (fst $1, PatternAscription (Id (fst $1) NoInfo (snd $1)) $3) }
              | FieldId
                { (fst $1, Id (fst $1) NoInfo (snd $1)) }

FieldPatterns :: { [(Name, PatternBase NoInfo Name)] }
               : FieldPatterns1 { $1 }
               |                { [] }

FieldPatterns1 :: { [(Name, PatternBase NoInfo Name)] }
               : FieldPattern ',' FieldPatterns1 { $1 : $3 }
               | FieldPattern                    { [$1] }


maybeAscription(p) : ':' p { Just $2 }
                   |       { Nothing }

Value :: { Value }
Value : IntValue { $1 }
      | FloatValue { $1 }
      | StringValue { $1 }
      | BoolValue { $1 }
      | ArrayValue { $1 }

CatValues :: { [Value] }
CatValues : Value CatValues { $1 : $2 }
          |                 { [] }

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

StringValue :: { Value }
StringValue : stringlit  {% let L pos (STRINGLIT s) = $1 in do
                             s' <- mapM (getIntValue . fromIntegral . ord) s
                             t <- lift $ gets parserIntType
                             return $ ArrayValue (arrayFromList $ map (PrimValue . SignedValue) s') $ Prim $ Signed t }

BoolValue :: { Value }
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
         | hexreallit {% let L pos (REALLIT num) = $1 in do num' <- getRealValue num; return (num', pos)}

PrimLit :: { (PrimValue, SrcLoc) }
        : SignedLit { let (x,loc) = $1 in (SignedValue x, loc) }
        | UnsignedLit { let (x,loc) = $1 in (UnsignedValue x, loc) }
        | FloatLit { let (x,loc) = $1 in (FloatValue x, loc) }

        | true   { (BoolValue True, $1) }
        | false  { (BoolValue False, $1) }

ArrayValue :: { Value }
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

           -- Errors
           | '[' ']'
             {% emptyArrayError $1 }

RowType :: { TypeBase () () }
RowType : '[' ']' RowType   { arrayOf $3 (rank 1) Nonunique }
        | '[' ']' PrimType  { arrayOf (Prim $3) (rank 1) Nonunique }

Values :: { [Value] }
Values : Value ',' Values { $1 : $3 }
       | Value            { [$1] }
       |                  { [] }

{

addDoc :: String -> UncheckedDec -> UncheckedDec
addDoc doc (ValDec val) = ValDec (val { valBindDoc = Just doc })
addDoc doc (TypeDec tp) = TypeDec (tp { typeDoc = Just doc })
addDoc doc (SigDec sig) = SigDec (sig { sigDoc = Just doc })
addDoc doc (ModDec mod) = ModDec (mod { modDoc = Just doc })
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

getLinesFromM :: Monad m => m T.Text -> ReadLineMonad a -> m a
getLinesFromM _ (Value x) = return x
getLinesFromM fetch (GetLine f) = do
  s <- fetch
  getLinesFromM fetch $ f s

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
patternExp (Id v _ loc) = return $ Var (QualName [] v) NoInfo loc
patternExp (TuplePattern pats loc) = TupLit <$> (mapM patternExp pats) <*> return loc
patternExp (Wildcard _ loc) = parseErrorAt loc $ Just "cannot have wildcard here."
patternExp (PatternAscription pat _) = patternExp pat
patternExp (PatternParens pat _) = patternExp pat
patternExp (RecordPattern fs loc) = RecordLit <$> mapM field fs <*> pure loc
  where field (name, pat) = RecordFieldExplicit name <$> patternExp pat <*> pure loc

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
parseError (L loc EOF) =
  parseErrorAt (srclocOf loc) $ Just "unexpected end of file."
parseError (L loc DOC{}) =
  parseErrorAt (srclocOf loc) $
  Just "documentation comments ('-- |') are only permitted when preceding declarations."
parseError tok = parseErrorAt (srclocOf tok) Nothing

parseErrorAt :: SrcLoc -> Maybe String -> ParserMonad a
parseErrorAt loc Nothing = throwError $ locStr loc ++ ": Parse error.\n"
parseErrorAt loc (Just s) = throwError $ locStr loc ++ ": " ++ s

emptyArrayError :: SrcLoc -> ParserMonad a
emptyArrayError loc =
  parseErrorAt loc $
  Just "write empty arrays as 'empty(t)', for element type 't'.\n"

twoDotsRange :: SrcLoc -> ParserMonad a
twoDotsRange loc = parseErrorAt loc $ Just "use '...' for ranges, not '..'.\n"

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

parseIncrementalM :: Monad m =>
                     ParserMonad a
                   -> m T.Text -> FilePath -> T.Text
                   -> m (Either ParseError a)
parseIncrementalM p fetch file program =
  getLinesFromM fetch $ parseInMonad p file program

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

-- | Parse an Futhark expression incrementally from monadic actions, using the
-- 'FilePath' as the source name for error messages.
parseExpIncrM :: Monad m =>
                  m T.Text -> FilePath -> T.Text
               -> m (Either ParseError UncheckedExp)
parseExpIncrM = parseIncrementalM expression

}
