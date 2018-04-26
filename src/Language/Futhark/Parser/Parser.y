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

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import Control.Arrow
import Data.Array
import qualified Data.Text as T
import Data.Char (ord)
import Data.Maybe (fromMaybe, fromJust)
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

      id              { L _ (ID _) }
      'id['           { L _ (INDEXING _) }

      qid             { L _ (QUALID _ _) }
      'qid['          { L _ (QUALINDEXING _ _) }

      'qid.('         { L _ (QUALPAREN _ _) }

      unop            { L _ (UNOP _) }
      qunop           { L _ (QUALUNOP _ _) }

      intlit          { L _ (INTLIT _) }
      i8lit           { L _ (I8LIT _) }
      i16lit          { L _ (I16LIT _) }
      i32lit          { L _ (I32LIT _) }
      i64lit          { L _ (I64LIT _) }
      u8lit           { L _ (U8LIT _) }
      u16lit          { L _ (U16LIT _) }
      u32lit          { L _ (U32LIT _) }
      u64lit          { L _ (U64LIT _) }
      floatlit        { L _ (FLOATLIT _) }
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
      '|>...'         { L _ (SYMBOL PipeRight _ _) }
      '<|...'         { L _ (SYMBOL PipeLeft _ _) }
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
      '\'^'           { L $$ APOSTROPHE_THEN_HAT }
      '`'             { L $$ BACKTICK }
      entry           { L $$ ENTRY }
      '->'            { L $$ RIGHT_ARROW }
      '<-'            { L $$ LEFT_ARROW }
      ':'             { L $$ COLON }
      '.'             { L $$ DOT }
      for             { L $$ FOR }
      do              { L $$ DO }
      with            { L $$ WITH }
      reshape         { L $$ RESHAPE }
      rearrange       { L $$ REARRANGE }
      rotate          { L $$ ROTATE }
      zip             { L $$ ZIP }
      unzip           { L $$ UNZIP }
      unsafe          { L $$ UNSAFE }
      concat          { L $$ CONCAT }
      true            { L $$ TRUE }
      false           { L $$ FALSE }
      empty           { L $$ EMPTY }
      while           { L $$ WHILE }
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
%right '...' '..<' '..>' '..'
%left '`'
%left '<-'
%left '|>...'
%right '<|...'
%left '||...'
%left '&&...'
%left '<=...' '>=...' '>...' '<' '<...' '==...' '!=...'
%left '&...' '^...' '|...'
%left '<<...' '>>...' '>>>...'
%left '+...' '-...' '-'
%left '*...' '*' '/...' '%...' '//...' '%%...'
%left '**...'
%right '->'
%left juxtprec
%nonassoc with
%left indexprec rotate rearrange
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
      { let L loc (STRINGLIT s) = $2 in [LocalDec (OpenDec (ModImport s NoInfo loc) [] NoInfo $1) (srcspan $1 $>)] }
    | local Dec         { map (`LocalDec` $1) $2 }
;

SigExp :: { UncheckedSigExp }
        : QualName            { let (v, loc) = $1 in SigVar v loc }
        | '{' Specs '}'  { SigSpecs $2 (srcspan $1 $>) }
        | SigExp with TypeRef { SigWith $1 $3 (srcspan $1 $>) }
        | '(' SigExp ')'      { SigParens $2 (srcspan $1 $>) }
        | '(' id ':' SigExp ')' '->' SigExp
                              { let L _ (ID name) = $2
                                in SigArrow (Just name) $4 $7 (srcspan $1 $>) }
        | SigExp '->' SigExp  { SigArrow Nothing $1 $3 (srcspan $1 $>) }

TypeRef :: { TypeRefBase NoInfo Name }
         : QualName '=' TypeExpTerm { TypeRef (fst $1) (TypeDecl $3 NoInfo) (srcspan (snd $1) $>) }

SigBind :: { SigBindBase NoInfo Name }
         : module type id '=' SigExp
          { let L _ (ID name) = $3
            in SigBind name $5 Nothing (srcspan $1 $>) }

ModExp :: { UncheckedModExp }
        : ModExp ':' SigExp
          { ModAscript $1 $3 NoInfo (srclocOf $1) }
        | '\\' ModParam maybeAscription(SimpleSigExp) '->' ModExp
          { ModLambda $2 (fmap (,NoInfo) $3) $5 (srcspan $1 $>) }
        | import stringlit
          { let L _ (STRINGLIT s) = $2 in ModImport s NoInfo (srcspan $1 $>) }
        | ModExpApply
          { $1 }
        | ModExpAtom
          { $1 }


ModExpApply :: { UncheckedModExp }
             : ModExpAtom ModExpAtom %prec juxtprec
               { ModApply $1 $2 NoInfo NoInfo (srcspan $1 $>) }
             | ModExpApply ModExpAtom %prec juxtprec
               { ModApply $1 $2 NoInfo NoInfo (srcspan $1 $>) }

ModExpAtom :: { UncheckedModExp }
            : '(' ModExp ')'
              { ModParens $2 (srcspan $1 $>) }
            | QualName
              { let (v, loc) = $1 in ModVar v loc }
            | '{' Decs '}' { ModDecs $2 (srcspan $1 $>) }

SimpleSigExp :: { UncheckedSigExp }
             : QualName            { let (v, loc) = $1 in SigVar v loc }
             | '(' SigExp ')'      { $2 }

ModBind :: { ModBindBase NoInfo Name }
         : module id ModParams maybeAscription(SigExp) '=' ModExp
           { let L floc (ID fname) = $2;
             in ModBind fname $3 (fmap (,NoInfo) $4) $6 Nothing (srcspan $1 $>)
           }

ModParam :: { ModParamBase NoInfo Name }
          : '(' id ':' SigExp ')' { let L _ (ID name) = $2 in ModParam name $4 NoInfo (srcspan $1 $>) }

ModParams :: { [ModParamBase NoInfo Name] }
           : ModParam ModParams { $1 : $2 }
           |                    { [] }

Spec :: { SpecBase NoInfo Name }
      : val id TypeParams ':' TypeExpDecl
        { let L loc (ID name) = $2
          in ValSpec name $3 $5 Nothing (srcspan $1 $>) }
      | val BindingBinOp ':' TypeExpDecl
        { ValSpec $2 [] $4 Nothing (srcspan $1 $>) }
      | val BindingUnOp ':' TypeExpDecl
        { ValSpec $2 [] $4 Nothing (srcspan $1 $>) }
      | TypeAbbr
        { TypeAbbrSpec $1 }
      | type id TypeParams
        { let L _ (ID name) = $2
          in TypeSpec name $3 Nothing (srcspan $1 $>) }
      | type 'id[' id ']' TypeParams
        { let L _ (INDEXING name) = $2; L ploc (ID pname) = $3
          in TypeSpec name (TypeParamDim pname ploc : $5) Nothing (srcspan $1 $>) }
      | module id ':' SigExp
        { let L _ (ID name) = $2
          in ModSpec name $4 (srcspan $1 $>) }
      | include SigExp
        { IncludeSpec $2 (srcspan $1 $>) }
      | Doc Spec
        { addDocSpec $1 $2 }

Specs :: { [SpecBase NoInfo Name] }
       : Spec Specs { $1 : $2 }
       |            { [] }

TypeParam :: { TypeParamBase Name }
           : '[' id ']' { let L _ (ID name) = $2 in TypeParamDim name (srcspan $1 $>) }
           | '\'' id { let L _ (ID name) = $2 in TypeParamType name (srcspan $1 $>) }
           | '\'^' id { let L _ (ID name) = $2 in TypeParamLiftedType name (srcspan $1 $>) }

TypeParams :: { [TypeParamBase Name] }
            : TypeParam TypeParams { $1 : $2 }
            |                      { [] }

TypeParams1 :: { (TypeParamBase Name, [TypeParamBase Name]) }
            : TypeParam TypeParams { ($1, $2) }

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
      | '<|...'    { binOpName $1 }
      | '|>...'    { binOpName $1 }
      | '<'        { QualName [] (nameFromString "<") }

BindingUnOp :: { Name }
      : UnOp {% let (QualName qs name, _) = $1 in do
                   unless (null qs) $ fail "Cannot use a qualified name in binding position."
                   return name }

BindingBinOp :: { Name }
      : BinOp {% let QualName qs name = $1 in do
                   unless (null qs) $ fail "Cannot use a qualified name in binding position."
                   return name }
      | '-'   { nameFromString "-" }

BindingId :: { (Name, SrcLoc) }
     : id                   { let L loc (ID name) = $1 in (name, loc) }
     | '(' BindingBinOp ')' { ($2, $1) }
     | '(' BindingUnOp ')'  { ($2, $1) }

Val    :: { ValBindBase NoInfo Name }
Val     : let BindingId TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let (name, _) = $2
            in ValBind (name==defaultEntryPoint) name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing (srcspan $1 $>)
          }

        | entry BindingId TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let (name, loc) = $2
            in ValBind True name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing (srcspan $1 $>) }

        | let FunParam BindingBinOp FunParam maybeAscription(TypeExpDecl) '=' Exp
          { ValBind False $3 (fmap declaredType $5) NoInfo [] [$2,$4] $7 Nothing (srcspan $1 $>)
          }

        | let BindingUnOp TypeParams FunParams maybeAscription(TypeExpDecl) '=' Exp
          { let name = $2
            in ValBind (name==defaultEntryPoint) name (fmap declaredType $5) NoInfo
               $3 $4 $7 Nothing (srcspan $1 $>)
          }

TypeExpDecl :: { TypeDeclBase NoInfo Name }
             : TypeExp %prec bottom { TypeDecl $1 NoInfo }

TypeAbbr :: { TypeBindBase NoInfo Name }
TypeAbbr : type id TypeParams '=' TypeExpDecl
           { let L _ (ID name) = $2
              in TypeBind name $3 $5 Nothing (srcspan $1 $>) }
         | type 'id[' id ']' TypeParams '=' TypeExpDecl
           { let L loc (INDEXING name) = $2; L ploc (ID pname) = $3
             in TypeBind name (TypeParamDim pname ploc:$5) $7 Nothing (srcspan $1 $>) }

TypeExp :: { UncheckedTypeExp }
         : '(' id ':' TypeExp ')' '->' TypeExp
           { let L _ (ID v) = $2 in TEArrow (Just v) $4 $7 (srcspan $1 $>) }
         | TypeExpTerm '->' TypeExp
           { TEArrow Nothing $1 $3 (srcspan $1 $>) }
         | TypeExpTerm { $1 }


TypeExpTerm :: { UncheckedTypeExp }
         : '*' TypeExpTerm
           { TEUnique $2 (srcspan $1 $>) }
         | '[' DimDecl ']' TypeExpTerm %prec indexprec
           { TEArray $4 (fst $2) (srcspan $1 $>) }
         | '['  ']' TypeExpTerm %prec indexprec
           { TEArray $3 AnyDim (srcspan $1 $>) }
         | TypeExpApply { $1 }

         -- Errors
         | '[' DimDecl ']' %prec bottom
           {% parseErrorAt (srcspan $1 $>) $ Just $
                unlines ["missing array row type.",
                         "Did you mean []"  ++ pretty (fst $2) ++ "?"]
           }

TypeExpApply :: { UncheckedTypeExp }
              : TypeExpApply TypeArg
                { TEApply $1 $2 (srcspan $1 $>) }
              | 'id[' DimDecl ']'
                { let L loc (INDEXING v) = $1
                  in TEApply (TEVar (qualName v) loc) (TypeArgExpDim (fst $2) loc) (srcspan $1 $>) }
              | 'qid[' DimDecl ']'
                { let L loc (QUALINDEXING qs v) = $1
                  in TEApply (TEVar (QualName qs v) loc) (TypeArgExpDim (fst $2) loc) (srcspan $1 $>) }
              | TypeExpAtom
                { $1 }

TypeExpAtom :: { UncheckedTypeExp }
             : '(' TypeExp ')'                { $2 }
             | '(' ')'                        { TETuple [] (srcspan $1 $>) }
             | '(' TypeExp ',' TupleTypes ')' { TETuple ($2:$4) (srcspan $1 $>) }
             | '{' '}'                        { TERecord [] (srcspan $1 $>) }
             | '{' FieldTypes1 '}'            { TERecord $2 (srcspan $1 $>) }
             | QualName                       { TEVar (fst $1) (snd $1) }

TypeArg :: { TypeArgExp Name }
         : '[' DimDecl ']' { TypeArgExpDim (fst $2) (srcspan $1 $>) }
         | '[' ']'         { TypeArgExpDim AnyDim (srcspan $1 $>) }
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
     : Exp ':' TypeExpDecl { Ascript $1 $3 (srcspan $1 $>) }
     | Exp2 %prec ':'      { $1 }

Exp2 :: { UncheckedExp }
     : if Exp then Exp else Exp %prec ifprec
                      { If $2 $4 $6 NoInfo (srcspan $1 $>) }

     | loop TypeParams Pattern LoopForm do Exp %prec ifprec
         {% fmap (\t -> DoLoop $2 $3 t $4 $6 (srcspan $1 $>)) (patternExp $3) }

     | loop TypeParams Pattern '=' Exp LoopForm do Exp %prec ifprec
         { DoLoop $2 $3 $5 $6 $8 (srcspan $1 $>) }

     | LetExp %prec letprec { $1 }

     | reshape Atom Atom
                      { Reshape $2 $3 NoInfo (srcspan $1 $>) }

     | rearrange '(' NaturalInts ')' Atom
                      { Rearrange $3 $5 (srcspan $1 $>) }

     | rotate '@' NaturalInt Atom Atom { Rotate $3 $4 $5 (srcspan $1 $>) }

     | rotate Atom Atom
                      { Rotate 0 $2 $3 (srcspan $1 $>) }

     | concat Atom Atom
                      { Concat 0 $2 $3 (srcspan $1 $>) }

     | concat '@' NaturalInt Atom Atom
                      { Concat $3 $4 $5 (srcspan $1 $>) }

     | zip Atoms1
                      { Zip 0 (fst $2) (snd $2) NoInfo
                        (srcspan $1 (mconcat (map srclocOf (snd $>)))) }

     | zip '@' NaturalInt Atoms1
                      { Zip $3 (fst $4) (snd $4) NoInfo
                        (srcspan $1 (mconcat (map srclocOf (snd $>)))) }

     | unzip Atom  { Unzip $2 [] (srcspan $1 $>) }

     | unsafe Exp2     { Unsafe $2 (srcspan $1 $>) }

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
     | Exp2 '|>...' Exp2   { binOp $1 $2 $3 }
     | Exp2 '<|...' Exp2   { binOp $1 $2 $3 }

     | Exp2 '<' Exp2              { binOp $1 (L $2 (SYMBOL Less [] (nameFromString "<"))) $3 }
     | Exp2 '`' QualName '`' Exp2 { BinOp (fst $3) NoInfo ($1, NoInfo) ($5, NoInfo) NoInfo (srclocOf $1) }

     | Exp2 '...' Exp2           { Range $1 Nothing (ToInclusive $3) NoInfo (srcspan $1 $>) }
     | Exp2 '..<' Exp2           { Range $1 Nothing (UpToExclusive $3) NoInfo (srcspan $1 $>) }
     | Exp2 '..>' Exp2           { Range $1 Nothing (DownToExclusive $3) NoInfo (srcspan $1 $>) }
     | Exp2 '..' Exp2 '...' Exp2 { Range $1 (Just $3) (ToInclusive $5) NoInfo (srcspan $1 $>) }
     | Exp2 '..' Exp2 '..<' Exp2 { Range $1 (Just $3) (UpToExclusive $5) NoInfo (srcspan $1 $>) }
     | Exp2 '..' Exp2 '..>' Exp2 { Range $1 (Just $3) (DownToExclusive $5) NoInfo (srcspan $1 $>) }
     | Exp2 '..' Atom            {% twoDotsRange $2 }
     | Atom '..' Exp2            {% twoDotsRange $2 }
     | '-' Exp2
       { Negate $2 $1 }

     | Exp2 with '[' DimIndices ']' '<-' Exp2
       { Update $1 $4 $7 (srcspan $1 $>) }

     | '\\' TypeParams FunParams1 maybeAscription(TypeExpTerm) '->' Exp
       { Lambda $2 (fst $3 : snd $3) $6 (fmap (flip TypeDecl NoInfo) $4) NoInfo (srcspan $1 $>) }

     | Apply { $1 }

Apply :: { UncheckedExp }
      : Apply Atom %prec juxtprec
        { Apply $1 $2 NoInfo NoInfo (srcspan $1 $>) }
      | UnOp Atom %prec juxtprec
        { Apply (Var (fst $1) NoInfo (snd $1)) $2 NoInfo NoInfo (srcspan (snd $1) $>) }
      | Atom %prec juxtprec
        { $1 }

Atom :: { UncheckedExp }
Atom : PrimLit        { Literal (fst $1) (snd $1) }
     | intlit         { let L loc (INTLIT x) = $1 in IntLit x NoInfo loc }
     | floatlit       { let L loc (FLOATLIT x) = $1 in FloatLit x NoInfo loc }
     | stringlit      { let L loc (STRINGLIT s) = $1 in
                        ArrayLit (map (flip Literal loc . SignedValue . Int32Value . fromIntegral . ord) s) NoInfo loc }
     | empty '(' TypeExpDecl ')'   { Empty $3 NoInfo (srcspan $1 $>) }
     | '(' Exp ')' FieldAccesses
       { foldl (\x (y, _) -> Project y x NoInfo (srclocOf x))
               (Parens $2 (srcspan $1 $3))
               $4 }
     | '(' Exp ')[' DimIndices ']'    { Index (Parens $2 $1) $4 NoInfo (srcspan $1 $>) }
     | '(' Exp ',' Exps1 ')'          { TupLit ($2 : fst $4 : snd $4) (srcspan $1 $>) }
     | '('      ')'                   { TupLit [] (srcspan $1 $>) }
     | '[' Exps1 ']'                  { ArrayLit (fst $2:snd $2) NoInfo (srcspan $1 $>) }
     | '['       ']'                  { ArrayLit [] NoInfo (srcspan $1 $>) }

     | QualVarSlice FieldAccesses
       { let (v,slice,loc) = $1
         in foldl (\x (y, _) -> Project y x NoInfo (srclocOf x))
                  (Index (Var v NoInfo loc) slice NoInfo loc)
                  $2 }
     | QualName FieldAccesses
       { foldl (\x (y, _) -> Project y x NoInfo (srclocOf x))
               (Var (fst $1) NoInfo (snd $1))
               $2 }
     | '{' Fields '}' { RecordLit $2 (srcspan $1 $>) }
     | 'qid.(' Exp ')'
       { let L loc (QUALPAREN qs name) = $1 in QualParens (QualName qs name) $2 loc }

     -- Operator sections.
     | '(' UnOp ')'
        { Var (fst $2) NoInfo (srcspan (snd $2) $>) }
     | '(' '-' ')'
        { OpSection (QualName [] (nameFromString "-")) NoInfo NoInfo NoInfo NoInfo (srcspan $1 $>) }
     | '(' Exp2 '-' ')'
        { OpSectionLeft (QualName [] (nameFromString "-"))
           NoInfo $2 (NoInfo, NoInfo) NoInfo (srcspan $1 $>) }
     | '(' BinOp Exp2 ')'
       { OpSectionRight $2 NoInfo $3 (NoInfo, NoInfo) NoInfo (srcspan $1 $>) }
     | '(' Exp2 BinOp ')'
       { OpSectionLeft $3 NoInfo $2 (NoInfo, NoInfo) NoInfo (srcspan $1 $>) }
     | '(' BinOp ')'
       { OpSection $2 NoInfo NoInfo NoInfo NoInfo (srcspan $1 $>) }

PrimLit :: { (PrimValue, SrcLoc) }
        : true   { (BoolValue True, $1) }
        | false  { (BoolValue False, $1) }

        | i8lit   { let L loc (I8LIT num)  = $1 in (SignedValue $ Int8Value num, loc) }
        | i16lit  { let L loc (I16LIT num) = $1 in (SignedValue $ Int16Value num, loc) }
        | i32lit  { let L loc (I32LIT num) = $1 in (SignedValue $ Int32Value num, loc) }
        | i64lit  { let L loc (I64LIT num) = $1 in (SignedValue $ Int64Value num, loc) }

        | u8lit  { let L loc (U8LIT num)  = $1 in (UnsignedValue $ Int8Value $ fromIntegral num, loc) }
        | u16lit { let L loc (U16LIT num) = $1 in (UnsignedValue $ Int16Value $ fromIntegral num, loc) }
        | u32lit { let L loc (U32LIT num) = $1 in (UnsignedValue $ Int32Value $ fromIntegral num, loc) }
        | u64lit { let L loc (U64LIT num) = $1 in (UnsignedValue $ Int64Value $ fromIntegral num, loc) }

        | f32lit { let L loc (F32LIT num) = $1 in (FloatValue $ Float32Value num, loc) }
        | f64lit { let L loc (F64LIT num) = $1 in (FloatValue $ Float64Value num, loc) }

        | charlit { let L loc (CHARLIT char) = $1
                    in (SignedValue $ Int32Value $ fromIntegral $ ord char, loc) }

Atoms1 :: { (UncheckedExp, [UncheckedExp]) }
        : Atom Atoms1 { ($1, fst $2 : snd $2) }
        | Atom        { ($1, []) }

Exps1 :: { (UncheckedExp, [UncheckedExp]) }
        : Exp ',' Exps1 { ($1, fst $3 : snd $3) }
        | Exp           { ($1, []) }

FieldAccess :: { (Name, SrcLoc) }
             : '.' FieldId { (fst $2, srcspan $1 (snd $>)) }

FieldAccesses :: { [(Name, SrcLoc)] }
               : FieldAccess FieldAccesses { $1 : $2 }
               |                           { [] }

Field :: { FieldBase NoInfo Name }
       : FieldId '=' Exp { RecordFieldExplicit (fst $1) $3 (srcspan (snd $1) $>) }
       | id              { let L loc (ID s) = $1 in RecordFieldImplicit s NoInfo loc }

Fields :: { [FieldBase NoInfo Name] }
        : Fields1 { $1 }
        |         { [] }

Fields1 :: { [FieldBase NoInfo Name] }
        : Field ',' Fields1 { $1 : $3 }
        | Field             { [$1] }

LetExp :: { UncheckedExp }
     : let Pattern '=' Exp LetBody
                      { LetPat [] $2 $4 $5 (srcspan $1 $>) }
     | let TypeParams1 Pattern '=' Exp LetBody
                      { LetPat (fst $2 : snd $2) $3 $5 $6 (srcspan $1 $>) }

     | let id TypeParams FunParams1 maybeAscription(TypeExpDecl) '=' Exp LetBody
       { let L _ (ID name) = $2
         in LetFun name ($3, fst $4 : snd $4, (fmap declaredType $5), NoInfo, $7) $8 (srcspan $1 $>) }

     | let VarSlice '=' Exp LetBody
                      { let (v,slice,loc) = $2; ident = Ident v NoInfo loc
                        in LetWith ident ident slice $4 $5 (srcspan $1 $>) }

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
              { let L _ (INDEXING v) = $1
                in (v, $2, srcspan $1 $>) }

QualVarSlice :: { (QualName Name, [UncheckedDimIndex], SrcLoc) }
              : VarSlice
                { let (x, y, z) = $1 in (QualName [] x, y, z) }
              | 'qid[' DimIndices ']'
                { let L _ (QUALINDEXING qs v) = $1 in (QualName qs v, $2, srcspan $1 $>) }

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
VarId : id { let L loc (ID name) = $1 in Ident name NoInfo loc }

FieldId :: { (Name, SrcLoc) }
         : id     { let L loc (ID name) = $1 in (name, loc) }
         | intlit { let L loc (INTLIT n) = $1 in (nameFromString (show n), loc) }

Pattern :: { PatternBase NoInfo Name }
Pattern : InnerPattern ':' TypeExpDecl { PatternAscription $1 $3 (srcspan $1 $>) }
        | InnerPattern                 { $1 }

Patterns1 :: { [PatternBase NoInfo Name] }
           : Pattern               { [$1] }
           | Pattern ',' Patterns1 { $1 : $3 }

InnerPattern :: { PatternBase NoInfo Name }
InnerPattern : id                               { let L loc (ID name) = $1 in Id name NoInfo loc }
             | '_'                              { Wildcard NoInfo $1 }
             | '(' ')'                          { TuplePattern [] (srcspan $1 $>) }
             | '(' Pattern ')'                  { PatternParens $2 (srcspan $1 $>) }
             | '(' Pattern ',' Patterns1 ')'    { TuplePattern ($2:$4) (srcspan $1 $>) }
             | '{' FieldPatterns '}'            { RecordPattern $2 (srcspan $1 $>) }

FieldPattern :: { (Name, PatternBase NoInfo Name) }
              : FieldId '=' Pattern
                { (fst $1, $3) }
              | FieldId ':' TypeExpDecl
              { (fst $1, PatternAscription (Id (fst $1) NoInfo (snd $1)) $3 (srcspan (snd $1) $>)) }
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
           : intlit   { let L _ (INTLIT num) = $1 in fromIntegral num  }

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
         : FloatLit     { PrimValue (FloatValue (fst $1)) }
         | '-' FloatLit { PrimValue (FloatValue (floatNegate (fst $2))) }

StringValue :: { Value }
StringValue : stringlit  { let L pos (STRINGLIT s) = $1 in
                           ArrayValue (arrayFromList $ map (PrimValue . SignedValue . Int32Value . fromIntegral . ord) s) $ Prim $ Signed Int32 }

BoolValue :: { Value }
BoolValue : true           { PrimValue $ BoolValue True }
          | false          { PrimValue $ BoolValue False }

SignedLit :: { (IntValue, SrcLoc) }
          : i8lit   { let L loc (I8LIT num)  = $1 in (Int8Value num, loc) }
          | i16lit  { let L loc (I16LIT num) = $1 in (Int16Value num, loc) }
          | i32lit  { let L loc (I32LIT num) = $1 in (Int32Value num, loc) }
          | i64lit  { let L loc (I64LIT num) = $1 in (Int64Value num, loc) }
          | intlit  { let L loc (INTLIT num) = $1 in (Int32Value $ fromInteger num, loc) }
          | charlit { let L loc (CHARLIT char) = $1 in (Int32Value $ fromIntegral $ ord char, loc) }

UnsignedLit :: { (IntValue, SrcLoc) }
            : u8lit  { let L pos (U8LIT num)  = $1 in (Int8Value $ fromIntegral num, pos) }
            | u16lit { let L pos (U16LIT num) = $1 in (Int16Value $ fromIntegral num, pos) }
            | u32lit { let L pos (U32LIT num) = $1 in (Int32Value $ fromIntegral num, pos) }
            | u64lit { let L pos (U64LIT num) = $1 in (Int64Value $ fromIntegral num, pos) }

FloatLit :: { (FloatValue, SrcLoc) }
         : f32lit { let L loc (F32LIT num) = $1 in (Float32Value num, loc) }
         | f64lit { let L loc (F64LIT num) = $1 in (Float64Value num, loc) }
         | floatlit { let L loc (FLOATLIT num) = $1 in (Float64Value num, loc) }

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
RowType : '[' ']' RowType   { fromJust $ arrayOf $3 (rank 1) Nonunique }
        | '[' ']' PrimType  { fromJust $ arrayOf (Prim $3) (rank 1) Nonunique }

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
               }

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
patternExp (PatternAscription pat _ _) = patternExp pat
patternExp (PatternParens pat _) = patternExp pat
patternExp (RecordPattern fs loc) = RecordLit <$> mapM field fs <*> pure loc
  where field (name, pat) = RecordFieldExplicit name <$> patternExp pat <*> pure loc

eof :: L Token
eof = L (SrcLoc $ Loc (Pos "" 0 0 0) (Pos "" 0 0 0)) EOF

binOpName (L _ (SYMBOL _ qs op)) = QualName qs op

binOp x (L _ (SYMBOL _ qs op)) y =
  BinOp (QualName qs op) NoInfo (x, NoInfo) (y, NoInfo) NoInfo $
  srcspan x y

getTokens :: ParserMonad [L Token]
getTokens = lift $ lift get

putTokens :: [L Token] -> ParserMonad ()
putTokens ts = lift $ lift $ put ts

primTypeFromName :: Name -> ParserMonad PrimType
primTypeFromName s = maybe boom return $ M.lookup s namesToPrimTypes
  where boom = fail $ "No type named " ++ nameToString s

getFilename :: ParserMonad FilePath
getFilename = lift $ gets parserFile

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
  where env = ParserEnv file

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
