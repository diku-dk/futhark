{
-- | Futhark parser written with Happy.
module Language.Futhark.Parser.Parser
  ( prog
  , expression
  , declaration
  , modExpression
  , futharkType
  , parse
  , parseWithComments
  , SyntaxError(..)
  , Comment(..)
  )
  where

import Data.Bifunctor (second)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.State
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Char (ord)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (genericLength)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Monoid

import Language.Futhark.Syntax hiding (ID)
import Language.Futhark.Prop
import Language.Futhark.Pretty
import Language.Futhark.Parser.Lexer (Token(..))
import Futhark.Util.Pretty
import Futhark.Util.Loc
import Language.Futhark.Parser.Monad

}

%name prog Prog
%name futharkType TypeExp
%name expression Exp
%name modExpression ModExp
%name declaration Dec

%tokentype { L Token }
%error { parseError }
%errorhandlertype explist
%monad { ParserMonad }
%lexer { lexer } { L _ EOF }

%token
      if              { L $$ IF }
      then            { L $$ THEN }
      else            { L $$ ELSE }
      let             { L $$ LET }
      def             { L $$ DEF }
      loop            { L $$ LOOP }
      in              { L $$ IN }
      match           { L $$ MATCH }
      case            { L $$ CASE }

      id              { L _ (ID _) }
      '...['          { L _ INDEXING }

      constructor     { L _ (CONSTRUCTOR _) }

      natlit          { L _ (NATLIT _ _) }
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
      f16lit          { L _ (F16LIT _) }
      f32lit          { L _ (F32LIT _) }
      f64lit          { L _ (F64LIT _) }
      stringlit       { L _ (STRINGLIT _) }
      charlit         { L _ (CHARLIT _) }

      '.'             { L $$ DOT }
      '..'            { L $$ TWO_DOTS }
      '...'           { L $$ THREE_DOTS }
      '..<'           { L $$ TWO_DOTS_LT }
      '..>'           { L $$ TWO_DOTS_GT }
      '='             { L $$ EQU }

      '*'             { L $$ ASTERISK }
      '-'             { L $$ NEGATE }
      '!'             { L $$ BANG }
      '<'             { L $$ LTH }
      '^'             { L $$ HAT }
      '~'             { L $$ TILDE }
      '|'             { L $$ PIPE  }

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
      '|>...'         { L _ (SYMBOL PipeRight _ _) }
      '<|...'         { L _ (SYMBOL PipeLeft _ _) }
      '|...'          { L _ (SYMBOL Bor _ _) }
      '&...'          { L _ (SYMBOL Band _ _) }
      '^...'          { L _ (SYMBOL Xor _ _) }
      '||...'         { L _ (SYMBOL LogOr _ _) }
      '&&...'         { L _ (SYMBOL LogAnd _ _) }
      '!...'          { L _ (SYMBOL Bang _ _) }
      '=...'          { L _ (SYMBOL Equ _ _) }

      '('             { L $$ LPAR }
      ')'             { L $$ RPAR }
      '{'             { L $$ LCURLY }
      '}'             { L $$ RCURLY }
      '['             { L $$ LBRACKET }
      ']'             { L $$ RBRACKET }
      '#['            { L $$ HASH_LBRACKET }
      ','             { L $$ COMMA }
      '_'             { L $$ UNDERSCORE }
      '\\'            { L $$ BACKSLASH }
      '\''            { L $$ APOSTROPHE }
      '\'^'           { L $$ APOSTROPHE_THEN_HAT }
      '\'~'           { L $$ APOSTROPHE_THEN_TILDE }
      '`'             { L $$ BACKTICK }
      entry           { L $$ ENTRY }
      '->'            { L $$ RIGHT_ARROW }
      ':'             { L $$ COLON }
      ':>'            { L $$ COLON_GT }
      '?'             { L $$ QUESTION_MARK  }
      for             { L $$ FOR }
      do              { L $$ DO }
      with            { L $$ WITH }
      assert          { L $$ ASSERT }
      true            { L $$ TRUE }
      false           { L $$ FALSE }
      while           { L $$ WHILE }
      include         { L $$ INCLUDE }
      import          { L $$ IMPORT }
      type            { L $$ TYPE }
      module          { L $$ MODULE }
      val             { L $$ VAL }
      open            { L $$ OPEN }
      local           { L $$ LOCAL }
      doc             { L _  (DOC _) }
      hole            { L $$ HOLE }

%left bottom
%left ifprec letprec caseprec typeprec enumprec sumprec
%left ',' case id constructor '(' '{'
%right ':' ':>'
%right '...' '..<' '..>' '..'
%left '`'
%right '->'
%left with
%left '='
%left '|>...'
%right '<|...'
%left '||...'
%left '&&...'
%left '<=...' '>=...' '>...' '<' '<...' '==...' '!=...' '!...' '=...'
%left '&...' '^...' '^' '|...' '|'
%left '<<...' '>>...'
%left '+...' '-...' '-'
%left '*...' '*' '/...' '%...' '//...' '%%...'
%left '**...'
%left juxtprec
%left '[' '...[' indexprec
%left top
%%

-- The main parser.

Doc :: { DocComment }
     : doc { let L loc (DOC s) = $1 in DocComment s (srclocOf loc) }

-- Four cases to avoid ambiguities.
Prog :: { UncheckedProg }
      -- File begins with a file comment, followed by a Dec with a comment.
      : Doc Doc Dec_ Decs { Prog (Just $1) (addDoc $2 $3 : $4) }
      -- File begins with a file comment, followed by a Dec with no comment.
      | Doc Dec_ Decs     { Prog (Just $1) ($2 : $3) }
      -- File begins with a dec with no comment.
      | Dec_ Decs         { Prog Nothing ($1 : $2) }
      -- File is empty.
      |                   { Prog Nothing [] }
;

Dec :: { UncheckedDec }
    : Dec_              { $1 }
    | Doc Dec_          { addDoc $1 $2 }

Decs :: { [UncheckedDec] }
      : Decs_     { reverse $1 }

Decs_ :: { [UncheckedDec] }
      :           { [] }
      | Decs_ Dec { $2 : $1 }

Dec_ :: { UncheckedDec }
    : Val               { ValDec $1 }
    | TypeAbbr          { TypeDec $1 }
    | ModTypeBind       { ModTypeDec $1 }
    | ModBind           { ModDec $1 }
    | open ModExp       { OpenDec $2 (srclocOf $1) }
    | import stringlit
      { let L _ (STRINGLIT s) = $2 in ImportDec (T.unpack s) NoInfo (srcspan $1 $>) }
    | local Dec         { LocalDec $2 (srcspan $1 $>) }
    | '#[' AttrInfo ']' Dec_
                        { addAttr $2 $4 }

;

ModTypeExp :: { UncheckedModTypeExp }
        : QualName                { let (v, loc) = $1 in ModTypeVar v NoInfo (srclocOf loc) }
        | '{' Specs '}'           { ModTypeSpecs $2 (srcspan $1 $>) }
        | ModTypeExp with TypeRef { ModTypeWith $1 $3 (srcspan $1 $>) }
        | '(' ModTypeExp ')'      { ModTypeParens $2 (srcspan $1 $>) }
        | '(' id ':' ModTypeExp ')' '->' ModTypeExp
                                  { let L _ (ID name) = $2
                                    in ModTypeArrow (Just name) $4 $7 (srcspan $1 $>) }
        | ModTypeExp '->' ModTypeExp  { ModTypeArrow Nothing $1 $3 (srcspan $1 $>) }

TypeRef :: { TypeRefBase NoInfo Name }
         : QualName TypeParams '=' TypeExpTerm
           { TypeRef (fst $1) $2 $4 (srcspan (snd $1) $>) }

ModTypeBind :: { ModTypeBindBase NoInfo Name }
         : module type id '=' ModTypeExp
          { let L _ (ID name) = $3
            in ModTypeBind name $5 Nothing (srcspan $1 $>) }

ModExp :: { UncheckedModExp }
        : ModExp ':' ModTypeExp
          { ModAscript $1 $3 NoInfo (srcspan $1 $>) }
        | '\\' ModParam maybeAscription(SimpleModTypeExp) '->' ModExp
          { ModLambda $2 (fmap (,NoInfo) $3) $5 (srcspan $1 $>) }
        | import stringlit
          { let L _ (STRINGLIT s) = $2 in ModImport (T.unpack s) NoInfo (srcspan $1 $>) }
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
              { let (v, loc) = $1 in ModVar v (srclocOf loc) }
            | '{' Decs '}' { ModDecs $2 (srcspan $1 $>) }

SimpleModTypeExp :: { UncheckedModTypeExp }
             : QualName            { let (v, loc) = $1 in ModTypeVar v NoInfo (srclocOf loc) }
             | '(' ModTypeExp ')'      { $2 }

ModBind :: { ModBindBase NoInfo Name }
         : module id ModParams maybeAscription(ModTypeExp) '=' ModExp
           { let L floc (ID fname) = $2;
             in ModBind fname $3 (fmap (,NoInfo) $4) $6 Nothing (srcspan $1 $>)
           }

ModParam :: { ModParamBase NoInfo Name }
          : '(' id ':' ModTypeExp ')' { let L _ (ID name) = $2 in ModParam name $4 NoInfo (srcspan $1 $>) }

ModParams :: { [ModParamBase NoInfo Name] }
           : ModParam ModParams { $1 : $2 }
           |                    { [] }

Liftedness :: { Liftedness }
            :     { Unlifted }
            | '~' { SizeLifted }
            | '^' { Lifted }

Spec :: { SpecBase NoInfo Name }
      : val id TypeParams ':' TypeExp
        { let L loc (ID name) = $2
          in ValSpec name $3 $5 NoInfo Nothing (srcspan $1 $>) }
      | val BindingBinOp TypeParams ':' TypeExp
        { ValSpec $2 $3 $5 NoInfo Nothing (srcspan $1 $>) }
      | TypeAbbr
        { TypeAbbrSpec $1 }

      | type Liftedness id TypeParams
        { let L _ (ID name) = $3
          in TypeSpec $2 name $4 Nothing (srcspan $1 $>) }

      | module id ':' ModTypeExp
        { let L _ (ID name) = $2
          in ModSpec name $4 Nothing (srcspan $1 $>) }
      | include ModTypeExp
        { IncludeSpec $2 (srcspan $1 $>) }
      | Doc Spec
        { addDocSpec $1 $2 }
      | '#[' AttrInfo ']' Spec
        { addAttrSpec $2 $4 }

Specs :: { [SpecBase NoInfo Name] }
       : Specs_      { reverse $1 }

Specs_ :: { [SpecBase NoInfo Name] }
       : Specs_ Spec { $2 : $1 }
       |             { [] }

SizeBinder :: { SizeBinder Name }
            : '[' id ']'  { let L _ (ID name) = $2 in SizeBinder name (srcspan $1 $>) }
            | '...[' id ']' { let L _ (ID name) = $2 in SizeBinder name (srcspan $1 $>) }

SizeBinders1 :: { [SizeBinder Name] }
             : SizeBinder SizeBinders1 { $1 : $2 }
             | SizeBinder              { [$1] }

TypeTypeParam :: { TypeParamBase Name }
           : '\'' id { let L _ (ID name) = $2 in TypeParamType Unlifted name (srcspan $1 $>) }
           | '\'~' id { let L _ (ID name) = $2 in TypeParamType SizeLifted name (srcspan $1 $>) }
           | '\'^' id { let L _ (ID name) = $2 in TypeParamType Lifted name (srcspan $1 $>) }

TypeParam :: { TypeParamBase Name }
           : '[' id ']' { let L _ (ID name) = $2 in TypeParamDim name (srcspan $1 $>) }
           | '...[' id ']' { let L _ (ID name) = $2 in TypeParamDim name (srcspan $1 $>) }
           | TypeTypeParam { $1 }

TypeParams :: { [TypeParamBase Name] }
            : TypeParam TypeParams { $1 : $2 }
            |                      { [] }

-- Due to an ambiguity between in-place updates ("let x[i] ...") and
-- local functions with size parameters, the latter need a special
-- nonterminal.
LocalFunTypeParams :: { [TypeParamBase Name] }
                    : '[' id ']' TypeParams
                      { let L _ (ID name) = $2 in TypeParamDim name (srcspan $1 $>) : $4 }
                    | TypeTypeParam TypeParams { $1 : $2 }
                    |                          { [] }


-- Note that this production does not include Minus, but does include
-- operator sections.
BinOp :: { (QualName Name, Loc) }
      : '+...'     { binOpName $1 }
      | '-...'     { binOpName $1 }
      | '*...'     { binOpName $1 }
      | '*'        { (qualName (nameFromString "*"), $1) }
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
      | '^'        { (qualName (nameFromString "^"), $1) }
      | '&...'     { binOpName $1 }
      | '|...'     { binOpName $1 }
      | '|'        { (qualName (nameFromString "|"), $1) }
      | '>>...'    { binOpName $1 }
      | '<<...'    { binOpName $1 }
      | '<|...'    { binOpName $1 }
      | '|>...'    { binOpName $1 }
      | '<'        { (qualName (nameFromString "<"), $1) }
      | '!...'     { binOpName $1 }
      | '=...'     { binOpName $1 }
      | '`' QualName '`' { $2 }

BindingBinOp :: { Name }
      : BinOp {% let (QualName qs name, loc) = $1 in do
                   unless (null qs) $ parseErrorAt loc $
                     Just "Cannot use a qualified name in binding position."
                   pure name }
      | '-'   { nameFromString "-" }
      | '!'   {% parseErrorAt $1 $ Just $ "'!' is a prefix operator and cannot be used as infix operator." }

BindingId :: { (Name, Loc) }
     : id                   { let L loc (ID name) = $1 in (name, loc) }
     | '(' BindingBinOp ')' { ($2, $1) }

Val    :: { ValBindBase NoInfo Name }
Val     : def BindingId TypeParams FunParams maybeAscription(TypeExp) '=' Exp
          { let (name, _) = $2
            in ValBind Nothing name $5 NoInfo
               $3 $4 $7 Nothing mempty (srcspan $1 $>)
          }

        | entry BindingId TypeParams FunParams maybeAscription(TypeExp) '=' Exp
          { let (name, loc) = $2
            in ValBind (Just NoInfo) name $5 NoInfo
               $3 $4 $7 Nothing mempty (srcspan $1 $>) }

        | def FunParam BindingBinOp FunParam maybeAscription(TypeExp) '=' Exp
          { ValBind Nothing $3 $5 NoInfo [] [$2,$4] $7
            Nothing mempty (srcspan $1 $>)
          }

        -- The next two for backwards compatibility.
        | let BindingId TypeParams FunParams maybeAscription(TypeExp) '=' Exp
          { let (name, _) = $2
            in ValBind Nothing name $5 NoInfo
               $3 $4 $7 Nothing mempty (srcspan $1 $>)
          }
        | let FunParam BindingBinOp FunParam maybeAscription(TypeExp) '=' Exp
          { ValBind Nothing $3 $5 NoInfo [] [$2,$4] $7
            Nothing mempty (srcspan $1 $>)
          }

        -- Some error cases
        | def '(' Pat ',' Pats1 ')' '=' Exp
          {% parseErrorAt (srcspan $2 $6) $ Just $
             T.unlines ["Cannot bind patterns at top level.",
                        "Bind a single name instead."]
          }

        | let '(' Pat ',' Pats1 ')' '=' Exp
          {% parseErrorAt (srcspan $2 $6) $ Just $
             T.unlines ["Cannot bind patterns at top level.",
                        "Bind a single name instead."]
          }

TypeAbbr :: { TypeBindBase NoInfo Name }
TypeAbbr : type Liftedness id TypeParams '=' TypeExp
           { let L _ (ID name) = $3
              in TypeBind name $2 $4 $6 NoInfo Nothing (srcspan $1 $>) }

TypeExp :: { UncheckedTypeExp }
         : '(' id ':' TypeExp ')' '->' TypeExp
           { let L _ (ID v) = $2 in TEArrow (Just v) $4 $7 (srcspan $1 $>) }
         | TypeExpTerm '->' TypeExp
           { TEArrow Nothing $1 $3 (srcspan $1 $>) }
         | '?' TypeExpDims '.' TypeExp { TEDim $2 $4 (srcspan $1 $>) }
         | TypeExpTerm %prec typeprec { $1 }

TypeExpDims :: { [Name] }
         : '[' id ']'             { let L _ (ID v) = $2 in [v] }
         | '[' id ']' TypeExpDims { let L _ (ID v) = $2 in v : $4 }
         | '...[' id ']'             { let L _ (ID v) = $2 in [v] }
         | '...[' id ']' TypeExpDims { let L _ (ID v) = $2 in v : $4 }

TypeExpTerm :: { UncheckedTypeExp }
         : '*' TypeExpTerm
           { TEUnique $2 (srcspan $1 $>) }
         | TypeExpApply %prec typeprec { $1 }
         | SumClauses %prec sumprec
           { let (cs, loc) = $1 in TESum cs (srclocOf loc) }

SumClauses :: { ([(Name, [UncheckedTypeExp])], Loc) }
            : SumClauses '|' SumClause %prec sumprec
              { let (cs, loc1) = $1; (c, ts, loc2) = $3
                in (cs++[(c, ts)], locOf (srcspan loc1 loc2)) }
            | SumClause  %prec sumprec
              { let (n, ts, loc) = $1 in ([(n, ts)], loc) }

SumPayload :: { [UncheckedTypeExp] }
           : %prec bottom           { [] }
           | TypeExpAtom SumPayload { $1 : $2 }

SumClause :: { (Name, [UncheckedTypeExp], Loc) }
           : Constr SumPayload
             { (fst $1, $2, locOf (srcspan (snd $1) $>)) }

TypeExpApply :: { UncheckedTypeExp }
              : TypeExpApply TypeArg
                { TEApply $1 $2 (srcspan $1 $>) }
              | TypeExpAtom
                { $1 }

TypeExpAtom :: { UncheckedTypeExp }
             : '(' TypeExp ')'                { TEParens $2 (srcspan $1 $>) }
             | '(' ')'                        { TETuple [] (srcspan $1 $>) }
             | '(' TypeExp ',' TupleTypes ')' { TETuple ($2:$4) (srcspan $1 $>) }
             | '{' FieldTypes '}'             { TERecord $2 (srcspan $1 $>) }
             | SizeExp TypeExpTerm            { TEArray $1 $2 (srcspan $1 $>) }
             | QualName                       { TEVar (fst $1) (srclocOf (snd $1)) }

Constr :: { (Name, Loc) }
        : constructor { let L _ (CONSTRUCTOR c) = $1 in (c, locOf $1) }

TypeArg :: { TypeArgExp NoInfo Name }
         : SizeExp %prec top
           { TypeArgExpSize $1 }
         | TypeExpAtom
           { TypeArgExpType $1 }

FieldType :: { (Name, UncheckedTypeExp) }
FieldType : FieldId ':' TypeExp { (fst $1, $3) }

FieldTypes :: { [(Name, UncheckedTypeExp)] }
FieldTypes :                          { [] }
           | FieldType                { [$1] }
           | FieldType ',' FieldTypes { $1 : $3 }

TupleTypes :: { [UncheckedTypeExp] }
            : TypeExp                { [$1] }
            | TypeExp ','            { [$1] }
            | TypeExp ',' TupleTypes { $1 : $3 }


SizeExp :: { SizeExp NoInfo Name }
         : '[' Exp ']'    { SizeExp $2 (srcspan $1 $>) }
         | '['     ']'    { SizeExpAny (srcspan $1 $>) }
         | '...[' Exp ']' { SizeExp $2 (srcspan $1 $>) }
         | '...['     ']' { SizeExpAny (srcspan $1 $>) }

FunParam :: { PatBase NoInfo Name ParamType }
FunParam : ParamPat { fmap (toParam Observe) $1 }

FunParams1 :: { (PatBase NoInfo Name ParamType, [PatBase NoInfo Name ParamType]) }
FunParams1 : FunParam            { ($1, []) }
           | FunParam FunParams1 { ($1, fst $2 : snd $2) }

FunParams :: { [PatBase NoInfo Name ParamType ] }
FunParams :                     { [] }
           | FunParam FunParams { $1 : $2 }

QualName :: { (QualName Name, Loc) }
          : id
            { let L vloc (ID v) = $1 in (QualName [] v, vloc) }
          | QualName '.' id
            { let {L ploc (ID f) = $3; (QualName qs v,vloc) = $1;}
              in (QualName (qs++[v]) f, locOf (srcspan ploc vloc)) }

-- Expressions are divided into several layers.  The first distinction
-- (between Exp and Exp2) is to factor out ascription, which we do not
-- permit inside array slices (there is an ambiguity with
-- array slices).
Exp :: { UncheckedExp }
     : Exp ':' TypeExp  { Ascript $1 $3 (srcspan $1 $>) }
     | Exp ':>' TypeExp { Coerce $1 $3 NoInfo (srcspan $1 $>) }
     | Exp2 %prec ':'   { $1 }

Exp2 :: { UncheckedExp }
     : IfExp                { $1 }
     | LoopExp              { $1 }
     | LetExp %prec letprec { $1 }
     | MatchExp             { $1 }

     | assert Atom Atom    { Assert $2 $3 NoInfo (srcspan $1 $>) }
     | '#[' AttrInfo ']' Exp %prec bottom
                           { Attr $2 $4 (srcspan $1 $>) }

     | BinOpExp                  { $1 }
     | RangeExp                  { $1 }
     | Exp2 '..' Atom            {% twoDotsRange $2 }
     | Atom '..' Exp2            {% twoDotsRange $2 }
     | '-' Exp2  %prec juxtprec  { Negate $2 (srcspan $1 $>) }
     | '!' Exp2 %prec juxtprec   { Not $2 (srcspan $1 $>) }

     | Exp2 with '[' DimIndices ']' '=' Exp2
       { Update $1 $4 $7 (srcspan $1 $>) }
     | Exp2 with '...[' DimIndices ']' '=' Exp2
       { Update $1 $4 $7 (srcspan $1 $>) }

     | Exp2 with FieldAccesses_ '=' Exp2
       { RecordUpdate $1 (map fst $3) $5 NoInfo (srcspan $1 $>) }

     | '\\' FunParams1 maybeAscription(TypeExpTerm) '->' Exp %prec letprec
       { Lambda (fst $2 : snd $2) $5 $3 NoInfo (srcspan $1 $>) }

     | ApplyList {% applyExp $1 }

ApplyList :: { NE.NonEmpty UncheckedExp }
          : Atom ApplyList %prec juxtprec
            { NE.cons $1 $2 }
          | Atom %prec juxtprec
            { NE.singleton $1 }

Atom :: { UncheckedExp }
Atom : PrimLit        { Literal (fst $1) (srclocOf (snd $1)) }
     | Constr         { Constr (fst $1) [] NoInfo (srclocOf (snd $1)) }
     | charlit        { let L loc (CHARLIT x) = $1
                        in IntLit (toInteger (ord x)) NoInfo (srclocOf loc) }
     | intlit         { let L loc (INTLIT x) = $1 in IntLit x NoInfo (srclocOf loc) }
     | natlit         { let L loc (NATLIT _ x) = $1 in IntLit x NoInfo (srclocOf loc) }
     | floatlit       { let L loc (FLOATLIT x) = $1 in FloatLit x NoInfo (srclocOf loc) }
     | stringlit      { let L loc (STRINGLIT s) = $1 in
                        StringLit (BS.unpack (T.encodeUtf8 s)) (srclocOf loc) }
     | hole           { Hole NoInfo (srclocOf $1) }
     | '(' Exp ')'            { Parens $2 (srcspan $1 $>) }
     | '(' Exp ',' Exps1 ')'  { TupLit ($2 : $4) (srcspan $1 $>) }
     | '('      ')'           { TupLit [] (srcspan $1 $>) }
     | '[' Exps1 ']'          { ArrayLit $2 NoInfo (srcspan $1 $>) }
     | '['       ']'          { ArrayLit [] NoInfo (srcspan $1 $>) }

     | id { let L loc (ID v)  = $1 in Var (QualName [] v) NoInfo (srclocOf loc) }

     | Atom '.' id
       { let L ploc (ID f) = $3
         in case $1 of
              Var (QualName qs v) NoInfo vloc ->
                Var (QualName (qs++[v]) f) NoInfo (srcspan vloc ploc)
              _ ->
                Project f $1 NoInfo (srcspan $1 ploc) }
     | Atom '.' natlit
       { let L ploc (NATLIT f _) = $3
         in Project f $1 NoInfo (srcspan $1 ploc) }
     | Atom '.' '(' Exp ')'
       {% case $1 of
            Var qn NoInfo vloc ->
              pure (QualParens (qn, srclocOf vloc) $4 (srcspan vloc $>))
            _ ->
              parseErrorAt $3 (Just "Can only locally open module names, not arbitrary expressions")
        }
     | Atom '...[' DimIndices ']'
       { AppExp (Index $1 $3 (srcspan $1 $>)) NoInfo }
     | '{' Fields '}' { RecordLit $2 (srcspan $1 $>) }

     | SectionExp { $1 }

NumLit :: { (PrimValue, Loc) }
        : i8lit   { let L loc (I8LIT num)  = $1 in (SignedValue $ Int8Value num, loc) }
        | i16lit  { let L loc (I16LIT num) = $1 in (SignedValue $ Int16Value num, loc) }
        | i32lit  { let L loc (I32LIT num) = $1 in (SignedValue $ Int32Value num, loc) }
        | i64lit  { let L loc (I64LIT num) = $1 in (SignedValue $ Int64Value num, loc) }

        | u8lit  { let L loc (U8LIT num)  = $1 in (UnsignedValue $ Int8Value $ fromIntegral num, loc) }
        | u16lit { let L loc (U16LIT num) = $1 in (UnsignedValue $ Int16Value $ fromIntegral num, loc) }
        | u32lit { let L loc (U32LIT num) = $1 in (UnsignedValue $ Int32Value $ fromIntegral num, loc) }
        | u64lit { let L loc (U64LIT num) = $1 in (UnsignedValue $ Int64Value $ fromIntegral num, loc) }

        | f16lit { let L loc (F16LIT num) = $1 in (FloatValue $ Float16Value num, loc) }
        | f32lit { let L loc (F32LIT num) = $1 in (FloatValue $ Float32Value num, loc) }
        | f64lit { let L loc (F64LIT num) = $1 in (FloatValue $ Float64Value num, loc) }


PrimLit :: { (PrimValue, Loc) }
        : true   { (BoolValue True, $1) }
        | false  { (BoolValue False, $1) }
        | NumLit { $1 }

Exps1 :: { [UncheckedExp] }
       : Exps1_ { reverse $1 }

Exps1_ :: { [UncheckedExp] }
        : Exps1_ ',' Exp { $3 : $1 }
        | Exps1_ ','     { $1 }
        | Exp            { [$1] }

FieldAccesses :: { [(Name, Loc)] }
               : '.' FieldId FieldAccesses { $2 : $3 }
               |                           { [] }

FieldAccesses_ :: { [(Name, Loc)] }
               : FieldId FieldAccesses { (fst $1, snd $1) : $2 }

Field :: { FieldBase NoInfo Name }
       : FieldId '=' Exp { RecordFieldExplicit (fst $1) $3 (srcspan (snd $1) $>) }
       | id              { let L loc (ID s) = $1 in RecordFieldImplicit s NoInfo (srclocOf loc) }

Fields :: { [FieldBase NoInfo Name] }
       : Field ',' Fields { $1 : $3 }
       | Field            { [$1] }
       |                  { [] }

LetExp :: { UncheckedExp }
     : let SizeBinders1 Pat '=' Exp LetBody
       { AppExp (LetPat $2 $3 $5 $6 (srcspan $1 $>)) NoInfo }
     | let Pat '=' Exp LetBody
       { AppExp (LetPat [] $2 $4 $5 (srcspan $1 $>)) NoInfo }

     | let id LocalFunTypeParams FunParams1 maybeAscription(TypeExp) '=' Exp LetBody
       { let L _ (ID name) = $2
         in AppExp (LetFun name ($3, fst $4 : snd $4, $5, NoInfo, $7)
                    $8 (srcspan $1 $>))
                   NoInfo}

     | let id '...[' DimIndices ']' '=' Exp LetBody
       { let L vloc (ID v) = $2; ident = Ident v NoInfo (srclocOf vloc)
         in AppExp (LetWith ident ident $4 $7 $8 (srcspan $1 $>)) NoInfo }

LetBody :: { UncheckedExp }
    : in Exp %prec letprec { $2 }
    | LetExp %prec letprec { $1 }
    | def {% parseErrorAt $1 (Just "Unexpected \"def\" - missing \"in\"?") }
    | type {% parseErrorAt $1 (Just "Unexpected \"type\" - missing \"in\"?") }
    | module {% parseErrorAt $1 (Just "Unexpected \"module\" - missing \"in\"?") }

BinOpExp :: { UncheckedExp }
  : Exp2 '+...' Exp2    { binOp $1 $2 $3 }
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
  | Exp2 '<<...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '&...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '|...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '|' Exp2       { binOp $1 (L $2 (SYMBOL Bor [] (nameFromString "|"))) $3 }
  | Exp2 '&&...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '||...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '^...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '^' Exp2       { binOp $1 (L $2 (SYMBOL Xor [] (nameFromString "^"))) $3 }
  | Exp2 '==...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '!=...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '<...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '<=...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '>...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '>=...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '|>...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '<|...' Exp2   { binOp $1 $2 $3 }
  | Exp2 '<' Exp2       { binOp $1 (L $2 (SYMBOL Less [] (nameFromString "<"))) $3 }
  | Exp2 '!...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '=...' Exp2    { binOp $1 $2 $3 }
  | Exp2 '`' QualName '`' Exp2 { AppExp (BinOp (second srclocOf $3) NoInfo ($1, NoInfo) ($5, NoInfo) (srcspan $1 $>)) NoInfo }

SectionExp :: { UncheckedExp }
  : '(' '-' ')'
     { OpSection (qualName (nameFromString "-")) NoInfo (srcspan $1 $>) }
  | '(' Exp2 '-' ')'
    { OpSectionLeft (qualName (nameFromString "-"))
      NoInfo $2 (NoInfo, NoInfo) (NoInfo, NoInfo) (srcspan $1 $>) }
  | '(' BinOp Exp2 ')'
    { OpSectionRight (fst $2) NoInfo $3 (NoInfo, NoInfo) NoInfo (srcspan $1 $>) }
  | '(' Exp2 BinOp ')'
    { OpSectionLeft (fst $3) NoInfo $2 (NoInfo, NoInfo) (NoInfo, NoInfo) (srcspan $1 $>) }
  | '(' BinOp ')'
    { OpSection (fst $2) NoInfo (srcspan $1 $>) }

  | '(' '.' FieldAccesses_ ')'
    { ProjectSection (map fst $3) NoInfo (srcspan $1 $>) }

  | '(' '.' '[' DimIndices ']' ')'
    { IndexSection $4 NoInfo (srcspan $1 $>) }

RangeExp :: { UncheckedExp }
  : Exp2 '...' Exp2           { AppExp (Range $1 Nothing (ToInclusive $3) (srcspan $1 $>)) NoInfo }
  | Exp2 '..<' Exp2           { AppExp (Range $1 Nothing (UpToExclusive $3) (srcspan $1 $>)) NoInfo }
  | Exp2 '..>' Exp2           { AppExp (Range $1 Nothing (DownToExclusive $3)  (srcspan $1 $>)) NoInfo }
  | Exp2 '..' Exp2 '...' Exp2 { AppExp (Range $1 (Just $3) (ToInclusive $5) (srcspan $1 $>)) NoInfo }
  | Exp2 '..' Exp2 '..<' Exp2 { AppExp (Range $1 (Just $3) (UpToExclusive $5) (srcspan $1 $>)) NoInfo }
  | Exp2 '..' Exp2 '..>' Exp2 { AppExp (Range $1 (Just $3) (DownToExclusive $5) (srcspan $1 $>)) NoInfo }

IfExp :: { UncheckedExp }
       : if Exp then Exp else Exp %prec ifprec
         { AppExp (If $2 $4 $6 (srcspan $1 $>)) NoInfo }

LoopExp :: { UncheckedExp }
         : loop Pat LoopForm do Exp %prec ifprec
           {% fmap (\t -> AppExp (Loop [] (fmap (toParam Observe) $2) t $3 $5 (srcspan $1 $>)) NoInfo) (patternExp $2) }
         | loop Pat '=' Exp LoopForm do Exp %prec ifprec
           { AppExp (Loop [] (fmap (toParam Observe) $2) $4 $5 $7 (srcspan $1 $>)) NoInfo }

MatchExp :: { UncheckedExp }
          : match Exp Cases
            { let loc = srcspan $1 (NE.toList $>)
              in AppExp (Match $2 $> loc) NoInfo }

Cases :: { NE.NonEmpty (CaseBase NoInfo Name) }
       : Case  %prec caseprec { NE.singleton $1 }
       | Case Cases           { NE.cons $1 $2 }

Case :: { CaseBase NoInfo Name }
      : case Pat '->' Exp
        { let loc = srcspan $1 $> in CasePat $2 $> loc }

Pat :: { PatBase NoInfo Name StructType }
          : '#[' AttrInfo ']' Pat    { PatAttr $2 $4 (srcspan $1 $>) }
          | InnerPat ':' TypeExp     { PatAscription $1 $3 (srcspan $1 $>) }
          | InnerPat                 { $1 }
          | Constr ConstrFields       { let (n, loc) = $1;
                                            loc' = srcspan loc $>
                                        in PatConstr n NoInfo $2 loc'}

-- Parameter patterns are slightly restricted; see #2017.
ParamPat :: { PatBase NoInfo Name StructType }
               : id                   { let L loc (ID name) = $1 in Id name NoInfo (srclocOf loc) }
               | '(' BindingBinOp ')' { Id $2 NoInfo (srcspan $1 $>) }
               | '_'                  { Wildcard NoInfo (srclocOf $1) }
               | '(' ')'              { TuplePat [] (srcspan $1 $>) }
               | '(' Pat ')'          { PatParens $2 (srcspan $1 $>) }
               | '(' Pat ',' Pats1 ')'{ TuplePat ($2:$4) (srcspan $1 $>) }
               | '{' CFieldPats '}'   { RecordPat $2 (srcspan $1 $>) }
               | PatLiteralNoNeg      { PatLit (fst $1) NoInfo (srclocOf (snd $1)) }
               | Constr               { let (n, loc) = $1
                                        in PatConstr n NoInfo [] (srclocOf loc) }

Pats1 :: { [PatBase NoInfo Name StructType] }
           : Pat            { [$1] }
           | Pat ','       { [$1] }
           | Pat ',' Pats1 { $1 : $3 }

InnerPat :: { PatBase NoInfo Name StructType }
               : id                   { let L loc (ID name) = $1 in Id name NoInfo (srclocOf loc) }
               | '(' BindingBinOp ')' { Id $2 NoInfo (srcspan $1 $>) }
               | '_'                  { Wildcard NoInfo (srclocOf $1) }
               | '(' ')'              { TuplePat [] (srcspan $1 $>) }
               | '(' Pat ')'          { PatParens $2 (srcspan $1 $>) }
               | '(' Pat ',' Pats1 ')'{ TuplePat ($2:$4) (srcspan $1 $>) }
               | '{' CFieldPats '}'   { RecordPat $2 (srcspan $1 $>) }
               | PatLiteral           { PatLit (fst $1) NoInfo (srclocOf (snd $1)) }
               | Constr               { let (n, loc) = $1
                                        in PatConstr n NoInfo [] (srclocOf loc) }

ConstrFields :: { [PatBase NoInfo Name StructType] }
              : InnerPat                { [$1] }
              | ConstrFields InnerPat   { $1 ++ [$2] }

CFieldPat :: { (Name, PatBase NoInfo Name StructType) }
               : FieldId '=' Pat
               { (fst $1, $3) }
               | FieldId ':' TypeExp
               { (fst $1, PatAscription (Id (fst $1) NoInfo (srclocOf (snd $1))) $3 (srcspan (snd $1) $>)) }
               | FieldId
               { (fst $1, Id (fst $1) NoInfo (srclocOf (snd $1))) }

CFieldPats :: { [(Name, PatBase NoInfo Name StructType)] }
                : CFieldPats1 { $1 }
                |             { [] }

CFieldPats1 :: { [(Name, PatBase NoInfo Name StructType)] }
                 : CFieldPat ',' CFieldPats1 { $1 : $3 }
                 | CFieldPat ','             { [$1] }
                 | CFieldPat                 { [$1] }

PatLiteralNoNeg :: { (PatLit, Loc) }
             : charlit  { let L loc (CHARLIT x) = $1
                          in (PatLitInt (toInteger (ord x)), loc) }
             | PrimLit  { (PatLitPrim (fst $1), snd $1) }
             | intlit   { let L loc (INTLIT x) = $1 in (PatLitInt x, loc) }
             | natlit   { let L loc (NATLIT _ x) = $1 in (PatLitInt x, loc) }
             | floatlit { let L loc (FLOATLIT x) = $1 in (PatLitFloat x, loc) }

PatLiteral :: { (PatLit, Loc) }
             : PatLiteralNoNeg           { $1 }
             | '-' NumLit %prec bottom   { (PatLitPrim (primNegate (fst $2)), snd $2) }
             | '-' intlit %prec bottom   { let L loc (INTLIT x) = $2 in (PatLitInt (negate x), loc) }
             | '-' natlit %prec bottom   { let L loc (NATLIT _ x) = $2 in (PatLitInt (negate x), loc) }
             | '-' floatlit              { let L loc (FLOATLIT x) = $2 in (PatLitFloat (negate x), loc) }

LoopForm :: { LoopFormBase NoInfo Name }
LoopForm : for VarId '<' Exp
           { For $2 $4 }
         | for Pat in Exp
           { ForIn $2 $4 }
         | while Exp
           { While $2 }

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
             :                         { [] }
             | DimIndex                { [$1] }
             | DimIndex ',' DimIndices { $1 : $3 }

VarId :: { IdentBase NoInfo Name StructType }
VarId : id { let L loc (ID name) = $1 in Ident name NoInfo (srclocOf loc) }

FieldId :: { (Name, Loc) }
         : id     { let L loc (ID name) = $1 in (name, loc) }
         | natlit { let L loc (NATLIT x _) = $1 in (x, loc) }

maybeAscription(p) : ':' p { Just $2 }
                   |       { Nothing }

AttrAtom :: { (AttrAtom Name, Loc) }
          : id     { let L loc (ID s) =     $1 in (AtomName s, loc) }
          | intlit { let L loc (INTLIT x) = $1 in (AtomInt x, loc) }
          | natlit { let L loc (NATLIT _ x) = $1 in (AtomInt x, loc) }

AttrInfo :: { AttrInfo Name }
         : AttrAtom         { let (x,y) = $1 in AttrAtom x (srclocOf y) }
         | id '(' Attrs ')' { let L _ (ID s) = $1 in AttrComp s $3 (srcspan $1 $>) }

Attrs :: { [AttrInfo Name] }
       :                    { [] }
       | AttrInfo           { [$1] }
       | AttrInfo ',' Attrs { $1 : $3 }
