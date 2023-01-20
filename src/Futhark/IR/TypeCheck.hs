{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type checker checks whether the program is type-consistent.
module Futhark.IR.TypeCheck
  ( -- * Interface
    checkProg,
    TypeError (..),
    ErrorCase (..),

    -- * Extensionality
    TypeM,
    bad,
    context,
    Checkable (..),
    lookupVar,
    lookupAliases,
    checkOpWith,

    -- * Checkers
    require,
    requireI,
    requirePrimExp,
    checkSubExp,
    checkCerts,
    checkExp,
    checkStms,
    checkStm,
    checkType,
    checkExtType,
    matchExtPat,
    matchExtBranchType,
    argType,
    noArgAliases,
    checkArg,
    checkSOACArrayArgs,
    checkLambda,
    checkBody,
    consume,
    binding,
    alternative,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Parallel.Strategies
import Data.List (find, intercalate, isPrefixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Futhark.Analysis.Alias
import Futhark.Analysis.PrimExp
import Futhark.Construct (instantiateShapes)
import Futhark.IR.Aliases hiding (lookupAliases)
import Futhark.Util
import Futhark.Util.Pretty (align, docText, indent, ppTuple', pretty, (<+>), (</>))

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data ErrorCase rep
  = TypeError T.Text
  | UnexpectedType (Exp rep) Type [Type]
  | ReturnTypeError Name [ExtType] [ExtType]
  | DupDefinitionError Name
  | DupParamError Name VName
  | DupPatError VName
  | InvalidPatError (Pat (LetDec (Aliases rep))) [ExtType] (Maybe String)
  | UnknownVariableError VName
  | UnknownFunctionError Name
  | ParameterMismatch (Maybe Name) [Type] [Type]
  | SlicingError Int Int
  | BadAnnotation String Type Type
  | ReturnAliased Name VName
  | UniqueReturnAliased Name
  | NotAnArray VName Type
  | PermutationError [Int] Int (Maybe VName)

instance Checkable rep => Show (ErrorCase rep) where
  show (TypeError msg) =
    "Type error:\n" ++ T.unpack msg
  show (UnexpectedType e _ []) =
    "Type of expression\n"
      ++ T.unpack (docText $ indent 2 $ pretty e)
      ++ "\ncannot have any type - possibly a bug in the type checker."
  show (UnexpectedType e t ts) =
    "Type of expression\n"
      ++ T.unpack (docText $ indent 2 $ pretty e)
      ++ "\nmust be one of "
      ++ intercalate ", " (map prettyString ts)
      ++ ", but is "
      ++ prettyString t
      ++ "."
  show (ReturnTypeError fname rettype bodytype) =
    "Declaration of function "
      ++ nameToString fname
      ++ " declares return type\n  "
      ++ T.unpack (prettyTuple rettype)
      ++ "\nBut body has type\n  "
      ++ T.unpack (prettyTuple bodytype)
  show (DupDefinitionError name) =
    "Duplicate definition of function " ++ nameToString name ++ ""
  show (DupParamError funname paramname) =
    "Parameter "
      ++ prettyString paramname
      ++ " mentioned multiple times in argument list of function "
      ++ nameToString funname
      ++ "."
  show (DupPatError name) =
    "Variable " ++ prettyString name ++ " bound twice in pattern."
  show (InvalidPatError pat t desc) =
    "Pat\n"
      ++ prettyString pat
      ++ "\ncannot match value of type\n"
      ++ T.unpack (prettyTupleLines t)
      ++ end
    where
      end = case desc of
        Nothing -> "."
        Just desc' -> ":\n" ++ desc'
  show (UnknownVariableError name) =
    "Use of unknown variable " ++ prettyString name ++ "."
  show (UnknownFunctionError fname) =
    "Call of unknown function " ++ nameToString fname ++ "."
  show (ParameterMismatch fname expected got) =
    "In call of "
      ++ fname'
      ++ ":\n"
      ++ "expecting "
      ++ show nexpected
      ++ " arguments of type(s)\n"
      ++ intercalate ", " (map prettyString expected)
      ++ "\nGot "
      ++ show ngot
      ++ " arguments of types\n"
      ++ intercalate ", " (map prettyString got)
    where
      nexpected = length expected
      ngot = length got
      fname' = maybe "anonymous function" (("function " ++) . nameToString) fname
  show (SlicingError dims got) =
    show got ++ " indices given, but type of indexee has " ++ show dims ++ " dimension(s)."
  show (BadAnnotation desc expected got) =
    "Annotation of \""
      ++ desc
      ++ "\" type of expression is "
      ++ prettyString expected
      ++ ", but derived to be "
      ++ prettyString got
      ++ "."
  show (ReturnAliased fname name) =
    "Unique return value of function "
      ++ nameToString fname
      ++ " is aliased to "
      ++ prettyString name
      ++ ", which is not consumed."
  show (UniqueReturnAliased fname) =
    "A unique tuple element of return value of function "
      ++ nameToString fname
      ++ " is aliased to some other tuple component."
  show (NotAnArray e t) =
    "The expression "
      ++ prettyString e
      ++ " is expected to be an array, but is "
      ++ prettyString t
      ++ "."
  show (PermutationError perm rank name) =
    "The permutation ("
      ++ intercalate ", " (map show perm)
      ++ ") is not valid for array "
      ++ name'
      ++ "of rank "
      ++ show rank
      ++ "."
    where
      name' = maybe "" ((++ " ") . prettyString) name

-- | A type error.
data TypeError rep = Error [T.Text] (ErrorCase rep)

instance Checkable rep => Show (TypeError rep) where
  show (Error [] err) =
    show err
  show (Error msgs err) =
    intercalate "\n" (map T.unpack msgs) ++ "\n" ++ show err

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding rep = ([RetType (Aliases rep)], [FParam (Aliases rep)])

type VarBinding rep = NameInfo (Aliases rep)

data Usage
  = Consumed
  | Observed
  deriving (Eq, Ord, Show)

data Occurence = Occurence
  { observed :: Names,
    consumed :: Names
  }
  deriving (Eq, Show)

observation :: Names -> Occurence
observation = flip Occurence mempty

consumption :: Names -> Occurence
consumption = Occurence mempty

nullOccurence :: Occurence -> Bool
nullOccurence occ = observed occ == mempty && consumed occ == mempty

type Occurences = [Occurence]

allConsumed :: Occurences -> Names
allConsumed = mconcat . map consumed

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) (map filt occurs1) ++ occurs2
  where
    filt occ =
      occ {observed = observed occ `namesSubtract` postcons}
    postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) (map filt occurs1) ++ occurs2
  where
    filt occ =
      occ
        { consumed = consumed occ `namesSubtract` postcons,
          observed = observed occ `namesSubtract` postcons
        }
    postcons = allConsumed occurs2

unOccur :: Names -> Occurences -> Occurences
unOccur to_be_removed = filter (not . nullOccurence) . map unOccur'
  where
    unOccur' occ =
      occ
        { observed = observed occ `namesSubtract` to_be_removed,
          consumed = consumed occ `namesSubtract` to_be_removed
        }

-- | The 'Consumption' data structure is used to keep track of which
-- variables have been consumed, as well as whether a violation has been detected.
data Consumption
  = ConsumptionError T.Text
  | Consumption Occurences
  deriving (Show)

instance Semigroup Consumption where
  ConsumptionError e <> _ = ConsumptionError e
  _ <> ConsumptionError e = ConsumptionError e
  Consumption o1 <> Consumption o2
    | v : _ <- namesToList $ consumed_in_o1 `namesIntersection` used_in_o2 =
        ConsumptionError $ "Variable " <> prettyText v <> " referenced after being consumed."
    | otherwise =
        Consumption $ o1 `seqOccurences` o2
    where
      consumed_in_o1 = mconcat $ map consumed o1
      used_in_o2 = mconcat $ map consumed o2 <> map observed o2

instance Monoid Consumption where
  mempty = Consumption mempty

-- | The environment contains a variable table and a function table.
-- Type checking happens with access to this environment.  The
-- function table is only initialised at the very beginning, but the
-- variable table will be extended during type-checking when
-- let-expressions are encountered.
data Env rep = Env
  { envVtable :: M.Map VName (VarBinding rep),
    envFtable :: M.Map Name (FunBinding rep),
    envCheckOp :: Op (Aliases rep) -> TypeM rep (),
    envContext :: [T.Text]
  }

data TState = TState
  { stateNames :: Names,
    stateCons :: Consumption
  }

-- | The type checker runs in this monad.
newtype TypeM rep a
  = TypeM (ReaderT (Env rep) (StateT TState (Either (TypeError rep))) a)
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader (Env rep),
      MonadState TState
    )

instance
  Checkable rep =>
  HasScope (Aliases rep) (TypeM rep)
  where
  lookupType = fmap typeOf . lookupVar
  askScope = asks $ M.fromList . mapMaybe varType . M.toList . envVtable
    where
      varType (name, dec) = Just (name, dec)

runTypeM ::
  Env rep ->
  TypeM rep a ->
  Either (TypeError rep) a
runTypeM env (TypeM m) =
  evalStateT (runReaderT m env) (TState mempty mempty)

-- | Signal a type error.
bad :: ErrorCase rep -> TypeM rep a
bad e = do
  messages <- asks envContext
  TypeM $ lift $ lift $ Left $ Error (reverse messages) e

tell :: Consumption -> TypeM rep ()
tell cons = modify $ \s -> s {stateCons = stateCons s <> cons}

-- | Add information about what is being type-checked to the current
-- context.  Liberal use of this combinator makes it easier to track
-- type errors, as the strings are added to type errors signalled via
-- 'bad'.
context ::
  T.Text ->
  TypeM rep a ->
  TypeM rep a
context s = local $ \env -> env {envContext = s : envContext env}

message :: Pretty a => T.Text -> a -> T.Text
message s x = docText $ pretty s <+> align (pretty x)

-- | Mark a name as bound.  If the name has been bound previously in
-- the program, report a type error.
bound :: VName -> TypeM rep ()
bound name = do
  already_seen <- gets $ nameIn name . stateNames
  when already_seen . bad . TypeError $
    "Name " <> prettyText name <> " bound twice"
  modify $ \s -> s {stateNames = oneName name <> stateNames s}

occur :: Occurences -> TypeM rep ()
occur = tell . Consumption . filter (not . nullOccurence)

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe ::
  Checkable rep =>
  VName ->
  TypeM rep ()
observe name = do
  dec <- lookupVar name
  unless (primType $ typeOf dec) $
    occur [observation $ oneName name <> aliases dec]

-- | Proclaim that we have written to the given variables.
consume :: Checkable rep => Names -> TypeM rep ()
consume als = do
  scope <- askScope
  let isArray = maybe False (not . primType . typeOf) . (`M.lookup` scope)
  occur [consumption $ namesFromList $ filter isArray $ namesToList als]

collectOccurences :: TypeM rep a -> TypeM rep (a, Occurences)
collectOccurences m = do
  old <- gets stateCons
  modify $ \s -> s {stateCons = mempty}
  x <- m
  new <- gets stateCons
  modify $ \s -> s {stateCons = old}
  o <- checkConsumption new
  pure (x, o)

checkOpWith ::
  (Op (Aliases rep) -> TypeM rep ()) ->
  TypeM rep a ->
  TypeM rep a
checkOpWith checker = local $ \env -> env {envCheckOp = checker}

checkConsumption :: Consumption -> TypeM rep Occurences
checkConsumption (ConsumptionError e) = bad $ TypeError e
checkConsumption (Consumption os) = pure os

-- | Type check two mutually exclusive control flow branches.  Think
-- @if@.  This interacts with consumption checking, as it is OK for an
-- array to be consumed in both branches.
alternative :: TypeM rep a -> TypeM rep b -> TypeM rep (a, b)
alternative m1 m2 = do
  (x, os1) <- collectOccurences m1
  (y, os2) <- collectOccurences m2
  tell $ Consumption $ os1 `altOccurences` os2
  pure (x, y)

alternatives :: [TypeM rep ()] -> TypeM rep ()
alternatives [] = pure ()
alternatives (x : xs) = void $ x `alternative` alternatives xs

-- | Permit consumption of only the specified names.  If one of these
-- names is consumed, the consumption will be rewritten to be a
-- consumption of the corresponding alias set.  Consumption of
-- anything else will result in a type error.
consumeOnlyParams :: [(VName, Names)] -> TypeM rep a -> TypeM rep a
consumeOnlyParams consumable m = do
  (x, os) <- collectOccurences m
  tell . Consumption =<< mapM inspect os
  pure x
  where
    inspect o = do
      new_consumed <- mconcat <$> mapM wasConsumed (namesToList $ consumed o)
      pure o {consumed = new_consumed}
    wasConsumed v
      | Just als <- lookup v consumable = pure als
      | otherwise =
          bad . TypeError . T.unlines $
            [ prettyText v <> " was invalidly consumed.",
              what <> " can be consumed here."
            ]
    what
      | null consumable = "Nothing"
      | otherwise = "Only " <> T.intercalate ", " (map (prettyText . fst) consumable)

-- | Given the immediate aliases, compute the full transitive alias
-- set (including the immediate aliases).
expandAliases :: Names -> Env rep -> Names
expandAliases names env = names <> aliasesOfAliases
  where
    aliasesOfAliases = mconcat . map look . namesToList $ names
    look k = case M.lookup k $ envVtable env of
      Just (LetName (als, _)) -> unAliases als
      _ -> mempty

binding ::
  Checkable rep =>
  Scope (Aliases rep) ->
  TypeM rep a ->
  TypeM rep a
binding stms = check . local (`bindVars` stms)
  where
    bindVars = M.foldlWithKey' bindVar
    boundnames = M.keys stms

    bindVar env name (LetName (AliasDec als, dec)) =
      let als'
            | primType (typeOf dec) = mempty
            | otherwise = expandAliases als env
       in env
            { envVtable =
                M.insert name (LetName (AliasDec als', dec)) $ envVtable env
            }
    bindVar env name dec =
      env {envVtable = M.insert name dec $ envVtable env}

    -- Check whether the bound variables have been used correctly
    -- within their scope.
    check m = do
      mapM_ bound $ M.keys stms
      (a, os) <- collectOccurences m
      tell $ Consumption $ unOccur (namesFromList boundnames) os
      pure a

lookupVar :: VName -> TypeM rep (NameInfo (Aliases rep))
lookupVar name = do
  stm <- asks $ M.lookup name . envVtable
  case stm of
    Nothing -> bad $ UnknownVariableError name
    Just dec -> pure dec

lookupAliases :: Checkable rep => VName -> TypeM rep Names
lookupAliases name = do
  info <- lookupVar name
  pure $
    if primType $ typeOf info
      then mempty
      else oneName name <> aliases info

aliases :: NameInfo (Aliases rep) -> Names
aliases (LetName (als, _)) = unAliases als
aliases _ = mempty

subExpAliasesM :: Checkable rep => SubExp -> TypeM rep Names
subExpAliasesM Constant {} = pure mempty
subExpAliasesM (Var v) = lookupAliases v

lookupFun ::
  Checkable rep =>
  Name ->
  [SubExp] ->
  TypeM rep ([RetType rep], [DeclType])
lookupFun fname args = do
  stm <- asks $ M.lookup fname . envFtable
  case stm of
    Nothing -> bad $ UnknownFunctionError fname
    Just (ftype, params) -> do
      argts <- mapM subExpType args
      case applyRetType ftype params $ zip args argts of
        Nothing ->
          bad $ ParameterMismatch (Just fname) (map paramType params) argts
        Just rt ->
          pure (rt, map paramDeclType params)

-- | @checkAnnotation loc s t1 t2@ checks if @t2@ is equal to
-- @t1@.  If not, a 'BadAnnotation' is raised.
checkAnnotation ::
  String ->
  Type ->
  Type ->
  TypeM rep ()
checkAnnotation desc t1 t2
  | t2 == t1 = pure ()
  | otherwise = bad $ BadAnnotation desc t1 t2

-- | @require ts se@ causes a '(TypeError vn)' if the type of @se@ is
-- not a subtype of one of the types in @ts@.
require :: Checkable rep => [Type] -> SubExp -> TypeM rep ()
require ts se = do
  t <- checkSubExp se
  unless (t `elem` ts) $
    bad $
      UnexpectedType (BasicOp $ SubExp se) t ts

-- | Variant of 'require' working on variable names.
requireI :: Checkable rep => [Type] -> VName -> TypeM rep ()
requireI ts ident = require ts $ Var ident

checkArrIdent ::
  Checkable rep =>
  VName ->
  TypeM rep (Shape, PrimType)
checkArrIdent v = do
  t <- lookupType v
  case t of
    Array pt shape _ -> pure (shape, pt)
    _ -> bad $ NotAnArray v t

checkAccIdent ::
  Checkable rep =>
  VName ->
  TypeM rep (Shape, [Type])
checkAccIdent v = do
  t <- lookupType v
  case t of
    Acc _ ispace ts _ ->
      pure (ispace, ts)
    _ ->
      bad . TypeError $
        prettyText v
          <> " should be an accumulator but is of type "
          <> prettyText t

checkOpaques :: OpaqueTypes -> Either (TypeError rep) ()
checkOpaques (OpaqueTypes types) = descend [] types
  where
    descend _ [] = pure ()
    descend known ((name, t) : ts) = do
      check known t
      descend (name : known) ts
    check known (OpaqueRecord fs) =
      mapM_ (checkEntryPointType known . snd) fs
    check _ (OpaqueType _) =
      pure ()
    checkEntryPointType known (TypeOpaque s) =
      when (s `notElem` known) $
        Left . Error [] . TypeError $
          "Opaque not defined before first use: " <> nameToText s
    checkEntryPointType _ (TypeTransparent _) = pure ()

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg ::
  Checkable rep =>
  Prog (Aliases rep) ->
  Either (TypeError rep) ()
checkProg (Prog opaques consts funs) = do
  checkOpaques opaques
  let typeenv =
        Env
          { envVtable = M.empty,
            envFtable = mempty,
            envContext = [],
            envCheckOp = checkOp
          }
  let const_names = foldMap (patNames . stmPat) consts
      onFunction ftable vtable fun = runTypeM typeenv $ do
        modify $ \s -> s {stateNames = namesFromList const_names}
        local (\env -> env {envFtable = ftable, envVtable = vtable}) $
          checkFun fun
  ftable <-
    runTypeM typeenv buildFtable
  vtable <-
    runTypeM typeenv {envFtable = ftable} $ checkStms consts $ asks envVtable
  sequence_ $ parMap rpar (onFunction ftable vtable) funs
  where
    buildFtable = do
      table <- initialFtable
      foldM expand table funs
    expand ftable (FunDef _ _ name ret params _)
      | M.member name ftable =
          bad $ DupDefinitionError name
      | otherwise =
          pure $ M.insert name (ret, params) ftable

initialFtable ::
  Checkable rep =>
  TypeM rep (M.Map Name (FunBinding rep))
initialFtable = fmap M.fromList $ mapM addBuiltin $ M.toList builtInFunctions
  where
    addBuiltin (fname, (t, ts)) = do
      ps <- mapM (primFParam name) ts
      pure (fname, ([primRetType t], ps))
    name = VName (nameFromString "x") 0

checkFun ::
  Checkable rep =>
  FunDef (Aliases rep) ->
  TypeM rep ()
checkFun (FunDef _ _ fname rettype params body) =
  context ("In function " <> nameToText fname)
    $ checkFun'
      ( fname,
        map declExtTypeOf rettype,
        funParamsToNameInfos params
      )
      (Just consumable)
    $ do
      checkFunParams params
      checkRetType rettype
      context "When checking function body" $ checkFunBody rettype body
  where
    consumable =
      [ (paramName param, mempty)
        | param <- params,
          unique $ paramDeclType param
      ]

funParamsToNameInfos ::
  [FParam rep] ->
  [(VName, NameInfo (Aliases rep))]
funParamsToNameInfos = map nameTypeAndDec
  where
    nameTypeAndDec fparam =
      ( paramName fparam,
        FParamName $ paramDec fparam
      )

checkFunParams ::
  Checkable rep =>
  [FParam rep] ->
  TypeM rep ()
checkFunParams = mapM_ $ \param ->
  context ("In function parameter " <> prettyText param) $
    checkFParamDec (paramName param) (paramDec param)

checkLambdaParams ::
  Checkable rep =>
  [LParam rep] ->
  TypeM rep ()
checkLambdaParams = mapM_ $ \param ->
  context ("In lambda parameter " <> prettyText param) $
    checkLParamDec (paramName param) (paramDec param)

checkFun' ::
  Checkable rep =>
  ( Name,
    [DeclExtType],
    [(VName, NameInfo (Aliases rep))]
  ) ->
  Maybe [(VName, Names)] ->
  TypeM rep [Names] ->
  TypeM rep ()
checkFun' (fname, rettype, params) consumable check = do
  checkNoDuplicateParams
  binding (M.fromList params) $
    maybe id consumeOnlyParams consumable $ do
      body_aliases <- check
      scope <- askScope
      let isArray = maybe False ((> 0) . arrayRank . typeOf) . (`M.lookup` scope)
      context
        ( "When checking the body aliases: "
            <> prettyText (map namesToList body_aliases)
        )
        $ checkReturnAlias
        $ map (namesFromList . filter isArray . namesToList) body_aliases
  where
    param_names = map fst params

    checkNoDuplicateParams = foldM_ expand [] param_names

    expand seen pname
      | Just _ <- find (== pname) seen =
          bad $ DupParamError fname pname
      | otherwise =
          pure $ pname : seen
    checkReturnAlias =
      foldM_ checkReturnAlias' mempty . returnAliasing rettype

    checkReturnAlias' seen (Unique, names)
      | any (`S.member` S.map fst seen) $ namesToList names =
          bad $ UniqueReturnAliased fname
      | otherwise = do
          consume names
          pure $ seen <> tag Unique names
    checkReturnAlias' seen (Nonunique, names)
      | any (`S.member` seen) $ tag Unique names =
          bad $ UniqueReturnAliased fname
      | otherwise = pure $ seen <> tag Nonunique names

    tag u = S.fromList . map (,u) . namesToList

    returnAliasing expected got =
      reverse $
        zip (reverse (map uniqueness expected) ++ repeat Nonunique) $
          reverse got

checkSubExp :: Checkable rep => SubExp -> TypeM rep Type
checkSubExp (Constant val) =
  pure $ Prim $ primValueType val
checkSubExp (Var ident) = context ("In subexp " <> prettyText ident) $ do
  observe ident
  lookupType ident

checkCerts :: Checkable rep => Certs -> TypeM rep ()
checkCerts (Certs cs) = mapM_ (requireI [Prim Unit]) cs

checkSubExpRes :: Checkable rep => SubExpRes -> TypeM rep Type
checkSubExpRes (SubExpRes cs se) = do
  checkCerts cs
  checkSubExp se

checkStms ::
  Checkable rep =>
  Stms (Aliases rep) ->
  TypeM rep a ->
  TypeM rep a
checkStms origstms m = delve $ stmsToList origstms
  where
    delve (stm@(Let pat _ e) : stms) = do
      context (docText $ "In expression of statement" </> indent 2 (pretty pat)) $
        checkExp e
      checkStm stm $
        delve stms
    delve [] =
      m

checkResult ::
  Checkable rep =>
  Result ->
  TypeM rep ()
checkResult = mapM_ checkSubExpRes

checkFunBody ::
  Checkable rep =>
  [RetType rep] ->
  Body (Aliases rep) ->
  TypeM rep [Names]
checkFunBody rt (Body (_, rep) stms res) = do
  checkBodyDec rep
  checkStms stms $ do
    context "When checking body result" $ checkResult res
    context "When matching declared return type to result of body" $
      matchReturnType rt res
    map (`namesSubtract` bound_here) <$> mapM (subExpAliasesM . resSubExp) res
  where
    bound_here = namesFromList $ M.keys $ scopeOf stms

checkLambdaBody ::
  Checkable rep =>
  [Type] ->
  Body (Aliases rep) ->
  TypeM rep [Names]
checkLambdaBody ret (Body (_, rep) stms res) = do
  checkBodyDec rep
  checkStms stms $ do
    checkLambdaResult ret res
    map (`namesSubtract` bound_here) <$> mapM (subExpAliasesM . resSubExp) res
  where
    bound_here = namesFromList $ M.keys $ scopeOf stms

checkLambdaResult ::
  Checkable rep =>
  [Type] ->
  Result ->
  TypeM rep ()
checkLambdaResult ts es
  | length ts /= length es =
      bad . TypeError $
        "Lambda has return type "
          <> prettyTuple ts
          <> " describing "
          <> prettyText (length ts)
          <> " values, but body returns "
          <> prettyText (length es)
          <> " values: "
          <> prettyTuple es
  | otherwise = forM_ (zip ts es) $ \(t, e) -> do
      et <- checkSubExpRes e
      unless (et == t) . bad . TypeError $
        "Subexpression "
          <> prettyText e
          <> " has type "
          <> prettyText et
          <> " but expected "
          <> prettyText t

checkBody ::
  Checkable rep =>
  Body (Aliases rep) ->
  TypeM rep [Names]
checkBody (Body (_, rep) stms res) = do
  checkBodyDec rep
  checkStms stms $ do
    checkResult res
    map (`namesSubtract` bound_here) <$> mapM (subExpAliasesM . resSubExp) res
  where
    bound_here = namesFromList $ M.keys $ scopeOf stms

checkBasicOp :: Checkable rep => BasicOp -> TypeM rep ()
checkBasicOp (SubExp es) =
  void $ checkSubExp es
checkBasicOp (Opaque _ es) =
  void $ checkSubExp es
checkBasicOp (ArrayLit [] _) =
  pure ()
checkBasicOp (ArrayLit (e : es') t) = do
  let check elemt eleme = do
        elemet <- checkSubExp eleme
        unless (elemet == elemt) . bad . TypeError $
          prettyText elemet
            <> " is not of expected type "
            <> prettyText elemt
            <> "."
  et <- checkSubExp e

  -- Compare that type with the one given for the array literal.
  checkAnnotation "array-element" t et

  mapM_ (check et) es'
checkBasicOp (UnOp op e) = require [Prim $ unOpType op] e
checkBasicOp (BinOp op e1 e2) = checkBinOpArgs (binOpType op) e1 e2
checkBasicOp (CmpOp op e1 e2) = checkCmpOp op e1 e2
checkBasicOp (ConvOp op e) = require [Prim $ fst $ convOpType op] e
checkBasicOp (Index ident (Slice idxes)) = do
  vt <- lookupType ident
  observe ident
  when (arrayRank vt /= length idxes) $
    bad $
      SlicingError (arrayRank vt) (length idxes)
  mapM_ checkDimIndex idxes
checkBasicOp (Update _ src (Slice idxes) se) = do
  (src_shape, src_pt) <- checkArrIdent src
  when (shapeRank src_shape /= length idxes) $
    bad $
      SlicingError (shapeRank src_shape) (length idxes)

  se_aliases <- subExpAliasesM se
  when (src `nameIn` se_aliases) $
    bad $
      TypeError "The target of an Update must not alias the value to be written."

  mapM_ checkDimIndex idxes
  require [arrayOf (Prim src_pt) (Shape (sliceDims (Slice idxes))) NoUniqueness] se
  consume =<< lookupAliases src
checkBasicOp (FlatIndex ident slice) = do
  vt <- lookupType ident
  observe ident
  when (arrayRank vt /= 1) $
    bad $
      SlicingError (arrayRank vt) 1
  checkFlatSlice slice
checkBasicOp (FlatUpdate src slice v) = do
  (src_shape, src_pt) <- checkArrIdent src
  when (shapeRank src_shape /= 1) $
    bad $
      SlicingError (shapeRank src_shape) 1

  v_aliases <- lookupAliases v
  when (src `nameIn` v_aliases) $
    bad $
      TypeError "The target of an Update must not alias the value to be written."

  checkFlatSlice slice
  requireI [arrayOf (Prim src_pt) (Shape (flatSliceDims slice)) NoUniqueness] v
  consume =<< lookupAliases src
checkBasicOp (Iota e x s et) = do
  require [Prim int64] e
  require [Prim $ IntType et] x
  require [Prim $ IntType et] s
checkBasicOp (Replicate (Shape dims) valexp) = do
  mapM_ (require [Prim int64]) dims
  void $ checkSubExp valexp
checkBasicOp (Scratch _ shape) =
  mapM_ checkSubExp shape
checkBasicOp (Reshape k newshape arrexp) = do
  rank <- shapeRank . fst <$> checkArrIdent arrexp
  mapM_ (require [Prim int64]) $ shapeDims newshape
  case k of
    ReshapeCoerce ->
      when (shapeRank newshape /= rank) . bad $
        TypeError "Coercion changes rank of array."
    ReshapeArbitrary ->
      pure ()
checkBasicOp (Rearrange perm arr) = do
  arrt <- lookupType arr
  let rank = arrayRank arrt
  when (length perm /= rank || sort perm /= [0 .. rank - 1]) $
    bad $
      PermutationError perm rank $
        Just arr
checkBasicOp (Rotate rots arr) = do
  arrt <- lookupType arr
  let rank = arrayRank arrt
  mapM_ (require [Prim int64]) rots
  when (length rots /= rank) $
    bad $
      TypeError $
        "Cannot rotate "
          <> prettyText (length rots)
          <> " dimensions of "
          <> prettyText rank
          <> "-dimensional array."
checkBasicOp (Concat i (arr1exp :| arr2exps) ressize) = do
  arr1_dims <- shapeDims . fst <$> checkArrIdent arr1exp
  arr2s_dims <- map (shapeDims . fst) <$> mapM checkArrIdent arr2exps
  unless (all ((== dropAt i 1 arr1_dims) . dropAt i 1) arr2s_dims) $
    bad $
      TypeError "Types of arguments to concat do not match."
  require [Prim int64] ressize
checkBasicOp (Copy e) =
  void $ checkArrIdent e
checkBasicOp (Manifest perm arr) =
  checkBasicOp $ Rearrange perm arr -- Basically same thing!
checkBasicOp (Assert e (ErrorMsg parts) _) = do
  require [Prim Bool] e
  mapM_ checkPart parts
  where
    checkPart ErrorString {} = pure ()
    checkPart (ErrorVal t x) = require [Prim t] x
checkBasicOp (UpdateAcc acc is ses) = do
  (shape, ts) <- checkAccIdent acc

  unless (length ses == length ts) . bad . TypeError $
    "Accumulator requires "
      <> prettyText (length ts)
      <> " values, but "
      <> prettyText (length ses)
      <> " provided."

  unless (length is == shapeRank shape) $
    bad . TypeError $
      "Accumulator requires "
        <> prettyText (shapeRank shape)
        <> " indices, but "
        <> prettyText (length is)
        <> " provided."

  zipWithM_ require (map pure ts) ses
  consume =<< lookupAliases acc

matchLoopResultExt ::
  Checkable rep =>
  [Param DeclType] ->
  Result ->
  TypeM rep ()
matchLoopResultExt merge loopres = do
  let rettype_ext =
        existentialiseExtTypes (map paramName merge) $
          staticShapes $
            map typeOf merge

  bodyt <- mapM subExpResType loopres

  case instantiateShapes (fmap resSubExp . (`maybeNth` loopres)) rettype_ext of
    Nothing ->
      bad $
        ReturnTypeError
          (nameFromString "<loop body>")
          rettype_ext
          (staticShapes bodyt)
    Just rettype' ->
      unless (bodyt `subtypesOf` rettype') . bad $
        ReturnTypeError
          (nameFromString "<loop body>")
          (staticShapes rettype')
          (staticShapes bodyt)

checkExp ::
  Checkable rep =>
  Exp (Aliases rep) ->
  TypeM rep ()
checkExp (BasicOp op) = checkBasicOp op
checkExp (Match ses cases def_case info) = do
  ses_ts <- mapM checkSubExp ses
  alternatives $
    context "in body of last case" (checkCaseBody def_case)
      : map (checkCase ses_ts) cases
  where
    checkVal t (Just v) = Prim (primValueType v) == t
    checkVal _ Nothing = True
    checkCase ses_ts (Case vs body) = do
      let ok = length vs == length ses_ts && and (zipWith checkVal ses_ts vs)
      unless ok . bad . TypeError . docText $
        "Scrutinee"
          </> indent 2 (ppTuple' $ map pretty ses)
          </> "cannot match pattern"
          </> indent 2 (ppTuple' $ map pretty vs)
      context ("in body of case " <> prettyTuple vs) $ checkCaseBody body
    checkCaseBody body = do
      void $ checkBody body
      matchBranchType (matchReturns info) body
checkExp (Apply fname args rettype_annot _) = do
  (rettype_derived, paramtypes) <- lookupFun fname $ map fst args
  argflows <- mapM (checkArg . fst) args
  when (rettype_derived /= rettype_annot) $
    bad . TypeError . docText $
      "Expected apply result type:"
        </> indent 2 (pretty rettype_derived)
        </> "But annotation is:"
        </> indent 2 (pretty rettype_annot)
  consumeArgs paramtypes argflows
checkExp (DoLoop merge form loopbody) = do
  let (mergepat, mergeexps) = unzip merge
  mergeargs <- mapM checkArg mergeexps

  checkLoopArgs

  binding (scopeOf form) $ do
    form_consumable <- checkForm mergeargs form

    let rettype = map paramDeclType mergepat
        consumable =
          [ (paramName param, mempty)
            | param <- mergepat,
              unique $ paramDeclType param
          ]
            ++ form_consumable

    context "Inside the loop body"
      $ checkFun'
        ( nameFromString "<loop body>",
          staticShapes rettype,
          funParamsToNameInfos mergepat
        )
        (Just consumable)
      $ do
        checkFunParams mergepat
        checkBodyDec $ snd $ bodyDec loopbody

        checkStms (bodyStms loopbody) $ do
          context "In loop body result" $
            checkResult $
              bodyResult loopbody

          context "When matching result of body with loop parameters" $
            matchLoopResult (map fst merge) $
              bodyResult loopbody

          let bound_here =
                namesFromList $ M.keys $ scopeOf $ bodyStms loopbody
          map (`namesSubtract` bound_here)
            <$> mapM (subExpAliasesM . resSubExp) (bodyResult loopbody)
  where
    checkLoopVar (p, a) = do
      a_t <- lookupType a
      observe a
      case peelArray 1 a_t of
        Just a_t_r -> do
          checkLParamDec (paramName p) $ paramDec p
          unless (a_t_r `subtypeOf` typeOf (paramDec p)) $
            bad . TypeError $
              "Loop parameter "
                <> prettyText p
                <> " not valid for element of "
                <> prettyText a
                <> ", which has row type "
                <> prettyText a_t_r
          als <- lookupAliases a
          pure (paramName p, als)
        _ ->
          bad . TypeError $
            "Cannot loop over "
              <> prettyText a
              <> " of type "
              <> prettyText a_t
    checkForm mergeargs (ForLoop loopvar it boundexp loopvars) = do
      iparam <- primFParam loopvar $ IntType it
      let mergepat = map fst merge
          funparams = iparam : mergepat
          paramts = map paramDeclType funparams

      consumable <- mapM checkLoopVar loopvars
      boundarg <- checkArg boundexp
      checkFuncall Nothing paramts $ boundarg : mergeargs
      pure consumable
    checkForm mergeargs (WhileLoop cond) = do
      case find ((== cond) . paramName . fst) merge of
        Just (condparam, _) ->
          unless (paramType condparam == Prim Bool) $
            bad . TypeError $
              "Conditional '"
                <> prettyText cond
                <> "' of while-loop is not boolean, but "
                <> prettyText (paramType condparam)
                <> "."
        Nothing ->
          bad $
            TypeError $
              "Conditional '" <> prettyText cond <> "' of while-loop is not a merge variable."
      let mergepat = map fst merge
          funparams = mergepat
          paramts = map paramDeclType funparams
      checkFuncall Nothing paramts mergeargs
      pure mempty

    checkLoopArgs = do
      let (params, args) = unzip merge

      argtypes <- mapM subExpType args

      let expected = expectedTypes (map paramName params) params args
      unless (expected == argtypes) . bad . TypeError . docText $
        "Loop parameters"
          </> indent 2 (ppTuple' $ map pretty params)
          </> "cannot accept initial values"
          </> indent 2 (ppTuple' $ map pretty args)
          </> "of types"
          </> indent 2 (ppTuple' $ map pretty argtypes)
checkExp (WithAcc inputs lam) = do
  unless (length (lambdaParams lam) == 2 * num_accs) . bad . TypeError $
    prettyText (length (lambdaParams lam))
      <> " parameters, but "
      <> prettyText num_accs
      <> " accumulators."

  let cert_params = take num_accs $ lambdaParams lam
  acc_args <- forM (zip inputs cert_params) $ \((shape, arrs, op), p) -> do
    mapM_ (require [Prim int64]) (shapeDims shape)
    elem_ts <- forM arrs $ \arr -> do
      arr_t <- lookupType arr
      unless (shapeDims shape `isPrefixOf` arrayDims arr_t) $
        bad . TypeError $
          prettyText arr <> " is not an array of outer shape " <> prettyText shape
      consume =<< lookupAliases arr
      pure $ stripArray (shapeRank shape) arr_t

    case op of
      Just (op_lam, nes) -> do
        let mkArrArg t = (t, mempty)
        nes_ts <- mapM checkSubExp nes
        unless (nes_ts == lambdaReturnType op_lam) $
          bad . TypeError . T.unlines $
            [ "Accumulator operator return type: " <> prettyText (lambdaReturnType op_lam),
              "Type of neutral elements: " <> prettyText nes_ts
            ]
        checkLambda op_lam $
          replicate (shapeRank shape) (Prim int64, mempty)
            ++ map mkArrArg (elem_ts ++ elem_ts)
      Nothing ->
        pure ()

    pure (Acc (paramName p) shape elem_ts NoUniqueness, mempty)

  checkAnyLambda False lam $ replicate num_accs (Prim Unit, mempty) ++ acc_args
  where
    num_accs = length inputs
checkExp (Op op) = do
  checker <- asks envCheckOp
  checker op

checkSOACArrayArgs ::
  Checkable rep =>
  SubExp ->
  [VName] ->
  TypeM rep [Arg]
checkSOACArrayArgs width = mapM checkSOACArrayArg
  where
    checkSOACArrayArg v = do
      (t, als) <- checkArg $ Var v
      case t of
        Acc {} -> pure (t, als)
        Array {} -> do
          let argSize = arraySize 0 t
          unless (argSize == width) . bad . TypeError $
            "SOAC argument "
              <> prettyText v
              <> " has outer size "
              <> prettyText argSize
              <> ", but width of SOAC is "
              <> prettyText width
          pure (rowType t, als)
        _ ->
          bad . TypeError $
            "SOAC argument " <> prettyText v <> " is not an array"

checkType ::
  Checkable rep =>
  TypeBase Shape u ->
  TypeM rep ()
checkType (Mem (ScalarSpace d _)) = mapM_ (require [Prim int64]) d
checkType (Acc cert shape ts _) = do
  requireI [Prim Unit] cert
  mapM_ (require [Prim int64]) $ shapeDims shape
  mapM_ checkType ts
checkType t = mapM_ checkSubExp $ arrayDims t

checkExtType ::
  Checkable rep =>
  TypeBase ExtShape u ->
  TypeM rep ()
checkExtType = mapM_ checkExtDim . shapeDims . arrayShape
  where
    checkExtDim (Free se) = void $ checkSubExp se
    checkExtDim (Ext _) = pure ()

checkCmpOp ::
  Checkable rep =>
  CmpOp ->
  SubExp ->
  SubExp ->
  TypeM rep ()
checkCmpOp (CmpEq t) x y = do
  require [Prim t] x
  require [Prim t] y
checkCmpOp (CmpUlt t) x y = checkBinOpArgs (IntType t) x y
checkCmpOp (CmpUle t) x y = checkBinOpArgs (IntType t) x y
checkCmpOp (CmpSlt t) x y = checkBinOpArgs (IntType t) x y
checkCmpOp (CmpSle t) x y = checkBinOpArgs (IntType t) x y
checkCmpOp (FCmpLt t) x y = checkBinOpArgs (FloatType t) x y
checkCmpOp (FCmpLe t) x y = checkBinOpArgs (FloatType t) x y
checkCmpOp CmpLlt x y = checkBinOpArgs Bool x y
checkCmpOp CmpLle x y = checkBinOpArgs Bool x y

checkBinOpArgs ::
  Checkable rep =>
  PrimType ->
  SubExp ->
  SubExp ->
  TypeM rep ()
checkBinOpArgs t e1 e2 = do
  require [Prim t] e1
  require [Prim t] e2

checkPatElem ::
  Checkable rep =>
  PatElem (LetDec rep) ->
  TypeM rep ()
checkPatElem (PatElem name dec) =
  context ("When checking pattern element " <> prettyText name) $
    checkLetBoundDec name dec

checkFlatDimIndex ::
  Checkable rep =>
  FlatDimIndex SubExp ->
  TypeM rep ()
checkFlatDimIndex (FlatDimIndex n s) = mapM_ (require [Prim int64]) [n, s]

checkFlatSlice ::
  Checkable rep =>
  FlatSlice SubExp ->
  TypeM rep ()
checkFlatSlice (FlatSlice offset idxs) = do
  require [Prim int64] offset
  mapM_ checkFlatDimIndex idxs

checkDimIndex ::
  Checkable rep =>
  DimIndex SubExp ->
  TypeM rep ()
checkDimIndex (DimFix i) = require [Prim int64] i
checkDimIndex (DimSlice i n s) = mapM_ (require [Prim int64]) [i, n, s]

checkStm ::
  Checkable rep =>
  Stm (Aliases rep) ->
  TypeM rep a ->
  TypeM rep a
checkStm stm@(Let pat (StmAux (Certs cs) _ (_, dec)) e) m = do
  context "When checking certificates" $ mapM_ (requireI [Prim Unit]) cs
  context "When checking expression annotation" $ checkExpDec dec
  context ("When matching\n" <> message "  " pat <> "\nwith\n" <> message "  " e) $
    matchPat pat e
  binding (maybeWithoutAliases $ scopeOf stm) $ do
    mapM_ checkPatElem (patElems $ removePatAliases pat)
    m
  where
    -- FIXME: this is wrong.  However, the core language type system
    -- is not strong enough to fully capture the aliases we want (see
    -- issue #803).  Since we eventually inline everything anyway, and
    -- our intra-procedural alias analysis is much simpler and
    -- correct, I could not justify spending time on improving the
    -- inter-procedural alias analysis.  If we ever stop inlining
    -- everything, probably we need to go back and refine this.
    maybeWithoutAliases =
      case stmExp stm of
        Apply {} -> M.map withoutAliases
        _ -> id
    withoutAliases (LetName (_, ldec)) = LetName (mempty, ldec)
    withoutAliases info = info

matchExtPat ::
  Checkable rep =>
  Pat (LetDec (Aliases rep)) ->
  [ExtType] ->
  TypeM rep ()
matchExtPat pat ts =
  unless (expExtTypesFromPat pat == ts) $
    bad $
      InvalidPatError pat ts Nothing

matchExtReturnType ::
  Checkable rep =>
  [ExtType] ->
  Result ->
  TypeM rep ()
matchExtReturnType rettype res = do
  ts <- mapM subExpResType res
  matchExtReturns rettype res ts

matchExtBranchType ::
  Checkable rep =>
  [ExtType] ->
  Body (Aliases rep) ->
  TypeM rep ()
matchExtBranchType rettype (Body _ stms res) = do
  ts <- extendedScope (traverse subExpResType res) stmscope
  matchExtReturns rettype res ts
  where
    stmscope = scopeOf stms

matchExtReturns :: [ExtType] -> Result -> [Type] -> TypeM rep ()
matchExtReturns rettype res ts = do
  let problem :: TypeM rep a
      problem =
        bad . TypeError . T.unlines $
          [ "Type annotation is",
            "  " <> prettyTuple rettype,
            "But result returns type",
            "  " <> prettyTuple ts
          ]

  unless (length res == length rettype) problem

  let ctx_vals = zip res ts
      instantiateExt i = case maybeNth i ctx_vals of
        Just (SubExpRes _ se, Prim (IntType Int64)) -> pure se
        _ -> problem

  rettype' <- instantiateShapes instantiateExt rettype

  unless (rettype' == ts) problem

validApply ::
  ArrayShape shape =>
  [TypeBase shape Uniqueness] ->
  [TypeBase shape NoUniqueness] ->
  Bool
validApply expected got =
  length got == length expected
    && and
      ( zipWith
          subtypeOf
          (map rankShaped got)
          (map (fromDecl . rankShaped) expected)
      )

type Arg = (Type, Names)

argType :: Arg -> Type
argType (t, _) = t

-- | Remove all aliases from the 'Arg'.
argAliases :: Arg -> Names
argAliases (_, als) = als

noArgAliases :: Arg -> Arg
noArgAliases (t, _) = (t, mempty)

checkArg ::
  Checkable rep =>
  SubExp ->
  TypeM rep Arg
checkArg arg = do
  argt <- checkSubExp arg
  als <- subExpAliasesM arg
  pure (argt, als)

checkFuncall ::
  Maybe Name ->
  [DeclType] ->
  [Arg] ->
  TypeM rep ()
checkFuncall fname paramts args = do
  let argts = map argType args
  unless (validApply paramts argts) $
    bad $
      ParameterMismatch fname (map fromDecl paramts) $
        map argType args
  consumeArgs paramts args

consumeArgs ::
  [DeclType] ->
  [Arg] ->
  TypeM rep ()
consumeArgs paramts args =
  forM_ (zip (map diet paramts) args) $ \(d, (_, als)) ->
    occur [consumption (consumeArg als d)]
  where
    consumeArg als Consume = als
    consumeArg _ _ = mempty

-- The boolean indicates whether we only allow consumption of
-- parameters.
checkAnyLambda ::
  Checkable rep => Bool -> Lambda (Aliases rep) -> [Arg] -> TypeM rep ()
checkAnyLambda soac (Lambda params body rettype) args = do
  let fname = nameFromString "<anonymous>"
  if length params == length args
    then do
      -- Consumption for this is done explicitly elsewhere.
      checkFuncall
        Nothing
        (map ((`toDecl` Nonunique) . paramType) params)
        $ map noArgAliases args
      let consumable =
            if soac
              then Just $ zip (map paramName params) (map argAliases args)
              else Nothing
          params' =
            [(paramName param, LParamName $ paramDec param) | param <- params]
      checkFun'
        (fname, staticShapes $ map (`toDecl` Nonunique) rettype, params')
        consumable
        $ do
          checkLambdaParams params
          mapM_ checkType rettype
          checkLambdaBody rettype body
    else
      bad . TypeError $
        "Anonymous function defined with "
          <> prettyText (length params)
          <> " parameters:\n"
          <> prettyText params
          <> "\nbut expected to take "
          <> prettyText (length args)
          <> " arguments."

checkLambda :: Checkable rep => Lambda (Aliases rep) -> [Arg] -> TypeM rep ()
checkLambda = checkAnyLambda True

checkPrimExp :: Checkable rep => PrimExp VName -> TypeM rep ()
checkPrimExp ValueExp {} = pure ()
checkPrimExp (LeafExp v pt) = requireI [Prim pt] v
checkPrimExp (BinOpExp op x y) = do
  requirePrimExp (binOpType op) x
  requirePrimExp (binOpType op) y
checkPrimExp (CmpOpExp op x y) = do
  requirePrimExp (cmpOpType op) x
  requirePrimExp (cmpOpType op) y
checkPrimExp (UnOpExp op x) = requirePrimExp (unOpType op) x
checkPrimExp (ConvOpExp op x) = requirePrimExp (fst $ convOpType op) x
checkPrimExp (FunExp h args t) = do
  (h_ts, h_ret, _) <-
    maybe
      (bad $ TypeError $ "Unknown function: " <> T.pack h)
      pure
      $ M.lookup h primFuns
  when (length h_ts /= length args) . bad . TypeError $
    "Function expects "
      <> prettyText (length h_ts)
      <> " parameters, but given "
      <> prettyText (length args)
      <> " arguments."
  when (h_ret /= t) . bad . TypeError $
    "Function return annotation is "
      <> prettyText t
      <> ", but expected "
      <> prettyText h_ret
  zipWithM_ requirePrimExp h_ts args

requirePrimExp :: Checkable rep => PrimType -> PrimExp VName -> TypeM rep ()
requirePrimExp t e = context ("in PrimExp " <> prettyText e) $ do
  checkPrimExp e
  unless (primExpType e == t) . bad . TypeError $
    prettyText e <> " must have type " <> prettyText t

-- | The class of representations that can be type-checked.
class (AliasableRep rep, TypedOp (OpC rep (Aliases rep))) => Checkable rep where
  checkExpDec :: ExpDec rep -> TypeM rep ()
  checkBodyDec :: BodyDec rep -> TypeM rep ()
  checkFParamDec :: VName -> FParamInfo rep -> TypeM rep ()
  checkLParamDec :: VName -> LParamInfo rep -> TypeM rep ()
  checkLetBoundDec :: VName -> LetDec rep -> TypeM rep ()
  checkRetType :: [RetType rep] -> TypeM rep ()
  matchPat :: Pat (LetDec (Aliases rep)) -> Exp (Aliases rep) -> TypeM rep ()
  primFParam :: VName -> PrimType -> TypeM rep (FParam (Aliases rep))
  matchReturnType :: [RetType rep] -> Result -> TypeM rep ()
  matchBranchType :: [BranchType rep] -> Body (Aliases rep) -> TypeM rep ()
  matchLoopResult :: [FParam (Aliases rep)] -> Result -> TypeM rep ()

  -- | Used at top level; can be locally changed with 'checkOpWith'.
  checkOp :: Op (Aliases rep) -> TypeM rep ()

  default checkExpDec :: ExpDec rep ~ () => ExpDec rep -> TypeM rep ()
  checkExpDec = pure

  default checkBodyDec :: BodyDec rep ~ () => BodyDec rep -> TypeM rep ()
  checkBodyDec = pure

  default checkFParamDec :: FParamInfo rep ~ DeclType => VName -> FParamInfo rep -> TypeM rep ()
  checkFParamDec _ = checkType

  default checkLParamDec :: LParamInfo rep ~ Type => VName -> LParamInfo rep -> TypeM rep ()
  checkLParamDec _ = checkType

  default checkLetBoundDec :: LetDec rep ~ Type => VName -> LetDec rep -> TypeM rep ()
  checkLetBoundDec _ = checkType

  default checkRetType :: RetType rep ~ DeclExtType => [RetType rep] -> TypeM rep ()
  checkRetType = mapM_ $ checkExtType . declExtTypeOf

  default matchPat :: Pat (LetDec (Aliases rep)) -> Exp (Aliases rep) -> TypeM rep ()
  matchPat pat = matchExtPat pat <=< expExtType

  default primFParam :: FParamInfo rep ~ DeclType => VName -> PrimType -> TypeM rep (FParam (Aliases rep))
  primFParam name t = pure $ Param mempty name (Prim t)

  default matchReturnType :: RetType rep ~ DeclExtType => [RetType rep] -> Result -> TypeM rep ()
  matchReturnType = matchExtReturnType . map fromDecl

  default matchBranchType :: BranchType rep ~ ExtType => [BranchType rep] -> Body (Aliases rep) -> TypeM rep ()
  matchBranchType = matchExtBranchType

  default matchLoopResult ::
    FParamInfo rep ~ DeclType =>
    [FParam (Aliases rep)] ->
    Result ->
    TypeM rep ()
  matchLoopResult = matchLoopResultExt
