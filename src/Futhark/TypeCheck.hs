{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables #-}
-- | The type checker checks whether the program is type-consistent.
module Futhark.TypeCheck
  ( -- * Interface
    checkProg
  , checkProgNoUniqueness
  , TypeError
  , ErrorCase
    -- * Extensionality
  , TypeM
  , bad
  , context
  , message
  , Checkable (..)
  , module Futhark.TypeCheck.TypeError
  , lookupVar
  , VarBindingLore (..)
    -- * Checkers
  , require
  , requireI
  , checkSubExp
  , checkType
  , checkExtType
  , matchExtPattern
  , matchExtReturnType
  )
  where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.Loc (noLoc)
import Data.List
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import qualified Futhark.Representation.AST as AST
import Futhark.Representation.Aliases hiding (TypeEnv)
import Futhark.TypeCheck.TypeError
import Futhark.Analysis.Alias
import qualified Futhark.Util.Pretty as PP

-- | Information about an error that occured during type checking.
data TypeError lore = Error [String] (ErrorCase lore)

-- | What went wrong.
type ErrorCase lore =
  GenTypeError VName (Exp lore) (Several ExtType) (Several (PatElemT (Annotations.LetBound lore)))

instance PrettyLore lore => Show (TypeError lore) where
  show (Error [] err) =
    show err
  show (Error msgs err) =
    intercalate "\n" msgs ++ "\n" ++ show err

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding lore = (RetType lore, [FParam lore])

data VarBindingLore lore = LetBound (Annotations.LetBound lore)
                         | FunBound (Annotations.FParam lore)
                         | LambdaBound (Annotations.LParam lore)

data VarBinding lore = Bound Type (VarBindingLore lore) Names
                     | WasConsumed

data Usage = Consumed
           | Observed
             deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names
                           , consumed :: Names
                           }
             deriving (Eq, Show)

observation :: Names -> Occurence
observation = flip Occurence HS.empty

consumption :: Names -> Occurence
consumption = Occurence HS.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = HS.null (observed occ) && HS.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = HM.HashMap VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb HM.empty
  where comb m (Occurence obs cons) =
          let m' = HS.foldl' (ins Observed) m obs
          in HS.foldl' (ins Consumed) m' cons
        ins v m k = HM.insertWith (++) k [v] m

allConsumed :: Occurences -> Names
allConsumed = HS.unions . map consumed

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) $ map filt occurs1 ++ occurs2
  where filt occ =
          occ { consumed = consumed occ `HS.difference` postcons
              , observed = observed occ `HS.difference` postcons }
        postcons = allConsumed occurs2

unOccur :: Names -> Occurences -> Occurences
unOccur to_be_removed = filter (not . nullOccurence) . map unOccur'
  where unOccur' occ =
          occ { observed = observed occ `HS.difference` to_be_removed
              , consumed = consumed occ `HS.difference` to_be_removed
              }

-- | The 'Consumption' data structure is used to keep track of which
-- variables have been consumed, as well as whether a violation has been detected.
data Consumption = ConsumptionError String
                 | Consumption Occurences
                 deriving (Show)

instance Monoid Consumption where
  mempty = Consumption mempty
  ConsumptionError e `mappend` _ = ConsumptionError e
  _ `mappend` ConsumptionError e = ConsumptionError e
  Consumption o1 `mappend` Consumption o2
    | v:_ <- HS.toList $ consumed_in_o1 `HS.intersection` used_in_o2 =
        ConsumptionError $ "Variable " <> pretty v <> " referenced after being consumed."
    | otherwise =
        Consumption $ o1 `seqOccurences` o2
    where consumed_in_o1 = mconcat $ map consumed o1
          used_in_o2 = mconcat $ map consumed o2 <> map observed o2

-- | The environment contains a variable table and a function table.
-- Type checking happens with access to this environment.  The
-- function table is only initialised at the very beginning, but the
-- variable table will be extended during type-checking when
-- let-expressions are encountered.
data TypeEnv lore =
  TypeEnv { envVtable :: HM.HashMap VName (VarBinding lore)
          , envFtable :: HM.HashMap Name (FunBinding lore)
          , envCheckOccurences :: Bool
          , envContext :: [String]
          }

-- | The type checker runs in this monad.
newtype TypeM lore a = TypeM (RWST
                              (TypeEnv lore)     -- Reader
                              Consumption        -- Writer
                              ()                 -- State
                              (Either (TypeError lore)) -- Inner monad
                              a)
  deriving (Monad, Functor, Applicative,
            MonadReader (TypeEnv lore),
            MonadWriter Consumption)

instance HasTypeEnv (TypeM lore) where
  lookupType name = do (t, _, _) <- lookupVar name
                       return t
  askTypeEnv = asks $ HM.fromList . mapMaybe varType . HM.toList . envVtable
    where varType (name, Bound t _ _ ) = Just (name, t)
          varType (_,    WasConsumed)  = Nothing

runTypeM :: TypeEnv lore -> TypeM lore a
         -> Either (TypeError lore) a
runTypeM env (TypeM m) = fst <$> evalRWST m env ()

bad :: ErrorCase lore -> TypeM lore a
bad e = do
  messages <- asks envContext
  TypeM $ lift $ Left $ Error (reverse messages) e

-- | Add information about what is being type-checked to the current
-- context.  Liberal use of this combinator makes it easier to track
-- type errors, as the strings are added to type errors signalled via
-- 'bad'.
context :: String
          -> TypeM lore a
          -> TypeM lore a
context s = local $ \env -> env { envContext = s : envContext env}

message :: PP.Pretty a =>
           String -> a -> String
message s x = PP.prettyDoc 80 $
              PP.text s PP.<+> PP.align (PP.ppr x)

liftEitherS :: Either String a -> TypeM lore a
liftEitherS = either (bad . TypeError noLoc) return

occur :: Occurences -> TypeM lore ()
occur = tell . Consumption

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: VName -> TypeM lore ()
observe name = do
  (t, _, names) <- lookupVar name
  unless (basicType t) $
    occur [observation names]

-- | Proclaim that we have written to the given variable.
consume :: Names -> TypeM lore ()
consume als = occur [consumption als]

collectOccurences :: TypeM lore a -> TypeM lore (a, Occurences)
collectOccurences m = pass $ do
  (x, c) <- listen m
  o <- maybeCheckConsumption c
  return ((x, o), const mempty)

noDataflow :: TypeM lore a -> TypeM lore a
noDataflow = censor $ const mempty

maybeCheckConsumption :: Consumption -> TypeM lore Occurences
maybeCheckConsumption (ConsumptionError e) = do
  check <- asks envCheckOccurences
  if check
    then bad $ TypeError noLoc e
    else return mempty
maybeCheckConsumption (Consumption os) =
  return os

alternative :: TypeM lore a -> TypeM lore b -> TypeM lore (a,b)
alternative m1 m2 = pass $ do
  (x, c1) <- listen m1
  (y, c2) <- listen m2
  os1 <- maybeCheckConsumption c1
  os2 <- maybeCheckConsumption c2
  let usage = Consumption $ os1 `altOccurences` os2
  return ((x, y), const usage)

-- | Permit consumption of only the specified names.  If one of these
-- names is consumed, the consumption will be rewritten to be a
-- consumption of the corresponding alias set.  Consumption of
-- anything else will result in a type error.
consumeOnlyParams :: [(VName, Names)] -> TypeM lore a -> TypeM lore a
consumeOnlyParams consumable m = do
  (x, os) <- collectOccurences m
  tell . Consumption =<< mapM inspect os
  return x
  where inspect :: Occurence -> TypeM lore Occurence
        inspect o = do
          new_consumed <- mconcat <$> mapM wasConsumed (HS.toList $ consumed o)
          return o { consumed = new_consumed }
        wasConsumed v
          | Just als <- lookup v consumable = return als
          | otherwise =
            bad $ TypeError noLoc $ pretty v ++ " was invalidly consumed."

-- | Given the immediate aliases, compute the full transitive alias
-- set (including the immediate aliases).
expandAliases :: Names -> TypeEnv lore -> Names
expandAliases names env = names `HS.union` aliasesOfAliases
  where aliasesOfAliases =  mconcat . map look . HS.toList $ names
        look k = case HM.lookup k $ envVtable env of
          Just (Bound _ _ als) -> als
          _                    -> mempty

binding :: Checkable lore =>
           [((Ident, VarBindingLore lore), Names)]
        -> TypeM lore a
        -> TypeM lore a
binding bnds = check . local (`bindVars` bnds)
  where bindVars = foldl bindVar
        boundnames = map (identName . fst . fst) bnds
        boundnameset = HS.fromList boundnames

        bindVar env ((Ident name tp, attr), immediate) =
          let names = expandAliases immediate env
              inedges = HS.toList names
              update (Bound tp' thisattr thesenames) =
                Bound tp' thisattr $ HS.insert name thesenames
              update b = b
          in env { envVtable =
                      HM.insert name (Bound tp attr names) $
                      adjustSeveral update inedges $
                      envVtable env
                 }

        adjustSeveral f = flip $ foldl $ flip $ HM.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          already_bound <- asks envVtable
          case filter ((`HM.member` already_bound) . identName . fst . fst) bnds of
            []  -> return ()
            ((v, _),_):_ -> bad $ TypeError noLoc $
                            "Variable " ++ pretty v ++ " being redefined."
          (a, os) <- collectOccurences m
          tell $ Consumption $ unOccur boundnameset os
          return a

lookupVar :: VName -> TypeM lore (Type, VarBindingLore lore, Names)
lookupVar name = do
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name noLoc
    Just (Bound t lore names) -> return (t, lore, names)
    Just WasConsumed          -> bad $ UseAfterConsume name noLoc noLoc

lookupAliases :: VName -> TypeM lore Names
lookupAliases name = do (_, _, als) <- lookupVar name
                        return $ HS.insert name als

subExpAliasesM :: SubExp -> TypeM lore Names
subExpAliasesM Constant{} = return mempty
subExpAliasesM (Var v)    = lookupAliases v

lookupFun :: forall lore.Lore lore =>
             Name
          -> [SubExp]
          -> TypeM lore (RetType lore, [DeclType])
lookupFun fname args = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname noLoc
    Just (ftype, params) -> do
      argts <- mapM subExpType args
      case applyRetType ftype params $
           zip args argts of
        Nothing ->
          bad $ ParameterMismatch (Just fname) noLoc
          (Right $ map (justOne . staticShapes1 . paramType) params) $
          map (justOne . staticShapes1) argts
        Just rt ->
          return (rt, map paramDeclType params)

-- | Determine if the types of two subexpressions are identical.
-- Causes a 'TypeError vn' if they fail to match, and otherwise
-- returns their common type.
matchSubExpTypes :: SubExp -> SubExp -> TypeM lore Type
matchSubExpTypes e1 e2 = do
  t1 <- subExpType e1
  t2 <- subExpType e2
  if t1 `subtypeOf` t2
    then return t1 -- arbitrary
    else bad $
         UnifyError (PrimOp $ SubExp e1) (justOne $ staticShapes1 t1)
         (PrimOp $ SubExp e2) (justOne $ staticShapes1 t2)

-- | @checkAnnotation loc s t1 t2@ checks if @t2@ is equal to
-- @t1@.  If not, a 'BadAnnotation' is raised.
checkAnnotation :: String -> Type -> Type
                -> TypeM lore ()
checkAnnotation desc t1 t2
  | t2 == t1 = return ()
  | otherwise = bad $ BadAnnotation noLoc desc
                (justOne $ staticShapes1 t1) (justOne $ staticShapes1 t2)

-- | @require ts se@ causes a '(TypeError vn)' if the type of @se@ is
-- not a subtype of one of the types in @ts@.
require :: Checkable lore => [Type] -> SubExp -> TypeM lore ()
require ts se = do
  t <- checkSubExp se
  unless (t `elem` ts) $
    bad $ UnexpectedType noLoc (PrimOp $ SubExp se)
    (justOne $ staticShapes1 t)
    (map (justOne . staticShapes1) ts)

-- | Variant of 'require' working on variable names.
requireI :: Checkable lore => [Type] -> VName -> TypeM lore ()
requireI ts ident = require ts $ Var ident

checkArrIdent :: VName -> TypeM lore Type
checkArrIdent v = do
  t <- lookupType v
  case t of
    Array{} -> return t
    _       -> bad $ NotAnArray noLoc (PrimOp $ SubExp $ Var v) $
               justOne $ staticShapes1 t

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: Checkable lore =>
             AST.Prog lore -> Either (TypeError lore) ()
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: Checkable lore =>
                         AST.Prog lore -> Either (TypeError lore) ()
checkProgNoUniqueness = checkProg' False

checkProg' :: Checkable lore =>
              Bool -> AST.Prog lore -> Either (TypeError lore) ()
checkProg' checkoccurs prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = HM.empty
                        , envFtable = ftable
                        , envCheckOccurences = checkoccurs
                        , envContext = []
                        }

  runTypeM typeenv $
    mapM_ (noDataflow . checkFun) $ progFunctions prog'
  where
    prog' = aliasAnalysis prog
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = foldM expand (initialFtable prog')
                  (progFunctions prog')
    expand ftable (FunDec name ret params _)
      | HM.member name ftable =
        Left $ Error [] $ DupDefinitionError name noLoc noLoc
      | otherwise =
        Right $ HM.insert name (ret,params) ftable

-- The prog argument is just to disambiguate the lore.
initialFtable :: forall lore.Checkable lore =>
                 Prog lore -> HM.HashMap Name (FunBinding lore)
initialFtable _ = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) =
          (basicRetType t,
           map (basicFParam (representative :: lore) name) ts)
        name = ID (nameFromString "x", 0)

checkFun :: Checkable lore =>
            FunDec lore -> TypeM lore ()
checkFun (FunDec fname rettype params body) =
  context ("In function " ++ nameToString fname) $
    checkFun' (fname,
               retTypeValues rettype,
               funParamsToNamesTypesAndLores params,
               body) $
    consumeOnlyParams consumable $ do
      checkFunParams params
      checkRetType rettype
      checkFunBody fname rettype body
        where consumable = [ (paramName param, mempty)
                           | param <- params
                           , unique $ paramDeclType param
                           ]

funParamsToNamesTypesAndLores :: Checkable lore =>
                                 [FParam lore]
                              -> [(VName, DeclType, VarBindingLore lore)]
funParamsToNamesTypesAndLores = map nameTypeAndLore
  where nameTypeAndLore fparam = (paramName fparam,
                                  paramDeclType fparam,
                                  FunBound $ paramAttr fparam)

lamParamsToNamesTypesAndLores :: Checkable lore =>
                                 [LParam lore]
                              -> [(VName, DeclType, VarBindingLore lore)]
lamParamsToNamesTypesAndLores = map nameTypeAndLore
  where nameTypeAndLore fparam = (paramName fparam,
                                  toDecl (paramType fparam) Unique,
                                  LambdaBound $ paramAttr fparam)

checkAnonymousFun :: Checkable lore =>
                     (Name, [Type], [LParam (Aliases lore)], BodyT (Aliases lore))
                  -> TypeM lore ()
checkAnonymousFun (fname, rettype, params, body) =
  checkFun' (fname,
             staticShapes $ map (`toDecl` Nonunique) rettype,
             [ (paramName param,
                toDecl (paramType param) Unique,
                LambdaBound $ paramAttr param)
             | param <- params ],
             body) $ do
    checkLambdaParams params
    mapM_ checkType rettype
    checkLambdaBody rettype body

checkFunParams :: Checkable lore =>
                  [FParam lore] -> TypeM lore ()
checkFunParams = mapM_ $ \param ->
  context ("In function parameter " ++ pretty param) $
    checkFParamLore (paramName param) (paramAttr param)

checkLambdaParams :: Checkable lore =>
                     [LParam lore] -> TypeM lore ()
checkLambdaParams = mapM_ $ \param ->
  context ("In lambda parameter " ++ pretty param) $
    checkLParamLore (paramName param) (paramAttr param)

checkFun' :: (Checkable lore) =>
             (Name,
              [DeclExtType],
              [(VName, DeclType, VarBindingLore lore)],
              BodyT (Aliases lore))
          -> TypeM lore ()
          -> TypeM lore ()
checkFun' (fname, rettype, params_with_lore, body) check = do
  checkNoDuplicateParams
  binding params_for_binding
    check

  checkReturnAlias $ bodyAliases body
  where (param_names, param_types, _param_attrs) = unzip3 params_with_lore

        params_for_binding =
          [ ((Ident pname (fromDecl ptype), pattr), mempty) |
            (pname, ptype, pattr) <- params_with_lore ]

        checkNoDuplicateParams = foldM_ expand [] param_names

        expand seen pname
          | Just _ <- find (==pname) seen =
            bad $ DupParamError fname pname noLoc
          | otherwise =
            return $ pname : seen

        notAliasingParam names =
          forM_ (zip param_names param_types) $ \(pname, ptype) ->
            when (not (unique ptype) && pname `HS.member` names) $
              bad $ ReturnAliased fname pname noLoc

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' HS.empty .
          returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`HS.member` HS.map snd seen) $ HS.toList names =
            bad $ UniqueReturnAliased fname noLoc
          | otherwise = do
            notAliasingParam names
            return $ seen `HS.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`HS.member` seen) $ HS.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname noLoc
          | otherwise = return $ seen `HS.union` tag Nonunique names

        tag u = HS.map $ \name -> (u, name)

        returnAliasing expected got =
          [ (uniqueness p, names) |
            (p,names) <- zip expected got ]

checkSubExp :: Checkable lore => SubExp -> TypeM lore Type
checkSubExp (Constant val) =
  return $ Basic $ basicValueType val
checkSubExp (Var ident) = context ("In subexp " ++ pretty ident) $ do
  observe ident
  lookupType ident

checkBindings :: Checkable lore =>
                 [Binding lore] -> TypeM lore a
              -> TypeM lore a
checkBindings origbnds m = delve origbnds
  where delve (Let pat (_,annot) e:bnds) = do
          context ("In expression\n" ++ message "  " e) $
            checkExp e
          checkExpLore annot
          checkBinding pat e $
            delve bnds
        delve [] =
          m

checkResult :: Checkable lore =>
               Result -> TypeM lore ()
checkResult = mapM_ checkSubExp

checkFunBody :: Checkable lore =>
                Name
             -> RetType lore
             -> Body lore
             -> TypeM lore ()
checkFunBody fname rt (Body (_,lore) bnds res) = do
  checkBindings bnds $ do
    checkResult res
    matchReturnType fname rt res
  checkBodyLore lore

checkLambdaBody :: Checkable lore =>
                   [Type] -> Body lore -> TypeM lore ()
checkLambdaBody ret (Body (_,lore) bnds res) = do
  checkBindings bnds $ checkLambdaResult ret res
  checkBodyLore lore

checkLambdaResult :: Checkable lore =>
                     [Type] -> Result -> TypeM lore ()
checkLambdaResult ts es
  | length ts /= length es =
    bad $ TypeError noLoc $
    "Lambda has return type " ++ prettyTuple ts ++
    " describing " ++ show (length ts) ++ " values, but body returns " ++
    show (length es) ++ " values: " ++ prettyTuple es
  | otherwise = forM_ (zip ts es) $ \(t, e) -> do
      et <- checkSubExp e
      unless (et == t) $
        bad $ TypeError noLoc $
        "Subexpression " ++ pretty e ++ " has type " ++ pretty et ++
        " but expected " ++ pretty t

checkBody :: Checkable lore =>
             Body lore -> TypeM lore ()
checkBody (Body (_,lore) bnds res) = do
  checkBindings bnds $ checkResult res
  checkBodyLore lore

checkPrimOp :: Checkable lore =>
               PrimOp lore -> TypeM lore ()

checkPrimOp (SubExp es) =
  void $ checkSubExp es

checkPrimOp (ArrayLit [] _) =
  return ()

checkPrimOp (ArrayLit (e:es') t) = do
  let check elemt eleme = do
        elemet <- checkSubExp eleme
        unless (elemet == elemt) $
          bad $ TypeError noLoc $ pretty elemet ++
          " is not of expected type " ++ pretty elemt ++ "."
  et <- checkSubExp e

  -- Compare that type with the one given for the array literal.
  checkAnnotation "array-element" t et

  mapM_ (check et) es'

checkPrimOp (BinOp op e1 e2 t) = checkBinOp op e1 e2 t

checkPrimOp (Not e) =
  require [Basic Bool] e

checkPrimOp (Complement e) =
  require [Basic Int] e

checkPrimOp (Negate e) =
  require [Basic Int, Basic Float32, Basic Float64] e

checkPrimOp (Abs e) =
  require [Basic Int] e

checkPrimOp (Signum e) =
  require [Basic Int] e

checkPrimOp (Index cs ident idxes) = do
  mapM_ (requireI [Basic Cert]) cs
  vt <- lookupType ident
  observe ident
  when (arrayRank vt < length idxes) $
    bad $ IndexingError ident
          (arrayRank vt) (length idxes) noLoc
  mapM_ (require [Basic Int]) idxes

checkPrimOp (Iota e) =
  require [Basic Int] e

checkPrimOp (Replicate countexp valexp) = do
  require [Basic Int] countexp
  void $ checkSubExp valexp

checkPrimOp (Scratch _ shape) =
  mapM_ checkSubExp shape

checkPrimOp (Reshape cs newshape arrexp) = do
  rank <- arrayRank <$> checkArrIdent arrexp
  mapM_ (requireI [Basic Cert]) cs
  mapM_ (require [Basic Int] . newDim) newshape
  zipWithM_ (checkDimChange rank) newshape [0..]
  where checkDimChange _ (DimNew _) _ =
          return ()
        checkDimChange rank (DimCoercion se) i
          | i >= rank =
            bad $ TypeError noLoc $
            "Asked to coerce dimension " ++ show i ++ " to " ++ pretty se ++
            ", but array " ++ pretty arrexp ++ " has only " ++ pretty rank ++ " dimensions"
          | otherwise =
            return ()

checkPrimOp (Rearrange cs perm arr) = do
  mapM_ (requireI [Basic Cert]) cs
  arrt <- lookupType arr
  let rank = arrayRank arrt
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError noLoc perm rank $ Just arr

checkPrimOp (Stripe cs stride arr) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] stride
  void $ checkArrIdent arr

checkPrimOp (Unstripe cs stride arr) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] stride
  void $ checkArrIdent arr

checkPrimOp (Split cs sizeexps arrexp) = do
  mapM_ (requireI [Basic Cert]) cs
  mapM_ (require [Basic Int]) sizeexps
  void $ checkArrIdent arrexp

checkPrimOp (Concat cs arr1exp arr2exps ressize) = do
  mapM_ (requireI [Basic Cert]) cs
  arr1t  <- checkArrIdent arr1exp
  arr2ts <- mapM checkArrIdent arr2exps
  let success = all (== stripArray 1 arr1t) $
                map (stripArray 1) arr2ts
  unless success $
    bad $ TypeError noLoc $
    "Types of arguments to concat do not match.  Got " ++
    pretty arr1t ++ " and " ++ intercalate ", " (map pretty arr2ts)
  require [Basic Int] ressize

checkPrimOp (Copy e) =
  void $ checkArrIdent e

checkPrimOp (Assert e _) =
  require [Basic Bool] e

checkPrimOp (Alloc e _) =
  require [Basic Int] e

checkPrimOp (Partition cs _ flags arrs) = do
  mapM_ (requireI [Basic Cert]) cs
  flagst <- lookupType flags
  unless (rowType flagst == Basic Int) $
    bad $ TypeError noLoc $ "Flag array has type " ++ pretty flagst ++ "."
  forM_ arrs $ \arr -> do
    arrt <- lookupType arr
    unless (arrayRank arrt > 0) $
      bad $ TypeError noLoc $
      "Array argument " ++ pretty arr ++
      " to partition has type " ++ pretty arrt ++ "."

checkLoopOp :: Checkable lore =>
               LoopOp lore -> TypeM lore ()

checkLoopOp (DoLoop respat merge form loopbody) = do
  let (mergepat, mergeexps) = unzip merge
  mergeargs <- mapM checkArg mergeexps

  funparams <- case form of
    ForLoop loopvar boundexp -> do
      iparam <- basicFParamM loopvar Int
      let funparams = iparam : mergepat
          paramts   = map paramDeclType funparams

      boundarg <- checkArg boundexp
      checkFuncall Nothing paramts $ boundarg : mergeargs
      return funparams
    WhileLoop cond -> do
      case find ((==cond) . paramName . fst) merge of
        Just (condparam,_) ->
          unless (paramType condparam == Basic Bool) $
          bad $ TypeError noLoc $
          "Conditional '" ++ pretty cond ++ "' of while-loop is not boolean, but " ++
          pretty (paramType condparam) ++ "."
        Nothing ->
          bad $ TypeError noLoc $
          "Conditional '" ++ pretty cond ++ "' of while-loop is not a merge varible."
      let funparams = mergepat
          paramts   = map paramDeclType funparams
      checkFuncall Nothing paramts mergeargs
      return funparams

  let rettype = map paramDeclType mergepat
      consumable = [ (paramName param, mempty)
                   | param <- mergepat,
                     unique $ paramDeclType param
                   ]

  context "Inside the loop body" $
    checkFun' (nameFromString "<loop body>",
               staticShapes rettype,
               funParamsToNamesTypesAndLores funparams,
               loopbody) $
      consumeOnlyParams consumable $ do
        checkFunParams funparams
        checkBody loopbody
        bodyt <- map (`toDecl` Unique) <$> bodyExtType loopbody
        unless (map rankShaped bodyt `subtypesOf`
                map rankShaped (staticShapes rettype)) $
          bad $ ReturnTypeError noLoc (nameFromString "<loop body>")
          (Several $ map fromDecl $ staticShapes rettype)
          (Several $ map fromDecl bodyt)
  forM_ respat $ \res ->
    case find ((==res) . paramName) mergepat of
      Nothing -> bad $ TypeError noLoc $
                 "Loop result variable " ++
                 textual res ++
                 " is not a merge variable."
      Just _  -> return ()

checkLoopOp (Map ass size fun arrexps) = do
  mapM_ (requireI [Basic Cert]) ass
  require [Basic Int] size
  arrargs <- checkSOACArrayArgs size arrexps
  void $ checkLambda fun arrargs

checkLoopOp (ConcatMap cd size fun inarrs) = do
  mapM_ (requireI [Basic Cert]) cd
  require [Basic Int] size
  forM_ inarrs $ \inarr -> do
    args <- mapM (checkArg . Var) inarr
    void $ checkConcatMapLambda fun args

checkLoopOp (Reduce ass size fun inputs) = do
  let (startexps, arrexps) = unzip inputs
  mapM_ (requireI [Basic Cert]) ass
  require [Basic Int] size
  startargs <- mapM checkArg startexps
  arrargs   <- checkSOACArrayArgs size arrexps
  checkLambda fun $ startargs ++ arrargs
  let startt      = map argType startargs
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    bad $ TypeError noLoc $
    "Accumulator is of type " ++ prettyTuple startt ++
    ", but reduce function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    bad $ TypeError noLoc $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but reduce function returns type " ++ prettyTuple funret ++ "."

-- Scan is exactly identical to Reduce.  Duplicate for clarity anyway.
checkLoopOp (Scan ass size fun inputs) = do
  let (startexps, arrexps) = unzip inputs
  mapM_ (requireI [Basic Cert]) ass
  require [Basic Int] size
  startargs <- mapM checkArg startexps
  arrargs   <- checkSOACArrayArgs size arrexps
  checkLambda fun $ startargs ++ arrargs
  let startt      = map argType startargs
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    bad $ TypeError noLoc $
    "Initial value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    bad $ TypeError noLoc $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."

checkLoopOp (Redomap ass size outerfun innerfun accexps arrexps) = do
  mapM_ (requireI [Basic Cert]) ass
  require [Basic Int] size
  arrargs <- checkSOACArrayArgs size arrexps
  accargs <- mapM checkArg accexps
  checkLambda innerfun $ accargs ++ arrargs
  let innerRetType = lambdaReturnType innerfun
      innerAccType = take (length accexps) innerRetType
      asArg t = (t, mempty)
  checkLambda outerfun $ map asArg $ innerAccType ++ innerAccType
  let acct = map argType accargs
      outerRetType = lambdaReturnType outerfun
  unless (acct == innerAccType ) $
    bad $ TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap inner reduction returns type " ++ prettyTuple innerRetType ++ "."
  unless (acct == outerRetType) $
    bad $ TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap outer reduction returns type " ++ prettyTuple outerRetType ++ "."

checkLoopOp (MapKernel cs w index ispace inps returns body) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] w
  mapM_ (require [Basic Int]) bounds
  index_param <- basicLParamM index Int
  iparams' <- forM iparams $ \iparam -> basicLParamM iparam Int
  forM_ returns $ \(t, perm) ->
    let return_rank = arrayRank t + rank
    in unless (sort perm == [0..return_rank - 1]) $
       bad $ TypeError noLoc $
       "Permutation " ++ pretty perm ++
       " not valid for returning " ++ pretty t ++
       " from a rank " ++ pretty rank ++ " kernel."
  checkFun' (nameFromString "<kernel body>",
             map (`toDecl` Nonunique) $ staticShapes rettype,
             lamParamsToNamesTypesAndLores $ index_param : iparams' ++ map kernelInputParam inps,
             body) $ do
    checkLambdaParams $ map kernelInputParam inps
    mapM_ checkKernelInput inps
    checkBody body
    bodyt <- bodyExtType body
    unless (map rankShaped bodyt ==
            map rankShaped (staticShapes rettype)) $
      bad $
      ReturnTypeError noLoc (nameFromString "<kernel body>")
      (Several $ staticShapes rettype)
      (Several bodyt)
  where (iparams, bounds) = unzip ispace
        rank = length ispace
        (rettype, _) = unzip returns
        checkKernelInput inp = do
          checkExp $ PrimOp $ Index []
            (kernelInputArray inp) (kernelInputIndices inp)

          arr_t <- lookupType $ kernelInputArray inp
          unless (stripArray (length $ kernelInputIndices inp) arr_t ==
                  kernelInputType inp) $
            bad $ TypeError noLoc $
            "Kernel input " ++ pretty inp ++ " has inconsistent type."

checkLoopOp (ReduceKernel cs w kernel_size parfun seqfun accexps arrexps) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] w
  checkKernelSize kernel_size
  arrargs <- checkSOACArrayArgs w arrexps
  accargs <- mapM checkArg accexps

  case lambdaParams seqfun of
    [] -> bad $ TypeError noLoc "Fold function takes no parameters."
    chunk_param : _
      | Basic Int <- paramType chunk_param -> do
          let seq_args = (Basic Int, mempty) :
                         [ (t `arrayOfRow` Var (paramName chunk_param), als)
                         | (t, als) <- arrargs ]
          checkLambda seqfun seq_args
      | otherwise ->
          bad $ TypeError noLoc "First parameter of fold function is not integer-typed."

  let seqRetType = lambdaReturnType seqfun
      asArg t = (t, mempty)
  checkLambda parfun $ map asArg $ Basic Int : seqRetType ++ seqRetType
  let acct = map argType accargs
      parRetType = lambdaReturnType parfun
  unless (acct == seqRetType) $
    bad $ TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap fold function returns type " ++ prettyTuple seqRetType ++ "."
  unless (acct == parRetType) $
    bad $ TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomap reduction function returns type " ++ prettyTuple parRetType ++ "."

checkLoopOp (ScanKernel cs w kernel_size _ fun input) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] w
  checkKernelSize kernel_size
  let (nes, arrs) = unzip input
      other_index_arg = (Basic Int, mempty)
  arrargs <- checkSOACArrayArgs w arrs
  accargs <- mapM checkArg nes
  checkLambda fun $ other_index_arg : accargs ++ arrargs
  let startt      = map argType accargs
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun
  unless (startt == funret) $
    bad $ TypeError noLoc $
    "Initial value is of type " ++ prettyTuple startt ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype == funret) $
    bad $ TypeError noLoc $
    "Array element value is of type " ++ prettyTuple intupletype ++
    ", but scan function returns type " ++ prettyTuple funret ++ "."

checkLoopOp (Stream ass size form lam arrexps _) = do
  let accexps = getStreamAccums form
  mapM_ (requireI [Basic Cert]) ass
  require [Basic Int] size
  accargs <- mapM checkArg accexps
  arrargs <- mapM lookupType arrexps
  _ <- checkSOACArrayArgs size arrexps
  let chunk = head $ extLambdaParams lam
  let asArg t = (t, mempty)
      inttp   = Basic Int
      lamarrs'= map (`setOuterSize` Var (paramName chunk)) arrargs
  checkExtLambda lam $ asArg inttp :
                       accargs ++ map asArg lamarrs'
  let acc_len= length accexps
  let lamrtp = take acc_len $ extLambdaReturnType lam
  unless (staticShapes (map argType accargs) == lamrtp) $
    bad $ TypeError noLoc "Stream with inconsistent accumulator type in lambda."
  -- check reduce's lambda, if any
  _ <- case form of
        RedLike _ lam0 _ -> do
            let acct = map argType accargs
                outerRetType = lambdaReturnType lam0
            checkLambda lam0 (accargs ++ accargs)
            unless (acct == outerRetType) $
                bad $ TypeError noLoc $ "Initial value is of type " ++ prettyTuple acct ++
                      ", but stream's reduce lambda returns type " ++ prettyTuple outerRetType ++ "."
        _ -> return ()
  -- just get the dflow of lambda on the fakearg, which does not alias
  -- arr, so we can later check that aliases of arr are not used inside lam.
  -- let fakearg = (fromDecl $ addNames $ removeNames $ typeOf arr', mempty, srclocOf pos)
  let fake_lamarrs' = map asArg lamarrs'
  (_,occurs) <- collectOccurences $
                checkExtLambda lam $ asArg inttp :
                                     accargs ++ fake_lamarrs'
  let usages = usageMap occurs
  arr_aliases <- mapM lookupAliases arrexps
  let aliased_syms = HS.toList $ HS.fromList $ concatMap HS.toList arr_aliases
  when (any (`HM.member` usages) aliased_syms) $
     bad $ TypeError noLoc "Stream with input array used inside lambda."
  -- check outerdim of Lambda's streamed-in array params are NOT specified,
  -- and that return type inner dimens are all specified but not as other
  -- lambda parameters!
  let lamarr_rtp = drop acc_len $ extLambdaReturnType lam
      lamarr_ptp = map paramType $ drop (acc_len+1) $ extLambdaParams lam
      names_lamparams = HS.fromList $ map paramName $ extLambdaParams lam
  _ <- mapM (checkOuterDim (paramName chunk) . head .    shapeDims . arrayShape) lamarr_ptp
  _ <- mapM (checkInnerDim names_lamparams   . tail . extShapeDims . arrayShape) lamarr_rtp
  return ()
    where checkOuterDim chunknm outdim = do
            let chunk_str = textual chunknm
            case outdim of
                    Constant _ ->
                      bad $ TypeError noLoc ("Stream: outer dimension of stream should NOT"++
                                             " be specified since it is "++chunk_str++"by default.")
                    Var idd    ->
                      if idd == chunknm then return True
                      else bad $ TypeError noLoc ("Stream: outer dimension of stream should NOT"++
                                                  " be specified since it is "++chunk_str++"by default.")
          boundDim (Free (Var idd)) = return $ Just idd
          boundDim (Free _        ) = return Nothing
          boundDim (Ext  _        ) =
            bad $ TypeError noLoc $ "Stream's lambda: inner dimensions of the"++
                                    " streamed-out arrays MUST be specified!"
          checkInnerDim lamparnms innerdims = do
            rtp_iner_syms <- catMaybes <$> mapM boundDim innerdims
            case find (`HS.member` lamparnms) rtp_iner_syms of
                Just name -> bad $ TypeError noLoc $
                                   "Stream's lambda: " ++ textual (baseName name) ++
                                   " cannot specify an inner result shape"
                _ -> return True

checkExp :: Checkable lore =>
            Exp lore -> TypeM lore ()

checkExp (PrimOp op) = checkPrimOp op

checkExp (LoopOp op) = checkLoopOp op

checkExp (If e1 e2 e3 ts) = do
  require [Basic Bool] e1
  _ <- checkBody e2 `alternative` checkBody e3
  ts2 <- bodyExtType e2
  ts3 <- bodyExtType e3
  unless ((ts2 `generaliseExtTypes` ts3) `subtypesOf` ts) $
    bad $ TypeError noLoc $
    unlines ["If-expression branches have types",
             "  " ++ prettyTuple ts2 ++ ", and",
             "  " ++ prettyTuple ts3,
             "But the annotation is",
             "  " ++ prettyTuple ts]

checkExp (Apply fname args t)
  | "trace" <- nameToString fname = do
  argts <- mapM (checkSubExp . fst) args
  when (staticShapes argts /= map fromDecl (retTypeValues t)) $
    bad $ TypeError noLoc $ "Expected apply result type " ++ pretty t
    ++ " but got " ++ pretty argts

checkExp (Apply fname args rettype_annot) = do
  (rettype_derived, paramtypes) <- lookupFun fname $ map fst args
  argflows <- mapM (checkArg . fst) args
  when (rettype_derived /= rettype_annot) $
    bad $ TypeError noLoc $ "Expected apply result type " ++ pretty rettype_derived
    ++ " but annotation is " ++ pretty rettype_annot
  checkFuncall (Just fname) paramtypes argflows

checkSOACArrayArgs :: Checkable lore =>
                      SubExp -> [VName] -> TypeM lore [Arg]
checkSOACArrayArgs width vs =
  forM vs $ \v -> do
    (vt, v') <- checkSOACArrayArg v
    let argSize = arraySize 0 vt
    unless (argSize == width) $
      bad $ TypeError noLoc $
      "SOAC argument " ++ pretty v ++ " has outer size " ++
      pretty argSize ++ ", but width of SOAC is " ++
      pretty width
    return v'
  where checkSOACArrayArg ident = do
          (t, als) <- checkArg $ Var ident
          case peelArray 1 t of
            Nothing -> bad $ TypeError noLoc $
                       "SOAC argument " ++ pretty ident ++ " is not an array"
            Just rt -> return (t, (rt, als))

checkType :: Checkable lore =>
             TypeBase Shape u -> TypeM lore ()
checkType = mapM_ checkSubExp . arrayDims

checkExtType :: Checkable lore =>
                TypeBase ExtShape u
             -> TypeM lore ()
checkExtType = mapM_ checkExtDim . extShapeDims . arrayShape
  where checkExtDim (Free se) = void $ checkSubExp se
        checkExtDim (Ext _)   = return ()

checkBinOp :: Checkable lore =>
              BinOp -> SubExp -> SubExp -> BasicType
           -> TypeM lore ()
checkBinOp Plus e1 e2 t = checkPolyBinOp Plus [Float32, Float64, Int] e1 e2 t
checkBinOp Minus e1 e2 t = checkPolyBinOp Minus [Float32, Float64, Int] e1 e2 t
checkBinOp Pow e1 e2 t = checkPolyBinOp Pow [Float32, Float64, Int] e1 e2 t
checkBinOp Times e1 e2 t = checkPolyBinOp Times [Float32, Float64, Int] e1 e2 t
checkBinOp FloatDiv e1 e2 t = checkPolyBinOp FloatDiv [Float32, Float64] e1 e2 t
checkBinOp Div e1 e2 t = checkPolyBinOp Div [Int] e1 e2 t
checkBinOp Mod e1 e2 t = checkPolyBinOp Mod [Int] e1 e2 t
checkBinOp Quot e1 e2 t = checkPolyBinOp Quot [Int] e1 e2 t
checkBinOp Rem e1 e2 t = checkPolyBinOp Rem [Int] e1 e2 t
checkBinOp ShiftR e1 e2 t = checkPolyBinOp ShiftR [Int] e1 e2 t
checkBinOp ShiftL e1 e2 t = checkPolyBinOp ShiftL [Int] e1 e2 t
checkBinOp Band e1 e2 t = checkPolyBinOp Band [Int] e1 e2 t
checkBinOp Xor e1 e2 t = checkPolyBinOp Xor [Int] e1 e2 t
checkBinOp Bor e1 e2 t = checkPolyBinOp Bor [Int] e1 e2 t
checkBinOp LogAnd e1 e2 t = checkPolyBinOp LogAnd [Bool] e1 e2 t
checkBinOp LogOr e1 e2 t = checkPolyBinOp LogOr [Bool] e1 e2 t
checkBinOp Equal e1 e2 t = checkRelOp Equal [Int, Float32, Float64] e1 e2 t
checkBinOp Less e1 e2 t = checkRelOp Less [Int, Float32, Float64] e1 e2 t
checkBinOp Leq e1 e2 t = checkRelOp Leq [Int, Float32, Float64] e1 e2 t

checkRelOp :: Checkable lore =>
              BinOp -> [BasicType]
           -> SubExp -> SubExp
           -> BasicType
           -> TypeM lore ()
checkRelOp op tl e1 e2 t = do
  require (map Basic tl) e1
  require (map Basic tl) e2
  _ <- matchSubExpTypes e1 e2
  checkAnnotation (pretty op ++ " result") (Basic t) $ Basic Bool

checkPolyBinOp :: Checkable lore =>
                  BinOp -> [BasicType]
               -> SubExp -> SubExp -> BasicType
               -> TypeM lore ()
checkPolyBinOp op tl e1 e2 t = do
  require (map Basic tl) e1
  require (map Basic tl) e2
  t' <- matchSubExpTypes e1 e2
  checkAnnotation (pretty op ++ " result") (Basic t) t'

checkKernelSize :: Checkable lore =>
                   KernelSize -> TypeM lore ()
checkKernelSize (KernelSize num_groups workgroup_size per_thread_elements
                 num_elements offset_multiple num_threads) = do
  require [Basic Int] num_groups
  require [Basic Int] workgroup_size
  require [Basic Int] per_thread_elements
  require [Basic Int] num_elements
  require [Basic Int] offset_multiple
  require [Basic Int] num_threads

checkPatElem :: Checkable lore =>
                PatElem lore -> TypeM lore ()
checkPatElem (PatElem name bindage attr) = do
  checkBindage bindage
  checkLetBoundLore name attr

checkBindage :: Checkable lore =>
                Bindage -> TypeM lore ()
checkBindage BindVar = return ()
checkBindage (BindInPlace cs src is) = do
  mapM_ (requireI [Basic Cert]) cs
  srct <- lookupType src
  mapM_ (require [Basic Int]) is

  consume =<< lookupAliases src

  -- Check that the new value has the same type as what is already
  -- there (It does not have to be unique, though.)
  case peelArray (length is) srct of
    Nothing -> bad $ IndexingError src
                     (arrayRank srct) (length is) noLoc
    Just _  -> return ()

checkBinding :: Checkable lore =>
                Pattern lore -> Exp lore
             -> TypeM lore a
             -> TypeM lore a
checkBinding pat e m = do
  context ("When matching\n" ++ message "  " pat ++ "\nwith\n" ++ message "  " e) $
    matchPattern (removePatternAliases pat) (removeExpAliases e)
  binding (zip (identsAndLore pat)
           (map (unNames . fst . patElemAttr) $
            patternElements pat)) $ do
    mapM_ checkPatElem (patternElements $ removePatternAliases pat)
    m
  where identsAndLore = map identAndLore . patternElements . removePatternAliases
        identAndLore bindee = (patElemIdent bindee, LetBound $ patElemAttr bindee)

matchExtPattern :: Checkable lore => [PatElem lore] -> [ExtType] -> TypeM lore ()
matchExtPattern pat ts = do
  (ts', restpat, _) <- liftEitherS $ patternContext pat ts
  unless (length restpat == length ts') $
    bad $ InvalidPatternError (Several pat) (Several ts) Nothing noLoc
  evalStateT (zipWithM_ checkBinding' restpat ts') []
  where checkBinding' patElem@(PatElem name _ _) t = do
          lift $ checkAnnotation ("binding of variable " ++ textual name)
            (patElemRequires patElem) t
          add name

        add name = do
          seen <- gets $ elem name
          if seen
            then lift $ bad $ DupPatternError name noLoc noLoc
            else modify (name:)

matchExtReturnType :: Name -> [ExtType] -> Result
                   -> TypeM lore ()
matchExtReturnType fname rettype ses = do
  ts <- staticShapes <$> mapM subExpType ses
  unless (ts `subtypesOf` rettype) $
    bad $ ReturnTypeError noLoc fname
          (Several rettype)
          (Several ts)

patternContext :: Typed attr =>
                  [PatElemT attr] -> [ExtType] ->
                  Either String ([Type], [PatElemT attr], [PatElemT attr])
patternContext pat rt = do
  (rt', (restpat,_), shapepat) <- runRWST (mapM extract rt) () (pat, HM.empty)
  return (rt', restpat, shapepat)
  where extract t = setArrayShape t <$> Shape <$>
                    mapM extract' (extShapeDims $ arrayShape t)
        extract' (Free se) = return se
        extract' (Ext x)   = correspondingVar x
        correspondingVar x = do
          (remnames, m) <- get
          case (remnames, HM.lookup x m) of
            (_, Just v) -> return $ Var $ patElemName v
            (v:vs, Nothing)
              | Basic Int <- patElemType v -> do
                tell [v]
                put (vs, HM.insert x v m)
                return $ Var $ patElemName v
            (_, Nothing) ->
              lift $ Left "Pattern cannot match context"

validApply :: ArrayShape shape =>
              [TypeBase shape Uniqueness]
           -> [TypeBase shape NoUniqueness]
           -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf
       (map rankShaped got)
       (map (fromDecl . rankShaped) expected))

type Arg = (Type, Names)

argType :: Arg -> Type
argType (t, _) = t

argAliases :: Arg -> Names
argAliases (_, als) = als

checkArg :: Checkable lore =>
            SubExp -> TypeM lore Arg
checkArg arg = do als <- subExpAliasesM arg
                  argt <- subExpType arg
                  return (argt, als)

checkFuncall :: Checkable lore =>
                Maybe Name
             -> [DeclType] -> [Arg]
             -> TypeM lore ()
checkFuncall fname paramts args = do
  let argts = map argType args
  unless (validApply paramts argts) $
    bad $ ParameterMismatch fname noLoc
          (Right $ map (justOne . staticShapes1 . fromDecl) paramts) $
          map (justOne . staticShapes1 . argType) args
  forM_ (zip (map diet paramts) args) $ \(d, (_, als)) ->
    occur [consumption (consumeArg als d)]
  where consumeArg als Consume = als
        consumeArg _   Observe = mempty

checkLambda :: Checkable lore =>
               Lambda lore -> [Arg] -> TypeM lore ()
checkLambda (Lambda i params body ret) args = do
  mapM_ checkType ret
  iparam <- basicLParamM i Int
  if length params == length args then do
    checkFuncall Nothing (map ((`toDecl` Nonunique) . paramType) params) args
    consumeOnlyParams (zip (map paramName params) (map argAliases args)) $
      checkAnonymousFun (nameFromString "<anonymous>", ret, iparam:params, body)
  else bad $ TypeError noLoc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkConcatMapLambda :: Checkable lore =>
                        Lambda lore -> [Arg] -> TypeM lore ()
checkConcatMapLambda (Lambda i params body rettype) args = do
  mapM_ checkType rettype
  iparam <- basicLParamM i Int
  let (_,elemparams) =
        splitAt (length params - length args) params
      fname = nameFromString "<anonymous>"
      rettype' = [ arrayOf t (ExtShape [Ext 0]) Nonunique
                 | t <- staticShapes rettype ]
  if length elemparams == length args then do
    checkFuncall Nothing (map ((`toDecl` Nonunique) . paramType) elemparams) args
    consumeOnlyParams (zip (map paramName params) (map argAliases args)) $
     checkFun' (fname,
                rettype',
                [ (paramName param,
                   toDecl (paramType param) Unique,
                   LambdaBound $ paramAttr param)
                | param <- iparam:params ],
                body) $
      checkBindings (bodyBindings body) $ do
        checkResult $ bodyResult body
        matchExtReturnType fname (map fromDecl rettype') $ bodyResult body
  else bad $ TypeError noLoc $ "concatMap function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " array arguments."

checkExtLambda :: Checkable lore =>
                  ExtLambda lore -> [Arg] -> TypeM lore ()
checkExtLambda (ExtLambda i params body rettype) args =
  if length params == length args then do
    iparam <- basicLParamM i Int
    checkFuncall Nothing (map ((`toDecl` Nonunique) . paramType) params) args
    let fname = nameFromString "<anonymous>"
    consumeOnlyParams (zip (map paramName params) (map argAliases args)) $
      checkFun' (fname,
                 map (`toDecl` Nonunique) rettype,
                 [ (paramName param,
                    toDecl (paramType param) Unique,
                    LambdaBound $ paramAttr param)
                 | param <- iparam:params ],
                 body) $
      checkBindings (bodyBindings body) $ do
        checkResult $ bodyResult body
        matchExtReturnType fname rettype $ bodyResult body
    else bad $ TypeError noLoc $
         "Existential lambda defined with " ++ show (length params) ++
         " parameters, but expected to take " ++ show (length args) ++ " arguments."

-- | The class of lores that can be type-checked.
class (FreeIn (Annotations.Exp lore),
       FreeIn (Annotations.LetBound lore),
       FreeIn (Annotations.Body lore),
       Lore lore, PrettyLore lore) => Checkable lore where
  checkExpLore :: Annotations.Exp lore -> TypeM lore ()
  checkBodyLore :: Annotations.Body lore -> TypeM lore ()
  checkFParamLore :: VName -> Annotations.FParam lore -> TypeM lore ()
  checkLParamLore :: VName -> Annotations.LParam lore -> TypeM lore ()
  checkLetBoundLore :: VName -> Annotations.LetBound lore -> TypeM lore ()
  checkRetType :: AST.RetType lore -> TypeM lore ()
  matchPattern :: AST.Pattern lore -> AST.Exp lore ->
                  TypeM lore ()
  basicFParam :: lore -> VName -> BasicType -> AST.FParam lore
  basicLParam :: lore -> VName -> BasicType -> AST.LParam lore
  matchReturnType :: Name -> RetType lore -> AST.Result -> TypeM lore ()

basicFParamM :: forall lore.Checkable lore =>
                VName -> BasicType -> TypeM lore (AST.FParam lore)
basicFParamM name t =
  return $ basicFParam (representative :: lore) name t

basicLParamM :: forall lore.Checkable lore =>
                VName -> BasicType -> TypeM lore (AST.LParam lore)
basicLParamM name t =
  return $ basicLParam (representative :: lore) name t
