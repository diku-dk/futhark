{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
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
  , checkIdent
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

import Data.List
import Data.Loc
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Text.PrettyPrint.Mainland as PP

import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST as AST
import Futhark.Representation.Aliases
import Futhark.MonadFreshNames
import Futhark.TypeCheck.TypeError
import Futhark.Analysis.Alias

-- | Information about an error that occured during type checking.
data TypeError lore = Error [String] (ErrorCase lore)

-- | What went wrong.
type ErrorCase lore =
  GenTypeError VName (Exp lore) (Several DeclType) (Several Ident)

instance PrettyLore lore => Show (TypeError lore) where
  show (Error [] err) =
    show err
  show (Error msgs err) =
    intercalate "\n" msgs ++ "\n" ++ show err

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding lore = (RetType lore, [(Maybe VName, DeclType)])

data VarBindingLore lore = LetBound (Lore.LetBound lore)
                         | FunBound (Lore.FParam lore)
                         | LambdaBound

data VarBinding lore = Bound Type (VarBindingLore lore) Names
                     | WasConsumed SrcLoc

data Usage = Consumed SrcLoc
           | Observed SrcLoc
             deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names
                           , consumed :: Names
                           , location :: SrcLoc
                           }
             deriving (Eq, Show)

instance Located Occurence where
  locOf = locOf . location

observation :: Names -> SrcLoc -> Occurence
observation = flip Occurence HS.empty

consumption :: Names -> SrcLoc -> Occurence
consumption = Occurence HS.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = HS.null (observed occ) && HS.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = HM.HashMap VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb HM.empty
  where comb m (Occurence obs cons loc) =
          let m' = HS.foldl' (ins $ Observed loc) m obs
          in HS.foldl' (ins $ Consumed loc) m' cons
        ins v m k = HM.insertWith (++) k [v] m

combineOccurences :: VName -> Usage -> Usage
                  -> Either (ErrorCase lore) Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume name (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences
                -> Either (ErrorCase lore) ()
checkOccurences = void . HM.traverseWithKey comb . usageMap
  where comb _    []     = Right ()
        comb name (u:us) = foldM_ (combineOccurences name) u us

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

-- | The 'VarUsage' data structure is used to keep track of which
-- variables have been referenced inside an expression, as well as
-- which variables the resulting expression may possibly alias.
data Dataflow = Dataflow {
    usageOccurences :: Occurences
  } deriving (Show)

instance Monoid Dataflow where
  mempty = Dataflow mempty
  Dataflow o1 `mappend` Dataflow o2 =
    Dataflow (o1 ++ o2)

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
                              Dataflow           -- Writer
                              (NameSource VName) -- State
                              (Either (TypeError lore)) -- Inner monad
                              a)
  deriving (Monad, Functor, Applicative,
            MonadReader (TypeEnv lore),
            MonadWriter Dataflow,
            MonadState VNameSource)

runTypeM :: TypeEnv lore -> NameSource VName -> TypeM lore a
         -> Either (TypeError lore) a
runTypeM env src (TypeM m) = fst <$> evalRWST m env src

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
message s x = PP.pretty 80 $
              PP.text s PP.<+> PP.align (PP.ppr x)

instance MonadFreshNames (TypeM lore) where
  getNameSource = get
  putNameSource = put

liftEither :: Either (ErrorCase lore) a -> TypeM lore a
liftEither = either bad return

liftEitherS :: SrcLoc -> Either String a -> TypeM lore a
liftEitherS loc = either (bad . TypeError loc) return

occur :: Occurences -> TypeM lore ()
occur occurs = tell Dataflow { usageOccurences = occurs }

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: Ident -> TypeM lore ()
observe (Ident nm t loc)
  | basicType t = return ()
  | otherwise   = do names <- lookupAliases nm loc
                     occur [observation names loc]

-- | Proclaim that we have written to the given variable.
consume :: SrcLoc -> Names -> TypeM lore ()
consume loc als = occur [consumption als loc]

collectDataflow :: TypeM lore a -> TypeM lore (a, Dataflow)
collectDataflow m = pass $ do
  (x, dataflow) <- listen m
  return ((x, dataflow), const mempty)

noDataflow :: TypeM lore a -> TypeM lore a
noDataflow = censor $ const mempty

maybeCheckOccurences :: Occurences -> TypeM lore ()
maybeCheckOccurences us = do
  check <- asks envCheckOccurences
  when check $ liftEither $ checkOccurences us

alternative :: TypeM lore a -> TypeM lore b -> TypeM lore (a,b)
alternative m1 m2 = pass $ do
  (x, Dataflow occurs1) <- listen m1
  (y, Dataflow occurs2) <- listen m2
  maybeCheckOccurences occurs1
  maybeCheckOccurences occurs2
  let usage = Dataflow $ occurs1 `altOccurences` occurs2
  return ((x, y), const usage)

-- | Make all bindings nonunique.
noUnique :: TypeM lore a -> TypeM lore a
noUnique = local (\env -> env { envVtable = HM.map f $ envVtable env})
  where f (Bound t attr names) = Bound (t `setUniqueness` Nonunique) attr names
        f (WasConsumed loc)    = WasConsumed loc

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

        bindVar env ((Ident name tp _, attr), immediate) =
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
            ((v, _),_):_ -> bad $ TypeError (srclocOf v) $
                            "Variable " ++ pretty v ++ " being redefined."
          (a, usages) <- collectOccurences m
          maybeCheckOccurences usages
          return a

        -- Collect and remove all occurences in @bnds@.  This relies
        -- on the fact that no variables shadow any other.
        collectOccurences m = pass $ do
          (x, usage) <- listen m
          let (relevant, rest) = split $ usageOccurences usage
          return ((x, relevant), const $ usage { usageOccurences = rest })
          where split = unzip .
                        map (\occ ->
                             let (obs1, obs2) = divide $ observed occ
                                 (con1, con2) = divide $ consumed occ
                             in (occ { observed = obs1, consumed = con1 },
                                 occ { observed = obs2, consumed = con2 }))
                divide s = (s `HS.intersection` boundnameset,
                            s `HS.difference` boundnameset)

lookupVar :: VName -> SrcLoc -> TypeM lore (Type, VarBindingLore lore, Names)
lookupVar name loc = do
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name loc
    Just (Bound t lore names) -> return (t, lore, names)
    Just (WasConsumed wloc) -> bad $ UseAfterConsume name loc wloc

lookupType :: VName -> SrcLoc -> TypeM lore Type
lookupType name loc = do (t, _, _) <- lookupVar name loc
                         return t

lookupAliases :: VName -> SrcLoc -> TypeM lore Names
lookupAliases name loc = do (_, _, als) <- lookupVar name loc
                            return $ HS.insert name als

subExpAliasesM :: SubExp -> TypeM lore Names
subExpAliasesM (Constant {}) = return mempty
subExpAliasesM (Var v)       = lookupAliases (identName v) (srclocOf v)

lookupFun :: SrcLoc -> Name
          -> TypeM lore (RetType lore, [DeclType])
lookupFun loc fname = do
  bnd <- asks $ HM.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname loc
    Just (ftype, params) -> do
      let (_, paramtypes) = unzip params
      return (ftype, paramtypes)

-- | @t1 `unifyTypes` t2@ attempts to unify @t2@ and @t2@.  If
-- unification cannot happen, 'Nothing' is returned, otherwise a type
-- that combines the aliasing of @t1@ and @t2@ is returned.  The
-- uniqueness of the resulting type will be the least of the
-- uniqueness of @t1@ and @t2@.
unifyTypes :: Type -> Type -> Maybe Type
unifyTypes (Basic t1) (Basic t2) = Basic <$> t1 `unifyBasicTypes` t2
unifyTypes (Array t1 ds1 u1) (Array t2 ds2 u2)
  | shapeRank ds1 == shapeRank ds2 = do
  t <- t1 `unifyBasicTypes` t2
  Just $ Array t ds2 (u1 <> u2)
unifyTypes (Mem size1) (Mem size2)
  | size1 == size2 = Just $ Mem size1
unifyTypes _ _ = Nothing

-- | As 'unifyTypes', but for element types.
unifyBasicTypes :: BasicType -> BasicType -> Maybe BasicType
unifyBasicTypes t1 t2
  | t1 == t2  = Just t1
  | otherwise = Nothing

-- | Determine if two types are identical, ignoring uniqueness.
-- Causes a '(TypeError vn)' if they fail to match, and otherwise returns
-- one of them.
unifySubExpTypes :: SubExp -> SubExp -> TypeM lore Type
unifySubExpTypes e1 e2 =
  maybe (bad $ UnifyError (PrimOp $ SubExp e1) (justOne $ toDecl t1)
                          (PrimOp $ SubExp e2) (justOne $ toDecl t2)) return $
  unifyTypes t1 t2
  where t1 = subExpType e1
        t2 = subExpType e2

-- | @checkAnnotation loc s t1 t2@ checks if @t2@ is a subtype of
-- @t1@.  If not, a 'BadAnnotation' is raised.
checkAnnotation :: SrcLoc -> String -> Type -> Type
                -> TypeM lore ()
checkAnnotation loc desc t1 t2
  | t2 `subtypeOf` t1 = return ()
  | otherwise = bad $ BadAnnotation loc desc
                (justOne $ toDecl t1) (justOne $ toDecl t2)

-- | @require ts se@ causes a '(TypeError vn)' if the type of @se@ is
-- not equal with one of the types in @ts@.
require :: Checkable lore => [Type] -> SubExp -> TypeM lore ()
require ts se = do
  t <- checkSubExp se
  unless (any (t `similarTo`) ts) $
    bad $ UnexpectedType (PrimOp $ SubExp se)
    (justOne $ toDecl t)
    (map (justOne . toDecl) ts)

-- | Variant of 'require' working on identifiers.
requireI :: Checkable lore => [Type] -> Ident -> TypeM lore ()
requireI ts ident = require ts $ Var ident

checkArrSubExp :: SubExp -> TypeM lore Type
checkArrSubExp e = case subExpType e of
  t@(Array {}) -> return t
  t            -> bad $ NotAnArray (srclocOf e) (PrimOp $ SubExp e) $
                  justOne $ toDecl t

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

  runTypeM typeenv src $
    mapM_ (noDataflow . checkFun) $ progFunctions prog'
  where
    prog' = aliasAnalysis prog
    src = newNameSourceForProg prog'
    -- To build the ftable we loop through the list of function
    -- definitions.  In addition to the normal ftable information
    -- (name, return type, argument types), we also keep track of
    -- position information, in order to report both locations of
    -- duplicate function definitions.  The position information is
    -- removed at the end.
    buildFtable = HM.map rmLoc <$>
                  foldM expand (HM.map addLoc (initialFtable prog'))
                  (progFunctions prog')
    expand ftable (FunDec name ret args _ pos)
      | Just (_,_,pos2) <- HM.lookup name ftable =
        Left $ Error [] $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map (toDecl . bindeeType) args -- Throw away argument names.
            argnames = map (Just . bindeeName) args
        in Right $ HM.insert name (ret,zip argnames argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    addLoc (t, ts) = (t, ts, noLoc)

-- The prog argument is just to disambiguate the lore.
initialFtable :: Lore lore => Prog lore -> HM.HashMap Name (FunBinding lore)
initialFtable _ = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = (basicRetType t,
                              zip (repeat Nothing) $ map Basic ts)

checkFun :: Checkable lore =>
            FunDec lore -> TypeM lore ()
checkFun (FunDec fname rettype params body loc) =
  context ("In function " ++ nameToString fname) $
    checkFun' (fname,
               retTypeValues rettype,
               funParamsToIdentsAndLores params,
               body,
               loc) $ do
      checkFunParams params
      checkRetType rettype
      checkFunBody fname rettype body

funParamsToIdentsAndLores :: Checkable lore =>
                             [FParam lore]
                          -> [(Ident, VarBindingLore lore)]
funParamsToIdentsAndLores = map identAndLore
  where identAndLore fparam = (bindeeIdent fparam,
                               FunBound $ bindeeLore fparam)

checkFunParams :: Checkable lore =>
                  [FParam lore] -> TypeM lore ()
checkFunParams = mapM_ $ \param ->
  context ("In function parameter " ++ pretty param) $
  checkFParamLore $ bindeeLore param

checkAnonymousFun :: Checkable lore =>
                     (Name, [Type], [Ident], BodyT (Aliases lore), SrcLoc)
                  -> TypeM lore ()
checkAnonymousFun (fname, rettype, params, body, loc) =
  checkFun' (fname,
             staticShapes rettype,
             [ (param, LambdaBound) | param <- params ],
             body,
             loc) $ do
    mapM_ checkType rettype
    checkBody body

checkFun' :: (Checkable lore) =>
             (Name,
              [ExtType],
              [(Ident, VarBindingLore lore)],
              BodyT (Aliases lore),
              SrcLoc)
          -> TypeM lore ()
          -> TypeM lore ()
checkFun' (fname, rettype, paramsWithLore, body, loc) check = do
  checkParams
  binding (zip paramsWithLore $ repeat mempty) check

  checkReturnAlias $ bodyAliases body

  unless (bodyExtType body `subtypesOf` rettype) $
    bad $ ReturnTypeError loc fname
    (Several $ map toDecl rettype)
    (Several $ map toDecl $ bodyExtType body)

  where params = map fst paramsWithLore

        checkParams = foldM_ expand [] params

        expand params' ident@(Ident pname _ _)
          | Just _ <- find ((==identName ident) . identName) params' =
            bad $ DupParamError fname pname loc
          | otherwise =
            return $ ident : params'

        notAliasingParam names =
          forM_ params $ \p ->
            when (not (unique $ identType p) &&
                  identName p `HS.member` names) $
              bad $ ReturnAliased fname (identName p) loc

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' HS.empty .
          returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`HS.member` HS.map snd seen) $ HS.toList names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = do
            notAliasingParam names
            return $ seen `HS.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`HS.member` seen) $ HS.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname loc
          | otherwise = return $ seen `HS.union` tag Nonunique names

        tag u = HS.map $ \name -> (u, name)

        returnAliasing expected got =
          [ (uniqueness p, names) |
            (p,names) <- zip expected got ]

checkSubExp :: Checkable lore => SubExp -> TypeM lore Type
checkSubExp (Constant val _) =
  return $ Basic $ basicValueType val
checkSubExp (Var ident) = context ("In subexp " ++ pretty ident) $ do
  observe ident
  checkIdent ident

checkBindings :: Checkable lore =>
                 [Binding lore] -> TypeM lore a
              -> TypeM lore a
checkBindings origbnds m = delve origbnds
  where delve (Let pat (_,annot) e:bnds) = do
          ((), dataflow) <-
            collectDataflow $
            context ("In expression\n" ++ message "  " e) $
            checkExp e
          checkExpLore annot
          scope <-
            checkBinding (srclocOf e) pat e dataflow
          scope $
            delve bnds
        delve [] =
          m

checkResult :: Checkable lore =>
               Result -> TypeM lore ()
checkResult (Result cs es _) = do
  mapM_ (requireI [Basic Cert]) cs
  mapM_ checkSubExp es

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

checkBody :: Checkable lore =>
             Body lore -> TypeM lore ()
checkBody (Body (_,lore) bnds res) = do
  checkBindings bnds $ checkResult res
  checkBodyLore lore

checkPrimOp :: Checkable lore =>
               PrimOp lore -> TypeM lore ()

checkPrimOp (SubExp es) =
  void $ checkSubExp es

checkPrimOp (ArrayLit es t loc) = do
  mapM_ checkSubExp es
  -- Find the universal type of the array arguments.
  et <- case es of
          [] -> return t
          e:es' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyTypes` subExpType eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ pretty (subExpType eleme) ++ " is not of expected type " ++ pretty elemt ++ "."
            in foldM check (subExpType e) es'

  -- Unify that type with the one given for the array literal.
  checkAnnotation loc "array-element" t et

checkPrimOp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkPrimOp (Not e _) =
  require [Basic Bool] e

checkPrimOp (Negate e _) =
  require [Basic Int, Basic Real] e

checkPrimOp (Index cs ident idxes pos) = do
  mapM_ (requireI [Basic Cert]) cs
  vt <- checkIdent ident
  observe ident
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (identName ident)
          (arrayRank vt) (length idxes) pos
  mapM_ (require [Basic Int]) idxes

checkPrimOp (Update cs src idxes ve loc) = do
  mapM_ (requireI [Basic Cert]) cs
  srct <- checkIdent src
  mapM_ (require [Basic Int]) idxes

  unless (unique srct || basicType srct) $
    bad $ TypeError loc $ "Source '" ++ textual srcname ++ show src ++ "' is not unique"

  venames <- subExpAliasesM ve

  when (srcname `HS.member` venames) $
    bad $ BadLetWithValue loc

  consume loc =<< lookupAliases srcname srcloc

  case peelArray (length idxes) srct of
    Nothing -> bad $ IndexingError srcname
                     (arrayRank srct) (length idxes) loc
    Just rt -> require [rt] ve
  where srcname = identName src
        srcloc  = srclocOf src

checkPrimOp (Iota e _) =
  require [Basic Int] e

checkPrimOp (Replicate countexp valexp _) = do
  require [Basic Int] countexp
  void $ checkSubExp valexp

checkPrimOp (Reshape cs shapeexps arrexp _) = do
  mapM_ (requireI [Basic Cert]) cs
  mapM_ (require [Basic Int]) shapeexps
  void $ checkArrSubExp arrexp

checkPrimOp (Rearrange cs perm arrexp pos) = do
  mapM_ (requireI [Basic Cert]) cs
  arrt <- checkSubExp arrexp
  let rank = arrayRank arrt
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank name
  where name = case arrexp of Var v -> Just $ identName v
                              _     -> Nothing

checkPrimOp (Rotate cs _ arrexp _) = do
  mapM_ (requireI [Basic Cert]) cs
  void $ checkArrSubExp arrexp

checkPrimOp (Split cs splitexp arrexp secsize _) = do
  mapM_ (requireI [Basic Cert]) cs
  require [Basic Int] splitexp
  require [Basic Int] secsize
  void $ checkArrSubExp arrexp

checkPrimOp (Concat cs arr1exp arr2exp ressize _) = do
  mapM_ (requireI [Basic Cert]) cs
  arr1t <- checkArrSubExp arr1exp
  _ <- require [arr1t] arr2exp
  require [Basic Int] ressize

checkPrimOp (Copy e _) =
  void $ checkSubExp e

checkPrimOp (Assert e _) =
  require [Basic Bool] e

checkPrimOp (Conjoin es _) =
  mapM_ (require [Basic Cert]) es

checkPrimOp (Alloc e _) =
  require [Basic Int] e

checkLoopOp :: Checkable lore =>
               LoopOp lore -> TypeM lore ()

checkLoopOp (DoLoop respat merge (Ident loopvar loopvart loopvarloc)
             boundexp loopbody loc) = do
  let (mergepat, mergeexps) = unzip merge
  unless (loopvart == Basic Int) $
    bad $ TypeError loopvarloc "Type annotation of loop variable is not int"
  boundarg <- checkArg boundexp
  mergeargs <- mapM checkArg mergeexps
  iparam <- basicFParam loopvar Int loopvarloc
  let funparams = iparam : mergepat
      paramts   = map (toDecl . bindeeType) funparams
      rettype   = map bindeeType mergepat
  checkFuncall Nothing loc paramts $ boundarg : mergeargs

  noUnique $ checkFun' (nameFromString "<loop body>",
                             staticShapes rettype,
                             funParamsToIdentsAndLores funparams,
                             loopbody,
                             loc) $ do
    checkFunParams funparams
    checkBody loopbody
  forM_ respat $ \res ->
    case find ((==identName res) . bindeeName) mergepat of
      Nothing -> bad $ TypeError loc $ "Loop result variable " ++
                                       textual (identName res) ++
                                       " is not a merge variable."
      Just v  -> return res { identType = bindeeType v }

checkLoopOp (Map ass fun arrexps _) = do
  mapM_ (requireI [Basic Cert]) ass
  arrargs <- mapM checkSOACArrayArg arrexps
  void $ checkLambda fun arrargs

checkLoopOp (Reduce ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  mapM_ (requireI [Basic Cert]) ass
  startargs <- mapM checkArg startexps
  arrargs   <- mapM checkSOACArrayArg arrexps
  checkLambda fun $ startargs ++ arrargs
  let startt      = map argType startargs
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun
  unless (startt `subtypesOf` funret) $
      bad $ TypeError pos $ "Accumulator is of type " ++ prettyTuple startt ++
                            ", but reduce function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
      bad $ TypeError pos $ "Array element value is of type " ++ prettyTuple intupletype ++
                            ", but scan function returns type " ++ prettyTuple funret ++ "."

-- Scan is exactly identical to Reduce.  Duplicate for clarity anyway.
checkLoopOp (Scan ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  mapM_ (requireI [Basic Cert]) ass
  startargs <- mapM checkArg startexps
  arrargs   <- mapM checkSOACArrayArg arrexps
  checkLambda fun $ startargs ++ arrargs
  let startt      = map argType startargs
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun
  unless (startt `subtypesOf` funret) $
    bad $ TypeError pos $ "Initial value is of type " ++ prettyTuple startt ++
                          ", but scan function returns type " ++ prettyTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
    bad $ TypeError pos $ "Array element value is of type " ++ prettyTuple intupletype ++
                          ", but scan function returns type " ++ prettyTuple funret ++ "."

checkLoopOp (Filter ass fun arrexps loc) = do
  mapM_ (requireI [Basic Cert]) ass
  arrargs <- mapM checkSOACArrayArg arrexps
  checkLambda fun arrargs
  let funret = lambdaReturnType fun
  when (funret /= [Basic Bool]) $
    bad $ TypeError loc "Filter function does not return bool."
  when (any (unique . identType) $ lambdaParams fun) $
    bad $ TypeError loc "Filter function consumes its arguments."

checkLoopOp (Redomap ass outerfun innerfun accexps arrexps pos) = do
  mapM_ (requireI [Basic Cert]) ass
  arrargs <- mapM checkSOACArrayArg arrexps
  accargs <- mapM checkArg accexps
  checkLambda innerfun $ accargs ++ arrargs
  let innerRetType = lambdaReturnType innerfun
      asArg t = (t, mempty, mempty, pos)
  checkLambda outerfun $ map asArg $ innerRetType ++ innerRetType

  let acct = map argType accargs
      outerRetType = lambdaReturnType outerfun
  unless (innerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomapT inner reduction returns type " ++ prettyTuple innerRetType ++ "."
  unless (outerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ prettyTuple acct ++
          ", but redomapT outer reduction returns type " ++ prettyTuple outerRetType ++ "."

checkExp :: Checkable lore =>
            Exp lore -> TypeM lore ()

checkExp (PrimOp op) = checkPrimOp op

checkExp (LoopOp op) = checkLoopOp op

checkExp (If e1 e2 e3 ts loc) = do
  require [Basic Bool] e1
  (_, dflow) <-
    collectDataflow $ checkBody e2 `alternative` checkBody e3
  tell dflow
  let ts2 = bodyExtType e2
      ts3 = bodyExtType e3
  unless (ts2 `generaliseExtTypes` ts3 `subtypesOf` ts) $
    bad $ TypeError loc $
    unlines ["If-expression branches have types",
             "  " ++ prettyTuple ts2 ++ ", and",
             "  " ++ prettyTuple ts3,
             "Which is not a subtype of annotation",
             "  " ++ prettyTuple ts]

checkExp (Apply fname args t loc)
  | "trace" <- nameToString fname = do
  argts <- mapM (checkSubExp . fst) args
  when (staticShapes argts /= retTypeValues t) $
    bad $ TypeError loc $ "Expected apply result type " ++ pretty t
    ++ " but got " ++ pretty argts

checkExp (Apply fname args rettype loc) = do
  checkRetType rettype
  (rettype', paramtypes) <- lookupFun loc fname
  argflows <- mapM (checkArg . fst) args

  when (rettype' /= rettype) $
    bad $ TypeError loc $ "Expected apply result type " ++ pretty rettype
    ++ " but got " ++ pretty rettype'
  checkFuncall (Just fname) loc paramtypes argflows

checkSOACArrayArg :: Checkable lore => SubExp
                  -> TypeM lore Arg
checkSOACArrayArg e = do
  (t, als, dflow, argloc) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (rt, als, dflow, argloc)

checkType :: Checkable lore =>
             Type -> TypeM lore ()
checkType = mapM_ checkSubExp . arrayDims

checkExtType :: Checkable lore =>
                TypeBase ExtShape
             -> TypeM lore ()
checkExtType t = mapM_ checkExtDim $ extShapeDims $ arrayShape t
  where checkExtDim (Free se) = void $ checkSubExp se
        checkExtDim (Ext _)   = return ()

checkIdent :: Checkable lore =>
              Ident -> TypeM lore Type
checkIdent (Ident name t loc) =
  context ("In ident " ++ pretty t ++ " " ++ pretty name) $ do
    derived <- lookupType name loc
    unless (derived `subtypeOf` t) $
      bad $ BadAnnotation loc "ident"
      (justOne $ toDecl t) (justOne $ toDecl derived)
    return t

checkBinOp :: Checkable lore =>
              BinOp -> SubExp -> SubExp -> Type -> SrcLoc
           -> TypeM lore ()
checkBinOp Plus e1 e2 t pos = checkPolyBinOp Plus [Real, Int] e1 e2 t pos
checkBinOp Minus e1 e2 t pos = checkPolyBinOp Minus [Real, Int] e1 e2 t pos
checkBinOp Pow e1 e2 t pos = checkPolyBinOp Pow [Real, Int] e1 e2 t pos
checkBinOp Times e1 e2 t pos = checkPolyBinOp Times [Real, Int] e1 e2 t pos
checkBinOp Divide e1 e2 t pos = checkPolyBinOp Divide [Real, Int] e1 e2 t pos
checkBinOp Mod e1 e2 t pos = checkPolyBinOp Mod [Int] e1 e2 t pos
checkBinOp ShiftR e1 e2 t pos = checkPolyBinOp ShiftR [Int] e1 e2 t pos
checkBinOp ShiftL e1 e2 t pos = checkPolyBinOp ShiftL [Int] e1 e2 t pos
checkBinOp Band e1 e2 t pos = checkPolyBinOp Band [Int] e1 e2 t pos
checkBinOp Xor e1 e2 t pos = checkPolyBinOp Xor [Int] e1 e2 t pos
checkBinOp Bor e1 e2 t pos = checkPolyBinOp Bor [Int] e1 e2 t pos
checkBinOp LogAnd e1 e2 t pos = checkPolyBinOp LogAnd [Bool] e1 e2 t pos
checkBinOp LogOr e1 e2 t pos = checkPolyBinOp LogOr [Bool] e1 e2 t pos
checkBinOp Equal e1 e2 t pos = checkRelOp Equal [Int, Real] e1 e2 t pos
checkBinOp Less e1 e2 t pos = checkRelOp Less [Int, Real] e1 e2 t pos
checkBinOp Leq e1 e2 t pos = checkRelOp Leq [Int, Real] e1 e2 t pos

checkRelOp :: Checkable lore =>
              BinOp -> [BasicType]
           -> SubExp -> SubExp
           -> Type -> SrcLoc
           -> TypeM lore ()
checkRelOp op tl e1 e2 t pos = do
  require (map Basic tl) e1
  require (map Basic tl) e2
  _ <- unifySubExpTypes e1 e2
  checkAnnotation pos (opStr op ++ " result") t $ Basic Bool

checkPolyBinOp :: Checkable lore =>
                  BinOp -> [BasicType]
               -> SubExp -> SubExp -> Type -> SrcLoc
               -> TypeM lore ()
checkPolyBinOp op tl e1 e2 t pos = do
  require (map Basic tl) e1
  require (map Basic tl) e2
  t' <- unifySubExpTypes e1 e2
  checkAnnotation pos (opStr op ++ " result") t t'

sequentially :: Checkable lore =>
                TypeM lore a -> (a -> Dataflow -> TypeM lore b) -> TypeM lore b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  occur $ usageOccurences m1flow `seqOccurences`
          usageOccurences m2flow
  return b

checkBinding :: Checkable lore =>
                SrcLoc -> Pattern lore -> Exp lore -> Dataflow
             -> TypeM lore (TypeM lore a -> TypeM lore a)
checkBinding loc pat e dflow =
  context ("When matching\n" ++ message "  " pat ++ "\nwith\n" ++ message "  " e) $ do
    matchPattern loc (removePatternAliases pat) (removeExpAliases e)
    return $ \m -> sequentially (tell dflow)
                   (const . const $
                    binding (zip (identsAndLore pat)
                             (map (unNames . fst . bindeeLore) $
                              patternBindees pat))
                    (checkPatSizes (patternIdents pat) >> m))
  where identsAndLore = map identAndLore . patternBindees . removePatternAliases
        identAndLore bindee = (bindeeIdent bindee, LetBound $ bindeeLore bindee)

matchExtPattern :: SrcLoc -> [Ident] -> [ExtType] -> TypeM lore ()
matchExtPattern loc pat ts = do
  (ts', restpat, _) <- liftEitherS loc $ patternContext pat ts
  unless (length restpat == length ts') $
    bad $ InvalidPatternError (Several pat) (Several $ map toDecl ts) Nothing loc
  evalStateT (zipWithM_ checkBinding' restpat ts') []
  where checkBinding' (Ident name namet vloc) t = do
          lift $ checkAnnotation vloc ("binding of variable " ++ textual name) namet t
          add $ Ident name namet vloc
          return $ Ident name namet vloc

        add ident = do
          bnd <- gets $ find (==ident)
          case bnd of
            Nothing -> modify (ident:)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError name (srclocOf ident) pos2

matchExtReturnType :: Name -> [ExtType] -> Result
               -> TypeM lore ()
matchExtReturnType fname rettype (Result _ ses loc) =
  unless (staticShapes ts `subtypesOf` rettype) $
  bad $ ReturnTypeError loc fname
        (Several $ map toDecl rettype)
        (Several $ map toDecl ts)
  where ts = map subExpType ses

patternContext :: [Ident] -> [ExtType] ->
                  Either String ([Type], [Ident], [Ident])
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
            (_, Just v) -> return $ Var v
            (v:vs, Nothing)
              | Basic Int <- identType v -> do
                tell [v]
                put (vs, HM.insert x v m)
                return $ Var v
            (_, Nothing) ->
              lift $ Left "Pattern cannot match context"

checkPatSizes :: Checkable lore =>
                 [IdentBase Shape] -> TypeM lore ()
checkPatSizes = mapM_ checkBndSizes

checkBndSizes :: Checkable lore =>
                 IdentBase Shape -> TypeM lore ()
checkBndSizes (Ident _ t _) = do
  let dims = arrayDims t
  mapM_ (require [Basic Int]) dims

validApply :: [DeclType] -> [Type] -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf (map toDecl got) expected)

type Arg = (Type, Names, Dataflow, SrcLoc)

argType :: Arg -> Type
argType (t, _, _, _) = t

checkArg :: Checkable lore =>
            SubExp -> TypeM lore Arg
checkArg arg = do (argt, dflow) <- collectDataflow $ checkSubExp arg
                  als <- subExpAliasesM arg
                  return (argt, als, dflow, srclocOf arg)

checkFuncall :: Checkable lore =>
                Maybe Name -> SrcLoc
             -> [DeclType] -> [Arg]
             -> TypeM lore ()
checkFuncall fname loc paramtypes args = do
  let argts = map argType args
  unless (validApply paramtypes argts) $
    bad $ ParameterMismatch fname loc
          (Right $ map justOne paramtypes) $
          map (justOne . toDecl) argts
  forM_ (zip (map diet paramtypes) args) $ \(d, (_, als, dflow, argloc)) -> do
    maybeCheckOccurences $ usageOccurences dflow
    let occurs = [consumption (consumeArg als d) argloc]
    occur $ usageOccurences dflow `seqOccurences` occurs
  where consumeArg als Consume = als
        consumeArg _   Observe = mempty

checkLambda :: Checkable lore =>
               Lambda lore -> [Arg] -> TypeM lore ()
checkLambda (Lambda params body ret loc) args = do
  mapM_ checkType ret
  if length params == length args then do
    let setParamShape param shape =
          param { identType = identType param `setArrayDims` shape }
        params' = zipWith setParamShape params $
                  map (arrayDims . argType) args
    checkFuncall Nothing loc (map (toDecl . identType) params') args
    noUnique $ checkAnonymousFun
      (nameFromString "<anonymous>", ret, params', body, loc)
  else bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

-- | The class of lores that can be type-checked.
class (FreeIn (Lore.Exp lore),
       FreeIn (Lore.LetBound lore),
       FreeIn (Lore.Body lore),
       Lore lore, PrettyLore lore) => Checkable lore where
  checkExpLore :: Lore.Exp lore -> TypeM lore ()
  checkBodyLore :: Lore.Body lore -> TypeM lore ()
  checkFParamLore :: Lore.FParam lore -> TypeM lore ()
  checkRetType :: AST.RetType lore -> TypeM lore ()
  matchPattern :: SrcLoc -> AST.Pattern lore -> AST.Exp lore ->
                  TypeM lore ()
  basicFParam :: VName -> BasicType -> SrcLoc -> TypeM lore (AST.FParam lore)
  matchReturnType :: Name -> RetType lore -> AST.Result -> TypeM lore ()
