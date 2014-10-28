{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
-- | The type checker checks whether the program is type-consistent.
module Futhark.TypeCheck
  ( -- * Interface
    checkProg
  , checkProgNoUniqueness
  , TypeError
    -- * Extensionality
  , TypeM
  , bad
  , Checkable (..)
  , module Futhark.TypeCheck.TypeError
    -- * Checkers
  , require
  , requireI
  , checkSubExp
  , checkIdent
  , checkExtType
  , matchExtPattern
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

import Futhark.Representation.AST.Lore (Lore)
import qualified Futhark.Representation.AST.Lore as Lore
import qualified Futhark.Representation.AST as AST
import Futhark.Representation.Aliases
import Futhark.MonadFreshNames
import Futhark.TypeCheck.TypeError
import Futhark.Analysis.Alias

-- | Information about an error that occured during type checking.
type TypeError lore = GenTypeError VName (Exp lore) (Several DeclType) (Several Ident)

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding lore = (ResType lore, [(Maybe VName, DeclType)])

data VarBinding = Bound Type Names
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

combineOccurences :: VName -> Usage -> Usage -> Either (TypeError lore) Usage
combineOccurences _ (Observed loc) (Observed _) = Right $ Observed loc
combineOccurences name (Consumed wloc) (Observed rloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Observed rloc) (Consumed wloc) =
  Left $ UseAfterConsume name rloc wloc
combineOccurences name (Consumed loc1) (Consumed loc2) =
  Left $ UseAfterConsume name (max loc1 loc2) (min loc1 loc2)

checkOccurences :: Occurences -> Either (TypeError lore) ()
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
  TypeEnv { envVtable :: HM.HashMap VName VarBinding
          , envFtable :: HM.HashMap Name (FunBinding lore)
          , envCheckOccurences :: Bool
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

bad :: TypeError lore -> TypeM lore a
bad = TypeM . lift . Left

instance MonadFreshNames (TypeM lore) where
  getNameSource = get
  putNameSource = put

liftEither :: Either (TypeError lore) a -> TypeM lore a
liftEither = either bad return

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
  where f (Bound t names)   = Bound (t `setUniqueness` Nonunique) names
        f (WasConsumed loc) = WasConsumed loc

-- | Given the immediate aliases, compute the full transitive alias
-- set (including the immediate aliases).
expandAliases :: Names -> TypeEnv lore -> Names
expandAliases names env = names `HS.union` aliasesOfAliases
  where aliasesOfAliases =  mconcat . map look . HS.toList $ names
        look k = case HM.lookup k $ envVtable env of
          Just (Bound _ als) -> als
          _                  -> mempty

binding :: Checkable lore =>
           [(Ident, Names)] -> TypeM lore a -> TypeM lore a
binding bnds = check . local (`bindVars` bnds)
  where bindVars = foldl bindVar
        boundnames = map (identName . fst) bnds
        boundnameset = HS.fromList boundnames

        bindVar env (Ident name tp _, immediate) =
          let names = expandAliases immediate env
              inedges = HS.toList names
              update (Bound tp' thesenames) =
                Bound tp' $ HS.insert name thesenames
              update b = b
          in env { envVtable = HM.insert name (Bound tp names) $
                               adjustSeveral update inedges $
                               envVtable env }

        adjustSeveral f = flip $ foldl $ flip $ HM.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          already_bound <- asks envVtable
          case filter ((`HM.member` already_bound) . identName . fst) bnds of
            []  -> return ()
            (v,_):_ -> bad $ TypeError (srclocOf v) $
                       "Variable " ++ textual (identName v) ++ " being redefined."
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

lookupVar :: VName -> SrcLoc -> TypeM lore (Type, Names)
lookupVar name pos = do
  bnd <- asks $ HM.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name pos
    Just (Bound t names) -> return (t, names)
    Just (WasConsumed wloc) -> bad $ UseAfterConsume name pos wloc

lookupType :: VName -> SrcLoc -> TypeM lore Type
lookupType name loc = fst <$> lookupVar name loc

lookupAliases :: VName -> SrcLoc -> TypeM lore Names
lookupAliases name loc = HS.insert name <$> snd <$> lookupVar name loc

subExpAliasesM :: SubExp -> TypeM lore Names
subExpAliasesM (Constant {}) = return mempty
subExpAliasesM (Var v)       = lookupAliases (identName v) (srclocOf v)

lookupFun :: SrcLoc -> Name
          -> TypeM lore (ResType lore, [DeclType])
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

-- | @checkAnnotation loc s t1 t2@ returns @t2@ if @t1@ contains no
-- type, and otherwise tries to unify them with 'unifyTypes'.  If
-- this fails, a 'BadAnnotation' is raised.
checkAnnotation :: SrcLoc -> String -> Type -> Type
                -> TypeM lore Type
checkAnnotation loc desc t1 t2 =
  case unifyTypes t1 t2 of
    Nothing -> bad $ BadAnnotation loc desc
                     (justOne $ toDecl t1) (justOne $ toDecl t2)
    Just t  -> return t

-- | @require ts e@ causes a '(TypeError vn)' if the type of @e@ does
-- not unify with one of the types in @ts@.  Otherwise, simply returns
-- @e@.  This function is very useful in 'checkExp'.
require :: [Type] -> SubExp -> TypeM lore SubExp
require ts e
  | any (subExpType e `similarTo`) ts = return e
  | otherwise = bad $ UnexpectedType (PrimOp $ SubExp e)
                      (justOne $ toDecl $ subExpType e)
                      (map (justOne . toDecl) ts)

-- | Variant of 'require' working on identifiers.
requireI :: [Type] -> Ident -> TypeM lore Ident
requireI ts ident
  | any (identType ident `similarTo`) ts = return ident
  | otherwise = bad $ UnexpectedType (PrimOp $ SubExp $ Var ident)
                      (justOne $ toDecl $ identType ident)
                      (map (justOne . toDecl) ts)

rowTypeM :: SubExp -> TypeM lore Type
rowTypeM e = maybe wrong return $ peelArray 1 $ subExpType e
  where wrong = bad $ NotAnArray (srclocOf e) (PrimOp $ SubExp e) $
                      justOne $ toDecl $ subExpType e

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: Checkable lore =>
             AST.Prog lore -> Either (TypeError lore) (Prog lore)
checkProg = checkProg' True

-- | As 'checkProg', but don't check whether uniqueness constraints
-- are being upheld.  The uniqueness of types must still be correct.
checkProgNoUniqueness :: Checkable lore =>
                         AST.Prog lore -> Either (TypeError lore) (Prog lore)
checkProgNoUniqueness = checkProg' False

checkProg' :: Checkable lore =>
              Bool -> AST.Prog lore -> Either (TypeError lore) (Prog lore)
checkProg' checkoccurs prog = do
  ftable <- buildFtable
  let typeenv = TypeEnv { envVtable = HM.empty
                        , envFtable = ftable
                        , envCheckOccurences = checkoccurs
                        }

  liftM Prog $ runTypeM typeenv src $
        mapM (noDataflow . checkFun) $ progFunctions prog'
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
        Left $ DupDefinitionError name pos pos2
      | otherwise =
        let argtypes = map (toDecl . bindeeType) args -- Throw away argument names.
            argnames = map (Just . bindeeName) args
        in Right $ HM.insert name (ret,zip argnames argtypes,pos) ftable
    rmLoc (ret,args,_) = (ret,args)
    addLoc (t, ts) = (t, ts, noLoc)

-- The prog argument is just to disambiguate the lore.
initialFtable :: Lore lore => Prog lore -> HM.HashMap Name (FunBinding lore)
initialFtable _ = HM.map addBuiltin builtInFunctions
  where addBuiltin (t, ts) = (staticResType [Basic t],
                              zip (repeat Nothing) $ map Basic ts)

checkFun :: Checkable lore =>
            FunDec lore -> TypeM lore (FunDec lore)
checkFun (FunDec fname rettype params body loc) = do
  (rettype', body') <-
    checkFun' (fname, rettype, map bindeeIdent params, body, loc) $ do
      mapM_ (checkFParamLore . bindeeLore) params
      checkFunBody rettype body
  return $ FunDec fname rettype' params body' loc

checkFunBody :: Checkable lore =>
                ResType lore -> BodyT (Aliases lore)
             -> TypeM lore (ResType lore, BodyT (Aliases lore))
checkFunBody rettype body = do
  checkResType rettype
  (,) <$> pure rettype <*> checkBody body

checkAnonymousFun :: Checkable lore =>
                     (Name, ResType lore, [Ident], BodyT (Aliases lore), SrcLoc)
                  -> TypeM lore (ResType lore, BodyT (Aliases lore))
checkAnonymousFun fundec@(_, rettype, _, body, _) =
  checkFun' fundec $ checkFunBody rettype body

checkFun' :: (Checkable lore) =>
             (Name, ResType lore, [Ident], BodyT (Aliases lore), SrcLoc)
          -> TypeM lore (ResType lore, BodyT (Aliases lore))
          -> TypeM lore (ResType lore, BodyT (Aliases lore))
checkFun' (fname, rettype, params, _, loc) check = do
  params' <- checkParams
  (rettype',body') <- binding (zip params' $ repeat mempty) check

  checkReturnAlias $ bodyAliases body'

  if bodyType body' `subtypesOf` resTypeValues rettype' then
    return (rettype', body')
  else bad $ ReturnTypeError loc fname
             (Several $ map toDecl $ resTypeValues rettype')
             (Several $ map toDecl $ bodyType body')

  where checkParams = reverse <$> foldM expand [] params

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
          returnAliasing (resTypeValues rettype)

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

checkSubExp :: Checkable lore => SubExp -> TypeM lore SubExp
checkSubExp (Constant v loc) =
  return $ Constant v loc
checkSubExp (Var ident) = do
  ident' <- checkIdent ident
  observe ident'
  return $ Var ident'

checkBody :: Checkable lore =>
             Body lore -> TypeM lore (Body lore)

checkBody (Body (als,lore) origbnds (Result cs es loc)) = do
  (bnds', res) <- delve origbnds
  -- We need to remove the names that are bound in bnds' from the
  -- alias sets.
  lore' <- checkBodyLore lore
  return $ Body (als,lore') bnds' res
  where delve (Let pat (eals,annot) e:bnds) = do
          (e', dataflow) <- collectDataflow $ checkExp e
          annot' <- checkExpLore annot
          (scope, pat') <-
            checkBinding (srclocOf e) pat (typeOf e') dataflow
          scope $ do
            (bnds', res') <- delve bnds
            return (Let pat' (eals,annot') e' : bnds', res')
        delve [] = do
          cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
          es' <- mapM checkSubExp es
          return ([], Result cs' es' loc)

checkPrimOp :: Checkable lore =>
               PrimOp lore -> TypeM lore (PrimOp lore)

checkPrimOp (SubExp es) =
  SubExp <$> checkSubExp es

checkPrimOp (ArrayLit es t loc) = do
  es' <- mapM checkSubExp es
  -- Find the universal type of the array arguments.
  et <- case es' of
          [] -> return t
          e:es'' ->
            let check elemt eleme
                  | Just elemt' <- elemt `unifyTypes` subExpType eleme =
                    return elemt'
                  | otherwise =
                    bad $ TypeError loc $ ppType (subExpType eleme) ++ " is not of expected type " ++ ppType elemt ++ "."
            in foldM check (subExpType e) es''

  -- Unify that type with the one given for the array literal.
  t' <- checkAnnotation loc "array-element" t et

  return $ ArrayLit es' t' loc

checkPrimOp (BinOp op e1 e2 t pos) = checkBinOp op e1 e2 t pos

checkPrimOp (Not e pos) = do
  e' <- require [Basic Bool] =<< checkSubExp e
  return $ Not e' pos

checkPrimOp (Negate e loc) = do
  e' <- require [Basic Int, Basic Real] =<< checkSubExp e
  return $ Negate e' loc

checkPrimOp (Index cs ident idxes pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  ident' <- checkIdent ident
  observe ident'
  vt <- lookupType (identName ident') pos
  when (arrayRank vt < length idxes) $
    bad $ IndexingError (identName ident)
          (arrayRank vt) (length idxes) pos
  idxes' <- mapM (require [Basic Int] <=< checkSubExp) idxes
  return $ Index cs' ident' idxes' pos

checkPrimOp (Update cs src idxes ve loc) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  src' <- checkIdent src
  idxes' <- mapM (require [Basic Int] <=< checkSubExp) idxes
  ve' <- checkSubExp ve

  unless (unique (identType src') || basicType (identType src')) $
    bad $ TypeError loc $ "Source '" ++ textual (identName src') ++ show src ++ "' is not unique"
  venames <- subExpAliasesM ve'
  when (identName src `HS.member` venames) $
    bad $ BadLetWithValue loc

  consume loc =<< lookupAliases (identName src') (srclocOf src')

  case peelArray (length idxes) (identType src') of
    Nothing -> bad $ IndexingError (identName src)
                     (arrayRank $ identType src') (length idxes) loc
    Just _ -> return $ Update cs' src' idxes' ve' loc

checkPrimOp (Iota e pos) = do
  e' <- require [Basic Int] =<< checkSubExp e
  return $ Iota e' pos

checkPrimOp (Replicate countexp valexp pos) = do
  countexp' <- require [Basic Int] =<< checkSubExp countexp
  valexp' <- checkSubExp valexp
  return $ Replicate countexp' valexp' pos

checkPrimOp (Reshape cs shapeexps arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  shapeexps' <- mapM (require [Basic Int] <=< checkSubExp) shapeexps
  arrexp' <- checkSubExp arrexp
  return (Reshape cs' shapeexps' arrexp' pos)

checkPrimOp (Rearrange cs perm arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arrexp' <- checkSubExp arrexp
  let rank = arrayRank $ subExpType arrexp'
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError pos perm rank name
  return $ Rearrange cs' perm arrexp' pos
  where name = case arrexp of Var v -> Just $ identName v
                              _     -> Nothing

checkPrimOp (Rotate cs n arrexp pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arrexp' <- checkSubExp arrexp
  return $ Rotate cs' n arrexp' pos

checkPrimOp (Split cs splitexp arrexp secsize pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  splitexp' <- require [Basic Int] =<< checkSubExp splitexp
  secsize' <- require [Basic Int] =<< checkSubExp secsize
  arrexp' <- checkSubExp arrexp
  _ <- rowTypeM arrexp' -- Just check that it's an array.
  return $ Split cs' splitexp' arrexp' secsize' pos

checkPrimOp (Concat cs arr1exp arr2exp ressize pos) = do
  cs' <- mapM (requireI [Basic Cert] <=< checkIdent) cs
  arr1exp' <- checkSubExp arr1exp
  arr2exp' <- require [subExpType arr1exp'] =<< checkSubExp arr2exp
  ressize' <- require [Basic Int] =<< checkSubExp ressize
  _ <- rowTypeM arr2exp' -- Just check that it's an array.
  return $ Concat cs' arr1exp' arr2exp' ressize' pos

checkPrimOp (Copy e pos) = do
  e' <- checkSubExp e
  return $ Copy e' pos

checkPrimOp (Assert e pos) = do
  e' <- require [Basic Bool] =<< checkSubExp e
  return $ Assert e' pos

checkPrimOp (Conjoin es pos) = do
  es' <- mapM (require [Basic Cert] <=< checkSubExp) es
  return $ Conjoin es' pos

checkPrimOp (Alloc e loc) =
  Alloc <$> (require [Basic Int] =<< checkSubExp e) <*> pure loc

checkLoopOp :: Checkable lore =>
               LoopOp lore -> TypeM lore (LoopOp lore)

checkLoopOp (DoLoop respat merge (Ident loopvar loopvart loopvarloc)
                 boundexp loopbody loc) = do
  let (mergepat, mergeexps) = unzip merge
  unless (loopvart == Basic Int) $
    bad $ TypeError loopvarloc "Type annotation of loop variable is not int"
  (boundexp', boundarg) <- checkArg boundexp
  (mergeexps', mergeargs) <- unzip <$> mapM checkArg mergeexps
  let mergeparams = mergepat
      funparams = Ident loopvar (Basic Int) loopvarloc : mergeparams
      paramts   = map (toDecl . identType) funparams
      rettype   = map identType mergeparams
      setIdentType v t = v { identType = t }
      mergepat' = zipWith setIdentType mergepat rettype
  checkFuncall Nothing loc paramts $ boundarg : mergeargs

  (_, loopbody') <-
    noUnique $ checkAnonymousFun (nameFromString "<loop body>",
                                  staticResType rettype,
                                  funparams,
                                  loopbody,
                                  loc)
  respat' <-
    forM respat $ \res ->
      case find ((==identName res) . identName) mergepat' of
        Nothing -> bad $ TypeError loc $ "Loop result variable " ++
                                         textual (identName res) ++
                                         " is not a merge variable."
        Just v  -> return res { identType = identType v }

  return $ DoLoop respat' (zip mergepat' mergeexps')
                  (Ident loopvar (Basic Int) loopvarloc) boundexp'
                  loopbody' loc

checkLoopOp (Map ass fun arrexps pos) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'    <- checkLambda fun arrargs
  return $ Map ass' fun' arrexps' pos

checkLoopOp (Reduce ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  (arrexps', arrargs)     <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'                    <- checkLambda fun $ startargs ++ arrargs
  let startt      = map subExpType startexps'
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun'
  unless (startt `subtypesOf` funret) $
      bad $ TypeError pos $ "Accumulator is of type " ++ ppTuple startt ++
                            ", but reduce function returns type " ++ ppTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
      bad $ TypeError pos $ "Array element value is of type " ++ ppTuple intupletype ++
                            ", but scan function returns type " ++ ppTuple funret ++ "."
  return $ Reduce ass' fun' (zip startexps' arrexps') pos

-- ScanT is exactly identical to ReduceT.  Duplicate for clarity
-- anyway.
checkLoopOp (Scan ass fun inputs pos) = do
  let (startexps, arrexps) = unzip inputs
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (startexps', startargs) <- unzip <$> mapM checkArg startexps
  (arrexps', arrargs)     <- unzip <$> mapM checkSOACArrayArg arrexps
  fun'                    <- checkLambda fun $ startargs ++ arrargs
  let startt      = map subExpType startexps'
      intupletype = map argType arrargs
      funret      = lambdaReturnType fun'
  unless (startt `subtypesOf` funret) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple startt ++
                          ", but scan function returns type " ++ ppTuple funret ++ "."
  unless (intupletype `subtypesOf` funret) $
    bad $ TypeError pos $ "Array element value is of type " ++ ppTuple intupletype ++
                          ", but scan function returns type " ++ ppTuple funret ++ "."
  return $ Scan ass' fun' (zip startexps' arrexps') pos

checkLoopOp (Filter ass fun arrexps loc) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (arrexps', arrargs) <- unzip <$> mapM checkSOACArrayArg arrexps
  fun' <- checkLambda fun arrargs
  let funret = lambdaReturnType fun'
  when (funret /= [Basic Bool]) $
    bad $ TypeError loc "Filter function does not return bool."
  when (any (unique . identType) $ lambdaParams fun) $
    bad $ TypeError loc "Filter function consumes its arguments."
  return $ Filter ass' fun' arrexps' loc

checkLoopOp (Redomap ass outerfun innerfun accexps arrexps pos) = do
  ass' <- mapM (requireI [Basic Cert] <=< checkIdent) ass
  (arrexps', arrargs)  <- unzip <$> mapM checkSOACArrayArg arrexps
  (accexps', accargs)  <- unzip <$> mapM checkArg accexps
  innerfun' <- checkLambda innerfun $ accargs ++ arrargs
  let innerRetType = lambdaReturnType innerfun'
      asArg t = (t, mempty, mempty, pos)
  outerfun' <- checkLambda outerfun $ map asArg $ innerRetType ++ innerRetType

  let acct = map subExpType accexps'
      outerRetType = lambdaReturnType outerfun'
  unless (innerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple acct ++
          ", but redomapT inner reduction returns type " ++ ppTuple innerRetType ++ "."
  unless (outerRetType `subtypesOf` acct) $
    bad $ TypeError pos $ "Initial value is of type " ++ ppTuple acct ++
          ", but redomapT outer reduction returns type " ++ ppTuple outerRetType ++ "."

  return $ Redomap ass' outerfun' innerfun' accexps' arrexps' pos

checkExp :: Checkable lore =>
            Exp lore -> TypeM lore (Exp lore)

checkExp (PrimOp op) = PrimOp <$> checkPrimOp op

checkExp (LoopOp op) = LoopOp <$> checkLoopOp op

checkExp (If e1 e2 e3 t pos) = do
  e1' <- require [Basic Bool] =<< checkSubExp e1
  ((e2', e3'), dflow) <- collectDataflow $ checkBody e2 `alternative` checkBody e3
  tell dflow
  let t' = extResType (bodyType e2') `generaliseResTypes`
           extResType (bodyType e3') `generaliseResTypes`
           t
  when (t' /= t) $
    bad $ TypeError pos $ "Expected if result type " ++ ppResType t
    ++ " but got " ++ ppResType t'
  return $ If e1' e2' e3' t' pos

checkExp (Apply fname args t loc)
  | "trace" <- nameToString fname = do
  args' <- mapM (checkSubExp . fst) args
  let t' = staticResType $ map subExpType args'
  when (t' /= t) $
    bad $ TypeError loc $ "Expected apply result type " ++ ppResType t
    ++ " but got " ++ ppResType t'
  return $ Apply fname [(arg, Observe) | arg <- args'] t' loc

checkExp (Apply fname args rettype loc) = do
  checkResType rettype
  (rettype', paramtypes) <- lookupFun loc fname
  (args', argflows) <- unzip <$> mapM (checkArg . fst) args

  when (rettype' /= rettype) $
    bad $ TypeError loc $ "Expected apply result type " ++ ppResType rettype
    ++ " but got " ++ ppResType rettype'
  checkFuncall (Just fname) loc paramtypes argflows
  return $ Apply fname (zip args' $ map diet paramtypes) rettype' loc

checkSOACArrayArg :: Checkable lore => SubExp
                  -> TypeM lore (SubExp, Arg)
checkSOACArrayArg e = do
  (e', (t, als, dflow, argloc)) <- checkArg e
  case peelArray 1 t of
    Nothing -> bad $ TypeError argloc "SOAC argument is not an array"
    Just rt -> return (e', (rt, als, dflow, argloc))

checkType :: Checkable lore =>
             Type -> TypeM lore Type
checkType t = do dims <- mapM checkSubExp $ arrayDims t
                 return $ t `setArrayDims` dims

checkExtType :: Checkable lore =>
                TypeBase ExtShape
             -> TypeM lore ()
checkExtType t = mapM_ checkExtDim $ extShapeDims $ arrayShape t
  where checkExtDim (Free se) = Free <$> checkSubExp se
        checkExtDim (Ext x)   = return $ Ext x

checkIdent :: Checkable lore =>
              Ident -> TypeM lore Ident
checkIdent (Ident name t pos) = do
  vt <- lookupType name pos
  t'' <- checkAnnotation pos ("variable " ++ textual name) t vt
  return $ Ident name t'' pos

checkBinOp :: Checkable lore =>
              BinOp -> SubExp -> SubExp -> Type -> SrcLoc
           -> TypeM lore (PrimOp lore)
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
           -> TypeM lore (PrimOp lore)
checkRelOp op tl e1 e2 t pos = do
  e1' <- require (map Basic tl) =<< checkSubExp e1
  e2' <- require (map Basic tl) =<< checkSubExp e2
  _ <- unifySubExpTypes e1' e2'
  t' <- checkAnnotation pos (opStr op ++ " result") t $ Basic Bool
  return $ BinOp op e1' e2' t' pos

checkPolyBinOp :: Checkable lore =>
                  BinOp -> [BasicType]
               -> SubExp -> SubExp -> Type -> SrcLoc
               -> TypeM lore (PrimOp lore)
checkPolyBinOp op tl e1 e2 t pos = do
  e1' <- require (map Basic tl) =<< checkSubExp e1
  e2' <- require (map Basic tl) =<< checkSubExp e2
  t' <- unifySubExpTypes e1' e2'
  t'' <- checkAnnotation pos (opStr op ++ " result") t t'
  return $ BinOp op e1' e2' t'' pos

sequentially :: Checkable lore =>
                TypeM lore a -> (a -> Dataflow -> TypeM lore b) -> TypeM lore b
sequentially m1 m2 = do
  (a, m1flow) <- collectDataflow m1
  (b, m2flow) <- collectDataflow $ m2 a m1flow
  occur $ usageOccurences m1flow `seqOccurences`
          usageOccurences m2flow
  return b

checkBinding :: Checkable lore =>
                SrcLoc -> Pattern lore -> ResType lore -> Dataflow
             -> TypeM lore (TypeM lore a -> TypeM lore a, Pattern lore)
checkBinding loc pat ts dflow = do
  matchPattern loc pat ts
  return (\m -> sequentially (tell dflow)
                (const . const $
                 binding (zip (patternIdents pat)
                          (map (unNames . fst . bindeeLore) $ patternBindees pat))
                 (checkPatSizes (patternIdents pat) >> m)),
          pat)

matchExtPattern :: SrcLoc -> [Ident] -> [ExtType] -> TypeM lore ()
matchExtPattern loc pat ts = do
  (ts', restpat, _) <- patternContext loc pat ts
  unless (length restpat == length ts') $
    bad $ InvalidPatternError (Several pat) (Several $ map toDecl ts) loc
  evalStateT (zipWithM_ checkBinding' restpat ts') []
  where checkBinding' (Ident name namet vloc) t = do
          t' <- lift $
                checkAnnotation vloc
                ("binding of variable " ++ textual name) namet t
          add $ Ident name t' vloc
          return $ Ident name t' vloc

        add ident = do
          bnd <- gets $ find (==ident)
          case bnd of
            Nothing -> modify (ident:)
            Just (Ident name _ pos2) ->
              lift $ bad $ DupPatternError name (srclocOf ident) pos2

patternContext :: SrcLoc -> [Ident] -> [ExtType] ->
                  TypeM lore ([Type], [Ident], [Ident])
patternContext loc pat rt = do
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
              lift $ bad $ TypeError loc "Pattern cannot match context"

checkPatSizes :: Checkable lore =>
                 [IdentBase Shape] -> TypeM lore ()
checkPatSizes = mapM_ checkBndSizes

checkBndSizes :: Checkable lore =>
                 IdentBase Shape -> TypeM lore ()
checkBndSizes (Ident _ t _) = do
  let dims = arrayDims t
  mapM_ (require [Basic Int] <=< checkSubExp) dims

validApply :: [DeclType] -> [Type] -> Bool
validApply expected got =
  length got == length expected &&
  and (zipWith subtypeOf (map toDecl got) expected)

type Arg = (Type, Names, Dataflow, SrcLoc)

argType :: Arg -> Type
argType (t, _, _, _) = t

checkArg :: Checkable lore =>
            SubExp -> TypeM lore (SubExp, Arg)
checkArg arg = do (arg', dflow) <- collectDataflow $ checkSubExp arg
                  als <- subExpAliasesM arg
                  return (arg', (subExpType arg', als, dflow, srclocOf arg'))

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
               Lambda lore -> [Arg] -> TypeM lore (Lambda lore)
checkLambda (Lambda params body ret loc) args = do
  ret' <- mapM checkType ret
  if length params == length args then do
    let setParamShape param shape =
          param { identType = identType param `setArrayDims` shape }
        params' = zipWith setParamShape params $
                  map (arrayDims . argType) args
    checkFuncall Nothing loc (map (toDecl . identType) params') args
    (_, body') <-
      noUnique $ checkAnonymousFun
      (nameFromString "<anonymous>", staticResType ret', params', body, loc)
    return $ Lambda params' body' ret' loc
  else bad $ TypeError loc $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

-- | The class of lores that can be type-checked.
class (FreeIn (Lore.Exp lore),
       FreeIn (Lore.LetBound lore),
       FreeIn (Lore.Body lore),
       Lore lore, PrettyLore lore) => Checkable lore where
  checkExpLore :: Lore.Exp lore -> TypeM lore (Lore.Exp lore)
  checkBodyLore :: Lore.Body lore -> TypeM lore (Lore.Body lore)
  checkBindingLore :: Lore.LetBound lore -> TypeM lore ()
  checkFParamLore :: Lore.FParam lore -> TypeM lore ()
  checkResType :: ResType lore -> TypeM lore ()
  matchPattern :: SrcLoc -> Pattern lore -> ResType lore ->
                  TypeM lore ()
