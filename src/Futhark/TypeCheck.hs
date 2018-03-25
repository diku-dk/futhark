{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, ScopedTypeVariables #-}
-- | The type checker checks whether the program is type-consistent.
module Futhark.TypeCheck
  ( -- * Interface
    checkProg
  , TypeError (..)
  , ErrorCase (..)

    -- * Extensionality
  , TypeM
  , bad
  , context
  , message
  , Checkable (..)
  , lookupVar
  , lookupAliases
  , Occurences
  , UsageMap
  , usageMap
  , collectOccurences
  , subCheck

    -- * Checkers
  , require
  , requireI
  , requirePrimExp
  , checkSubExp
  , checkExp
  , checkStms
  , checkStm
  , checkType
  , checkExtType
  , matchExtPattern
  , matchExtReturnType
  , matchExtBranchType
  , argType
  , argAliases
  , noArgAliases
  , checkArg
  , checkSOACArrayArgs
  , checkLambda
  , checkFun'
  , checkLambdaParams
  , checkBody
  , checkLambdaBody
  , consume
  , consumeOnlyParams
  , binding
  )
  where

import Control.Parallel.Strategies
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS.Strict
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Semigroup as Sem

import Futhark.Analysis.PrimExp
import Futhark.Construct (instantiateShapes)
import Futhark.Representation.Aliases
import Futhark.Analysis.Alias
import Futhark.Util
import Futhark.Util.Pretty (Pretty, prettyDoc, indent, ppr, text, (<+>), align)

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data ErrorCase lore =
    TypeError String
  | UnexpectedType (Exp lore) Type [Type]
  | ReturnTypeError Name [ExtType] [ExtType]
  | DupDefinitionError Name
  | DupParamError Name VName
  | DupPatternError VName
  | InvalidPatternError (Pattern (Aliases lore)) [ExtType] (Maybe String)
  | UnknownVariableError VName
  | UnknownFunctionError Name
  | ParameterMismatch (Maybe Name) [Type] [Type]
  | SlicingError Int Int
  | BadAnnotation String Type Type
  | ReturnAliased Name VName
  | UniqueReturnAliased Name
  | NotAnArray VName Type
  | PermutationError [Int] Int (Maybe VName)

instance Checkable lore => Show (ErrorCase lore) where
  show (TypeError msg) =
    "Type error:\n" ++ msg
  show (UnexpectedType e _ []) =
    "Type of expression\n" ++
    prettyDoc 160 (indent 2 $ ppr e) ++
    "\ncannot have any type - possibly a bug in the type checker."
  show (UnexpectedType e t ts) =
    "Type of expression\n" ++
    prettyDoc 160 (indent 2 $ ppr e) ++
    "\nmust be one of " ++ intercalate ", " (map pretty ts) ++ ", but is " ++
    pretty t ++ "."
  show (ReturnTypeError fname rettype bodytype) =
    "Declaration of function " ++ nameToString fname ++
    " declares return type\n  " ++ prettyTuple rettype ++
    "\nBut body has type\n  " ++ prettyTuple bodytype
  show (DupDefinitionError name) =
    "Duplicate definition of function " ++ nameToString name ++ ""
  show (DupParamError funname paramname) =
    "Parameter " ++ pretty paramname ++
    " mentioned multiple times in argument list of function " ++
    nameToString funname ++ "."
  show (DupPatternError name) =
    "Variable " ++ pretty name ++ " bound twice in pattern."
  show (InvalidPatternError pat t desc) =
    "Pattern " ++ pretty pat ++
    " cannot match value of type " ++ prettyTuple t ++ end
    where end = case desc of Nothing -> "."
                             Just desc' -> ":\n" ++ desc'
  show (UnknownVariableError name) =
    "Use of unknown variable " ++ pretty name ++ "."
  show (UnknownFunctionError fname) =
    "Call of unknown function " ++ nameToString fname ++ "."
  show (ParameterMismatch fname expected got) =
    "In call of " ++ fname' ++ ":\n" ++
    "expecting " ++ show nexpected ++ " argument(s) of type(s) " ++
     expected' ++ ", but got " ++ show ngot ++
    " arguments of types " ++ intercalate ", " (map pretty got) ++ "."
    where (nexpected, expected') =
            (length expected, intercalate ", " $ map pretty expected)
          ngot = length got
          fname' = maybe "anonymous function" (("function "++) . nameToString) fname
  show (SlicingError dims got) =
    show got ++ " indices given, but type of indexee has " ++ show dims ++ " dimension(s)."
  show (BadAnnotation desc expected got) =
    "Annotation of \"" ++ desc ++ "\" type of expression is " ++ pretty expected ++
    ", but derived to be " ++ pretty got ++ "."
  show (ReturnAliased fname name) =
    "Unique return value of function " ++ nameToString fname ++
    " is aliased to " ++ pretty name ++ ", which is not consumed."
  show (UniqueReturnAliased fname) =
    "A unique tuple element of return value of function " ++
    nameToString fname ++ " is aliased to some other tuple component."
  show (NotAnArray e t) =
    "The expression " ++ pretty e ++
    " is expected to be an array, but is " ++ pretty t ++ "."
  show (PermutationError perm rank name) =
    "The permutation (" ++ intercalate ", " (map show perm) ++
    ") is not valid for array " ++ name' ++ "of rank " ++ show rank ++ "."
    where name' = maybe "" ((++" ") . pretty) name

-- | A type error.
data TypeError lore = Error [String] (ErrorCase lore)

instance Checkable lore => Show (TypeError lore) where
  show (Error [] err) =
    show err
  show (Error msgs err) =
    intercalate "\n" msgs ++ "\n" ++ show err

-- | A tuple of a return type and a list of parameters, possibly
-- named.
type FunBinding lore = ([RetType (Aliases lore)], [FParam (Aliases lore)])

type VarBinding lore = NameInfo (Aliases lore)

data Usage = Consumed
           | Observed
             deriving (Eq, Ord, Show)

data Occurence = Occurence { observed :: Names
                           , consumed :: Names
                           }
             deriving (Eq, Show)

observation :: Names -> Occurence
observation = flip Occurence S.empty

consumption :: Names -> Occurence
consumption = Occurence S.empty

nullOccurence :: Occurence -> Bool
nullOccurence occ = S.null (observed occ) && S.null (consumed occ)

type Occurences = [Occurence]

type UsageMap = M.Map VName [Usage]

usageMap :: Occurences -> UsageMap
usageMap = foldl comb M.empty
  where comb m (Occurence obs cons) =
          let m' = S.foldl' (ins Observed) m obs
          in S.foldl' (ins Consumed) m' cons
        ins v m k = M.insertWith (++) k [v] m

allConsumed :: Occurences -> Names
allConsumed = S.unions . map consumed

seqOccurences :: Occurences -> Occurences -> Occurences
seqOccurences occurs1 occurs2 =
  filter (not . nullOccurence) (map filt occurs1) ++ occurs2
  where filt occ =
          occ { observed = observed occ `S.difference` postcons }
        postcons = allConsumed occurs2

altOccurences :: Occurences -> Occurences -> Occurences
altOccurences occurs1 occurs2 =
  filter (not . nullOccurence) (map filt occurs1) ++ occurs2
  where filt occ =
          occ { consumed = consumed occ `S.difference` postcons
              , observed = observed occ `S.difference` postcons }
        postcons = allConsumed occurs2

unOccur :: Names -> Occurences -> Occurences
unOccur to_be_removed = filter (not . nullOccurence) . map unOccur'
  where unOccur' occ =
          occ { observed = observed occ `S.difference` to_be_removed
              , consumed = consumed occ `S.difference` to_be_removed
              }

-- | The 'Consumption' data structure is used to keep track of which
-- variables have been consumed, as well as whether a violation has been detected.
data Consumption = ConsumptionError String
                 | Consumption Occurences
                 deriving (Show)

instance Sem.Semigroup Consumption where
  ConsumptionError e <> _ = ConsumptionError e
  _ <> ConsumptionError e = ConsumptionError e
  Consumption o1 <> Consumption o2
    | v:_ <- S.toList $ consumed_in_o1 `S.intersection` used_in_o2 =
        ConsumptionError $ "Variable " <> pretty v <> " referenced after being consumed."
    | otherwise =
        Consumption $ o1 `seqOccurences` o2
    where consumed_in_o1 = mconcat $ map consumed o1
          used_in_o2 = mconcat $ map consumed o2 <> map observed o2

instance Monoid Consumption where
  mempty = Consumption mempty
  mappend = (Sem.<>)

-- | The environment contains a variable table and a function table.
-- Type checking happens with access to this environment.  The
-- function table is only initialised at the very beginning, but the
-- variable table will be extended during type-checking when
-- let-expressions are encountered.
data Env lore =
  Env { envVtable :: M.Map VName (VarBinding lore)
      , envFtable :: M.Map Name (FunBinding lore)
      , envContext :: [String]
      }

-- | The type checker runs in this monad.
newtype TypeM lore a = TypeM (RWST
                              (Env lore)  -- Reader
                              Consumption -- Writer
                              Names       -- State
                              (Either (TypeError lore)) -- Inner monad
                              a)
  deriving (Monad, Functor, Applicative,
            MonadReader (Env lore),
            MonadWriter Consumption,
            MonadState Names)

instance Checkable lore =>
         HasScope (Aliases lore) (TypeM lore) where
  lookupType = fmap typeOf . lookupVar
  askScope = asks $ M.fromList . mapMaybe varType . M.toList . envVtable
    where varType (name, attr) = Just (name, attr)

runTypeM :: Env lore -> TypeM lore a
         -> Either (TypeError lore) (a, Consumption)
runTypeM env (TypeM m) = evalRWST m env mempty

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

message :: Pretty a =>
           String -> a -> String
message s x = prettyDoc 80 $
              text s <+> align (ppr x)

-- | Mark a name as bound.  If the name has been bound previously in
-- the program, report a type error.
bound :: VName -> TypeM lore ()
bound name = do already_seen <- gets $ S.member name
                when already_seen $
                  bad $ TypeError $ "Name " ++ pretty name ++ " bound twice"
                modify $ S.insert name

occur :: Occurences -> TypeM lore ()
occur = tell . Consumption . filter (not . nullOccurence)

-- | Proclaim that we have made read-only use of the given variable.
-- No-op unless the variable is array-typed.
observe :: Checkable lore =>
           VName -> TypeM lore ()
observe name = do
  attr <- lookupVar name
  unless (primType $ typeOf attr) $
    occur [observation $ S.insert name $ aliases attr]

-- | Proclaim that we have written to the given variable.
consume :: Names -> TypeM lore ()
consume als = occur [consumption als]

collectOccurences :: TypeM lore a -> TypeM lore (a, Occurences)
collectOccurences m = pass $ do
  (x, c) <- listen m
  o <- checkConsumption c
  return ((x, o), const mempty)

checkConsumption :: Consumption -> TypeM lore Occurences
checkConsumption (ConsumptionError e) = bad $ TypeError e
checkConsumption (Consumption os)     = return os

alternative :: TypeM lore a -> TypeM lore b -> TypeM lore (a,b)
alternative m1 m2 = pass $ do
  (x, c1) <- listen m1
  (y, c2) <- listen m2
  os1 <- checkConsumption c1
  os2 <- checkConsumption c2
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
  where inspect o = do
          new_consumed <- mconcat <$> mapM wasConsumed (S.toList $ consumed o)
          return o { consumed = new_consumed }
        wasConsumed v
          | Just als <- lookup v consumable = return als
          | otherwise =
            bad $ TypeError $
            unlines [pretty v ++ " was invalidly consumed.",
                     what ++ " can be consumed here."]
        what | null consumable = "Nothing"
             | otherwise = "Only " ++ intercalate ", " (map (pretty . fst) consumable)

-- | Given the immediate aliases, compute the full transitive alias
-- set (including the immediate aliases).
expandAliases :: Names -> Env lore -> Names
expandAliases names env = names `S.union` aliasesOfAliases
  where aliasesOfAliases =  mconcat . map look . S.toList $ names
        look k = case M.lookup k $ envVtable env of
          Just (LetInfo (als, _)) -> unNames als
          _                       -> mempty

binding :: Scope (Aliases lore)
        -> TypeM lore a
        -> TypeM lore a
binding bnds = check . local (`bindVars` bnds)
  where bindVars = M.foldlWithKey' bindVar
        boundnames = M.keys bnds
        boundnameset = S.fromList boundnames

        bindVar env name (LetInfo (Names' als, attr)) =
          let als' = expandAliases als env
              inedges = S.toList als'
              update (LetInfo (Names' thesenames, thisattr)) =
                LetInfo (Names' $ S.insert name thesenames, thisattr)
              update b = b
          in env { envVtable =
                      M.insert name (LetInfo (Names' als', attr)) $
                      adjustSeveral update inedges $
                      envVtable env
                 }
        bindVar env name attr =
          env { envVtable = M.insert name attr $ envVtable env }

        adjustSeveral f = flip $ foldl $ flip $ M.adjust f

        -- Check whether the bound variables have been used correctly
        -- within their scope.
        check m = do
          mapM_ bound $ M.keys bnds
          (a, os) <- collectOccurences m
          tell $ Consumption $ unOccur boundnameset os
          return a

lookupVar :: VName -> TypeM lore (NameInfo (Aliases lore))
lookupVar name = do
  bnd <- asks $ M.lookup name . envVtable
  case bnd of
    Nothing -> bad $ UnknownVariableError name
    Just attr -> return attr

lookupAliases :: VName -> TypeM lore Names
lookupAliases name = do
  als <- aliases <$> lookupVar name
  return $ S.insert name als

aliases :: NameInfo (Aliases lore) -> Names
aliases (LetInfo (als, _)) = unNames als
aliases _ = mempty

subExpAliasesM :: SubExp -> TypeM lore Names
subExpAliasesM Constant{} = return mempty
subExpAliasesM (Var v)    = lookupAliases v

lookupFun :: Checkable lore =>
             Name
          -> [SubExp]
          -> TypeM lore ([RetType lore], [DeclType])
lookupFun fname args = do
  bnd <- asks $ M.lookup fname . envFtable
  case bnd of
    Nothing -> bad $ UnknownFunctionError fname
    Just (ftype, params) -> do
      argts <- mapM subExpType args
      case applyRetType ftype params $ zip args argts of
        Nothing ->
          bad $ ParameterMismatch (Just fname) (map paramType params) argts
        Just rt ->
          return (rt, map paramDeclType params)

-- | @checkAnnotation loc s t1 t2@ checks if @t2@ is equal to
-- @t1@.  If not, a 'BadAnnotation' is raised.
checkAnnotation :: String -> Type -> Type
                -> TypeM lore ()
checkAnnotation desc t1 t2
  | t2 == t1 = return ()
  | otherwise = bad $ BadAnnotation desc t1 t2

-- | @require ts se@ causes a '(TypeError vn)' if the type of @se@ is
-- not a subtype of one of the types in @ts@.
require :: Checkable lore => [Type] -> SubExp -> TypeM lore ()
require ts se = do
  t <- checkSubExp se
  unless (t `elem` ts) $
    bad $ UnexpectedType (BasicOp $ SubExp se) t ts

-- | Variant of 'require' working on variable names.
requireI :: Checkable lore => [Type] -> VName -> TypeM lore ()
requireI ts ident = require ts $ Var ident

checkArrIdent :: Checkable lore =>
                 VName -> TypeM lore Type
checkArrIdent v = do
  t <- lookupType v
  case t of
    Array{} -> return t
    _       -> bad $ NotAnArray v t

-- | Type check a program containing arbitrary type information,
-- yielding either a type error or a program with complete type
-- information.
checkProg :: Checkable lore =>
             Prog lore -> Either (TypeError lore) ()
checkProg prog = do
  let typeenv = Env { envVtable = M.empty
                    , envFtable = mempty
                    , envContext = []
                    }
  let onFunction ftable fun =
        fmap fst $ runTypeM typeenv $
        local (\env -> env { envFtable = ftable }) $
        checkFun fun
  (ftable, _) <- runTypeM typeenv buildFtable
  sequence_ $ parMap rpar (onFunction ftable) $ progFunctions prog'
  where
    prog' = aliasAnalysis prog
    buildFtable = do table <- initialFtable prog'
                     foldM expand table $ progFunctions prog'
    expand ftable (FunDef _ name ret params _)
      | M.member name ftable =
          bad $ DupDefinitionError name
      | otherwise =
          return $ M.insert name (ret,params) ftable

-- The prog argument is just to disambiguate the lore.
initialFtable :: Checkable lore =>
                 Prog (Aliases lore) -> TypeM lore (M.Map Name (FunBinding lore))
initialFtable _ = fmap M.fromList $ mapM addBuiltin $ M.toList builtInFunctions
  where addBuiltin (fname, (t, ts)) = do
          ps <- mapM (primFParam name) ts
          return (fname, ([primRetType t], ps))
        name = VName (nameFromString "x") 0

checkFun :: Checkable lore =>
            FunDef (Aliases lore) -> TypeM lore ()
checkFun (FunDef _ fname rettype params body) =
  context ("In function " ++ nameToString fname) $
    checkFun' (fname,
               retTypeValues rettype,
               funParamsToNameInfos params,
               body) consumable $ do
      checkFunParams params
      checkRetType rettype
      checkFunBody rettype body
        where consumable = [ (paramName param, mempty)
                           | param <- params
                           , unique $ paramDeclType param
                           ]

funParamsToNameInfos :: [FParam lore]
                     -> [(VName, NameInfo (Aliases lore))]
funParamsToNameInfos = map nameTypeAndLore
  where nameTypeAndLore fparam = (paramName fparam,
                                  FParamInfo $ paramAttr fparam)

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

checkFun' :: Checkable lore =>
             (Name,
              [DeclExtType],
              [(VName, NameInfo (Aliases lore))],
              BodyT (Aliases lore))
          -> [(VName, Names)]
          -> TypeM lore ()
          -> TypeM lore ()
checkFun' (fname, rettype, params, body) consumable check = do
  checkNoDuplicateParams
  binding (M.fromList params) $
    consumeOnlyParams consumable $ do
      check
      checkReturnAlias $ bodyAliases body
  where param_names = map fst params

        checkNoDuplicateParams = foldM_ expand [] param_names

        expand seen pname
          | Just _ <- find (==pname) seen =
            bad $ DupParamError fname pname
          | otherwise =
            return $ pname : seen

        -- | Check that unique return values do not alias a
        -- non-consumed parameter.
        checkReturnAlias =
          foldM_ checkReturnAlias' S.empty . returnAliasing rettype

        checkReturnAlias' seen (Unique, names)
          | any (`S.member` S.map snd seen) $ S.toList names =
            bad $ UniqueReturnAliased fname
          | otherwise = do
            consume names
            return $ seen `S.union` tag Unique names
        checkReturnAlias' seen (Nonunique, names)
          | any (`S.member` seen) $ S.toList $ tag Unique names =
            bad $ UniqueReturnAliased fname
          | otherwise = return $ seen `S.union` tag Nonunique names

        tag u = S.map $ \name -> (u, name)

        returnAliasing expected got =
          reverse $
          zip (reverse (map uniqueness expected) ++ repeat Nonunique) $
          reverse got

subCheck :: forall lore newlore a.
            (Checkable newlore,
             RetType lore ~ RetType newlore,
             LetAttr lore ~ LetAttr newlore,
             FParamAttr lore ~ FParamAttr newlore,
             LParamAttr lore ~ LParamAttr newlore) =>
            TypeM newlore a ->
            TypeM lore a
subCheck m = do
  typeenv <- newEnv <$> ask
  case runTypeM typeenv m of
    Left err -> bad $ TypeError $ show err
    Right (x, cons) -> tell cons >> return x
    where newEnv :: Env lore -> Env newlore
          newEnv (Env vtable ftable ctx) =
            Env (M.map coerceVar vtable) ftable ctx
          coerceVar (LetInfo x) = LetInfo x
          coerceVar (FParamInfo x) = FParamInfo x
          coerceVar (LParamInfo x) = LParamInfo x
          coerceVar (IndexInfo it) = IndexInfo it

checkSubExp :: Checkable lore => SubExp -> TypeM lore Type
checkSubExp (Constant val) =
  return $ Prim $ primValueType val
checkSubExp (Var ident) = context ("In subexp " ++ pretty ident) $ do
  observe ident
  lookupType ident

checkStms :: Checkable lore =>
             Stms (Aliases lore) -> TypeM lore a
          -> TypeM lore a
checkStms origbnds m = delve $ stmsToList origbnds
  where delve (stm@(Let pat _ e):bnds) = do
          context ("In expression of statement " ++ pretty pat) $
            checkExp e
          checkStm stm $
            delve bnds
        delve [] =
          m

checkResult :: Checkable lore =>
               Result -> TypeM lore ()
checkResult = mapM_ checkSubExp

checkFunBody :: Checkable lore =>
                [RetType lore]
             -> Body (Aliases lore)
             -> TypeM lore ()
checkFunBody rt (Body (_,lore) bnds res) = do
  checkStms bnds $ do
    context "When checking body result" $ checkResult res
    context "When matching declared return type to result of body" $
      matchReturnType rt res
  checkBodyLore lore

checkLambdaBody :: Checkable lore =>
                   [Type] -> Body (Aliases lore) -> TypeM lore ()
checkLambdaBody ret (Body (_,lore) bnds res) = do
  checkStms bnds $ checkLambdaResult ret res
  checkBodyLore lore

checkLambdaResult :: Checkable lore =>
                     [Type] -> Result -> TypeM lore ()
checkLambdaResult ts es
  | length ts /= length es =
    bad $ TypeError $
    "Lambda has return type " ++ prettyTuple ts ++
    " describing " ++ show (length ts) ++ " values, but body returns " ++
    show (length es) ++ " values: " ++ prettyTuple es
  | otherwise = forM_ (zip ts es) $ \(t, e) -> do
      et <- checkSubExp e
      unless (et == t) $
        bad $ TypeError $
        "Subexpression " ++ pretty e ++ " has type " ++ pretty et ++
        " but expected " ++ pretty t

checkBody :: Checkable lore =>
             Body (Aliases lore) -> TypeM lore ()
checkBody (Body (_,lore) bnds res) = do
  checkStms bnds $ checkResult res
  checkBodyLore lore

checkBasicOp :: Checkable lore =>
               BasicOp (Aliases lore) -> TypeM lore ()

checkBasicOp (SubExp es) =
  void $ checkSubExp es

checkBasicOp (Opaque es) =
  void $ checkSubExp es

checkBasicOp (ArrayLit [] _) =
  return ()

checkBasicOp (ArrayLit (e:es') t) = do
  let check elemt eleme = do
        elemet <- checkSubExp eleme
        unless (elemet == elemt) $
          bad $ TypeError $ pretty elemet ++
          " is not of expected type " ++ pretty elemt ++ "."
  et <- checkSubExp e

  -- Compare that type with the one given for the array literal.
  checkAnnotation "array-element" t et

  mapM_ (check et) es'

checkBasicOp (UnOp op e) = require [Prim $ unOpType op] e

checkBasicOp (BinOp op e1 e2) = checkBinOpArgs (binOpType op) e1 e2

checkBasicOp (CmpOp op e1 e2) = checkCmpOp op e1 e2

checkBasicOp (ConvOp op e) = require [Prim $ fst $ convOpType op] e

checkBasicOp (Index ident idxes) = do
  vt <- lookupType ident
  observe ident
  when (arrayRank vt /= length idxes) $
    bad $ SlicingError (arrayRank vt) (length idxes)
  mapM_ checkDimIndex idxes

checkBasicOp (Update src idxes se) = do
  src_t <- checkArrIdent src
  when (arrayRank src_t /= length idxes) $
    bad $ SlicingError (arrayRank src_t) (length idxes)

  mapM_ checkDimIndex idxes
  require [Prim (elemType src_t) `arrayOfShape` Shape (sliceDims idxes)] se
  consume =<< lookupAliases src

checkBasicOp (Iota e x s et) = do
  require [Prim int32] e
  require [Prim $ IntType et] x
  require [Prim $ IntType et] s

checkBasicOp (Replicate (Shape dims) valexp) = do
  mapM_ (require [Prim int32]) dims
  void $ checkSubExp valexp

checkBasicOp (Repeat shapes innershape v) = do
  v_t <- lookupType v
  mapM_ (mapM_ (require [Prim int32]) . shapeDims) $ innershape : shapes
  unless (length shapes == arrayRank v_t) $
    bad $ TypeError "Incorrect number of shapes in repeat."

checkBasicOp (Scratch _ shape) =
  mapM_ checkSubExp shape

checkBasicOp (Reshape newshape arrexp) = do
  rank <- arrayRank <$> checkArrIdent arrexp
  mapM_ (require [Prim int32] . newDim) newshape
  zipWithM_ (checkDimChange rank) newshape [0..]
  where checkDimChange _ (DimNew _) _ =
          return ()
        checkDimChange rank (DimCoercion se) i
          | i >= rank =
            bad $ TypeError $
            "Asked to coerce dimension " ++ show i ++ " to " ++ pretty se ++
            ", but array " ++ pretty arrexp ++ " has only " ++ pretty rank ++ " dimensions"
          | otherwise =
            return ()

checkBasicOp (Rearrange perm arr) = do
  arrt <- lookupType arr
  let rank = arrayRank arrt
  when (length perm /= rank || sort perm /= [0..rank-1]) $
    bad $ PermutationError perm rank $ Just arr

checkBasicOp (Rotate rots arr) = do
  arrt <- lookupType arr
  let rank = arrayRank arrt
  when (length rots /= rank) $
    bad $ TypeError $ "Cannot rotate " ++ show (length rots) ++
    " dimensions of " ++ show rank ++ "-dimensional array."

checkBasicOp (Concat i arr1exp arr2exps ressize) = do
  arr1t  <- checkArrIdent arr1exp
  arr2ts <- mapM checkArrIdent arr2exps
  let success = all (== (dropAt i 1 $ arrayDims arr1t)) $
                map (dropAt i 1 . arrayDims) arr2ts
  unless success $
    bad $ TypeError $
    "Types of arguments to concat do not match.  Got " ++
    pretty arr1t ++ " and " ++ intercalate ", " (map pretty arr2ts)
  require [Prim int32] ressize

checkBasicOp (Copy e) =
  void $ checkArrIdent e

checkBasicOp (Manifest perm arr) =
  checkBasicOp $ Rearrange perm arr -- Basically same thing!

checkBasicOp (Assert e _ _) =
  require [Prim Bool] e

checkBasicOp (Partition _ flags arrs) = do
  flagst <- lookupType flags
  unless (rowType flagst == Prim int32) $
    bad $ TypeError $ "Flag array has type " ++ pretty flagst ++ "."
  forM_ arrs $ \arr -> do
    arrt <- lookupType arr
    unless (arrayRank arrt > 0) $
      bad $ TypeError $
      "Array argument " ++ pretty arr ++
      " to partition has type " ++ pretty arrt ++ "."

checkExp :: Checkable lore =>
            Exp (Aliases lore) -> TypeM lore ()

checkExp (BasicOp op) = checkBasicOp op

checkExp (If e1 e2 e3 info) = do
  require [Prim Bool] e1
  _ <- checkBody e2 `alternative` checkBody e3
  context "in true branch" $ matchBranchType (ifReturns info) e2
  context "in false branch" $ matchBranchType (ifReturns info) e3

checkExp (Apply fname args rettype_annot _) = do
  (rettype_derived, paramtypes) <- lookupFun fname $ map fst args
  argflows <- mapM (checkArg . fst) args
  when (rettype_derived /= rettype_annot) $
    bad $ TypeError $ "Expected apply result type " ++ pretty rettype_derived
    ++ " but annotation is " ++ pretty rettype_annot
  checkFuncall (Just fname) paramtypes argflows

checkExp (DoLoop ctxmerge valmerge form loopbody) = do
  let merge = ctxmerge ++ valmerge
      (mergepat, mergeexps) = unzip merge
  mergeargs <- mapM checkArg mergeexps

  binding (scopeOf form) $ do
    case form of
      ForLoop loopvar it boundexp loopvars -> do
        iparam <- primFParam loopvar $ IntType it
        let funparams = iparam : mergepat
            paramts   = map paramDeclType funparams

        forM_ loopvars $ \(p,a) -> do
          a_t <- lookupType a
          observe a
          case peelArray 1 a_t of
            Just a_t_r -> do
              checkLParamLore (paramName p) $ paramAttr p
              unless (a_t_r `subtypeOf` typeOf (paramAttr p)) $
                 bad $ TypeError $ "Loop parameter " ++ pretty p ++
                 " not valid for element of " ++ pretty a ++ ", which has row type " ++ pretty a_t_r
              return ()
            _ -> bad $ TypeError $ "Cannot loop over " ++ pretty a ++
                 " of type " ++ pretty a_t

        boundarg <- checkArg boundexp
        checkFuncall Nothing paramts $ boundarg : mergeargs

      WhileLoop cond -> do
        case find ((==cond) . paramName . fst) merge of
          Just (condparam,_) ->
            unless (paramType condparam == Prim Bool) $
            bad $ TypeError $
            "Conditional '" ++ pretty cond ++ "' of while-loop is not boolean, but " ++
            pretty (paramType condparam) ++ "."
          Nothing ->
            bad $ TypeError $
            "Conditional '" ++ pretty cond ++ "' of while-loop is not a merge varible."
        let funparams = mergepat
            paramts   = map paramDeclType funparams
        checkFuncall Nothing paramts mergeargs

    let rettype = map paramDeclType mergepat
        consumable = [ (paramName param, mempty)
                     | param <- mergepat,
                       unique $ paramDeclType param
                     ]

    context "Inside the loop body" $
      checkFun' (nameFromString "<loop body>",
                 staticShapes rettype,
                 funParamsToNameInfos mergepat,
                 loopbody) consumable $ do
          checkFunParams mergepat
          checkBody loopbody
          bodyt <- map (`toDecl` Unique) <$> bodyExtType loopbody
          unless (map rankShaped bodyt `subtypesOf`
                  map rankShaped (staticShapes rettype)) $
            bad $ ReturnTypeError (nameFromString "<loop body>")
            (map fromDecl $ staticShapes rettype)
            (map fromDecl bodyt)

checkExp (Op op) = checkOp op

checkSOACArrayArgs :: Checkable lore =>
                      SubExp -> [VName] -> TypeM lore [Arg]
checkSOACArrayArgs width vs =
  forM vs $ \v -> do
    (vt, v') <- checkSOACArrayArg v
    let argSize = arraySize 0 vt
    unless (argSize == width) $
      bad $ TypeError $
      "SOAC argument " ++ pretty v ++ " has outer size " ++
      pretty argSize ++ ", but width of SOAC is " ++
      pretty width
    return v'
  where checkSOACArrayArg ident = do
          (t, als) <- checkArg $ Var ident
          case peelArray 1 t of
            Nothing -> bad $ TypeError $
                       "SOAC argument " ++ pretty ident ++ " is not an array"
            Just rt -> return (t, (rt, als))

checkType :: Checkable lore =>
             TypeBase Shape u -> TypeM lore ()
checkType = mapM_ checkSubExp . arrayDims

checkExtType :: Checkable lore =>
                TypeBase ExtShape u
             -> TypeM lore ()
checkExtType = mapM_ checkExtDim . shapeDims . arrayShape
  where checkExtDim (Free se) = void $ checkSubExp se
        checkExtDim (Ext _)   = return ()

checkCmpOp :: Checkable lore =>
              CmpOp -> SubExp -> SubExp
           -> TypeM lore ()
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

checkBinOpArgs :: Checkable lore =>
                  PrimType -> SubExp -> SubExp -> TypeM lore ()
checkBinOpArgs t e1 e2 = do
  require [Prim t] e1
  require [Prim t] e2

checkPatElem :: Checkable lore =>
                PatElemT (LetAttr lore) -> TypeM lore ()
checkPatElem (PatElem name attr) = checkLetBoundLore name attr

checkDimIndex :: Checkable lore =>
                 DimIndex SubExp -> TypeM lore ()
checkDimIndex (DimFix i) = require [Prim int32] i
checkDimIndex (DimSlice i n s) = mapM_ (require [Prim int32]) [i,n,s]

checkStm :: Checkable lore =>
            Stm (Aliases lore)
         -> TypeM lore a
         -> TypeM lore a
checkStm stm@(Let pat (StmAux (Certificates cs) (_,attr)) e) m = do
  mapM_ (requireI [Prim Cert]) cs
  checkExpLore attr
  context ("When matching\n" ++ message "  " pat ++ "\nwith\n" ++ message "  " e) $
    matchPattern pat e
  binding (scopeOf stm) $ do
    mapM_ checkPatElem (patternElements $ removePatternAliases pat)
    m

matchExtPattern :: Checkable lore =>
                   Pattern (Aliases lore) -> [ExtType] -> TypeM lore ()
matchExtPattern pat ts =
  unless (expExtTypesFromPattern pat == ts) $
    bad $ InvalidPatternError pat ts Nothing

matchExtReturnType :: Checkable lore =>
                      [ExtType] -> Result -> TypeM lore ()
matchExtReturnType rettype res = do
  ts <- mapM subExpType res
  matchExtReturns rettype res ts

matchExtBranchType :: Checkable lore =>
                      [ExtType] -> Body (Aliases lore) -> TypeM lore ()
matchExtBranchType rettype (Body _ stms res) = do
  ts <- extendedScope (traverse subExpType res) stmscope
  matchExtReturns rettype res ts
  where stmscope = scopeOf stms

matchExtReturns :: Checkable lore =>
                   [ExtType] -> Result -> [Type] -> TypeM lore ()
matchExtReturns rettype res ts = do
  let problem :: TypeM lore a
      problem = bad $ TypeError $ unlines [ "Type annotation is"
                                          , "  " ++ prettyTuple rettype
                                          , "But result returns type"
                                          , "  " ++ prettyTuple ts ]

  let (ctx_res, val_res) = splitFromEnd (length rettype) res
      (ctx_ts, val_ts) = splitFromEnd (length rettype) ts

  unless (length val_res == length rettype) problem

  let ctx_vals = zip ctx_res ctx_ts
      instantiateExt i = case maybeNth i ctx_vals of
                           Just (se, Prim (IntType Int32)) -> return se
                           _ -> problem

  rettype' <- instantiateShapes instantiateExt rettype

  unless (rettype' == val_ts) problem

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

-- | Remove all aliases from the 'Arg'.
argAliases :: Arg -> Names
argAliases (_, als) = als

noArgAliases :: Arg -> Arg
noArgAliases (t, _) = (t, mempty)

checkArg :: Checkable lore =>
            SubExp -> TypeM lore Arg
checkArg arg = do argt <- checkSubExp arg
                  als <- subExpAliasesM arg
                  return (argt, als)

checkFuncall :: Maybe Name
             -> [DeclType] -> [Arg]
             -> TypeM lore ()
checkFuncall fname paramts args = do
  let argts = map argType args
  unless (validApply paramts argts) $
    bad $ ParameterMismatch fname
          (map fromDecl paramts) $
          map argType args
  forM_ (zip (map diet paramts) args) $ \(d, (_, als)) ->
    occur [consumption (consumeArg als d)]
  where consumeArg als Consume = als
        consumeArg _   Observe = mempty

checkLambda :: Checkable lore =>
               Lambda (Aliases lore) -> [Arg] -> TypeM lore ()
checkLambda (Lambda params body rettype) args = do
  let fname = nameFromString "<anonymous>"
  if length params == length args then do
    checkFuncall Nothing
      (map ((`toDecl` Nonunique) . paramType) params) args
    let consumable = zip (map paramName params) (map argAliases args)
    checkFun' (fname,
               staticShapes $ map (`toDecl` Nonunique) rettype,
               [ (paramName param,
                  LParamInfo $ paramAttr param)
               | param <- params ],
               body) consumable $ do
      checkLambdaParams params
      mapM_ checkType rettype
      checkLambdaBody rettype body
  else bad $ TypeError $ "Anonymous function defined with " ++ show (length params) ++ " parameters, but expected to take " ++ show (length args) ++ " arguments."

checkPrimExp :: Checkable lore => PrimExp VName -> TypeM lore ()
checkPrimExp ValueExp{} = return ()
checkPrimExp (LeafExp v pt) = requireI [Prim pt] v
checkPrimExp (BinOpExp op x y) = do requirePrimExp (binOpType op) x
                                    requirePrimExp (binOpType op) y
checkPrimExp (CmpOpExp op x y) = do requirePrimExp (cmpOpType op) x
                                    requirePrimExp (cmpOpType op) y
checkPrimExp (UnOpExp op x) = requirePrimExp (unOpType op) x
checkPrimExp (ConvOpExp op x) = requirePrimExp (fst $ convOpType op) x
checkPrimExp (FunExp h args t) = do
  (h_ts, h_ret, _) <- maybe (bad $ TypeError $ "Unknown function: " ++ h)
                      return $ M.lookup h primFuns
  when (length h_ts /= length args) $
    bad $ TypeError $ "Function expects " ++ show (length h_ts) ++
    " parameters, but given " ++ show (length args) ++ " arguments."
  when (h_ret /= t) $
    bad $ TypeError $ "Function return annotation is " ++ pretty t ++
    ", but expected " ++ pretty h_ret
  zipWithM_ requirePrimExp h_ts args

requirePrimExp :: Checkable lore => PrimType -> PrimExp VName -> TypeM lore ()
requirePrimExp t e = context ("in PrimExp " ++ pretty e) $ do
  checkPrimExp e
  unless (primExpType e == t) $ bad $ TypeError $
    pretty e ++ " must have type " ++ pretty t

-- | The class of lores that can be type-checked.
class (Attributes lore, CanBeAliased (Op lore)) => Checkable lore where
  checkExpLore :: ExpAttr lore -> TypeM lore ()
  checkBodyLore :: BodyAttr lore -> TypeM lore ()
  checkFParamLore :: VName -> FParamAttr lore -> TypeM lore ()
  checkLParamLore :: VName -> LParamAttr lore -> TypeM lore ()
  checkLetBoundLore :: VName -> LetAttr lore -> TypeM lore ()
  checkRetType :: [RetType lore] -> TypeM lore ()
  checkOp :: OpWithAliases (Op lore) -> TypeM lore ()
  matchPattern :: Pattern (Aliases lore) -> Exp (Aliases lore) -> TypeM lore ()
  primFParam :: VName -> PrimType -> TypeM lore (FParam (Aliases lore))
  primLParam :: VName -> PrimType -> TypeM lore (LParam (Aliases lore))
  matchReturnType :: [RetType lore] -> Result -> TypeM lore ()
  matchBranchType :: [BranchType lore] -> Body (Aliases lore) -> TypeM lore ()
