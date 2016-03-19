---- Fully expanded structured types.  These are what the type checker
---- will need to turn 'UserType's into.
import Data.Either
import qualified Data.HashMap.Lazy as HM

-- | Whatever; doesn't matter.
data PrimType = Int | Float
              deriving (Eq, Ord, Show)

-- | Types that can be elements of tuple-arrays.
data TupleArrayElemTypeBase =
    PrimArrayElem PrimType
  | ArrayArrayElem ArrayTypeBase
  | TupleArrayElem [TupleArrayElemTypeBase]
  deriving (Eq, Ord, Show)

-- | An array type.
data ArrayTypeBase =
    PrimArray PrimType Int
    -- ^ An array whose elements are primitive types.
  | TupleArray [TupleArrayElemTypeBase] Int
    -- ^ An array whose elements are tuples.
    deriving (Eq, Ord, Show)

-- | An Futhark type is either an array, a prim type, or a tuple.
data TypeBase = Prim PrimType
              | Array ArrayTypeBase
              | Tuple [TypeBase]
              deriving (Eq, Ord, Show)


---- Non-expanded non-structural types that may include names of other
---- types.  These are what the user actually types, and what the
---- parser produces.

data UserType = UserTuple [UserType]
              | UserArray UserType
              | UserPrim PrimType
              | UserType String -- ^ Type name.

data TypeDecl =
  TypeDecl { declName :: String -- ^ Name of the type we are declaring.
           , declType :: UserType -- ^ Its declaration.
           }

-- ^ Mapping from type name to fully expanded type. 
type TypeEnv = [(String,TypeBase)]

-- | Convert a list of type declarations into a type environment.
-- This can fail if a type declaration references an unknown type, or
-- if there is a circular dependency.
checkTypes :: [TypeDecl] -> Either String TypeEnv
checkTypes declarations =
  let
    baseMap = foldr
                (\(TypeDecl string usertype) map -> HM.insert string usertype map)
                HM.empty
                declarations
  in
    checkTypes2 declarations baseMap []

checkTypes2 :: [TypeDecl] -> HM.HashMap String UserType -> TypeEnv -> Either String TypeEnv
checkTypes2 [] _ typeEnv = Right typeEnv
checkTypes2 ((TypeDecl string someType):rest) hashmap typeEnv =
  case getUserType someType hashmap [string] of
    (Left error) -> Left error
    (Right someType) -> case lookup string typeEnv of
                        Just _ -> Left $ "Error: type " ++ string ++ " already defined."
                        Nothing -> checkTypes2 rest hashmap $ (string, someType):typeEnv


getUserType :: UserType -> HM.HashMap String UserType -> [String] -> Either String TypeBase
getUserType usertype hashmap unknowns =
    case usertype of
      (UserPrim somePrim)   -> Right $ Prim somePrim
      (UserType someTarget) ->  if someTarget `elem` unknowns then Left $ "Error: Cyclical definition of " ++ someTarget
                                else getUserTypeBase someTarget hashmap unknowns
      (UserArray userType)  -> getUserType userType hashmap unknowns
      (UserTuple userTypes) -> getUserTupleBaseTypes userTypes hashmap unknowns

getUserTypeBase :: String -> HM.HashMap String UserType -> [String] -> Either String TypeBase
getUserTypeBase target hashmap unknowns =
--  if target `elem` unknowns then Left $ "Error: Cyclical definition of " ++ target
--  else
    case HM.lookup target hashmap
      of (Just (UserPrim somePrim)) -> Right $ Prim somePrim
         (Just (UserType someTarget)) -> getUserType (UserType someTarget) hashmap $ target : unknowns
         (Just (UserTuple userTypes)) -> getUserTupleBaseTypes userTypes hashmap unknowns
         (Just (UserArray userType)) -> getUserType userType hashmap unknowns
         Nothing -> Left $ "UserType " ++ target ++ " not defined"

getUserTupleBaseTypes :: [UserType] -> HM.HashMap String UserType -> [String] -> Either String TypeBase
getUserTupleBaseTypes declarations hashmap unknowns =
  let
    tuple_base_types = map (\inType -> getUserType inType hashmap unknowns) declarations
    errors = lefts tuple_base_types
    types = rights tuple_base_types
  in
    case errors of
      (error:rest) -> Left error
      []           -> Right $ Tuple types

exampleDecls :: [TypeDecl]
exampleDecls = [ TypeDecl "t0" $ UserPrim Int
               , TypeDecl "t1" $ UserType "t0"
               , TypeDecl "t2" $ UserArray $
                 UserTuple [UserType "t1", UserType "t0", UserPrim Int]
               ]
exampleDeclsResult = checkTypes exampleDecls
-- | checkTypes exampleDecls returns
-- | Right [("t2",Tuple [Prim Int,Prim Int,Prim Float]),
-- |        ("t1",Prim Int),
-- |        ("t0",Prim Int)]


redefinitionOfTypeTest :: [TypeDecl]
redefinitionOfTypeTest = [ TypeDecl "t0" $ UserPrim Int
                         , TypeDecl "t1" $ UserType "t0"
                         , TypeDecl "t0" $ UserPrim Float
                         , TypeDecl "t2" $ UserArray $
                           UserTuple [UserType "t0", UserType "t1", UserPrim Float]
                         ]
redefinitionOfTypeTestResult = checkTypes redefinitionOfTypeTest
-- | checkTypes redefinitionOfTypeTest returns Left "Error: type t0 already defined"


cycleTest :: [TypeDecl]
cycleTest = [ TypeDecl "t0" $ UserPrim Int
            , TypeDecl "t1" $ UserType "t2"
            , TypeDecl "t2" $ UserType "t3"
            , TypeDecl "t3" $ UserArray $ UserType "t1"
            ]
cycleTestResult = checkTypes cycleTest
-- | checkTypes cycleTest returns Left "Error: Cyclical definition of t1"



cycleTest2 :: [TypeDecl]
cycleTest2 = [ TypeDecl "t0" $ UserPrim Int
            , TypeDecl "t1" $ UserType "t2"
            , TypeDecl "t2" $ UserType "t3"
            , TypeDecl "t3" $ UserArray $ UserTuple [UserPrim Float, UserType "t1"]
            ]

cycleTest2Result = checkTypes cycleTest2

-- | checkTypes cycleTest2 returns Left "Error: Cyclical definition of t1"




cycleTest3Result = checkTypes [TypeDecl "a" (UserType "b"), TypeDecl "b" (UserType "c"), TypeDecl "c"  (UserType "c")]
cycleTest4Result = checkTypes [TypeDecl "a" (UserType "b"), TypeDecl "b" (UserType "b")]




-- | Corresponds to the type environment to be produced from
-- exampleDecls.
exampleTypeEnv :: TypeEnv
exampleTypeEnv = [ ("t0", Prim Int)
                 , ("t1", Prim Int)
                 , ("t2", Array $
                    TupleArray [PrimArrayElem Int,
                                PrimArrayElem Int,
                                PrimArrayElem Float]
                    1)
                 ]

tests =  [ exampleDeclsResult -- Right TypeEnv
         , redefinitionOfTypeTestResult -- Left Error
         , cycleTestResult -- Left Error
         , cycleTest2Result -- Left Error
         , cycleTest3Result -- Left Error
         , cycleTest4Result -- Left Error
         ]
