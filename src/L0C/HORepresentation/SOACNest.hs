module L0C.HORepresentation.SOACNest
  ( SOACNest (..)
  , Combinator (..)
  , nesting
  , setNesting
  , body
  , setBody
  , params
  , returnType
  , NestBody (..)
  , bodyParams
  , Nesting (..)
  , bodyToLambda
  , lambdaToBody
  , setInputs
  , certificates
  , fromExp
  , toExp
  , fromSOAC
  , toSOAC
  , letPattern
  , inputBindings
  , inputsPerLevel
  )
  where

import Control.Applicative
import Control.Monad

import Data.Loc
import Data.Maybe
import qualified Data.HashSet as HS

import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.L0 hiding (MapT, ReduceT, ScanT, FilterT, RedomapT, returnType)
import L0C.Substitute

-- Current problems:
--
-- * Some "nests" are detected that really are not.  For example,
-- nested reduces that do not use the same accumulator.  Also look at
-- how they deal with their lambda params.  Also, the inputs to a
-- nested loop should not be used inside the body, or it's not a
-- proper nest!  (I think...)

data Nesting = Nesting {
    nestingParams     :: [Ident]
  , nestingInputs     :: [SOAC.Input]
  , nestingResult     :: [Ident]
  , nestingPostExp    :: Exp
  , nestingReturnType :: [DeclType]
  } deriving (Eq, Ord, Show)

data NestBody = Lambda TupleLambda
              | NewNest Nesting Combinator
                deriving (Show)

instance Substitute NestBody where
  substituteNames m (NewNest n comb) =
    let n'    = substituteNames m
                n { nestingParams = nestingParams n' }
        comb' = substituteNames m comb
    in NewNest n' comb'
  substituteNames m (Lambda l) =
    Lambda l { tupleLambdaBody =
                 substituteNames m $ tupleLambdaBody l
             }

bodyParams :: NestBody -> [Parameter]
bodyParams (Lambda l)       = tupleLambdaParams l
bodyParams (NewNest nest _) = map toParam $ nestingParams nest

bodyToLambda :: NestBody -> TupleLambda
bodyToLambda (Lambda l) = l
bodyToLambda (NewNest (Nesting paramIds inps bndIds postExp retTypes) op) =
  TupleLambda { tupleLambdaSrcLoc = loc
              , tupleLambdaParams = map toParam paramIds
              , tupleLambdaReturnType = retTypes
              , tupleLambdaBody =
                letPattern bndIds
                (SOAC.toExp $ toSOAC $ SOACNest inps op)
                postExp
              }
  where loc = srclocOf op

lambdaToBody :: TupleLambda -> NestBody
lambdaToBody l = fromMaybe (Lambda l) $ liftM (uncurry $ flip NewNest) $ nested l

data Combinator = MapT Certificates NestBody [Nesting] SrcLoc
                | ReduceT Certificates NestBody [Nesting] [Exp] SrcLoc
                | ScanT Certificates NestBody [Nesting] [Exp] SrcLoc
                | FilterT Certificates NestBody [Nesting] SrcLoc
                | RedomapT Certificates TupleLambda NestBody [Nesting] [Exp] SrcLoc
                 deriving (Show)

instance Located Combinator where
  locOf (MapT _ _ _ loc) = locOf loc
  locOf (ReduceT _ _ _ _ loc) = locOf loc
  locOf (ScanT _ _ _ _ loc) = locOf loc
  locOf (FilterT _ _ _ loc) = locOf loc
  locOf (RedomapT _ _ _ _ _ loc) = locOf loc

instance Substitute Combinator where
  substituteNames m comb =
    substituteNames m (body comb) `setBody`
    (map (substituteNames m) (nesting comb) `setNesting` comb)

instance Substitute Nesting where
  substituteNames m n =
    n { nestingInputs = map (substituteNames m) $ nestingInputs n }

nesting :: Combinator -> [Nesting]
nesting (MapT _ _ ls _) = ls
nesting (ReduceT _ _ ls _ _) = ls
nesting (ScanT _ _ ls _ _) = ls
nesting (FilterT _ _ ls _) = ls
nesting (RedomapT _ _ _ ls _ _) = ls

setNesting :: [Nesting] -> Combinator -> Combinator
setNesting ls (MapT cs b _ loc) = MapT cs b ls loc
setNesting ls (ReduceT cs b _ es loc) = ReduceT cs b ls es loc
setNesting ls (ScanT cs b _ es loc) = ScanT cs b ls es loc
setNesting ls (FilterT cs b _ loc) = FilterT cs b ls loc
setNesting ls (RedomapT cs l b _ es loc) = RedomapT cs l b ls es loc

body :: Combinator -> NestBody
body (MapT _ b _ _) = b
body (ReduceT _ b _ _ _) = b
body (ScanT _ b _ _ _) = b
body (FilterT _ b _ _) = b
body (RedomapT _ _ b _ _ _) = b

setBody :: NestBody -> Combinator -> Combinator
setBody b (MapT cs _ ls loc) = MapT cs b ls loc
setBody b (ReduceT cs _ ls es loc) = ReduceT cs b ls es loc
setBody b (ScanT cs _ ls es loc) = ScanT cs b ls es loc
setBody b (FilterT cs _ ls loc) = FilterT cs b ls loc
setBody b (RedomapT cs l _ ls es loc) = RedomapT cs l b ls es loc

combinatorFirstLoop :: Combinator -> ([Parameter], [DeclType])
combinatorFirstLoop comb =
  case nesting comb of
      nest:_ -> (map toParam $ nestingParams nest,
                 nestingReturnType nest)
      []     -> case body comb of
                  Lambda l       -> (tupleLambdaParams l,
                                     tupleLambdaReturnType l)
                  NewNest nest _ -> (map toParam $ nestingParams nest,
                                    nestingReturnType nest)

params :: Combinator -> [Parameter]
params = fst . combinatorFirstLoop

returnType :: Combinator -> [DeclType]
returnType = snd . combinatorFirstLoop

data SOACNest = SOACNest { inputs :: [SOAC.Input]
                         , operation :: Combinator
                         }
                deriving (Show)

instance Located SOACNest where
  locOf = locOf . operation

setInputs :: [SOAC.Input] -> SOACNest -> SOACNest
setInputs arrs nest = nest { inputs = arrs }

-- | Returns the certificates used in a SOACNest.
certificates :: SOACNest -> Certificates
certificates (SOACNest _ (MapT     cs _     _ _)) = cs
certificates (SOACNest _ (ReduceT  cs _   _ _ _)) = cs
certificates (SOACNest _ (ScanT    cs _   _ _ _)) = cs
certificates (SOACNest _ (FilterT  cs _   _   _)) = cs
certificates (SOACNest _ (RedomapT cs _ _ _ _ _)) = cs

fromExp :: Exp -> Either SOAC.NotSOAC SOACNest
fromExp = liftM fromSOAC . SOAC.fromExp

toExp :: SOACNest -> Exp
toExp = SOAC.toExp . toSOAC

fromSOAC :: SOAC -> SOACNest
fromSOAC (SOAC.MapT cs l as loc)
  | Just (MapT cs2 l2 ps _, nest) <- nested l =
      SOACNest as $ MapT (cs++cs2) l2 (nest:ps) loc
  | otherwise =
      SOACNest as $ MapT cs (lambdaToBody l) [] loc
fromSOAC (SOAC.ReduceT cs l args loc)
  | Just (ReduceT cs2 l2 ps _ _, nest) <- nested l =
      SOACNest (map snd args) $
      ReduceT (cs++cs2) l2 (nest:ps) (map fst args) loc
  | otherwise =
      SOACNest (map snd args) $
      ReduceT cs (lambdaToBody l) [] (map fst args) loc
fromSOAC (SOAC.ScanT cs l args loc)
  | Just (ScanT cs2 l2 ps _ _, nest) <- nested l =
      SOACNest (map snd args) $
      ScanT (cs++cs2) l2 (nest:ps) (map fst args) loc
  | otherwise =
      SOACNest (map snd args) $
      ScanT cs (lambdaToBody l) [] (map fst args) loc
fromSOAC (SOAC.FilterT cs l as loc)
  | Just (FilterT cs2 l2 ps  _, nest) <- nested l =
      SOACNest as $ FilterT (cs++cs2) l2 (nest:ps) loc
  | otherwise =
      SOACNest as $ FilterT cs (lambdaToBody l) [] loc
fromSOAC (SOAC.RedomapT cs ol l es as loc) =
  -- Never nested, because we need a way to test alpha-equivalence of
  -- the outer combining function.
  SOACNest as $ RedomapT cs ol (lambdaToBody l) [] es loc

nested :: TupleLambda -> Maybe (Combinator, Nesting)
nested l
  | LetPat pat e pe _ <- tupleLambdaBody l, -- Is a let-binding...
    Right soac <- fromSOAC <$> SOAC.fromExp e, -- ...the bindee is a SOAC...
    Just ids   <- tupId pat, -- ...with a tuple-pattern...
    Just pe' <-
      checkPostExp (map fromParam $ tupleLambdaParams l) pe = -- ...where the body is a tuple
                                                              -- literal of the bound variables.
      Just (operation soac,
            -- ... FIXME: need more checks here.
            Nesting { nestingParams = map fromParam $ tupleLambdaParams l
                    , nestingInputs = inputs soac
                    , nestingResult = ids
                    , nestingPostExp = pe'
                    , nestingReturnType = tupleLambdaReturnType l
                    })
  | otherwise = Nothing

tupId :: TupIdent -> Maybe [Ident]
tupId (TupId pats _) = mapM tupId' pats
  where tupId' (Id ident) = Just ident
        tupId' _          = Nothing
tupId _ = Nothing

checkPostExp :: [Ident] -> Exp -> Maybe Exp
checkPostExp ks e
  | HS.null $ HS.fromList ks `HS.intersection` freeInExp e = Just e
  | otherwise                                              = Nothing

toSOAC :: SOACNest -> SOAC
toSOAC (SOACNest as comb@(MapT cs b _ loc)) =
  SOAC.MapT cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(ReduceT cs b _ es loc)) =
  SOAC.ReduceT cs (subLambda b comb) (zip es as) loc
toSOAC (SOACNest as comb@(ScanT cs b _ es loc)) =
  SOAC.ScanT cs (subLambda b comb) (zip es as) loc
toSOAC (SOACNest as comb@(FilterT cs b _ loc)) =
  SOAC.FilterT cs (subLambda b comb) as loc
toSOAC (SOACNest as comb@(RedomapT cs l b _ es loc)) =
  SOAC.RedomapT cs l (subLambda b comb) es as loc

subLambda :: NestBody -> Combinator -> TupleLambda
subLambda b comb =
  case nesting comb of
    [] -> bodyToLambda b
    (Nesting paramIds inps bndIds postExp retTypes:rest) ->
      TupleLambda { tupleLambdaReturnType = retTypes
                  , tupleLambdaBody       =
                    letPattern bndIds
                    (SOAC.toExp $ toSOAC $ SOACNest inps (rest `setNesting` comb))
                    postExp
                  , tupleLambdaSrcLoc     = loc
                  , tupleLambdaParams     = map toParam paramIds
                  }
  where loc = srclocOf comb

letPattern :: [Ident] -> Exp -> Exp -> Exp
letPattern bndIds e postExp =
  LetPat (TupId (map Id bndIds) loc) e postExp loc
  where loc = srclocOf e

inputBindings :: SOACNest -> [[Ident]]
inputBindings outernest =
  inputBindings' ps comb
  where ps  = [ inpArr >> Just param
                | (inpArr,param) <- zip inpArrs $ nextInputParams comb]
        inpArrs = map SOAC.inputArray $ inputs outernest
        comb    = operation outernest

inputBindings' :: [Maybe Ident] -> Combinator -> [[Ident]]
inputBindings' ps comb =
  zipWith (++) (map maybeToList ps) $
  case nesting comb of
    [] ->
      case body comb of
        Lambda _              ->
          replicate (length ps) []
        NewNest nest comb' ->
          inputBindings' (usedParams ps nest comb') comb'
    nest:nests ->
      let comb' = nests `setNesting` comb
      in inputBindings' (usedParams ps nest comb') comb'

usedParams :: [Maybe Ident] -> Nesting -> Combinator -> [Maybe Ident]
usedParams ps nest comb = nestingInputParam ps nest $ nextInputParams comb

nextInputParams :: Combinator -> [Ident]
nextInputParams comb =
  case nesting comb of
    []        -> case body comb of
                   Lambda l       -> map fromParam $ tupleLambdaParams l -- FIXME: remove accumulator params!
                   NewNest nest _ -> nestingParams nest
    nest:_ -> nestingParams nest

nestingInputParam :: [Maybe Ident] -> Nesting -> [Ident] -> [Maybe Ident]
nestingInputParam ps nest nextparams =
  map ((`lookupParamIn` zip inps nextparams)=<<) ps
  where inps = nestingInputs nest
        lookupParamIn = lookup . SOAC.varInput

inputsPerLevel :: SOACNest -> [[SOAC.Input]]
inputsPerLevel = nestedInputs' . operation
  where nestedInputs' comb =
          map nestingInputs (nesting comb) ++
          case body comb of
            Lambda _           -> []
            NewNest nest comb' -> nestingInputs nest : nestedInputs' comb'
