{-# LANGUAGE UndecidableInstances #-}

module Futhark.Analysis.Refinement.Representation where

import Control.Applicative
import Control.Monad.State
import Data.Functor.Identity
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Futhark.Analysis.Refinement.CNF
import Futhark.MonadFreshNames
import Futhark.SoP.Convert
import Futhark.SoP.Monad
import Futhark.SoP.SoP (SoP, Substitute (..))
import Futhark.SoP.SoP qualified as SoP
import Futhark.Util.Pretty
import Language.Futhark (VName)
import Language.Futhark qualified as E
import Prelude

data Hole
  = Unnamed
  | Hole VName
  | CHole VName Term
  deriving (Show, Eq, Ord)

data Term
  = -- Exps
    Var VName
  | THole Hole
  | SoP (SoP Term)
  | Len Term
  | Elems Term
  | Set (S.Set Term)
  | Array [Term]
  | Range Term Term Term
  | Idx Term Term
  | Union Exp Exp
  | Unions Term Term Term Term
  | Sigma Exp Exp Exp
  | If Exp Exp Exp
  | BoolToInt Term
  | -- Props
    (:<) Exp Exp
  | (:<=) Exp Exp
  | (:>) Exp Exp
  | (:>=) Exp Exp
  | (:==) Exp Exp
  | (:/=) Exp Exp
  | PermutationOf Term Term
  | Forall VName Term Term
  | Not Term
  | Bool Bool
  | CNFTerm (CNF Term)
  deriving (Show, Eq, Ord)

instance Pretty Term where
  pretty (Var x) = pretty x
  pretty (THole h) = pretty h
  pretty (SoP sop) = pretty sop
  pretty (Len xs) = prettyPre "len" [xs]
  pretty (Elems x) = prettyPre "elems" [x]
  pretty (Set ts) = pretty ts
  pretty (Array ts) = pretty ts
  pretty (Idx arr i) = parens (pretty arr) <> "[" <> pretty i <> "]"
  pretty (Union x y) = pretty x <+> "U" <+> pretty y
  pretty (Unions i range cond xs) =
    "U_{"
      <> pretty i
      <> "="
        <+> pretty range
      <> ","
        <+> pretty cond
      <> "}"
        <+> parens (pretty xs)
  pretty (Sigma i set e) =
    "Σ_"
      <> pretty i
      <> "="
        <+> pretty set
        <+> parens (pretty e)
  pretty (If c t f) =
    "If"
      <+> parens (pretty c)
      <+> "then"
      <+> parens (pretty t)
      <+> "else"
      <+> parens (pretty f)
  pretty (BoolToInt e) = "bool_to_int" <> parens (pretty e)
  pretty (Range from step to) = prettyPre "range" [from, from ~+~ step, to]
  pretty (x :> y) = prettyBin ">" x y
  pretty (x :< y) = prettyBin "<" x y
  pretty (x :<= y) = prettyBin "<=" x y
  pretty (x :>= y) = prettyBin ">=" x y
  pretty (x :== y) = prettyBin "==" x y
  pretty (x :/= y) = prettyBin "/=" x y
  pretty (PermutationOf x y) = prettyPre "permutationOf" [x, y]
  pretty (Not x) = "!" <> parens (pretty x)
  pretty (Bool b) = pretty b
  pretty (CNFTerm cnf) = pretty cnf
  pretty x = error $ show x

instance Pretty Hole where
  pretty (CHole _ t) = "nest(" <> pretty t <> ")"
  pretty _ = "_"

prettyPre :: (Pretty b) => Doc a -> [b] -> Doc a
prettyPre op args =
  op <> parens (mconcat $ punctuate comma $ map pretty args)

prettyBin :: (Pretty b) => Doc a -> b -> b -> Doc a
prettyBin op x y =
  pretty x <+> op <+> pretty y

instance ASTMappable Term where
  astMap m (Var x) = (mapOnTerm m) $ Var x
  astMap m (THole h) = (mapOnTerm m) $ THole h
  astMap m (SoP sop) = do
    sop' <- foldl (SoP..+.) (SoP.int2SoP 0) <$> mapM g (SoP.sopToLists sop)
    case SoP.justSym sop' of
      Just x -> pure x
      Nothing -> pure $ SoP sop'
    where
      g (ts, n) = do
        ts' <- traverse (mapOnTerm m) ts
        pure $ foldl (SoP..*.) (SoP.int2SoP 1) (SoP.int2SoP n : map termToSoP ts')
  astMap m (Len t) = Len <$> mapOnTerm m t
  astMap m (Elems t) = Elems <$> mapOnTerm m t
  astMap m (Set ts) = (Set . S.fromList) <$> traverse (mapOnTerm m) (S.toList ts)
  astMap m (Array ts) = Array <$> traverse (mapOnTerm m) ts
  astMap m (Range from step to) = Range <$> astMap m from <*> astMap m step <*> astMap m to
  astMap m (Idx arr i) = Idx <$> astMap m arr <*> astMap m i
  astMap m (Union x y) = Union <$> astMap m x <*> astMap m y
  astMap m (Unions i s c xs) = Unions <$> astMap m i <*> astMap m s <*> astMap m c <*> astMap m xs
  astMap m (Sigma i set e) = Sigma <$> astMap m i <*> astMap m set <*> astMap m e
  astMap m (BoolToInt x) = BoolToInt <$> astMap m x
  astMap m (If c t f) = If <$> astMap m c <*> astMap m t <*> astMap m f
  astMap m (x :> y) = (:>) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :< y) = (:<) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :>= y) = (:>=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :<= y) = (:<=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :== y) = (:==) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (x :/= y) = (:/=) <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (PermutationOf x y) = PermutationOf <$> mapOnTerm m x <*> mapOnTerm m y
  astMap m (Forall x p1 p2) = Forall x <$> mapOnTerm m p1 <*> mapOnTerm m p2
  astMap _ t@(Bool {}) = pure t
  astMap m (Not p) = Not <$> astMap m p
  astMap m (CNFTerm cnf) = CNFTerm <$> astMap m cnf

class ASTMappable a where
  astMap :: (Monad m) => ASTMapper m -> a -> m a

data ASTMapper m = ASTMapper
  { mapOnTerm :: Term -> m Term
  }

instance (ASTMappable a) => Substitute VName Term a where
  substitute subst = idMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \e ->
                case e of
                  (Var x)
                    | Just x' <- subst M.!? x -> pure x'
                    | otherwise -> pure $ Var x
                  _ -> astMap m e
          }

idMap :: (ASTMappable a) => ASTMapper Identity -> a -> a
idMap m = runIdentity . astMap m

flatten :: (ASTMappable a) => a -> a
flatten = idMap m
  where
    m =
      ASTMapper
        { mapOnTerm =
            \e ->
              case e of
                Var x -> pure $ Var x
                THole h -> pure $ THole h
                _ -> astMap m e
        }

class Free a where
  fv :: a -> Set VName

instance (Free a) => Free (CNF a) where
  fv = (foldMap . foldMap) fv . cnfToLists

instance Free Term where
  fv = flip execState mempty . astMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \e ->
                case e of
                  Var x -> do
                    modify (S.insert x)
                    pure e
                  THole h -> do
                    put $ fv h
                    pure e
                  Unions i s cond xs -> do
                    put $ (fv s <> fv cond <> fv xs) S.\\ fv i
                    pure e
                  Sigma i set e -> do
                    put $ (fv set <> fv e) S.\\ fv i
                    pure e
                  Forall x p1 p2 -> do
                    put $ (fv p1 <> fv p2) S.\\ S.singleton x
                    pure e
                  _ -> astMap m e
          }

instance Free Hole where
  fv Unnamed = mempty
  fv (Hole x) = S.singleton x
  fv (CHole x term) = fv term

data Subst = Subst
  { termSubst :: Map VName Term,
    context :: Map VName (Term -> Term)
  }

instance Show Subst where
  show (Subst s_term ctxs) =
    show s_term ++ "\n" ++ show (fmap ($ (Bool True)) ctxs)

instance Semigroup Subst where
  (Subst s_term1 s_ctx1) <> (Subst s_term2 s_ctx2) =
    Subst (s_term1 <> s_term2) (s_ctx1 <> s_ctx2)

instance Monoid Subst where
  mempty = Subst mempty mempty

instance Free Subst where
  fv (Subst s_term _) = foldMap fv $ M.elems s_term

class AddSubst a where
  addSubst :: VName -> a -> Subst -> Subst

instance AddSubst Term where
  addSubst x t s = s {termSubst = M.insert x t $ termSubst s}

instance AddSubst (Term -> Term) where
  addSubst x ctx s =
    s {context = M.insertWith (.) x ctx $ context s}

class Context a where
  contexts :: a -> [(Term -> Term, Term)]

splits :: [a] -> [([a], a, [a])]
splits = splits' []
  where
    splits' _ [] = []
    splits' xs (y : ys) = (xs, y, ys) : splits' (xs ++ [y]) ys

instance Context Term where
  contexts Var {} = mempty
  contexts THole {} = mempty
  contexts (SoP sop) =
    term_contexts
    where
      term_contexts = do
        (lt, (ts, a), rt) <- splits $ SoP.sopToLists sop
        (l, t, r) <- splits ts
        pure
          ( \t' ->
              SoP $
                SoP.sopFromList $
                  lt ++ [(l ++ [t'] ++ r, a)] ++ rt,
            t
          )
  contexts (Len t) =
    [(Len, t)]
  contexts (Elems t) =
    [(Elems, t)]
  contexts (Set ts) =
    map (\(l, t, r) -> ((\t' -> Set $ S.fromList $ l ++ [t'] ++ r), t)) $
      splits $
        S.toList ts
  contexts (Array ts) =
    map (\(l, t, r) -> ((\t' -> Array $ l ++ [t'] ++ r), t)) $
      splits ts
  contexts (Range from step to) =
    [ ( \from' -> Range from' step to,
        from
      ),
      ( \step' -> Range from step' to,
        step
      ),
      ( \to' -> Range from step to',
        to
      )
    ]
  contexts (Idx arr i) =
    [ ( \arr' -> Idx arr' i,
        arr
      ),
      ( \i' -> Idx arr i',
        i
      )
    ]
  contexts (Union x y) =
    [ ( \x' -> Union x' y,
        x
      ),
      ( \y' -> Union x y',
        y
      )
    ]
  contexts (Unions i s c xs) =
    [ ( \s' -> Unions i s' c xs,
        s
      ),
      ( \c' -> Unions i s c' xs,
        c
      ),
      ( \xs' -> Unions i s c xs',
        xs
      )
    ]
  contexts (Sigma i s e) =
    [ ( \s' -> Sigma i s' e,
        s
      ),
      ( \e' -> Sigma i s e',
        e
      )
    ]
  contexts (If c t f) =
    [ ( \c' -> If c' t f,
        c
      ),
      ( \t' -> If c t' f,
        t
      ),
      ( \f' -> If c t f',
        f
      )
    ]
  contexts (BoolToInt x) =
    [ ( BoolToInt,
        x
      )
    ]
  contexts (e1 :> e2) =
    [ ( \e1' -> e1' :> e2,
        e1
      ),
      ( \e2' -> e1 :> e2',
        e2
      )
    ]
  contexts (e1 :< e2) =
    [ ( \e1' -> e1' :< e2,
        e1
      ),
      ( \e2' -> e1 :< e2',
        e2
      )
    ]
  contexts (e1 :>= e2) =
    [ ( \e1' -> e1' :>= e2,
        e1
      ),
      ( \e2' -> e1 :>= e2',
        e2
      )
    ]
  contexts (e1 :<= e2) =
    [ ( \e1' -> e1' :<= e2,
        e1
      ),
      ( \e2' -> e1 :<= e2',
        e2
      )
    ]
  contexts (e1 :/= e2) =
    [ ( \e1' -> e1' :/= e2,
        e1
      ),
      ( \e2' -> e1 :/= e2',
        e2
      )
    ]
  contexts (e1 :== e2) =
    [ ( \e1' -> e1' :== e2,
        e1
      ),
      ( \e2' -> e1 :== e2',
        e2
      )
    ]
  contexts (PermutationOf e1 e2) =
    [ ( \e1' -> PermutationOf e1' e2,
        e1
      ),
      ( \e2' -> PermutationOf e1 e2',
        e2
      )
    ]
  contexts (Forall x p1 p2) =
    [ ( \p1' -> Forall x p1' p2,
        p1
      ),
      ( \p2' -> Forall x p1 p2',
        p2
      )
    ]
  contexts (Not p) =
    [ ( \p' -> Not p',
        p
      )
    ]
  contexts Bool {} = mempty
  contexts (CNFTerm cnf) = mempty

class Replace a where
  replace :: Subst -> a -> a

(@) :: (Replace a) => a -> Subst -> a
x @ s = replace s x

infixl 9 @

instance {-# OVERLAPS #-} (Ord u, Replace u) => Replace (SoP.Term u) where
  replace s =
    SoP.toTerm . map (replace s) . SoP.termToList

instance Replace Term where
  replace s = idMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \t ->
                case t of
                  Var x -> do
                    case termSubst s M.!? x of
                      Nothing -> pure t
                      Just t' -> pure t'
                  THole Unnamed -> pure t
                  THole (Hole x) ->
                    case termSubst s M.!? x of
                      Nothing -> pure t
                      Just t' -> pure t'
                  THole (CHole x term) ->
                    case context s M.!? x of
                      Nothing -> pure $ THole $ CHole x $ replace s term
                      Just ctx -> pure $ ctx $ replace s term
                  _ -> astMap m t
          }

instance (Functor f, Replace a) => Replace (f a) where
  replace s = fmap (replace s)

instance (ASTMappable a) => ASTMappable (CNF a) where
  astMap m = traverse (astMap m)

class Rename a where
  rename :: (MonadFreshNames m) => Subst -> a -> m a

instance Rename Hole where
  rename s h@(Unnamed) = pure h
  rename s h@(Hole {}) = pure h
  rename s (CHole f term) =
    CHole f <$> rename s term

instance Rename Term where
  rename s = astMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \t ->
                case t of
                  Var x -> do
                    case termSubst s M.!? x of
                      Nothing -> pure t
                      Just t' -> pure t'
                  THole h -> THole <$> rename s h
                  Unions (Var i) set c xs -> do
                    i' <- newVName $ E.baseString i
                    let s' = addSubst i (Var i') s
                    Unions (Var i') <$> rename s' set <*> rename s' c <*> rename s' xs
                  Sigma (Var i) set e -> do
                    i' <- newVName $ E.baseString i
                    let s' = addSubst i (Var i') s
                    Sigma (Var i') <$> rename s' set <*> rename s' e
                  Forall x p1 p2 -> do
                    x' <- newVName $ E.baseString x
                    let s' = addSubst x (Var x') s
                    Forall x' <$> rename s' p1 <*> rename s' p2
                  _ -> astMap m t
          }

class Holes a where
  holes :: a -> Set VName
  instHole :: (MonadFreshNames m) => a -> m a
  hole :: a
  mkHole :: (MonadFreshNames m) => m a
  mkCHole :: (MonadFreshNames m) => m (Term -> a)

instance Holes Hole where
  holes Unnamed = mempty
  holes (Hole x) = S.singleton x
  holes (CHole x term) = S.singleton x <> holes term

  instHole Unnamed = mkHole
  instHole h = pure h

  mkHole = do
    h <- newVName "hole"
    pure $ Hole h

  mkCHole = do
    ctx <- newVName "ctx"
    pure $ CHole ctx

  hole = Unnamed

instance (Holes a) => Holes (CNF a) where
  holes = (foldMap . foldMap) holes . cnfToLists
  instHole = traverse instHole

instance Holes Term where
  holes = flip execState mempty . astMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \t ->
                case t of
                  Var x -> pure t
                  THole h -> do
                    modify $ (<> holes h)
                    pure t
                  _ -> astMap m t
          }

  instHole = astMap m
    where
      m =
        ASTMapper
          { mapOnTerm =
              \t ->
                case t of
                  Var x -> pure t
                  THole h -> THole <$> instHole h
                  _ -> astMap m t
          }

  mkHole = THole <$> mkHole

  mkCHole = do
    (THole .) <$> mkCHole

  hole = THole hole

hasHoles :: (Holes a) => a -> Bool
hasHoles = not . S.null . holes

mkHoles :: (Holes a, MonadFreshNames m) => Int -> m [a]
mkHoles = flip replicateM mkHole

instance Nameable Term where
  mkName (VNameSource i) = (Var $ E.VName "x" i, VNameSource $ i + 1)

instance ToSoP Term E.Exp where
  toSoPNum e = do
    x <- lookupUntransPE e
    pure (1, SoP.sym2SoP x)

occursIn :: (Free a) => VName -> a -> Bool
occursIn x = S.member x . fv

getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

negateProp :: Prop -> Prop
negateProp (x :== y) = x :/= y
negateProp (Not p) = p
negateProp (x :< y) = x :>= y
negateProp (x :<= y) = x :> y
negateProp (x :> y) = x :<= y
negateProp (x :>= y) = x :< y
negateProp (x :/= y) = x :== y
negateProp p = Not p

setSize :: Exp -> Maybe Exp
setSize (Range from step to)
  | step == intToTerm 1 -- Should be a monadic check with env
    =
      Just $ to ~-~ from ~+~ intToTerm 1
setSize _ = Nothing

-- Fix
knownFromCtx :: (Term -> Term) -> [Term]
knownFromCtx ctx = fromMaybe mempty $ knownFromCtx' $ ctx garbage
  where
    garbage = Not $ Len $ THole $ Unnamed
    knownFromCtx' :: Term -> Maybe [Term]
    knownFromCtx' (Not (Len (THole Unnamed))) = pure mempty
    knownFromCtx' (Unions (Var i) set (CNFTerm cnf) e)
      | [ps] <- dnfToLists $ toDNF cnf =
          (ps ++) <$> (knownFromCtx' set <|> knownFromCtx' e)
    knownFromCtx' e = choice $ map (knownFromCtx' . snd) $ contexts e

choice :: (Foldable t, Alternative f) => t (f a) -> f a
choice = foldl (<|>) empty

type Prop = Term

type Exp = Term

(~-~) :: Exp -> Exp -> Exp
x ~-~ y = flatten $ SoP $ termToSoP x SoP..-. termToSoP y

(~+~) :: Exp -> Exp -> Exp
x ~+~ y = flatten $ SoP $ termToSoP x SoP..+. termToSoP y

(~*~) :: Exp -> Exp -> Exp
x ~*~ y = flatten $ SoP $ termToSoP x SoP..*. termToSoP y

termToSoP :: Exp -> SoP Exp
termToSoP e =
  case flatten e of
    SoP sop -> sop
    e -> SoP.sym2SoP e

termToSet :: Exp -> Exp
termToSet (Array rs) = Set $ S.fromList rs
termToSet e = e

intToTerm :: Integer -> Term
intToTerm = SoP . SoP.int2SoP

(...) :: Term -> Term -> Term
from ... to = Range from (intToTerm 1) to

infix 1 ...

-- Old instances that didn't use astMap

-- instance Free Term where
--   fv (Var x) = S.singleton x
--   fv (THole h) = fv h
--   fv (SoP sop) = foldMap (foldMap fv . fst) $ SoP.sopToLists sop
--   fv (Len e) = fv e
--   fv (Elems e) = fv e
--   fv (Set es) = foldMap fv es
--   fv (Array es) = foldMap fv es
--   fv (Range from step to) = fv from <> fv step <> fv to
--   fv (Idx arr i) = fv arr <> fv i
--   fv (Union x y) = fv x <> fv y
--   fv (Unions i s cond xs) = (fv s <> fv cond <> fv xs) S.\\ fv i
--   fv (Sigma i set e) = fv set <> fv e S.\\ fv i
--   fv (If c t f) = fv c <> fv t <> fv f
--   fv (BoolToInt x) = fv x
--   fv (e1 :== e2) = fv e1 <> fv e2
--   fv (e1 :> e2) = fv e1 <> fv e2
--   fv (PermutationOf e1 e2) = fv e1 <> fv e2
--   fv (Forall x p1 p2) = (fv p1 <> fv p2) S.\\ S.singleton x
--   fv (Not p) = fv p
--   fv Bool {} = mempty
--   fv (CNFTerm cnf) = fv cnf
--   fv t = error $ show t

-- instance Replace Term where
--  replace s t@(Var x) =
--    case termSubst s M.!? x of
--      Nothing -> t
--      Just t' -> t'
--  replace s h@(THole Unnamed) = h
--  replace s h@(THole (Hole x)) =
--    case termSubst s M.!? x of
--      Nothing -> h
--      Just t -> t
--  replace s (THole (CHole x term)) =
--    case context s M.!? x of
--      Nothing -> THole $ CHole x term'
--      Just ctx -> ctx term'
--    where
--      term' = replace s term
--  replace s (SoP sop) = SoP $ SoP.mapSymSoP_ (replace s) sop
--  replace s (Len t) = Len $ replace s t
--  replace s (Elems t) = Elems $ replace s t
--  replace s (Set ts) = Set $ S.map (replace s) ts
--  replace s (Array ts) = Array $ map (replace s) ts
--  replace s (Range from step to) = Range (replace s from) (replace s step) (replace s to)
--  replace s (Idx arr i) = Idx (replace s arr) (replace s i)
--  replace s (Union x y) = Union (replace s x) (replace s y)
--  replace s (Unions (Var i) set c xs) = Unions (Var i) (replace s set) (replace s c) (replace s xs)
--  replace s (Unions i set c xs) = Unions (replace s i) (replace s set) (replace s c) (replace s xs)
--  replace s (Sigma (Var i) set e) = Sigma (Var i) (replace s set) (replace s e)
--  replace s (Sigma i set e) = Sigma (replace s i) (replace s set) (replace s e)
--  replace s (If c t f) = If (replace s c) (replace s t) (replace s f)
--  replace s (BoolToInt x) = BoolToInt (replace s x)
--  replace s (x :== y) = replace s x :== replace s y
--  replace s (x :> y) = replace s x :> replace s y
--  replace s (Forall x p1 p2) =
--    Forall x (replace s p1) (replace s p2)
--  replace _ t@Bool {} = t
--  replace s (Not p) = Not $ replace s p
--  replace s (CNFTerm cnf) = CNFTerm $ replace s <$> cnf
--  replace _ t = error $ prettyString t

-- instance Rename Term where
--  rename s (Var x) =
--    case termSubst s M.!? x of
--      Just t -> pure t
--      Nothing -> pure $ Var x
--  rename s (THole h) = THole <$> rename s h
--  rename s (SoP sop) = SoP <$> SoP.mapSymSoPM (rename s) sop
--  rename s (Len t) = Len <$> rename s t
--  rename s (Elems t) = Elems <$> rename s t
--  rename s (Set ts) = (Set . S.fromList) <$> mapM (rename s) (S.toList ts)
--  rename s (Array ts) = Array <$> mapM (rename s) ts
--  rename s (Range from step to) = Range <$> rename s from <*> rename s step <*> rename s to
--  rename s (Idx arr i) = Idx <$> rename s arr <*> rename s i
--  rename s (Union x y) = Union <$> rename s x <*> rename s y
--  rename s (Unions (Var i) set c xs) = do
--    i' <- newVName $ E.baseString i
--    let s' = addSubst i (Var i') s
--    Unions (Var i') <$> rename s' set <*> rename s' c <*> rename s' xs
--  rename s (Unions i set c xs) =
--    Unions <$> rename s i <*> rename s set <*> rename s c <*> rename s xs
--  rename s (Sigma (Var i) set e) = do
--    i' <- newVName $ E.baseString i
--    let s' = addSubst i (Var i') s
--    Sigma (Var i') <$> rename s' set <*> rename s' e
--  rename s (Sigma i set e) =
--    Sigma <$> rename s i <*> rename s set <*> rename s e
--  rename s (If c t f) = If <$> rename s c <*> rename s t <*> rename s f
--  rename s (BoolToInt x) = BoolToInt <$> rename s x
--  rename s (x :== y) = (:==) <$> rename s x <*> rename s y
--  rename s (x :> y) = (:>) <$> rename s x <*> rename s y
--  rename s (PermutationOf x y) = PermutationOf <$> rename s x <*> rename s y
--  rename s (Forall x p1 p2) = do
--    x' <- newVName $ E.baseString x
--    let s' = addSubst x (Var x') s
--    Forall x' <$> rename s' p1 <*> rename s' p2
--  rename _ t@Bool {} = pure t
--  rename s (Not p) = Not <$> rename s p
--  rename s (CNFTerm cnf) =
--    CNFTerm <$> traverse (rename s) cnf
--  rename s t = error $ show t

-- instance Holes Term where
--  holes Var {} = mempty
--  holes (THole h) = holes h
--  holes (SoP sop) = foldMap holes $ concatMap fst $ SoP.sopToLists sop
--  holes (Len t) = holes t
--  holes (Elems t) = holes t
--  holes (Set ts) = foldMap holes ts
--  holes (Array ts) = foldMap holes ts
--  holes (Range from step to) = holes from <> holes step <> holes to
--  holes (Idx arr i) = holes arr <> holes i
--  holes (Union x y) = holes x <> holes y
--  holes (Unions i s c xs) = holes i <> holes s <> holes c <> holes xs
--  holes (Sigma i set e) = holes i <> holes set <> holes e
--  holes (If c t f) = holes c <> holes t <> holes f
--  holes (BoolToInt x) = holes x
--  holes (x :== y) = holes x <> holes y
--  holes (x :> y) = holes x <> holes y
--  holes (PermutationOf x y) = holes x <> holes y
--  holes (Forall x p1 p2) = holes p1 <> holes p2
--  holes Bool {} = mempty
--  holes (Not p) = holes p
--  holes (CNFTerm cnf) = holes cnf
--
--  instHole t@(Var {}) = pure t
--  instHole (THole h) = THole <$> instHole h
--  instHole (SoP sop) = SoP <$> SoP.mapSymSoPM instHole sop
--  instHole (Len t) = Len <$> instHole t
--  instHole (Elems t) = Elems <$> instHole t
--  instHole (Set ts) = (Set . S.fromList) <$> mapM instHole (S.toList ts)
--  instHole (Array ts) = Array <$> mapM instHole ts
--  instHole (Range from step to) = Range <$> instHole from <*> instHole step <*> instHole to
--  instHole (Idx arr i) = Idx <$> instHole arr <*> instHole i
--  instHole (Union x y) = Union <$> instHole x <*> instHole y
--  instHole (Unions i s c xs) = Unions <$> instHole i <*> instHole s <*> instHole c <*> instHole xs
--  instHole (Sigma i set e) = Sigma <$> instHole i <*> instHole set <*> instHole e
--  instHole (If c t f) = If <$> instHole c <*> instHole t <*> instHole f
--  instHole (BoolToInt x) = BoolToInt <$> instHole x
--  instHole (x :== y) = (:==) <$> instHole x <*> instHole y
--  instHole (x :> y) = (:>) <$> instHole x <*> instHole y
--  instHole (PermutationOf x y) = PermutationOf <$> instHole x <*> instHole y
--  instHole (Forall x p1 p2) = Forall x <$> instHole p1 <*> instHole p2
--  instHole t@Bool {} = pure t
--  instHole (Not p) = Not <$> instHole p
--  instHole (CNFTerm cnf) = CNFTerm <$> instHole cnf
--
--  mkHole = THole <$> mkHole
--
--  mkCHole = do
--    (THole .) <$> mkCHole
--
--  hole = THole hole
