{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module L0.Traversals
  ( Mapper(..)
  , identityMapper
  , mapExpM
  , mapExp
  , Folder(..)
  , foldExpM
  , foldExp
  , Walker(..)
  , identityWalker
  , walkExpM
  , foldlPattern
  , buildExpPattern
  , progNames
  )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Set as S

import L0.AbSyn

data Mapper ty m = Mapper {
    mapOnExp :: Exp ty -> m (Exp ty)
  , mapOnType :: ty -> m ty
  , mapOnLambda :: Lambda ty -> m (Lambda ty)
  , mapOnPattern :: TupIdent ty -> m (TupIdent ty)
  , mapOnIdent :: Ident ty -> m (Ident ty)
  , mapOnValue :: Value -> m Value
  }

identityMapper :: Monad m => Mapper ty m
identityMapper = Mapper {
                   mapOnExp = return
                 , mapOnType = return
                 , mapOnLambda = return
                 , mapOnPattern = return
                 , mapOnIdent = return
                 , mapOnValue = return
                 }

mapExpM :: (Applicative m, Monad m) => Mapper ty m -> Exp ty -> m (Exp ty)
mapExpM tv (Var ident) =
  pure Var <*> mapOnIdent tv ident
mapExpM tv (Literal val loc) =
  pure Literal <*> mapOnValue tv val <*> pure loc
mapExpM tv (TupLit els loc) =
  pure TupLit <*> mapM (mapOnExp tv) els <*> pure loc
mapExpM tv (ArrayLit els elt loc) =
  pure ArrayLit <*> mapM (mapOnExp tv) els <*> mapOnType tv elt <*> pure loc
mapExpM tv (BinOp bop x y t loc) =
  pure (BinOp bop) <*>
         mapOnExp tv x <*> mapOnExp tv y <*>
         mapOnType tv t <*> pure loc
mapExpM tv (And x y loc) =
  pure And <*> mapOnExp tv x <*> mapOnExp tv y <*> pure loc
mapExpM tv (Or x y loc) =
  pure Or <*> mapOnExp tv x <*> mapOnExp tv y <*> pure loc
mapExpM tv (Not x loc) =
  pure Not <*> mapOnExp tv x <*> pure loc
mapExpM tv (Negate x t loc) =
  pure Negate <*> mapOnExp tv x <*> mapOnType tv t <*> pure loc
mapExpM tv (If c texp fexp t loc) =
  pure If <*> mapOnExp tv c <*>
         mapOnExp tv texp <*> mapOnExp tv fexp <*>
         mapOnType tv t <*> pure loc
mapExpM tv (Apply fname args t loc) =
  pure (Apply fname) <*> mapM (mapOnExp tv) args <*>
         mapOnType tv t <*> pure loc
mapExpM tv (LetPat pat e body loc) =
  pure LetPat <*> mapOnPattern tv pat <*> mapOnExp tv e <*>
         mapOnExp tv body <*> pure loc
mapExpM tv (LetWith dest src idxexps vexp body loc) =
  pure LetWith <*> mapOnIdent tv dest <*> mapOnIdent tv src <*>
         mapM (mapOnExp tv) idxexps <*> mapOnExp tv vexp <*>
         mapOnExp tv body <*> pure loc
mapExpM tv (Index arr idxexps int outt loc) =
  pure Index <*> mapOnIdent tv arr <*>
         mapM (mapOnExp tv) idxexps <*>
         mapOnType tv int <*> mapOnType tv outt <*> pure loc
mapExpM tv (Iota nexp loc) =
  pure Iota <*> mapOnExp tv nexp <*> pure loc
mapExpM tv (Size e loc) =
  pure Size <*> mapOnExp tv e <*> pure loc
mapExpM tv (Replicate nexp vexp loc) =
  pure Replicate <*> mapOnExp tv nexp <*> mapOnExp tv vexp <*> pure loc
mapExpM tv (Reshape shape arrexp loc) =
  pure Reshape <*> mapM (mapOnExp tv) shape <*>
       mapOnExp tv arrexp <*> pure loc
mapExpM tv (Transpose e loc) =
  pure Transpose <*> mapOnExp tv e <*> pure loc
mapExpM tv (Map fun e int outt loc) =
  pure Map <*> mapOnLambda tv fun <*> mapOnExp tv e <*>
       mapOnType tv int <*> mapOnType tv outt <*> pure loc
mapExpM tv (Reduce fun startexp arrexp int loc) =
  pure Reduce <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
       mapOnType tv int <*> pure loc
mapExpM tv (Zip args loc) = do
  args' <- forM args $ \(argexp, argt) -> do
                              argexp' <- mapOnExp tv argexp
                              argt' <- mapOnType tv argt
                              pure (argexp', argt')
  pure $ Zip args' loc
mapExpM tv (Unzip e ts loc) =
  pure Unzip <*> mapOnExp tv e <*> mapM (mapOnType tv) ts <*> pure loc
mapExpM tv (Scan fun startexp arrexp t loc) =
  pure Scan <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapOnExp tv arrexp <*>
       mapOnType tv t <*> pure loc
mapExpM tv (Filter fun arrexp t loc) =
  pure Filter <*> mapOnLambda tv fun <*> mapOnExp tv arrexp <*>
       mapOnType tv t <*> pure loc
mapExpM tv (Mapall fun arrexp int outt loc) =
  pure Mapall <*> mapOnLambda tv fun <*> mapOnExp tv arrexp <*>
       mapOnType tv int <*> mapOnType tv outt <*> pure loc
mapExpM tv (Redomap redfun mapfun accexp arrexp intype outtype loc) =
  pure Redomap <*> mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
       mapOnExp tv accexp <*>mapOnExp tv arrexp <*>
       mapOnType tv intype <*> mapOnType tv outtype <*> pure loc
mapExpM tv (Split nexp arrexp t loc) =
  pure Split <*> mapOnExp tv nexp <*> mapOnExp tv arrexp <*>
       mapOnType tv t <*> pure loc
mapExpM tv (Concat x y t loc) =
  pure Concat <*> mapOnExp tv x <*> mapOnExp tv y <*>
       mapOnType tv t <*> pure loc
mapExpM tv (Copy e loc) =
  pure Copy <*> mapOnExp tv e <*> pure loc
mapExpM tv (DoLoop mergepat mergeexp loopvar boundexp loopbody letbody loc) =
  pure DoLoop <*> mapOnPattern tv mergepat <*> mapOnExp tv mergeexp <*>
       mapOnIdent tv loopvar <*> mapOnExp tv boundexp <*>
       mapOnExp tv loopbody <*> mapOnExp tv letbody <*> pure loc
mapExpM tv (Map2 fun arrexps intype outtype loc) =
  pure Map2 <*> mapOnLambda tv fun <*> mapM (mapOnExp tv) arrexps <*>
       mapOnType tv intype <*> mapOnType tv outtype <*> pure loc
mapExpM tv (Reduce2 fun startexp arrexps intype loc) =
  pure Reduce2 <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapM (mapOnExp tv) arrexps <*>
       mapOnType tv intype <*> pure loc
mapExpM tv (Scan2 fun startexp arrexps intype loc) =
  pure Scan2 <*> mapOnLambda tv fun <*>
       mapOnExp tv startexp <*> mapM (mapOnExp tv) arrexps <*>
       mapOnType tv intype <*> pure loc
mapExpM tv (Filter2 fun arrexps eltype loc) =
  pure Filter2 <*> mapOnLambda tv fun <*>
       mapM (mapOnExp tv) arrexps <*>
       mapOnType tv eltype <*> pure loc
mapExpM tv (Mapall2 fun arrexps intype outtype loc) =
  pure Mapall2 <*> mapOnLambda tv fun <*>
       mapM (mapOnExp tv) arrexps <*>
       mapOnType tv intype <*> mapOnType tv outtype <*> pure loc
mapExpM tv (Redomap2 redfun mapfun accexp arrexps intype outtype loc) =
  pure Redomap2 <*> mapOnLambda tv redfun <*> mapOnLambda tv mapfun <*>
       mapOnExp tv accexp <*> mapM (mapOnExp tv) arrexps <*>
       mapOnType tv intype <*> mapOnType tv outtype <*> pure loc

mapExp :: Mapper ty Identity -> Exp ty -> Exp ty
mapExp m = runIdentity . mapExpM m

data Folder ty a m = Folder {
    foldOnExp :: a -> Exp ty -> m a
  , foldOnType :: a -> ty -> m a
  , foldOnLambda :: a -> Lambda ty -> m a
  , foldOnPattern :: a -> TupIdent ty -> m a
  , foldOnIdent :: a -> Ident ty -> m a
  , foldOnValue :: a -> Value -> m a
  }

foldExpM :: (Monad m, Functor m) => Folder ty a m -> a -> Exp ty -> m a
foldExpM f x e = execStateT (mapExpM m e) x
  where m = Mapper {
              mapOnExp = wrap foldOnExp
            , mapOnType = wrap foldOnType
            , mapOnLambda = wrap foldOnLambda
            , mapOnPattern = wrap foldOnPattern
            , mapOnIdent = wrap foldOnIdent
            , mapOnValue = wrap foldOnValue
            }
        wrap op k = do
          v <- get
          put =<< lift (op f v k)
          return k

foldExp :: Folder ty a Identity -> a -> Exp ty -> a
foldExp m x = runIdentity . foldExpM m x

data Walker ty m = Walker {
    walkOnExp :: Exp ty -> m ()
  , walkOnType :: ty -> m ()
  , walkOnLambda :: Lambda ty -> m ()
  , walkOnPattern :: TupIdent ty -> m ()
  , walkOnIdent :: Ident ty -> m ()
  , walkOnValue :: Value -> m ()
  }

identityWalker :: Monad m => Walker ty m
identityWalker = Walker {
                   walkOnExp = const $ return ()
                 , walkOnType = const $ return ()
                 , walkOnLambda = const $ return ()
                 , walkOnPattern = const $ return ()
                 , walkOnIdent = const $ return ()
                 , walkOnValue = const $ return ()
                 }

walkExpM :: (Monad m, Applicative m) => Walker ty m -> Exp ty -> m ()
walkExpM f = void . mapExpM m
  where m = Mapper {
              mapOnExp = wrap walkOnExp
            , mapOnType = wrap walkOnType
            , mapOnLambda = wrap walkOnLambda
            , mapOnPattern = wrap walkOnPattern
            , mapOnIdent = wrap walkOnIdent
            , mapOnValue = wrap walkOnValue
            }
        wrap op k = op f k >> return k

------------------------------------------
------------- foldlPattern ---------------
------------------------------------------
foldlPattern :: TypeBox tf =>   (a -> Exp tf    -> a) ->
                                (a -> Lambda tf -> a) ->
                                a -> Exp tf -> a
foldlPattern _ _ ne (Literal _ _)  = ne
foldlPattern _ _ ne (Var     _)    = ne
foldlPattern f _ ne (Negate e _ _) = f ne e
foldlPattern f _ ne (Not    e _  ) = f ne e
foldlPattern f _ ne (Copy   e _  ) = f ne e

foldlPattern f _ ne (TupLit els _)     = foldl f ne els
foldlPattern f _ ne (ArrayLit els _ _) = foldl f ne els
foldlPattern f _ ne (BinOp _ e1 e2 _ _)= foldl f ne [e1, e2]
foldlPattern f _ ne (And e1 e2 _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (Or  e1 e2 _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (If e1 e2 e3 _ _)  = foldl f ne [e1, e2, e3]
foldlPattern f _ ne (Apply _ es _ _)   = foldl f ne es

foldlPattern f _ ne (DoLoop _ mergeexp _ n lbody letexp _) =
    foldl f ne (mergeexp : [n,lbody,letexp])
foldlPattern f _ ne (LetWith _ _ inds el body _) = foldl f ne (el : body : inds)
foldlPattern f _ ne (LetPat  _ e body _)   = foldl f ne [e, body]
foldlPattern f _ ne (Index _ inds _ _ _ )  = foldl f ne inds

foldlPattern f _ ne (Iota      e _    )    = f ne e
foldlPattern f _ ne (Size      e _    )    = f ne e
foldlPattern f _ ne (Transpose e _    )    = f ne e
foldlPattern f _ ne (Unzip     e   _ _)    = f ne e
foldlPattern f _ ne (Zip exptps _)         = foldl f ne (fst (unzip exptps))
foldlPattern f _ ne (Replicate e1 e2 _)  = foldl f ne [e1, e2]
foldlPattern f _ ne (Reshape es e _)       = foldl f ne (e:es)
foldlPattern f doLam ne (Map lam e _ _ _)      = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Mapall lam e _ _ _)   = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Reduce lam e1 e2 _ _) = foldl f (doLam ne lam) (e1:e2:getLambdaExps lam)
foldlPattern f doLam ne (Scan lam e1 e2 _ _)   = foldl f (doLam ne lam) (e1:e2:getLambdaExps lam)
foldlPattern f doLam ne (Filter lam e _ _)     = foldl f (doLam ne lam) (e:getLambdaExps lam)
foldlPattern f doLam ne (Redomap lam1 lam2 e1 e2 _ _ _) =
    foldl   f (doLam (doLam ne lam1) lam2)
            (e1:e2: getLambdaExps lam1 ++ getLambdaExps lam2 )
foldlPattern f _ ne (Split e1 e2 _ _)      = foldl f ne [e1, e2]
foldlPattern f _ ne (Concat e1 e2 _ _)     = foldl f ne [e1, e2]
-- soac2 implem (Cosmin) --
foldlPattern f doLam ne (Map2 lam e _ _ _)      = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Mapall2 lam e _ _ _)   = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Reduce2 lam e1 e2 _ _) = foldl f (doLam ne lam) (e1:e2++getLambdaExps lam)
foldlPattern f doLam ne (Scan2 lam e1 e2 _ _)   = foldl f (doLam ne lam) (e1:e2++getLambdaExps lam)
foldlPattern f doLam ne (Filter2 lam e _ _)     = foldl f (doLam ne lam) (e++getLambdaExps lam)
foldlPattern f doLam ne (Redomap2 lam1 lam2 e1 e2 _ _ _) =
    foldl   f (doLam (doLam ne lam1) lam2)
            (e1 : e2 ++ getLambdaExps lam1 ++ getLambdaExps lam2 )


getLambdaExps :: TypeBox tf => Lambda tf -> [Exp tf]
getLambdaExps (AnonymFun _ body   _ _) = [body]
getLambdaExps (CurryFun  _ params _ _) = params

-----------------------------------------------
------------- buildExpPattern   ---------------
-----------------------------------------------
buildExpPattern :: TypeBox tf => (Exp tf -> Exp tf) -> Exp tf -> Exp tf
buildExpPattern f = mapExp f'
  where f' = identityMapper {
               mapOnExp = return . f
             , mapOnLambda = return . buildLambda f
             }

buildLambda :: TypeBox tf => (Exp tf -> Exp tf) -> Lambda tf -> Lambda tf
buildLambda f (AnonymFun tps body  tp pos) = AnonymFun tps     (f body  ) tp pos
buildLambda f (CurryFun  nm params tp pos) = CurryFun  nm  (map f params) tp pos

-- | Return the set of all variable names bound in program.
progNames :: forall ty.TypeBox ty => Prog ty -> S.Set Name
progNames = execWriter . mapM funNames
  where names = identityWalker {
                  walkOnExp = expNames
                , walkOnLambda = lambdaNames
                , walkOnPattern = patNames
                }

        one = tell . S.singleton . identName
        funNames (_, _, params, body, _) =
          mapM_ one params >> expNames body

        expNames e@(LetWith dest _ _ _ _ _) =
          one dest >> walkExpM names e
        expNames e = walkExpM names e

        lambdaNames (AnonymFun params body _ _) =
          mapM_ one params >> expNames body
        lambdaNames _ = return ()

        patNames (Id ident)     = one ident
        patNames (TupId pats _) = mapM_ patNames pats
