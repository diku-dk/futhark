-- | Expand arrays inside of kernels when possible.
module Futhark.Pass.ExpandArrays
       ( expandArrays )
       where

import Control.Applicative
import Control.Monad.RWS
import Data.List
import qualified Data.HashSet as HS

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.SOACS
import Futhark.Tools
import Futhark.Pass

expandArrays :: Pass SOACS SOACS
expandArrays = simplePass
               "expand arrays"
               "Expand arrays inside kernels" $
               intraproceduralTransformation transformFunDec

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = do
  body' <- transformBody $ funDecBody fundec
  return fundec { funDecBody = body' }

transformBody :: MonadFreshNames m =>
                 Body -> m Body
transformBody (Body () bnds res) = do
  bnds' <- concat <$> mapM transformBinding bnds
  return $ Body () bnds' res

transformBinding :: MonadFreshNames m =>
                    Binding -> m [Binding]

transformBinding (Let pat () e) = do
  (bnds, e') <- transformExp =<< mapExpM transform e
  return $ bnds ++ [Let pat () e']
  where transform = identityMapper { mapOnBody = transformBody
                                   , mapOnLambda = transformLambda
                                   , mapOnExtLambda = transformExtLambda
                                   }

transformLambda :: MonadFreshNames m =>
                   Lambda -> m Lambda
transformLambda lam = do
  body' <- transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

transformExtLambda :: MonadFreshNames m =>
                      ExtLambda -> m ExtLambda
transformExtLambda lam = do
  body' <- transformBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }

transformExp :: MonadFreshNames m =>
                Exp -> m ([Binding], Exp)
transformExp (LoopOp (MapKernel cs w thread_num ispace inps returns body)) = do
  (body', expanded) <- expandInKernel thread_num ispace inps $ expandInBody body
  let inps' = inps ++ map expandedInput expanded
  return (map expansionBinding expanded,
          LoopOp $ MapKernel cs w thread_num ispace inps' returns body')
transformExp e =
  return ([], e)

type ExpandM = RWS ExpandEnv [ExpandedArray] VNameSource

data ExpandedArray =
  ExpandedArray { expandedInput :: KernelInput SOACS
                , _expandedName :: VName
                , _expandedType :: Type
                }

expansionBinding :: ExpandedArray -> Binding
expansionBinding (ExpandedArray _ name t) =
  mkLet' [] [Ident name t] $ PrimOp $ Scratch (elemType t) (arrayDims t)

data ExpandEnv = ExpandEnv { envKernelSpace :: [(VName, SubExp)]
                           , envKernelVariant :: Names
                           }

variantIn :: Names -> ExpandM a -> ExpandM a
variantIn names = local $ \env -> env { envKernelVariant = names <> envKernelVariant env }

expandInKernel :: MonadFreshNames m =>
                  VName -> [(VName, SubExp)] -> [KernelInput SOACS]
               -> ExpandM a
               -> m (a, [ExpandedArray])
expandInKernel thread_num ispace inps m =
  modifyNameSource $ frob . runRWS m (ExpandEnv ispace variant)
  where frob (x,y,z) = ((x,z),y)

        variant = thread_num `HS.insert`
                  HS.fromList (map fst ispace ++ map kernelInputName inps)

expandInBody :: Body -> ExpandM Body
expandInBody (Body _ bnds res) = do
  bnds' <- variantIn (boundByBindings bnds) $ mapM expandInBinding bnds
  return $ mkBody bnds' res

expandInBinding :: Binding -> ExpandM Binding
expandInBinding (Let pat () e) = do
  e'   <- expandInExp e
  pat' <- if expandForExp e'
          then expandInPattern pat
          else return pat
  return $ Let pat' () e'
  where expandForExp (LoopOp DoLoop{})    = False
        expandForExp (PrimOp Index{})     = False
        expandForExp (PrimOp Reshape{})   = False
        expandForExp (PrimOp Rearrange{}) = False
        expandForExp _                    = True

expandInPattern :: Pattern -> ExpandM Pattern
expandInPattern pat@(Pattern context values) = do
  variant <- mappend (HS.fromList $ patternNames pat) <$>
             asks envKernelVariant
  let invariantShape =
        not . any (`HS.member` variant) . shapeVars
  Pattern context <$> mapM (expandInPatElem invariantShape) values
  where expandInPatElem invariantShape pat_elem
          | BindVar <- patElemBindage pat_elem,
            Array _ shape _ <- patElemType pat_elem,
            invariantShape shape = do
              new_bindage <- inPlaceInput
                             (patElemName pat_elem)
                             (patElemType pat_elem)
              return pat_elem { patElemBindage = new_bindage }
          | otherwise =
              return pat_elem

inPlaceInput :: VName -> Type -> ExpandM Bindage
inPlaceInput name t = do
  -- We need to create two names: one for the expanded array, and one
  -- for the new kernel input.
  expanded_name <- newVName $ baseString name <> "_expanded"
  input_name    <- newVName $ baseString name <> "_expanded_slice"
  kernel_dims <- asks $ map snd . envKernelSpace
  kernel_indices <- asks $ map fst . envKernelSpace
  let new_input = KernelInput (Param input_name t) expanded_name $
                  map Var kernel_indices
      expanded_t = arrayOfShape t $ Shape kernel_dims
  tell [ExpandedArray new_input expanded_name expanded_t]
  return $ BindInPlace [] input_name []

expandInExp :: Exp -> ExpandM Exp
expandInExp (LoopOp (DoLoop res merge form body)) =
  return $ LoopOp $ DoLoop res merge form body -- Cannot safely do this inside do-loops.
expandInExp e = mapExpM transform e
  where transform = identityMapper { mapOnBody = expandInBody
                                   , mapOnLambda = fail "Cannot expand in lambda"
                                   , mapOnExtLambda = fail "Cannot expand in ext lambda"
                                   }
