-- | The OpenCL code generator is a fragile and sensitive thing and it
-- needs a carefully massaged program to work at all.
--
-- This pass will turn most SOACs into sequential loops, except for
-- outermost map nests.  Additionally, reduces and scans containing
-- maps will be interchanged inwards if possible, bringing the maps
-- out and exposing more parallelism.
module Futhark.KernelBabysitting
       ( babysitKernels )
       where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State
import qualified Data.HashMap.Lazy as HM
import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.Basic
import Futhark.Tools
import qualified Futhark.FirstOrderTransform as FOT

babysitKernels :: Prog -> Prog
babysitKernels = intraproceduralTransformation transformFunDec

transformFunDec :: MonadFreshNames m => FunDec -> m FunDec
transformFunDec fundec = do
  (body', _) <- modifyNameSource $ runState (runBinderT m HM.empty)
  return fundec { funDecBody = body' }
  where m = bindingIdentTypes (map paramIdent $ funDecParams fundec) $
            transformBody $ funDecBody fundec

type SequentialiseM = Binder Basic

transformBody :: Body -> SequentialiseM Body
transformBody (Body () bnds res) = insertBindingsM $ do
  mapM_ transformBinding bnds
  return $ resultBody res

transformBinding :: Binding -> SequentialiseM ()
transformBinding (Let pat () (LoopOp (Map cs w fun arrs)))
  | Body () [bnd] res <- lambdaBody fun, -- Body has a single binding
    map Var (patternNames $ bindingPattern bnd) == res, -- Returned verbatim
    LoopOp (Map {}) <- bindingExp bnd = do
      fun' <- transformLambda fun
      addBinding $ Let pat () $ LoopOp $ Map cs w fun' arrs

  | otherwise = do
      fun' <- joinBinder $ FOT.transformLambda fun
      addBinding $ Let pat () $ LoopOp $ Map cs w fun' arrs

-- ISWIM to get out the maps.
transformBinding (Let res_pat () (LoopOp op))
  | Scan cs w scan_fun scan_input <- op,
    Body () [bnd] res <- lambdaBody scan_fun, -- Body has a single binding
    map Var (patternNames $ bindingPattern bnd) == res, -- Returned verbatim
    LoopOp (Map map_cs map_w map_fun map_arrs) <- bindingExp bnd,
    map paramName (lambdaParams scan_fun) == map_arrs = do
      let (accs, arrs) = unzip scan_input
      arrs' <- forM arrs $ \arr -> do
                 t <- lookupType arr
                 let perm = [1,0] ++ [2..arrayRank t-1]
                 letExp (baseString arr) $ PrimOp $ Rearrange [] perm arr
      accs' <- mapM (letExp "acc" . PrimOp . SubExp) accs

      let map_arrs' = accs' ++ arrs'
          (scan_acc_params, scan_elem_params) =
            splitAt (length arrs) $ lambdaParams scan_fun
          map_params = map removeParamOuterDim scan_acc_params ++
                       map (setParamOuterDimTo w) scan_elem_params
          map_rettype = map (setOuterDimTo w) $ lambdaReturnType scan_fun
          map_fun' = Lambda (lambdaIndex map_fun) map_params map_body map_rettype

          scan_params = lambdaParams map_fun
          scan_body = lambdaBody map_fun
          scan_rettype = lambdaReturnType map_fun
          scan_fun' = Lambda (lambdaIndex scan_fun) scan_params scan_body scan_rettype
          scan_input' = map (first Var) $
                        uncurry zip $ splitAt (length arrs') $ map paramName map_params

          map_body = mkBody [Let (setPatternOuterDimTo w $ bindingPattern bnd) () $
                             LoopOp $ Scan cs w scan_fun' scan_input']
                            res

      res_pat' <- liftM (basicPattern' []) $
                  mapM (newIdent' (<>"_transposed") . transposeIdentType) $
                  patternValueIdents res_pat

      transformBinding $ Let res_pat' () $ LoopOp $ Map map_cs map_w map_fun' map_arrs'

      forM_ (zip (patternValueIdents res_pat)
                 (patternValueIdents res_pat')) $ \(to, from) -> do
        let perm = [1,0] ++ [2..arrayRank (identType from)-1]
        addBinding $ Let (basicPattern' [] [to]) () $
                     PrimOp $ Rearrange [] perm $ identName from
  where removeParamOuterDim param =
          let t = rowType $ paramType param
          in param { paramIdent = (paramIdent param) { identType = t } }

        setParamOuterDimTo w param =
          let t = setOuterDimTo w $ paramType param
          in param { paramIdent = (paramIdent param) { identType = t } }

        setIdentOuterDimTo w ident =
          let t = setOuterDimTo w $ identType ident
          in ident { identType = t }

        setOuterDimTo w t =
          arrayOfRow (rowType t) w

        setPatternOuterDimTo w pat =
          basicPattern' [] $ map (setIdentOuterDimTo w) $ patternValueIdents pat

        transposeIdentType ident =
          ident { identType = transposeType $ identType ident }

-- IRWIM to get out the maps.
transformBinding (Let res_pat () (LoopOp op))
  | Reduce cs w red_fun red_input <- op,
    Body () [bnd] res <- lambdaBody red_fun, -- Body has a single binding
    map Var (patternNames $ bindingPattern bnd) == res, -- Returned verbatim
    LoopOp (Map map_cs map_w map_fun map_arrs) <- bindingExp bnd,
    map paramName (lambdaParams red_fun) == map_arrs = do
      let (accs, arrs) = unzip red_input
      arrs' <- forM arrs $ \arr -> do
                 t <- lookupType arr
                 let perm = [1,0] ++ [2..arrayRank t-1]
                 letExp (baseString arr) $ PrimOp $ Rearrange [] perm arr
      accs' <- mapM (letExp "acc" . PrimOp . SubExp) accs

      let map_arrs' = accs' ++ arrs'
          (red_acc_params, red_elem_params) =
            splitAt (length arrs) $ lambdaParams red_fun
          map_params = map removeParamOuterDim red_acc_params ++
                       red_elem_params
          map_rettype = map rowType $ lambdaReturnType red_fun
          map_fun' = Lambda (lambdaIndex map_fun) map_params map_body map_rettype

          red_params = lambdaParams map_fun
          red_body = lambdaBody map_fun
          red_rettype = lambdaReturnType map_fun
          red_fun' = Lambda (lambdaIndex red_fun) red_params red_body red_rettype
          red_input' = map (first Var) $
                       uncurry zip $ splitAt (length arrs') $ map paramName map_params

          map_body = mkBody [Let (stripPatternOuterDim $ bindingPattern bnd) () $
                             LoopOp $ Reduce cs w red_fun' red_input']
                            res

      transformBinding $ Let res_pat () $ LoopOp $ Map map_cs map_w map_fun' map_arrs'
  where removeParamOuterDim param =
          let t = rowType $ paramType param
          in param { paramIdent = (paramIdent param) { identType = t } }

        stripIdentOuterDim ident =
          ident { identType = rowType $ identType ident }

        stripPatternOuterDim pat =
          basicPattern' [] $ map stripIdentOuterDim $ patternValueIdents pat

transformBinding (Let pat () (LoopOp (DoLoop res merge form body))) = do
  body' <- bindingParamTypes (map fst merge) $ bindingIdentTypes form_idents $
           transformBody body
  addBinding $ Let pat () $ LoopOp $ DoLoop res merge form body'
  where form_idents = case form of ForLoop i _ -> [Ident i $ Basic Int]
                                   WhileLoop _ -> []

transformBinding (Let pat () (LoopOp (Stream cs w form lam arrs _)))
  | Just accs <- redForm form = do
  let (body_bnds,res) = sequentialStreamWholeArray w accs lam arrs
      reshapeRes t (Var v)
        | null (arrayDims t) = PrimOp $ SubExp $ Var v
        | otherwise          = shapeCoerce cs (arrayDims t) v
      reshapeRes _ se        = PrimOp $ SubExp se
      res_bnds =
        [ mkLet' [] [ident] $ reshapeRes (identType ident) se
        | (ident,se) <- zip (patternValueIdents pat) res]
  mapM_ transformBinding body_bnds
  shapemap <- shapeMapping (patternValueTypes pat) <$> mapM subExpType res
  forM_ (HM.toList shapemap) $ \(name,se) ->
    when (name `elem` patternContextNames pat) $
      transformBinding =<< mkLetNames' [name] (PrimOp $ SubExp se)
  mapM_ transformBinding res_bnds
  where redForm (RedLike _ _ accs) = Just accs
        redForm (Sequential accs)  = Just accs
        redForm _                  = Nothing

transformBinding (Let pat () e) = do
  e' <- mapExpM transform e
  ((), bnds) <- runBinder $ FOT.transformBinding $ Let pat () e'
  mapM_ addBinding bnds

transform :: Mapper Basic Basic SequentialiseM
transform = identityMapper { mapOnBody = transformBody
                           , mapOnLambda = transformLambda
                           , mapOnExtLambda = transformExtLambda
                           }

transformLambda :: Lambda -> SequentialiseM Lambda
transformLambda lam = do
  body' <- bindingParamTypes (lambdaParams lam) $
           transformBody $ lambdaBody lam
  return lam { lambdaBody = body' }

transformExtLambda :: ExtLambda -> SequentialiseM ExtLambda
transformExtLambda lam = do
  body' <- bindingParamTypes (extLambdaParams lam) $
           transformBody $ extLambdaBody lam
  return lam { extLambdaBody = body' }
