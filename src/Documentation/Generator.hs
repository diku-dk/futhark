
module Documentation.Generator (renderDecs) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Data.Maybe (maybe)
import qualified Data.Map as M
import System.FilePath (normalise)

import Language.Futhark.TypeChecker (FileModule(..))
import Language.Futhark.TypeChecker.Monad
import Language.Futhark

import Text.Blaze.Html5 as H hiding (text, map, main)
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString)

import Documentation.Util

type Context = (String,FileModule)
type DocEnv = M.Map (Namespace,VName) (String)
type DocM = ReaderT Context (State DocEnv)

renderDecs :: [Dec] -> DocM Html
renderDecs decs = asks snd >>= f
  where f fm = mconcat <$> mapM (fmap pre) [ dec | Just dec <- map (prettyDec fm) decs ]

prettyDec :: FileModule -> Dec -> Maybe (DocM Html)
prettyDec fileModule dec = case dec of
  (FunDec f) -> return <$> prettyFun fileModule f
  (SigDec s) -> prettySig fileModule s
  (ModDec m) -> prettyMod fileModule m
  (ValDec v) -> prettyVal fileModule v
  (TypeDec t) -> renderType fileModule t
  (OpenDec _x _xs _names _) -> Nothing
                            --Just $ prettyOpen fileModule (x:xs) names
  (LocalDec _ _) -> Nothing

--prettyOpen :: FileModule -> [ModExpBase Info VName] -> Info [VName] -> DocM Html
--prettyOpen fm xs (Info names) = mconcat <$> mapM (renderModExp fm) xs
--  where FileModule (Env { envModTable = modtable }) = fm
    --envs = foldMap (renderEnv . (\(ModEnv e) -> e) . (modtable M.!)) names

prettyFun :: FileModule -> FunBindBase t VName -> Maybe Html
prettyFun fm (FunBind _ name _retdecl _rettype _tparams _args _ _ doc)
  | Just (BoundF (tps,pts,rett)) <- M.lookup name vtable
  , visible Term name fm = Just $
    renderDoc doc <> toHtml "val " <> vnameHtml name <>
    foldMap (toHtml " " <>) (map prettyTypeParam tps) <> toHtml ": " <>
    foldMap (\t -> prettyType t <> toHtml " -> ") pts <> prettyType rett
    where FileModule (Env {envVtable = vtable}) = fm
prettyFun _ _ = Nothing

prettyVal :: FileModule -> ValBindBase Info VName -> Maybe (DocM Html)
prettyVal fm (ValBind _entry name maybe_t _ _e _ doc)
  | Just (BoundV st) <- M.lookup name vtable
  , visible Term name fm =
    Just . return . H.div $
    renderDoc doc <> toHtml "let " <> vnameHtml name <> toHtml " : " <>
    maybe (prettyType st) typeExpHtml maybe_t
    where (FileModule (Env {envVtable = vtable})) = fm
prettyVal _ _ = Nothing

prettySig :: FileModule -> SigBindBase Info VName -> Maybe (DocM Html)
prettySig fm (SigBind vname se _ doc)
  | M.member vname sigtable && visible Signature vname fm =
    Just $ H.div <$> do
      name <- vnameHtmlM Signature vname
      expHtml <- renderSigExp se
      return $ renderDoc doc <> toHtml "module type " <> name <> toHtml " = " <> expHtml
    where (FileModule (Env { envSigTable = sigtable })) = fm
prettySig _ _ = Nothing

prettyMod :: FileModule -> ModBindBase Info VName -> Maybe (DocM Html)
prettyMod fm (ModBind name ps sig _me _)
  | Just env <- M.lookup name modtable
  , visible Structure name fm = Just $ pre <$> do
    vname <- vnameHtmlM Structure name
    params <- modParamHtml ps
    s <- case sig of Nothing -> envSig env
                     Just (s,_) -> renderSigExp s
    return $ toHtml "module " <> vname <> toHtml ": " <> params <> s
    where FileModule (Env { envModTable = modtable}) = fm
          envSig (ModEnv e) = renderEnv e
          envSig (ModFun (FunSig _ _ (MTy _ m))) = envSig m
prettyMod _ _ = Nothing

renderType :: FileModule -> TypeBindBase Info VName -> Maybe (DocM Html)
renderType fm tb
  | M.member name typeTable
  , visible Type name fm = Just $ H.div <$> typeBindHtml tb
    where (FileModule (Env {envTypeTable = typeTable})) = fm
          (TypeBind { typeAlias = name }) = tb
renderType _ _ = Nothing

_ = let 
  renderModExp :: FileModule -> ModExpBase Info VName -> DocM Html
  renderModExp fm e = case e of
    ModVar v _ -> renderQualName Structure v
    ModParens e' _ -> parens <$> renderModExp fm e'
    ModImport v _ -> return $ toHtml $ show v
    ModDecs ds _ -> renderDecs ds --nestedBlock "{" "}" (stack $ punctuate line $ map ppr ds)
    ModApply _f _a _ _ _ -> return mempty --parens $ ppr f <+> parens (ppr a)
    ModAscript _me se _ _ -> (toHtml "{...}: " <>) <$> renderSigExp se--ppr me <> colon <+> ppr se
    ModLambda _param _maybe_sig _body _ -> error "It should not be possible to open a lambda"
  in renderModExp

visible :: Namespace -> VName -> FileModule -> Bool
visible ns vname@(VName name _) (FileModule env)
  | Just vname' <- M.lookup (ns,name) (envNameMap env)
  = vname == vname'
visible _ _ _ = False

renderDoc :: ToMarkup a => Maybe a -> Html
renderDoc (Just doc) = H.div ! A.style (fromString "padding-left: 2em") $ toHtml $ doc
renderDoc Nothing = mempty

renderEnv :: Env -> DocM Html
renderEnv (Env vtable ttable sigtable modtable _) =
  return $ braces (mconcat specs)
  where specs = typeBinds ++ valBinds ++ sigBinds ++ modBinds
        typeBinds = map renderTypeBind (M.toList ttable)
        valBinds = map renderValBind (M.toList vtable)
        sigBinds = map renderModType (M.toList sigtable)
        modBinds = map renderMod (M.toList modtable)

renderModType :: (VName, MTy) -> Html
renderModType (name, _sig) =
  toHtml "module type " <> vnameHtml name

renderMod :: (VName, Mod) -> Html
renderMod (name, _mod) =
  toHtml "module " <> vnameHtml name

renderValBind :: (VName, ValBinding) -> Html
renderValBind = H.div . prettyValBind

renderTypeBind :: (VName, TypeBinding) -> Html
renderTypeBind (name, TypeAbbr tps tp) =
  H.div $ typeHtml name tps <> prettyType tp

prettyValBind :: (VName, ValBinding) -> Html
prettyValBind (name, (BoundF (tps, pts, rettype))) =
  toHtml "val " <> vnameHtml name <> foldMap (toHtml " " <>) (map prettyTypeParam tps) <> toHtml ": " <>
  foldMap (\t -> prettyType t <> toHtml " -> ") pts <> toHtml " " <> prettyType rettype
prettyValBind (name, BoundV t) = toHtml "val " <> vnameHtml name <> toHtml " : " <> prettyType t

prettyType :: StructType -> Html
prettyType t = case t of
  Prim et -> primTypeHtml et
  Record fs
    | Just ts <- areTupleFields fs ->
        parens $ commas (map prettyType ts)
    | otherwise ->
        braces $ commas (map ppField $ M.toList fs)
    where ppField (name, tp) = toHtml (nameToString name) <> toHtml ":" <> prettyType tp
  TypeVar et targs ->
    prettyTypeName et <> foldMap ((<> toHtml " ") . prettyTypeArg) targs
  Array arr -> prettyArray arr

prettyArray :: ArrayTypeBase (ShapeDecl VName) () -> Html
prettyArray arr = case arr of
  PrimArray et (ShapeDecl ds) u _ ->
    prettyU u <> foldMap (brackets . prettyD) ds <> primTypeHtml et 
  PolyArray et targs shape u _ ->
    prettyU u <> prettyShapeDecl shape <> prettyTypeName et <>
    foldMap (<> toHtml " ") (map prettyTypeArg targs)
  RecordArray fs shape u
    | Just ts <- areTupleFields fs ->
        prefix <> parens (commas $ map prettyElem ts)
    | otherwise ->
        prefix <> braces (commas $ map ppField $ M.toList fs)
    where prefix = prettyU u <> prettyShapeDecl shape
          ppField (name, tp) = toHtml (nameToString name) <> toHtml ":" <> prettyElem tp

prettyElem :: RecordArrayElemTypeBase (ShapeDecl VName) () -> Html
prettyElem e = case e of
  PrimArrayElem bt _ u -> prettyU u <> primTypeHtml bt
  PolyArrayElem bt targs _ u ->
    prettyU u <> prettyTypeName  bt <> foldMap (toHtml " " <>) (map prettyTypeArg targs)
  ArrayArrayElem at -> prettyArray at
  RecordArrayElem fs
    | Just ts <- areTupleFields fs
      -> parens $ commas $ map prettyElem ts
    | otherwise
      -> braces . commas $ map ppField $ M.toList fs
    where ppField (name, t) = toHtml (nameToString name) <> toHtml ":" <> prettyElem t

prettyShapeDecl :: ShapeDecl VName -> Html
prettyShapeDecl (ShapeDecl ds) =
  foldMap (brackets . prettyDimDecl) ds

prettyTypeArg :: TypeArg (ShapeDecl VName) () -> Html
prettyTypeArg (TypeArgDim d _) = brackets $ prettyDimDecl d
prettyTypeArg (TypeArgType t _) = prettyType t

modParamHtml :: [ModParamBase Info VName] -> DocM Html
modParamHtml [] = return mempty
modParamHtml ((ModParam pname psig _):mps) = liftM2 f (renderSigExp psig) (modParamHtml mps)
  where f se params = toHtml "(" <> vnameHtml pname <> toHtml ": " <> se <> toHtml ") -> " <>
                      params

prettyD :: DimDecl VName -> Html
prettyD (NamedDim v) = prettyQualName v
prettyD (BoundDim _) = mempty
prettyD (ConstDim _) = mempty
prettyD AnyDim = mempty

renderSigExp :: SigExpBase Info VName -> DocM Html
renderSigExp e = case e of
  (SigVar v _) -> renderQualName Signature v
  (SigParens e' _) -> parens <$> renderSigExp e'
  (SigSpecs ss _) -> braces . mconcat <$> mapM specHtml ss
  (SigWith s (TypeRef v t) _) ->
    do e' <- renderSigExp s
       --name <- renderQualName Type v
       return $ e' <> toHtml " with " <> prettyQualName v <> toHtml " = "  <> typeDeclHtml t
  (SigArrow Nothing e1 e2 _) ->
    liftM2 f (renderSigExp e1) (renderSigExp e2)
    where f e1' e2' = e1' <> toHtml " -> " <> e2'
  (SigArrow (Just v) e1 e2 _) ->
    do name <- vnameHtmlM Signature v
       e1' <- renderSigExp e1
       e2' <- renderSigExp e2
       return $ toHtml "(" <> name <> toHtml ": " <> e1' <> toHtml ") -> " <> e2'

vnameHtml :: VName -> Html
vnameHtml (VName name tag) = 
    H.span ! A.id (fromString (show tag)) $ renderName name

vnameHtmlM :: Namespace -> VName -> DocM Html
vnameHtmlM ns (VName name tag) = 
    do file <- asks fst
       modify (M.insert (ns,VName name tag) file)
       return $ H.span ! A.id (fromString (show tag)) $ renderName name

specHtml :: SpecBase Info VName -> DocM Html
specHtml spec = case spec of
  (TypeAbbrSpec tpsig) -> H.div <$> typeBindHtml tpsig
  (TypeSpec name ps _ doc) -> return . H.div $
    renderDoc doc <> toHtml "type " <> vnameHtml name <>
    joinBy (toHtml " ") (map prettyTypeParam ps)
  (ValSpec name tparams params rettype _ doc) -> return . H.div $
    renderDoc doc <>
    toHtml "val " <> vnameHtml name <>
    foldMap (toHtml " " <>) (map prettyTypeParam tparams) <>
    toHtml " : " <> foldMap (\tp -> paramBaseHtml tp <> toHtml " -> ") params <>
    typeDeclHtml rettype
  (ModSpec name sig _) ->
    do m <- vnameHtmlM Structure name
       s <- renderSigExp sig
       return $ toHtml "module " <> m <> toHtml ": "<> s
  (IncludeSpec e _) -> H.div . (toHtml "include " <>) <$> renderSigExp e

paramBaseHtml :: ParamBase Info VName -> Html
paramBaseHtml (NamedParam v t _) = parens $ vnameHtml v <> toHtml ": " <> typeDeclHtml t
paramBaseHtml (UnnamedParam t) = typeDeclHtml t

typeDeclHtml :: TypeDeclBase f VName -> Html
typeDeclHtml = typeExpHtml . declaredType

typeExpHtml :: TypeExp VName -> Html
typeExpHtml e = case e of
  (TEUnique t _ ) -> toHtml "*" >> typeExpHtml t
  (TEArray at d _) -> brackets (prettyDimDecl d) <> typeExpHtml at
  (TETuple ts _) -> parens $ commas (map typeExpHtml ts)
  (TERecord fs _) -> braces $ commas (map ppField fs)
    where ppField (name, t) = toHtml (nameToString name) <> toHtml "=" <> typeExpHtml t
  (TEVar name  _) -> qualNameHtml name
  (TEApply t args _) -> qualNameHtml t <> foldMap (toHtml " " <>) (map prettyTypeArgExp args)

qualNameHtml :: QualName VName -> Html
qualNameHtml (QualName names (VName name tag)) =
  if tag <= maxIntrinsicTag
      then prefix <> renderName name
      else prefix <> (a ! A.href (fromString ("#" ++ show tag)) $ renderName name)
  where prefix :: Html
        prefix = foldMap ((<> toHtml ".") . renderName) names

renderQualName :: Namespace -> QualName VName -> DocM Html
renderQualName ns (QualName names (VName name tag)) =
  if tag <= maxIntrinsicTag
      then return $ prefix <> renderName name
      else f <$> ref
  where prefix :: Html
        prefix = mapM_ ((<> toHtml ".") . renderName) names
        f s = prefix <> (a ! A.href (fromString s) $ renderName name)
        trunc ('/':xs) = xs
        trunc xs = xs
        --link = do (file,vname) <- gets (M.! (ns,QualName names name))
        ref = do --vname <- getVName ns (QualName names name)
                 Just file <- gets (M.lookup (ns, VName name tag))
                 current <- asks fst
                 if file == current
                     then return ("#" ++ show tag)
                     else return $ relativise (trunc file) (trunc current) ++ ".html#" ++ show tag

relativise :: FilePath -> FilePath -> FilePath
relativise dest src = concat (replicate (length $ filter (== '/') src') "../") ++ dest'
  where (dest',src') = unzip $ dropWhile (uncurry (==)) (zip (normalise dest) (normalise src))

--getVName :: Namespace -> QualName Name -> DocM VName
--getVName ns (QualName names name) = do
--  (FileModule env)  <- asks snd
--  let nm = envNameMap (go names env)
--      Just vname = M.lookup (ns,name) nm
--  return vname
--  --return . (M.! (ns,name)) . envNameMap $ go names env
--  where go [] e = e
--        go (x:xs) e = go xs (f x e) --  $ f $ envModTable e M.! (envNameMap e M.! (Structure,x))
--        --f (ModEnv env) = env
--        f x e | Just y <- M.lookup (Structure,x) (envNameMap e)
--              , Just (ModEnv env) <- M.lookup y (envModTable e)
--              = env

prettyDimDecl :: DimDecl VName -> Html
prettyDimDecl AnyDim = mempty
prettyDimDecl (NamedDim v) = prettyQualName v
prettyDimDecl (BoundDim v) = toHtml "#" <> vnameHtml v
prettyDimDecl (ConstDim n) = toHtml (show n)

prettyTypeArgExp :: TypeArgExp VName -> Html
prettyTypeArgExp (TypeArgExpDim d _) = prettyDimDecl d
prettyTypeArgExp (TypeArgExpType d) = typeExpHtml d

prettyTypeParam :: TypeParam -> Html
prettyTypeParam (TypeParamDim name _) = brackets $ vnameHtml name
prettyTypeParam (TypeParamType name _) = toHtml "'" <> vnameHtml name

typeBindHtml :: TypeBindBase Info VName -> DocM Html
typeBindHtml (TypeBind name params usertype _ doc) =
    return $ renderDoc doc <> typeHtml name params <> typeDeclHtml usertype

typeHtml :: VName -> [TypeParam] -> Html
typeHtml name params =
  toHtml "type " <> vnameHtml name <>
  joinBy (toHtml " ") (map prettyTypeParam params) <>
  toHtml " = "
