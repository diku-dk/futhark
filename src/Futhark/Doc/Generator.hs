-- | The core logic of @futhark doc@.
module Futhark.Doc.Generator (renderFiles) where

import CMarkGFM qualified as GFM
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer (Writer, WriterT, runWriter, runWriterT, tell)
import Data.Bifunctor (second)
import Data.Char (isAlpha, isSpace, toUpper)
import Data.List (find, groupBy, inits, intersperse, isPrefixOf, partition, sort, sortOn, tails)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text qualified as T
import Data.Version
import Futhark.Util.Pretty (Doc, docText, pretty)
import Futhark.Version
import Language.Futhark
import Language.Futhark.Semantic
import Language.Futhark.TypeChecker.Monad hiding (warn)
import System.FilePath (makeRelative, splitPath, (-<.>), (</>))
import Text.Blaze.Html5 (AttributeValue, Html, toHtml, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Prelude hiding (abs)

docToHtml :: Doc a -> Html
docToHtml = toHtml . docText

primTypeHtml :: PrimType -> Html
primTypeHtml = docToHtml . pretty

prettyU :: Uniqueness -> Html
prettyU = docToHtml . pretty

renderName :: Name -> Html
renderName name = docToHtml (pretty name)

joinBy :: Html -> [Html] -> Html
joinBy _ [] = mempty
joinBy _ [x] = x
joinBy sep (x : xs) = x <> foldMap (sep <>) xs

commas :: [Html] -> Html
commas = joinBy ", "

parens :: Html -> Html
parens x = "(" <> x <> ")"

braces :: Html -> Html
braces x = "{" <> x <> "}"

brackets :: Html -> Html
brackets x = "[" <> x <> "]"

pipes :: [Html] -> Html
pipes = joinBy " | "

-- | A set of names that we should not generate links to, because they
-- are uninteresting.  These are for example type parameters.
type NoLink = S.Set VName

data Context = Context
  { ctxCurrent :: String,
    ctxFileMod :: FileModule,
    ctxImports :: Imports,
    ctxNoLink :: NoLink,
    ctxFileMap :: FileMap,
    -- | Local module types that show up in the
    -- interface.  These should be documented,
    -- but clearly marked local.
    ctxVisibleMTys :: S.Set VName
  }

type FileMap = M.Map VName (FilePath, Namespace)

type DocM = ReaderT Context (WriterT Documented (Writer Warnings))

data IndexWhat = IndexValue | IndexFunction | IndexModule | IndexModuleType | IndexType

-- | We keep a mapping of the names we have actually documented, so we
-- can generate an index.
type Documented = M.Map VName IndexWhat

warn :: SrcLoc -> Doc () -> DocM ()
warn loc s = lift $ lift $ tell $ singleWarning loc s

document :: VName -> IndexWhat -> DocM ()
document v what = tell $ M.singleton v what

noLink :: [VName] -> DocM a -> DocM a
noLink names = local $ \ctx ->
  ctx {ctxNoLink = S.fromList names <> ctxNoLink ctx}

selfLink :: AttributeValue -> Html -> Html
selfLink s = H.a ! A.id s ! A.href ("#" <> s) ! A.class_ "self_link"

fullRow :: Html -> Html
fullRow = H.tr . (H.td ! A.colspan "3")

emptyRow :: Html
emptyRow = H.tr $ H.td mempty <> H.td mempty <> H.td mempty

specRow :: Html -> Html -> Html -> Html
specRow a b c =
  H.tr $
    (H.td ! A.class_ "spec_lhs") a
      <> (H.td ! A.class_ "spec_eql") b
      <> (H.td ! A.class_ "spec_rhs") c

vnameToFileMap :: Imports -> FileMap
vnameToFileMap = mconcat . map forFile
  where
    forFile (file, FileModule abs file_env _prog _) =
      mconcat (map (vname Type) (M.keys abs))
        <> forEnv file_env
      where
        file' = makeRelative "/" $ includeToFilePath file
        vname ns v = M.singleton (qualLeaf v) (file', ns)
        vname' ((ns, _), v) = vname ns v

        forEnv env =
          mconcat (map vname' $ M.toList $ envNameMap env)
            <> mconcat (map forMty $ M.elems $ envModTypeTable env)
        forMod (ModEnv env) = forEnv env
        forMod ModFun {} = mempty
        forMty = forMod . mtyMod

-- | @renderFiles important_imports imports@ produces HTML files
-- documenting the type-checked program @imports@, with the files in
-- @important_imports@ considered most important.  The HTML files must
-- be written to the specific locations indicated in the return value,
-- or the relative links will be wrong.
renderFiles :: [ImportName] -> Imports -> ([(FilePath, Html)], Warnings)
renderFiles important_imports imports = runWriter $ do
  (import_pages, documented) <- runWriterT $
    forM imports $ \(current, fm) ->
      let ctx =
            Context
              { ctxCurrent = makeRelative "/" $ includeToFilePath current,
                ctxFileMod = fm,
                ctxImports = imports,
                ctxNoLink = mempty,
                ctxFileMap = file_map,
                ctxVisibleMTys = progModuleTypes $ fileProg fm
              }
       in flip runReaderT ctx $ do
            (first_paragraph, maybe_abstract, maybe_sections) <- headerDoc $ fileProg fm

            synopsis <- (H.div ! A.id "module") <$> synopsisDecs (progDecs $ fileProg fm)

            description <- describeDecs $ progDecs $ fileProg fm

            pure
              ( current,
                ( H.docTypeHtml ! A.lang "en"
                    $ addBoilerplateWithNav
                      important_imports
                      imports
                      ("doc" </> includeToFilePath current)
                      (includeToString current)
                    $ H.main
                    $ maybe_abstract
                      <> selfLink "synopsis" (H.h2 "Synopsis")
                      <> (H.div ! A.id "overview") synopsis
                      <> selfLink "description" (H.h2 "Description")
                      <> description
                      <> maybe_sections,
                  first_paragraph
                )
              )

  pure $
    [ ("index.html", contentsPage important_imports $ map (fmap snd) import_pages),
      ("doc-index.html", indexPage important_imports imports documented file_map)
    ]
      ++ map (importHtml *** fst) import_pages
  where
    file_map = vnameToFileMap imports
    importHtml import_name =
      "doc" </> makeRelative "/" (fromString (includeToString import_name)) -<.> "html"

-- | The header documentation (which need not be present) can contain
-- an abstract and further sections.
headerDoc :: Prog -> DocM (Html, Html, Html)
headerDoc prog =
  case progDoc prog of
    Just (DocComment doc loc) -> do
      let (abstract, more_sections) = splitHeaderDoc $ T.unpack doc
      first_paragraph <- docHtml $ Just $ DocComment (firstParagraph abstract) loc
      abstract' <- docHtml $ Just $ DocComment (T.pack abstract) loc
      more_sections' <- docHtml $ Just $ DocComment (T.pack more_sections) loc
      pure
        ( first_paragraph,
          selfLink "abstract" (H.h2 "Abstract") <> abstract',
          more_sections'
        )
    _ -> pure mempty
  where
    splitHeaderDoc s =
      fromMaybe (s, mempty) $
        find (("\n##" `isPrefixOf`) . snd) $
          zip (inits s) (tails s)
    firstParagraph = T.pack . unlines . takeWhile (not . paragraphSeparator) . lines
    paragraphSeparator = all isSpace

contentsPage :: [ImportName] -> [(ImportName, Html)] -> Html
contentsPage important_imports pages =
  H.docTypeHtml $
    addBoilerplate "index.html" "Futhark Library Documentation" $
      H.main $
        ( if null important_pages
            then mempty
            else H.h2 "Main libraries" <> fileList important_pages
        )
          <> ( if null unimportant_pages
                 then mempty
                 else H.h2 "Supporting libraries" <> fileList unimportant_pages
             )
  where
    (important_pages, unimportant_pages) =
      partition ((`elem` important_imports) . fst) pages

    fileList pages' =
      H.dl ! A.class_ "file_list" $
        mconcat $
          map linkTo $
            sortOn fst pages'

    linkTo (name, maybe_abstract) =
      H.div ! A.class_ "file_desc" $
        (H.dt ! A.class_ "desc_header") (importLink "index.html" name)
          <> (H.dd ! A.class_ "desc_doc") maybe_abstract

importLink :: FilePath -> ImportName -> Html
importLink current name =
  let file =
        relativise
          ("doc" </> makeRelative "/" (includeToFilePath name) -<.> "html")
          current
   in (H.a ! A.href (fromString file) $ fromString (includeToString name))

indexPage :: [ImportName] -> Imports -> Documented -> FileMap -> Html
indexPage important_imports imports documented fm =
  H.docTypeHtml $
    addBoilerplateWithNav important_imports imports "doc-index.html" "Index" $
      H.main $
        ( H.ul ! A.id "doc_index_list" $
            mconcat $
              map initialListEntry $
                letter_group_links ++ [symbol_group_link]
        )
          <> ( H.table ! A.id "doc_index" $
                 H.thead (H.tr $ H.td "Who" <> H.td "What" <> H.td "Where")
                   <> mconcat (letter_groups ++ [symbol_group])
             )
  where
    (letter_names, sym_names) =
      partition (isLetterName . baseString . fst) $
        sortOn (map toUpper . baseString . fst) $
          mapMaybe isDocumented $
            M.toList fm

    isDocumented (k, (file, _)) = do
      what <- M.lookup k documented
      Just (k, (file, what))

    (letter_groups, letter_group_links) =
      unzip $ map tbodyForNames $ groupBy sameInitial letter_names
    (symbol_group, symbol_group_link) =
      tbodyForInitial "Symbols" sym_names

    isLetterName [] = False
    isLetterName (c : _) = isAlpha c

    sameInitial (x, _) (y, _) =
      case (baseString x, baseString y) of
        (x' : _, y' : _) -> toUpper x' == toUpper y'
        _ -> False

    tbodyForNames names@((s, _) : _) =
      tbodyForInitial (map toUpper $ take 1 $ baseString s) names
    tbodyForNames _ = mempty

    tbodyForInitial initial names =
      ( H.tbody $ mconcat $ initial' : map linkTo names,
        initial
      )
      where
        initial' =
          H.tr
            $ H.td ! A.colspan "2" ! A.class_ "doc_index_initial"
            $ H.a
              ! A.id (fromString initial)
              ! A.href (fromString $ '#' : initial)
            $ fromString initial

    initialListEntry initial =
      H.li $ H.a ! A.href (fromString $ '#' : initial) $ fromString initial

    linkTo (name, (file, what)) =
      let file' = makeRelative "/" file
          link =
            (H.a ! A.href (fromString (makeRelative "/" $ "doc" </> vnameLink' name "" file'))) $
              fromString $
                baseString name
          what' = case what of
            IndexValue -> "value"
            IndexFunction -> "function"
            IndexType -> "type"
            IndexModuleType -> "module type"
            IndexModule -> "module"
          html_file = "doc" </> file' -<.> "html"
       in H.tr $
            (H.td ! A.class_ "doc_index_name" $ link)
              <> (H.td ! A.class_ "doc_index_namespace" $ what')
              <> ( H.td ! A.class_ "doc_index_file" $
                     (H.a ! A.href (fromString html_file) $ fromString file)
                 )

addBoilerplate :: String -> String -> Html -> Html
addBoilerplate current titleText content =
  let headHtml =
        H.head $
          H.meta
            ! A.charset "utf-8"
            <> H.title (fromString titleText)
            <> H.link
              ! A.href (fromString $ relativise "style.css" current)
              ! A.rel "stylesheet"
              ! A.type_ "text/css"

      navigation =
        H.ul ! A.id "navigation" $
          H.li (H.a ! A.href (fromString $ relativise "index.html" current) $ "Contents")
            <> H.li (H.a ! A.href (fromString $ relativise "doc-index.html" current) $ "Index")

      madeByHtml =
        "Generated by "
          <> (H.a ! A.href futhark_doc_url) "futhark-doc"
          <> " "
          <> fromString (showVersion version)
   in headHtml
        <> H.body
          ( (H.div ! A.id "header") (H.h1 (toHtml titleText) <> navigation)
              <> (H.div ! A.id "content") content
              <> (H.div ! A.id "footer") madeByHtml
          )
  where
    futhark_doc_url =
      "https://futhark.readthedocs.io/en/latest/man/futhark-doc.html"

addBoilerplateWithNav :: [ImportName] -> Imports -> String -> String -> Html -> Html
addBoilerplateWithNav important_imports imports current titleText content =
  addBoilerplate current titleText $
    (H.nav ! A.id "filenav" $ files) <> content
  where
    files = H.ul $ mconcat $ map pp $ sort $ filter visible important_imports
    pp name = H.li $ importLink current name
    visible = (`elem` map fst imports)

synopsisDecs :: [Dec] -> DocM Html
synopsisDecs decs = do
  visible <- asks ctxVisibleMTys
  fm <- asks ctxFileMod
  -- We add an empty row to avoid generating invalid HTML in cases
  -- where all rows are otherwise colspan=2.
  (H.table ! A.class_ "specs") . (emptyRow <>) . mconcat
    <$> sequence (mapMaybe (synopsisDec visible fm) decs)

synopsisDec :: S.Set VName -> FileModule -> Dec -> Maybe (DocM Html)
synopsisDec visible fm dec = case dec of
  ModTypeDec s -> synopsisModType mempty s
  ModDec m -> synopsisMod fm m
  ValDec v -> synopsisValBind v
  TypeDec t -> synopsisType t
  OpenDec x _
    | Just opened <- synopsisOpened x -> Just $ do
        opened' <- opened
        pure $ fullRow $ keyword "open " <> opened'
    | otherwise ->
        Just $
          pure $
            fullRow $
              keyword "open" <> fromString (" <" <> prettyString x <> ">")
  LocalDec (ModTypeDec s) _
    | modTypeName s `S.member` visible ->
        synopsisModType (keyword "local" <> " ") s
  LocalDec {} -> Nothing
  ImportDec {} -> Nothing

synopsisOpened :: ModExp -> Maybe (DocM Html)
synopsisOpened (ModVar qn _) = Just $ qualNameHtml qn
synopsisOpened (ModParens me _) = do
  me' <- synopsisOpened me
  Just $ parens <$> me'
synopsisOpened (ModImport _ (Info file) _) = Just $ do
  current <- asks ctxCurrent
  let dest = fromString $ relativise (includeToFilePath file) current -<.> "html"
  pure $
    keyword "import "
      <> (H.a ! A.href dest) (fromString (show (includeToString file)))
synopsisOpened (ModAscript _ se _ _) = Just $ do
  se' <- synopsisModTypeExp se
  pure $ "... : " <> se'
synopsisOpened _ = Nothing

synopsisValBind :: ValBind -> Maybe (DocM Html)
synopsisValBind vb = Just $ do
  let name' = vnameSynopsisDef $ valBindName vb
  (lhs, mhs, rhs) <- valBindHtml name' vb
  pure $ specRow lhs (mhs <> " : ") rhs

valBindHtml :: Html -> ValBind -> DocM (Html, Html, Html)
valBindHtml name (ValBind _ _ retdecl (Info rettype) tparams params _ _ _ _) = do
  let tparams' = mconcat $ map ((" " <>) . typeParamHtml) tparams
      noLink' =
        noLink $ map typeParamName tparams <> foldMap patNames params
  rettype' <- noLink' $ maybe (retTypeHtml rettype) typeExpHtml retdecl
  params' <- noLink' $ mapM paramHtml params
  pure
    ( keyword "val " <> (H.span ! A.class_ "decl_name") name,
      tparams',
      mconcat (intersperse " -> " $ params' ++ [rettype'])
    )

synopsisModType :: Html -> ModTypeBind -> Maybe (DocM Html)
synopsisModType prefix sb = Just $ do
  let name' = vnameSynopsisDef $ modTypeName sb
  fullRow <$> do
    se' <- synopsisModTypeExp $ modTypeExp sb
    pure $ prefix <> keyword "module type " <> name' <> " = " <> se'

synopsisMod :: FileModule -> ModBind -> Maybe (DocM Html)
synopsisMod fm (ModBind name ps sig _ _ _) =
  case sig of
    Nothing -> (proceed <=< envModType) <$> M.lookup name modtable
    Just (s, _) -> Just $ proceed =<< synopsisModTypeExp s
  where
    proceed sig' = do
      let name' = vnameSynopsisDef name
      ps' <- modParamHtml ps
      pure $ specRow (keyword "module " <> name') ": " (ps' <> sig')

    FileModule _abs Env {envModTable = modtable} _ _ = fm
    envModType (ModEnv e) = renderEnv e
    envModType (ModFun (FunModType _ _ (MTy _ m))) = envModType m

synopsisType :: TypeBind -> Maybe (DocM Html)
synopsisType tb = Just $ do
  let name' = vnameSynopsisDef $ typeAlias tb
  fullRow <$> typeBindHtml name' tb

typeBindHtml :: Html -> TypeBind -> DocM Html
typeBindHtml name' (TypeBind _ l tparams t _ _ _) = do
  t' <- noLink (map typeParamName tparams) $ typeExpHtml t
  pure $ typeAbbrevHtml l name' tparams <> " = " <> t'

renderEnv :: Env -> DocM Html
renderEnv (Env vtable ttable sigtable modtable _) = do
  typeBinds <- mapM renderTypeBind (M.toList ttable)
  valBinds <- mapM renderValBind (M.toList vtable)
  sigBinds <- mapM renderModType (M.toList sigtable)
  modBinds <- mapM renderMod (M.toList modtable)
  pure $ braces $ mconcat $ typeBinds ++ valBinds ++ sigBinds ++ modBinds

renderModType :: (VName, MTy) -> DocM Html
renderModType (name, _sig) =
  (keyword "module type " <>) <$> qualNameHtml (qualName name)

renderMod :: (VName, Mod) -> DocM Html
renderMod (name, _mod) =
  (keyword "module " <>) <$> qualNameHtml (qualName name)

renderValBind :: (VName, BoundV) -> DocM Html
renderValBind = fmap H.div . synopsisValBindBind

renderTypeBind :: (VName, TypeBinding) -> DocM Html
renderTypeBind (name, TypeAbbr l tps tp) = do
  tp' <- retTypeHtml $ toResRet Nonunique tp
  pure $ H.div $ typeAbbrevHtml l (vnameHtml name) tps <> " = " <> tp'

synopsisValBindBind :: (VName, BoundV) -> DocM Html
synopsisValBindBind (name, BoundV tps t) = do
  let tps' = map typeParamHtml tps
  t' <- typeHtml $ second (const Nonunique) t
  pure $
    keyword "val "
      <> vnameHtml name
      <> mconcat (map (" " <>) tps')
      <> ": "
      <> t'

dietHtml :: Diet -> Html
dietHtml Consume = "*"
dietHtml Observe = ""

typeHtml :: TypeBase Size Uniqueness -> DocM Html
typeHtml t = case t of
  Array u shape et -> do
    shape' <- prettyShape shape
    et' <- typeHtml $ Scalar $ second (const Nonunique) et
    pure $ prettyU u <> shape' <> et'
  Scalar (Prim et) -> pure $ primTypeHtml et
  Scalar (Record fs)
    | Just ts <- areTupleFields fs ->
        parens . commas <$> mapM typeHtml ts
    | otherwise ->
        braces . commas <$> mapM ppField (M.toList fs)
    where
      ppField (name, tp) = do
        tp' <- typeHtml tp
        pure $ toHtml (nameToString name) <> ": " <> tp'
  Scalar (TypeVar u et targs) -> do
    targs' <- mapM typeArgHtml targs
    et' <- qualNameHtml et
    pure $ prettyU u <> et' <> mconcat (map (" " <>) targs')
  Scalar (Arrow _ pname d t1 t2) -> do
    t1' <- typeHtml $ second (const Nonunique) t1
    t2' <- retTypeHtml t2
    pure $ case pname of
      Named v ->
        parens (vnameHtml v <> ": " <> dietHtml d <> t1') <> " -> " <> t2'
      Unnamed ->
        dietHtml d <> t1' <> " -> " <> t2'
  Scalar (Sum cs) -> pipes <$> mapM ppClause (sortConstrs cs)
    where
      ppClause (n, ts) = joinBy " " . (ppConstr n :) <$> mapM typeHtml ts
      ppConstr name = "#" <> toHtml (nameToString name)

retTypeHtml :: ResRetType -> DocM Html
retTypeHtml (RetType [] t) = typeHtml t
retTypeHtml (RetType dims t) = do
  t' <- typeHtml t
  pure $ "?" <> mconcat (map (brackets . vnameHtml) dims) <> "." <> t'

prettyShape :: Shape Size -> DocM Html
prettyShape (Shape ds) =
  mconcat <$> mapM dimDeclHtml ds

typeArgHtml :: TypeArg Size -> DocM Html
typeArgHtml (TypeArgDim d) = dimDeclHtml d
typeArgHtml (TypeArgType t) = typeHtml $ second (const Nonunique) t

modParamHtml :: [ModParamBase Info VName] -> DocM Html
modParamHtml [] = pure mempty
modParamHtml (ModParam pname psig _ _ : mps) =
  liftM2 f (synopsisModTypeExp psig) (modParamHtml mps)
  where
    f se params =
      "("
        <> vnameHtml pname
        <> ": "
        <> se
        <> ") -> "
        <> params

synopsisModTypeExp :: ModTypeExpBase Info VName -> DocM Html
synopsisModTypeExp e = case e of
  ModTypeVar v _ _ -> qualNameHtml v
  ModTypeParens e' _ -> parens <$> synopsisModTypeExp e'
  ModTypeSpecs ss _ -> braces . (H.table ! A.class_ "specs") . mconcat <$> mapM synopsisSpec ss
  ModTypeWith s (TypeRef v ps t _) _ -> do
    s' <- synopsisModTypeExp s
    t' <- typeExpHtml t
    v' <- qualNameHtml v
    let ps' = mconcat $ map ((" " <>) . typeParamHtml) ps
    pure $ s' <> keyword " with " <> v' <> ps' <> " = " <> t'
  ModTypeArrow Nothing e1 e2 _ ->
    liftM2 f (synopsisModTypeExp e1) (synopsisModTypeExp e2)
    where
      f e1' e2' = e1' <> " -> " <> e2'
  ModTypeArrow (Just v) e1 e2 _ ->
    do
      let name = vnameHtml v
      e1' <- synopsisModTypeExp e1
      e2' <- noLink [v] $ synopsisModTypeExp e2
      pure $ "(" <> name <> ": " <> e1' <> ") -> " <> e2'

keyword :: String -> Html
keyword = (H.span ! A.class_ "keyword") . fromString

vnameHtml :: VName -> Html
vnameHtml (VName name tag) =
  H.span ! A.id (fromString (show tag)) $ renderName name

vnameDescDef :: VName -> IndexWhat -> DocM Html
vnameDescDef v what = do
  document v what
  pure $ H.a ! A.id (fromString (show (baseTag v))) $ renderName (baseName v)

vnameSynopsisDef :: VName -> Html
vnameSynopsisDef (VName name tag) =
  H.span ! A.id (fromString (show tag ++ "s")) $
    H.a ! A.href (fromString ("#" ++ show tag)) $
      renderName name

vnameSynopsisRef :: VName -> Html
vnameSynopsisRef v =
  H.a
    ! A.class_ "synopsis_link"
    ! A.href (fromString ("#" ++ show (baseTag v) ++ "s"))
    $ "↑"

synopsisSpec :: SpecBase Info VName -> DocM Html
synopsisSpec spec = case spec of
  TypeAbbrSpec tpsig ->
    fullRow <$> typeBindHtml (vnameSynopsisDef $ typeAlias tpsig) tpsig
  TypeSpec l name ps _ _ ->
    pure $ fullRow $ keyword l' <> vnameSynopsisDef name <> mconcat (map ((" " <>) . typeParamHtml) ps)
    where
      l' = case l of
        Unlifted -> "type "
        SizeLifted -> "type~ "
        Lifted -> "type^ "
  ValSpec name tparams rettype _ _ _ -> do
    let tparams' = map typeParamHtml tparams
    rettype' <- noLink (map typeParamName tparams) $ typeExpHtml rettype
    pure $
      specRow
        (keyword "val " <> vnameSynopsisDef name)
        (mconcat (map (" " <>) tparams') <> ": ")
        rettype'
  ModSpec name sig _ _ ->
    specRow (keyword "module " <> vnameSynopsisDef name) ": " <$> synopsisModTypeExp sig
  IncludeSpec e _ -> fullRow . (keyword "include " <>) <$> synopsisModTypeExp e

typeExpHtml :: TypeExp Info VName -> DocM Html
typeExpHtml e = case e of
  TEUnique t _ -> ("*" <>) <$> typeExpHtml t
  TEArray d at _ -> do
    at' <- typeExpHtml at
    d' <- dimExpHtml d
    pure $ d' <> at'
  TETuple ts _ -> parens . commas <$> mapM typeExpHtml ts
  TERecord fs _ -> braces . commas <$> mapM ppField fs
    where
      ppField (name, t) = do
        t' <- typeExpHtml t
        pure $ toHtml (nameToString name) <> ": " <> t'
  TEVar name _ -> qualNameHtml name
  TEParens te _ -> parens <$> typeExpHtml te
  TEApply t arg _ -> do
    t' <- typeExpHtml t
    arg' <- typeArgExpHtml arg
    pure $ t' <> " " <> arg'
  TEArrow pname t1 t2 _ -> do
    t1' <- case t1 of
      TEArrow {} -> parens <$> typeExpHtml t1
      _ -> typeExpHtml t1
    t2' <- typeExpHtml t2
    pure $ case pname of
      Just v ->
        parens (vnameHtml v <> ": " <> t1') <> " -> " <> t2'
      Nothing ->
        t1' <> " -> " <> t2'
  TESum cs _ -> pipes <$> mapM ppClause cs
    where
      ppClause (n, ts) = joinBy " " . (ppConstr n :) <$> mapM typeExpHtml ts
      ppConstr name = "#" <> toHtml (nameToString name)
  TEDim dims t _ -> do
    t' <- typeExpHtml t
    pure $ "?" <> mconcat (map (brackets . renderName . baseName) dims) <> "." <> t'

qualNameHtml :: QualName VName -> DocM Html
qualNameHtml (QualName names vname@(VName name tag)) =
  if tag <= maxIntrinsicTag
    then pure $ renderName name
    else f <$> ref
  where
    prefix :: Html
    prefix = mapM_ ((<> ".") . renderName . baseName) names
    f (Just s) = H.a ! A.href (fromString s) $ prefix <> renderName name
    f Nothing = prefix <> renderName name

    ref = do
      boring <- asks $ S.member vname . ctxNoLink
      if boring
        then pure Nothing
        else Just <$> vnameLink vname

vnameLink :: VName -> DocM String
vnameLink vname = do
  current <- asks ctxCurrent
  file <- asks $ maybe current fst . M.lookup vname . ctxFileMap
  pure $ vnameLink' vname current file

vnameLink' :: VName -> String -> String -> String
vnameLink' (VName _ tag) current file =
  if file == current
    then "#" ++ show tag
    else relativise file current -<.> ".html#" ++ show tag

paramHtml :: Pat ParamType -> DocM Html
paramHtml pat = do
  let (pat_param, d, t) = patternParam pat
  t' <- typeHtml $ second (const Nonunique) t
  pure $ case pat_param of
    Named v -> parens (vnameHtml v <> ": " <> dietHtml d <> t')
    Unnamed -> t'

relativise :: FilePath -> FilePath -> FilePath
relativise dest src =
  concat (replicate (length (splitPath src) - 1) "../") ++ makeRelative "/" dest

dimDeclHtml :: Size -> DocM Html
dimDeclHtml = pure . brackets . toHtml . prettyString

dimExpHtml :: SizeExp Info VName -> DocM Html
dimExpHtml (SizeExpAny _) = pure $ brackets mempty
dimExpHtml (SizeExp e _) = pure $ brackets $ toHtml $ prettyString e

typeArgExpHtml :: TypeArgExp Info VName -> DocM Html
typeArgExpHtml (TypeArgExpSize d) = dimExpHtml d
typeArgExpHtml (TypeArgExpType d) = typeExpHtml d

typeParamHtml :: TypeParam -> Html
typeParamHtml (TypeParamDim name _) =
  brackets $ vnameHtml name
typeParamHtml (TypeParamType l name _) =
  "'" <> fromString (prettyString l) <> vnameHtml name

typeAbbrevHtml :: Liftedness -> Html -> [TypeParam] -> Html
typeAbbrevHtml l name params =
  what <> name <> mconcat (map ((" " <>) . typeParamHtml) params)
  where
    what = keyword $ "type" ++ prettyString l ++ " "

docHtml :: Maybe DocComment -> DocM Html
docHtml (Just (DocComment doc loc)) =
  H.preEscapedText
    . GFM.commonmarkToHtml [] [GFM.extAutolink]
    . T.pack
    <$> identifierLinks loc (T.unpack doc)
docHtml Nothing = pure mempty

identifierLinks :: SrcLoc -> String -> DocM String
identifierLinks _ [] = pure []
identifierLinks loc s
  | Just ((name, namespace, file), s') <- identifierReference s = do
      let proceed x = (x <>) <$> identifierLinks loc s'
          unknown = proceed $ "`" <> name <> "`"
      case knownNamespace namespace of
        Just namespace' -> do
          maybe_v <- lookupName (namespace', name, file)
          case maybe_v of
            Nothing -> do
              warn loc $
                "Identifier '"
                  <> fromString name
                  <> "' not found in namespace '"
                  <> fromString namespace
                  <> "'"
                  <> fromString (maybe "" (" in file " <>) file)
                  <> "."
              unknown
            Just v' -> do
              link <- vnameLink v'
              proceed $ "[`" <> name <> "`](" <> link <> ")"
        _ -> do
          warn loc $ "Unknown namespace '" <> fromString namespace <> "'."
          unknown
  where
    knownNamespace "term" = Just Term
    knownNamespace "mtype" = Just Signature
    knownNamespace "type" = Just Type
    knownNamespace _ = Nothing
identifierLinks loc (c : s') = (c :) <$> identifierLinks loc s'

lookupName :: (Namespace, String, Maybe FilePath) -> DocM (Maybe VName)
lookupName (namespace, name, file) = do
  current <- asks ctxCurrent
  let file' = mkImportFrom (mkInitialImport current) <$> file
  env <- lookupEnvForFile file'
  case M.lookup (namespace, nameFromString name) . envNameMap =<< env of
    Nothing -> pure Nothing
    Just qn -> pure $ Just $ qualLeaf qn

lookupEnvForFile :: Maybe ImportName -> DocM (Maybe Env)
lookupEnvForFile Nothing = asks $ Just . fileEnv . ctxFileMod
lookupEnvForFile (Just file) = asks $ fmap fileEnv . lookup file . ctxImports

describeGeneric ::
  VName ->
  IndexWhat ->
  Maybe DocComment ->
  (Html -> DocM Html) ->
  DocM Html
describeGeneric name what doc f = do
  name' <- H.span ! A.class_ "decl_name" <$> vnameDescDef name what
  decl_type <- f name'
  doc' <- docHtml doc
  let decl_doc = H.dd ! A.class_ "desc_doc" $ doc'
      decl_header =
        (H.dt ! A.class_ "desc_header") $
          vnameSynopsisRef name <> decl_type
  pure $ decl_header <> decl_doc

describeGenericMod ::
  VName ->
  IndexWhat ->
  ModTypeExp ->
  Maybe DocComment ->
  (Html -> DocM Html) ->
  DocM Html
describeGenericMod name what se doc f = do
  name' <- H.span ! A.class_ "decl_name" <$> vnameDescDef name what

  decl_type <- f name'

  doc' <- case se of
    ModTypeSpecs specs _ -> (<>) <$> docHtml doc <*> describeSpecs specs
    _ -> docHtml doc

  let decl_doc = H.dd ! A.class_ "desc_doc" $ doc'
      decl_header =
        (H.dt ! A.class_ "desc_header") $
          vnameSynopsisRef name <> decl_type
  pure $ decl_header <> decl_doc

describeDecs :: [Dec] -> DocM Html
describeDecs decs = do
  visible <- asks ctxVisibleMTys
  H.dl . mconcat
    <$> mapM
      (fmap $ H.div ! A.class_ "decl_description")
      (mapMaybe (describeDec visible) decs)

describeDec :: S.Set VName -> Dec -> Maybe (DocM Html)
describeDec _ (ValDec vb) = Just $
  describeGeneric (valBindName vb) (valBindWhat vb) (valBindDoc vb) $ \name -> do
    (lhs, mhs, rhs) <- valBindHtml name vb
    pure $ lhs <> mhs <> ": " <> rhs
describeDec _ (TypeDec vb) =
  Just $
    describeGeneric (typeAlias vb) IndexType (typeDoc vb) (`typeBindHtml` vb)
describeDec _ (ModTypeDec (ModTypeBind name se doc _)) = Just $
  describeGenericMod name IndexModuleType se doc $ \name' ->
    pure $ keyword "module type " <> name'
describeDec _ (ModDec mb) = Just $
  describeGeneric (modName mb) IndexModule (modDoc mb) $ \name' ->
    pure $ keyword "module " <> name'
describeDec _ OpenDec {} = Nothing
describeDec visible (LocalDec (ModTypeDec (ModTypeBind name se doc _)) _)
  | name `S.member` visible = Just $
      describeGenericMod name IndexModuleType se doc $ \name' ->
        pure $ keyword "local module type " <> name'
describeDec _ LocalDec {} = Nothing
describeDec _ ImportDec {} = Nothing

valBindWhat :: ValBind -> IndexWhat
valBindWhat vb
  | null (valBindParams vb),
    RetType _ t <- unInfo $ valBindRetType vb,
    orderZero t =
      IndexValue
  | otherwise =
      IndexFunction

describeSpecs :: [Spec] -> DocM Html
describeSpecs specs =
  H.dl . mconcat <$> mapM describeSpec specs

describeSpec :: Spec -> DocM Html
describeSpec (ValSpec name tparams t _ doc _) =
  describeGeneric name what doc $ \name' -> do
    let tparams' = mconcat $ map ((" " <>) . typeParamHtml) tparams
    t' <- noLink (map typeParamName tparams) $ typeExpHtml t
    pure $ keyword "val " <> name' <> tparams' <> ": " <> t'
  where
    what =
      case t of
        TEArrow {} -> IndexFunction
        _ -> IndexValue
describeSpec (TypeAbbrSpec vb) =
  describeGeneric (typeAlias vb) IndexType (typeDoc vb) (`typeBindHtml` vb)
describeSpec (TypeSpec l name tparams doc _) =
  describeGeneric name IndexType doc $
    pure . (\name' -> typeAbbrevHtml l name' tparams)
describeSpec (ModSpec name se doc _) =
  describeGenericMod name IndexModule se doc $ \name' ->
    case se of
      ModTypeSpecs {} -> pure $ keyword "module " <> name'
      _ -> do
        se' <- synopsisModTypeExp se
        pure $ keyword "module " <> name' <> ": " <> se'
describeSpec (IncludeSpec sig _) = do
  sig' <- synopsisModTypeExp sig
  doc' <- docHtml Nothing
  let decl_header =
        (H.dt ! A.class_ "desc_header") $
          (H.span ! A.class_ "synopsis_link") mempty
            <> keyword "include "
            <> sig'
      decl_doc = H.dd ! A.class_ "desc_doc" $ doc'
  pure $ decl_header <> decl_doc
