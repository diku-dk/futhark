--{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import System.FilePath ((-<.>), takeDirectory, (</>), normalise)
import System.Directory (createDirectoryIfMissing)

import Futhark.Compiler (readProgram)
import Futhark.Util.Options
import Futhark.Pipeline (runFutharkM, FutharkM)
import Language.Futhark.TypeChecker
import Language.Futhark.TypeChecker.Monad
import Language.Futhark
import Language.Futhark.Attributes (maxIntrinsicTag)
import Futhark.Representation.AST (pretty)
import Futhark.Util.Pretty hiding ((</>), render)
import qualified Text.PrettyPrint.Mainland as PP (pretty)
import Text.PrettyPrint.Mainland ((<+>))

import Text.Blaze.Html5 as H hiding (text, map, main)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Data.String (fromString)

main :: IO ()
main = mainWithOptions () [] f
  where f [file] _ = Just $ (runFutharkM (m file)  True >> return ())
        f _ _ = Nothing
        m ::FilePath -> Futhark.Pipeline.FutharkM ()
        m file = do
          (Prog p,w,imports,vns) <- readProgram file
          --liftIO (printModule (M.lookup "test" imports))
          --liftIO (print (fst . unzip . M.toList $ imports))
          --liftIO (print . progImports $ Prog p)
          --liftIO (print file)
          
          liftIO (printDecs imports p)
          
          --liftIO (mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec ((fromJust $ M.lookup "math" imports))) p)
          --liftIO (putStrLn . pretty . Prog $ p)
          --liftIO (forM_ p printDec)

type Context = (String,FileModule)
--type DocEnv = M.Map (Namespace,QualName Name) (String,VName)
type DocEnv = M.Map (Namespace,VName) (String)
type DocM = ReaderT Context (State DocEnv)

printDecs :: Imports -> [Dec] -> IO ()
printDecs imports decs = mapM_ write . run $ mapM (f $ render decs) (init (M.toList imports))
  where run s = evalState s M.empty
        f g x = (fst x,) <$> g x
        write (name, html) = do let file = "doc/" ++ name -<.> "html"
                                createDirectoryIfMissing True (takeDirectory file)
                                writeFile file html

render :: [Dec] -> (String, FileModule) -> State DocEnv String
render decs (name,fm) = runReaderT m (name,fm)
  where putName = h1 (toHtml name)
        m = renderHtml . docTypeHtml . (putName <>) <$> renderDecs decs

renderDecs :: [Dec] -> DocM Html
renderDecs decs = asks snd >>= f
  where f fm = mconcat <$> mapM (fmap pre) [ html | Just html <- map (prettyDec fm) decs ]

docToHtml = toHtml . PP.pretty 80

prettyDec :: FileModule -> Dec -> Maybe (DocM Html)
prettyDec fileModule dec = case dec of
  (FunDec f) -> return <$> prettyFun fileModule f
  (SigDec s) -> prettySig fileModule s
  (ModDec m) -> prettyMod fileModule m
  (ValDec v) -> prettyVal fileModule v
  (TypeDec td) -> return <$> renderType fileModule td
  (OpenDec x xs names _) -> Nothing
                            --Just $ prettyOpen fileModule (x:xs) names
                            --Just . return $ toHtml "opendec" -- <> foldMap (prettyMod . ModDec) (x:xs)
                            -- <> foldMap renderVName (unInfo names)
  (LocalDec _ _) -> Nothing --Just $ text "localdec"

--prettyOpen :: FileModule -> [ModExpBase Info VName] -> Info [VName] -> DocM Html
--prettyOpen fm xs (Info names) = mconcat <$> mapM (renderModExp fm) xs
--  where FileModule (Env { envModTable = modtable }) = fm
    --envs = foldMap (renderEnv . (\(ModEnv e) -> e) . (modtable M.!)) names

renderModExp :: FileModule -> ModExpBase Info VName -> DocM Html
renderModExp fm e = case e of
    ModVar v _ -> renderQualName Structure v
    ModParens e _ -> (toHtml "(" <>) . (<> toHtml ")") <$> renderModExp fm e 
    ModImport v _ -> return $ toHtml $ show v
    ModDecs ds _ -> renderDecs ds --nestedBlock "{" "}" (stack $ punctuate line $ map ppr ds)
    ModApply f a _ _ _ -> return mempty --parens $ ppr f <+> parens (ppr a)
    ModAscript me se _ _ -> return $ toHtml "{...}: " <> sigExpHtml se--ppr me <> colon <+> ppr se
    ModLambda param maybe_sig body _ -> error "It should not be possible to open a lambda"

renderType :: FileModule -> TypeBindBase Info VName -> Maybe Html
renderType fm tb
  | M.member name typeTable
  , visible Type name fm = Just . H.div $ typeBindHtml tb
    where (FileModule (Env {envTypeTable = typeTable})) = fm
          (TypeBind { typeAlias = name }) = tb
renderType _ _ = Nothing

prettyVal :: FileModule -> ValBindBase Info VName -> Maybe (DocM Html)
prettyVal fm (ValBind _entry name maybe_t _ _e _ doc)
  | Just (BoundV st) <- M.lookup name vtable
  , visible Term name fm =
    Just . return . H.div $
    renderDoc doc <> toHtml "let " <> vnameHtml name <> toHtml " : " <>
    docToHtml (prettyType st)
    where (FileModule (Env {envVtable = vtable})) = fm
prettyVal _ _ = Nothing

visible :: Namespace -> VName -> FileModule -> Bool
visible ns vname@(VName name _) (FileModule env)
  | Just vname' <- M.lookup (ns,name) (envNameMap env)
  = vname == vname'
visible _ _ _ = False

renderDoc (Just doc) = H.div ! A.style (fromString "padding-left: 2em") $ toHtml $ doc
renderDoc Nothing = mempty

prettySig :: FileModule -> SigBindBase Info VName -> Maybe (DocM Html)
prettySig fm (SigBind vname exp loc doc)
  | M.member vname sigtable && visible Signature vname fm =
    Just $ H.div <$> do
      name <- vnameHtmlM Signature vname
      expHtml <- renderSigExp exp
      return $ renderDoc doc <> toHtml "module type " <> name <> toHtml " = " <> expHtml
    where (FileModule (Env { envSigTable = sigtable })) = fm
prettySig _ _ = Nothing

prettyMod :: FileModule -> ModBindBase Info VName -> Maybe (DocM Html)
prettyMod fm (ModBind name ps sig e loc)
  | Just env <- M.lookup name modtable
  , visible Structure name fm = Just $ pre <$> do
    s <- sig' env
    vname <- vnameHtmlM Structure name
    return $ toHtml "module " <> vname <> s-- <+> spread (map prettyModParam ps)
    --mempty --sig' env
    where FileModule (Env { envModTable = modtable}) = fm
          sig' env = case sig of Nothing    
                                   | (ModEnv e) <- env
                                   -> (toHtml ": " <>) <$> renderEnv e
                                 Just (s,_) -> do
                                   params <- modParamHtml ps
                                   return $ toHtml ": " <> params <> sigExpHtml s <> toHtml " "
prettyMod _ _ = Nothing


prettyEnv :: Env -> Doc
prettyEnv (Env vtable ttable sigtable modtable _) =
  nestedBlock "{" "}" (stack $ punctuate line specs)
  where valBinds = map prettyValBind (M.toList vtable)
        typeBinds = map (text . show) (M.toList ttable)
        sigBinds = map (text . show) (M.toList sigtable)
        modBinds = map (text . show) (M.toList modtable)
        specs = typeBinds ++ valBinds ++ sigBinds ++ modBinds

renderEnv :: Env -> DocM Html
renderEnv (Env vtable ttable sigtable modtable _) =
  return $ toHtml "{" <> mconcat specs <> toHtml "}"
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
renderValBind = H.div . docToHtml . prettyValBind

renderTypeBind :: (VName, TypeBinding) -> Html
renderTypeBind (name, TypeAbbr tps tp) =
  H.div $ toHtml "type " <> vnameHtml name <> toHtml " = " <> docToHtml (prettyType tp)

prettyValBind :: (VName, ValBinding) -> Doc
prettyValBind (name, (BoundF (tps, pts, rt))) =
  text "val" <+> prettyName name <+> spread (map prettyTypeParam tps) <> colon <+>
  mconcat (map (\p -> prettyType  p <+> text "-> ") pts) <+> prettyType rt
prettyValBind (name, BoundV t) = text "val" <+> prettyName name <+> colon <+> prettyType t

prettyType :: StructType -> Doc
prettyType t = case t of
  (Prim et) -> ppr et
  (Array (PrimArray et (ShapeDecl ds) u _)) ->
    ppr u <> mconcat (map (brackets . prettyD) ds)  <> ppr et
  Record fs
    | Just ts <- areTupleFields fs ->
        parens $ commasep $ map prettyType ts
    | otherwise ->
        braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> prettyType t
  (Array (PolyArray _ _ _ _ _)) -> text "polyarray"
  (Array (RecordArray _ _ _)) -> undefined
  (TypeVar et targs) -> ppr (show et)

modParamHtml :: [ModParamBase Info VName] -> DocM Html
modParamHtml [] = return mempty
modParamHtml ((ModParam pname psig _):mps) = liftM2 f (renderSigExp psig) (modParamHtml mps)
  where f sigExp params = toHtml "(" <> vnameHtml pname <> toHtml ": " <> sigExp <> toHtml ") -> " <>
                          params

prettyD (NamedDim v) = prettyQualName v
prettyD (BoundDim _) = mempty
prettyD (ConstDim _) = mempty
prettyD AnyDim = mempty

--prettyModParam :: ModParamBase Info VName -> Doc
--prettyModParam (ModParam pname psig _) =
--    parens (prettyName pname <> colon <+> prettySigExp psig)

renderSigExp :: SigExpBase Info VName -> DocM Html
renderSigExp e = case e of
  (SigVar v _) -> renderQualName Signature v
  (SigParens e _) -> return $ toHtml "(" <> sigExpHtml e <> toHtml ")"
  (SigSpecs ss _) -> return $ toHtml "{" <> mapM_ specHtml ss <> toHtml "}"
  (SigWith s (TypeRef v td) _) ->
    return $ sigExpHtml s <> toHtml " with " <> docToHtml (prettyQualName v) <> toHtml " = "  <> typeDeclHtml td
  (SigArrow Nothing e1 e2 _) ->
    return $ sigExpHtml e1 <> toHtml " -> " <> sigExpHtml e2
  (SigArrow (Just v) e1 e2 _) ->
    return $ toHtml "(" <> vnameHtml v <> toHtml ": " <> sigExpHtml e1 <> toHtml ") -> " <> sigExpHtml e2

sigExpHtml :: SigExpBase Info VName ->  Html
sigExpHtml e = case e of
  (SigVar v _) -> qualNameHtml v -- . realName fm Signature $ v
  (SigParens e _) -> toHtml "(" <> sigExpHtml e <> toHtml ")"
  (SigSpecs ss _) -> toHtml "{" <> mapM_ specHtml ss <> toHtml "}"
  (SigWith s (TypeRef v td) _) ->
    sigExpHtml s <> toHtml " with " <> docToHtml (prettyQualName v) <> toHtml " = "  <> typeDeclHtml td
  (SigArrow Nothing e1 e2 _) ->
    sigExpHtml e1 <> toHtml " -> " <> sigExpHtml e2
  (SigArrow (Just v) e1 e2 _) ->
    toHtml "(" <> vnameHtml v <> toHtml ": " <> sigExpHtml e1 <> toHtml ") -> " <> sigExpHtml e2

--prettySigExp :: SigExpBase Info VName -> Doc
--prettySigExp e = case e of
--  (SigVar v _) -> prettyQualName v
--  (SigParens e _) -> parens (prettySigExp e)
--  (SigSpecs ss _) -> nestedBlock "{" "}" (stack $ punctuate line $ map prettySpec ss)
--  (SigWith s (TypeRef v td) _) ->
--    prettySigExp s <+> text "with" <+> prettyQualName v <+> equals <+> prettyTypeDecl td

vnameHtml :: VName -> Html
vnameHtml (VName name i) = 
    H.span ! A.id (fromString (show i)) $ renderName name

vnameHtmlM :: Namespace -> VName -> DocM Html
vnameHtmlM ns (VName name i) = 
    do file <- asks fst
       --modify (M.insert (ns,QualName quals name) (file,VName name i))
       modify (M.insert (ns,VName name i) file)
       return $ H.span ! A.id (fromString (show i)) $ renderName name

renderName :: Name -> Html
renderName name = docToHtml (ppr name)

specHtml :: SpecBase Info VName -> Html
specHtml spec = case spec of
  (TypeAbbrSpec tpsig) -> H.div $ typeBindHtml tpsig
  (TypeSpec name ps _ doc) -> H.div $
    renderDoc doc <> toHtml "type " <> vnameHtml name <>
    docToHtml (spread (map prettyTypeParam ps))
  (ValSpec name tparams params rettype _ doc) -> H.div $ do 
    renderDoc doc
    docToHtml $ (text "val" <+> prettyName name) <+> spread (map prettyTypeParam tparams)
    toHtml " : " <> mapM_ (\p -> typeDeclHtml p <> toHtml " -> ") params
    toHtml " " <> typeDeclHtml rettype
  (ModSpec name sig _) ->
    toHtml "module " >> docToHtml (prettyName name) >> toHtml ": " >> sigExpHtml sig
  (IncludeSpec e _) -> H.div $ toHtml "include " >> sigExpHtml e

--prettySpec spec = case spec of
--  (TypeAbbrSpec tpsig) -> prettyTypeBind tpsig
--  (TypeSpec name ps _ _) -> text "type" <+> prettyName name <+> spread (map prettyTypeParam ps)
--  (ValSpec name tparams params rettype _ _) ->
--    text "val" <+> prettyName name <+> spread (map prettyTypeParam tparams) <> colon <+>
--    mconcat (map (\p -> prettyTypeDecl  p <+> text "-> ") params) <+> prettyTypeDecl rettype
--  (ModSpec name sig _) ->
--    text "module" <+> prettyName name <> colon <+> prettySigExp sig
--  (IncludeSpec e _) -> text "include" <+> prettySigExp e

prettyTypeDecl :: TypeDeclBase Info VName -> Doc
prettyTypeDecl = prettyTypeExp . declaredType

typeDeclHtml = typeExpHtml . declaredType

typeExpHtml :: TypeExp VName -> Html
typeExpHtml e = case e of
  (TEUnique t _ ) -> toHtml "*" >> typeExpHtml t
  (TEArray at d _) -> docToHtml (brackets (prettyDimDecl d)) >> typeExpHtml at
  (TETuple ts _) -> docToHtml $ parens $ commasep $ map prettyTypeExp ts
  (TERecord fs _) -> undefined
  (TEVar name  _) -> qualNameHtml name
  (TEApply t args _) -> docToHtml $ prettyQualName t <+> spread (map prettyTypeArg args)

qualNameHtml :: QualName VName -> Html
qualNameHtml (QualName names (VName name i)) =
  if i <= maxIntrinsicTag
      then prefix <> renderName name
      else prefix <> (a ! A.href (fromString ("#" ++ show i)) $ renderName name)
  where prefix :: Html
        prefix = mapM_ ((<> toHtml ".") . docToHtml . ppr) names

renderQualName :: Namespace -> QualName VName -> DocM Html
renderQualName ns (QualName names (VName name i)) =
  if i <= maxIntrinsicTag
      then return $ prefix <> renderName name
      else f <$> link
  where prefix :: Html
        prefix = mapM_ ((<> toHtml ".") . docToHtml . ppr) names
        f link = prefix <> (a ! A.href (fromString link) $ renderName name)
        --link = do (file,vname) <- gets (M.! (ns,QualName names name))
        link = do --vname <- getVName ns (QualName names name)
                  Just file <- gets (M.lookup (ns, VName name i))
                  current <- asks fst
                  return $ relativise file current ++ ".html#" ++ show i

relativise :: FilePath -> FilePath -> FilePath
relativise target source = target
  where (target',source') = unzip $ dropWhile (uncurry (==)) (zip (normalise target) (normalise source))

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

prettyTypeExp :: TypeExp VName -> Doc
prettyTypeExp e = case e of
  (TEUnique t _ ) -> undefined
  (TEArray at d _) -> brackets (prettyDimDecl d) <> prettyTypeExp at
  (TETuple ts _) -> parens $ commasep $ map prettyTypeExp ts
  (TERecord fs _) -> undefined
  (TEVar name _) -> prettyQualName name
  (TEApply t args _) -> prettyQualName t <+> spread (map prettyTypeArg args)

prettyDimDecl :: DimDecl VName -> Doc
prettyDimDecl = ppr

prettyTypeArg :: TypeArgExp VName -> Doc
prettyTypeArg = ppr

prettyName :: VName -> Doc
prettyName (VName name _) = ppr name

renderVName :: VName -> Html
renderVName = docToHtml . prettyName

prettyTypeParam :: TypeParam -> Doc
prettyTypeParam (TypeParamDim name _) = brackets $ prettyName name
prettyTypeParam (TypeParamType name _) = text "'" <> prettyName name

typeBindHtml :: TypeBindBase Info VName -> Html
typeBindHtml (TypeBind name params usertype _ doc) =
    renderDoc doc <> toHtml "type " <> vnameHtml name <>
    docToHtml (spread (map prettyTypeParam params)) <>
    toHtml " = " <> typeDeclHtml usertype

prettyQualName v = ppr v'
  where v' = QualName names name
        QualName names (VName name _) = v

prettyFun fm (FunBind entry name retdecl rettype tparams _args _ _ doc)
  | Just (BoundF (tps,pt,rt)) <- M.lookup name vtable
  , visible Term name fm = Just $
    (renderDoc doc <>) . docToHtml $ text "val" <+> prettyName name <+>
    spread (map prettyTypeParam tps) <> colon <+>
    mconcat (map (\p -> prettyType p <+> text "-> ") pt) <+> prettyType rt--rettype
--    text "let" <+> ppr name <+>
--    spread (map ppr tparams ++ map ppr args) <> retdecl'
    where FileModule (Env {envVtable = vtable}) = fm
--           retdecl' = case retdecl of
--                           Just rettype -> text ":" <+> ppr rettype
--                           Nothing      -> mempty
prettyFun _ _ = Nothing

--foo :: Imports -> FutharkM ()
printModule (Just (FileModule (Env vtable ttable sigtable modtable namemap))) =
  do
      print "vtable"
      mapM_ (putStrLn . PP.pretty 80 . prt) . M.toList $ vtable
      print "ttable"
      print ttable
      print "sigtable"
      mapM_ (putStrLn . pretty) . M.keys $ sigtable
      print "modtable"
      --mapM_ (putStrLn . pretty) . M.keys $ modtable
      mapM_ (putStrLn . PP.pretty 80 . (\(a,b) -> ppr a <+> pm b)) . M.toList $ modtable
      --print "namemap"
      --mapM_ (print) . M.toList $ namemap

pm (ModEnv e) = prettyEnv e

prt (name,val) = text "let" <+> prettyVar name val

prettyVar name (BoundF (tparams,args,rettype)) =
  ppr name <+> spread (map ppr tparams ++ map ppr args) <> text ":" <+> ppr rettype
prettyVar name (BoundV st) = text "st" <+> ppr st

printDec (ValDec v) = printVal v
printDec (FunDec f) = printFun f
printDec (TypeDec t) = putStrLn ("type " ++ pretty t)
printDec (SigDec s) = printSig s
printDec (ModDec m) = printMod m
printDec (OpenDec modExp modExps names loc) = printModExp modExp
printDec (LocalDec dec _) = putStr "local " >> printDec dec
--printDec dec = putStrLn (pretty dec)


printModExp (ModVar v _) = putStrLn $ "var " ++ pretty v
printModExp (ModParens e _) = putStrLn "(" >> printModExp e >> putStrLn ")"
printModExp (ModImport v _) = putStrLn $ "import " ++ v
printModExp (ModDecs decs _) = forM_ decs printDec
printModExp (ModApply f a _ _ _) = printModExp f >> printModExp a
printModExp (ModAscript me se _ _) = printModExp me >> printSigExp se
printModExp (ModLambda param maybe_sig body loc) =
  printParams param >> forM_ maybe_sig printSigExp >> printModExp body

printParams p = return ()

printSigExp se = return ()

printMod (ModBind name ps sig e _) =
  putStrLn ("module " ++ show name)

printSig (SigBind name exp loc _) =
  putStrLn ("sig " ++ show name)

printVal (ValBind entry name maybeType _ def loc _) =
  putStrLn (show name)

printFun (FunBind entry name retDecl retType typeParams args body loc _) =
  putStrLn ("function " ++ show name)

-- no way to know where things are imported from
--   cannot refer to other files
--   cannot see where type is from e.g.
-- the file name prefix discrepancy

-- max intrinsic tag, i attributtes
-- fix qualname
-- fix braces
-- fix functors

-- implement toplevel
-- multiple files and links between them
-- links everywhere to everything
-- link type only if defined in same module ???
-- env should be rendered like valSpec
