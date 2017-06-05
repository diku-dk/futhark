{-#LANGUAGE StandaloneDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (maybe, fromJust)
import Data.Foldable (traverse_, find)

import Futhark.Compiler (readProgram)
import Futhark.Util.Options
import Futhark.Pipeline (runFutharkM, FutharkM)
import Language.Futhark.TypeChecker
import Language.Futhark.TypeChecker.Monad
import Language.Futhark
import Language.Futhark.Attributes (maxIntrinsicTag, progImports)
import Futhark.Representation.AST (pretty)
import Futhark.Util.Pretty
import qualified Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland hiding (pretty)

import Text.Blaze.Html5 as H hiding (text, map, main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Data.String (fromString)

main :: IO ()
main = mainWithOptions () [] f
  where f [file] _ = Just $ (runFutharkM (m file)  True >> return ())
        f _ _ = Nothing
        m ::FilePath -> Futhark.Pipeline.FutharkM ()
        m file = do
          (Prog p,w,imports,vns) <- readProgram file
          --liftIO (printModule (M.lookup "colour" imports))
          --liftIO (print (fst . unzip . M.toList $ imports))
          --liftIO (print . progImports $ Prog p)
          --liftIO (print file)
          
          liftIO (printDecs imports p)
          
          --liftIO (mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec ((fromJust $ M.lookup "math" imports))) p)
          --liftIO (putStrLn . pretty . Prog $ p)
          --liftIO (forM_ p printDec)

printDecs :: Imports -> [Dec] -> IO ()
--printDecs imports = mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec ((fromJust $ M.lookup "math" imports)))
--printDecs :: Imports -> _ --[Dec] -> IO ()
printDecs imports decs = mapM_ (\(name,fm) -> putStrLn . renderHtml . docTypeHtml $ putName name >> mapM_ (f fm) decs)
                         [last (M.toList imports)]
  where putName name = h1 (toHtml name)--putStrLn "" >> putStrLn name >> putStrLn ""
        --output = PP.pretty 80
        --output = renderHtml . docTypeHtml . toHtml . PP.pretty 80
        output = Prelude.id --toHtml . PP.pretty 80
        f fm dec = case prettyDec fm dec of Nothing -> return ()
                                            Just d ->  output $ pre d
--instance Monoid Html where

docToHtml = toHtml . PP.pretty 80

prettyDec :: FileModule ->  Dec -> Maybe Html
prettyDec fileModule dec = case dec of
  (FunDec f) -> prettyFun fileModule f
  (SigDec s) -> prettySig fileModule s
  (ModDec m) -> prettyMod fileModule m
--  (ValDec v) -> Just $ text "value"
--  (TypeDec td) -> Just $ text "typedec"
--  (OpenDec _ _ _ _) -> Just $ text "opendec"
--  (LocalDec _ _) -> Just $ text "localdec"
  _ -> Nothing


prettySig :: FileModule -> SigBindBase Info VName-> Maybe Html
prettySig fm (SigBind vname exp loc)
  | M.member vname sigtable =
    Just $ H.div $ toHtml "module type " <> vnameHtml vname <> toHtml " = " <> sigExpHtml exp fm
    where (FileModule (Env { envSigTable = sigtable })) = fm
prettySig _ _ = Nothing

prettyMod :: FileModule -> ModBindBase Info VName -> Maybe Html
prettyMod fm (ModBind vname ps sig e loc)
  | Just env <- M.lookup vname modtable = Just $ pre $ --(ppr (ModBind vname ps sig e loc))
    do docToHtml $ text "module"  <+> ppr name -- <+> spread (map prettyModParam ps)
       sig' env
    where (VName name _) = vname
          (FileModule (Env { envModTable = modtable})) = fm
          sig' env = case sig of Nothing    
                                   | (ModEnv e) <- env
                                   -> docToHtml $ colon <+> prettyEnv e --(modTypeFromExp e prog)
                                 Just (s,_) -> toHtml ": " <> modParamHtml ps fm <> sigExpHtml s fm <> toHtml " "
prettyMod _ _ = Nothing


prettyEnv :: Env -> Doc
prettyEnv (Env vtable ttable sigtable modtable _) =
  nestedBlock "{" "}" (stack $ punctuate line specs)
  where valBinds = map prettyValBind (M.toList vtable)
        specs = valBinds ++ []

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
    where prettyD (NamedDim v) = prettyQualName v
  Record fs
    | Just ts <- areTupleFields fs ->
        parens $ commasep $ map prettyType ts
    | otherwise ->
        braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> prettyType t

modParamHtml :: [ModParamBase Info VName] -> FileModule -> Html
modParamHtml [] fm = mempty
modParamHtml ((ModParam pname psig _):mps) fm =
  toHtml "(" <> vnameHtml pname <> toHtml ": " <> sigExpHtml psig fm <> toHtml ") -> " <>
  modParamHtml mps fm

prettyModParam :: ModParamBase Info VName -> Doc
prettyModParam (ModParam pname psig _) =
    parens (prettyName pname <> colon <+> prettySigExp psig)

realName (FileModule (Env {envNameMap = nm})) ns (QualName names (VName name i)) =
  QualName names x
  where rn = M.lookup (ns,name) nm
        x = case rn of Just y -> y
                       Nothing -> error (show nm)

sigExpHtml :: SigExpBase Info VName -> FileModule -> Html
sigExpHtml e fm = case e of
  (SigVar v _) -> qualNameHtml . realName fm Signature $ v
  (SigParens e _) -> toHtml "(" <> sigExpHtml e fm <> toHtml ")"
  (SigSpecs ss _) -> toHtml "{" <> mapM_ (specHtml fm) ss <> toHtml "}"
  (SigWith s (TypeRef v td) _) ->
    sigExpHtml s fm <> toHtml " with " <> docToHtml (prettyQualName v) <> toHtml " = "  <> typeDeclHtml td fm
  (SigArrow Nothing e1 e2 _) ->
    sigExpHtml e1 fm <> toHtml " -> " <> sigExpHtml e2 fm
  (SigArrow (Just v) e1 e2 _) ->
    toHtml "(" <> vnameHtml v <> toHtml ": " <> sigExpHtml e1 fm <> toHtml ") -> " <> sigExpHtml e2 fm

prettySigExp :: SigExpBase Info VName -> Doc
prettySigExp e = case e of
  (SigVar v _) -> prettyQualName v
  (SigParens e _) -> parens (prettySigExp e)
  (SigSpecs ss _) -> nestedBlock "{" "}" (stack $ punctuate line $ map prettySpec ss)
  (SigWith s (TypeRef v td) _) ->
    prettySigExp s <+> text "with" <+> prettyQualName v <+> equals <+> prettyTypeDecl td

vnameHtml :: VName -> Html
vnameHtml (VName name i) = 
    H.span ! A.id (fromString (show i)) $ docToHtml (ppr name)

specHtml fm spec = case spec of
  (TypeAbbrSpec tpsig) -> docToHtml $ prettyTypeBind tpsig
  (TypeSpec name ps _) -> H.div $ do
    toHtml "type "
    vnameHtml name
    docToHtml (spread (map prettyTypeParam ps))
  (ValSpec name tparams params rettype _) -> H.div $ do 
    docToHtml $ (text "val" <+> prettyName name) <+> spread (map prettyTypeParam tparams)
    toHtml " : "
    --docToHtml $ mconcat (map (\p -> prettyTypeDecl  p <+> text "-> ") params)
    mapM_ (\p -> typeDeclHtml p fm >> toHtml " -> ") params
    toHtml " " >> typeDeclHtml rettype fm
  (ModSpec name sig _) ->
    toHtml "module " >> docToHtml (prettyName name) >> toHtml ": " >> sigExpHtml sig fm
  (IncludeSpec e _) -> H.div $ toHtml "include " >> sigExpHtml e fm

prettySpec spec = case spec of
  (TypeAbbrSpec tpsig) -> prettyTypeBind tpsig
  (TypeSpec name ps _) -> text "type" <+> prettyName name <+> spread (map prettyTypeParam ps)
  (ValSpec name tparams params rettype _) ->
    text "val" <+> prettyName name <+> spread (map prettyTypeParam tparams) <> colon <+>
    mconcat (map (\p -> prettyTypeDecl  p <+> text "-> ") params) <+> prettyTypeDecl rettype
  (ModSpec name sig _) ->
    text "module" <+> prettyName name <> colon <+> prettySigExp sig
  (IncludeSpec e _) -> text "include" <+> prettySigExp e

prettyTypeDecl :: TypeDeclBase Info VName -> Doc
prettyTypeDecl = prettyTypeExp . declaredType

typeDeclHtml = typeExpHtml . declaredType

typeExpHtml :: TypeExp VName -> FileModule-> Html
typeExpHtml e fm = case e of
  (TEUnique t _ ) -> toHtml "*" >> typeExpHtml t fm
  (TEArray at d _) -> docToHtml (brackets (prettyDimDecl d)) >> typeExpHtml at fm
  (TETuple ts _) -> docToHtml $ parens $ commasep $ map prettyTypeExp ts
  (TERecord fs _) -> undefined
  (TEVar name  _) -> qualNameHtml name
  (TEApply t args _) -> docToHtml $ prettyQualName t <+> spread (map prettyTypeArg args)

--qualNameHtml = docToHtml . prettyQualName 
qualNameHtml (QualName names (VName name i)) =
  if i <= maxIntrinsicTag
      then prefix <> docToHtml (ppr name)
      else prefix <> (a ! A.href (fromString ("#" ++ show i)) $ docToHtml (ppr name))
  where prefix :: Html
        prefix = mapM_ ((<> toHtml ".") . docToHtml . ppr) names

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

prettyTypeParam :: TypeParam -> Doc
prettyTypeParam (TypeParamDim name _) = brackets $ prettyName name
prettyTypeParam (TypeParamType name _) = text "'" <> prettyName name

prettyTypeBind :: TypeBindBase Info VName -> Doc
prettyTypeBind = undefined

prettyQualName v = ppr v'
  where v' = QualName names name
        QualName names (VName name _) = v

prettyFun fm (FunBind entry vname retdecl rettype tparams args body loc)
  | Just (BoundF (tps,pt,rt)) <- M.lookup vname vtable = Just $
    docToHtml $ text "val" <+> ppr name <+> spread (map prettyTypeParam tps) <> colon <+>
    mconcat (map (\p -> ppr p <+> text "-> ") pt) <+> ppr rt--rettype
--    text "let" <+> ppr name <+>
--    spread (map ppr tparams ++ map ppr args) <> retdecl'
    where (VName name _) = vname
          (FileModule (Env {envVtable = vtable})) = fm
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
      mapM_ (putStrLn . pretty) . M.keys $ modtable
      --print "namemap"
      --mapM_ (print) . M.toList $ namemap

prt (name,val) = text "let" <+> prettyVal name val

prettyVal name (BoundF (tparams,args,rettype)) =
  ppr name <+> spread (map ppr tparams ++ map ppr args) <> text ":" <+> ppr rettype
--prettyVal name (BoundV st) = "st" ++ pretty st

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

printSig (SigBind name exp loc) =
  putStrLn ("sig " ++ show name)

printVal (ValBind entry name maybeType _ def loc) =
  putStrLn (show name)

printFun (FunBind entry name retDecl retType typeParams args body loc) =
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
