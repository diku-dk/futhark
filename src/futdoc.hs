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
import Futhark.Representation.AST (pretty)
import Futhark.Util.Pretty
import qualified Text.PrettyPrint.Mainland as PP
import Text.PrettyPrint.Mainland hiding (pretty)

main :: IO ()
main = mainWithOptions () [] f
  where f [file] _ = Just $ (runFutharkM (m file)  True >> return ())
        f _ _ = Nothing
        m ::FilePath -> Futhark.Pipeline.FutharkM ()
        m file = do
          (Prog p,w,imports,vns) <- readProgram file
          --liftIO (printModule (M.lookup "colour" imports))
          liftIO (print (fst . unzip . M.toList $ imports))
          --liftIO (print file)
          liftIO (printDecs imports p)
          --liftIO (mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec ((fromJust $ M.lookup "math" imports))) p)
          --liftIO (putStrLn . pretty . Prog $ p)
          --liftIO (forM_ p printDec)

printDecs :: Imports -> [Dec] -> IO ()
--printDecs imports = mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec ((fromJust $ M.lookup "math" imports)))
--printDecs :: Imports -> _ --[Dec] -> IO ()
printDecs imports decs = mapM_ (\(name,fm) -> putName name >> mapM_ (maybe (return ()) (putStrLn . PP.pretty 80) . prettyDec fm decs) decs)
                         (M.toList imports)
  where putName name = putStrLn "" >> putStrLn name >> putStrLn ""

prettyDec :: FileModule -> [Dec] ->  Dec -> Maybe Doc
prettyDec fileModule decs dec = case dec of
  (FunDec f) -> prettyFun fileModule f
  (SigDec s) -> prettySig fileModule s
  (ModDec m) -> prettyMod fileModule decs m
--  (ValDec v) -> Just $ text "value"
--  (TypeDec td) -> Just $ text "typedec"
--  (OpenDec _ _ _ _) -> Just $ text "opendec"
--  (LocalDec _ _) -> Just $ text "localdec"
  _ -> Nothing


prettySig fm (SigBind vname exp loc)
  | M.member vname sigtable =
    Just $ text "module type" <+> ppr name <+> equals <+> prettySigExp exp
    where (VName name _) = vname
          (FileModule (Env { envSigTable = sigtable })) = fm
prettySig _ _ = Nothing

prettyMod :: FileModule -> [Dec] -> ModBindBase Info VName -> Maybe Doc
prettyMod fm prog (ModBind vname ps sig e loc)
  | Just env <- M.lookup vname modtable = Just $ --(ppr (ModBind vname ps sig e loc))
    text "module"  <+> ppr name <+> spread (map prettyModParam ps) <+> sig' env
    where (VName name _) = vname
          (FileModule (Env { envModTable = modtable})) = fm
          sig' env = case sig of Nothing    
                                   | (ModEnv e) <- env
                                   -> colon <+> prettyEnv e --(modTypeFromExp e prog)
                                 Just (s,_) -> colon <+> prettySigExp s <> text " "
prettyMod _ _ _ = Nothing


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


modTypeFromExp :: ModExpBase Info VName -> [Dec] -> SigExpBase Info VName
modTypeFromExp e prog = case e of
  (ModApply (ModVar (QualName names name) _) a _ _ _) -> functorRetType name
  where functorRetType name = (fst . fromJust . snd . fromJust) $ find ((== name). fst) [(modName,modSig) | ModDec (ModBind modName _ modSig _ _) <- prog]
  

prettyModParam :: ModParamBase Info VName -> Doc
prettyModParam (ModParam pname psig _) =
    parens (prettyName pname <> colon <+> prettySigExp psig)

prettySigExp e = case e of
  (SigVar v _) -> prettyQualName v
  (SigParens e _) -> parens (prettySigExp e)
  (SigSpecs ss _) -> nestedBlock "{" "}" (stack $ punctuate line $ map prettySpec ss)
  (SigWith s (TypeRef v td) _) ->
    prettySigExp s <+> text "with" <+> prettyQualName v <+> equals <+> prettyTypeDecl td

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
    text "val" <+> ppr name <+> spread (map prettyTypeParam tps) <> colon <+>
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
  putStrLn ("value " ++ show name)

printFun (FunBind entry name retDecl retType typeParams args body loc) =
  putStrLn ("function " ++ show name)

-- TODO
-- split into lib + exe
-- use text not strings
-- use imports
-- print more information not just names
-- print the content of modules
-- finish the other cases
