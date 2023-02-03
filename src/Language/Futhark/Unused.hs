module Language.Futhark.Unused
  (findUnused) where

import Futhark.Compiler (Imports, dumpError, fileProg, newFutharkConfig, readProgramFiles)
import Futhark.Pipeline (FutharkM, Verbosity (..), runFutharkM)
import Language.Futhark (QualName(QualName))
import Language.Futhark.Syntax 
import Language.Futhark.Semantic

-- ignore top level definitions in "root" files.
-- root files are the given FilePaths.
findUnused :: [FilePath] -> FutharkM Imports
findUnused files = do
  (_w, imports, _vns) <- readProgramFiles [] files
  pure imports

-- We are looking for QualNames that represent functions.
allUsedFuncs :: FileModule -> [QualName VName]
allUsedFuncs (FileModule _ _env (Prog _doc decs) _) = do
  let funcDecs = filter isFuncDec decs
  
  []


isFuncDec :: DecBase f vn -> Bool
isFuncDec (ValDec _) = True
isFuncDec _ = False

-- getDecs :: ProgBase Info VName -> [DecBase Info VName]
-- getDecs (Prog _ decs) = decs

funcsInDef :: DecBase Info VName -> [QualName VName]
funcsInDef (ValDec (ValBind _en _vn _rd _rt _tp _bp body _doc _attr _loc)) = 
  funcsInDef' body
funcsInDef _ = []

-- Handles every constructor in ExpBase that can contain a function usage.
funcsInDef' :: ExpBase Info VName -> [QualName VName]
funcsInDef' (Parens ex _) = funcsInDef' ex
funcsInDef' (QualParens (qn, _) ex _) = qn : funcsInDef' ex
funcsInDef' (TupLit exs _) = concatMap funcsInDef' exs
funcsInDef' (RecordLit exs _) = concatMap (funcsInDef' . getFieldExp) exs
funcsInDef' (ArrayLit exs _ _) = concatMap funcsInDef' exs
funcsInDef' (Attr _ ex _) = funcsInDef' ex
funcsInDef' (Project _ ex _ _) = funcsInDef' ex
funcsInDef' (Not ex _) = funcsInDef' ex
funcsInDef' (Assert ex1 ex2 _ _) = funcsInDef' ex1 <> funcsInDef' ex2
funcsInDef' (Constr _ exs _ _) = concatMap funcsInDef' exs
funcsInDef' (Update ex1 _ ex2 _) = funcsInDef' ex1 <> funcsInDef' ex2
funcsInDef' (RecordUpdate ex1 _ ex2 _ _) = funcsInDef' ex1 <> funcsInDef' ex2
funcsInDef' (Lambda _ ex _ _ _) = funcsInDef' ex
funcsInDef' (OpSection qn _ _) = [qn]
funcsInDef' (OpSectionLeft qn _ ex _ _ _) = qn : funcsInDef' ex
funcsInDef' (OpSectionRight qn _ ex _ _ _) = qn : funcsInDef' ex
funcsInDef' (Ascript ex _ _) = funcsInDef' ex
funcsInDef' _ = []

getFieldExp :: FieldBase Info VName -> ExpBase Info VName
getFieldExp (RecordFieldExplicit _ exp _) = exp
getFieldExp _ = error "placeholder" -- TODO: provide an appropriate error here.
