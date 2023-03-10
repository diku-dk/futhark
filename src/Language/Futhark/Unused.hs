module Language.Futhark.Unused (findUnused2, partDefFuncs, getBody, getDecs) where

import Data.Bifunctor qualified as BI
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Maybe (catMaybes, maybeToList)
import Language.Futhark
import Language.Futhark.Semantic (FileModule (FileModule))

import Debug.Trace
import System.FilePath
-- Pending work:
-- ∘ Here is a piece of advice: stop using the AST as quickly as possible.
-- ∘ Gather the immediate information you need into your own bespoke data structures, and then work on those.
-- ∘ Keeping in mind the previous point: Find transitive dependencies in the top level definitions.
-- ∘ take care of modules and their main problem as described in https://github.com/diku-dk/futhark/issues/550#issuecomment-1428229674

-- Steps:
-- ∘ For each definition of a VName x in any file (corresponding to a top level definition), find which VNames are mentioned in the definition of x. (Intuitively, which functions are called directly.)
-- ∘ Compute the transitive closure of the information gathered in (1), which means you now have a mapping from each VName to the set of VNames used in its definition (even indirectly).
-- ∘ For those VNames defined in root files, take the union of their reachable names.
-- ∘ Subtract the set produced in (3) from the set of all VNames. This gives you a set of VNames that are not reachable from the root files.

-- Guidelines:
-- (1) create a mapping for functions in *any* import to the directly usable functions in it.
-- (2) create a mapping from normalized filename to top-level functions inside the filename.
-- (3) Compute transitive closure for the map in (1)

-- findUnused :: [FilePath] -> [(FilePath, FileModule)] -> [VName]
-- findUnused :: [FilePath] -> [(FilePath, FileModule)] -> [(FilePath, VName, SrcLoc)]
-- findUnused files imps = do
--   let (directImports, realImports) = partition ((`elem` map (normalise . dropExtension) files) . fst) imps
--       used = map (usedFuncsInMod . snd) directImports
--       outside = concatMap (\(fp, im) -> map (\(x, y) -> (fp, x, y)) $ unusedInMod used im) realImports
--   outside

-- We are looking for VNames that represent functions.

isFuncDec :: DecBase f vn -> Bool
isFuncDec (ValDec _) = True
isFuncDec _ = False

getDecs :: ProgBase Info VName -> [DecBase Info VName]
getDecs (Prog _ decs) = decs


findUnused2 :: [FilePath] -> [(FilePath, FileModule)] -> M.Map FilePath [(VName, SrcLoc)]
findUnused2 fp fml = do
  let nfp = map (normalise . dropExtension) fp
      (af,bf) = partDefFuncs nfp fml
      uf = S.unions $ map snd $ M.toList $ tClosure (traceShowId bf) af
      imf = M.fromList $ map (BI.second importFuncs) $ filter (\(p,_) -> p `notElem` nfp) fml
  M.map (filter (\(vn,_) -> vn `notElem` uf)) imf

type VMap = M.Map VName (S.Set VName)

-- possible future optimization:  remove VNames that aren't referenced in any top level import
-- convert to sets since list equality is incorrect
tClosure :: VMap -> VMap -> VMap
tClosure bf af =
  let bf' = tStep af bf in
  traceShow bf' $
  if bf == bf'
        then bf
        else tClosure bf' af

tStep :: VMap -> VMap -> VMap
tStep af = M.map (S.unions . S.foldl (<>) [] . S.map (\y -> maybeToList $ y `M.lookup` af))


-- get all functions and base functions, both as maps.
partDefFuncs :: [FilePath] -> [(FilePath, FileModule)] -> (VMap, VMap)
partDefFuncs fp fml = do
  let af = concatMap (defFuncs . snd) fml
      bf = M.fromList $ concatMap (defFuncs . snd) $ filter (\(ifp, _) -> ifp `elem` fp) fml
  (M.fromList af, bf)

defFuncs :: FileModule -> [(VName,S.Set VName)]
defFuncs (FileModule _ _env (Prog _doc decs) _) =
  map funcsInDefNew $ filter isFuncDec decs



isModDec :: DecBase f vn -> Bool
isModDec (ModDec _) = True
isModDec _ = False

getMaps :: DecBase f vn -> [M.Map VName VName]
getMaps (ModDec (ModBind {modExp = me})) = getMaps' me

getMaps' :: ModExpBase Info VName -> [M.Map VName VName]
getMaps' (ModParens me _) = getMaps' me
getMaps' (ModDecs db _) = concatMap getMaps db
getMaps' (ModApply _ _ _ (Info mp) _) = [mp]
getMaps' (ModAscript me _ (Info mp) _) = getMaps' me <> [mp]
getMaps' (ModLambda _ mmp me _) = getMaps' me <> map (\(_, Info mp) -> mp) (maybeToList mmp)
getMaps' _ = []


defMods :: FileModule -> [(VName,VName)]
defMods (FileModule _ _env (Prog _doc decs) _) = do
  let a = filter isModDec decs
  []

modFunsMaps :: DecBase f vn -> [VName]
modFunsMaps db = []

importFuncs :: FileModule -> [(VName, SrcLoc)]
importFuncs (FileModule _ _env (Prog _doc decs) _) =
  map (\(ValDec (ValBind {valBindName = vn, valBindLocation = loc})) -> (vn,loc) ) $ filter isFuncDec decs

funcsInDefNew :: DecBase Info VName -> (VName, S.Set VName)
funcsInDefNew (ValDec (ValBind _en vn _rd _rt _tp _bp body _doc _attr _loc)) =
  (vn, S.fromList $ map (\(QualName _ v) -> v) $ funcsInDef' body)
funcsInDefNew _ = error "not a val dec." -- TODO: change error

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
funcsInDef' (Var ex _ _) = [ex]
funcsInDef' (AppExp app _) =
  case app of
    Apply ex1 ex2 _ _ -> funcsInDef' ex1 <> funcsInDef' ex2
    Coerce ex _ _ -> funcsInDef' ex
    Range ex1 mb_ex inc_ex _ -> concatMap funcsInDef' (ex1 : fromInc inc_ex : maybeToList mb_ex)
    LetPat _ _ ex1 ex2 _ -> funcsInDef' ex1 <> funcsInDef' ex2
    LetFun _ (_, _, _, _, ex1) ex2 _ -> funcsInDef' ex1 <> funcsInDef' ex2
    If ex1 ex2 ex3 _ -> funcsInDef' ex1 <> funcsInDef' ex2 <> funcsInDef' ex3
    DoLoop _ _ ex1 loop_ex ex2 _ -> funcsInDef' (fromLoop loop_ex) <> funcsInDef' ex1 <> funcsInDef' ex2
    BinOp (qn, _) _ (ex1, _) (ex2, _) _ -> qn : funcsInDef' ex1 <> funcsInDef' ex2
    LetWith _ _ sl ex1 ex2 _ -> concatMap funcsInDef' $ fromSlice sl <> [ex1, ex2]
    Index ex sl _ -> concatMap funcsInDef' $ ex : fromSlice sl
    Match ex cases _ -> concatMap funcsInDef' $ ex : map fromCase (NE.toList cases)
funcsInDef' _ = []

fromInc :: Inclusiveness (ExpBase Info VName) -> ExpBase Info VName
fromInc (DownToExclusive x) = x
fromInc (ToInclusive x) = x
fromInc (UpToExclusive x) = x

fromLoop :: LoopFormBase f vn -> ExpBase f vn
fromLoop (For _ x) = x
fromLoop (ForIn _ x) = x
fromLoop (While x) = x

fromSlice :: [DimIndexBase f vn] -> [ExpBase f vn]
fromSlice = concatMap fromDimInd

fromDimInd :: DimIndexBase f vn -> [ExpBase f vn]
fromDimInd (DimFix x) = [x]
fromDimInd (DimSlice m1 m2 m3) = catMaybes [m1, m2, m3]

fromCase :: CaseBase f vn -> ExpBase f vn
fromCase (CasePat _ x _) = x

getFieldExp :: FieldBase Info VName -> ExpBase Info VName
getFieldExp (RecordFieldExplicit _ ex _) = ex
getFieldExp _ = error "placeholder" -- TODO: provide an appropriate error here.

-- testing functions
getBody :: DecBase f vn -> ExpBase f vn
getBody (ValDec (ValBind _en _vn _rd _rt _tp _bp body _doc _attr _loc)) = body
getBody _ = error ""
