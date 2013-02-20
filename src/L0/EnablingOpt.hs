{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

module L0.EnablingOpt ( copyCtProp
                      , enablingOpts
                      , EnablingOptError(..))
  where

import Control.Applicative
import Control.Monad.Reader

--import Data.Either

--import Control.Monad.State
import Data.Array
--import Data.List

import Data.Bits
import Data.Loc

import qualified Data.Map as M

--import qualified Data.Set as S

import L0.AbSyn

--import Debug.Trace

-- | Information about an error during type checking.  The 'Show'
-- instance for this type produces a human-readable description.
data EnablingOptError = EnablingOptError SrcLoc String
               -- ^ A general error happened at the given position and
               -- for the given reason.
               | DupParamError String String SrcLoc
               -- ^ Two function parameters share the same name.
               | CopyCtPropError SrcLoc String 
               -- ^ Copy/Constant Propagation Error
               | TypeError SrcLoc String
               | Div0Error SrcLoc

instance Show EnablingOptError where
    show (EnablingOptError pos msg) =
        "Enabling Optimization Error at " ++ locStr pos ++ ":\n" ++ msg
    show (DupParamError funname paramname pos) =
        "Parameter " ++ paramname ++
        " mentioned multiple times in argument list of function " ++
        funname ++ " at " ++ locStr pos ++ "."
    show (CopyCtPropError pos msg ) = --ee
        "Copy or Constant Folding and Propagation Implementation Error " ++ 
        msg ++ " at " ++ locStr pos -- ++ ppExp 0 ee 
    show (TypeError pos s) =
        "Type error at " ++ locStr pos ++ " in " ++ s ++ 
        " during interpretation.  This implies a bug in the type checker."
    show (Div0Error pos) =
        "Division by zero Error detected during copy/constant propagation and folding at line: " 
        ++ locStr pos 

--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
enablingOpts prog = do
    (success, prog') <- copyCtProp prog
    if(success) 
    then enablingOpts prog'
    else return prog'


-----------------------------------------------------------------
-----------------------------------------------------------------
---- Copy and Constant Propagation + Constant Folding        ----
-----------------------------------------------------------------
-----------------------------------------------------------------

-----------------------------------------------
-- The data to be stored in vtable           --
--   the third param (Bool) indicates if the -- 
--   binding is to be removed from program   --
-----------------------------------------------
data CtOrId tf  = Constant Value   tf Bool
                -- value for constant propagation

                | VarId    String  tf Bool
                -- Variable id for copy propagation

                | SymArr  (Exp tf) tf Bool
                -- various other opportunities for copy
                -- propagation, for the moment: (i) an indexed variable,
                -- (ii) an iota array, (iii) a replicated array, (iv) a TupLit, 
                -- and (v) an ArrayLit.   I leave this one open, i.e., (Exp tf),
                -- as I do not know exactly what we need here
                -- To Cosmin: Clean it up in the end, i.e., get rid of (Exp tf).

data CPropEnv tf = CopyPropEnv {   
                        envVtable  :: M.Map String (CtOrId tf)
                  }

newtype CPropM tf a = CPropM (ReaderT (CPropEnv tf) (Either EnablingOptError) a)
    deriving (MonadReader (CPropEnv tf), Monad, Applicative, Functor)

-- | The enabling optimizations run in this monad.  Note that it has no mutable
-- state, but merely keeps track of current bindings in a 'TypeEnv'.
-- The 'Either' monad is used for error handling.
runCPropM :: CPropM tf a -> CPropEnv tf -> Either EnablingOptError a
runCPropM  (CPropM a) env = runReaderT a env

badCPropM :: EnablingOptError -> CPropM tf a
badCPropM = CPropM . lift . Left


-- | Bind a name as a common (non-merge) variable.
-- TypeBox tf => 
bindVar :: CPropEnv tf -> (String, CtOrId tf) -> CPropEnv tf
bindVar env (name,val) =
  env { envVtable = M.insert name val $ envVtable env }

bindVars :: CPropEnv tf -> [(String, CtOrId tf)] -> CPropEnv tf
bindVars = foldl bindVar

binding :: [(String, CtOrId tf)] -> CPropM tf a -> CPropM tf a
binding bnds = local (`bindVars` bnds)

-- | Remove the binding for a name.
-- TypeBox tf => 
remVar :: CPropEnv tf -> String -> CPropEnv tf
remVar env name = env { envVtable = M.delete name $ envVtable env }

remVars :: CPropEnv tf -> [String] -> CPropEnv tf
remVars = foldl remVar

remBindings :: [String] -> CPropM tf a -> CPropM tf a
remBindings keys = local (`remVars` keys)

-- | Applies Copy/Constant Propagation and Folding to an Entire Program.
-- TypeBox tf => 
copyCtProp :: TypeBox tf => Prog tf -> Either EnablingOptError (Bool, Prog tf)
copyCtProp prog = do
    let env = CopyPropEnv { envVtable = M.empty }
    -- res   <- runCPropM (mapM copyCtPropFun prog) env
    -- let (bs, rs) = unzip res
    (bs, rs) <- unzip <$> runCPropM (mapM copyCtPropFun prog) env
    return (foldl (||) False bs, rs)

copyCtPropFun :: TypeBox tf => FunDec tf -> CPropM tf (Bool, FunDec tf)
copyCtPropFun (fname, rettype, args, body, pos) = do
    (s, body') <- copyCtPropExp body 
    return (s, (fname, rettype, args, body', pos))

--------------------------------------------------------------------
--------------------------------------------------------------------
---- Main Function: Copy/Ct propagation and folding for exps    ----
--------------------------------------------------------------------
--------------------------------------------------------------------

copyCtPropExp :: TypeBox tf => Exp tf -> CPropM tf (Bool, (Exp tf))

copyCtPropExp (LetWith nm e inds el body pos) = do
    (s, e')        <- copyCtPropExp e 
    (se,el')       <- copyCtPropExp el
    (ss, inds')    <- unzip <$> mapM copyCtPropExp inds
    (sbody, body') <- copyCtPropExp body
    -- propagating (nm,e[inds]) would be incorrect and  
    -- would defeat the in-place semantics of LetWith 
    return ( s || se || foldl (||) False ss || sbody
           , LetWith nm e' inds' el' body' pos     ) 

copyCtPropExp (LetPat pat e body pos) = do
    (s1, e') <- copyCtPropExp e
    remv <- isRemovablePat pat e'
    bnds <- getPropBnds pat e' remv
    (s2, body') <-  if null bnds   then copyCtPropExp body
                    else binding bnds $ copyCtPropExp body
    let (s3, e_res) = if remv then (True,body')
                      else (False, LetPat pat e' body' pos)
    return (s1 || s2 || s3, e_res)

 
copyCtPropExp (DoLoop ind n body mergevars pos) = do
    (s1, n')   <- copyCtPropExp n
    let mergenames = map identName mergevars
    bnds       <- mapM (\vnm -> asks $ M.lookup vnm . envVtable) mergenames
    let idbnds1 = zip bnds mergevars
    let idbnds  = filter ( \(x,_) -> isValidBnd     x ) idbnds1
    let toadd   = filter ( \(x,_) -> isRemovableBnd x ) idbnds 
    let remkeys = map (\(_, (Ident s _ _) ) -> s) idbnds
    (s2, body')<- remBindings remkeys $ copyCtPropExp body
    let newloop = DoLoop ind n' body' mergevars pos
    (_, resloop) <- foldM (writeBackBnd pos) (False, newloop) toadd
    --(_, resloop) <- foldM (\y bnd -> writeBackBnd pos y bnd) (False, newloop) toadd
    --let ccc = (toadd !! 0)
    --(_, resloop1) <- writeBackBnd pos (False,newloop) (toadd !! 0)
    return (s1 || s2, resloop)--newloop)--resloop1)

copyCtPropExp e@(Var (Ident vnm _ pos)) = do 
    -- let _ = trace ("In VarExp: "++ppExp 0 e) e
    bnd <- asks $ M.lookup vnm . envVtable
    case bnd of
        Nothing                 -> return (False, e)
        Just (Constant v   _ _) -> return (True,  Literal v     )
        Just (VarId  id' tp1 _) -> return (True,  Var (Ident id' tp1 pos)) -- or tp
        Just (SymArr e'    _ _) ->
            case e' of
                Replicate _ _ _ _ -> return (False, e )
                TupLit    _ _     -> return (False, e )
                ArrayLit  _ _ _   -> return (False, e )
                Index _ _ _ _ _   -> return (True,  e')
                Iota  _ _         -> return (True,  e')
                _                 -> return (False, e )

copyCtPropExp eee@(Index idd@(Ident vnm tp p) inds tp1 tp2 pos) = do 
  (ss1, inds')  <- unzip <$> mapM copyCtPropExp inds
  let ss  = foldl (||) False ss1
  bnd <- asks $ M.lookup vnm . envVtable 
  case bnd of
    Nothing               -> return (ss,    Index idd inds' tp1 tp2 pos)
    Just (VarId  id' _ _) -> return (True,  Index (Ident id' tp p) inds' tp1 tp2 pos)
    Just (Constant v _ _) -> 
      case v of
        ArrayVal _ _ _ ->
          let sh = arrayShape v 
          in case ctIndex inds' of
               Nothing -> return (ss, Index idd inds' tp1 tp2 pos)
               Just iis-> 
                 if (length iis == length sh)
                 then case getArrValInd v iis of
                        Nothing -> return (ss, Index idd inds' tp1 tp2 pos)
                        Just el -> return (True, Literal el)
                 else return (ss, Index idd inds' tp1 tp2 pos)
        _ -> badCPropM $ TypeError pos  " indexing into a non-array value "
    Just (SymArr e' _ _) -> 
      case (e', inds') of 
        (Iota _ _, [ii]) -> return (True, ii)
        (Iota _ _, _)    -> badCPropM $ TypeError pos  " bad indexing in iota "
        (Replicate _ vvv@(Var vv@(Ident _ _ _)) _ _, _:is') -> do
            (_, inner) <- if null is' then copyCtPropExp vvv
                                      else copyCtPropExp (Index vv is' tp1 tp2 pos) 
            return (True, inner)
        (Replicate _ (Index a ais _ _ _) _ _, _:is') -> do
            (_, inner) <- copyCtPropExp (Index a (ais ++ is') tp1 tp2 pos)
            return (True, inner)
        (Replicate _ (Iota n _) _ _, _:is') -> do
            if     (length is' == 0) then return (True, Iota n pos )
            else if(length is' == 1) then return (True, head is')
            else badCPropM $ TypeError pos  (" illegal indexing: " ++ ppExp 0 eee)
        (Replicate _ _ _ _, _) -> 
            return (ss, Index idd inds' tp1 tp2 pos)
        (ArrayLit _ _ _   , _) ->
            case ctIndex inds' of
                Nothing  -> return (ss, Index idd inds' tp1 tp2 pos)
                Just iis -> case getArrLitInd e' iis of
                                Nothing -> return (ss, Index idd inds' tp1 tp2 pos)
                                Just el -> return (True, el)
        (Index aa ais t1 t2 _,_) -> do
            (_, inner) <- copyCtPropExp( Index aa (ais ++ inds') t1 t2 pos ) 
            return (True, inner)
        (TupLit   _ _, _       ) -> badCPropM $ TypeError pos  " indexing into a tuple "
        _ -> badCPropM $ CopyCtPropError pos (" Unreachable case in copyCtPropExp of Index exp: " ++
                                              ppExp 0 eee++" is bound to "++ppExp 0 e' ) --e 
                                              --" index-exp of "++ppExp 0 eee++" bound to "++ppExp 0 e' ) --e


copyCtPropExp (Literal v)       = do 
    return (False, Literal v)

copyCtPropExp (TupLit els pos) = do 
    res <- mapM copyCtPropExp els
    let (bs, els') = unzip res
    return (foldl (||) False bs, TupLit els' pos)

copyCtPropExp (ArrayLit  els tp pos) = do 
    (bs, els') <- unzip <$> mapM copyCtPropExp els
    return (foldl (||) False bs, ArrayLit els' tp pos)
    
copyCtPropExp (BinOp bop e1 e2 tp pos) = do 
    (s1, e1')   <- copyCtPropExp e1
    (s2, e2')   <- copyCtPropExp e2
    (s3, res_e) <- ctFoldBinOp (BinOp bop e1' e2' tp pos)
    return (s1 || s2 || s3, res_e)

copyCtPropExp (And e1 e2 pos) = do 
    (s1, e1')   <- copyCtPropExp e1
    (s2, e2')   <- copyCtPropExp e2
    (s, res_e)  <- ctFoldBinOp (And e1' e2' pos)
    return (s || s1 || s2, res_e)

copyCtPropExp (Or e1 e2 pos) = do 
    (s1, e1')   <- copyCtPropExp e1
    (s2, e2')   <- copyCtPropExp e2
    (s, res_e)  <- ctFoldBinOp (Or e1' e2' pos)
    return (s || s1 || s2, res_e)


copyCtPropExp (Negate e _ pos) = do 
    (s, e')   <- copyCtPropExp e
    if( isValue e' ) 
    then case e' of
            Literal (IntVal  v _) -> return (True, Literal (IntVal  (0  -v) pos))
            Literal (RealVal v _) -> return (True, Literal (RealVal (0.0-v) pos))
            _ -> badCPropM $ TypeError pos  " ~ operands not of (the same) numeral type! "
    else return (s, e')

copyCtPropExp (Not e pos) = do 
    (s, e')   <- copyCtPropExp e
    if( isValue e' ) 
    then case e' of
            Literal (LogVal  v _) -> return (True, Literal (LogVal (not v) pos))
            _ -> badCPropM $ TypeError pos  " not operands not of (the same) numeral type! "    
    else return (s, e')

copyCtPropExp (If e1 e2 e3 tp pos) = do 
    (s1, e1')   <- copyCtPropExp e1
    (s2, e2')   <- copyCtPropExp e2
    (s3, e3')   <- copyCtPropExp e3
    if      isCt1 e1' then return (True, e2')
    else if isCt0 e1' then return (True, e3')
    else return (s1 || s2 || s3, If e1' e2' e3' tp pos)

copyCtPropExp (Apply fname es tp pos) = do 
    (ss,es') <- copyCtPropExpList es
    return (ss, Apply fname es' tp pos)

copyCtPropExp (Iota e pos) = do 
    (s, e')   <- copyCtPropExp e
    return (s, Iota e' pos)

copyCtPropExp (Size e pos) = do 
    (s, e')   <- copyCtPropExp e
    return (s, Size e' pos)

copyCtPropExp (Replicate e1 e2 tp pos) = do
    (s1, e1')   <- copyCtPropExp e1
    (s2, e2')   <- copyCtPropExp e2
    return (s1 || s2, Replicate e1' e2' tp pos)

copyCtPropExp (Reshape es e tp1 tp2 pos) = do
    (ss, es') <- copyCtPropExpList es
    (s,  e' ) <- copyCtPropExp e
    return (s || ss, Reshape es' e' tp1 tp2 pos)

copyCtPropExp (Transpose e tp1 tp2 pos) = do
    (s,  e' ) <- copyCtPropExp e
    return (s, Transpose e' tp1 tp2 pos)

copyCtPropExp (Map fname e tp1 tp2 pos) = do
    (s,  e' ) <- copyCtPropExp e
    return (s, Map fname e' tp1 tp2 pos)

copyCtPropExp (Reduce fname e1 e2 tp pos) = do
    (s1, e1') <- copyCtPropExp e1
    (s2, e2') <- copyCtPropExp e2
    return (s1 || s2, Reduce fname e1' e2' tp pos)

-------------------------------------------------------
------- ZipWith was replaced with map . zip!!!  -------
-------------------------------------------------------
--copyCtPropExp (ZipWith fname es tp1 tp2 pos) = do
--    (ss, es') <- copyCtPropExpList es
--    return (ss, ZipWith fname es' tp1 tp2 pos)

copyCtPropExp (Zip exptps pos) = do
    let (es, tps) = unzip exptps
    (ss, es') <- copyCtPropExpList es
    return (ss, Zip (zip es' tps) pos)

copyCtPropExp (Unzip e tps pos)= do
    (s, e') <- copyCtPropExp e
    return (s, Unzip e' tps pos)

copyCtPropExp (Scan fname e1 e2 tp pos) = do
    (s1, e1') <- copyCtPropExp e1
    (s2, e2') <- copyCtPropExp e2
    return (s1 || s2, Scan fname e1' e2' tp pos)

copyCtPropExp (Filter fname e tp pos) = do
    (s, e') <- copyCtPropExp e
    return (s, Filter fname e' tp pos)

copyCtPropExp (Mapall fname e tp1 tp2 pos) = do
    (s, e') <- copyCtPropExp e
    return (s, Mapall fname e' tp1 tp2 pos)

copyCtPropExp (Redomap f g e1 e2 tp1 tp2 pos) = do
    (s1, e1') <- copyCtPropExp e1
    (s2, e2') <- copyCtPropExp e2
    return (s1 || s2, Redomap f g e1' e2' tp1 tp2 pos)

copyCtPropExp (Split e1 e2 tp pos) = do
    (s1, e1') <- copyCtPropExp e1
    (s2, e2') <- copyCtPropExp e2
    return (s1 || s2, Split e1' e2' tp pos)

copyCtPropExp (Concat e1 e2 tp pos) = do
    (s1, e1') <- copyCtPropExp e1
    (s2, e2') <- copyCtPropExp e2
    return (s1 || s2, Concat e1' e2' tp pos)

copyCtPropExp (Write e tp pos) = do
    (s, e') <- copyCtPropExp e
    return (s, Write e' tp pos)

copyCtPropExp r@(Read _ _) = do
    return (False, r)



-- copyCtPropExp e = do
--    return (False, e)


copyCtPropExpList :: TypeBox tf => [Exp tf] -> CPropM tf (Bool, [Exp tf])
copyCtPropExpList es = do
    (ss, es') <- unzip <$> mapM copyCtPropExp es
    return (foldl (||) False ss, es')


------------------------------------------------
---- Constant Folding                       ----
------------------------------------------------

ctFoldBinOp :: TypeBox tf => Exp tf -> CPropM tf (Bool, (Exp tf))
ctFoldBinOp e@(BinOp Plus e1 e2 _ pos) = do
    if isCt0 e1 then return (True,e2) else if isCt0 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1+v2) pos))
                (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (RealVal (v1+v2) pos))
                _ -> badCPropM $ TypeError pos  " + operands not of (the same) numeral type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Minus e1 e2 _ pos) = do
    if isCt0 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1-v2) pos))
                (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (RealVal (v1-v2) pos))
                _ -> badCPropM $ TypeError pos  " - operands not of (the same) numeral type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Times e1 e2 _ pos) = do
    if      isCt0 e1 then return (True,e1) else if isCt0 e2 then return (True,e2)
    else if isCt1 e1 then return (True,e2) else if isCt1 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1*v2) pos))
                (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (RealVal (v1*v2) pos))
                _ -> badCPropM $ TypeError pos  " * operands not of (the same) numeral type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Divide e1 e2 _ pos) = do
    if      isCt0 e1 then return (True,e1) 
    else if isCt0 e2 then badCPropM $ Div0Error pos
    else if isCt1 e2 then return (True,e1) 
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (div v1 v2) pos))
                (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (RealVal (v1 / v2)   pos))
                _ -> badCPropM $ TypeError pos  " / operands not of (the same) numeral type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Pow e1 e2 _ pos) = do
    if      isCt0 e1 || isCt1 e1 || isCt1 e2 then return (True,e1) 
    else if isCt0 e2 then case e1 of
                            Literal (IntVal  _ _) -> return (True, Literal (IntVal  1   pos))
                            Literal (RealVal _ _) -> return (True, Literal (RealVal 1.0 pos))
                            _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1 ^v2) pos))
                (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (RealVal (v1**v2) pos))
                _ -> badCPropM $ TypeError pos  " pow operands not of (the same) numeral type! "
         else return (False, e)
ctFoldBinOp e@(BinOp ShiftL e1 e2 _ pos) = do
    if      isCt0 e2 then return (True,e1) 
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (shiftL v1 v2) pos))
                _ -> badCPropM $ TypeError pos  " << operands not of integer type! "
         else return (False, e)
ctFoldBinOp e@(BinOp ShiftR e1 e2 _ pos) = do
    if      isCt0 e2 then return (True,e1) 
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (shiftR v1 v2) pos))
                _ -> badCPropM $ TypeError pos  " >> operands not of integer type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Band e1 e2 _ pos) = do
    if      isCt0 e1 then return (True,e1) else if isCt0 e2 then return (True,e2)
    else if isCt1 e1 then return (True,e2) else if isCt1 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1 .&. v2) pos))
                _ -> badCPropM $ TypeError pos  " & operands not of integer type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Bor e1 e2 _ pos) = do
    if      isCt0 e1 then return (True,e2) else if isCt0 e2 then return (True,e1)
    else if isCt1 e1 then return (True,e1) else if isCt1 e2 then return (True,e2)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (v1 .|. v2) pos))
                _ -> badCPropM $ TypeError pos  " | operands not of integer type! "
         else return (False, e)
ctFoldBinOp e@(BinOp Xor e1 e2 _ pos) = do
    if      isCt0 e1 then return (True,e2) else if isCt0 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (IntVal  (xor v1 v2) pos))
                _ -> badCPropM $ TypeError pos  " ^ operands not of integer type! "
         else return (False, e)
ctFoldBinOp e@(And e1 e2 pos) = do
    if      isCt0 e1 then return (True,e1) else if isCt0 e2 then return (True,e2)
    else if isCt1 e1 then return (True,e2) else if isCt1 e2 then return (True,e1)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (LogVal  v1 _), Literal (LogVal  v2 _)) -> return (True, Literal (LogVal  (v1 && v2) pos))
                _ -> badCPropM $ TypeError pos  " && operands not of boolean type! "
         else return (False, e)
ctFoldBinOp e@(Or e1 e2 pos) = do
    if      isCt0 e1 then return (True,e2) else if isCt0 e2 then return (True,e1)
    else if isCt1 e1 then return (True,e1) else if isCt1 e2 then return (True,e2)
    else if(isValue e1 && isValue e2)
         then case (e1, e2) of
                (Literal (LogVal  v1 _), Literal (LogVal  v2 _)) -> return (True, Literal (LogVal  (v1 || v2) pos))
                _ -> badCPropM $ TypeError pos  " || operands not of boolean type! "
         else return (False, e)

ctFoldBinOp e@(BinOp Equal e1 e2 _ pos) = do
    if(isValue e1 && isValue e2) then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and test equality with 0 or 0.0!
        (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        (Literal (LogVal  v1 _), Literal (LogVal  v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        (Literal (CharVal v1 _), Literal (CharVal v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        --(Literal (TupVal  v1 _), Literal (TupVal  v2 _)) -> return (True, Literal (LogVal (v1==v2) pos))
        _ -> badCPropM $ TypeError pos  " equal operands not of (the same) basic type! "
    else return (False, e)
ctFoldBinOp e@(BinOp Less e1 e2 _ pos) = do
    if(isValue e1 && isValue e2) then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        (Literal (LogVal  v1 _), Literal (LogVal  v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        (Literal (CharVal v1 _), Literal (CharVal v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        --(Literal (TupVal  v1 _), Literal (TupVal  v2 _)) -> return (True, Literal (LogVal (v1<v2) pos))
        _ -> badCPropM $ TypeError pos  " less-than operands not of (the same) basic type! "
    else return (False, e)
ctFoldBinOp e@(BinOp Leq e1 e2 _ pos) = do
    if(isValue e1 && isValue e2) then
      case (e1, e2) of
        -- for numerals we could build node e1-e2, simplify and compare with 0 or 0.0!
        (Literal (IntVal  v1 _), Literal (IntVal  v2 _)) -> return (True, Literal (LogVal (v1<=v2) pos))
        (Literal (RealVal v1 _), Literal (RealVal v2 _)) -> return (True, Literal (LogVal (v1<=v2) pos))
        (Literal (LogVal  v1 _), Literal (LogVal  v2 _)) -> return (True, Literal (LogVal (v1<=v2) pos))
        (Literal (CharVal v1 _), Literal (CharVal v2 _)) -> return (True, Literal (LogVal (v1<=v2) pos))
        --(Literal (TupVal  v1 _), Literal (TupVal  v2 _)) -> return (True, Literal (LogVal (v1<=v2) pos))
        _ -> badCPropM $ TypeError pos  " less-than-or-equal operands not of (the same) basic type! "
    else return (False, e)
ctFoldBinOp e = return (False, e)


----------------------------------------------------
---- Helpers for VTABLE bindings                 ---
----------------------------------------------------

isRemovableBnd :: Maybe (CtOrId tf) -> Bool
isRemovableBnd bnd = case bnd of
                    Nothing             -> False
                    Just (Constant _ _ b) -> b
                    Just (VarId    _ _ b) -> b
                    Just (SymArr   _ _ b) -> b
isValidBnd :: Maybe (CtOrId tf) -> Bool
isValidBnd bnd = case bnd of
                    Nothing -> False
                    Just _  -> True

writeBackBnd :: TypeBox tf => SrcLoc -> (Bool,Exp tf) -> (Maybe (CtOrId tf), Ident tf) -> CPropM tf (Bool,Exp tf)
writeBackBnd pos (_,loop) (bnd,ident) = do
-- nm denotes a merged variable that is to be removed by propagation, 
-- hence needs to be written back, i.e., the result is: `let nm = bnd in loop'
    case bnd of
        Nothing -> badCPropM $ CopyCtPropError pos (" Broken invariant in writeBackBnd: " ++
                                                      "merged-var binding is Nothing") --e
        Just (Constant val _ b) -> 
            if b then return ( False, (LetPat (Id ident) (Literal val)   loop pos) )
            else badCPropM $ CopyCtPropError pos (" Broken invariant in writeBackBnd: " ++
                                                    "write-back merged-var not removable") --e
        Just (VarId    ii  tp b) ->                        -- could use `ident' here
            if b then return ( False, (LetPat (Id ident) (Var (Ident ii tp pos)) loop pos) )  
            else badCPropM $ CopyCtPropError pos (" Broken invariant in writeBackBnd: " ++
                                                    "write-back merged-var not removable") --e
        Just (SymArr   eee _ b) -> 
            if b then return ( False, (LetPat (Id ident) eee loop pos) )
            else badCPropM $ CopyCtPropError pos (" Broken invariant in writeBackBnd: " ++
                                                    "write-back merged-var not removable") --e


----------------------------------------------------
---- Helpers for Constant Folding                ---
----------------------------------------------------


isValue :: TypeBox tf => Exp tf -> Bool
isValue e = case e of
              Literal _ -> True
              _         -> False 

isCt1 :: TypeBox tf => Exp tf -> Bool
isCt1 e = case e of
            Literal (IntVal  one _)  -> (one == 1  )
            Literal (RealVal one _)  -> (one == 1.0)
            Literal (LogVal True _)  -> True
            _                        -> False
isCt0 :: TypeBox tf => Exp tf -> Bool
isCt0 e = case e of
            Literal (IntVal  zr   _) -> (zr == 0  )
            Literal (RealVal zr   _) -> (zr == 0.0)
            Literal (LogVal False _) -> True
            _                        -> False

----------------------------------------------------
---- Helpers for Constant/Copy Propagation       ---
----------------------------------------------------

isBasicTypeVal :: Value -> Bool
isBasicTypeVal (IntVal     _ _) = True
isBasicTypeVal (RealVal    _ _) = True
isBasicTypeVal (LogVal     _ _) = True
isBasicTypeVal (CharVal    _ _) = True
isBasicTypeVal (ArrayVal _ _ _) = False
isBasicTypeVal (TupVal    vs _) = 
    foldl (&&) True (map isBasicTypeVal vs)

isCtOrCopy :: TypeBox tf => Exp tf -> Bool
isCtOrCopy (Literal  val   ) = isBasicTypeVal val
isCtOrCopy (TupLit   ts _  ) = foldl (&&) True (map isCtOrCopy ts)
isCtOrCopy (Var           _) = True
isCtOrCopy (Iota        _ _) = True
isCtOrCopy (Index _ _ _ _ _) = True
isCtOrCopy _                 = False

isRemovablePat  :: TypeBox tf => TupIdent tf -> Exp tf -> CPropM tf Bool 
isRemovablePat (Id _) e = 
 let s=case e of
        Var     _         -> True
        Index   _ _ _ _ _ -> True
        Iota    _ _       -> True
        Literal v         -> isBasicTypeVal v
        TupLit  _ _       -> False
        _                 -> False
 in return s

isRemovablePat (TupId tups _) e = 
    case e of
          Var (Ident vnm _ _)      -> do
              bnd <- asks $ M.lookup vnm . envVtable
              case bnd of
                  Just (Constant val@(TupVal ts   _) _ _) -> 
                      return ( isBasicTypeVal val && length ts == length tups )
                  Just (SymArr   tup@(TupLit ts _  ) _ _) -> 
                      return ( isCtOrCopy tup && length ts == length tups ) 
                  _ ->  return False
          TupLit  _ _              -> return (isCtOrCopy     e  )
          Literal val@(TupVal _ _) -> return (isBasicTypeVal val)
          _ -> return False

getPropBnds :: TypeBox tf => TupIdent tf -> Exp tf -> Bool -> CPropM tf [(String,CtOrId tf)]
getPropBnds ( Id (Ident var tp pos) ) e to_rem = 
  let r = case e of
            Literal v            -> [(var, (Constant v (boxType (valueType v)) to_rem))]
            Var (Ident id1 tp1 _)-> [(var, (VarId  id1 tp1 to_rem))]
            Index   _ _ _ _ _    -> [(var, (SymArr e   tp  to_rem))]
            TupLit     _  _      -> [(var, (SymArr e   tp  to_rem))]

            Iota           _ _   -> let newtp = boxType (Array (Int pos) Nothing pos) -- (Just n) does not work Exp tf
                                    in  [(var, SymArr e newtp to_rem)]
            Replicate _ _ _ _    -> [(var, SymArr e tp to_rem)] 
            ArrayLit    _ _ _    -> [(var, SymArr e tp to_rem)]
            _ -> [] 
  in return r 
getPropBnds pat@(TupId ids _) e to_rem = 
    case e of
        TupLit  ts _          ->
            if( length ids == length ts )
            then do lst <- mapM  (\(x,y)->getPropBnds x y to_rem) (zip ids ts)
                    return (foldl (++) [] lst)
            else return []
        Literal (TupVal ts _) ->
            if( length ids == length ts )
            then do lst <- mapM (\(x,y)->getPropBnds x (Literal y) to_rem) (zip ids ts)
                    return (foldl (++) [] lst)
            else return []
        Var (Ident vnm _ _)   -> do 
            bnd <- asks $ M.lookup vnm . envVtable
            case bnd of
                Just (SymArr tup@(TupLit   _ _) _ _) -> getPropBnds pat tup           to_rem
                Just (Constant tup@(TupVal _ _) _ _) -> getPropBnds pat (Literal tup) to_rem 
                _                                    -> return []
        _ -> return []

ctIndex :: TypeBox tf => [Exp tf] -> Maybe [Int]
ctIndex []     = Just []
ctIndex (i:is) = 
  case i of
    Literal (IntVal ii _) ->  
      let x = ctIndex is in
      case x of
        Nothing -> Nothing
        Just y -> Just (ii:y)
    _ -> Nothing 

getArrValInd :: Value -> [Int] -> Maybe Value
getArrValInd v [] = if isBasicTypeVal v then Just v else Nothing 
getArrValInd (ArrayVal arr _ _) (i:is) = getArrValInd (arr ! i) is
getArrValInd _ _ = Nothing 

getArrLitInd :: TypeBox tf => Exp tf -> [Int] -> Maybe (Exp tf)
getArrLitInd e [] = if isCtOrCopy e then Just e else Nothing 
getArrLitInd (ArrayLit els _ _) (i:is) = getArrLitInd (els !! i) is
getArrLitInd (Literal arr@(ArrayVal _ _ _)) (i:is) = 
    case getArrValInd arr (i:is) of
        Nothing -> Nothing
        Just v  -> Just (Literal v) 
getArrLitInd _ _ = Nothing 

