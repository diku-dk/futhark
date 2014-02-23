-- | Create a graph of interactions between (tupleless) SOACs.  The
-- resulting graph is only complete if the program is normalised (in
-- particular, SOACs must only appear immediately in the bindee
-- position of a let-pattern).
module L0C.SOACFlowGraph
  ( makeFlowGraph
  , ppFlowGraph
  , makeFlowGraphString
  , FlowGraph(..)
  , ExpFlowGraph(..)
  )
  where

import Control.Monad.Writer

import Data.Graph
import Data.Maybe
import Data.List
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import L0C.HORepresentation.SOAC (SOAC)
import qualified L0C.HORepresentation.SOAC as SOAC
import L0C.InternalRep

newtype FlowGraph = FlowGraph (HM.HashMap Name ExpFlowGraph)

newtype ExpFlowGraph =
  ExpFlowGraph {
    expFlowGraph :: HM.HashMap String (String, HS.HashSet (String, [String]), ExpFlowGraph)
  }

graphInDepOrder :: ExpFlowGraph
                -> [(String, (String, HS.HashSet (String, [String]), ExpFlowGraph))]
graphInDepOrder = reverse . flattenSCCs . stronglyConnComp . buildGraph
  where buildGraph (ExpFlowGraph m) =
          [ (node, name, deps) |
            node@(name, (_, users,_)) <- m',
            let users' = HS.map fst users,
            let deps = [ other
                         | other <- map fst m', other `HS.member` users' ] ]
          where m' = HM.toList m

ppFlowGraph :: FlowGraph -> String
ppFlowGraph (FlowGraph m) = intercalate "\n" . map ppFunFlow . HM.toList $ m
  where ppFunFlow (fname, eg) =
          "function " ++ nameToString fname ++ ":\n" ++
          concatMap (padLines . ppExpGraph) (graphInDepOrder eg)
        ppExpGraph (name, (soac, users, eg)) =
          name ++ " (" ++ soac ++ ") -> " ++
          intercalate ", " (map ppUsage $ HS.toList users) ++ ":\n" ++
          intercalate "" (map (padLines . ppExpGraph) $ graphInDepOrder eg)
        ppUsage (user, [])   = user
        ppUsage (user, trns) = user ++ "(" ++ intercalate ", " trns ++ ")"
        pad = ("  "++)
        padLines = unlines . map pad . lines

makeFlowGraphString :: Prog -> String
makeFlowGraphString = ppFlowGraph . makeFlowGraph

makeFlowGraph :: Prog -> FlowGraph
makeFlowGraph = FlowGraph . HM.fromList . map flowForFun . progFunctions

data SOACInfo = SOACInfo {
    soacType     :: String
  , soacProduced :: HS.HashSet VName
  , soacConsumed :: HM.HashMap VName (HS.HashSet [String])
  , soacBodyInfo :: AccFlow
  }

type AccFlow = HM.HashMap String SOACInfo

flowForFun :: FunDec -> (Name, ExpFlowGraph)
flowForFun (fname, _, _, fbody, _) =
  let allInfos = execWriter $ flowForBody fbody
      usages name (consumer, info) =
        case HM.lookup name $ soacConsumed info of
          Nothing -> HS.empty
          Just ss -> HS.map (\s -> (consumer, s)) ss
      uses infos name = mconcat $ map (usages name) $ HM.toList infos
      graph infos =
        HM.fromList [ (soacname, (soacType info, users, ExpFlowGraph $ graph $ soacBodyInfo info)) |
                      (soacname, info) <- HM.toList infos,
                      let users = mconcat $ map (uses infos) $ HS.toList $ soacProduced info
                    ]
  in (fname, ExpFlowGraph $ graph allInfos)

type FlowM = Writer AccFlow

soacSeen :: VName -> [VName] -> SOAC -> FlowM ()
soacSeen name produced soac =
  tell $ HM.singleton
       (textual name)
       SOACInfo {
           soacType = desc
         , soacProduced = HS.fromList produced
         , soacConsumed =
             HM.fromListWith HS.union $ mapMaybe inspectInput $ SOAC.inputs soac
         , soacBodyInfo =
           mconcat (map (execWriter . flowForBody) bodys)
         }
  where (desc, bodys) =
          case soac of
            SOAC.Map _ l _ _  -> ("mapT", [lambdaBody l])
            SOAC.Filter _ l _ _ -> ("filterT", [lambdaBody l])
            SOAC.Scan _ l _ _ -> ("scanT", [lambdaBody l])
            SOAC.Reduce _ l _ _ -> ("reduceT", [lambdaBody l])
            SOAC.Redomap _ l1 l2 _ _ _ -> ("redomapT", [lambdaBody l1, lambdaBody l2])

        inspectInput (SOAC.Input ts (SOAC.Var v)) =
          Just (identName v, HS.singleton $ map descTransform ts)
        inspectInput (SOAC.Input _ (SOAC.Iota _)) =
          Nothing
        inspectInput (SOAC.Input ts (SOAC.Index _ v _ _)) =
          Just (identName v, HS.singleton $ "index" : map descTransform ts)

        descTransform (SOAC.Transpose {})    = "transpose"
        descTransform (SOAC.Reshape {})      = "reshape"
        descTransform (SOAC.ReshapeOuter {}) = "reshape"
        descTransform (SOAC.ReshapeInner {}) = "reshape"
        descTransform SOAC.Repeat            = "replicate"

flowForBody :: Body -> FlowM ()
flowForBody (LetPat pat e body _)
  | Right e' <- SOAC.fromExp e,
    names@(name:_) <- map identName pat = do
  soacSeen name names e'
  flowForBody body
flowForBody (LetPat pat e body _) = do
  flowForExp e
  tell $ HM.map expand $ execWriter $ flowForBody body
  where names = HS.fromList $ map identName pat
        freeInE = HS.toList $ freeNamesInExp e
        expand info =
          info { soacConsumed =
                   HM.fromList $ concatMap update $
                   HM.toList $ soacConsumed info
               }
        update (usedName, s)
          | usedName `HS.member` names =
            [ (name, HS.map ("complex":) s) | name <- freeInE ]
          | otherwise =
            [(usedName, s)]
flowForBody (DoLoop merge _ boundexp loopbody body _)
  | names@(name:_) <- map (identName . fst) merge = do
  flowForBody body
  tell $ HM.singleton
         (textual name)
         SOACInfo {
           soacType = "loop"
         , soacProduced = HS.fromList names
         , soacConsumed =
             HM.fromList
                 [ (used, HS.singleton []) |
                   used <- HS.toList
                           $ mconcat (freeNamesInBody loopbody :
                                      map freeNamesInExp
                                      (SubExp boundexp :
                                      map (SubExp . snd) merge))
                          `HS.difference` HS.fromList names
                 ]
         , soacBodyInfo = execWriter $ flowForBody loopbody
         }

flowForBody b = walkBodyM flow b

flowForExp :: Exp -> FlowM ()
flowForExp = walkExpM flow

flow :: Walker FlowM
flow = identityWalker {
         walkOnExp = flowForExp
       , walkOnBody = flowForBody
       , walkOnLambda = flowForBody . lambdaBody
       }
