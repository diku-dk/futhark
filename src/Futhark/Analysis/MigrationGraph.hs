module Futhark.Analysis.MigrationGraph where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Futhark.IR.GPU hiding (Result)

-- | A handle that identifies a specific 'Vertex'.
type Id = Int

type IdSet = IS.IntSet

-- | Maps a name to its corresponding vertex handle.
nameToId :: VName -> Id
nameToId = baseTag

-- | Maps a set of names to a list of corresponding vertex handles.
namesToIds :: Names -> [Id]
namesToIds = IM.keys . namesIntMap

-- | A measurement of how many if statement branch bodies a variable binding is
-- nested within.
type ForkDepth = Int

-- If a route passes through the edge u->v and the fork depth
--   1) increases from u to v, then u is within a conditional branch.
--   2) decreases from u to v, then v binds the result of two or more branches.

-- Metadata on the environment that a variable is declared within.
data Meta = Meta
  { -- | The fork depth of the variable. See 'ForkDepth'.
    metaForkDepth :: ForkDepth,
    -- | An id for the subgraph within which the variable exists, usually
    -- defined at the body level. A read may only be delayed to a point within
    -- its own subgraph.
    metaGraphId :: Maybe Id
  }

-- | A graph representation of some program variable.
data Vertex = Vertex
  { -- | The handle for this vertex in the graph.
    vertexId :: Id,
    -- | How many branch bodies this variable binding is nested within, and
    -- in which subgraph it exists.
    vertexMeta :: Meta,
    -- | Whether a route passes through this vertex, and from where.
    vertexRouting :: Routing,
    -- | Handles of vertices that this vertex has an edge to.
    vertexEdges :: Edges
  }

-- | Constructs a 'Vertex' with no route or edges declared.
vertex :: Id -> Meta -> Vertex
vertex i m = Vertex {
    vertexId      = i,
    vertexMeta    = m,
    vertexRouting = NoRoute,
    vertexEdges   = mempty
  }

-- | All relevant edges that have been declared from some vertex, plus
-- bookkeeping to track their exhaustion and reversal.
data Edges
  = -- | The vertex has an edge to a sink; all other declared edges are
    -- irrelevant. The edge cannot become exhausted, and it is reversed if a
    -- route passes through the vertex.
    ToSink
  | -- | All vertices that the vertex has a declared edge to, and which of
    -- those edges that are not exhausted nor reversed, if not all.
    ToNodes IdSet (Maybe IdSet)

instance Semigroup Edges where
  ToSink <> _ = ToSink
  _ <> ToSink = ToSink
  (ToNodes a1 e1) <> (ToNodes a2 e2) =
    ToNodes (a1 <> a2) (e1 <> e2)

instance Monoid Edges where
  -- The empty set of edges.
  mempty = ToNodes IS.empty Nothing

-- Creates a set of edges where no edge is reversed or exhausted.
declareEdges :: [Id] -> Edges
declareEdges is = ToNodes (IS.fromList is) Nothing

-- Like 'declareEdges' but for a single vertex.
oneEdge :: Id -> Edges
oneEdge i = ToNodes (IS.singleton i) Nothing
 
-- | Route tracking for some vertex.
-- If a route passes through the vertex then both an ingoing and an outgoing
-- edge to/from that vertex will have been reversed, and the vertex will in
-- effect have lost one edge and gained another. The gained edge will be to
-- the prior vertex along the route that passes through.
data Routing
  = -- | No route passes through the vertex, and no edges have been reversed,
    -- added, nor deleted compared to what was declared.
    NoRoute
  | -- | A route passes through the vertex, and the prior vertex is the source
    -- of that route. The edge gained by reversal is by definition exhausted.
    FromSource
  | -- | A route passes through the vertex, and this is the handle of the prior
    -- vertex. The edge gained by reversal may be exhausted.
    FromNode Id Exhaustion

-- | Whether some edge is exhausted or not. No sink can be reached via an
-- exhausted edge.
data Exhaustion = Exhausted | NotExhausted

-- | A map from vertex 'Id' to its corresponding 'Vertex'.
newtype Graph = Graph (IM.IntMap Vertex)

-- | The empty graph.
empty :: Graph
empty = Graph (IM.empty)

-- | Insert a new vertex in the graph. If its variable already is represented
-- in the graph, the existing vertex is replaced with the supplied vertex.
insert :: Vertex -> Graph -> Graph
insert v (Graph m) = Graph $ IM.insert (vertexId v) v m

-- | Adjust the vertex with this specific id. When no vertex with that id is a
-- member of the graph, the original graph is returned.
adjust :: (Vertex -> Vertex) -> Id -> Graph -> Graph
adjust f i (Graph m) = Graph $ IM.adjust f i m

-- | Connect the vertex with this id to a sink. When no vertex with that id is a
-- member of the graph, the original graph is returned.
connectToSink :: Id -> Graph -> Graph
connectToSink = adjust $ \v -> v { vertexEdges = ToSink }

-- | Add these edges to the vertex with this id. When no vertex with that id is
-- a member of the graph, the original graph is returned.
addEdges :: Edges -> Id -> Graph -> Graph
addEdges es = adjust $ \v -> v { vertexEdges = es <> vertexEdges v }

-- | Does a vertex for the given id exist in the graph?
member :: Id -> Graph -> Bool
member i (Graph m) = IM.member i m

-- | Returns the vertex from the graph with the given id.
get :: Id -> Graph -> Maybe Vertex
get i (Graph m) = IM.lookup i m



-- | Searches the edge u->v where u might be a source. If u is not a source then
-- the type of the edge that connects u to v is also provided.
type EdgeSearcher m = Maybe (Vertex, EdgeType) -> Vertex -> m Search

-- | Control flow of a graph search.
data Search
  = Continue -- ^ No result was found in this branch of the search tree.
  | Complete -- ^ The search was successful.

-- | Continue the search.
continue :: Monad m => m Search
continue = return Continue

-- | @a `andThen` b@ first searches @a@, and if no result is found, continues
-- to search @b@.
andThen :: Monad m => m Search -> m Search -> m Search
andThen first next = do
  s <- first
  case s of
    Continue -> next
    _        -> return s

-- | Whether a vertex is reached via a normal or reversed edge.
data EdgeType = Normal | Reversed

-- | Search the non-source, non-sink vertices that some vertex has an edge to,
-- returning as soon a result is found or when the search space has been
-- exhausted.
searchEdges :: Monad m
            => EdgeSearcher m
            -> EdgeType -- How the vertex is accessed.
            -> Vertex   -- The vertex.
            -> Graph    -- The graph that contains the vertex and its edges.
            -> m Search
searchEdges search et u g =
  case (et, vertexRouting u) of
    (Normal,   FromSource  ) -> continue
    (Normal,   FromNode i _) -> searchVertex (search $ Just (u, Reversed)) i g
    (Reversed, FromNode i _) -> do
      searchNormals `andThen` searchVertex (search $ Just (u, Reversed)) i g
    _ -> searchNormals
  where
    searchNormals
      | ToSink       <- vertexEdges u = continue
      | ToNodes es _ <- vertexEdges u = searchNorms es

    searchNorms es =
      case IS.minView es of
        Nothing       -> continue
        Just (i, es') -> do
          let searchEdge = searchVertex (search $ Just (u, Normal)) i g
          searchEdge `andThen` searchNorms es'

-- | Search the vertex with the given id in the graph.
searchVertex :: Monad m
             => (Vertex -> m Search)
             -> Id
             -> Graph
             -> m Search
searchVertex f i g
  | Just v <- get i g = f v
  | otherwise         = continue


-- | @routeMany srcs g@ attempts to create a route in @g@ from every vertex in
-- in @srcs@. Returns the ids for the last vertices along the created routes,
-- and an updated graph.
routeMany :: [Id] -> Graph -> ([Id], Graph)
routeMany srcs graph =
  foldl' f ([], graph) srcs
  where
    f (snks, g) src =
      case route src g of
        (Nothing, g') -> (snks, g')
        (Just snk, g') -> (snk:snks, g')

-- | @route src g@ attempts to find a path in @g@ from the source connected
-- vertex @src@. If a sink is found, all edges along the path will be reversed
-- to create a route, and the id of the vertex that connect to the sink is
-- returned.
route :: Id -> Graph -> (Maybe Id, Graph)
route i g = (Nothing, g) -- TODO




{-
-- | A set of vertices visited by a graph traversal. Used to detect cycles.
type Visited = IdSet

-- | Visits all vertices reachable from some vertex, including the vertex
-- itself. All edges will be traversed, also those that are exhausted.
-- Each vertex will be visited at most once.
walk :: Monad m =>
     -> Commander m -- Callback to control traversal.
     -> Visited     -- Vertices visited so far.
     -> Id          -- Id of the previous vertex.
     -> EdgeType    -- The kind of edge this vertex is reached via.
     -> Id          -- Id of this vertex.
     -> Graph       -- The graph.
     -> m (Command, Visited)
walk _ vs _ _ i g
  | IS.member i vs
  = return (Continue, g, vs)
walk _ vs _ _ i g
  | Nothing <- get i g
  = return (Continue, g, vs)
walk f vs prev et i g
  | Just v <- get i g
  = do cmd <- f prev et v
       let vs' = IS.insert i vs
       case cmd of
         Complete  -> return (Complete, g, vs')
         Backtrack -> return (Continue, g, vs')
         Continue  -> walkVertex f vs' prev et v g




-- The result of searching a vertex via an edge.
data SearchResult
  -- | Exhausted search via this edge.
  = DeadEnd
  -- | Search had to backtrack because a cycle was detected. The first set is
  -- all members of the cycle(s) that search backtracked from, the second set
  -- is the cycle members that search have yet to backtrack from.
  | CycleDetected IdSet IdSet
  -- Search had to backtrack for another reason.
  | EarlyReturn
  -- | A path was found to this sink via the edge.
  | SinkFound Id
  -- | Search completed with some other result.
  | Abort




search :: Monad m =>
       -> SearchType    -- Whether to traverse exhausted edges.
       -> Commander m a -- Callback to control search.
       -> Visited       -- Vertices visited by the search so far.
       -> Id            -- Id of the previous vertex.
       -> EdgeType      -- The kind of edge this vertex is reached via.
       -> Id            -- Id of this vertex.
       -> Graph         -- The graph.
       -> m (SearchResult a, Graph, Visited)
search _ _ vs _ _ i g
  | IS.member i vs
  = let res = CycleDetected IS.empty (IS.singleton i)
    in return (res, g, IS.insert i vs)
search _ _ vs _ _ i g
  | Nothing <- get i g
  = (DeadEnd, g, IS.insert i vs)
search sType f vs prev et i g
  | Just v <- get i g
  = do cmd <- f prev et v
       let vs' = IS.insert i vs
       case cmd of
         Complete r -> return (Result r, g, vs')
         Backtrack  -> return (EarlyReturn, g, vs')
         Continue   -> searchVertex sType f vs' prev et v g






searchVertex :: Monad m =>
             -> SearchType    -- Whether to traverse exhausted edges.
             -> Commander m a -- Callback to control search.
             -> Visited       -- Vertices visited so far, incl. this
             -> Id            -- Id of the previous vertex.
             -> EdgeType      -- The kind of edge this vertex is reached via.
             -> Vertex        -- This vertex.
             -> Graph         -- The graph.
             -> m (SearchResult a, Graph, Visited)
searchVertex sType f vs prev Normal v g =
  let i = vertexId v in
  case vertexRouting v of
    FromSource ->
      return (DeadEnd, g, vs)
    FromNode _ Exhausted | NonExhausted <- sType ->
      return (DeadEnd, g, vs)
    FromNode from _ -> do
      (res, g', vs') <- search sType f vs i Reversed from g
      case res of
        DeadEnd ->
          let v'  = v { vertexRouting = FromNode from Exhausted }
              g'' = insert v' g'
          in return (res, g'', vs')
        CycleDetected bf bh ->
          let bh' = bh
        SinkFound _ ->
          let v'  = v { vertexRouting = FromNode prev NotExhausted }
              g'' = insert v' g'
          in return (res, g'', vs')
        _ -> return (res, g', vs')

    NoRoute -> 

  = DeadEnd
  -- | Search had to backtrack because a cycle was detected. The first set is
  -- all members of the cycle(s) that search backtracked from, the second set
  -- is the cycle members that search have yet to backtrack from.
  | CycleDetected IdSet IdSet
  -- Search had to backtrack for another reason.
  | EarlyReturn
  -- | A path was found to this sink via the edge.
  | SinkFound Id
  -- | Search completed with some other result.
  | Result a

vertexWalk :: Id -> EdgeType -> Vertex -> Vertex
vertexWalk prev Normal v
  | NoRoute <- vertexRouting v
  = -- All edges
vertexWalk prev Normal v
  | FromSource <- vertexRouting v
  = -- no edges
vertexWalk prev Normal v
  | FromNode _ Exhausted <- vertexRouting v
  = -- no edges
vertexWalk prev Normal v
  | FromNode from NotExhausted <- vertexRouting v
  = -- from is only edge
vertexWalk prev Reversed v
  | NoRoute <- vertexRouting v
  = 
vertexWalk prev Reversed v
  | FromSource <- vertexRouting v
  =
vertexWalk prev Reversed v
  | FromNode _ Exhausted <- vertexRouting v
  =
vertexWalk prev Reversed v
  | FromNode from NotExhausted <- vertexRouting v
  =


-}


















