{-|
This module contains the type definitions and basic operations for the graph
that "Futhark.Analysis.MigrationTable" internally uses to construct a migration
table.

= Overview

The 'Graph' type is a data flow dependency graph of program variables, each
variable represented by a 'Vertex'. A vertex may have edges to other vertices
or to a sink, which is a special vertex with no graph representation. Each edge
to a vertex is either from another vertex or from a source, which is another
special vertex with no graph representation.

The primary graph operation provided by this module is 'route'. Given the vertex
that some unspecified source has an edge to, a path is attempted found to a
sink. If a sink can be reached from the source, all edges along the path is
reversed. The path in the opposite direction of reversed edges from a source to
some sink is (also) called a route.

When routing fails to find a sink in some subgraph reached via an edge then that
edge is marked exhausted. No sink can be reached via an exhausted edge, and any
subsequent routing attempt will skip pathfinding along such edge.
-}
module Futhark.Analysis.MigrationTable.Graph (
  -- * Types
  Graph,
  Id,
  IdSet,
  Vertex (..),
  Meta (..),
  ForkDepth,
  Routing (..),
  Exhaustion (..),
  Edges (..),

  -- * Construction
  empty,
  nameToId,
  namesToIds,
  vertex,
  declareEdges,
  oneEdge,

  -- * Insertion
  insert,

  -- * Update
  adjust,
  connectToSink,
  addEdges,

  -- * Query
  member,
  get,

  -- * Routing
  route,
  routeMany,
) where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe (fromJust)
import Futhark.IR.GPU hiding (Result)


{- TYPES -}


-- | A data flow dependency graph of program variables, each variable
-- represented by a 'Vertex'.
newtype Graph = Graph (IM.IntMap Vertex)

-- | A handle that identifies a specific 'Vertex'.
type Id = Int

-- | A set of 'Id's.
type IdSet = IS.IntSet

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

-- | Metadata on the environment that a variable is declared within.
data Meta = Meta
  { -- | The fork depth of the variable.
    metaForkDepth :: ForkDepth,
    -- | An id for the subgraph within which the variable exists, usually
    -- defined at the body level. A read may only be delayed to a point within
    -- its own subgraph.
    metaGraphId :: Maybe Id
  }

-- | A measurement of how many if statement branch bodies a variable binding is
-- nested within.
type ForkDepth = Int

-- If a route passes through the edge u->v and the fork depth
--   1) increases from u to v, then u is within a conditional branch.
--   2) decreases from u to v, then v binds the result of two or more branches.
 
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
    -- vertex. The edge gained by reversal may be exhausted. Routing assumes
    -- that at most one 'FromNode' routing exists to each vertex in a graph.
    FromNode Id Exhaustion

-- | Whether some edge is exhausted or not. No sink can be reached via an
-- exhausted edge.
data Exhaustion = Exhausted | NotExhausted

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


{- CONSTRUCTION -}


-- | The empty graph.
empty :: Graph
empty = Graph (IM.empty)

-- | Maps a name to its corresponding vertex handle.
nameToId :: VName -> Id
nameToId = baseTag

-- | Maps a set of names to a list of corresponding vertex handles.
namesToIds :: Names -> [Id]
namesToIds = IM.keys . namesIntMap

-- | Constructs a 'Vertex' without any edges.
vertex :: Id -> Meta -> Vertex
vertex i m = Vertex {
    vertexId      = i,
    vertexMeta    = m,
    vertexRouting = NoRoute,
    vertexEdges   = mempty
  }

-- | Creates a set of edges where no edge is reversed or exhausted.
declareEdges :: [Id] -> Edges
declareEdges is = ToNodes (IS.fromList is) Nothing

-- | Like 'declareEdges' but for a single vertex.
oneEdge :: Id -> Edges
oneEdge i = ToNodes (IS.singleton i) Nothing


{- INSERTION -}


-- | Insert a new vertex in the graph. If its variable already is represented
-- in the graph, the existing vertex is replaced with the supplied vertex.
insert :: Vertex -> Graph -> Graph
insert v (Graph m) = Graph $ IM.insert (vertexId v) v m


{- UPDATE -}


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


{- QUERY -}


-- | Does a vertex for the given id exist in the graph?
member :: Id -> Graph -> Bool
member i (Graph m) = IM.member i m

-- | Returns the vertex with the given id.
get :: Id -> Graph -> Maybe Vertex
get i (Graph m) = IM.lookup i m


{- ROUTING -}


-- | @route src g@ attempts to find a path in @g@ from the source connected
-- vertex with id @src@. If a sink is found, all edges along the path will be
-- reversed to create a route, and the id of the vertex that connects to the
-- sink is returned.
route :: Id -> Graph -> (Maybe Id, Graph)
route src g =
  case route' IM.empty 0 Nothing Normal src g of
    (DeadEnd, g')       -> (Nothing,  g')
    (SinkFound snk, g') -> (Just snk, g')
    _                   -> error
      "Unexpected error occurred in Futhark.Analysis.MigrationTable.Graph"

-- | @routeMany srcs g@ attempts to create a 'route' in @g@ from every vertex
-- in @srcs@. Returns the ids for the vertices connected to all found sinks.
routeMany :: [Id] -> Graph -> ([Id], Graph)
routeMany srcs graph =
  foldl' f ([], graph) srcs
  where
    f (snks, g) src =
      case route src g of
        (Nothing, g') -> (snks, g')
        (Just snk, g') -> (snk:snks, g')



{- INTERNALS BELOW -}



-- | A set of vertices visited by a graph traversal, and at what depth they were
-- encountered. Used to detect cycles.
type Pending = IM.IntMap Depth

-- | Search depth. Distance to some vertex from some root.
type Depth = Int

-- | The outcome of attempted to find a route through a vertex.
data RoutingResult
  -- | No sink could be reached through this vertex.
  = DeadEnd
  -- | A cycle was detected. A sink can be reached through this vertex if a sink
  -- can be reached from the vertex at this depth. If no sink can be reached
  -- from the vertex at this depth, then the graph should be updated by these
  -- actions. Until the vertex is reached, the status of these vertices are
  -- pending.
  | CycleDetected Depth [Graph -> Graph] Pending
  -- | A sink was found. This is the id of the vertex connected to it.
  | SinkFound Id

instance Semigroup RoutingResult where
  SinkFound i <> _ = SinkFound i
  _ <> SinkFound i = SinkFound i
  CycleDetected d1 as1 _ <> CycleDetected d2 as2 p2 =
    CycleDetected (min d1 d2) (as1 ++ as2) p2
  _ <> CycleDetected d as p = CycleDetected d as p
  CycleDetected d as p <> _ = CycleDetected d as p
  DeadEnd <> DeadEnd = DeadEnd

instance Monoid RoutingResult where
  mempty = DeadEnd

-- | Whether a vertex is reached via a normal or reversed edge.
data EdgeType = Normal | Reversed

route' :: Pending
       -> Depth
       -> Maybe Id
       -> EdgeType
       -> Id
       -> Graph
       -> (RoutingResult, Graph)
route' p d prev et i g
  | Just d' <- IM.lookup i p
  = let foundCycle = (CycleDetected d' [] p, g) in
    case et of
      -- Accessing some vertex v via a normal edge corresponds to accessing v_in
      -- via a normal edge. If v_in has a reversed edge then that is the only
      -- edge that is available. Otherwise v_in has a single edge to v_out
      -- whose edges then are available.
      -- All edges available via this edge were already available via the edge
      -- that first reached the vertex.
      Normal -> foundCycle
      -- Accessing some vertex v via a reversed edge corresponds to accessing
      -- v_out via a reversed edge. All other edges of v_out are available, and
      -- the edge from v_in to v_out has been reversed, implying that v_in has
      -- a single reversed edge that also is available.
      -- There exists at most one reversed edge to each vertex. Since this
      -- vertex was reached via one, and the vertex already have been reached,
      -- then the first reach must have been via a normal edge that only could
      -- traverse a reversed edge. The reversed edge from v_out to v_in thus
      -- completes a cycle, but a sink might be reachable via all the other
      -- edges from v_out.
      -- The depth for the vertex need not be updated as this is the only
      -- reversed edge and 'prev' already is in the 'Pending' map. It follows
      -- that no (new) cycle can start here.
      Reversed -> let (res, g') = routeNormals (fromJust $ get i g) g p
                  in (fst foundCycle <> res, g')
  | Just v <- get i g
  = routeVertex v
  | otherwise
  = backtrack

  where
    backtrack = (DeadEnd, g)

    routeVertex v =
      case (et, vertexRouting v) of
        (Normal,   FromSource)           -> backtrack
        (Normal,   FromNode _ Exhausted) -> backtrack
        (Normal,   FromNode rev _) -> entry (routeReversed rev)
        (Reversed, FromNode rev _) -> entry (routeAll rev v)
        _                          -> entry (routeNormals v)

    entry f =
      let (res, g0) = f g (IM.insert i d p)
      in case res of
           CycleDetected d' as _ | d == d'
             -> (DeadEnd, foldl' (\g1 a -> a g1) g0 as)
           _ -> (res, g0)

    routeAll rev v g0 p0 =
      let (res, g1) = routeNormals v g0 p0
      in case res of
           DeadEnd
             -> routeReversed rev g1 p0
           CycleDetected _ _ p1
             -> let (res', g2) = routeReversed rev g1 p1
                in (res <> res', g2)
           SinkFound _
             -> (res, g1)

    routeReversed rev g0 p0 =
      let (res, g') = route' p0 (d+1) (Just i) Reversed rev g0
          exhaust = (flip adjust) i $ \v -> 
            v { vertexRouting = FromNode rev Exhausted }
      in case (res, et) of
           (DeadEnd, _)
             -> (res, exhaust g')
           (CycleDetected d' as p', _)
             -> (CycleDetected d' (exhaust:as) p', g')
           (SinkFound _, Normal)
             -> (res, setRoute g')
           (SinkFound _, Reversed)
             -> (res, adjust (\v -> v { vertexEdges = withPrev (vertexEdges v),
                                        vertexRouting = NoRoute }) i g')

    setRoute g' = adjust (\v -> v { vertexRouting = routing }) i g'

    routing =
      case prev of
        Nothing -> FromSource
        Just i' -> FromNode i' NotExhausted

    withPrev edges
      | Just i' <- prev
      , ToNodes es (Just es') <- edges
      = ToNodes es (Just $ IS.insert i' es')

      | otherwise -- should never happen
      = edges

    routeNormals v g0 p0
      | ToSink <- vertexEdges v
      -- There cannot be a reversed edge to a vertex with an edge to a sink.
      = (SinkFound i, setRoute g0)

      | ToNodes es nx <- vertexEdges v
      = let (res, g', nx') =
              case nx of
                Just es' -> routeNorms (IS.toAscList es') g0 p0
                Nothing  -> routeNorms (IS.toAscList es ) g0 p0
            edges = ToNodes es (Just $ IS.fromDistinctAscList nx')
            exhaust = (flip adjust) i $ \v' -> 
              v' { vertexEdges = ToNodes es (Just IS.empty) }
        in case (res, et) of
             (DeadEnd, _)
               -> (res, exhaust g')
             (CycleDetected d' as p', _)
               -> let res' = CycleDetected d' (exhaust:as) p'
                      v'   = v { vertexEdges = edges }
                  in (res', insert v' g')
             (SinkFound _, Normal)
               -> let v' = v { vertexEdges = edges, vertexRouting = routing }
                  in (res, insert v' g')
             (SinkFound _, Reversed)
               -> let v' = v { vertexEdges = withPrev edges }
                  in (res, insert v' g')

    routeNorms []     g0 _  = (DeadEnd, g0, [])
    routeNorms (e:es) g0 p0 =
      let (res, g1) = route' p0 (d+1) (Just i) Normal e g0 in
      case res of
        DeadEnd              -> routeNorms es g1 p0
        SinkFound _          -> (res, g1, es)
        CycleDetected _ _ p1 -> let (res', g2, es') = routeNorms es g1 p1
                                in (res <> res', g2, e:es')







{-

-- Produces a value from a vertex given the type of edge it is accessed via and
-- the combined value of all edge vertices, excluding those that are supersets
-- of the value that will be produced. The latter may occur in case of cycles.
type Reducer a = a -> EdgeType -> Vertex -> a

-- | Search the graph for a sink, starting with the vertex of the provided id.
-- All edges will be traversed, even the exhausted, and the visited vertices
-- will 
search :: Monoid a  -- associative <>
       => Reducer a    -- map vertex to a
       -> a            -- initial a
       -> EdgeType
       -> Id
       -> Graph
       -> Result a
searchAll s et i g = error



data Result a
  = Produced a
  | FoundSink

-- | A path to some vertex. The head is the vertex that was reached, along with
-- the type of edge is was found via.
type Path [(EdgeType, Id)]

-- 


-- Cycle of values all produce the same result





-- | Searches the edge u->v where u is absent if it is a source.
type EdgeSearcher m = Maybe Vertex -> EdgeType -> Vertex -> m Search

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
    (Normal,   FromNode i _) -> searchVertex (search (Just u) Reversed) i g
    (Reversed, FromNode i _) -> do
      searchNormals `andThen` searchVertex (search (Just u) Reversed) i g
    _ -> searchNormals
  where
    searchNormals
      | ToSink       <- vertexEdges u = continue
      | ToNodes es _ <- vertexEdges u = searchNorms es

    searchNorms es =
      case IS.minView es of
        Nothing       -> continue
        Just (i, es') -> do
          let searchEdge = searchVertex (search (Just u) Normal) i g
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


-}





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