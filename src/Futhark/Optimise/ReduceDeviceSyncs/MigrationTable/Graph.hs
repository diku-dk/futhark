-- | This module contains the type definitions and basic operations
-- for the graph that
-- "Futhark.Optimise.ReduceDeviceSyncs.MigrationTable" internally uses
-- to construct a migration table.  It is however completely
-- Futhark-agnostic and implements a generic graph abstraction.
--
-- = Overview
--
-- The 'Graph' type is a data flow dependency graph of program variables, each
-- variable represented by a 'Vertex'. A vertex may have edges to other vertices
-- or to a sink, which is a special vertex with no graph representation. Each
-- edge to a vertex is either from another vertex or from a source, which also
-- is a special vertex with no graph representation.
--
-- The primary graph operation provided by this module is 'route'. Given the
-- vertex that some unspecified source has an edge to, a path is attempted
-- found to a sink. If a sink can be reached from the source, all edges along
-- the path are reversed. The path in the opposite direction of reversed edges
-- from a source to some sink is a route.
--
-- Routes can be used to find a minimum vertex cut in the graph through what
-- amounts to a specialized depth-first search implementation of the
-- Ford-Fulkerson method. When viewed in this way each graph edge has a capacity
-- of 1 and the reversing of edges to create routes amounts to sending reverse
-- flow through a residual network (the current state of the graph).
-- The max-flow min-cut theorem allows one to determine a minimum edge cut that
-- separates the sources and sinks.
--
-- If each vertex @v@ in the graph is viewed as two vertices, @v_in@ and
-- @v_out@, with all ingoing edges to @v@ going to @v_in@, all outgoing edges
-- from @v@ going from @v_out@, and @v_in@ connected to @v_out@ with a single
-- edge, then the minimum edge cut of the view amounts to a minimum vertex cut
-- in the actual graph. The view need not be manifested as whether @v_in@ or
-- @v_out@ is reached by an edge to @v@ can be determined from whether that edge
-- is reversed or not. The presence of an outgoing, reversed edge also gives the
-- state of the virtual edge that connects @v_in@ to @v_out@.
--
-- When routing fails to find a sink in some subgraph reached via an edge then
-- that edge is marked exhausted. No sink can be reached via an exhausted edge,
-- and any subsequent routing attempt will skip pathfinding along such edge.
module Futhark.Optimise.ReduceDeviceSyncs.MigrationTable.Graph
  ( -- * Types
    Graph,
    Id,
    IdSet,
    Vertex (..),
    Routing (..),
    Exhaustion (..),
    Edges (..),
    EdgeType (..),
    Visited,
    Result (..),

    -- * Construction
    empty,
    vertex,
    declareEdges,
    oneEdge,
    none,

    -- * Insertion
    insert,

    -- * Update
    adjust,
    connectToSink,
    addEdges,

    -- * Query
    member,
    lookup,
    isSinkConnected,

    -- * Routing
    route,
    routeMany,

    -- * Traversal
    fold,
    reduce,
  )
where

import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
--                                   TYPES                                    --
--------------------------------------------------------------------------------

-- | A data flow dependency graph of program variables, each variable
-- represented by a 'Vertex'.
newtype Graph m = Graph (IM.IntMap (Vertex m))

-- | A handle that identifies a specific 'Vertex'.
type Id = Int

-- | A set of 'Id's.
type IdSet = IS.IntSet

-- | A graph representation of some program variable.
data Vertex m = Vertex
  { -- | The handle for this vertex in the graph.
    vertexId :: Id,
    -- | Custom data associated with the variable.
    vertexMeta :: m,
    -- | Whether a route passes through this vertex, and from where.
    vertexRouting :: Routing,
    -- | Handles of vertices that this vertex has an edge to.
    vertexEdges :: Edges
  }

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
  deriving (Show, Eq, Ord)

-- | Whether some edge is exhausted or not. No sink can be reached via an
-- exhausted edge.
data Exhaustion = Exhausted | NotExhausted
  deriving (Show, Eq, Ord)

-- | All relevant edges that have been declared from some vertex, plus
-- bookkeeping to track their exhaustion and reversal.
data Edges
  = -- | The vertex has an edge to a sink; all other declared edges are
    -- irrelevant. The edge cannot become exhausted, and it is reversed if a
    -- route passes through the vertex (@vertexRouting v /= NoRoute@).
    ToSink
  | -- | All vertices that the vertex has a declared edge to, and which of
    -- those edges that are not exhausted nor reversed, if not all.
    ToNodes IdSet (Maybe IdSet)
  deriving (Show, Eq, Ord)

instance Semigroup Edges where
  ToSink <> _ = ToSink
  _ <> ToSink = ToSink
  (ToNodes a1 Nothing) <> (ToNodes a2 Nothing) =
    ToNodes (a1 <> a2) Nothing
  (ToNodes a1 (Just e1)) <> (ToNodes a2 Nothing) =
    ToNodes (a1 <> a2) $ Just (e1 <> IS.difference a2 a1)
  (ToNodes a1 Nothing) <> (ToNodes a2 (Just e2)) =
    ToNodes (a1 <> a2) $ Just (e2 <> IS.difference a1 a2)
  (ToNodes a1 (Just e1)) <> (ToNodes a2 (Just e2)) =
    let a = IS.difference e2 (IS.difference a1 e1)
        b = IS.difference e1 (IS.difference a2 e2)
     in ToNodes (a1 <> a2) $ Just (a <> b)

instance Monoid Edges where
  -- The empty set of edges.
  mempty = ToNodes IS.empty Nothing

-- | Whether a vertex is reached via a normal or reversed edge.
data EdgeType = Normal | Reversed
  deriving (Eq, Ord)

-- | State that tracks which vertices a traversal has visited, caching immediate
-- computations.
newtype Visited a = Visited {visited :: M.Map (EdgeType, Id) a}

-- | The result of a graph traversal that may abort early in case a sink is
-- reached.
data Result a
  = -- | The traversal finished without encountering a sink, producing this
    -- value.
    Produced a
  | -- | The traversal was aborted because a sink was reached.
    FoundSink
  deriving (Eq)

instance (Semigroup a) => Semigroup (Result a) where
  FoundSink <> _ = FoundSink
  _ <> FoundSink = FoundSink
  Produced x <> Produced y = Produced (x <> y)

--------------------------------------------------------------------------------
--                                CONSTRUCTION                                --
--------------------------------------------------------------------------------

-- | The empty graph.
empty :: Graph m
empty = Graph IM.empty

-- | Constructs a 'Vertex' without any edges.
vertex :: Id -> m -> Vertex m
vertex i m =
  Vertex
    { vertexId = i,
      vertexMeta = m,
      vertexRouting = NoRoute,
      vertexEdges = mempty
    }

-- | Creates a set of edges where no edge is reversed or exhausted.
declareEdges :: [Id] -> Edges
declareEdges is = ToNodes (IS.fromList is) Nothing

-- | Like 'declareEdges' but for a single vertex.
oneEdge :: Id -> Edges
oneEdge i = ToNodes (IS.singleton i) Nothing

-- | Initial 'Visited' state before any vertex has been visited.
none :: Visited a
none = Visited M.empty

--------------------------------------------------------------------------------
--                                 INSERTION                                  --
--------------------------------------------------------------------------------

-- | Insert a new vertex into the graph. If its variable already is represented
-- in the graph, the original graph is returned.
insert :: Vertex m -> Graph m -> Graph m
insert v (Graph m) = Graph $ IM.insertWith const (vertexId v) v m

--------------------------------------------------------------------------------
--                                   UPDATE                                   --
--------------------------------------------------------------------------------

-- | Adjust the vertex with this specific id. When no vertex with that id is a
-- member of the graph, the original graph is returned.
adjust :: (Vertex m -> Vertex m) -> Id -> Graph m -> Graph m
adjust f i (Graph m) = Graph $ IM.adjust f i m

-- | Connect the vertex with this id to a sink. When no vertex with that id is a
-- member of the graph, the original graph is returned.
connectToSink :: Id -> Graph m -> Graph m
connectToSink = adjust $ \v -> v {vertexEdges = ToSink}

-- | Add these edges to the vertex with this id. When no vertex with that id is
-- a member of the graph, the original graph is returned.
addEdges :: Edges -> Id -> Graph m -> Graph m
addEdges es = adjust $ \v -> v {vertexEdges = es <> vertexEdges v}

--------------------------------------------------------------------------------
--                                   QUERY                                    --
--------------------------------------------------------------------------------

-- | Does a vertex for the given id exist in the graph?
member :: Id -> Graph m -> Bool
member i (Graph m) = IM.member i m

-- | Returns the vertex with the given id.
lookup :: Id -> Graph m -> Maybe (Vertex m)
lookup i (Graph m) = IM.lookup i m

-- | Returns whether a vertex with the given id exists in the
-- graph and is connected directly to a sink.
isSinkConnected :: Id -> Graph m -> Bool
isSinkConnected i g =
  maybe False ((ToSink ==) . vertexEdges) (lookup i g)

--                                  ROUTING                                   --
--------------------------------------------------------------------------------

-- | @route src g@ attempts to find a path in @g@ from the source connected
-- vertex with id @src@. If a sink is found, all edges along the path will be
-- reversed to create a route, and the id of the vertex that connects to the
-- sink is returned.
route :: Id -> Graph m -> (Maybe Id, Graph m)
route src g =
  case route' IM.empty 0 Nothing Normal src g of
    (DeadEnd, g') -> (Nothing, g')
    (SinkFound snk, g') -> (Just snk, g')
    (CycleDetected {}, _) ->
      error
        "Routing did not escape cycle in Futhark.Analysis.MigrationTable.Graph."

-- | @routeMany srcs g@ attempts to create a 'route' in @g@ from every vertex
-- in @srcs@. Returns the ids for the vertices connected to each found sink.
routeMany :: [Id] -> Graph m -> ([Id], Graph m)
routeMany srcs graph =
  L.foldl' f ([], graph) srcs
  where
    f (snks, g) src =
      case route src g of
        (Nothing, g') -> (snks, g')
        (Just snk, g') -> (snk : snks, g')

--------------------------------------------------------------------------------
--                                 TRAVERSAL                                  --
--------------------------------------------------------------------------------

-- | @fold g f (a, vs) et i@ folds @f@ over the vertices in @g@ that can be
-- reached from the vertex with handle @i@ accessed via an edge of type @et@.
-- Each vertex @v@ may be visited up to two times, once for each type of edge
-- @e@ pointing to it, and each time @f a e v@ is evaluated to produce an
-- updated @a@ value to be used in future @f@ evaluations or to be returned.
-- The @vs@ set records which @f a e v@ evaluations already have taken place.
-- The function returns an updated 'Visited' set recording the evaluations it
-- has performed.
fold ::
  Graph m ->
  (a -> EdgeType -> Vertex m -> a) ->
  (a, Visited ()) ->
  EdgeType ->
  Id ->
  (a, Visited ())
fold g f (res, vs) et i
  | M.notMember (et, i) (visited vs),
    Just v <- lookup i g =
      let res' = f res et v
          vs' = Visited $ M.insert (et, i) () (visited vs)
          st = (res', vs')
       in case (et, vertexRouting v) of
            (Normal, FromSource) -> st
            (Normal, FromNode rev _) -> foldReversed st rev
            (Reversed, FromNode rev _) -> foldAll st rev (vertexEdges v)
            _ -> foldNormals st (vertexEdges v)
  | otherwise =
      (res, vs)
  where
    foldReversed st = fold g f st Reversed

    foldAll st rev es = foldReversed (foldNormals st es) rev

    foldNormals st ToSink = st
    foldNormals st (ToNodes es _) =
      IS.foldl' (\s -> fold g f s Normal) st es

-- | @reduce g r vs et i@ returns 'FoundSink' if a sink can be reached via the
-- vertex @v@ with id @i@ in @g@. Otherwise it returns 'Produced' @(r x et v)@
-- where @x@ is the '<>' aggregate of all values produced by reducing the
-- vertices that are available via the edges of @v@.
-- @et@ identifies the type of edge that @v@ is accessed by and thereby which
-- edges of @v@ that are available. @vs@ caches reductions of vertices that
-- previously have been visited in the graph.
--
-- The reduction of a cyclic reference resolves to 'mempty'.
reduce ::
  (Monoid a) =>
  Graph m ->
  (a -> EdgeType -> Vertex m -> a) ->
  Visited (Result a) ->
  EdgeType ->
  Id ->
  (Result a, Visited (Result a))
reduce g r vs et i
  | Just res <- M.lookup (et, i) (visited vs) =
      (res, vs)
  | Just v <- lookup i g =
      reduceVertex v
  | otherwise =
      (Produced mempty, vs) -- shouldn't happen
  where
    reduceVertex v =
      let (res, vs') = reduceEdges v
       in case res of
            Produced x -> cached (Produced $ r x et v) vs'
            FoundSink -> cached res vs'

    cached res vs0 =
      let vs1 = Visited (M.insert (et, i) res $ visited vs0)
       in (res, vs1)

    reduceEdges v =
      case (et, vertexRouting v) of
        (Normal, FromSource) -> (Produced mempty, vs)
        (Normal, FromNode rev _) -> entry (reduceReversed rev)
        (Reversed, FromNode rev _) -> entry (reduceAll rev $ vertexEdges v)
        _ -> entry (reduceNormals $ vertexEdges v)

    -- Handle cycles
    entry f = f $ Visited $ M.insert (et, i) (Produced mempty) (visited vs)

    reduceReversed rev vs' = reduce g r vs' Reversed rev

    reduceAll rev es vs0 =
      let (res, vs1) = reduceNormals es vs0
       in case res of
            Produced _ ->
              let (res', vs2) = reduceReversed rev vs1
               in (res <> res', vs2)
            FoundSink -> (res, vs1)

    reduceNormals ToSink vs' = (FoundSink, vs')
    reduceNormals (ToNodes es _) vs' = reduceNorms mempty (IS.elems es) vs'

    reduceNorms x [] vs0 = (Produced x, vs0)
    reduceNorms x (e : es) vs0 =
      let (res, vs1) = reduce g r vs0 Normal e
       in case res of
            Produced y -> reduceNorms (x <> y) es vs1
            FoundSink -> (res, vs1)

--------------------------------------------------------------------------------
--                             ROUTING INTERNALS                              --
--------------------------------------------------------------------------------

-- | A set of vertices visited by a graph traversal, and at what depth they were
-- encountered. Used to detect cycles.
type Pending = IM.IntMap Depth

-- | Search depth. Distance to some vertex from some search root.
type Depth = Int

-- | The outcome of attempted to find a route through a vertex.
data RoutingResult a
  = -- | No sink could be reached through this vertex.
    DeadEnd
  | -- | A cycle was detected. A sink can be reached through this vertex if a
    -- sink can be reached from the vertex at this depth. If no sink can be
    -- reached from the vertex at this depth, then the graph should be updated
    -- by these actions. Until the vertex is reached, the status of these
    -- vertices are pending.
    CycleDetected Depth [Graph a -> Graph a] Pending
  | -- | A sink was found. This is the id of the vertex connected to it.
    SinkFound Id

instance Semigroup (RoutingResult a) where
  SinkFound i <> _ = SinkFound i
  _ <> SinkFound i = SinkFound i
  CycleDetected d1 as1 _ <> CycleDetected d2 as2 p2 =
    CycleDetected (min d1 d2) (as1 ++ as2) p2
  _ <> CycleDetected d as p = CycleDetected d as p
  CycleDetected d as p <> _ = CycleDetected d as p
  DeadEnd <> DeadEnd = DeadEnd

instance Monoid (RoutingResult a) where
  mempty = DeadEnd

route' ::
  Pending ->
  Depth ->
  Maybe Id ->
  EdgeType ->
  Id ->
  Graph m ->
  (RoutingResult m, Graph m)
route' p d prev et i g
  | Just d' <- IM.lookup i p =
      let found_cycle = (CycleDetected d' [] p, g)
       in case et of
            -- Accessing some vertex v via a normal edge corresponds to accessing
            -- v_in via a normal edge. If v_in has a reversed edge then that is
            -- the only outgoing edge that is available.
            -- All outgoing edges available via this ingoing edge were already
            -- available via the edge that first reached the vertex.
            Normal -> found_cycle
            -- Accessing some vertex v via a reversed edge corresponds to
            -- accessing v_out via a reversed edge. All other edges of v_out are
            -- available, and the edge from v_in to v_out has been reversed,
            -- implying that v_in has a single reversed edge that also is
            -- available.
            -- There exists at most one reversed edge to each vertex. Since this
            -- vertex was reached via one, and the vertex already have been
            -- reached, then the first reach must have been via a normal edge
            -- that only could traverse a reversed edge. The reversed edge from
            -- v_out to v_in thus completes a cycle, but a sink might be
            -- reachable via any of the other edges from v_out.
            -- The depth for the vertex need not be updated as this is the only
            -- edge to v_out and 'prev' is already in the 'Pending' map.
            -- It follows that no (new) cycle can start here.
            Reversed ->
              let (res, g') = routeNormals (fromJust $ lookup i g) g p
               in (fst found_cycle <> res, g')
  | Just v <- lookup i g =
      routeVertex v
  | otherwise =
      backtrack
  where
    backtrack = (DeadEnd, g)

    routeVertex v =
      case (et, vertexRouting v) of
        (Normal, FromSource) -> backtrack
        (Normal, FromNode _ Exhausted) -> backtrack
        (Normal, FromNode rev _) -> entry (routeReversed rev)
        (Reversed, FromNode rev _) -> entry (routeAll rev v)
        _ -> entry (routeNormals v)

    entry f =
      let (res, g0) = f g (IM.insert i d p)
       in case res of
            CycleDetected d' as _
              | d == d' -> (DeadEnd, L.foldl' (\g1 a -> a g1) g0 as)
            _ | otherwise -> (res, g0)

    routeAll rev v g0 p0 =
      let (res, g1) = routeNormals v g0 p0
       in case res of
            DeadEnd -> routeReversed rev g1 p0
            CycleDetected _ _ p1 ->
              let (res', g2) = routeReversed rev g1 p1
               in (res <> res', g2)
            SinkFound _ -> (res, g1)

    routeReversed rev g0 p0 =
      let (res, g') = route' p0 (d + 1) (Just i) Reversed rev g0
          exhaust = flip adjust i $
            \v -> v {vertexRouting = FromNode rev Exhausted}
       in case (res, et) of
            (DeadEnd, _) ->
              (res, exhaust g')
            (CycleDetected d' as p', _) ->
              (CycleDetected d' (exhaust : as) p', g')
            (SinkFound _, Normal) ->
              (res, setRoute g')
            (SinkFound _, Reversed) ->
              let f v =
                    v
                      { vertexEdges = withPrev (vertexEdges v),
                        vertexRouting = NoRoute
                      }
               in (res, adjust f i g')

    setRoute = adjust (\v -> v {vertexRouting = routing}) i

    routing =
      case prev of
        Nothing -> FromSource
        Just i' -> FromNode i' NotExhausted

    withPrev edges
      | Just i' <- prev,
        ToNodes es (Just es') <- edges =
          ToNodes es (Just $ IS.insert i' es')
      | otherwise =
          edges -- shouldn't happen
    routeNormals v g0 p0
      | ToSink <- vertexEdges v =
          -- There cannot be a reversed edge to a vertex with an edge to a sink.
          (SinkFound i, setRoute g0)
      | ToNodes es nx <- vertexEdges v =
          let (res, g', nx') =
                case nx of
                  Just es' -> routeNorms (IS.toAscList es') g0 p0
                  Nothing -> routeNorms (IS.toAscList es) g0 p0
              edges = ToNodes es (Just $ IS.fromDistinctAscList nx')
              exhaust = flip adjust i $ \v' ->
                v' {vertexEdges = ToNodes es (Just IS.empty)}
           in case (res, et) of
                (DeadEnd, _) -> (res, exhaust g')
                (CycleDetected d' as p', _) ->
                  let res' = CycleDetected d' (exhaust : as) p'
                      v' = v {vertexEdges = edges}
                   in (res', insert v' g')
                (SinkFound _, Normal) ->
                  let v' = v {vertexEdges = edges, vertexRouting = routing}
                   in (res, insert v' g')
                (SinkFound _, Reversed) ->
                  let v' = v {vertexEdges = withPrev edges}
                   in (res, insert v' g')

    routeNorms [] g0 _ = (DeadEnd, g0, [])
    routeNorms (e : es) g0 p0 =
      let (res, g1) = route' p0 (d + 1) (Just i) Normal e g0
       in case res of
            DeadEnd -> routeNorms es g1 p0
            SinkFound _ -> (res, g1, es)
            CycleDetected _ _ p1 ->
              let (res', g2, es') = routeNorms es g1 p1
               in (res <> res', g2, e : es')
