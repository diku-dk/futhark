module Futhark.Analysis.MigrationGraph where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Futhark.IR.GPU

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
    ToNodes (IdSet) (Maybe (IdSet))

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


-- | Which vertices that were visited during a previous failed routing attempt.
-- Such set is valid until edges are added or removed from its corresponding
-- graph, which will happen if a routing attempt succeeds.
type Visited = IdSet

-- | @route v src g@ attempts to find a route in @g@ from the source connected
-- vertex @src@. @v@ is the set of vertices that were visited during a previous
-- failed routing attempt; see 'Visited' for how long such set it valid.
-- If the routing attempt fails, an updated visited set and graph is returned.
-- Upon success the id for the last vertex along the created route is returned
-- instead, along with an updated graph wherein edges along the route have been
-- reversed. In either case edges may be exhausted in the updated graph.
route :: Visited -> Id -> Graph -> (Either Visited Id, Graph)
route v src g = (Left v, g) -- TODO

-- | @routeMany v srcs g@ attempts to find a route in @g@ from every vertex in
-- in @srcs@. @v@ is the set of vertices that were visited during a previous
-- failed routing attempt; see 'Visited' for how long such set it valid.
-- Returns a (possibly empty) visited set corresponding to the last routing
-- attempt made, the ids for the last vertices along the created routes, and an
-- updated graph.
routeMany :: Visited -> [Id] -> Graph -> (Visited, [Id], Graph)
routeMany visited srcs graph =
  foldl' f (visited, [], graph) srcs
  where
    f (v, snks, g) src =
      case route v src g of
        (Left   v', g') -> (v', snks, g')
        (Right snk, g') -> (IS.empty, snk:snks, g')























