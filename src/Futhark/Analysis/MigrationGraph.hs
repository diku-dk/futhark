module Futhark.Analysis.MigrationGraph where

import Data.IntSet
import qualified Data.IntMap.Strict as IM
import Futhark.IR.GPU

-- | A handle that identifies a specific 'Vertex'.
type Id = Int

type IdSet = IntSet

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

-- Combined information on the immediate parent body that a variable is
-- declared within.
data BodyInfo = BodyInfo
  { -- | The fork depth of the variable. See 'ForkDepth'.
    bodyForkDepth :: ForkDepth,
    -- | The innermost loop body that the variable is declared within, if any.
    bodyLoopId :: Maybe Id
  }

-- | A graph representation of some program variable.
data Vertex = Vertex
  { -- | The handle for this vertex in the graph.
    vertexId :: Id,
    -- | How many branch bodies this variable binding is nested within, and
    -- in which loop body it is declared, if any.
    vertexBody :: BodyInfo,
    -- | Whether a route passes through this vertex, and from where.
    vertexRouting :: Routing,
    -- | Handles of vertices that this vertex has an edge to.
    vertexEdges :: Edges
  }

-- | Constructs a 'Vertex' with no route or edges declared.
vertex :: Id -> BodyInfo -> Vertex
vertex i bi = Vertex {
    vertexId      = i,
    vertexBody    = bi,
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
  mempty = ToNodes Data.IntSet.empty Nothing

-- Creates a set of edges where no edge is reversed or exhausted.
declareEdges :: [Id] -> Edges
declareEdges is = ToNodes (fromList is) Nothing
 
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

type Sink = ()

-- | A map from vertex 'Id' to corresponding 'Vertex', unless that vertex has
-- been marked as a sink.
newtype Graph = Graph (IM.IntMap (Either Sink Vertex))

-- | The empty graph.
empty :: Graph
empty = Graph (IM.empty)

-- | Insert a new vertex in the graph. If its variable already is represented
-- in the graph, the existing binding (vertex or sink) is replaced with the
-- supplied vertex.
insert :: Vertex -> Graph -> Graph
insert v (Graph m) = Graph $ IM.insert (vertexId v) (Right v) m

-- | Adjust the vertex with this specific id. When no vertex with that id is a
-- member of the graph, the original graph is returned.
adjust :: (Vertex -> Vertex) -> Id -> Graph -> Graph
adjust f i (Graph m) = Graph $ IM.adjust f' i m
  where
    f' (Right v) = Right (f v)
    f' x         = x

-- | Connect the vertex with this id to a sink. When no vertex with that id is a
-- member of the graph, the original graph is returned.
connectToSink :: Id -> Graph -> Graph
connectToSink = adjust $ \v -> v { vertexEdges = ToSink }

-- | Add these edges to the vertex with this id. When no vertex with that id is
-- a member of the graph, the original graph is returned.
addEdges :: Edges -> Id -> Graph -> Graph
addEdges es = adjust $ \v -> v { vertexEdges = es <> vertexEdges v }







insertSink :: Id -> Graph -> Graph
insertSink i g = g

insertWith :: (Vertex -> Vertex -> Vertex) -> Vertex -> Graph -> Graph
insertWith f i g = g

delete :: Id -> Graph -> Graph
delete i g = g



member :: Id -> Graph -> Bool
member i g = False




















