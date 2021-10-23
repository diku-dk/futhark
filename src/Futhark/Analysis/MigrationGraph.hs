module Futhark.Analysis.MigrationGraph where

import Data.Foldable (foldMap')
import Data.Set
import qualified Data.IntMap.Strict as IM
import Futhark.IR.GPU

-- | A handle that identifies a specific 'Vertex'.
type Id = Int

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

-- | All relevant edges that have been declared from some vertex, plus
-- bookkeeping to track their exhaustion and reversal.
data Edges
  = -- | The vertex has an edge to a sink; all other declared edges are
    -- irrelevant. The edge cannot become exhausted, and it is reversed if a
    -- route passes through the vertex.
    ToSink
  | -- | All vertices that the vertex has a declared edge to, and which of
    -- those edges that are not exhausted nor reversed, if not all.
    ToNodes (Set Id) (Maybe (Set Id))

instance Semigroup Edges where
  ToSink <> _ = ToSink
  _ <> ToSink = ToSink
  (ToNodes a1 e1) <> (ToNodes a2 e2) =
    ToNodes (a1 <> a2) (e1 <> e2)

instance Monoid Edges where
  -- The empty set of edges.
  mempty = ToNodes Data.Set.empty Nothing
 
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

-- | A map from vertex 'Id' to corresponding 'Vertex', unless that vertex has
-- been marked as a sink.
newtype Graph = Graph (IM.IntMap (Maybe Vertex))

empty :: Graph
empty = Graph (IM.empty)

addEdges :: Edges -> Id -> Graph -> Graph
addEdges es i g = g

connectToSink :: Id -> Graph -> Graph
connectToSink i g = g




















