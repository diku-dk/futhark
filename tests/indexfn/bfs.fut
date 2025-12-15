-- import "lib/github.com/diku-dk/segmented/segmented"

type queuePair = {vertex: i32, parent: i32}

def remove_duplicates [nQueue]  (nVerts) (queue: [nQueue]queuePair): []queuePair = 
    let verts = map (\q -> i64.i32 q.vertex) queue
    let indexes = iota nQueue 
    let H = hist i64.min nQueue nVerts verts indexes
    in map2 (\i j -> H[i] == j) verts indexes 
        |> zip queue
        |> filter (.1)
        |> map (.0)

-- Set the parent of each vertex in the queue. The queue must not contain duplicates
def update_parents [nVerts] (parents: *[nVerts]i32) (queue: []queuePair): *[nVerts]i32 =
    let qVerts = map (\q -> i64.i32 q.vertex) queue
    let qParents = map (\q -> q.parent) queue
    in scatter parents qVerts qParents

def edges_of_vertex (verts: []i32) (nEdges: i32) (edge: queuePair): i64 =
    let extended = verts ++ [nEdges]
    in i64.i32 (extended[edge.vertex + 1] - extended[edge.vertex])

def get_ith_edge_from_vert (verts: []i32) (edges: []i32) (parents: []i32) (q: queuePair) (i: i64) : queuePair =
    -- Get the i'th vertex of the edge
    let currentVert = edges[i64.i32 verts[q.vertex] + i]
    -- If it's an unseen vertex, add it to the queue with the current vertex being the parent
    -- Else return a placeholder that we filter later
    in if (parents[currentVert] == -1)
    then {vertex = currentVert, parent = q.vertex}
    else {vertex = -1, parent = -1}

def BFS [nVerts] [nEdges] (verts: [nVerts]i32) (edges: [nEdges]i32) (parents: *[nVerts]i32) (queue: *[]queuePair): [nVerts]i32 =
    -- Loop until we get an empty queue
    let (parents, _) = loop (parents, queue) while length queue > 0 do
        -- Setup a function that takes a queuePair, and returns how many vertexes goes out of it
        let get_edges_of_vert_fun = edges_of_vertex verts (i32.i64 nEdges)
        -- Setup a function that takes a queuePair and an int i, and returns the vertex pointed to by the i'th edge
        let get_ith_edge_from_vert_fun = get_ith_edge_from_vert verts edges parents

        -- Get the vertexes in the next layer
        let newQueue = expand (get_edges_of_vert_fun) (get_ith_edge_from_vert_fun) queue
        -- Remove empty placeholders ({-1, -1} queuePairs)
        let filteredQueue = filter (\q -> q.parent != -1) newQueue
        -- Remove duplicates from the queue
        let noDupesQueue = remove_duplicates nVerts filteredQueue

        in (update_parents parents noDupesQueue, noDupesQueue)
    in parents

def main [nVerts] [nEdges] (vertexes_enc: [nVerts]i32) (edges_enc: [nEdges]i32) =
    let start = 0

    let parents = replicate nVerts (-1)
    let queue = [{vertex = start, parent = start}]
    let parents = update_parents parents queue

    in BFS vertexes_enc edges_enc parents queue

-- ==
-- input @ data/randLocalGraph_J_10_20000000.in
-- output @ data/randLocalGraph_J_10_20000000.out
-- mem_16gb input @ data/rMatGraph_J_12_16000000.in
-- output @ data/rMatGraph_J_12_16000000.out
-- input @ data/3Dgrid_J_64000000.in
-- output @ data/3Dgrid_J_64000000.out




-- update parents, queue er unique preconditoin allrede en kommentar
--- expand kan goeres med vores segmented ops?
