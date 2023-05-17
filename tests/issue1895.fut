-- Setup from my program.
module real = f32

type real = real.t
type vec3 = {x:f32,y:f32,z:f32}

def dot (a: vec3) (b: vec3) =
  (a.x*b.x + a.y*b.y + a.z*b.z)

def vecsub (a: vec3) (b: vec3) =
  {x= a.x-b.x, y= a.y-b.y, z= a.y-b.z}

def cross ({x=ax,y=ay,z=az}: vec3)
          ({x=bx,y=by,z=bz}: vec3): vec3 =
  ({x=ay*bz-az*by, y=az*bx-ax*bz, z=ax*by-ay*bx})

def quadrance v = dot v v

def norm = quadrance >-> f32.sqrt

def to_vec3 (xs: [3]real) = {x=xs[0], y=xs[1], z=xs[2]}
def map_2d f = map (map f)
def map_3d f = map (map_2d f)

def grad 'a (f: a -> real) (primal: a) = vjp f primal (real.i64 1)

let jacfwd [n][m] (f: [n][m]vec3 -> real) (x: [n][m]vec3): [n][m]vec3 =
  let v3 xs = unflatten_3d xs |> map_2d to_vec3
  let tangent i = (replicate (n*m*3) 0 with [i] = 1) |> v3
  in tabulate (n*m*3) (\i -> jvp f x (tangent i)) |> v3


-- The function.
def fun atom_coords : real =
  let xs = flatten atom_coords
  -- "Random" reads are necessary (in the original program this reads an
  -- input of unknown pairs of indices, here the indexing is just nonsense):
  let dists = map2 (\i j -> norm (xs[i] `vecsub` xs[j]))
                   (indices atom_coords)
                   (indices atom_coords |> reverse)
  in f32.sum dists

-- ==
-- compiled random input { [1][32][20][3]f32 }
-- auto output
entry main coords =
  map (grad fun) (map_3d to_vec3 coords)
  |> map_3d (\v -> [v.x,v.y,v.z])
