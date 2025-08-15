-- Signed Distance Functions, as you would find in a ray marcher.
-- ==
-- tags { autodiff }
-- entry: jvp_normal vjp_normal
-- input { 0i32 0f64 1f64 0f64 } output { 1f64 0f64 0f64 }
-- input { 1i32 0f64 1f64 0f64 } output { 0.412393f64 0.907265f64 -0.082479f64 }
-- input { 2i32 0f64 1f64 0f64 } output { -0.375775f64 0.903687f64 -0.205287f64 }

type Vec = {x: f64, y: f64, z: f64}

type Angle = f64
type Position = Vec
type Radiance = Vec
type Direction = Vec
type Distance = f64
type Radius = f64
type BlockHalfWidths = Vec

def vmap (f: f64 -> f64) (v: Vec) =
  {x = f v.x, y = f v.y, z = f v.z}

def (<->) (a: Vec) (b: Vec) =
  {x = a.x - b.x, y = a.y - b.y, z = a.z - b.z}

def rotateY (theta: f64) ({x, y, z}: Vec) =
  let cos_theta = f64.cos theta
  let sin_theta = f64.sin theta
  in { x = f64.(cos_theta * x - sin_theta * z)
     , y
     , z = f64.(sin_theta * x + cos_theta * z)
     }

def dot (a: Vec) (b: Vec) : f64 =
  a.x * b.x + a.y * b.y + a.z * b.z

def norm v = f64.sqrt (dot v v)

type Object =
    #Wall Direction Distance
  | #Block Position BlockHalfWidths Angle
  | #Sphere Position Radius

def sdObject (obj: Object) (pos: Position) : Distance =
  match obj
  case #Wall nor d -> f64.(d + dot nor pos)
  case #Block blockPos halfWidths angle ->
    let pos' = rotateY angle (pos <-> blockPos)
    in norm (vmap (f64.max 0) (vmap f64.abs pos' <-> halfWidths))
  case #Sphere spherePos r ->
    let pos' = pos <-> spherePos
    in f64.(max (norm pos' - r) 0)

def vec x y z : Vec = {x, y, z}
def unvec ({x, y, z}: Vec) = (x, y, z)

def wall : Object = #Wall (vec 1 0 0) 2

def sphere : Object = #Sphere (vec (-1.0) (-1.2) 0.2) 0.8

def block : Object = #Block (vec 1.0 (-1.6) 1.2) (vec 0.6 0.8 0.6) (-0.5)

def get_obj (i: i32) =
  match i
  case 0 -> wall
  case 1 -> sphere
  case _ -> block

entry jvp_normal (obji: i32) x y z =
  let f i =
    jvp (sdObject (get_obj obji))
        (vec x y z)
        (vec (f64.bool (i == 0))
             (f64.bool (i == 1))
             (f64.bool (i == 2)))
  in (f 0, f 1, f 2)

entry vjp_normal (obji: i32) x y z =
  unvec (vjp (sdObject (get_obj obji)) (vec x y z) 1)
