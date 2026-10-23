-- Signed Distance Functions, as you would find in a ray marcher.
-- ==
-- tags { autodiff }
-- entry: jvp_normal vjp_normal
-- input { 0i32 0f32 1f32 0f32 } output { 1f32 0f32 0f32 }
-- input { 1i32 0f32 1f32 0f32 } output { 0.412393f32 0.907265f32 -0.082479f32 }
-- input { 2i32 0f32 1f32 0f32 } output { -0.375775f32 0.903687f32 -0.205287f32 }

type Vec = {x: f32, y: f32, z: f32}

type Angle = f32
type Position = Vec
type Radiance = Vec
type Direction = Vec
type Distance = f32
type Radius = f32
type BlockHalfWidths = Vec

def vmap (f: f32 -> f32) (v: Vec) =
  {x = f v.x, y = f v.y, z = f v.z}

def (<->) (a: Vec) (b: Vec) =
  {x = a.x - b.x, y = a.y - b.y, z = a.z - b.z}

def rotateY (theta: f32) ({x, y, z}: Vec) =
  let cos_theta = f32.cos theta
  let sin_theta = f32.sin theta
  in { x = f32.(cos_theta * x - sin_theta * z)
     , y
     , z = f32.(sin_theta * x + cos_theta * z)
     }

def dot (a: Vec) (b: Vec) : f32 =
  a.x * b.x + a.y * b.y + a.z * b.z

def norm v = f32.sqrt (dot v v)

type Object =
    #Wall Direction Distance
  | #Block Position BlockHalfWidths Angle
  | #Sphere Position Radius

def sdObject (obj: Object) (pos: Position) : Distance =
  match obj
  case #Wall nor d -> f32.(d + dot nor pos)
  case #Block blockPos halfWidths angle ->
    let pos' = rotateY angle (pos <-> blockPos)
    in norm (vmap (f32.max 0) (vmap f32.abs pos' <-> halfWidths))
  case #Sphere spherePos r ->
    let pos' = pos <-> spherePos
    in f32.(max (norm pos' - r) 0)

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
        (vec (f32.bool (i == 0))
             (f32.bool (i == 1))
             (f32.bool (i == 2)))
  in (f 0, f 1, f 2)

entry vjp_normal (obji: i32) x y z =
  unvec (vjp (sdObject (get_obj obji)) (vec x y z) 1)
