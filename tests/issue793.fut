-- Hit a bug in the tiling logic for splitting the loop prelude.

-- types
type Sphere = {pos: [3]f32, radius: f32, color: [4]u8}
type Intersection = {t: f32, index: i32, prim: u8}

-- constants
let DROP_OFF = 100f32
-- ray intersection primitive cases
let P_NONE = 0:u8
let P_SPHERE = 1:u8
let P_LIGHT = 2:u8
let P_POLYGON = 3:u8

-- render functions:

let dot [n] (a: [n]f32) (b: [n]f32): f32 =
    reduce (+) 0 (map2 (*) a b)

let sphereIntersect (rayO: [3]f32) (rayD: [3]f32) (s: Sphere): f32 =
    let d = map2 (-) s.pos rayO
    let b = dot d rayD
    let c = (dot d d) - s.radius * s.radius
    let disc = b * b - c
    in if (disc < 0) then DROP_OFF
    else let t = b - f32.sqrt disc
        in if (0 < t) then t else DROP_OFF

-- render function
let render [nspheres] [nlights]
           (dim: [2]i32)
           (spheres: [nspheres]Sphere)
           (lights: [nlights]Sphere)
           : [][4]u8 = -- return a color for each pixel
    let pixIndices = iota (dim[0] * dim[1])
    in map (\i -> -- for each pixel
            let coord = [i %% dim[0], i // dim[0]]
            let rayD: [3]f32 = [r32 dim[0], r32 (coord[0] - dim[0] / 2), r32 (dim[1] / 2 - coord[1])]
            let rayO: [3]f32 = [0, 0, 0]

            -- sphere intersections
            let sInts: []Intersection = map3 (\t index prim -> {t, index, prim}) -- using instead of zip to create a record
                (map (\sphere ->
                    sphereIntersect rayO rayD sphere
                ) spheres)
                (iota nspheres)
                (replicate nspheres P_SPHERE)

            -- light intersections
            let lInts: []Intersection = map3 (\t index prim -> {t, index, prim})
                (map (\light ->
                    sphereIntersect rayO rayD light
                ) lights)
                (iota nlights)
                (replicate nlights P_LIGHT)

            -- closest intersection and corresponding primitive index
            let min: Intersection = reduce (\min x->
                    if x.t < min.t then x else min
                ) {t = DROP_OFF, index = 0i32, prim = P_NONE} (concat sInts lInts)

            -- return color
            in if (min.prim == P_SPHERE)
            then unsafe (spheres[min.index].color)
            else if (min.prim == P_LIGHT)
            then unsafe (lights[min.index].color)
            else [0:u8, 0:u8, 0:u8, 0:u8]
        ) pixIndices

-- entry point
let main [s] (width: i32)
             (height: i32)
             -- spheres and lights
             (numS: i32)
             (numL: i32)
             (sPositions: [s][3]f32)
             (sRadii: [s]f32)
             (sColors: [s][4]u8)
             -- return pixel color
             : [][4]u8 =
    -- combine data for render function
    let totalS = numS + numL
    let k = totalS - numS
    let spheres = map3 (\p r c -> {pos = p, radius = r, color = c})
        sPositions[0 : numS] sRadii[0 : numS] sColors[0 : numS]
    let lights = map3 (\p r c -> {pos = p, radius = r, color = c})
                      (sPositions[numS : totalS] :> [k][3]f32)
                      (sRadii[numS : totalS] :> [k]f32)
                      (sColors[numS : totalS] :> [k][4]u8)
    in render [width, height] spheres lights
