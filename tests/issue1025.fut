-- Derived from futracer

module f32racer = {
  type t = f32
  type point2D = {x: t, y: t}
  type point3D = {x: t, y: t, z: t}
  type angles = {x: t, y: t, z: t}
}

module i32racer = {
  type t = i32
  type point2D = {x: t, y: t}
  type point3D = {x: t, y: t, z: t}
  type angles = {x: t, y: t, z: t}
}

type triangle = (f32racer.point3D, f32racer.point3D, f32racer.point3D)
type point_projected = {x: i32, y: i32, z: f32}
type point = i32racer.point2D
type triangle_projected = (point_projected, point_projected, point_projected)
type point_barycentric = (i32, i32racer.point3D, f32racer.point3D)
type rectangle = (i32racer.point2D, i32racer.point2D)

def barycentric_coordinates ({x, y}: i32racer.point2D) (triangle: triangle_projected) =
  let ({x = xp0, y = yp0, z = _}, {x = xp1, y = yp1, z = _}, {x = xp2, y = yp2, z = _}) = triangle
  in ((yp1 - yp2) * (x - xp2) + (xp2 - xp1) * (y - yp2))

def main [tn]
         (triangles_projected: [tn]triangle_projected)
         (w: i32)
         (h: i32)
         rects
         ((n_rects_x, n_rects_y): (i32, i32)) =
  let each_pixel [rtpn] (rect_triangles_projected: [rtpn]triangle_projected) (pixel_index: i32) =
    let p = {x = pixel_index / h, y = pixel_index % h}
    let each_triangle (t: triangle_projected) = barycentric_coordinates p t != 0
    in all each_triangle rect_triangles_projected
  let rect_in_rect (({x = x0a, y = y0a}, {x = x1a, y = y1a}): rectangle) (({x = x0b, y = y0b}, {x = x1b, y = y1b}): rectangle): bool = !(x1a <= x0b || x0a >= x1b || y1a <= y0b || y0a >= y1b)
  let bounding_box (( {x = x0, y = y0, z = _}
                    , {x = x1, y = y1, z = _}
                    , {x = x2, y = y2, z = _}
                    ): triangle_projected
  ): rectangle =
    ( { x = i32.min (i32.min x0 x1) x2
      , y = i32.min (i32.min y0 y1) y2
      }
    , { x = i32.max (i32.max x0 x1) x2
      , y = i32.max (i32.max y0 y1) y2
      }
    )
  let triangle_in_rect (rect: rectangle) (tri: triangle_projected): bool =
    let rect1 = bounding_box tri
    in rect_in_rect rect1 rect || rect_in_rect rect rect1
  let each_rect [bn] (rect: rectangle) (pixel_indices: [bn]i32) =
    let rect_triangles_projected =
      filter (triangle_in_rect rect) triangles_projected
    in map (each_pixel rect_triangles_projected) pixel_indices
  let rect_pixel_indices (totallen: i64) (({x = x0, y = y0}, {x = x1, y = y1}): rectangle) =
    let (xlen, ylen) = (i64.i32 (x1 - x0), i64.i32 (y1 - y0))
    let xs = map (+ x0) (map i32.i64 (iota xlen))
    let ys = map (+ y0) (map i32.i64 (iota ylen))
    in flatten (map (\x -> map (\y -> x * h + y) ys) xs) :> [totallen]i32
  let x_size = w / n_rects_x + i32.bool (w % n_rects_x > 0)
  let y_size = h / n_rects_y + i32.bool (h % n_rects_y > 0)
  let pixel_indicess = map (rect_pixel_indices (i64.i32 (x_size * y_size))) rects
  let pixelss = map2 each_rect rects pixel_indicess
  in pixelss
