-- ==
struct Vec3
  {
    struct F32
      {
        type t = ( f32 , f32 , f32 )
        fun add(a: t , b: t): t =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          (a1 + b1, a2 + b2 , a3 + b3)

        fun subtract(a: t , b: t): t =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          (a1 - b1, a2 - b2 , a3 - b3)

        fun scale(k: f32 , a: t): t =
          let (a1, a2, a3) = a in
          (a1 * k, a2 * k , a3 * k)

        fun dot(a: t , b: t): f32 =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          a1*b1 + a2*b2 + a3*b3
      }

    struct Int
      {
        type t = ( int , int , int )
        fun add(a: t , b: t): t =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          (a1 + b1, a2 + b2 , a3 + b3)

        fun subtract(a: t , b: t): t =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          (a1 - b1, a2 - b2 , a3 - b3)

        fun scale(k: int , a: t): t =
          let (a1, a2, a3) = a in
          (a1 * k, a2 * k , a3 * k)

        fun dot(a: t , b: t): int =
          let (a1, a2, a3) = a
          let (b1, b2, b3) = b in
          a1*b1 + a2*b2 + a3*b3
      }
  }

fun main(k: f32): Vec3.F32.t = Vec3.F32.scale (k, (0.0f32, 1.0f32, 2.0f32))
