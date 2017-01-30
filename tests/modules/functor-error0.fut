-- Error when passing a struct of the wrong signature to a functor.
-- ==
-- error: x.*i32.*f32

module F (P: { val x: i32 }) = {
  val x: i32 = P.x + 1
}

module F' = F({val x: f32 = 1.0f32})

fun main(): i32 = F'.x
