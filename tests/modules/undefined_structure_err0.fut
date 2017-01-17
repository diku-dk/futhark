-- We can not access a module before it has been defined.
-- ==
-- error: .*Unknown.*

fun try_me(): i32 = M0.number()
module M0 = {
  fun number(): i32 = 42
}

fun main(): i32 = try_me()