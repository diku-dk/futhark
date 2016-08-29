-- We can not access a struct before it has been defined.
-- ==
-- error: .*Unknown.*

fun try_me(): int = M0.number()
struct M0
  {
    fun number(): int = 42
  }

fun main(): int = try_me()