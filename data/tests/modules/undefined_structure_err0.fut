-- We can not access a struct before it has been defined.
-- ==
-- error: .*Unknown.*

fun int try_me() = M0.number()
struct M0 
  {
    fun int number() = 42
  }

fun int main() = try_me()