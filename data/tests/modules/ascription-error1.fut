-- We may not access structure members not part of the signature.
-- ==
-- error: Struct.g

sig SIG {
val f: int -> int
}

struct Struct: SIG {
fun f (x: int): int = x + 2
fun g (x: int): int = x + 3
}

fun main(x: int): int = Struct.g x
