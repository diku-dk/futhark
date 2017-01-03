-- Signature with abstract type.

sig MONOID {
type t

val neutral: t
val op: t -> t -> t
}

fun main(): int = 0
