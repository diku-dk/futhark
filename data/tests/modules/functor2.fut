-- Referring to a parameter-defined type in a functor return signature.
-- ==
-- input { 2 } output { 4 }

module F(P:{type t val f:t->t}): {type t = P.t val f2:t->t} = {
type t = P.t
fun f2(x: t): t = P.f (P.f x)
}

module F' = F({type t = int fun f (x: int): int = x+1})

fun main(x: int): F'.t = F'.f2 x
