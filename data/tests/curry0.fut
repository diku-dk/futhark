// Some simple currying of operators.

fun {[int],[int]} main([int] a) =
  {map(- 2, a), map(2 -, a)}
