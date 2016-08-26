-- No undefined types!
--
-- ==
-- error: .*not defined.*

type foo = bar

fun main(x: foo): foo = x
