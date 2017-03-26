-- No undefined types!
--
-- ==
-- error: Unknown type

type foo = bar

let main(x: foo): foo = x
