-- ==
-- input {
-- }
-- output {
--   [1]
--   empty(int)
--   [2, 3]
--   [4]
-- }
fun ([int],[int],[int],[int]) main() =
    let a = [1,2,3,4] in
    split( (1,1,3), [1,2,3,4] )
