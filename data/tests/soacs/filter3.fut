-- ==
-- input {
--   [(0,False), (1,True), (-2,True), (5,False), (42,True)]
-- }
-- output {
--   [(True,1), (True,-2), (True,42)]
-- }
fun [(bool,int)] main([(int,bool)] xs) =
  let tmp = filter(fn bool ((int,bool) x) =>
                     let (i,b) = x in b
                  , xs) in
  map(fn (bool,int) ((int,bool) x) =>
        let (i,b) = x in (b,i)
     , tmp)
