-- ==
-- input { [5i64,7i64] [[5],[7]] }
-- output { [7,9] }

def main = map2 (\n xs ->
                   #[unsafe]
                   let A = #[opaque] replicate n xs
                   let B = #[opaque] map (\x -> (opaque x)[0]+2i32) A
                   in B[0])
