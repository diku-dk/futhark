// --
// input {
//   {100, 1}
// }
// output {
//   1
// }

fun int main(int chunk, int m) =
  loop (m) = for j < chunk do
      let chunk_in = chunk do //+1 in
      // setting chunk_in to chunk+1 will disable the simplification!
      loop (m) = for i < chunk_in do
                    let ip1   = i + 1       in
                    let diff0 = ip1 - chunk_in in
                    let cond  = 0 < diff0   in
                    let diff  = if   cond
                                then diff0
                                else 0
                    in  m + diff + 1
      in m
  in m
