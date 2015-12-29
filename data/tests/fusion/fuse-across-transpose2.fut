-- ==
-- structure { Map 2 }
fun [[int]] main([[{int,int},m],n] a) =
  let b = map(fn [{int,int},m] ([{int,int}] row) =>
                map(fn {int,int} (int x, int y) =>
                      {x+y,x-y},
                    row),
                a) in
  let d = size(0,b) in -- FIXME
  let c = map(fn [int,d] ([{int,int}] row) =>
                map(+ , row),
              transpose(b)) in
  c
