fun [[int]] main([[{int,int}]] a) =
  let b = map(fn [{int,int}] ([{int,int}] row) =>
                map(fn {int,int} (int x, int y) =>
                      {x+y,x-y},
                    row),
              a) in
  let c = map(fn [int] ([{int,int}] row) =>
                map(+ , row),
              transpose(b)) in
  c
