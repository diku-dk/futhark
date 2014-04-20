fun [[int]] main([[int]] a1, [[int]] a2) =
  let b = map(fn [int] ({[int],[int]} row) =>
                let {x,y} = row in
                map(op+ , zip(x,y)),
              zip(a1,a2)) in
  b
