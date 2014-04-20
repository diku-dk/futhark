fun [[int]] main([[int]] a, [[int]] b) =
  let a2 = map(fn [int] ([int] r) => map(op+(1), r), transpose(a)) in
  let b2 = map(fn [int] ([int] r) => map(op+(1), r), transpose(b)) in
  let c  = map(fn [int] ({[int],[int]} rp) =>
                 let {rx,ry} = rp in
                 map(op+, zip(rx,ry)),
               zip(a2,b2)) in
  c