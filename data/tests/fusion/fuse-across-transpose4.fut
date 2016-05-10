-- ==
-- input { [[1,2,3],[4,5,6]] [[7,8,9],[1,2,3]] }
-- output { [[10, 7], [12, 9], [14, 11]] }
-- structure { Map 2 }
fun [[int]] main([[int,m],n] a, [[int,m],n] b) =
  let a2 = map(fn [int,n] ([int] r) => map(+1, r), transpose(a)) in
  let b2 = map(fn [int,n] ([int] r) => map(+1, r), transpose(b)) in
  let c  = map(fn [int,n] (([int],[int]) rp) =>
                 let (rx,ry) = rp in
                 map(+, zip(rx,ry)),
               zip(a2,b2)) in
  c
