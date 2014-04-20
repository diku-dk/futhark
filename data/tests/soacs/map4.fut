// Test a tricky case involving rewriting lambda arguments in the
// tuple transformer.

fun [int] inner([[{int,int}]] a) =
  map(fn int([{int,int}] r) => let {x,y} = r[0] in x+y, a)

fun [int] main([[int]] a1, [[int]] a2) =
  let a = map(fn [{int,int}] ({[int],[int]} p) =>
                let {p1,p2} = p in
                zip(p1,p2),
              zip(a1,a2)) in
  inner(a)