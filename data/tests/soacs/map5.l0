// Test a tricky case involving rewriting lambda arguments in the
// tuple transformer.

fun [int] inner([{[{int,int}]}] a) =
  map(fn int({[{int,int}]} r1) =>
        let {r2} = r1 in
        let {x,y} = r2[0] in
        x+y,
      a)

fun [int] main([[int]] a1, [[int]] a2) =
  inner(map(fn {[{int,int}]} ({[int],[int]} r) =>
              let {r1,r2} = r in
              {zip(r1,r2)},
            zip(a1,a2)))
