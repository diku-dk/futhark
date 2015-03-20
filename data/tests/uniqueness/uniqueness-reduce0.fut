// If the reduction function accumulator type is unique, consume the
// initial value, but only as much as is actually unique!

fun [int] main(*[int] a,[int] b) =
  let {x,y} =
    reduce(fn {*[int], [int]} ({*[int], [int]} acc, {[int], [int]} i) =>
             let {a2,b2} = acc in {a2,b2},
           {a,b}, zip(copy(replicate(10,iota(10))),
                      replicate(10,iota(10)))) in
  map(+, zip(b, x)) // Should be OK, because only a has been consumed.
