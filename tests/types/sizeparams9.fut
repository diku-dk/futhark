-- From #1573.  Make sure we can handle missing whitespace in the parser.

type a [n] = [n]f32
let f (a:a[]) : a[] = a
