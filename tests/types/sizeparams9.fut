-- From #1573.  Make sure we can handle missing whitespace in the parser.

type a [n] = [n]f32
def f (a: a []) : a [] = a
