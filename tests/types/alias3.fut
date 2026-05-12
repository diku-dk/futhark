-- Nest array type aliases

type t = i32
type ts [n] = [n]t
type tss [n] [m] = [n](ts [m])

def main (xss: tss [] []) : tss [] [] = xss
