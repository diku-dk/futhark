-- Nest array type aliases

type t = i32
type ts [n] = [n]t
type tss [n][m] = [n](ts [m])

let main(xss: tss [][]): tss [][] = xss
