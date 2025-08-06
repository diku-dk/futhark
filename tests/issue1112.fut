type triad 't = (t, t, t)
def triadMap 'a 'b (f: a -> b) (A: triad a) : triad b = (f A.0, f A.1, f A.2)
def triadMap2 'a 'b 'c (f: a -> b -> c) (A: triad a) (B: triad b) : triad c = (f A.0 B.0, f A.1 B.1, f A.2 B.2)
def triadFold 'a (f: a -> a -> a) (A: triad a) : a = f A.0 <| f A.1 A.2

type v3 = triad f32
type m33 = triad v3

type quaternion = {r: f32, v: v3}

def v3sum (v: v3) : f32 = triadFold (+) v
def v3add (a: v3) (b: v3) : v3 = triadMap2 (+) a b
def v3mul (a: v3) (b: v3) : v3 = triadMap2 (*) a b
def v3dot (a: v3) (b: v3) : f32 = v3mul a b |> v3sum

def gauss_jordan [m] [n] (A: [m][n]f32) =
  loop A for i < i64.min m n do
    let icol = map (\row -> row[i]) A
    let (j, _) =
      map f32.abs icol
      |> zip (iota m)
      |> drop i
      |> reduce_comm (\(i, a) (j, b) ->
                        if a < b
                        then (j, b)
                        else if b < a
                        then (i, a)
                        else if i < j
                        then (i, a)
                        else (j, b))
                     (0, 0)
    let f = (1 - A[i, i]) / A[j, i]
    let irow = map2 (f32.fma f) A[j] A[i]
    in map (\j ->
              if j == i
              then irow
              else let f = f32.neg A[j, i]
                   in map2 (f32.fma f) irow A[j])
           (iota m)

def hStack [m] [n] [l] (A: [m][n]f32) (B: [m][l]f32) = map2 concat A B
def vStack [m] [n] [l] (A: [m][n]f32) (B: [l][n]f32) = concat A B

def solveAB [m] [n] (A: [m][m]f32) (B: [m][n]f32) : [m][n]f32 =
  let AB = hStack A B |> gauss_jordan
  in AB[0:m, m:(m + n)] :> [m][n]f32

def solveAb [m] (A: [m][m]f32) (b: [m]f32) =
  unflatten (b :> [m * 1]f32) |> solveAB A |> flatten

def main u_bs (points': []v3) (forces: [](v3, v3)) =
  let C (x, y, z) =
    [ (1, 0, 0)
    , (0, 1, 0)
    , (0, 0, 1)
    , (0, -z, y)
    , (z, 0, -x)
    , (-y, x, 0)
    ]
  let CC C = map (\a -> (\b -> map (v3dot a) b) C) C
  let singleParticle u_b (f, t) =
    let f_ext = [f.0, f.1, f.2, t.0, t.1, t.2]
    let C_ps = map C points'
    let CC_ps = map CC C_ps |> transpose |> map transpose |> map (map f32.sum)
    let f_flow = map2 (\C u -> map (v3dot u) C) C_ps u_b |> map f32.sum
    let f_tot = map2 (+) f_flow f_ext
    let u = solveAb CC_ps f_tot
    in u
  in map2 singleParticle u_bs forces
