type comp = f64

type Layer [nx] [ny] =
    #l_inhomogeneous {epsilon: [nx][ny]comp, mu: [nx][ny]comp, thickness: f64}
  | #l_homogeneous {epsilon: comp, mu: comp, thickness: f64}

type ConvLayer [NM] =
    #inhomogeneous {Kz: [NM][NM]comp}
  | #homogeneous {Kz: [NM][NM]comp}

def makeConvLayer [nx] [ny] (NM: i64) (layer: Layer [nx] [ny]) : ConvLayer [NM] =
  match layer
  case #l_inhomogeneous {epsilon, mu, thickness} ->
    #inhomogeneous {Kz = tabulate_2d NM NM (\i j -> 0.0)}
  case #l_homogeneous {epsilon, mu, thickness} ->
    --BUG HERE:
    let Kz = tabulate_2d NM NM (\i j -> 0.0)
    in #homogeneous {Kz = Kz}

--BUT OK THIS WAY:
-- #homogeneous { Kz = tabulate_2d NM NM (\i j->0.0) }

--API demo/examples
entry main (x: i64) =
  --freespace problem setup
  let nx = 5
  --x pixel count
  let ny = 5
  --y pixel count
  let N = 5
  --modes in x
  let M = 5
  --modes in y
  let NM = N * M
  --total number of modes
  let layer = (#l_homogeneous {epsilon = 1.0, mu = 1.0, thickness = f64.inf} : Layer [nx] [ny])
  let c = makeConvLayer NM layer
  --run it
  in 1
