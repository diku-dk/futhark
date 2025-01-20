-- Fancy size-dependent programming via the module system; the
-- interpreter had a far too naive idea of how size expressions were
-- handled.
-- ==
-- entry: test_adam
-- input { [42f32,43f32] }

module type vspace = {
  module Scalar: real
  type scalar = Scalar.t
  type vector

  val zero : vector
  val scale : scalar -> vector -> vector
  val dot : vector -> vector -> scalar
  val + : vector -> vector -> vector
  val neg : vector -> vector
  val to_array : vector -> []scalar
}

module array_vec (T: real) (Size: {val n : i64}) = {
  module Scalar = T
  type scalar = Scalar.t
  type vector = [Size.n]scalar
  def zero : vector = rep (T.i32 0)

  def scale (s: scalar) (v: vector) : vector =
    map (s T.*) v

  def dot (v: vector) (u: vector) : scalar =
    T.sum (map2 (T.*) v u)

  def (+) (v: vector) (u: vector) : vector =
    map2 (T.+) v u

  def neg (v: vector) : vector =
    map T.neg v

  def from_array (x: [Size.n]scalar) : vector = x
  def to_array (x: vector) : [Size.n]scalar = x
}

module adam (V: vspace) = {
  local module S = V.Scalar
  local type a = V.scalar
  local type v = V.vector

  type state =
    { step: i32
    , mw: v
    , vw: a
    , w: v
    }

  def result (x: state) : v = x.w

  def initial_state (x: v) : state =
    {step = 0, mw = V.zero, vw = S.i32 0, w = x}

  type params =
    { beta1: a
    , beta2: a
    , eta: a
    , epsilon: -- learning rate
               a
    }

  def def_params : params =
    { beta1 = S.f32 0.9
    , beta2 = S.f32 0.999
    , eta = S.f32 1e-4
    , epsilon = S.f32 1e-5
    }

  def adam (p: params) (obj: v -> a) (state: state) : state =
    let grad: v = vjp obj state.w (S.i32 1)
    let mw': v = V.(p.beta1 `scale` state.mw + (S.(i32 1 - p.beta1) `scale` grad))
    let vw': a = S.(p.beta2 * state.vw + (i32 1 - p.beta2) * (grad `V.dot` grad))
    let pow (x: a) (n: i32) = S.(exp (i32 n * log x))
    let mw_hat: v = S.(i32 1 - p.beta1 `pow` state.step) `V.scale` mw'
    let vw_hat: a = S.((i32 1 - p.beta2 `pow` state.step) * vw')
    let s: a = S.(p.eta / (sqrt vw_hat + p.epsilon))
    let w: v = V.(state.w + neg (s `scale` mw_hat))
    in {mw = mw', vw = vw', step = state.step + 1, w}
}

module Size = {def n : i64 = 2}

module V = array_vec f32 Size
type v = [Size.n]f32
module Adam = adam V

def func (v: v) : f32 =
  let vv = v :> [2]f32
  let x: f32 = vv[0]
  let y: f32 = vv[1]
  in f32.(2 * x * x + (y - 2) * (y - 2) + 3)

entry test_adam (x0: [2]f32) =
  iterate 5 (Adam.adam Adam.def_params func) (Adam.initial_state (x0 :> [Size.n]f32))
  |> \{mw, step, vw, w} -> (mw, step, vw, w)
