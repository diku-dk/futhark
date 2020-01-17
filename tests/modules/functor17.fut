-- Test for shape declarations inside a parametric module.  (We used
-- to have a bug here.)
-- ==
-- input { } output { [1.0,2.0] [1] }

module PM(P: {type^ r}) = {

  type t = i32

  let f [n] (r: P.r) (a: [n]t) = (r,a)
}

module PMI = PM {type^ r = []f64}

let main = PMI.f [1.0,2.0] [1]
