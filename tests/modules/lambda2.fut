-- Another clever use of module lambdas, and keyword-like application.
-- ==
-- input { 9 } output { 3.0 }

module type operation = {type a type b val f : a -> b}

module compose = \(P: {module F: operation module G: operation with a = F.b}): (operation with a = P.F.a with b = P.G.b) ->
  {
    type a = P.F.a
    type b = P.G.b

    def f (x: a) = P.G.f (P.F.f x)
  }

module i32_to_f64 : operation with a = i32 with b = f64 = {
  type a = i32
  type b = f64
  def f (x: a) = f64.i32 x
}

module f64_sqrt : operation with a = f64 with b = f64 = {
  type a = f64
  type b = f64
  def f (x: a) = f64.sqrt x
}

module mysqrt = compose {module F = i32_to_f64 module G = f64_sqrt}

def main (x: i32) = mysqrt.f x
