-- Another clever use of module lambdas, and keyword-like application.
-- ==
-- input { 9 } output { 3.0f32 }

module type operation = {type a type b val f : a -> b}

module compose = \(P: {module F: operation module G: operation with a = F.b}): (operation with a = P.F.a with b = P.G.b) ->
  {
    type a = P.F.a
    type b = P.G.b

    def f (x: a) = P.G.f (P.F.f x)
  }

module i32_to_f32 : operation with a = i32 with b = f32 = {
  type a = i32
  type b = f32
  def f (x: a) = f32.i32 x
}

module f32_sqrt : operation with a = f32 with b = f32 = {
  type a = f32
  type b = f32
  def f (x: a) = f32.sqrt x
}

module mysqrt = compose {module F = i32_to_f32 module G = f32_sqrt}

def main (x: i32) = mysqrt.f x
