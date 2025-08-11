module newf32 = {
  type t = f32
}

module type mixture = {
  module V: {type t}
}

module pm (P: mixture) = {
  module V = P.V
}

module foo = {
  module V = newf32
}

module k_means_em = pm (pm foo: mixture)

def main (x: k_means_em.V.t) = x
