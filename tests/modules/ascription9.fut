-- Size annotations should not mess up module ascription.

module type vector = {
  type vector 'a
}

module mk_kmeans (D: {}) (V: vector) (R: {})
  : {
      type point = V.vector f32

      val kmeans [n] :
        (points: [n]point)
        -> ([n]point, i32)
    } = {
  type point = V.vector f32

  def kmeans [n] (points: [n]point) : ([n]point, i32) =
    (points, 0)
}
