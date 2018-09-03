-- ==
-- error: Value `f`

module type m = {
    type t
    val f: t -> ()
}

module m: m = {
    type t = [1]f32
    let f (_t: *t): () = ()
}

entry f (t: m.t): () = m.f t
