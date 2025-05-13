-- No sneaking through size-lifted types through type parameters!
-- ==
-- error: may not contain size-lifted

module type key = {type~ pctx}

module hashmap (K: key) = {
  type~ ctx = K.pctx

  type hashmap_inner 'tctx =
    { ctx: tctx
    }

  type hashmap = hashmap_inner ctx
}
