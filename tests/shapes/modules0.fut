-- Abstract types may not have anonymous sizes.
-- ==
-- error: info

module edge_handling (mapper: {type info}) = {
  let handle (i: i32) (info: mapper.info) = info
}

module m = edge_handling {type info = []f32}
