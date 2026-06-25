-- Models a situation where a complicated cache structure is passed around.

type Cache [dh] = {mask: [dh]f32, kcache: [dh]f32}

def test [dh] (cache: *Cache [dh]) : (i64, *Cache [dh]) =
  (2, cache with kcache[0] = 7)

entry testcache [dh] (cache: *Cache [dh]) : (i64, *Cache [dh]) =
  let gen_token (cache: *Cache [dh]) = test cache
  in loop (acc, cache) = (0, cache)
     for i < 5 do
       gen_token cache
