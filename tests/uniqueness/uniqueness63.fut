-- Respect explicit return uniqueness annotations on local functions.

type Cache [dh] =
  { mask: [dh]f32
  , kcache: [dh]f32
  }

type Params = [2][5]f32

entry testcache [dh] (cache: *Cache [dh]) (ps: Params) : (*Cache [dh], Params) =
  let gen_token [dh] (cache: *Cache [dh]) (ps: Params): (*Cache [dh], Params) = (cache, ps)
  in loop (cache: *Cache [dh], ps: Params) = (cache, ps)
     for i < 5 do
       gen_token cache ps
