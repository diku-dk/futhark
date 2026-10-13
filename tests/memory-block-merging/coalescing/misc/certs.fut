-- Reproducer for a short-circuiting bug where a SegMap that produces
-- certificates would end up depending on itself.
--
-- The essential shape: a map whose per-element function does an in-place update
-- at a *dynamic* index (`copy x with [k] = ...`, which needs a bounds
-- certificate) inside two nested loops, fed into a reduce. After flattening,
-- the certificate becomes a `[n]unit` array produced by a segmap; short-
-- circuiting then propagates that certificate back onto the very segmap that
-- produces it.

type rng = {x: [4]u32, k: i32}

def base_rand ({x, k}: rng) : (rng, u32) =
  let xi = x[(k + 2) % 4] - x[k]
  let x = (copy x) with [k] = xi
  let k = (k + 1) % 4
  in ({x, k}, xi)

def eng_rand (r: rng) : (rng, u32) =
  let r = loop rr = r for _j < 2 do (base_rand rr).0
  in base_rand r

def dist_rand (r: rng) : (rng, i32) =
  let (r, x) = loop (r, x) = eng_rand r for _i < 1 do eng_rand r
  in (r, i32.u32 x)

entry test (seed: i32) (n: i64) : i32 =
  let r0: rng = {x = map (\j -> u32.i32 (seed + i32.i64 j)) (iota 4), k = 0}
  let (r0, _) = dist_rand r0
  let rngs = tabulate n (\i -> r0 with x = map (u32.+ u32.i64 i) r0.x)
  let (_, xs) = unzip (map dist_rand rngs)
  in i32.sum xs / i32.i64 n
