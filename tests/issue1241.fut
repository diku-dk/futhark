type rng = {state: u64, inc: u64}

let rand ({state, inc}: rng) =
  let oldstate = state
  let state = oldstate * 6364136223846793005u64 + (inc|1u64)
  let xorshifted = u32.u64 (((oldstate >> 18u64) ^ oldstate) >> 27u64)
  let rot = u32.u64 (oldstate >> 59u64)
  in ({state, inc},
      (xorshifted >> rot) | (xorshifted << ((-rot) & 31u32)))

let rng_from_seed (xs: []i32) =
  let initseq = 0xda3e39cb94b95bdbu64 -- Should expose this somehow.
  let state = 0u64
  let inc = (initseq << 1u64) | 1u64
  let {state, inc} = (rand {state, inc}).0
  let state = loop state for x in xs do state + u64.i32 x
  in (rand {state, inc}).0

let dummy_rng (): rng =
  rng_from_seed [0]

type tup = (i64, i64)

let foo [n] (grid: *[n]tup): *[n]tup = grid
let bar [n] (grid: *[n]tup): *[n]tup = grid with [0] = (1, 1)

let foo_bar [n] (grid_foo: *[n]tup) (grid_bar: *[n]tup): (*[n]tup, *[n]tup) =
  (foo grid_foo, bar grid_bar)

let create_tup (_: rng): tup = (0, 0)

let dummy_grid (n: i64): [n]tup = replicate n ((create_tup (dummy_rng ())))

entry foo_bar_bar [n] (grid0: *[n]i64) (grid1: *[n]i64): ([n]i64, [n]i64) =
  unzip ((foo_bar (zip grid0 grid1) (dummy_grid n)).1)
