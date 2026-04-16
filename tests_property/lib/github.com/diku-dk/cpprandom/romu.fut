-- | A collection of `rng_engine`@term modules that implement the
-- [Romu family of random number
-- generators](http://www.romu-random.org/) by Mark Overton.  While
-- the Romu generators are mostly fast on out-of-order CPUs because
-- they can be "zero latency" by fitting into otherwise unused
-- execution slots, they may also be useful on GPU.  In particular,
-- the 32-bit variants run very fast.
--
-- As usual, the seeding, splitting, and joining functions are not
-- part of the upstream Romu algorithm, but are simply best-effort
-- implementations that try to mix in a little different entropy.
-- Caveat emptor.

open import "random"

-- The programming style has been left similar to the upstream Romu
-- implementations in C.

local
def ROTL64 (x: u64, n: u64) =
  (x << n) | (x >> (64 - n))

local
def ROTL32 (x: u32, n: u32) =
  (x << n) | (x >> (32 - n))

-- | More robust than anyone could need, but uses more registers than
-- `romu_trio`@term.
--
-- * Est. capacity: 2⁹⁰ bytes
-- * State size: 256 bits
module romu_quad : rng_engine with t = u64 = {
  type t = u64
  type rng = {w: u64, x: u64, y: u64, z: u64}

  def rng_from_seed (ss: []i32) =
    loop {w, x, y, z} = {w = 1, x = 2, y = 3, z = 4}
    for s in ss do
      { w = w ^ u64.i32 s ^ 1
      , x = x ^ u64.i32 s ^ 1
      , y = y ^ u64.i32 s ^ 2
      , z = z ^ u64.i32 s ^ 3
      }

  def rand {w = wp, x = xp, y = yp, z = zp} =
    let wState = 15241094284759029579 * zp
    let xState = zp + ROTL64 (wp, 52)
    let yState = yp - xp
    let zState = yp + wp
    let zState = ROTL64 (zState, 19)
    in ({w = wState, x = xState, y = yState, z = zState}, xp)

  def split_rng (n: i64) {w, x, y, z} =
    tabulate n (\i ->
                  rand { w = w ^ u64.i64 i
                       , x = x ^ u64.i64 i
                       , y = y ^ u64.i64 i
                       , z = z ^ u64.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { w = reduce (^) 0 (map (.w) rngs)
    , x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    , z = reduce (^) 0 (map (.z) rngs)
    }

  def min = u64.lowest
  def max = u64.highest
}

-- | Great for general purpose work, including huge jobs.
--
-- * Est. capacity: 2⁷⁵ bytes
-- * State size: 192 bits
module romu_trio : rng_engine with t = u64 = {
  type t = u64
  type rng = {x: u64, y: u64, z: u64}

  def rng_from_seed (ss: []i32) =
    loop {x, y, z} = {x = 1, y = 2, z = 3}
    for s in ss do
      { x = x ^ u64.i32 s ^ 1
      , y = y ^ u64.i32 s ^ 2
      , z = z ^ u64.i32 s ^ 3
      }

  def rand {x = xp, y = yp, z = zp} =
    let xState = 15241094284759029579 * zp
    let yState = yp - xp
    let yState = ROTL64 (yState, 12)
    let zState = zp - yp
    let zState = ROTL64 (zState, 44)
    in ({x = xState, y = yState, z = zState}, xp)

  def split_rng (n: i64) {x, y, z} =
    tabulate n (\i ->
                  rand { x = x ^ u64.i64 i
                       , y = y ^ u64.i64 i
                       , z = z ^ u64.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    , z = reduce (^) 0 (map (.z) rngs)
    }

  def min = u64.lowest
  def max = u64.highest
}

-- | Might be faster than `romu_trio`@term due to using fewer
-- registers, but might struggle with massive jobs.
--
-- * Est. capacity: 2⁶¹ bytes
-- * State size: 128 bits
module romu_duo : rng_engine with t = u64 = {
  type t = u64
  type rng = {x: u64, y: u64}

  def rng_from_seed (ss: []i32) =
    loop {x, y} = {x = 1, y = 2}
    for s in ss do
      { x = x ^ u64.i32 s ^ 1
      , y = y ^ u64.i32 s ^ 2
      }

  def rand {x = xp, y = yp} =
    let xState = 15241094284759029579 * yp
    let yState = ROTL64 (yp, 36) + ROTL64 (yp, 15) - xp
    in ({x = xState, y = yState}, xp)

  def split_rng (n: i64) {x, y} =
    tabulate n (\i ->
                  rand { x = x ^ u64.i64 i
                       , y = y ^ u64.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    }

  def min = u64.lowest
  def max = u64.highest
}

-- | The fastest generator using 64-bit arithmetic, but not suited for
-- huge jobs.
--
-- * Est. capacity: 2⁵¹ bytes
-- * State size: 128 bits
module romu_duo_jr : rng_engine with t = u64 = {
  type t = u64
  type rng = {x: u64, y: u64}

  def rng_from_seed (ss: []i32) =
    loop {x, y} = {x = 1, y = 2}
    for s in ss do
      { x = x ^ u64.i32 s ^ 1
      , y = y ^ u64.i32 s ^ 2
      }

  def rand {x = xp, y = yp} =
    let xState = 15241094284759029579 * yp
    let yState = yp - xp
    let yState = ROTL64 (yState, 27)
    in ({x = xState, y = yState}, xp)

  def split_rng (n: i64) {x, y} =
    tabulate n (\i ->
                  rand { x = x ^ u64.i64 i
                       , y = y ^ u64.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    }

  def min = u64.lowest
  def max = u64.highest
}

-- | 32-bit arithmetic: Good for general purpose use.
--
-- * Est. capacity: 2⁶¹ bytes
-- * State size: 128
module romu_quad32 : rng_engine with t = u32 = {
  type t = u32
  type rng = {w: u32, x: u32, y: u32, z: u32}

  def rng_from_seed (ss: []i32) =
    loop {w, x, y, z} = {w = 1, x = 2, y = 3, z = 4}
    for s in ss do
      { w = w ^ u32.i32 s ^ 1
      , x = x ^ u32.i32 s ^ 1
      , y = y ^ u32.i32 s ^ 2
      , z = z ^ u32.i32 s ^ 3
      }

  def rand {w = wp, x = xp, y = yp, z = zp} =
    let wState = 3323815723 * zp
    let xState = zp + ROTL32 (wp, 26)
    let yState = yp - xp
    let zState = yp + wp
    let zState = ROTL32 (zState, 9)
    in ({w = wState, x = xState, y = yState, z = zState}, xp)

  def split_rng (n: i64) {w, x, y, z} =
    tabulate n (\i ->
                  rand { w = w ^ u32.i64 i
                       , x = x ^ u32.i64 i
                       , y = y ^ u32.i64 i
                       , z = z ^ u32.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { w = reduce (^) 0 (map (.w) rngs)
    , x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    , z = reduce (^) 0 (map (.z) rngs)
    }

  def min = u32.lowest
  def max = u32.highest
}

-- | 32-bit arithmetic: Good for general purpose use, except for huge jobs.
--
-- * Est. capacity: 2⁵³ bytes
-- * State size = 96 bits.
module romu_trio32 : rng_engine with t = u32 = {
  type t = u32
  type rng = {x: u32, y: u32, z: u32}

  def rng_from_seed (ss: []i32) =
    loop {x, y, z} = {x = 1, y = 2, z = 3}
    for s in ss do
      { x = x ^ u32.i32 s ^ 1
      , y = y ^ u32.i32 s ^ 2
      , z = z ^ u32.i32 s ^ 3
      }

  def rand {x = xp, y = yp, z = zp} =
    let xState = 3323815723 * zp
    let yState = yp - xp
    let yState = ROTL32 (yState, 6)
    let zState = zp - yp
    let zState = ROTL32 (zState, 22)
    in ({x = xState, y = yState, z = zState}, xp)

  def split_rng (n: i64) {x, y, z} =
    tabulate n (\i ->
                  rand { x = x ^ u32.i64 i
                       , y = y ^ u32.i64 i
                       , z = z ^ u32.i64 i
                       }
                  |> (.0))

  def join_rng (rngs: []rng) =
    { x = reduce (^) 0 (map (.x) rngs)
    , y = reduce (^) 0 (map (.y) rngs)
    , z = reduce (^) 0 (map (.z) rngs)
    }

  def min = u32.lowest
  def max = u32.highest
}

-- | 32-bit arithmetic: Suitable only up to 2²⁶
-- output-values. Outputs 16-bit numbers.  Fixed period of
-- (2³²)-47.
--
-- * Capacity: 2²⁷ bytes
-- * State size; 32 bits
module romu_mono32 : rng_engine with t = u16 = {
  type rng = u32
  type t = u16

  def rng_from_seed (ss: []i32) : rng =
    loop acc = 0u32
    for seed in ss do
      acc ^ (u32.i32 seed & 0x1fffffff) + 1156979152

  def rand (state: u32) =
    let result = u16.u32 (state >> 16)
    let state = state * 3611795771
    let state = ROTL32 (state, 12)
    in (state, result)

  def split_rng (n: i64) state =
    tabulate n (\i -> (rand (state ^ u32.i64 i)).0)

  def join_rng = reduce (^) 0u32

  def min = u16.lowest
  def max = u16.highest
}
