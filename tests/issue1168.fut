type triad 't = (t, t, t)
def triadMap 'a 'b (f: a -> b) (A: triad a) : triad b = (f A.0, f A.1, f A.2)
def triadMap2 'a 'b 'c (f: a -> b -> c) (A: triad a) (B: triad b) : triad c = (f A.0 B.0, f A.1 B.1, f A.2 B.2)
def triadZip 'a 'b (A: triad a) (B: triad b) : triad (a, b) = triadMap2 (\a b -> (a, b)) A B
def triadUnzip 'a 'b (A: triad (a, b)) : (triad a, triad b) = (triadMap (.0) A, triadMap (.1) A)
def triadFold 'a (f: a -> a -> a) (A: triad a) : a = f A.0 <| f A.1 A.2
def triadShiftR 'a (A: triad a) : triad a = (A.2, A.0, A.1)
def triadShiftL 'a (A: triad a) : triad a = (A.1, A.2, A.0)
def triplet a = (a, a, a)

type v3 = triad f32
type m33 = triad v3

type quaternion = {r: f32, v: v3}

def v3sum (v: v3) : f32 = triadFold (+) v
def v3add (a: v3) (b: v3) : v3 = triadMap2 (+) a b
def v3sub (a: v3) (b: v3) : v3 = triadMap2 (-) a b
def v3mul (a: v3) (b: v3) : v3 = triadMap2 (*) a b
def cross (a: v3) (b: v3) : v3 = (a.1 * b.2 - a.2 * b.1, a.2 * b.0 - a.0 * b.2, a.0 * b.1 - a.1 * b.0)
def v3dot (a: v3) (b: v3) : f32 = v3mul a b |> v3sum
def scaleV3 (f: f32) = triadMap (* f)
def v3negate = triadMap f32.neg
def v3outer a b = triadMap (\f -> scaleV3 f b) a

def sumV3s = reduce_comm v3add (triplet 0)

def m33map2 f (A: m33) (B: m33) : m33 = triadMap2 (triadMap2 f) A B
def m33add = m33map2 (+)

def m33transpose (m: m33) =
  ( (m.0.0, m.1.0, m.2.0)
  , (m.0.1, m.1.1, m.2.1)
  , (m.0.2, m.1.2, m.2.2)
  )

def mvMult (m: m33) (v: v3) : v3 = triadMap (v3dot v) m

def sumM33s = reduce_comm m33add (triplet <| triplet 0)

def m33fromQuaternion (q: quaternion) : m33 =
  let a = q.r
  let b = q.v.0
  let c = q.v.1
  let d = q.v.2
  let aa = a * a
  let ab = a * b
  let ac = a * c
  let ad = a * d
  let bb = b * b
  let bc = b * c
  let bd = b * d
  let cc = c * c
  let cd = c * d
  let dd = d * d
  in ( (aa + bb - cc - dd, 2 * (bc - ad), 2 * (bd + ac))
     , (2 * (bc + ad), aa - bb + cc - dd, 2 * (cd - ab))
     , (2 * (bd - ac), 2 * (cd + ab), aa - bb - cc + dd)
     )

def dot a b = map2 (*) a b |> f32.sum
def outer [m] [n] (as: [m]f32) (bs: [n]f32) = map (\a -> map (* a) bs) as
def matVecMul [m] [n] (A: [m][n]f32) (b: [n]f32) : [m]f32 = map (dot b) A

def hash (x: u32) : u32 =
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x) * 0x45d9f3b
  let x = ((x >> 16) ^ x)
  in x

-- xoshiro128**
type PRNG = {state: (u32, u32, u32, u32)}
def rotl (x: u32) (k: u32) = x << k | x >> 32 - k

def next (g: PRNG) : (u32, PRNG) =
  let (a, b, c, d) = g.state
  let res = (rotl (b * 5) 7) * 9
  let t = b << 9
  let c = c ^ a
  let d = d ^ b
  let b = b ^ c
  let a = a ^ d
  let c = c ^ t
  let d = rotl d 11
  in (res, {state = (a, b, c, d)})

def split n (g: PRNG) =
  let (a, b, c, d) = g.state
  let (r, g') = next g
  let splitG i =
    let i' = u32.i64 i
    let r' = hash (r ^ i')
    let f a = hash (r' ^ a)
    in {state = (f a, f b, f c, f d)}
  in (tabulate n splitG, g')

def newGen (seed: i32) : PRNG =
  let seed' = u32.i32 seed
  let h0 = hash seed'
  let h1 = hash (seed' + 1)
  let h2 = hash (seed' + 2)
  let h3 = hash (seed' + 3)
  let a = h0 ^ h1
  let b = h1 ^ h2
  let c = h2 ^ h3
  let d = h3 ^ h0
  in {state = (a, b, c, d)}

def genI32 g =
  let (r, g) = next g
  in (i32.u32 r, g)

def genF32 g =
  let (i, g) = genI32 g
  in (f32.from_fraction (i64.i32 i) (i64.i32 i32.highest), g)

def randomArray 'a (f: (PRNG -> (a, PRNG))) n g : ([]a, PRNG) =
  split n g |> (\(a, g) -> (map f a |> map (.0), g))

def sample [m] 'a (array: [m]a) (n: i64) (g: PRNG) : ([]a, PRNG) =
  let (indices, g') = randomArray genI32 n g
  let samples = map (\i -> array[i32.abs (i % i32.i64 m)]) indices
  in (samples, g')

def normalPair g : ((f32, f32), PRNG) =
  let iteration (_, _, _, g) =
    let (x, g) = genF32 g
    let (y, g) = genF32 g
    let s = x * x + y * y
    in (s, x, y, g)
  let start = iteration (0 : f32, 0 : f32, 0 : f32, g)
  let (s, x, y, g) = iterate_until (\(s, _, _, _) -> 0 < s && s < 1) iteration start
  let f = f32.sqrt (-2 * f32.log s / s)
  in ((f * x, f * y), g)

def randomV3 g =
  let (x, g) = genF32 g
  let (y, g) = genF32 g
  let (z, g) = genF32 g
  in ((x, y, z), g)

-- uniform distribution in the unit sphere
def uniformSpherical g =
  let iteration (_, g) =
    let (x, g) = genF32 g
    let (y, g) = genF32 g
    let (z, g) = genF32 g
    in ((x, y, z), g)
  let start = iteration ((0 : f32, 0 : f32, 0 : f32), g)
  in iterate_until (\((x, y, z), _) -> x * x + y * y + z * z < 1) iteration start

type^ network 'p 'i 'o =
  { parameter: p
  , propagation: p -> i -> (o, o -> (i, p))
  , scale: f32 -> p -> p
  , add: p -> p -> p
  , zero: p
  , sum: (k: i64) -> [k]p -> p
  }

def eval 'p 'i 'o (net: network p i o) (input: i) =
  (net.propagation net.parameter input).0

def gradient 'p 'i 'o (errF: o -> o -> o) (net: network p i o) (input: i, ref: o) =
  let (o, f) = net.propagation net.parameter input
  in (.1) <| f <| errF ref o

def gradientDescentStep [n] 'p 'i 'o
                        (lr: f32) (errF: o -> o -> o) (net: network p i o) (samples: [n](i, o)) : network p i o =
  let g = map (gradient errF net) samples |> net.sum n
  in { parameter = net.add net.parameter (net.scale (-lr / f32.i64 n) g)
     , propagation = net.propagation
     , scale = net.scale
     , add = net.add
     , zero = net.zero
     , sum = net.sum
     }

def gradientDescent [n] 'p 'i 'o
                    (lr: f32)
                    (mf: f32)
                    (batchSize: i64)
                    (steps: i32)
                    (errF: o -> o -> o)
                    (net: network p i o)
                    (samples: [n](i, o))
                    (momentum: p)
                    (gen: PRNG) : (network p i o, p, PRNG) =
  let gradient p (input, ref) =
    let (o, f) = net.propagation p input
    in (.1) <| f <| errF ref o
  let iteration (p: p, m: p, gen: PRNG): (p, p, PRNG) =
    let (batch, gen) = sample samples batchSize gen
    let grad = map (gradient p) batch |> net.sum batchSize
    let m = net.add (net.scale mf m) (net.scale (1 - mf) grad)
    in (net.add p (net.scale (-lr / f32.i64 n) m), m, gen)
  let (parameter, momentum, gen) = iterate steps iteration (net.parameter, momentum, gen)
  in ( { parameter
       , propagation = net.propagation
       , scale = net.scale
       , add = net.add
       , zero = net.zero
       , sum = net.sum
       }
     , momentum
     , gen
     )

-- This is where the magic happens, the magic of function composition
def chain 'p1 'p2 'i 'm 'o (a: network p1 i m) (b: network p2 m o) : network (p1, p2) i o =
  let parameter = (a.parameter, b.parameter)
  let propagation (ap, bp) i =
    let (m, af) = a.propagation ap i
    let (o, bf) = b.propagation bp m
    let f o' =
      let (m', bg) = bf o'
      let (i', ag) = af m'
      in (i', (ag, bg))
    in (o, f)
  let scale f (ap, bp) = (a.scale f ap, b.scale f bp)
  let add (ap, bp) (ap', bp') = (a.add ap ap', b.add bp bp')
  let zero = (a.zero, b.zero)
  let sum k = unzip >-> (\(as, bs) -> (a.sum k as, b.sum k bs))
  in {parameter, propagation, scale, add, zero, sum}

def (<>) = chain

def stateless 'i 'o (propagation': i -> (o, o -> i)) : network () i o =
  let parameter = ()
  let propagation _ i = propagation' i |> (\(a, b) -> (a, b >-> (\i -> (i, ()))))
  let scale _ _ = ()
  let add _ _ = ()
  let zero = ()
  let sum _ _ = ()
  in {parameter, propagation, scale, add, zero, sum}

--evaluates pairs of values in the same network
def pairNetwork 'p 'i 'o (net: network p i o) : network p (i, i) (o, o) =
  let pairMap f (a, b) = (f a, f b)
  let parameter = net.parameter
  let propagation param i =
    let ((o1, f1), (o2, f2)) = pairMap (net.propagation param) i
    let f (o1, o2) =
      (f1 o1, f2 o2)
      |> (\((i1, g1), (i2, g2)) -> ((i1, i2), net.add g1 g2))
    in ((o1, o2), f)
  let scale = net.scale
  let add = net.add
  let zero = net.zero
  let sum = net.sum
  in {parameter, propagation, scale, add, zero, sum}

def linear [m] [n] (weights: [m][n]f32) : network ([m][n]f32) ([n]f32) ([m]f32) =
  let parameter = weights
  let propagation ws i =
    let forward = matVecMul ws i
    let backward o =
      let i' = matVecMul (transpose ws) o
      let g = outer o i
      in (i', g)
    in (forward, backward)
  let scale f = map (map (* f))
  let add = map2 (map2 (+))
  let zero = replicate m (replicate n 0)
  let sum k (ps: [k][m][n]f32) = ps |> transpose |> map transpose |> map (map f32.sum)
  in {parameter, propagation, scale, add, zero, sum}

def sum =
  let propagation [n] (is: [n]f32) =
    let forward = f32.sum is
    let backward = replicate n
    in (forward, backward)
  in stateless propagation

def sumInv n =
  let propagation (i: f32) =
    let forward = replicate n i
    let backward os = f32.sum os
    in (forward, backward)
  in stateless propagation

def genMap [n] 'p 'i 'o
           (ps: [n]p)
           (f: p -> i -> o)
           (df: p -> i -> o -> (i, p))
           (scale': f32 -> p -> p)
           (add': p -> p -> p)
           (zero': p) : network ([n]p) ([n]i) ([n]o) =
  let parameter = ps
  let propagation ps is = (map2 f ps is, map3 df ps is >-> unzip)
  let scale f = map (scale' f)
  let add = map2 add'
  let zero = replicate n zero'
  let sum k (ps: [k][n]p) = ps |> transpose |> map (reduce_comm add' zero')
  in {parameter, propagation, scale, add, zero, sum}

def statelessMap 'i 'o (n: i64) (f: i -> o) (df: i -> o -> i) =
  genMap (replicate n ()) (\_ -> f) (\_ i o -> (df i o, ())) (\_ _ -> ()) (\_ _ -> ()) ()

def bias [n] (biases: [n]f32) = genMap biases (+) (\_ _ o -> (o, o)) (*) (+) 0
def scale [n] (factors: [n]f32) = genMap factors (*) (\f i o -> (f * o, i * o)) (*) (+) 0

def smoothInvQuadMap [n] (cs: [n]f32) : network ([n]f32) ([n]v3) ([n]f32) =
  let f c v = f32.exp (-c * c * v3dot v v)
  let parameter = cs
  let propagation cs is =
    let forward = map2 f cs is
    let backward os =
      let ss = map2 (\f c -> -2 * c * f) forward cs
      let is' = map4 (\c s o i -> scaleV3 (c * s * o) i) cs ss os is
      let gs = map3 (\s o i -> s * o * v3dot i i) ss os is
      in (is', gs)
    in (forward, backward)
  let scale f = map (* f)
  let add = map2 (+)
  let zero = replicate n 0
  let sum k (ps: [k][n]f32) = ps |> transpose |> map f32.sum
  in {parameter, propagation, scale, add, zero, sum}

def particlePairs [n] (pairs: [n](v3, v3)) : network ([n](v3, v3)) (v3, m33) ([n]v3) =
  let parameter = pairs
  let propagation ps (p, R) =
    let forward = map (\(u, v) -> p `v3add` mvMult R v `v3sub` u) ps
    let backward os =
      let i =
        let pg = sumV3s os
        let Rg = map2 (\(_, v) o -> v3outer o v) ps os |> sumM33s
        -- check outer!!!
        in (pg, Rg)
      let g =
        let R' = m33transpose R
        in map (\o -> (v3negate o, mvMult R' o)) os
      in (i, g)
    in (forward, backward)
  let scale f = map (\(u, v) -> (scaleV3 f u, scaleV3 f v))
  let add = map2 (\(u, v) (u', v') -> (v3add u u', v3add v v'))
  let zero = replicate n ((0, 0, 0), (0, 0, 0))
  let sum k (ps: [k][n](v3, v3)) =
    ps |> transpose
    |> map (reduce_comm (\(u, v) (u', v') -> (v3add u u', v3add v v')) ((0, 0, 0), (0, 0, 0)))
  in {parameter, propagation, scale, add, zero, sum}

def atanMap n = statelessMap n (f32.atan) (\x e -> e / (x * x + 1))

def testNet [m] (fs1: [m]f32) = sumInv m <> scale fs1 <> sum

type^ interactionNet [n] 'p = {net: network p (v3, m33) f32, pairs: p -> [n](v3, v3)}

type networkParameter [m] [n] = ([m](v3, v3), ((((((([m]f32), [n][m]f32), [n]()), [n][n]f32), [n]()), [n]f32), ()))

def interactionNet [m] [n] (pairs: [m](v3, v3)) (cs: [m]f32) (ws1: [n][m]f32) (ws2: [n][n]f32) (fs: [n]f32) =
  let net =
    particlePairs pairs
    <> (smoothInvQuadMap cs
        <> linear ws1
        <> atanMap n
        <> linear ws2
        <> atanMap n
        <> scale fs
        <> sum)
  let pairs (x, _) = x
  in {net, pairs}

type coordinate = (v3, quaternion)
type sample = (coordinate, f32)
type^ iNet [m] [n] = interactionNet [m] (networkParameter [m] [n])

def fromParameter [m] [n] (pairs, cs, ws1, ws2, fs) (parameter: networkParameter [m] [n]) : iNet [m] [n] =
  let inet = interactionNet pairs cs ws1 ws2 fs
  let net = inet.net
  let net' =
    { parameter
    , add = net.add
    , scale = net.scale
    , sum = net.sum
    , propagation = net.propagation
    , zero = net.zero
    }
  in {net = net', pairs = inet.pairs}

def main [m] [n]
         lr
         mf
         batchSize
         steps
         stuff
         (netParameter: networkParameter [m] [n])
         (momentum: networkParameter [m] [n])
         (samples: []sample)
         (gen: PRNG) =
  let inet = fromParameter stuff netParameter
  let samples' = map (\((p, o), r) -> ((p, m33fromQuaternion o), r)) samples
  let (_, momentum', _) =
    gradientDescent lr
                    mf
                    batchSize
                    steps
                    (\a b -> b - a)
                    inet.net
                    samples'
                    momentum
                    gen
  in momentum'
