-- Heavily mangled from original test case.  This one was quite
-- hellish to shrink.
--
-- We really should not tile here at all, due to how much else is
-- going on in the kernel.  Feel free to remove this program if one
-- day we improve tiling to detect that it is not profitable.
local def hash (x: i32): i32 =
  let x = u32.i32 x
  let x = ((x >> 16) ^ x) * 73244475
  let x = ((x >> 16) ^ x) * 73244475
  let x = ((x >> 16) ^ x)
  in i32.u32 x

type rng = {state: u64, inc: u64}

def rand ({state = state, inc = inc}: rng) =
  let oldstate = state
  let state = oldstate * 6364136223846793005u64 + (inc | 1u64)
  let xorshifted = u32.u64 (((oldstate >> 18u64) ^ oldstate) >> 27u64)
  let rot = u32.u64 (oldstate >> 59u64)
  in
    ({state, inc}
    ,(xorshifted >> rot) | (xorshifted << ((-rot) & 31u32)))

type^ distribution[n] 'rng 'a 'b = {sample: rng -> (rng, [n]a)
,transform: [n]a -> [n]a
,log_prob: [n]a -> b
,log_prob_base: [n]a -> b}

module normal_diag = {
  def log_sqrt_2pi = f32.log <| f32.sqrt <| 2 * f32.pi
  
  def sample n rng: (rng, [n]f32) =
    let rngs = replicate n rng
    let (rngs, xs) = map rand rngs |> unzip
    in (rngs[0], map f32.u32 xs)
  
  def transform n means stds (xs: [n]f32) =
    map3 (\mean std x -> mean + x * std) means stds xs
  
  def log_prob n means stds (xs: [n]f32) =
    map3
      (
        \mean std x ->
          (-1) * ((x - mean) ** 2) / (2 * std ** 2) - f32.log std - log_sqrt_2pi
      ) means stds xs
    |> f32.sum
  
  def log_prob_base n (xs: [n]f32) =
    map (\x -> (-1) * (x ** 2) / 2 - log_sqrt_2pi) xs |> f32.sum
  
  def mk_dist [n] (means: [n]f32) stds: distribution [n] rng f32 f32 =
    {sample = sample n
    ,transform = transform n means stds
    ,log_prob = log_prob n means stds
    ,log_prob_base = log_prob_base n}
}

def mean [n] (xs: [n]f32) = f32.sum xs / f32.i64 n

def MAX_NUM_RV = 3i64

type addr = i64

type state[n] [n2] = {rng: rng
,log_like: f32
,trace: [MAX_NUM_RV](bool, [n]f32)
,conditioned: [MAX_NUM_RV](bool, [n]f32)
,log_probs: [MAX_NUM_RV](bool, f32)
,fix_unknown_size_n2: [n2]f32}

def mk_empty_state n n2 rng_init =
  {rng = rng_init
  ,log_like = 0.0f32
  ,trace = replicate MAX_NUM_RV (false, replicate n 0.0f32)
  ,conditioned = replicate MAX_NUM_RV (false, replicate n 0.0f32)
  ,log_probs = replicate MAX_NUM_RV (false, 0.0f32)
  ,fix_unknown_size_n2 = replicate n2 0.0f32}

-- Handlers.
type^ message[n] [n2] 'c 't2 = #sample (state [n] [n2])addr(distribution [n] rng c f32)
|#observe (state [n] [n2])addr(distribution [n2] rng t2 f32)([n]c)[n2]t2
|#return (state [n] [n2])[n]c

type^ handler [n] [n2] 'c 't2 = message [n] [n2] c t2 -> (state [n] [n2], [n]c)

def default_handler [n] [n2] 'c 't2 (req: message [n] [n2] c t2) =
  match req
    case #sample s _a d -> let (rng', c) = d.sample s.rng
      in (rng' s with rng =, c)
    case #observe s _a _d c _obs -> (s, c)
    case #return s c -> (s, c)

def store_log_probs_and_transform [n] [n2] 'c 't2 (parent_handler: handler [n] [n2] c t2) (req: message [n] [n2] c t2) =
  match req
    case #sample _s a d -> let (s: state [n] [n2], c) = parent_handler req
      let log_probs' = (copy s.log_probs) with [a] = (true, d.log_prob_base c)
      in (log_probs' s with log_probs =, d.transform c)
    case #observe _s a d _c obs -> let (s: state [n] [n2], c) = parent_handler req
      let log_probs' = (copy s.log_probs) with [a] = (true, d.log_prob obs)
      in (log_probs' s with log_probs =, c)
    case _ -> parent_handler req

def store_trace [n] [n2] 'c 't2 parent_handler (req: message [n] [n2] c t2) =
  match req
    case #sample _s a _d -> let (s: state [n] [n2], c) = parent_handler req
      let trace' = (copy s.trace) with [a] = (true, c)
      in (trace' s with trace =, c)
    case _ -> parent_handler req

def reuse_conditioned [n] [n2] 'c 't2 parent_handler (req: message [n] [n2] c t2) =
  match req
    case #sample s a _d -> let (conditioned, x) = s.conditioned[a]
      let (s, c) = if conditioned then (s, x) else parent_handler req
      let trace' = copy s.trace with [a] = (true, c)
      in (trace' s with trace =, c)
    case _ -> parent_handler req

def sample [n] [n2] 'c 't2 (h: handler [n] [n2] c t2) s a d =
  h (#sample s a d)

def observe [n] [n2] 'c 't2 (h: handler [n] [n2] c t2) s a d c obs =
  h (#observe s a d c obs)

def log_weight [n] [n2] (state_p: state [n] [n2]) (state_q: state [n] [n2]) =
  let log_ps = state_p.log_probs
  let log_qs = state_q.log_probs
  let log_p_div_q =
    map2
      (
        \(in_p, log_p) (in_q, log_q) ->
          if in_p && in_q then log_p - log_q else 0
      ) log_ps log_qs
    |> reduce (+) 0.0f32
  in state_p.log_like + log_p_div_q

def importance_sampling latent_dim n2 rng y p theta q phi =
  let sempty = mk_empty_state latent_dim n2 rng
  let handler = store_trace (store_log_probs_and_transform default_handler)
  let sq = q handler sempty y phi
  let state = sq.rng (sq.trace sempty with conditioned =) with rng =
  let handler = reuse_conditioned handler
  let sp = p handler state y theta
  let log_w = log_weight sp sq
  let log_p = (reduce (+) 0 (map (.1) sp.log_probs))
  in (sp.rng, log_w, log_p)

def grad 'a (f: a -> f32) (primal: a) = vjp f primal (f32.i32 1)

def normal (means, stddevs) =
  normal_diag.mk_dist means stddevs

def dot (xs: []f32) (ys: []f32) =
  f32.sum (map2 (*) xs ys)

def matvecmul [n] [m] (xss: [n][m]f32) (ys: [m]f32): *[n]f32 =
  map (dot ys) xss

def dense (in_dim: i64) (out_dim: i64) =
  let init (initfn: (n: i64) -> rng -> (rng, [n]f32)) rng=
    let (rng, weights) = initfn (in_dim * out_dim) rng
    let (_, bias) = initfn out_dim rng
    in (unflatten weights, bias)
  let apply (params: ([out_dim][in_dim]f32, [out_dim]f32)) (xs: [in_dim]f32): [out_dim]f32 =
    let (weightsT, bias) = params
    in map2 (+) (matvecmul weightsT xs) bias
  in apply

def LATENT_DIM = 2i64

def INPUT_DIM = 8i64

def model decoder handler s y theta =
  let (mu, sigma) = (replicate LATENT_DIM 0.0f32, replicate LATENT_DIM 1.0f32)
  let (s, z) = sample handler s 0 (normal (mu, sigma))
  let y_probs = (decoder theta z)
  let (s, _) = observe handler s 1 (normal (y_probs, y_probs)) z y
  in s

entry main [batch_sz] rng theta phi (ys: [batch_sz][INPUT_DIM]f32) =
  let (f_apply) = dense LATENT_DIM INPUT_DIM
  let fake_init n rng= (rng, replicate n 1.0f32)
  let (model, guide) = (model f_apply, model f_apply)
  let elbo_loss rngs ys (theta, phi)=
    let elbos =
      map2
        (
          \rng y ->
            let (_rng, log_w, log_p) = importance_sampling LATENT_DIM INPUT_DIM rng y model theta guide phi
            in log_w * log_p
        ) rngs ys
    in mean elbos
  in grad (elbo_loss rng ys) (theta, phi)