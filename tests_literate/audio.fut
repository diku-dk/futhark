def output_hz = 44100i64
def standard_pitch = 440.0f32

def pitch (i: i64): f32 =
  standard_pitch * 2 ** (f32.i64 i/12)

def num_samples (duration: f32): i64 =
  i64.f32 (f32.i64 output_hz * duration)

def sample (p: f32) (i: i64): f32 =
  f32.sin (2 * f32.pi * f32.i64 i * p / f32.i64 output_hz)

def note (i: i64) (duration: f32): []f32 =
  let p = pitch i
  let n = num_samples duration
  in tabulate n (sample p)

def break (duration: f32): []f32 =
  replicate (num_samples duration) 0.0

entry left =
  let c = note 3
  let d = note 5
  let e = note 7
  let f = note 8
  let g = note 10
  in c 0.3
       ++ d 0.3
       ++ e 0.3
       ++ c 0.3
       ++ c 0.3
       ++ d 0.3
       ++ e 0.3
       ++ c 0.3
       ++ e 0.3
       ++ f 0.3
       ++ g 0.6
       ++ e 0.3
       ++ f 0.3
       ++ g 0.6

entry right =
  let c = note 3
  let d = note 5
  let e = note 7
  let f = note 8
  let g = note 10
  in break (8 * 0.3)
           ++ c 0.3
           ++ d 0.3
           ++ e 0.3
           ++ c 0.3
           ++ c 0.3
           ++ d 0.3
           ++ e 0.3
           ++ c 0.3

-- > :audio left;
-- sampling_frequency: 44100
-- codec: ogg

-- > :audio right;
-- sampling_frequency: 44100
-- codec: ogg

entry stereo =
  let [k] left: [k]f32 = left
  let right = right :> [k]f32
  in [left, right]

-- > :audio stereo

entry surround =
  let [k] left: [k]f32 = left
  let right = right :> [k]f32
  in [left, right, right, right, right, right]

-- > :audio surround

-- > $loadaudio "mono.wav"

-- > $loadaudio "stereo.wav"
