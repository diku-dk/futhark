-- Is the precise spelling of numbers maintained?
--
-- Put in some Unicode for good measure: ᚴᚬᛏᛏᛅ ᚴᚬ ᚠᛅᛋᛏ

def a = 1_2_3

def b = 0x123

def c = 0rXIV

def d = 100f32

def e = 1_2i32

def h =
  [ (0xf.fp1f32, 0x11.ffp0f32, 0xf.fp-2f32, -0x11.ffp0f32, 0x0.f0p0f32)
  , (0xf.fp1, 0x11.ffp0, 0xf.fp-2, -0x11.ffp0, 0x0.f0p0)
  ]
