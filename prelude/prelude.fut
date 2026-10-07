-- | The default prelude that is implicitly available in all Futhark
-- files.

open import "soacs"
open import "array"
open import "math"
open import "functional"
open import "ad"

-- | Create single-precision float from integer.
def r32 (x: i32) : f32 = f32.i32 x

-- | Create integer from single-precision float.
def t32 (x: f32) : i32 = i32.f32 x

-- | Create double-precision float from integer.
def r64 (x: i32) : f64 = f64.i32 x

-- | Create integer from double-precision float.
def t64 (x: f64) : i32 = i32.f64 x

-- | Negate a boolean.  `not x` is the same as `!x`.  This function is
-- mostly useful for passing to `map`.
def not (x: bool) : bool = !x

-- | Semantically just identity, but serves as an optimisation
-- inhibitor.  The compiler will treat this function as a black box.
-- You can use this to work around optimisation deficiencies (or
-- bugs), although it should hopefully rarely be necessary.
-- Deprecated: use `#[opaque]` attribute instead.
def opaque 't (x: t) : t =
  #[opaque] x

-- | Semantically just identity, but at runtime, the argument value
-- will be printed.  Deprecated: use `#[trace]` attribute instead.
def trace 't (x: t) : t =
  #[trace(trace)] x

-- | Semantically just identity, but acts as a break point in
-- `futhark repl`.  Deprecated: use `#[break]` attribute instead.
def break 't (x: t) : t =
  #[break] x

-- | These operations only work in interpreted code. Trying to use them in a
-- compiled program will fail. All of these terminate execution in uncatchable
-- ways on failure - they are intended for use in `futhark literate`, test input
-- generation, etc.
module io
  : {
      -- | Return the contents of the given file as a byte array.
      val loadbytes [k] : [k]u8 -> ?[n].*[n]u8

      -- | Reads an image from the given file and returns it as a row-major
      -- array, with each pixel encoded as ARGB.
      val loadimg [k] : [k]u8 -> ?[n][m].*[n][m]u32

      -- | Read audio from the given file and returns it as a ``[][]f64``, where
      -- each row corresponds to a channel of the original soundfile. Most common
      -- audio-formats are supported, including mp3, ogg, wav, flac and opus.
      val loadaudio [k] : [k]u8 -> ?[n][m].*[n][m]f64

      -- | Load a Futhark value of known type (including size!) from the given
      -- file. If the type is a tuple, the file must contain one value for
      -- each element.
      val loadvalue 'a [k] : [k]u8 -> *a
    } = {
  def loadbytes = intrinsics.io_loadbytes
  def loadimg = intrinsics.io_loadimg
  def loadaudio = intrinsics.io_loadaudio
  def loadvalue = intrinsics.io_loadvalue
}
