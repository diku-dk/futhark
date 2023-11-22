-- | Bindings to the C math library.
--
-- Follows the naming scheme of the C functions when feasible.
module Futhark.Util.CMath
  ( roundFloat,
    ceilFloat,
    floorFloat,
    roundDouble,
    ceilDouble,
    floorDouble,
    nextafterf,
    nextafter,
    lgamma,
    lgammaf,
    tgamma,
    tgammaf,
    erf,
    erff,
    erfc,
    erfcf,
    cbrt,
    cbrtf,
    hypot,
    hypotf,
    ldexp,
    ldexpf,
  )
where

import Foreign.C.Types (CInt (..))

foreign import ccall "nearbyint" c_nearbyint :: Double -> Double

foreign import ccall "nearbyintf" c_nearbyintf :: Float -> Float

foreign import ccall "ceil" c_ceil :: Double -> Double

foreign import ccall "ceilf" c_ceilf :: Float -> Float

foreign import ccall "floor" c_floor :: Double -> Double

foreign import ccall "floorf" c_floorf :: Float -> Float

-- | Round a single-precision floating point number correctly.
roundFloat :: Float -> Float
roundFloat = c_nearbyintf

-- | Round a single-precision floating point number upwards correctly.
ceilFloat :: Float -> Float
ceilFloat = c_ceilf

-- | Round a single-precision floating point number downwards correctly.
floorFloat :: Float -> Float
floorFloat = c_floorf

-- | Round a double-precision floating point number correctly.
roundDouble :: Double -> Double
roundDouble = c_nearbyint

-- | Round a double-precision floating point number upwards correctly.
ceilDouble :: Double -> Double
ceilDouble = c_ceil

-- | Round a double-precision floating point number downwards correctly.
floorDouble :: Double -> Double
floorDouble = c_floor

foreign import ccall "nextafter" c_nextafter :: Double -> Double -> Double

foreign import ccall "nextafterf" c_nextafterf :: Float -> Float -> Float

-- | The next representable single-precision floating-point value in
-- the given direction.
nextafterf :: Float -> Float -> Float
nextafterf = c_nextafterf

-- | The next representable double-precision floating-point value in
-- the given direction.
nextafter :: Double -> Double -> Double
nextafter = c_nextafter

foreign import ccall "lgamma" c_lgamma :: Double -> Double

foreign import ccall "lgammaf" c_lgammaf :: Float -> Float

foreign import ccall "tgamma" c_tgamma :: Double -> Double

foreign import ccall "tgammaf" c_tgammaf :: Float -> Float

-- | The system-level @lgamma()@ function.
lgamma :: Double -> Double
lgamma = c_lgamma

-- | The system-level @lgammaf()@ function.
lgammaf :: Float -> Float
lgammaf = c_lgammaf

-- | The system-level @tgamma()@ function.
tgamma :: Double -> Double
tgamma = c_tgamma

-- | The system-level @tgammaf()@ function.
tgammaf :: Float -> Float
tgammaf = c_tgammaf

foreign import ccall "hypot" c_hypot :: Double -> Double -> Double

foreign import ccall "hypotf" c_hypotf :: Float -> Float -> Float

-- | The system-level @hypot@ function.
hypot :: Double -> Double -> Double
hypot = c_hypot

-- | The system-level @hypotf@ function.
hypotf :: Float -> Float -> Float
hypotf = c_hypotf

foreign import ccall "erf" c_erf :: Double -> Double

foreign import ccall "erff" c_erff :: Float -> Float

foreign import ccall "erfc" c_erfc :: Double -> Double

foreign import ccall "erfcf" c_erfcf :: Float -> Float

-- | The system-level @erf()@ function.
erf :: Double -> Double
erf = c_erf

-- | The system-level @erff()@ function.
erff :: Float -> Float
erff = c_erff

-- | The system-level @erfc()@ function.
erfc :: Double -> Double
erfc = c_erfc

-- | The system-level @erfcf()@ function.
erfcf :: Float -> Float
erfcf = c_erfcf

foreign import ccall "cbrt" c_cbrt :: Double -> Double

foreign import ccall "cbrtf" c_cbrtf :: Float -> Float

-- | The system-level @cbrt@ function.
cbrt :: Double -> Double
cbrt = c_cbrt

-- | The system-level @cbrtf@ function.
cbrtf :: Float -> Float
cbrtf = c_cbrtf

foreign import ccall "ldexp" c_ldexp :: Double -> CInt -> Double

foreign import ccall "ldexpf" c_ldexpf :: Float -> CInt -> Float

-- | The system-level @ldexp@ function.
ldexp :: Double -> CInt -> Double
ldexp = c_ldexp

-- | The system-level @ldexpf@ function.
ldexpf :: Float -> CInt -> Float
ldexpf = c_ldexpf
