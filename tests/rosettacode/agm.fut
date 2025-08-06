-- http://rosettacode.org/wiki/Arithmetic-geometric_mean
--
-- ==
-- input { 1.0f64 2.0f64 }
-- output { 1.456791f64 }

def agm (a: f64, g: f64) : f64 =
  let eps = 1.0E-16
  let (a, _) =
    loop (a, g) while f64.abs (a - g) > eps do
      ( (a + g) / 2.0
      , f64.sqrt (a * g)
      )
  in a

def main (x: f64) (y: f64) : f64 =
  agm (x, y)
