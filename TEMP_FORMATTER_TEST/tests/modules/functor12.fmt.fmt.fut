-- Even more Complicated nested modules in parametric modules ought to
-- work.
--
-- ==
-- input { [true,false] } output { [true,false] [0,0] }
module type mt =
{
  type cell
  val init: bool -> cell
}

module f1 (R: mt) = {
  type cell = R.cell
  
  def init [n] (bs: [n]bool): [n]cell =
    map R.init bs
}

module f2 (R: mt) = {
  open ()
    f1
      {
        type cell = (R.cell, i32)
        
        def init (b: bool) = (R.init b, 0)
      }
  )
}

module m1 = {
  type cell = bool
  
  def init (b: bool) = b
}

module m2 = f2 {m1}

def main [n] (bs: [n]bool) =
  unzip (m2.init bs)