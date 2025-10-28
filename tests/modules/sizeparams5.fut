module m
  : {
      type~ t '~a
      val mk '~a : () -> t a
    } = {
  type~ t '~a = ()
  def mk () = ()
}

def f '~a (b: bool) : m.t a = m.mk ()
