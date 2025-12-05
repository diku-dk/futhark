def and (_: []bool) : bool = ???

def inf = ???

def Assume (_: bool) : bool = ???

def Disjoint 't (_: i64 -> t) : bool = ???

def Monotonic 't (_ : t -> t -> bool) _ : bool = ???

def Range 't (_: t) (_ : (i64, i64)) : bool = ???

def Injective 't (_: []t) : bool = ???

def InjectiveRCD 't (_: []t) (_ : (i64, i64)) : bool = ???

def BijectiveRCD 't (_: []t) (_ : (i64, i64)) (_ : (i64, i64)) : bool = ???

def FiltPartInv (_X: []i64) (_pf: i64 -> bool) (_pp: i64 -> bool) = ???

def FiltPart 't (_X :[]t) (_Y: []t) (_pf: i64 -> bool) (_pp: i64 -> bool) =
  ???

def FiltPartInv2 (_X: []i64) (_pf: i64 -> bool) (_pp: i64 -> bool) (_pp2: i64 -> bool) = ???

def FiltPart2 't (_X :[]t) (_Y: []t) (_pf: i64 -> bool) (_pp: i64 -> bool) (_pp2: i64 -> bool) =
  ???
