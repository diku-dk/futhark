type size [n] = [n]()
type~ state = size []

entry construct (n: i64) : state = replicate n ()

entry destruct (s: state) : i64 = length s
