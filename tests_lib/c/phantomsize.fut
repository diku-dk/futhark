type size [n] = [0][n]()
type~ state = size []

entry construct (n: i64) : state = [] : [][n]()

entry destruct (s: state) : i64 =
  let [n] (_: size [n]) = s in n
