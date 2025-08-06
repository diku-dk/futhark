module vec : {type vec [x]} = {
  type vec [x] = [x]i32
}

def main [n] ((x: vec.vec [n]): vec.vec [n]) = 0
