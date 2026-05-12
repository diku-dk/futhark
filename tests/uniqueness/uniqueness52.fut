type constants 'p = {v: [10]p}

type~ t =
  { i32s: []i32
  , constants: constants i32
  }

def f ({i32s, constants}: *t) : *t =
  let i32s[0] = 0
  in {i32s, constants}
