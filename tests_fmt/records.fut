type a =
  {a: i32 ,
   b: i32}

type b = {a: i32 , b: i32}

type c =
  {a: i32 ,
   b: i32,
   c: (bool, -- comment here
       bool,
       bool)}


def main =
  let a = 0i32
  let b = 0i32
  let c = 0i32
  let x = {a, b, c}
  let {a,b,c} = x
  let {a=a,b=b,c=c} = x
  let x = {a = a
          , b =
              b,
          c}
  in x
