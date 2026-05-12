-- Size for something that is partially applied.

def f [n] (x: [n]f32) : [n]f32 = x

def main : []f32 -> []f32 = f
