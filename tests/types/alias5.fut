-- Uniqueness goes outside-in.

type uniqlist [n] = *[n]i32

def main (p: [][]i32) : [](uniqlist []) =
  p
