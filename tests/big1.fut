-- main
-- ==
-- tags { no_gtx780 }
-- no_python no_ispc compiled random input {11264000000i64} auto output
def main (n: i64) : i64 = iota n |> reduce (+) 0
