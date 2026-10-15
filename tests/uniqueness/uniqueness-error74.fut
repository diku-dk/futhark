-- Piping is only fresh when the function piped into returns a fresh value.
-- "id" does not, so the result still aliases the argument.
-- ==
-- error: aliased to "xs"

def main (xs: []i32) : *[]i32 = xs |> id
