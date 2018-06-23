-- Test that we can explicitly import the prelude.

module prelude = import "/futlib/prelude"

let main = prelude.t32
