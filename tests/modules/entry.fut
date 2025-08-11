-- OK to use module type in an entry point (although perhaps not
-- useful).

module M = {
             type t = bool
           }:
           {type t}

entry main (x: (M.t, M.t)) : (M.t, M.t) = x
