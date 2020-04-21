-- Proper inference of size in sum type in negative position, even
-- when not all the constructors of the sum type are known yet.

type sometype 'a = #someval a

let error : i32 -> sometype ([]i32) = \_ -> #someval []
