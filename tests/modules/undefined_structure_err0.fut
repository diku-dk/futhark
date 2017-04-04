-- We can not access a module before it has been defined.
-- ==
-- error: .*Unknown.*

let try_me(): i32 = M0.number()
module M0 = {
  let number(): i32 = 42
}

let main(): i32 = try_me()