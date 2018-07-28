module foo_mod = import "../fut-foo/foo" -- Naughty!

let baz = (0, 1, 0)
let foo = foo_mod.foo
