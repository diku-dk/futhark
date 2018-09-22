module foo_mod = import "../fut-foo/foo" -- Naughty!
module bar_mod = import "../fut-bar/bar" -- Naughty!

let baz = (0, 1, 1)
let foo = foo_mod.foo
let bar = bar_mod.bar
