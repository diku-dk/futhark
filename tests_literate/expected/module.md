Let us define a module.

```futhark
module m
  : {
```

abstract type

```futhark
      type t
    } = {
  type t = i32
```

hidden identity function

```futhark
  def f x = x
}
```

module is done!
