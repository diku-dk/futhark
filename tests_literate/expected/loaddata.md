```
> $loaddata "data/array.in"
```

```
[1i32, 2i32, 3i32]
```

```futhark
let add_scalar (y: i32) = map (+y)
```

```
> let (xs, y) = $loaddata "data/array_and_value.in" in add_scalar y xs
```

```
[11i32, 12i32, 13i32]
```
