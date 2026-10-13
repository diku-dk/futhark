```
> io.loadvalue "data/array.in" : [3]i32
```

```
[1, 2, 3]
```

```futhark
def add_scalar (y: i32) = map (+ y)
```

```
> let (xs : [3]i32, y) = io.loadvalue "data/array_and_value.in" in add_scalar y xs
```

```
[11, 12, 13]
```
