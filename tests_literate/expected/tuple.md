```futhark
entry f (x: i32, (y: i32, z: i32)) = x + y + z
```

```
> f (1,(2,3))
```

```
6i32
```

```futhark
entry g {x = x: i32, y = y: (i32, i32)} = x - y.0 + y.1
```

```
> g {y=(7,2),x=10}
```

```
5i32
```

```futhark
entry f' (x: i32) (y: i32) (z: i32) = (x, (y, z))
```

```
> f' 1 2 3
```

```
(1i32, (2i32, 3i32))
```

```futhark
entry g' (x: i32) (y: i32) (z: i32) = {x, y = (y, z)}
```

```
> g' 10 7 2
```

```
{x=10i32, y=(7i32, 2i32)}
```
