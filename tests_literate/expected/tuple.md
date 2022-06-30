
```futhark
entry f (x: i32, (y: i32, z: i32)) = x + y + z
```

```
> f (1i32, (2i32, 3i32))
```

```
6i32
```


```futhark
entry g {x: i32, y: (i32,i32)} = x - y.0 + y.1
```

```
> g {y=(7i32, 2i32), x=10i32}
```

```
5i32
```

