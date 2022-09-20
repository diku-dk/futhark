
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
entry g {x: i32, y: (i32,i32)} = x - y.0 + y.1
```

```
> g {y=(7,2),x=10}
```

```
5i32
```

