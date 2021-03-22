
```futhark
let checkerboard_f32 =
  map (\i -> map (\j -> f32.i64 ((j + i % 2) % 2))
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_f32
```


![](img-img/da58d646eb37f5f-img.png)


```futhark
let checkerboard_bool =
  map (\i -> map (\j -> ((j + i % 2) % 2) == 0)
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_bool
```


![](img-img/12ad892d6aae2374-img.png)

