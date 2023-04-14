```futhark
let checkerboard_f32 =
  map (\i -> map (\j -> f32.i64 ((j + i % 2) % 2))
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_f32
```

![](img-img/26b07dbf1f772d987b40f08e0b0e0eab-img.png)

```futhark
let checkerboard_bool =
  map (\i -> map (\j -> ((j + i % 2) % 2) == 0)
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_bool
```

![](img-img/6ffd56d07e2c485080c59b4cbd6682b0-img.png)


```
> :img checkerboard_bool;
file: foo.bar
```

![](img-img/foo.bar)
