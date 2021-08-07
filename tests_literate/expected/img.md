
```futhark
let checkerboard_f32 =
  map (\i -> map (\j -> f32.i64 ((j + i % 2) % 2))
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_f32
```


![](img-img/68567c4284c95bb9f52ea53ee8135145-img.png)


```futhark
let checkerboard_bool =
  map (\i -> map (\j -> ((j + i % 2) % 2) == 0)
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_bool
```


![](img-img/ef2e4404d70e1334398fecafbad60772-img.png)

