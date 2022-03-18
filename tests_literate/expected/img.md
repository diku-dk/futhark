
```futhark
let checkerboard_f32 =
  map (\i -> map (\j -> f32.i64 ((j + i % 2) % 2))
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_f32
```


![](img-img/61cff8ac3353fbfdd35d9502aff4b538-img.png)


```futhark
let checkerboard_bool =
  map (\i -> map (\j -> ((j + i % 2) % 2) == 0)
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_bool
```


![](img-img/0f6dfe6c60396642fa77352f2ff84c81-img.png)


```
> :img checkerboard_bool;
file: foo.bar
```


![](img-img/foo.bar)

