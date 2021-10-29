
```futhark
let checkerboard_f32 =
  map (\i -> map (\j -> f32.i64 ((j + i % 2) % 2))
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_f32
```


![](img-img/9b7e52ca2ad2288cc3bb7b9950b84d33-img.png)


```futhark
let checkerboard_bool =
  map (\i -> map (\j -> ((j + i % 2) % 2) == 0)
                 (iota 8))
      (iota 8)
```

```
> :img checkerboard_bool
```


![](img-img/25ace198189bb58978f8d2beff651632-img.png)

