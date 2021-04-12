
```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```


![](loadimg-img/8af55c5b45f68f3e-img.png)

