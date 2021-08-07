
```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```


![](loadimg-img/a6447a02e31e6575-img.png)

