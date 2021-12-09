
```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```


![](loadimg-img/a292be426eec644cd64e2e13c47fce9a-img.png)

