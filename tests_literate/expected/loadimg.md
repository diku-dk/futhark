```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```

![](loadimg-img/fdccf88abb5fb0001143631ae1c452e6-img.png)
