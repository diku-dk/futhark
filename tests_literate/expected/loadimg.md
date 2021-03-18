
```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```


![](loadimg-img/a51e96a90792803-img.png)

