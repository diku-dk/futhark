
```futhark
let foo [n][m] (img: [n][m]u32): [n][m]u32 =
  map (map id) img
```

```
> :img foo ($loadimg "../assets/ohyes.png")
```


![](loadimg-img/b137ba0033c18d7b522fdc0280b38e05-img.png)

