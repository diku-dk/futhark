```futhark
def foo [n] [m] (img: [n][m]u32) : [n][m]u32 =
  map (map id) img
```

```
> :img foo (io.loadimg "../assets/ohyes.png")
```

![](loadimg-img/43f5851f53fc50f23abf3df8b4808400-img.png)
