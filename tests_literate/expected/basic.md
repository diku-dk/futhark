Let us see if this works.

```futhark
let main x = x + 2
```

```
> main 2i32
```

```
4i32
```


```
> main 2.0f32
```

**FAILED**
```
Function "main" expects arguments of types:
i32
But called with arguments of types:
f32
```

The lack of a final newline here is intentional

```futhark
let x = true
```

