Let us see if this works.

```futhark
let main x = x + 2
```

```
> main 2
```

```
4i32
```


```
> main 2f32
```

**FAILED**
```
Function "main" expects 1 argument(s) of types:
i32
But applied to 1 argument(s) of types:
f32
```


The lack of a final newline here is intentional

```futhark
let x = true
```
