Let us see if this works.

```futhark
let main x = x + 2
```

```
> main 2
```

```
4
```


```
> main 2f32
```

**FAILED**
```
Error at basic.fut:6:11-15:
Cannot apply "main" to "2.0f32" (invalid type).
Expected: i32
Actual:   f32

```


The lack of a final newline here is intentional

```futhark
let x = true
```
