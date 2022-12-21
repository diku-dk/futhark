
```futhark
def fs = 44100i64
def standard_pitch = 440.0f32

def main =
  tabulate fs (\k -> f32.sin (2 * f32.pi * f32.i64 k * standard_pitch / f32.i64 fs))
     |> map ((*) (f32.i8 i8.highest))
     |> map i8.f32
```

```
> :audio main
```


![](audio-img/dc2770fc8484099098130e52533f3283-audio.wav)

