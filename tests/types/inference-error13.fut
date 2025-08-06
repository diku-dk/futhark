-- A record turns out to have the wrong type of field.
-- ==
-- error:

def f r =
  let y: f32 = r.l
  in (r : {l: i32})
