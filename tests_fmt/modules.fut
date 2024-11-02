module type mt1 = {
  type a type b
  type c
}

module type mt2 = mt1 with a = i32

module type mt3 = mt1 with a = i32
                      with b = bool
                      with c = f32

module m : mt1 with a = i32
               with b = bool
               with c = f32 = {
  type a = i32
  type b = bool
  type c = f32
  def x = 123
  def y = 321
}
