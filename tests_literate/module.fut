-- Let us define a module.

module m
  : {
      -- abstract type
      type t
    } = {
  type t = i32

  -- hidden identity function
  def f x = x
}

-- module is done!
