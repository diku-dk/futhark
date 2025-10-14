-- We can not access a module before it has been defined.
-- ==
-- error: .*Unknown.*

def try_me () : i32 = M0.number ()

module M0 = {
  def number () : i32 = 42
}

def main () : i32 = try_me ()
