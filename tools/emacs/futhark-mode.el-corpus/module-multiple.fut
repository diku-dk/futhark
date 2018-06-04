module B {
  module A {
    include dog

    type a = int

    val t = 3

    let main () : int = 0

    entry cat () : int = 0
  }

  module C {
    type c = f32
  }
}
