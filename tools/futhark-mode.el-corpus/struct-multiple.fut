struct B {
  struct A {
    default (f32)

    include dog

    type a = int

    val t = 3

    fun main () : int = 0

    entry cat () : int = 0
  }

  struct C {
    type c = f32
  }
}
