type ObjectGeom =
    #Wall f64
  | #Block ([3]f64)

type Object =
    #PassiveObject ObjectGeom
  | #Light ([3]f64)

def main (_: i32) : Object =
  #PassiveObject (#Block [1.0, -1.6, 1.2])
