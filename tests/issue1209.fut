type ObjectGeom =
    #Wall f32
  | #Block ([3]f32)

type Object =
    #PassiveObject ObjectGeom
  | #Light ([3]f32)

def main (_: i32) : Object =
  #PassiveObject (#Block [1.0, -1.6, 1.2])
