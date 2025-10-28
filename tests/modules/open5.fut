module type ModuleType = {
  val someVal : i32
}

module moduleinst : ModuleType = {
  def someVal = 0i32
}

module ModuleTypeOps (x: ModuleType) = {
  def mySomeVal = x.someVal
}

open ModuleTypeOps moduleinst
open ModuleTypeOps moduleinst

entry main = mySomeVal
