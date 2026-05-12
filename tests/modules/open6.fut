module type dummy = {}
module dummyinst : dummy = {}

module type ModuleType = {
  val someVal : i32
}

module moduleinst : ModuleType = {
  def someVal = 0i32
}

module ModuleTypeOps (x: ModuleType) = {
  def valGetter = x.someVal
}

module HigherModule (unused: dummy) = {
  open ModuleTypeOps moduleinst
  def myGet = valGetter
}

open ModuleTypeOps moduleinst

module test = HigherModule dummyinst

entry main = test.myGet
