module type MyModuleType = {
  val dummy : i32
}

module MyModuleOps (thisMod: MyModuleType) = {
  def test = (copy (1...10i32))
}

module MyModule : MyModuleType = {
  def dummy = 0i32
}

module MyModule_Ops = MyModuleOps MyModule

module MyModule2 : MyModuleType = {
  def dummy = 1i32
}

module MyModule2_Ops = MyModuleOps MyModule2

entry testfn =
  if true
  then MyModule_Ops.test
  else MyModule2_Ops.test
