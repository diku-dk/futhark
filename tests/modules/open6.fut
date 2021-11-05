module type dummy = {}
module dummyinst: dummy = {}

module type ModuleType = {
    val someVal: i32
}

module moduleinst: ModuleType = {
    let someVal = 0i32
}

module ModuleTypeOps (x: ModuleType) = {
    let valGetter = x.someVal
}

module HigherModule (unused: dummy) = {
    open ModuleTypeOps moduleinst
    let myGet = valGetter
}

open ModuleTypeOps moduleinst

module test = HigherModule dummyinst

entry main = test.myGet
