module type ModuleType = {
    val someVal: i32
}

module moduleinst: ModuleType = {
    let someVal = 0i32
}

module ModuleTypeOps (x: ModuleType) = {
    let mySomeVal = x.someVal
}

open ModuleTypeOps moduleinst
open ModuleTypeOps moduleinst

entry main = mySomeVal
