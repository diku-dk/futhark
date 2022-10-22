-- ==
-- error: type Op.t = i64

type L0 = i64

type L1 = [1]i64


module type Const = {
    type t
    val x: t
}

module L0: Const = {
    type t = L0
    def x : t = 0
}


module L1: Const = {
    type t = L1
    def x : t = [1i64]
}

module type Op = (X: Const) -> Const

module type Container = {
    module Op: Op
}

module L0Op: Container = {
    module Op (X: Const with t = L0): Const = {
        type t = L0
        def x = X.x + 1
    }
}


module L2 = L0Op.Op(L1)

entry main = L2.x
