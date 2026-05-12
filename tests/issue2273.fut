module fraction = {
  type t = (i64, i64)

  local
  def gcd a b =
    let a = i64.abs a
    let b = i64.abs b
    in (loop (a, b) = (i64.abs a, i64.abs b)
        while a != b && a != 0 do
          let (a, b) = if a < b then (a, b) else (b, a)
          in (a, b - a)).1

  def reduce (a: i64, b: i64) =
    let s = i64.sgn b
    let a = s * a
    let b = s * b
    let d = gcd a b
    in (a / d, b / d)

  def add (a, b) (c, d) = reduce (a * d + b * c, b * d)
  def sub (a, b) (c, d) = reduce (a * d - b * c, b * d)
  def mul (a, b) (c, d) = reduce (a * c, b * d)
  def div (a, b) (c, d) = reduce (a * d, b * c)
  def zero : t = (0, 1)
  def one : t = (1, 1)
}

module SI (v: real) = {
  module f = fraction

  type unit [s]
            [s_d]
            [m]
            [m_d]
            [kg]
            [kg_d]
            [A]
            [A_d]
            [K]
            [K_d]
            [mol]
            [mol_d]
            [cd]
            [cd_d] =
    [0][s][s_d][m][m_d][kg][kg_d][A][A_d][K][K_d][mol][mol_d][cd][cd_d]()

  type value [s]
             [s_d]
             [m]
             [m_d]
             [kg]
             [kg_d]
             [A]
             [A_d]
             [K]
             [K_d]
             [mol]
             [mol_d]
             [cd]
             [cd_d] =
    (v.t, unit [s] [s_d] [m] [m_d] [kg] [kg_d] [A] [A_d] [K] [K_d] [mol] [mol_d] [cd] [cd_d])

  def unit (s: f.t) (m: f.t) (kg: f.t) (A: f.t) (K: f.t) (mol: f.t) (cd: f.t) : unit [s.0] [s.1] [m.0] [m.1] [kg.0] [kg.1] [A.0] [A.1] [K.0] [K.1] [mol.0] [mol.1] [cd.0] [cd.1] =
    []

  def add [s0]
          [s_d0]
          [m0]
          [m_d0]
          [kg0]
          [kg_d0]
          [A0]
          [A_d0]
          [K0]
          [K_d0]
          [mol0]
          [mol_d0]
          [cd0]
          [cd_d0]
          [s1]
          [s_d1]
          [m1]
          [m_d1]
          [kg1]
          [kg_d1]
          [A1]
          [A_d1]
          [K1]
          [K_d1]
          [mol1]
          [mol_d1]
          [cd1]
          [cd_d1]
          (a: value [s0] [s_d0] [m0] [m_d0] [kg0] [kg_d0] [A0] [A_d0] [K0] [K_d0] [mol0] [mol_d0] [cd0] [cd_d0])
          (b: value [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1]) =
    (a.0 v.+ b.0, a.1 :> unit [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1])

  def sub [s0]
          [s_d0]
          [m0]
          [m_d0]
          [kg0]
          [kg_d0]
          [A0]
          [A_d0]
          [K0]
          [K_d0]
          [mol0]
          [mol_d0]
          [cd0]
          [cd_d0]
          [s1]
          [s_d1]
          [m1]
          [m_d1]
          [kg1]
          [kg_d1]
          [A1]
          [A_d1]
          [K1]
          [K_d1]
          [mol1]
          [mol_d1]
          [cd1]
          [cd_d1]
          (a: value [s0] [s_d0] [m0] [m_d0] [kg0] [kg_d0] [A0] [A_d0] [K0] [K_d0] [mol0] [mol_d0] [cd0] [cd_d0])
          (b: value [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1]) =
    (a.0 v.- b.0, a.1 :> unit [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1])

  def mul [s0]
          [s_d0]
          [m0]
          [m_d0]
          [kg0]
          [kg_d0]
          [A0]
          [A_d0]
          [K0]
          [K_d0]
          [mol0]
          [mol_d0]
          [cd0]
          [cd_d0]
          [s1]
          [s_d1]
          [m1]
          [m_d1]
          [kg1]
          [kg_d1]
          [A1]
          [A_d1]
          [K1]
          [K_d1]
          [mol1]
          [mol_d1]
          [cd1]
          [cd_d1]
          (a: value [s0] [s_d0] [m0] [m_d0] [kg0] [kg_d0] [A0] [A_d0] [K0] [K_d0] [mol0] [mol_d0] [cd0] [cd_d0])
          (b: value [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1]) =
    ( a.0 v.* b.0
    , unit (f.add (s0, s_d0) (s1, s_d1))
           (f.add (m0, m_d0) (m1, m_d1))
           (f.add (kg0, kg_d0) (kg1, kg_d1))
           (f.add (A0, A_d0) (A1, A_d1))
           (f.add (K0, K_d0) (K1, K_d1))
           (f.add (mol0, mol_d0) (mol1, mol_d1))
           (f.add (cd0, cd_d0) (cd1, cd_d1))
    )

  def div [s0]
          [s_d0]
          [m0]
          [m_d0]
          [kg0]
          [kg_d0]
          [A0]
          [A_d0]
          [K0]
          [K_d0]
          [mol0]
          [mol_d0]
          [cd0]
          [cd_d0]
          [s1]
          [s_d1]
          [m1]
          [m_d1]
          [kg1]
          [kg_d1]
          [A1]
          [A_d1]
          [K1]
          [K_d1]
          [mol1]
          [mol_d1]
          [cd1]
          [cd_d1]
          (a: value [s0] [s_d0] [m0] [m_d0] [kg0] [kg_d0] [A0] [A_d0] [K0] [K_d0] [mol0] [mol_d0] [cd0] [cd_d0])
          (b: value [s1] [s_d1] [m1] [m_d1] [kg1] [kg_d1] [A1] [A_d1] [K1] [K_d1] [mol1] [mol_d1] [cd1] [cd_d1]) =
    ( a.0 v./ b.0
    , unit (f.sub (s0, s_d0) (s1, s_d1))
           (f.sub (m0, m_d0) (m1, m_d1))
           (f.sub (kg0, kg_d0) (kg1, kg_d1))
           (f.sub (A0, A_d0) (A1, A_d1))
           (f.sub (K0, K_d0) (K1, K_d1))
           (f.sub (mol0, mol_d0) (mol1, mol_d1))
           (f.sub (cd0, cd_d0) (cd1, cd_d1))
    )

  def pow [s] [s_d] [m] [m_d] [kg] [kg_d] [A] [A_d] [K] [K_d] [mol] [mol_d] [cd] [cd_d]
          (base: value [s] [s_d] [m] [m_d] [kg] [kg_d] [A] [A_d] [K] [K_d] [mol] [mol_d] [cd] [cd_d])
          (exponent: f.t) =
    ( base.0 v.** v.from_fraction exponent.0 exponent.1
    , unit (f.mul exponent (s, s_d))
           (f.mul exponent (m, m_d))
           (f.mul exponent (kg, kg_d))
           (f.mul exponent (A, A_d))
           (f.mul exponent (K, K_d))
           (f.mul exponent (mol, mol_d))
           (f.mul exponent (cd, cd_d))
    )

  def (+) = add
  def (-) = sub
  def (*) = mul
  def (/) = div
  def lift (a: v.t) = (a, unit f.zero f.zero f.zero f.zero f.zero f.zero f.zero)

  module units = {
    -- base units
    def s = (v.i32 1, unit f.one f.zero f.zero f.zero f.zero f.zero f.zero)
    def m = (v.i32 1, unit f.zero f.one f.zero f.zero f.zero f.zero f.zero)
    def kg = (v.i32 1, unit f.zero f.zero f.one f.zero f.zero f.zero f.zero)
    def A = (v.i32 1, unit f.zero f.zero f.zero f.one f.zero f.zero f.zero)
    def K = (v.i32 1, unit f.zero f.zero f.zero f.zero f.one f.zero f.zero)
    def mol = (v.i32 1, unit f.zero f.zero f.zero f.zero f.zero f.one f.zero)
    def cd = (v.i32 1, unit f.zero f.zero f.zero f.zero f.zero f.zero f.one)
  }
}

local open SI f32
local open units

entry velocity = lift 10 * m / s
entry time = lift 100 * s
entry distance = velocity * time
