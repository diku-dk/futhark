module type mt1 = {
  type ctx
  type map 'ctx
  val mk : ctx -> map ctx
}

module mk (P: {type~ ctx}) : mt1 with ctx = P.ctx = {
  type~ ctx = P.ctx
  type map 'ctx = (i32, ctx)
  def mk ctx = (0i32, ctx)
}

module m_unit = mk {type ctx = ()}

module m_arr = mk {type~ ctx = ?[k].[k]bool}

-- Just to check whether we can make arrays of these types.
entry foo = [m_unit.mk ()]

entry bar = [m_arr.mk [true, false], m_arr.mk [false, true]]
