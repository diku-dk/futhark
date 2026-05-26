-- No lifted types in entry points!
-- ==
-- error: may not be higher-order

module type mod = {
  type^ fun

  val mk : i64 -> fun

  val app : fun -> i64 -> bool
}

module mod : mod = {
  type^ fun = {f: i64 -> bool}

  def mk (n: i64) : fun = {f = \x -> n == x}

  def app (fun: fun) (n: i64) : bool =
    fun.f n
}

entry gen_record_sums (n: i64) : mod.fun =
  mod.mk n

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: mod.fun) : bool =
  mod.app input 0
