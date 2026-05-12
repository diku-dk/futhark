module type dict = {
  type~ dict
  val mk : dict -> (dict, dict)
}

module naive_dict : dict with dict = ?[k].[k]bool = {
  type~ dict = []bool
  def mk x = (x, drop 1 x)
}
