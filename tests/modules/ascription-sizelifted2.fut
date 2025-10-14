module type dict = {
  type~ dict
  val mk : () -> (dict, dict)
}

module naive_dict : dict = {
  type~ dict = []bool
  def mk () = ([], [true])
}
