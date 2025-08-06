-- ==
-- error: "empties"

module type dict = {
  type~ dict
  val empties : (dict, dict)
}

module naive_dict : dict = {
  type~ dict = []bool
  def empties = ([], [true])
}
