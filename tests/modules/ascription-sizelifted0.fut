-- ==
-- error: "empty"

module type dict = {
  type~ dict
  val empty : dict
}

module naive_dict : dict = {
  type~ dict = []bool
  def empty = []
}
