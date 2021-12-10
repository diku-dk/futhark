-- Does the open declaration work at all?
-- ==
-- input { } output { 4 }

module M = {
  def the_value = 4
}

open M

def main = the_value
