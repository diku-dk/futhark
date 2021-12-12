-- Does open shadow correctly?
-- ==
-- input { } output { 4 }

def the_value = 2

module M = {
  def the_value = 4
}

open M

def main = the_value
