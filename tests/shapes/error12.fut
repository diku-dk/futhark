-- No hiding sizes inside sum types!
-- ==
-- error: Cannot apply "genarr"

type sometype 't = #someval t

def geni64 (maxsize: i64) : sometype i64 = #someval maxsize

def genarr 'elm
           (genelm: i64 -> sometype elm)
           (ownsize: i64) : sometype ([ownsize](sometype elm)) =
  #someval (tabulate ownsize genelm)

def main = genarr (genarr geni64) 1
