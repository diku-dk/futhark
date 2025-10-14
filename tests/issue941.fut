type sometype 't = #someval t

def geni32 (maxsize: i64) : sometype i64 = #someval maxsize

def genarr 'elm
           (genelm: i64 -> sometype elm)
           (ownsize: i64) : sometype ([ownsize](sometype elm)) =
  #someval (tabulate ownsize genelm)

def main = genarr geni32 1
