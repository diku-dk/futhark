-- ==
-- error: "return_global", which is not consumable

def global = ([1, 2, 3], 0)

def return_global () = global

def main i = (return_global ()).0 with [i] = 0
