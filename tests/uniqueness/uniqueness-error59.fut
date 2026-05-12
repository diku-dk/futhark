-- ==
-- error: aliases the free variable "global"

def global = ([1, 2, 3], 0)

def return_global () = global

def main i = (return_global ()).0 with [i] = 0
