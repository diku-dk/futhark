def l1 x = loop x for i < 10 do x + i

def l2 x =
  loop x for i < 10 do
    x + i

def l3 x =
  loop x = x for i < 10 do
    x + i

def l4 x =
  loop x =
    x for i < 10 do
    x + i
