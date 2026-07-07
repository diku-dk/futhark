-- Non-exhaustive match with wildcard patterns in a sum type should trigger a warning.
-- ==
-- error: Unmatched

type s = i64
type t = #I (bool,s,s)
       | #F (bool,s,s,s)
       | #P
       | #M
       | #NE

def op (a:t) (b:t) : t =
  match (a,b)
  case (#NE, _) -> b
  case (_, #NE) -> a
  case (#I(s1,x,b1), #I(s2,y,b2)) ->
    assert (s2 == false) (#I(((s1, x * b2 + y, b1 * b2))))
  case (#I(s1,x,b1), #F(s2,y,b2,f2)) ->
    assert (s2 == false) (#F(((s1, x * b2 + y, b1 * b2, f2))))
  case (#F(s1,x,b1,f1), #I(s2,y,b2)) ->
    assert (s2 == false) (#F(((s1, x + y * f1, b1, f1 / b2))))
  case (#I(s,x,b), #P) -> #F(((s, x, b, 1 / 10)))
  case (#P, #I(s,x,b)) ->
    assert (s == false) (#F(((false, x / b, 1, 1 / b))))
  case (#M, #I(s,x,b)) ->
    assert (s == false) (#I(true,x,b))
  case (#M, #F(s,x,b,f)) ->
    assert (s == false) (#F(true,x,b,f))
  case (#F _, #P) -> assert false (([])[0])
  case (#P, #F _) -> assert false (([])[0])
  case (#F _, #F _) -> assert false (([])[0])
