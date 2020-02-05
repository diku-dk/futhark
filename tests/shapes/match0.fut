-- Looking at the size of an existential match.
-- ==
-- input { 0 } output { 1 }
-- input { 1 } output { 2 }
-- input { 2 } output { 3 }
-- input { 3 } output { 9 }

let main i =
  length (match i
          case 0 -> iota 1
          case 1 -> iota 2
          case 2 -> iota 3
          case _ -> iota 9)
