-- Multiple levels of ascription.
-- ==
-- input {} output { 4 2 }

module outer: { val x: i32 module inner: { val y: i32 } } = {
  module inner: { val y: i32} = {
    let y = 2
  }
  let x = inner.y + 2
}

let main = (outer.x, outer.inner.y)
