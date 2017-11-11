module edge_handling (mapper: {}) = {
  let handle (g: i32): f32 =

    let base (): f32 = r32 g

    in base ()
}

module edge_handling_project_top = edge_handling {}

module edge_handling_project_bottom = edge_handling {}

let main (x: i32) =
  let _unused = edge_handling_project_top.handle 0

  let project_bottom () =
    edge_handling_project_bottom.handle x

  in project_bottom ()
