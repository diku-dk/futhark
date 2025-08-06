def lv_step (growth_prey: f32)
            (predation: f32)
            (growth_pred: f32)
            (decline_pred: f32)
            (prey: f32)
            (pred: f32) : (f32, f32) =
  let dprey = (growth_prey - predation * pred) * prey
  let dpred = (growth_pred * prey - decline_pred) * pred
  in (dprey, dpred)

def euler_method (step_size: f32)
                 (num_steps: i64)
                 (init_prey: f32)
                 (init_pred: f32)
                 (growth_prey: f32)
                 (predation: f32)
                 (growth_pred: f32)
                 (decline_pred: f32) : [](f32, f32) =
  let states = replicate (num_steps + 1) (init_prey, init_pred)
  let (_, states) =
    loop (curr_state, states) = ((init_prey, init_pred), states)
    for i < num_steps do
      let (curr_prey, curr_pred) = curr_state
      let (dprey, dpred) = lv_step growth_prey predation growth_pred decline_pred curr_prey curr_pred
      let next_state = (curr_prey + step_size * dprey, curr_pred + step_size * dpred)
      let states[i + 1] = next_state
      in (next_state, states)
  in states

def runge_kutta (step_size: f32)
                (num_steps: i64)
                ( init_prey: f32
                , init_pred: f32
                , growth_prey: f32
                , predation: f32
                , growth_pred: f32
                , decline_pred: f32
                ) : [](f32, f32) =
  let fn = lv_step growth_prey predation growth_pred decline_pred
  let states = replicate (num_steps) (init_prey, init_pred)
  let (_, states) =
    loop (curr_state, states) = ((init_prey, init_pred), states)
    for i < num_steps do
      let (curr_prey, curr_pred) = curr_state
      let (k1_prey, k1_pred) = fn curr_prey curr_pred
      let (k2_prey, k2_pred) = fn (curr_prey + step_size / 2 * k1_prey) (curr_pred + step_size / 2 * k1_pred)
      let (k3_prey, k3_pred) = fn (curr_prey + step_size / 2 * k2_prey) (curr_pred + step_size / 2 * k2_pred)
      let (k4_prey, k4_pred) = fn (curr_prey + step_size * k3_prey) (curr_pred + step_size * k3_pred)
      let next_state = (curr_prey + step_size / 6 * (k1_prey + 2 * k2_prey + 2 * k3_prey + k4_prey), curr_pred + step_size / 6 * (k1_pred + 2 * k2_pred + 2 * k3_pred + k4_pred))
      let states[i] = next_state
      in (next_state, states)
  in states

def to_array ((v1: f32), (v2: f32)) : [2]f32 =
  [v1, v2]

entry main (step_size: f32)
           (num_steps: i64)
           (init_prey: f32)
           (init_pred: f32)
           (growth_prey: f32)
           (predation: f32)
           (growth_pred: f32)
           (decline_pred: f32) : [][2]f32 =
  map to_array (runge_kutta step_size num_steps (init_prey, init_pred, growth_prey, predation, growth_pred, decline_pred))

entry runge_kutta_fwd (step_size: f32)
                      (num_steps: i64)
                      (init_prey: f32)
                      (init_pred: f32)
                      (growth_prey: f32)
                      (predation: f32)
                      (growth_pred: f32)
                      (decline_pred: f32)
                      (init_prey_tan: f32)
                      (init_pred_tan: f32)
                      (growth_prey_tan: f32)
                      (predation_tan: f32)
                      (growth_pred_tan: f32)
                      (decline_pred_tan: f32) : [][2]f32 =
  map to_array (jvp (runge_kutta step_size num_steps)
                    ( init_prey
                    , init_pred
                    , growth_prey
                    , predation
                    , growth_pred
                    , decline_pred
                    )
                    ( init_prey_tan
                    , init_pred_tan
                    , growth_prey_tan
                    , predation_tan
                    , growth_pred_tan
                    , decline_pred_tan
                    ))
