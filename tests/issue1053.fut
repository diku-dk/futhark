-- ==
-- error: trail_map

type model_params =
  { pct_pop: -- environment parameters
             f32
  , decay: --diffusion_ker_size: i32,
           f32
  , sensor_angle: -- agent parameters
                  f32
  , sensor_offset: f32
  , rot_angle: -- sensor_width: i32,
               f32
  , step_size: f32
  , deposit_amount: f32
  }

-- angle_jitter: f32

type agent =
  { loc: (f32, f32)
  , ang: f32
  }

type env [grid_h] [grid_w] [n_agents] =
  { model_params: model_params
  , trail_map: [grid_h][grid_w]f32
  , agent_list: [n_agents]agent
  }

def bounded (max: f32)
            (x: f32) : f32 =
  if x >= 0 && x < max
  then x
  else (x + max) f32.% max

def loc2grid (grid_size: i64)
             (real_loc: f32) : i64 =
  let gs_f = f32.i64 grid_size
  in if real_loc >= 0 && real_loc < gs_f
     then i64.f32 real_loc
     else i64.f32 (bounded gs_f real_loc)

def read_sensor [xn] [yn]
                (p: model_params)
                (trail_map: [yn][xn]f32)
                (x: f32, y: f32)
                (ang: f32) : f32 =
  let sx = f32.cos ang * p.sensor_offset + x |> loc2grid xn
  let sy = f32.sin ang * p.sensor_offset + y |> loc2grid yn
  in trail_map[sy, sx]

def move_step (p: model_params)
              ({ loc = (x: f32, y: f32)
               , ang = ang: f32
               }: agent
              ) : agent =
  let x_ = x + p.step_size * f32.cos ang
  let y_ = y + p.step_size * f32.sin ang
  in {loc = (x_, y_), ang}

def step_agent (p: model_params)
               (trail_map: [][]f32)
               ({loc, ang}: agent) : (agent, (i64, i64)) =
  let sl = read_sensor p trail_map loc (ang + p.sensor_angle)
  let sf = read_sensor p trail_map loc ang
  let sr = read_sensor p trail_map loc (ang - p.sensor_angle)
  let stepped =
    if sf >= sr && sf >= sl
    then move_step p {loc, ang}
    else (if sr >= sl
          then move_step p {loc, ang = ang - p.rot_angle}
          else move_step p {loc, ang = ang + p.rot_angle})
  in (stepped, (i64.f32 loc.0, i64.f32 loc.1))

def step_agents [h] [w] [a]
                ({model_params, trail_map, agent_list}: env [h] [w] [a]) : env [h] [w] [a] =
  let (stepped, deposits) = unzip (map (step_agent model_params trail_map) agent_list)
  let flat_deposits = map (\(x, y) -> y * w + x) deposits
  let deposited = reduce_by_index (flatten trail_map) (+) 0 flat_deposits (replicate a model_params.deposit_amount)
  in {model_params, trail_map = unflatten deposited, agent_list = stepped}

def disperse_cell [h] [w]
                  (p: model_params)
                  (trail_map: [h][w]f32)
                  (x: i64)
                  (y: i64) : f32 =
  let neighbors =
    map (\(dx, dy) ->
           trail_map[(y + dy + h) i32.% h
           ,(x + dx + w) i32.% w])
        [ (-1, 1)
        , (0, 1)
        , (1, 1)
        , (-1, 0)
        , (1, 0)
        , (-1, -1)
        , (0, -1)
        , (1, -1)
        ]
  let sum = trail_map[x, y] + reduce (+) 0 neighbors
  in p.decay * sum / 9

def disperse_trail [h] [w] [a]
                   ({model_params, trail_map, agent_list}: env [h] [w] [a]) : env [h] [w] [a] =
  { model_params
  , agent_list
  , trail_map = tabulate_2d h w (disperse_cell model_params trail_map)
  }

def simulation_step [h] [w] [a]
                    (e: env [h] [w] [a]) : env [h] [w] [a] =
  e |> step_agents |> disperse_trail

def to_deg (rad: f32) : i32 = 180 * rad / f32.pi |> f32.round |> i64.f32
def to_rad (deg: i64) : f32 = f32.i64 deg * f32.pi / 180

def build_test_env [h] [w] [a]
                   (trail_map: [h][w]f32)
                   (agent_xs: [a]f32)
                   (agent_ys: [a]f32)
                   (agent_angs: [a]i64) : env [h] [w] [a] =
  let model_params =
    { pct_pop = 0
    , decay = 0.5
    , sensor_angle = to_rad 45
    , sensor_offset = 2
    , rot_angle = to_rad 45
    , step_size = 1
    , deposit_amount = 9
    }
  let agent_list = map3 (\x y ang -> {loc = (x, y), ang = to_rad ang}) agent_xs agent_ys agent_angs
  in {model_params, agent_list, trail_map}

entry test_single_step_trail [h] [w]
                             (trail_map: [h][w]f32)
                             (x: f32)
                             (y: f32)
                             (ang: i64) : [h][w]f32 =
  let e = simulation_step (build_test_env trail_map [x] [y] [ang])
  in e.trail_map
