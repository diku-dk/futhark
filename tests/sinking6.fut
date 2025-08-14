-- At one point this did an incorrect sinking due to not looking
-- properly at WithAccs.

def pointToRoller (model: [2]f32) (p: f32) : f32 =
  let radius = model[0]
  let alpha = model[1]
  let p' = map (p *) [f32.sin alpha, 0.0]
  in (p'[0] - radius) ** 2

entry pointsToRollerGrad [n] (model: [2]f32) (pcd: [n]f32) : [2]f32 =
  vjp (\m -> map (pointToRoller m) pcd |> f32.sum) model 1
