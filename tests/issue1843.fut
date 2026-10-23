def N_coating_coefs = i64.i32 8
def N_coating_specs = i64.i32 26

type coating =
    #AntiReflective
  | #Mirror
  | #Absorbing
  | #PhaseGradient_IdealFocus {lam: f32, f: f32}
  | #PhaseGradient_RotSym_Dispersive {lam_d: f32, phi0: [N_coating_coefs]f32, GD: [N_coating_coefs]f32, GDD: [N_coating_coefs]f32}

def parse_coatings (coating_list_array: [][N_coating_specs]f32) : []coating =
  let parse (specs: [N_coating_specs]f32): coating =
    let enum = i64.f32 specs[0]
    in match enum
       case 0 ->
         -- AntiReflective
         #AntiReflective
       case _ ->
         #AntiReflective
  in map parse coating_list_array

entry RayTrace (coating_list_array: [][N_coating_specs]f32) : []coating =
  let coatings = parse_coatings coating_list_array
  in coatings
