type LandCounts = [5]i8

type ColorRequirement = {masks: LandCounts, count: i8}

type ProbTable = [16][9][4][18][18]f32

type ColorRequirements = #one_requirement i8ColorRequirement
|#two_requirements i8ColorRequirementColorRequirement

def sum_with_mask (counts: LandCounts) (masks: LandCounts) = reduce (+) 0 (map2 (&) counts masks)

def get_casting_probability_1 (lands: LandCounts) (cmc: i8) (requirement: ColorRequirement) (prob_to_cast: ProbTable) =
  prob_to_cast[cmc, requirement.count, 0, sum_with_mask lands requirement.masks, 0]

def get_casting_probability_2 (lands: LandCounts) (cmc: i8) (req1: ColorRequirement) (req2: ColorRequirement) (prob_to_cast: ProbTable) =
  prob_to_cast[cmc, req1.count, req2.count, sum_with_mask lands req1.masks, sum_with_mask lands req2.masks]

def get_casting_probability (lands: LandCounts) (requirements: ColorRequirements) (prob_to_cast: ProbTable) =
  match requirements
    case #one_requirement cmc req1 -> get_casting_probability_1 lands cmc req1 prob_to_cast
    case #two_requirements cmc req1 req2 -> get_casting_probability_2 lands cmc req1 req2 prob_to_cast

entry get_score [picked_count] (lands: LandCounts) (prob_to_cast: ProbTable) (picked_color_requirements: [picked_count]ColorRequirements) =
  map (\req -> get_casting_probability lands req prob_to_cast) picked_color_requirements