{-# LANGUAGE Strict #-}

-- | This module implements sanity checks for
--     a schedule, i.e., does the schedule conforms
--     with the target code ? 
module Futhark.Optimise.Schedules.Safety
  ( checkValidSched
  )
where

import Futhark.IR.SOACS
--import Futhark.Optimise.Schedules.EnvUtils
import Futhark.Optimise.Schedules.SchedUtils

-- | Checks the validity of the schedule. If the schedule is
--   wrong or ambiguous, then compilation terminates with an error.
--   Arguments:
--     1. the schedule
--     2. the expression target to rescheduling, typically a map
--   Results:
--     1. a boolean denoting whether the schedule is ready to fire
--     2. in case the schedule dictates the manifestation of result,
--        the associative and commutatuive binary-operator of the
--        accumulator and its neutral element, otherwise Nothing.
--  ToDo:
--    1. check that the target code does not contain in-place updates or scatterns
--    2. check whether the code uses accumulators:
--         - in the case when maps are padded then support them by encompassing
--           accumulations inside an if statement, i.e., @if i < M then accumulate@,
--           where @M@ refers to the original map size and @i@ the corresponding
--           index from @iota M@
--         - in schedules with redundant computation, probably we do not want to
--           support accumulators, hence signal error.
--    3. check safety for stripmining redomaps with Macc: all such strips
--         should be on top, because we do not change the operator.
checkValidSched :: HLSched -> Exp SOACS -> (Bool, Maybe (Lambda SOACS, [SubExp]))
checkValidSched _sched _e =
  (True, Nothing)

