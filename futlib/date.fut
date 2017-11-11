-- | A simple date library.  Inspired by code from LexiFi.
--
-- This library does not handle any ancient calendars or anything like
-- that.  It's designed for simplicity (and thereby speed).

-- The implementation is by a module which is immediately opened.  The
-- intent is to make the type of dates abstract.  When Futhark gets
-- separate interface files (or some other such mechanism), this
-- should go away.

import "/futlib/math"

module type date = {
  -- A date.
  type date

  -- Add days to date.
  val add_days: date -> i32 -> date
  -- Subtract days from date.
  val sub_days: date -> i32 -> date

  -- Add months to date.  If necessary, will truncate the day-number
  -- to the end of the new month.
  val add_months: date -> i32 -> date

  -- The time from the first date to the second, in fractions of a
  -- 365-day year.
  val diff_dates: date -> date -> f64

  -- Convert a date to a triple of (year,month,day).  Months and days
  -- are 1-indexed.  The time is assumed to be 12:00 that day.
  val triple_of_date: date -> (i32,i32,i32)

  -- The inverse of 'triple_of_date'.  The result is undefined if the
  -- triple does not describe a valid date.
  val date_of_triple: (i32,i32,i32) -> date

  -- True if the given triple encodes a valid (year,month,day)-date.
  val check_date: (i32,i32,i32) -> bool

  val same_date: date -> date -> bool

  -- The earliest of the two given dates.
  val earliest: date -> date -> date

  -- The latest of the two given dates.
  val latest: date -> date -> date
}

open ({
  type date = i32

  type gregorian = { year: i32,
                     month: i32,
                     day: i32,
                     hour: i32,
                     minute: i32 }

  let hours_in_day = 24
  let minutes_in_day = hours_in_day * 60
  let fminutes_in_day = r64 minutes_in_day
  let minutes_to_noon = (hours_in_day / 2) * 60

  -- Communications of the ACM by Henry F. Fliegel and Thomas C. Van Flandern,
  -- ``A Machine Algorithm for Processing Calendar Dates'',
  -- CACM, volume 11, number 10, October 1968, p. 657
  let date_of_gregorian ({year = y, month = m, day = d, hour = hr, minute = mn}: gregorian) =
    ((if m == 1 || m == 2 then
       (1461 * (y + 4800 - 1)) / 4 +
        (367 * (m + 10)) / 12 -
        (3 * ((y + 4900 - 1) / 100)) / 4
      else
       (1461 * (y + 4800)) / 4 +
         (367 * (m - 2)) / 12 -
         (3 * ((y + 4900) / 100)) / 4) + d - 32075 - 2444238) * minutes_in_day
    + hr * 60 + mn

  let gregorian_of_date (minutes_since_epoch: i32) =
    let jul = minutes_since_epoch / minutes_in_day
    let l = jul + 68569 + 2444238
    let n = (4 * l) / 146097
    let l = l - (146097 * n + 3) / 4
    let i = (4000 * (l + 1)) / 1461001
    let l = l - (1461 * i) / 4 + 31
    let j = (80 * l) / 2447
    let d = l - (2447 * j) / 80
    let l = j / 11
    let m = j + 2 - (12 * l)
    let y = 100 * (n - 49) + i + l
    let daytime = minutes_since_epoch % minutes_in_day
    in if daytime == minutes_to_noon
       then {year = y, month = m, day = d, hour = 12, minute = 0}
       else {year = y, month = m, day = d, hour = daytime / 60, minute = daytime % 60}

  let leap (year: i32) =
    year % 4 == 0 && ((year % 400 == 0) || (year % 100 != 0))

  let end_of_month(year: i32) (month: i32): i32 =
    if month == 2 && leap(year) then 29
    else if  month == 2 then 28
    else if ( month == 4 || month == 6 || month == 9 || month == 11 ) then 30
    else 31

  let check_date ((year,month,day): (i32,i32,i32)) =
    1 <= day &&
    1 <= month && month <= 12 &&
    day <= end_of_month year month

  let date_of_triple (year: i32, month: i32, day: i32) =
    date_of_gregorian {year=year, month=month, day=day, hour=12, minute=0}

  let triple_of_date (x: date) =
    let {year, month, day, hour = _, minute = _} = gregorian_of_date x
    in (year, month, day)

  let int_of_date (x: date) = x
  let date_of_int (x: i32) = x

  let fminutes_in_365 = r64 (minutes_in_day * 365)
  let inv_fminutes_in_365 = 1.0 / fminutes_in_365
  let inv_fminutes_in_day = 1.0 / fminutes_in_day

  let add_act_365 (t: date) (dt: f64) =
    date_of_int (t64 (r64 (int_of_date t) + fminutes_in_365 * dt))
  let add_days (t1: date) (displ: i32) =
    date_of_int(int_of_date t1 + displ * minutes_in_day)
  let sub_days (t1: date) (displ: i32) =
    add_days t1 (-displ)

  let add_months (date: date) (nbmonths: i32) =
    let {year=y, month=m, day=d, hour=h, minute=min} = gregorian_of_date date
    let m = m + nbmonths
    let (y, m) = (y + (m-1) / 12, (m-1) % 12 + 1)
    let (y, m) = if m <= 0 then (y - 1, m + 12) else (y, m)
    in date_of_gregorian {year=y,
                          month=m,
                          day=i32.min d (end_of_month y m),
                          hour=h,
                          minute=min}

  let days_between (t1: date) (t2: date) =
    (r64 (int_of_date t2 - int_of_date t1)) * inv_fminutes_in_day

  let diff_dates (t1: date) (t2: date) =
    days_between t1 t2 / 365.0

  let same_date (x: date) (y: date) = x == y

  let latest (x: date) (y: date) = i32.max x y
  let earliest (x: date) (y: date) = i32.min x y

} : date)
