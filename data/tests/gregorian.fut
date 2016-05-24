-- Date computations.  Some complex scalar expressions and a branch.
-- Once messed up the simplifier.

fun int MOD(int x, int y) = x - (x/y)*y

fun int hours_in_dayI   () = 24
fun int minutes_in_dayI () = hours_in_dayI() * 60
fun int minutes_to_noonI() = (hours_in_dayI() / 2) * 60
fun f64 minutes_in_day  () = 24.0*60.0

fun int date_of_gregorian( (int,int,int,int,int) date) =
  let (year, month, day, hour, mins) = date in
  let ym =
      if(month == 1 || month == 2)
      then    ( 1461 * ( year + 4800 - 1 ) ) / 4 +
                ( 367 * ( month + 10 ) ) / 12 -
                ( 3 * ( ( year + 4900 - 1 ) / 100 ) ) / 4
      else    ( 1461 * ( year + 4800 ) ) / 4 +
                ( 367 * ( month - 2 ) ) / 12 -
                ( 3 * ( ( year + 4900 ) / 100 ) ) / 4 in
  let tmp = ym + day - 32075 - 2444238

  in tmp * minutes_in_dayI() + hour * 60 + mins

fun (int,int,int,int,int)
  gregorian_of_date ( int minutes_since_epoch ) =
  let jul = minutes_since_epoch / minutes_in_dayI() in
  let l = jul + 68569 + 2444238 in
  let n = ( 4 * l ) / 146097 in
  let l = l - ( 146097 * n + 3 ) / 4 in
  let i = ( 4000 * ( l + 1 ) ) / 1461001 in
  let l = l - ( 1461 * i ) / 4 + 31 in
  let j = ( 80 * l ) / 2447 in
  let d = l - ( 2447 * j ) / 80 in
  let l = j / 11 in
  let m = j + 2 - ( 12 * l ) in
  let y = 100 * ( n - 49 ) + i + l in

  --let daytime = minutes_since_epoch mod minutes_in_day in
  let daytime = MOD( minutes_since_epoch, minutes_in_dayI() ) in

  if ( daytime == minutes_to_noonI() )

  --then [year = y; month = m; day = d; hour = 12; minute = 0]
  then (y, m, d, 12, 0)

  --else [year = y; month = m; day = d; hour = daytime / 60; minute = daytime mod 60]
  else (y, m, d, daytime / 60, MOD(daytime, 60) )

fun int main(int x) =
  date_of_gregorian(gregorian_of_date(x))
