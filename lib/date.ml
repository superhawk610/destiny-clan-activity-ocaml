open Printf

type date = {
  year : int;
  month : int;
  day : int;
}

let string_of_month = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | month -> "Unrecognized Month " ^ string_of_int month

let now () =
  let local = Unix.localtime (Unix.time ()) in

  {
    year=(local.tm_year + 1900);
    month=(local.tm_mon + 1);
    day=local.tm_mday
  }

let start_of_month d =
  { year=d.year; month=d.month; day=1 }

let from_iso str =
  let trimmed = Str.first_chars str 10 in
  let year = int_of_string (Str.first_chars trimmed 4) in
  let month = int_of_string (String.sub trimmed 5 2) in
  let day = int_of_string (Str.last_chars trimmed 2) in

  { year=year; month=month; day=day }

let to_iso d =
  string_of_int d.year
  ^ "-" ^ sprintf "%02d" d.month
  ^ "-" ^ sprintf "%02d" d.day

let to_string d =
  string_of_month d.month
  ^ " " ^ string_of_int d.day
  ^ ", " ^ string_of_int d.year

let compare_dates d1 d2 =
  if (d1.year > d2.year) then 1
  else if (d1.year < d2.year) then -1
  else if (d1.month > d2.month) then 1
  else if (d1.month < d2.month) then -1
  else if (d1.day > d2.day) then 1
  else if (d1.day < d2.day) then -1
  else 0

let is_before d1 d2 =
  compare_dates d1 d2 < 0

let is_after d1 d2 =
  compare_dates d1 d2 > 0

let is_same d1 d2 =
  compare_dates d1 d2 = 0