let max_months = 5
let max_days = 4
let max_hours = 3
let max_minutes = 2

let between x a b =
  x >= a && x <= b

let wellformed {year; month; day; hour; minute} =
  year >= 1 &&
  between month 1 5 &&
  between day 1 4 &&
  between hour 0 2 &&
  between minute 0 1
    
let moddiv (a, b) = (a mod b, a / b)
    
let mod_inc x carry max =
  let x' = x + carry in
  moddiv (x', max)

let next0 {year; month; day; hour; minute} =
  let (next_minute, carry) = mod_inc minute 1 max_minutes in
  let (next_hour, carry) = mod_inc hour carry max_hours in
  let (next_day, carry) = mod_inc day carry max_days in
  let (next_month, carry) = mod_inc month carry max_months in
  let next_year = year + carry in
  {year=next_year; month=next_month; day=next_day; hour=next_hour; minute=next_minute}
  
let next {year; month; day; hour; minute} =
  let {year; month; day; hour; minute} =
    next0 {year=year - 1; month=month - 1; day=day - 1; hour; minute} in
  {year=year + 1; month=month + 1; day=day + 1; hour; minute}
  
let minutes_in_year = max_minutes * max_hours * max_days * max_months
let minutes_in_month = max_minutes * max_hours * max_days
let minutes_in_day = max_minutes * max_hours
let minutes_in_hour = max_minutes

let of_int minutes =
  let (minutes, year) = moddiv (minutes, minutes_in_year) in
  let (minutes, month) = moddiv (minutes, minutes_in_month) in
  let (minutes, day) = moddiv (minutes, minutes_in_day) in
  let (minute, hour) = moddiv (minutes, minutes_in_hour) in 
  {year=year + 1; month=month + 1; day=day + 1; hour; minute}
