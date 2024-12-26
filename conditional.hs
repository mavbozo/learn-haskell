{-
Conditional Expressions
-}

max2 a b = if a >= b then a else b

-- we can use guard patterns
-- mav_v3 only works if a not= b and b not= c
max3_v1 a b c
  | a > b && a > c = a
  | b > a && b > c = b
  | c > a && c > b = c

max3_v2 a b c
  | a > b && b > c = a
  | a > c && c > b = a
  | b > a && a > c = b
  | b > c && c > a = b
  | c > a && a > b = c
  | c > b && b > a = c


max3_v3 a b c = if a > b then
                  if a > c then
                    a
                  else
                    c
                else
                  if b > c then
                    b
                else c

-- using intermediary value
-- a not= b and b not= c
max3_v4 a b c =
  let m
        | a > b = a 
        | a < b = b 
  in let x
           | m > c = m
           | m < c = c
     in x

-- using max2

max3_v5 a b c = max2 a (max2 b c)


-- day1900plus function
-- absolute number of days since January 1st 1900 without considering leap year
-- Jan 1st 1900 is day1900plus equals 1
-- d [1 - 31] day m [1-12] month y [0-99] year, --> [1..366]


-- day1900plus function requires function dpm
-- dpm(m) [1..12] --> [1..355] is a number of days passed in that year until the 1st date of month m without considering leap year
-- example: dpm(1) is number of days in that year until Jan 1st which is equal to 1
dpm b
  | b == 1 = 1
  | b == 2 = 32
  | b == 3 = 60 
  | b == 4 = 91
  | b == 5 = 121
  | b == 6 = 152
  | b == 7 = 182
  | b == 8 = 213
  | b == 9 = 244
  | b == 10 = 274
  | b == 11 = 305
  | b == 12 = 335

day1900plus d m y = dpm(m) + d - 1


-- day1900plus_v1 function
-- absolute number of days for that year since January 1st with leap year consideration
-- Jan 1st 1900 is day1900plus equals 1
-- only for year 1900 - 1999
-- d [1 - 31] day m [1-12] month y [0-99] year, --> [1..366]


-- is_leap_year y , y [0 .. 99], true if 1900 + y is leap year
is_leap_year y = ( (y `mod` 4 == 0) && (y `mod` 100 /= 0) ) || (y `mod` 400 == 0)


day1900plus_v1 d m y = dpm m + d - 1 + if ((m > 2) && (is_leap_year y)) then 1 else 0


-- day1900plus_v2 function
-- absolute number of days since January 1st 1900 with leap year consideration
-- Jan 1st 1900 is day1900plus_v2 equals 1
-- Jan 1st 1901 is day1900plus_v2 equals 366
-- only for year 1900 - 1999
-- d [1 - 31] day m [1-12] month y [0-99] year, --> [1..366]

day1900plus_v2 d m 1900 = dpm m + d - 1 + if ((m > 2) && (is_leap_year 1900)) then 1 else 0
day1900plus_v2 d m y = (dpm m + d - 1 + if ((m > 2) && (is_leap_year y)) then 1 else 0)
  + day1900plus_v2 31 12 (y - 1)


-- given tuple <j,m,s> j: hour, m: minute, s: seconds j (0..23), m (0..59), s (0..59)
-- returns number of seconds since 00:00:00

num_of_seconds j m s = (j * 3600) + (m * 60) + s


-- given tuple <j,m,s> j: hour, m: minute, s: seconds j (0..23), m (0..59), s (0..59)
-- returns string of "hour,minute,second am/pm" where hour 1 - 12

ampm_clock j m s =
  let
    ampm = if (0 <= j && j < 12) then "am" else "pm"
    jj = if j == 0 then 12 else j
    mm = m mod 12
  in
  show jj ++ "," ++ show mm ++ "," ++ show s ++ " " ++ ampm


-- celcius to fahrenheit reamur kelvin
-- celcius_to given c (degree of celcius) and code ('F', 'R', 'K')

celcius_to c 'F' = ( c * 9 / 5 ) + 32
celcius_to c 'R' = c * 8 / 10
celcius_to c 'K' = c + 273.15

-- water physical state given temperature in celcius and in 1 atm pressure

water_phy_state c
  | c < 0 = "solid"
  | 0 <= c && c < 100 = "liquid"
  | 100 <= c = "gas"
