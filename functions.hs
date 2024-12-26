{-
Functions

Note:
Functions in Haskell must start with lowercase unicode characters or an underscore.
Symbols beginning with an upper case letter are reserved for types and constructors.



-}



-- square 2 numbers
square_two x = x * x
-- call with: square_two 4 returns 16
y = square_two 4

-- square 3 numbers
square_three x = x * x * x
-- call with: square_two 2 returns 8
z = square_three 2

-- square 3 numbers using function square_two

square_three_v1 x = x * square_two x
-- call: square_three_v1 2
result = square_three_v1 2 == square_three 2
-- result = True

-- define maximum of 2 numbers
max2 x y = if x >= y
           then x
           else y

-- define maximum of 3 numbers using max2 function
max3 x y z = max2 x (max2 y z)


-- mean_olympique function
-- given 4 numbers, return the average of two numbers which are not the maximum and minimum of the 4 numbers
-- using max2_v2, min2, max4, min4

-- max2_v2 function
-- return max of 2 numbers
max2_v2 x y = ((x + y) + abs (x - y)) / 2

min2 x y = ((x + y) - abs (x - y)) / 2

max4 a b c d = max2_v2 (max2_v2 a b) (max2_v2 c d)

min4 a b c d = min2 (min2 a b) (min2 c d)

mean_olympique a b c d = (a + b + c + d - max4 a b c d - min4 a b c d) / 2

-- call
mo = mean_olympique 1 2 3 4
mo2 = mean_olympique (-1) (-2) 2 1


-- isPositive
isPositive x = x >= 0

-- isAnA, is a character 'A'
isAnA c = c == 'A'


-- isOrigin(x,y)
-- true if x and y represent x,y == 0,0
isOrigin x y = x == 0 && y == 0


-- isValid(x)
isValid x = x < 5 || x >= 500


-- FX2 , square of x
fx2 x = x * x

-- dif2 , square of difference between x and y
dif2 x y = fx2 (x - y)

-- LeastSquare, distance between 2 points

leastSquare x1 y1 x2 y2 = sqrt ( (dif2 y2 y1) + (dif2 x2 x1)  )


-- using let

foo_one a b =
  let p = a * b
  in (1 + p) * (1 - 2*p)


-- mean_olympique_v1

mean_olympique_v1 a b c d =
  let s = a + b + c + d
  in (s - max4 a b c d - min4 a b c d) / 2

-- mean_olympique_v2
mean_olympique_v2 a b c d =
  let s = a + b + c + d
      max1 = max4 a b c d
      min1 = min4 a b c d
  in (s - max1 - min1) / 2

  

