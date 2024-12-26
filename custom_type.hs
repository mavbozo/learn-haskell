import Control.Exception (assert)
-- haskell import can only be used at the top of file


-- Custom Type

-- Point (double, double)
-- create Point type
data Point = Point Float Float deriving (Show)

-- define typeclass
class PointLike p where
  absis :: p -> Float
  ordinat :: p -> Float
  is_origin :: p -> Bool

-- implementation of PointLike for Point

instance PointLike Point where
  absis (Point x _)  = x 
  ordinat (Point _ y) = y
  is_origin p = (absis p == 0) && (ordinat p == 0)

{-
p = Point 0 0
is_origin p

p = Point (-1) 1
is_origin p
-}


-- distance between 2 points

square x = x ** 2

distance :: Point -> Point -> Float
distance p1 p2 = sqrt ( (square ((absis p1) - (absis p2))) + (square ((ordinat p1) - (ordinat p2))) )

{-

distance (Point 3 4) (Point 0 0)


p1 = Point 3 4
p2 = Point 0 0

-}

-- distance from point to origin
distanceToZero :: Point -> Float
distanceToZero p = sqrt ( (square ((absis p))) + (square ((ordinat p))) )


-- return quadrant of the Point
quadrant :: Point -> Int
quadrant p
  | (absis p) == 0 && (ordinat p) == 0 = 0
  | (absis p) > 0 && (ordinat p) > 0 = 1
  | (absis p) > 0 && (ordinat p) < 0 = 2
  | (absis p) < 0 && (ordinat p) < 0 = 3
  | (absis p) < 0 && (ordinat p) > 0 = 4


-- FRACTION TYPE

data Fraction = Fraction Int Int deriving (Show)
-- the first part is numerator, the second part is denumerator where denumerator not= 0

-- Smart Constructor
-- https://wiki.haskell.org/Smart_constructors
-- constructor for Fraction instance
makeFraction :: Int -> Int -> Fraction 

-- constructor with runtime check
makeFraction numerator denumerator
  | denumerator /= 0 = Fraction numerator denumerator
  | otherwise = error "denumerator can not be 0"
{-
f1 = makeFraction 1 0
num f1 => error
denum f1 => error

f2 = makeFraction 1 2
num f2
denum f2
-}

-- instead of hardcoding error message we can use assert in makeFraction
makeFractionA :: Int -> Int -> Fraction 
makeFractionA numerator denumerator = assert (denumerator /= 0) $ Fraction numerator denumerator

{-
f3 = makeFractionA 1 0
-}

-- what about using compile time check for constructor?
makeFractionC :: Int -> Int -> Maybe Fraction 

-- constructor with runtime check
makeFractionC numerator denumerator
  | denumerator /= 0 = Just (Fraction numerator denumerator) 
  | otherwise = Nothing
  
{-
f1 = makeFractionC 1 0
num f1 => error
-}

-- define typeclass
class FractionLike f where
  num :: f -> Int -- return numerator
  denum :: f -> Int -- return denumerator

-- implement FractionLike for Fraction
instance FractionLike Fraction where
  num (Fraction n _)  = n
  denum (Fraction _ d) = d



{-
f0 = makeFraction 1 0
f1 = makeFraction 1 2

-}

class YesNo a where
  yesno :: a -> Bool

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno _ = False


main = do
  
  -- let f0 = makeFraction 1 2
  --     x = num f0
  putStrLn "Hello, this is x" 
