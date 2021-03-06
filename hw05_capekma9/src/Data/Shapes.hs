module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle { ciRadius :: Double }
               deriving (Show, Read, Eq)

data Triangle = EquilateralTriangle { etSide :: Double }
              | IsoscelesTriangle { itBase :: Double, itLeg :: Double }
              | ScaleneTriangle { stSideA :: Double, stSideB :: Double, stSideC :: Double }
              deriving (Show, Read, Eq)

data Quadrilateral = Square { sqSide :: Double}
                   | Rectangle { reSideA :: Double, reSideB :: Double }
                   deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle where
  valid (Circle r) = r > 0 

instance Validable Triangle where
  valid (EquilateralTriangle a) = a > 0
  valid (IsoscelesTriangle a b) = a + b > b && b + b > a && a > 0 && b > 0
  valid (ScaleneTriangle a b c) = a + b > c && a + c > b && b + c > a && a > 0 && b > 0 && c > 0

instance Validable Quadrilateral where
  valid (Square a) = a > 0
  valid (Rectangle a b) = a > 0 && b > 0

-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area

class Validable a => Shape2D a where
  area          :: a -> Double
  circumference :: a -> Double

instance Shape2D Circle where
  area c@(Circle radius) = case (valid c) of
    True  -> pi * radius^2
    _     -> 0
  
  circumference c@(Circle radius) = case (valid c) of
    True  -> 2 * pi * radius
    _     -> 0

instance Shape2D Triangle where
  area (EquilateralTriangle side)   = area (ScaleneTriangle side side side)
  area (IsoscelesTriangle base leg) = area (ScaleneTriangle base leg leg)
  area t@(ScaleneTriangle a b c)    = case (valid t) of
    True  -> sqrt $ s * (s - a) * (s - b) * (s - c)
              where s = (circumference t) / 2
    _     -> 0

  circumference (EquilateralTriangle side) = circumference (ScaleneTriangle side side side)
  circumference (IsoscelesTriangle base leg) = circumference (ScaleneTriangle base leg leg)
  circumference t@(ScaleneTriangle a b c) = case (valid t) of
    True  -> a + b + c
    _     -> 0

instance Shape2D Quadrilateral where
  area (Square side)   = area (Rectangle side side)
  area q@(Rectangle a b) = case (valid q) of
    True  -> a * b
    _     -> 0

  circumference (Square side)   = circumference (Rectangle side side)
  circumference r@(Rectangle a b) = case (valid r) of
    True  -> 2 * (a + b)
    _     -> 0
