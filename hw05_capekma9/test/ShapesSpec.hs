module ShapesSpec (spec) where

import Test.Hspec

import Data.Shapes

approxMatch a b = abs (a - b) < 0.000001

spec :: Spec
spec = do
    describe "Circle" $ do
      it "has defined validity check" $ do
        valid (Circle 10) `shouldBe` True
        valid (Circle 0.01) `shouldBe` True
        valid (Circle 0) `shouldBe` False
        valid (Circle (-2)) `shouldBe` False
      it "has defined area" $ do
        area (Circle 10) `shouldBe` 100 * pi
        area (Circle  1) `shouldBe`       pi
        area (Circle  7) `shouldBe`  49 * pi
        area (Circle  0) `shouldBe`   0
      it "has defined circumference" $ do
        circumference (Circle 10) `shouldBe` 20 * pi
        circumference (Circle  1) `shouldBe`  2 * pi
        circumference (Circle  7) `shouldBe` 14 * pi
        circumference (Circle  0) `shouldBe`  0

    describe "Triangle" $ do
      describe "EquilateralTriangle" $ do
        it "has defined validity check" $ do
          valid (EquilateralTriangle 10) `shouldBe` True
          valid (EquilateralTriangle 0.01) `shouldBe` True
          valid (EquilateralTriangle 0) `shouldBe` False
          valid (EquilateralTriangle (-2)) `shouldBe` False
        it "has defined area" $ do
          approxMatch (area (EquilateralTriangle 10)) 43.30127018922 `shouldBe` True
          approxMatch (area (EquilateralTriangle  2))  1.73205080757 `shouldBe` True
          approxMatch (area (EquilateralTriangle  7)) 21.21762239272 `shouldBe` True
          area (EquilateralTriangle 0) `shouldBe` 0
        it "has defined circumference" $ do
          circumference (EquilateralTriangle 10) `shouldBe` 30
          circumference (EquilateralTriangle  1) `shouldBe`  3
          circumference (EquilateralTriangle  7) `shouldBe` 21
          circumference (EquilateralTriangle  0) `shouldBe`  0
      describe "IsoscelesTriangle" $ do
        it "has defined validity check" $ do
          valid (IsoscelesTriangle 10 15) `shouldBe` True
          valid (IsoscelesTriangle 25 10) `shouldBe` False
          valid (IsoscelesTriangle 0.01 2) `shouldBe` True
          valid (IsoscelesTriangle 0 3) `shouldBe` False
          valid (IsoscelesTriangle 5 (-2)) `shouldBe` False
        it "has defined area" $ do
          approxMatch (area (IsoscelesTriangle 10 12)) 54.5435605731 `shouldBe` True
          approxMatch (area (IsoscelesTriangle  1  3))  1.4790199458 `shouldBe` True
          approxMatch (area (IsoscelesTriangle  7 15)) 51.0508325104 `shouldBe` True
          area (IsoscelesTriangle 0 5) `shouldBe` 0
          area (IsoscelesTriangle 10 0) `shouldBe` 0
        it "has defined circumference" $ do
          circumference (IsoscelesTriangle 10 12) `shouldBe` 34
          circumference (IsoscelesTriangle  1  3) `shouldBe`  7
          circumference (IsoscelesTriangle  7 15) `shouldBe` 37
          circumference (IsoscelesTriangle  0  5) `shouldBe`  0
          circumference (IsoscelesTriangle 10  0) `shouldBe`  0
      describe "ScaleneTriangle" $ do
        it "has defined validity check" $ do
          valid (ScaleneTriangle 10 15 7) `shouldBe` True
          valid (ScaleneTriangle 10 25 5) `shouldBe` False
          valid (ScaleneTriangle 21 16 5) `shouldBe` False
          valid (ScaleneTriangle 2 16 22) `shouldBe` False
          valid (ScaleneTriangle 0.5 2 2.3) `shouldBe` True
          valid (ScaleneTriangle 0 3 2) `shouldBe` False
          valid (ScaleneTriangle 5 3 0) `shouldBe` False
          valid (ScaleneTriangle 5 (-2) 1) `shouldBe` False
        it "has defined area" $ do
          approxMatch (area (ScaleneTriangle 10 12 7)) 34.9776714491 `shouldBe` True
          approxMatch (area (ScaleneTriangle  5  7 4))  9.7979589711 `shouldBe` True
          approxMatch (area (ScaleneTriangle  5  4 3))  6.0          `shouldBe` True
          area (ScaleneTriangle  0  0 2) `shouldBe` 0
          area (ScaleneTriangle  3 12 4) `shouldBe` 0
          area (ScaleneTriangle 10 15 4) `shouldBe` 0
        it "has defined circumference" $ do
          circumference (ScaleneTriangle 10 12 7) `shouldBe` 29
          circumference (ScaleneTriangle 5 7 4) `shouldBe` 16
          circumference (ScaleneTriangle 5 4 3) `shouldBe` 12
          circumference (ScaleneTriangle 0 0 2) `shouldBe` 0
          circumference (ScaleneTriangle 3 12 4) `shouldBe` 0
          circumference (ScaleneTriangle 10 15 4) `shouldBe` 0

    describe "Quadrilateral" $ do
      describe "Square" $ do
        it "has defined validity check" $ do
          valid (Square 10) `shouldBe` True
          valid (Square 0.01) `shouldBe` True
          valid (Square 0) `shouldBe` False
          valid (Square (-2)) `shouldBe` False
        it "has defined area" $ do
          area (Square 10) `shouldBe` 100
          area (Square 1) `shouldBe` 1
          area (Square 7) `shouldBe` 49
          area (Square 0) `shouldBe` 0
        it "has defined circumference" $ do
          circumference (Square 10) `shouldBe` 40
          circumference (Square 1) `shouldBe` 4
          circumference (Square 7) `shouldBe` 28
          circumference (Square 0) `shouldBe` 0
      describe "Rectangle" $ do
        it "has defined validity check" $ do
          valid (Rectangle 10 5) `shouldBe` True
          valid (Rectangle 0.01 0.15) `shouldBe` True
          valid (Rectangle 0 5) `shouldBe` False
          valid (Rectangle 5 (-2)) `shouldBe` False
        it "has defined area" $ do
          area (Rectangle 10 5) `shouldBe` 50
          area (Rectangle 1 7) `shouldBe` 7
          area (Rectangle 7 5) `shouldBe` 35
          area (Rectangle 0 2) `shouldBe` 0
          area (Rectangle 2 0) `shouldBe` 0
        it "has defined circumference" $ do
          circumference (Rectangle 10 5) `shouldBe` 30
          circumference (Rectangle 1 7) `shouldBe` 16
          circumference (Rectangle 7 5) `shouldBe` 24
          circumference (Rectangle 0 2) `shouldBe` 0
          circumference (Rectangle 2 0) `shouldBe` 0
