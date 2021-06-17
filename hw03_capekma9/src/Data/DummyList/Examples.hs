module Data.DummyList.Examples where

-- !!! DO NOT CHANGE THIS MODULE !!!

example1 = repeat . (*5) $ 5

example2 = [ round (sin x) | x <- [0,(pi/2)..] , x >= 20 ]

example3 = ['A'..'Z']
