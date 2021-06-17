module Data.MyString.Examples where

-- !!! DO NOT CHANGE THIS MODULE !!!

import Text.Printf

example1 = "Example 1"

example2 = "Example " ++ show 2

example3 :: String
example3 = printf "Example %d" (3 :: Int)
