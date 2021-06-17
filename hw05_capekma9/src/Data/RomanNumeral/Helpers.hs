module Data.RomanNumeral.Helpers where

import Data.Tuple (swap)

-- these are to help with RomanNumerals, feel free to edit it if needed

-- see: http://www.csgnetwork.com/csgromancnv.html

maximal = 4999
minimal = -4999

zero = "" -- zero is very special value
negativePrefix = '-'
messageBadIntegral integral = "Cannot convert to Roman Numeral: '" ++ show integral ++ "'"
messageBadNumeral numeral = "Illegal Roman Numeral: '" ++ numeral ++ "'"

intToChar :: Integral a => [(a, Char)]
intToChar = [ ( 1, 'I')
            , ( 5, 'V')
            , ( 10, 'X')
            , ( 50, 'L')
            , ( 100, 'C')
            , ( 500, 'D')
            , ( 1000, 'M')
            ]

charToInt :: Integral a => [(Char, a)]
charToInt = map swap intToChar
