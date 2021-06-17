module Data.RomanNumeral where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.List

-- Use Data.RomanNumeral.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.RomanNumeral.Helpers as Helpers

-- | RomanNumeral type (wrapper) for English numerals
newtype RomanNumeral = RomanNumeral String
                     deriving (Show, Read)

   -- | Pack Integer into RomanNumeral (English numeral string)
pack :: (Integral a, Show a) => a -> RomanNumeral
pack integral = RomanNumeral $ fromMaybe err (integral2RomanNumeral integral)
              where err = error $ Helpers.messageBadIntegral integral

-- | Unpack RomanNumeral (English numeral string) to Integer
unpack :: RomanNumeral -> Integer
unpack (RomanNumeral numeral) = fromMaybe err (romanNumeral2Integral numeral)
                              where err = error $ Helpers.messageBadNumeral numeral


-- | Translate Integral value to Roman Numeral String (if possible)
-- TODO: implement Integral->String translation
integral2RomanNumeral :: (Integral a, Show a) => a -> Maybe String
integral2RomanNumeral x
    | x > 4999 = Nothing -- compiling error when try to use Helpers.minimal
    | x < -4999 = Nothing
    | x < 0 = Just ("-" ++ (fromJust $ integral2RomanNumeral (- x)) )
    | x == 0 = Just Helpers.zero
    | x >= 1000 = Just (gimme 1000 ++ (fromJust $ integral2RomanNumeral (x - 1000)) )
    | x >= 900 = Just (gimme 100 ++ (fromJust $ integral2RomanNumeral (x + 100)) )
    | x >= 500 = Just (gimme 500 ++ (fromJust $ integral2RomanNumeral (x - 500)) )
    | x >= 400 = Just (gimme 100 ++ (fromJust $ integral2RomanNumeral (x + 100)) )
    | x >= 100 = Just (gimme 100 ++ (fromJust $ integral2RomanNumeral (x - 100)) )
    | x >= 90 = Just (gimme 10 ++ (fromJust $ integral2RomanNumeral (x + 10)) )
    | x >= 50 = Just (gimme 50 ++ (fromJust $ integral2RomanNumeral (x - 50)) )
    | x >= 40 = Just (gimme 10 ++ (fromJust $ integral2RomanNumeral (x + 10)) )
    | x >= 10 = Just (gimme 10 ++ (fromJust $ integral2RomanNumeral (x - 10)) )
    | x >= 9 = Just (gimme 1 ++ (fromJust $ integral2RomanNumeral (x + 1)) )
    | x >= 5 = Just (gimme 5 ++ (fromJust $ integral2RomanNumeral (x - 5)) )
    | x >= 4 = Just (gimme 1 ++ (fromJust $ integral2RomanNumeral (x + 1)) )
    | x >= 1 = Just (gimme 1 ++ (fromJust $ integral2RomanNumeral (x - 1)) )
        where
            gimme :: Integral a => a -> String
            gimme x = fromJust tmp
                where
                    tmp = case (Data.List.find (\(y, _) -> y == x) Helpers.intToChar) of
                        Just v  -> Just [snd v]
                        Nothing -> Nothing

-- | Translate Roman Numeral String to Integral value (if possible)
-- TODO: implement String->Integral translation
romanNumeral2Integral :: (Integral a, Show a) => String -> Maybe a
romanNumeral2Integral [] = Just 0
romanNumeral2Integral [x] = case (Data.List.find (\(y, _) -> y == x) Helpers.charToInt) of
                Nothing -> Nothing
                Just v  -> Just $ snd v
romanNumeral2Integral (x:xs) = case (Data.List.find (\(y, _) -> y == x) Helpers.charToInt) of
                Nothing -> if (x == Helpers.negativePrefix) then ret_neg else Nothing
                    where
                        ret_neg :: (Integral a, Show a) => Maybe a
                        ret_neg = case (romanNumeral2Integral xs) of
                            Just v2 -> Just (- v2)
                            Nothing -> Nothing
                Just v -> case (romanNumeral2Integral xs) of
                    Just v2 -> if ((fromMaybe "" (integral2RomanNumeral final)) == ([x]++xs)) then Just final else Nothing
                        where
                            final = sum (snd v) v2 (snd v_compare)
                            sum :: Integral a => a -> a -> a -> a
                            sum x y c
                                | x < c = y - x
                                | otherwise = x + y
                            v_compare = fromJust (Data.List.find (\(y, _) -> y == head xs) Helpers.charToInt)
                    Nothing -> Nothing

-- TODO: implement RomanNumeral instances of Bounde, Num, Ord, Eq, Enum, Real, and Integral
instance Bounded RomanNumeral where
    minBound = pack Helpers.minimal
    maxBound = pack Helpers.maximal

instance Eq RomanNumeral where
    (==) x y = unpack x == unpack y

instance Ord RomanNumeral where
    compare x y= compare (unpack x) (unpack y)

instance Num RomanNumeral where
    (+) x y = pack $ (unpack x) + (unpack y)
    (*) x y = pack $ (unpack x) * (unpack y)
    negate x = pack $ - (unpack x)
    abs n
        | n < pack 0 = - n
        | otherwise = n
    signum n
        | x < 0 = pack (-1)
        | x > 0  = pack 1
        | otherwise = pack 0
        where x = unpack n
    fromInteger x = pack x


instance Enum RomanNumeral where
    toEnum x = pack x
    fromEnum x = fromIntegral (unpack x)

instance Real RomanNumeral where
    toRational x = fromIntegral (unpack x)

instance Integral RomanNumeral where
    quotRem x y = (pack $ fst qr, pack $ snd qr)
        where qr = quotRem (unpack x) (unpack y)
    toInteger x = unpack x
