module RomanNumeralSpec (spec) where

import Control.Exception
import Test.Hspec

import Data.RomanNumeral


spec :: Spec
spec = do
    describe "pack" $ do
      it "translates basic numbers" $ do
        pack 0 `shouldBe` RomanNumeral ""
        pack 1 `shouldBe` RomanNumeral "I"
        pack 5 `shouldBe` RomanNumeral "V"
        pack 10 `shouldBe` RomanNumeral "X"
        pack 50 `shouldBe` RomanNumeral "L"
        pack 100 `shouldBe` RomanNumeral "C"
        pack 500 `shouldBe` RomanNumeral "D"
        pack 1000 `shouldBe` RomanNumeral "M"
      it "translates non-trivial numbers" $ do
        pack 3 `shouldBe` RomanNumeral "III"
        pack 4 `shouldBe` RomanNumeral "IV"
        pack 17 `shouldBe` RomanNumeral "XVII"
        pack 629 `shouldBe` RomanNumeral "DCXXIX"
        pack 1578 `shouldBe` RomanNumeral "MDLXXVIII"
        pack 2780 `shouldBe` RomanNumeral "MMDCCLXXX"
      it "translates negative numbers" $ do
        pack (-10) `shouldBe` RomanNumeral "-X"
        pack (-256) `shouldBe` RomanNumeral "-CCLVI"
        pack (-1001) `shouldBe` RomanNumeral "-MI"
        pack (-4236) `shouldBe` RomanNumeral "-MMMMCCXXXVI"
        pack (-4999) `shouldBe` RomanNumeral "-MMMMCMXCIX"
      it "cannot translate large numbers" $ do
        evaluate (pack 5172) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '5172'"
        evaluate (pack 21011) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '21011'"
        evaluate (pack 721011) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '721011'"
        evaluate (pack (-6246)) `shouldThrow` errorCall "Cannot convert to Roman Numeral: '-6246'"
    describe "unpack" $ do
      it "translates basic numbers" $ do
        unpack (RomanNumeral "") `shouldBe` 0
        unpack (RomanNumeral "I") `shouldBe` 1
        unpack (RomanNumeral "V") `shouldBe` 5
        unpack (RomanNumeral "X") `shouldBe` 10
        unpack (RomanNumeral "L") `shouldBe` 50
        unpack (RomanNumeral "C") `shouldBe` 100
        unpack (RomanNumeral "D") `shouldBe` 500
        unpack (RomanNumeral "M") `shouldBe` 1000
      it "translates non-trivial numbers" $ do
        unpack (RomanNumeral "XXIV") `shouldBe` 24
        unpack (RomanNumeral "XLVII") `shouldBe` 47
        unpack (RomanNumeral "CIX") `shouldBe` 109
        unpack (RomanNumeral "CLXXVIII") `shouldBe` 178
        unpack (RomanNumeral "DCCLXXXII") `shouldBe` 782
        unpack (RomanNumeral "MMCDLXX") `shouldBe` 2470
      it "translates negative numbers" $ do
        unpack (RomanNumeral "-LXXXVIII") `shouldBe` (-88)
        unpack (RomanNumeral "-CCXV") `shouldBe` (-215)
        unpack (RomanNumeral "-MXLVII") `shouldBe` (-1047)
        unpack (RomanNumeral "-MMMCDLI") `shouldBe` (-3451)
        unpack (RomanNumeral "-MMMMDCCCXLIX") `shouldBe` (-4849)
      it "cannot translate large numbers" $ do
        evaluate (unpack (RomanNumeral "-MMMMMMLVII")) `shouldThrow` errorCall "Illegal Roman Numeral: '-MMMMMMLVII'"
        evaluate (unpack (RomanNumeral "MMMMMMMMMMMMMM")) `shouldThrow` errorCall "Illegal Roman Numeral: 'MMMMMMMMMMMMMM'"
        evaluate (unpack (RomanNumeral "MMMMMMCDX")) `shouldThrow` errorCall "Illegal Roman Numeral: 'MMMMMMCDX'"
      it "cannot translate weird strings" $ do
        evaluate (unpack (RomanNumeral "OMG")) `shouldThrow` errorCall "Illegal Roman Numeral: 'OMG'"
        evaluate (unpack (RomanNumeral "XML")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XML'"
        evaluate (unpack (RomanNumeral "xxx")) `shouldThrow` errorCall "Illegal Roman Numeral: 'xxx'"
      it "cannot translate bad numerals" $ do
        evaluate (unpack (RomanNumeral "XXXXXXXXX")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XXXXXXXXX'"
        evaluate (unpack (RomanNumeral "XM")) `shouldThrow` errorCall "Illegal Roman Numeral: 'XM'"
        evaluate (unpack (RomanNumeral "IDC")) `shouldThrow` errorCall "Illegal Roman Numeral: 'IDC'"

    describe "RomanNumeral instances" $ do
      it "is Ord: compares numbers in strings" $ do
        (RomanNumeral "CXX" > RomanNumeral "CII") `shouldBe` True
        (RomanNumeral "MCCLIV" > RomanNumeral "MMMMDXXI") `shouldBe` False
        (RomanNumeral "XLV" <= RomanNumeral "-I") `shouldBe` False
      it "is Num: adds numbers in strings" $ do
        (RomanNumeral "V" + RomanNumeral "VI") `shouldBe` RomanNumeral "XI"
        (RomanNumeral "CXXIV" + RomanNumeral "LXXV") `shouldBe` RomanNumeral "CXCIX"
        (RomanNumeral "MCCXL" + RomanNumeral "-MM") `shouldBe` RomanNumeral "-DCCLX"
      it "is Num: subtract numbers in strings" $ do
        (RomanNumeral "MCCXL" - RomanNumeral "MM") `shouldBe` RomanNumeral "-DCCLX"
        (RomanNumeral "" - RomanNumeral "-VII") `shouldBe` RomanNumeral "VII"
        (RomanNumeral "CCCLVIII" - RomanNumeral "CLXXV") `shouldBe` RomanNumeral "CLXXXIII"
      it "is Num: multiplies numbers in strings" $ do
        (RomanNumeral "V" * RomanNumeral "VII") `shouldBe` RomanNumeral "XXXV"
        (RomanNumeral "CXXIV" * RomanNumeral "XVII") `shouldBe` RomanNumeral "MMCVIII"
        (RomanNumeral "-X" * RomanNumeral "VII") `shouldBe` RomanNumeral "-LXX"
      it "is Num: has defined absolute value" $ do
        abs (RomanNumeral "VI") `shouldBe` RomanNumeral "VI"
        abs (RomanNumeral "") `shouldBe` RomanNumeral ""
        abs (RomanNumeral "-MMMCCLIV") `shouldBe` RomanNumeral "MMMCCLIV"
        abs (RomanNumeral "-XV") `shouldBe` RomanNumeral "XV"
      it "is Num: has defined signum" $ do
        signum (RomanNumeral "MMDCLX") `shouldBe` RomanNumeral "I"
        signum (RomanNumeral "") `shouldBe` RomanNumeral ""
        signum (RomanNumeral "-XV") `shouldBe` RomanNumeral "-I"
      it "is Num: has defined negate" $ do
        negate (RomanNumeral "") `shouldBe` RomanNumeral ""
        negate (RomanNumeral "CLIV") `shouldBe` RomanNumeral "-CLIV"
        negate (RomanNumeral "-XV") `shouldBe` RomanNumeral "XV"
      it "is Num: has defined fromInteger (from numeric literal)" $ do
        (1258 :: RomanNumeral) `shouldBe` RomanNumeral "MCCLVIII"
        ((-1) :: RomanNumeral) `shouldBe` RomanNumeral "-I"
        (0 :: RomanNumeral) `shouldBe` RomanNumeral ""
      it "is Integral: can compute quotient and remainder" $ do
        (RomanNumeral "V" `quotRem` RomanNumeral "II") `shouldBe` (RomanNumeral "II", RomanNumeral "I")
        (RomanNumeral "-IV" `quotRem` RomanNumeral "III") `shouldBe` (RomanNumeral "-I", RomanNumeral "-I")
        (RomanNumeral "X" `quotRem` RomanNumeral "-III") `shouldBe` (RomanNumeral "-III", RomanNumeral "I")
      it "is Integral: has defined toInteger" $ do
        toInteger (RomanNumeral "CCLVI") `shouldBe` 256
        toInteger (RomanNumeral "") `shouldBe` 0
        toInteger (RomanNumeral "-DCCLXXXV") `shouldBe` (-785)
      it "is Integral: automatically gets `div` and `mod`" $ do
        (RomanNumeral "XVII" `div` RomanNumeral "III") `shouldBe` RomanNumeral "V"
        (RomanNumeral "V" `div` RomanNumeral "II") `shouldBe` RomanNumeral "II"
        (RomanNumeral "XVII" `mod` RomanNumeral "III") `shouldBe` RomanNumeral "II"
        (RomanNumeral "VI" `mod` RomanNumeral "II") `shouldBe` RomanNumeral ""
      it "is Enum: can use succ, pred, dot-dot syntactic sugar" $ do
        succ (RomanNumeral "X") `shouldBe` RomanNumeral "XI"
        pred (RomanNumeral "CCL") `shouldBe` RomanNumeral "CCXLIX"
        [(RomanNumeral "I")..(RomanNumeral "V")] `shouldBe` [RomanNumeral "I",RomanNumeral "II",RomanNumeral "III",RomanNumeral "IV",RomanNumeral "V"]
        [(RomanNumeral "V"),(RomanNumeral "X")..(RomanNumeral "XXX")] `shouldBe` [RomanNumeral "V",RomanNumeral "X",RomanNumeral "XV",RomanNumeral "XX",RomanNumeral "XXV",RomanNumeral "XXX"]
      it "is Bounded: has maxBound and minBound" $ do
        (minBound :: RomanNumeral) `shouldBe` RomanNumeral "-MMMMCMXCIX"
        (maxBound :: RomanNumeral) `shouldBe` RomanNumeral "MMMMCMXCIX"
        unpack (minBound :: RomanNumeral) `shouldBe` (-4999)
        unpack (maxBound :: RomanNumeral) `shouldBe` 4999
