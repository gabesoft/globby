module Main
  ( main
  ) where

import Test.Hspec

import Globber

main :: IO ()
main =
  hspec $
  describe "Testing Globber" $ do
    describe "empty pattern" $ do
      it "matches empty string" $ verifyTrue "" ""
      it "shouldn't match non-empty string" $ verifyFalse "" "string"
      isTrue "abcde" "abcde"
      isTrue "\\a\\b\\c\\d\\e" "abcde"
      isTrue "a]b" "a]b"
      isTrue "-adf]ai1" "-adf]ai1"
      isTrue "\\[a]" "[a]"
      isTrue "\\*\\*\\?" "**?"
      isTrue "\\a\\" "a\\"
      isTrue "ab\\*ba" "ab*ba"
      isTrue "ab\\*ba" "ab*ba"
      isTrue "ab\\[ba" "ab[ba"
      isTrue "ab[a\\]]ba" "ab]ba"
      isTrue "ab[a\\]]ba" "ababa"
      isTrue "[ab[c]" "a"
      isTrue "[ab[c]" "b"
      isTrue "[ab[c]" "c"
      isTrue "[ab[c]" "["
      isFalse "[ab[c]" "x"
      isTrue "[a-z]" "k"
      isFalse "[a-z]" "K"
      isTrue "[a-c-z]" "b"
      isTrue "[a-c-z]" "-"
      isFalse "[a-c-z]" "d"
      isFalse "[z-a]" "d"
      isTrue "/*/abc" "/xyz/abc"
      isFalse "/*/abc" "/xyz/acc"
      isFalse "/*/abc*k" "/xyz/acck"
      isTrue "/*/abc*k" "/xyz/abck"
      isTrue "/*/abc*k" "/xyz/abc123k"

is :: Bool -> [Char] -> [Char] -> SpecWith ()
is expected pat input = it (pat ++ sym ++ input) $ verify expected pat input
  where sym = if expected then " ~ " else " x "

isFalse :: String -> String -> SpecWith ()
isFalse = is False

isTrue :: String -> String -> SpecWith ()
isTrue = is True

verify :: Bool -> GlobPattern -> String -> Expectation
verify b p s = matchGlob p s `shouldBe` b

verifyTrue :: GlobPattern -> String -> Expectation
verifyTrue = verify True

verifyFalse :: GlobPattern -> String -> Expectation
verifyFalse = verify False
