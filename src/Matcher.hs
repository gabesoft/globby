-- | Glob matcher
module Matcher where

import Data.List (tails)
import qualified Data.Set as S
import Types

mrFalse :: Input -> MatcherResult
mrFalse xs = MResult [xs] False

mrTrue :: Input -> MatcherResult
mrTrue xs = MResult [xs] True

mFalse :: Matcher
mFalse = M mrFalse

mTrue :: Matcher
mTrue = M mrTrue

matchLiteral :: Char -> Matcher
matchLiteral c = M f
  where
    f [] = MResult [] False
    f (x:xs) =
      if x == c
        then mrTrue xs
        else mrFalse (x : xs)

matchAnyChar :: Matcher
matchAnyChar = M f
  where
    f [] = mrFalse []
    f (_:xs) = mrTrue xs

matchSetChar :: S.Set Char -> Matcher
matchSetChar s = M f
  where
    f [] = mrFalse []
    f (x:xs) =
      if S.member x s
        then mrTrue xs
        else mrFalse (x : xs)

matchAnyString :: Matcher
matchAnyString = M (\xs -> MResult (tails xs) True)

typeToMatcher :: MatcherType -> Matcher
typeToMatcher (Literal c) = matchLiteral c
typeToMatcher (Set s) = matchSetChar s
typeToMatcher AnyString = matchAnyString
typeToMatcher AnyChar = matchAnyChar

typesToMatcher :: [MatcherType] -> Matcher
typesToMatcher [] = M (\x -> MResult [x] (null x))
typesToMatcher xs = (foldr mappend mempty . fmap typeToMatcher) xs
