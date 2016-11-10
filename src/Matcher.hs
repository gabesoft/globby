-- | Glob matcher
module Matcher where

import qualified Data.Set as S
import Types

mfalse :: Input -> MatcherResult
mfalse xs = MResult xs False

mtrue :: Input -> MatcherResult
mtrue xs = MResult xs True

matchLiteral :: Char -> Matcher
matchLiteral c = M f
  where
    f [] = MResult [] False
    f (x:xs) =
      if x == c
        then mtrue xs
        else mfalse (x : xs)

matchAnyChar :: Matcher
matchAnyChar = M f
  where
    f [] = mfalse []
    f (_:xs) = mtrue xs

matchSetChar :: S.Set Char -> Matcher
matchSetChar s = M f
  where
    f [] = mfalse []
    f (x:xs) =
      if S.member x s
        then mtrue xs
        else mfalse (x : xs)

matchAnyString :: Matcher
matchAnyString = M (\_ -> mtrue [])

typeToMatcher :: MatcherType -> Matcher
typeToMatcher (Literal c) = matchLiteral c
typeToMatcher (Set s) = matchSetChar s
typeToMatcher AnyString = matchAnyString
typeToMatcher AnyChar = matchAnyChar

typesToMatcher :: [MatcherType] -> Matcher
typesToMatcher [] = M (\x -> MResult x (null x))
typesToMatcher xs = (foldr mappend mempty . fmap typeToMatcher) xs
