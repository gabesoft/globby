module Globber where

import Matcher
import Parser
import Types

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob pat input = isMatch (match mat input)
  where
    ts =
      case parse matchers pat of
        ErrorResult e -> error (show e)
        Result _ x -> x
    mat = typesToMatcher ts
    isMatch (MResult _ True) = True
    isMatch (MResult _ False) = False
