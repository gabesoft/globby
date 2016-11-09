module Globber where

import Control.Concurrent
import Control.Exception
import Control.Monad

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = False
