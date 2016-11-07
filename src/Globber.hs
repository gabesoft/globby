module Globber where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob _ _ = False

pingpong :: Bool -> Int -> IO ()
pingpong v n = do
  mvc <- newEmptyMVar -- MVar read by child
  mvp <- newEmptyMVar -- MVar read by parent
  let parent n
        | n > 0 = do
          when v $ putStr $ " " ++ show n
          putMVar mvc n
          takeMVar mvp >>= parent
        | otherwise = return ()
      child = do
        n <- takeMVar mvc
        putMVar mvp (n - 1)
        child
  tid <- forkIO child
  parent n `finally` killThread tid
  when v $ putStrLn ""
