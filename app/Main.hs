module Main where

import Lib

main :: IO ()
main = do
  d <- readTraces "dfltTracesL/LA100.nbt"
  print (length d)
  -- mapM_ print d
