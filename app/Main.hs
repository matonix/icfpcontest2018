module Main where

import Lib
import qualified Data.ByteString.Lazy as BL

-- main :: IO ()
-- main = do
--   eqTraces "dfltTracesL/LA001.nbt"
--   eqTraces "dfltTracesL/LA010.nbt"
--   eqTraces "dfltTracesL/LA100.nbt"

-- main :: IO ()
-- main = do
--   traces <- getTraces <$> BL.readFile "dfltTracesL/LA001.nbt"
--   print (length traces)
--   -- mapM_ print traces
--   BL.writeFile "teamTracesL/LA001.nbt" $ putTraces traces

main :: IO ()
main = do
  model <- getModel <$> BL.readFile "problemsL/LA001_tgt.mdl"
  print model
  BL.writeFile "teamProblemsL/LA001_tgt.mdl" $ putModel model

-- main :: IO ()
-- main = do
--   eqModel "problemsL/LA001_tgt.mdl"
