{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Trace where

import Data.Bits
import Control.Applicative
import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

data Trace
  = Halt
  | Wait
  | Flip
  | SMove { lld :: Diff }
  | LMove { sld1 :: Diff, sld2 :: Diff }
  | FusionP { nd :: Diff }
  | FusionS { nd :: Diff }
  | Fission { nd :: Diff, m :: Int }
  | Fill { nd :: Diff }
  deriving Show

data Diff = D { dx :: Int, dy :: Int, dz :: Int } deriving Show

-- | ビット。 I は 1, O は 0
data B = I | O deriving Show

-- | 8ビット。 左がMSB, 右がLSB
data Byte = B B B B B B B B B deriving Show

readTraces :: IO ()
readTraces = do
    let f = "dfltTracesL/LA100.nbt"
    rh <- openFile f ReadMode
    i  <- BL.hGetContents rh
    let d = runGet getTraces i
    -- mapM_ print d
    print (length d)

getTraces :: Get [Trace]
getTraces = many getTrace

getTrace :: Get Trace
getTrace = do
  w <- getWord8
  case bit8 w of
    B I I I I I I I I -> return Halt
    B I I I I I I I O -> return Wait
    B I I I I I I O I -> return Flip
    B O O llda1 llda0 O I O O -> do
      lldi <- fromIntegral . (.&. 0b00011111) <$> getWord8
      return $ SMove $ mkld llda1 llda0 (lldi - 15)
    B sld2a1 sld2a0 sld1a1 sld1a0 I I O O -> do
      sldi <- getWord8
      let sld1i = fromIntegral $ sldi .&. 0b00001111
      let sld2i = fromIntegral $ sldi `shiftR` 4
      let sld1 = mkld sld1a1 sld1a0 sld1i
      let sld2 = mkld sld2a1 sld2a0 sld2i
      return $ LMove sld1 sld2
    B nd4 nd3 nd2 nd1 nd0 I I I -> return $ FusionP (mknd nd4 nd3 nd2 nd1 nd0)
    B nd4 nd3 nd2 nd1 nd0 I I O -> return $ FusionS (mknd nd4 nd3 nd2 nd1 nd0)
    B nd4 nd3 nd2 nd1 nd0 I O I -> do
      m <- fromIntegral <$> getWord8
      return $ Fission (mknd nd4 nd3 nd2 nd1 nd0) m
    B nd4 nd3 nd2 nd1 nd0 O I I -> return $ Fill (mknd nd4 nd3 nd2 nd1 nd0)
    _ -> undefined

mkld :: B -> B -> Int -> Diff
mkld O I i = D i 0 0
mkld I O i = D 0 i 0
mkld I I i = D 0 0 i
mkld O O _ = undefined

mknd :: B -> B -> B -> B -> B -> Diff
mknd nd4 nd3 nd2 nd1 nd0 = tToD . reverse . map pred . iToT $ bToI [nd4, nd3, nd2, nd1, nd0]
  where
    bToI = foldl toInt 0
    toInt i I = 1 * 2 + i
    toInt i O = i
    iToT 0 = []
    iToT x = x `mod` 3 : iToT (x `div` 3)
    tToD [] = D 0 0 0
    tToD [dx] = D dx 0 0
    tToD [dx, dy] = D dx dy 0
    tToD [dx, dy, dz] = D dx dy dz
    tToD _ = undefined


-- http://d.hatena.ne.jp/keigoi/20101008/1286503107

bool2b :: Bool -> B
bool2b True = I
bool2b False = O

bit' :: Bits a => Int -> a -> B
bit' i b = bool2b $ testBit b i

-- | view patternで使う関数
bit8 :: Bits a => a -> Byte
bit8 b = B (bit' 7 b) (bit' 6 b) (bit' 5 b) (bit' 4 b) (bit' 3 b) (bit' 2 b) (bit' 1 b) (bit' 0 b)
