{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Trace where

import Bits
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put

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
  deriving (Show, Eq)

data Diff = D { dx :: Int, dy :: Int, dz :: Int } deriving (Show, Eq)

getTraces :: BL.ByteString -> [Trace]
getTraces = runGet (many getTrace)

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

-- | write

putTraces :: [Trace] -> BL.ByteString
putTraces = runPut . mconcat . map putTrace

putTrace :: Trace -> Put
putTrace Halt = putWord8 0b11111111
putTrace Wait = putWord8 0b11111110
putTrace Flip = putWord8 0b11111101
putTrace (SMove lld) = let
  (a, i) = dToB 15 lld
  in putWord16be $ shiftL a 12 .|. shiftL 0b100 8 .|. i .&. 0b11111
putTrace (LMove sld1 sld2) = let
  (a1, i1) = dToB 5 sld1
  (a2, i2) = dToB 5 sld2
  in putWord16be $ shiftL a2 14 .|. shiftL a1 12 .|. shiftL 0b1100 8 .|. shiftL (i2 .&. 0b1111) 4 .|. i1 .&. 0b1111
putTrace (FusionP nd) = putWord8 $ shiftL (ndToB nd) 3 .|. 0b111
putTrace (FusionS nd) = putWord8 $ shiftL (ndToB nd) 3 .|. 0b110
putTrace (Fission nd m) = putWord16be $ shiftL (ndToB nd) 11 .|. shiftL 0b101 8 .|. fromInteger (toInteger m)
putTrace (Fill nd) = putWord8 $ shiftL (ndToB nd) 3 .|. 0b011

dToB :: Num a => Int -> Diff -> (a, a)
dToB off (D dx 0 0) = (0b01, fromInteger $ toInteger $ dx + off)
dToB off (D 0 dy 0) = (0b10, fromInteger $ toInteger $ dy + off)
dToB off (D 0 0 dz) = (0b11, fromInteger $ toInteger $ dz + off)
dToB _ _ = undefined

ndToB :: Num a => Diff -> a
ndToB (D dx dy dz) = fromInteger $ toInteger $ (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1) - 1

-- | testing
eqTraces :: FilePath -> IO ()
eqTraces f = do
    i  <- BL.readFile f
    let i1 = getTraces i
    -- let i1 = getTraces (putTraces (getTraces i))
    let i2 = getTraces (putTraces i1)
    mapM_ print $ take 10 $ filter (uncurry (/=)) $ zip i1 i2
    print $ i1 == i2
