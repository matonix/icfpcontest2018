-- http://d.hatena.ne.jp/keigoi/20101008/1286503107

module Bits
  ( module Data.Bits
  , module Bits
  ) where

import Data.Bits

-- | ビット。 I は 1, O は 0
data B = I | O deriving (Show, Eq)

-- | 8ビット。 左がMSB, 右がLSB
data Byte = B B B B B B B B B deriving (Show, Eq)

bool2b :: Bool -> B
bool2b True = I
bool2b False = O

bit' :: Bits a => Int -> a -> B
bit' i b = bool2b $ testBit b i

-- | view patternで使う関数
bit8 :: Bits a => a -> Byte
bit8 b = B (bit' 7 b) (bit' 6 b) (bit' 5 b) (bit' 4 b) (bit' 3 b) (bit' 2 b) (bit' 1 b) (bit' 0 b)

toBits :: Bits a => a -> [B]
toBits b = map (`bit'` b) [0..7]

fromBits :: Bits a => [B] -> a
fromBits = (`shiftR` 1) . foldr f zeroBits where
  f I b = shiftL b 1 .|. bit 1
  f O b = shiftL b 1
