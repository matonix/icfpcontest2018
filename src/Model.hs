module Model where

import Bits
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Array
import Data.Word
import Data.List.Split

data Model = Model
  { resolution :: Word8
  , coordinates :: Coordinates
  } deriving (Show, Eq)

type Coordinates = Array (Word8, Word8, Word8) B

getModel :: BL.ByteString -> Model
getModel = runGet $ do
  r <- getWord8
  bs <- makeArray r <$> many getWord8
  return $ Model r bs

makeArray :: Word8 -> [Word8] -> Coordinates
makeArray r = listArray ((0,0,0), (r-1,r-1,r-1)) . concatMap toBits

putModel :: Model -> BL.ByteString
putModel (Model r c) = runPut $ do
  putWord8 r
  mconcat . map (putWord8 . fromBits) . chunksOf 8 $ elems c

-- | testing
eqModel :: FilePath -> IO ()
eqModel f = do
    i  <- BL.readFile f
    let i1 = getModel i
    -- let i1 = getTraces (putTraces (getTraces i))
    let i2 = getModel (putModel i1)
    -- mapM_ print $ take 10 $ filter (uncurry (/=)) $ zip i1 i2
    print $ i1 == i2
