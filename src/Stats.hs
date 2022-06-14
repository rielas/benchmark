module Stats (Stats (..), Stats.print) where

import Data.Map
import Text.Printf
import Prelude

data Slice = Slice
  {
    requests :: Integer,
    entrypoints :: Integer
  }

data Stats = Stats
  { lastTimestamp :: String,
    slices :: Map Integer Slice
  }

print :: Stats -> String
print stats =
  let lastTimestamp' = lastTimestamp stats
   in printf "Stats for %s:\n" lastTimestamp'
