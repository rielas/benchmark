module Stats
  ( Stats (..),
    Stats.print,
    Slice (..),
    lastInterval,
    empty,
    printHeader,
  )
where

import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Data.List hiding (head, lookup, reverse)
import Data.List.NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import Text.Printf
import Prelude hiding (head, lookup, reverse)

data Slice = Slice
  { requests :: Integer,
    entrypoints :: Integer
  }

data Stats = Stats
  { lastTimestamp :: String,
    slices :: Map.Map Integer Slice
  }

timeIntervals = 2 :| [3, 5, 8, 13, 21, 34, 55]

lastInterval :: DiffTime -> Integer
lastInterval time =
  let minutes = floor time `div` 60
      first = head timeIntervals :: Integer
   in fromMaybe first $ find (> minutes) timeIntervals

printHeader :: String
printHeader =
  "timestamp,"
    ++ foldl' (\acc t -> acc ++ printf " req %2d min," t) "" timeIntervals
    ++ foldl' (\acc t -> acc ++ printf " end %2d min," t) "" timeIntervals

print :: Stats -> String
print stats =
  let lastTimestamp' = lastTimestamp stats
      begin = printf "\n%s, " lastTimestamp'

      get field m = field <$> Map.lookup m (slices stats)

      getRequests m = get requests m

      getEntrypoints m = get entrypoints m

      printRequests m = maybe "      ," (printf "%6d,") (getRequests m)

      printEntrypoints m = maybe "      ," (printf "%5d,") (getEntrypoints m)

      requestsStats = foldl' (\acc t -> acc ++ printRequests t) "" timeIntervals

      entrypointsStats =
        foldl'
          (\acc t -> acc ++ printEntrypoints t)
          ""
          timeIntervals
   in begin ++ requestsStats ++ entrypointsStats

empty :: Stats
empty = Stats {lastTimestamp = "", slices = Map.empty}
