module Stats (Stats (..), Stats.print, Slice (..), lastInterval) where

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

print :: Stats -> String
print stats =
  let lastTimestamp' = lastTimestamp stats
      begin = printf "Stats for %s:\n" lastTimestamp'

      get field m = field <$> Map.lookup m (slices stats)

      getRequests m = get requests m

      getEntrypoints m = get entrypoints m

      printRequests m = maybe "" (printf "%d minutes: %d\n" m) (getRequests m)

      printEntrypoints m = maybe "" (printf "%d minutes: %d\n" m) (getEntrypoints m)

      requestsStats =
        "requests:\n"
          ++ foldl' (\acc t -> acc ++ printRequests t) "" timeIntervals

      entrypointsStats =
        "\nentrypoints:\n"
          ++ foldl' (\acc t -> acc ++ printEntrypoints t) "" timeIntervals
   in begin ++ requestsStats ++ entrypointsStats
