module Stats (Stats (..), Stats.print, Slice (..)) where

import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Data.List hiding (lookup)
import Data.Map hiding (foldl')
import Data.Maybe
import Text.Printf
import Prelude hiding (lookup)

data Slice = Slice
  { requests :: Integer,
    entrypoints :: Integer
  }

data Stats = Stats
  { lastTimestamp :: String,
    slices :: Map Integer Slice
  }

timeIntervals = [2, 3, 5, 8, 13, 21, 34, 55] :: [Integer]

print :: Stats -> String
print stats =
  let lastTimestamp' = lastTimestamp stats
      begin = printf "Stats for %s:\n" lastTimestamp'

      get field m = field <$> lookup m (slices stats)

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
