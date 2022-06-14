{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( mainIo,
    isScanStatus,
    getScanStatusText,
    getScanStatus,
    Status (..),
    getElapsedTime,
  )
where

import Data.Aeson
import Data.ByteString hiding (pack, putStr, putStrLn)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (isJust)
import Data.Text (Text, breakOn, breakOnEnd, dropAround, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read
import Data.Time
import Data.Time.Clock
import Debug.Trace
import GHC.Generics
import GHC.IO
import qualified Stats
import System.Exit
import Text.Printf
import Text.Show (Show, show)
import Prelude hiding (Show, drop, dropWhile, getLine, length, show)

data Snapshot = Snapshot
  { timestamp :: Double,
    status :: Status
  }
  deriving (Show)

data Status = Status
  { requests :: Integer,
    entry_points :: Integer,
    elapsed :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON Status

slices = [2, 3, 5, 8, 13, 21, 34, 55]

process :: ByteString -> Maybe Snapshot
process record = do
  let (timestamp, end) = breakSubstring " " record
  status <- getScanStatus record
  timestamp' <- getTimestamp timestamp
  return Snapshot {timestamp = timestamp', status = status}

getTimestamp :: ByteString -> Maybe Double
getTimestamp withBrakets =
  let text = decodeUtf8 withBrakets
      stripped = dropAround (== '[') text
   in case double stripped of
        Left res -> Nothing
        Right (res, _) -> Just res

isScanStatus :: ByteString -> Bool
isScanStatus = isInfixOf "NexPloit::ScanStatus: "

getScanStatusText :: ByteString -> ByteString
getScanStatusText record =
  let tag = "NexPloit::ScanStatus: "
      (_, status) = breakSubstring tag record
   in drop (length tag) status

getScanStatus :: ByteString -> Maybe Status
getScanStatus record =
  let json = getScanStatusText record
   in decodeStrict json

getElapsedTime :: Status -> Maybe DiffTime
getElapsedTime status =
  parseTimeM True defaultTimeLocale "%H:%M:%S" (elapsed status)

printStats :: Snapshot -> String
printStats snapshot =
  let elapsed' = elapsed . status $ snapshot
      requests' = requests . status $ snapshot
      entry_points' = entry_points . status $ snapshot
   in printf
        "%s requests: %6d,  entry points: %5d"
        elapsed'
        requests'
        entry_points'

mainIo :: IO b
mainIo = do
  line <- getLine
  let stats = process line
  case stats of
    Nothing -> pure ()
    Just stats -> putStrLn $ printStats stats
  mainIo
