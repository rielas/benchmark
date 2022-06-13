{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
  ( mainIo,
    isScanStatus,
    getScanStatusText,
    getScanStatus,
    Status (..),
  )
where

import Data.Aeson
import Data.ByteString hiding (pack, putStrLn)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (isJust)
import Data.Text (Text, breakOn, breakOnEnd, dropAround, pack)
import Data.Text.Encoding
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.IO (putStrLn)
import Data.Text.Read
import Debug.Trace
import GHC.Generics
import GHC.IO
import System.Exit
import Text.Show (Show, show)
import Prelude hiding (Show, drop, dropWhile, getLine, length, putStrLn, show)

data Snapshot = Snapshot
  { timestamp :: Double,
    status :: Status
  }
  deriving (Show)

data Status = Status
  { requests :: Integer,
    entry_points :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON Status

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

mainIo :: IO b
mainIo = do
  line <- getLine
  let stats = process line
  putStrLn (pack $ show stats)
  mainIo
