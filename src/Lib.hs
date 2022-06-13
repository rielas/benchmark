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
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (isJust)
import Data.Text
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Text.IO
import Data.Text.Read
import Debug.Trace
import GHC.Generics
import GHC.IO
import System.Exit
import Text.Show (Show, show)
import Prelude hiding (Show, getLine, putStrLn, show)

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

process :: Text -> Maybe Snapshot
process record = do
  let (timestamp, end) = breakOn " " record
  timestamp' <- getTimestamp timestamp
  status <- getScanStatus record
  return Snapshot {timestamp = timestamp', status = status}

getTimestamp :: Text -> Maybe Double
getTimestamp withBrakets =
  let stripped = dropAround (== '[') withBrakets
   in case double stripped of
        Left res -> Nothing
        Right (res, _) -> Just res

isScanStatus :: Text -> Bool
isScanStatus = isInfixOf "NexPloit::ScanStatus: "

getScanStatusText :: Text -> Text
getScanStatusText record =
  let (_, status) = breakOnEnd "NexPloit::ScanStatus: " record
   in status

getScanStatus :: Text -> Maybe Status
getScanStatus record =
  let json = getScanStatusText record
   in decode $ toLazyByteString $ encodeUtf8Builder json

mainIo :: IO b
mainIo = do
  line <- getLine
  let res = process line
  putStrLn (pack $ show res)
  mainIo
