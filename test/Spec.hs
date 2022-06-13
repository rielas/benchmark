{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Clock
import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

example = "[98101.056964] NexPloit::ScanStatus: {\"status\":\"running\",\"discovering\":true,\"requests\":3759,\"elapsed\":\"00:01:38.099290711\",\"entry_points\":365,\"total_params\":16,\"component_status\":{\"Insecure TLS Configurations\":{\"done\":0,\"total\":1}},\"request_statuses\":{\"200\":3265,\"301\":104,\"404\":15,\"401\":1,\"201\":5,\"500\":7,\"302\":2},\"tech_stack\":[{\"host\":\"brokencrystals.local\",\"tech\":[]},{\"host\":\"brokencrystals.local\",\"tech\":[]}],\"traffic_metrics\":{\"brokencrystals.local\":{\"sent\":607079,\"received\":64807173},\"www.robotstxt.org\":{\"sent\":42,\"received\":8964}},\"scan_uuid\":\"d5b571fe-4f3b-4482-878d-d55e451b2a5e\"}"

main = defaultMain $
  testCase "Parse scan status" $ do
    assertBool "is status message" $ isScanStatus example
    getScanStatusText example @?= "{\"status\":\"running\",\"discovering\":true,\"requests\":3759,\"elapsed\":\"00:01:38.099290711\",\"entry_points\":365,\"total_params\":16,\"component_status\":{\"Insecure TLS Configurations\":{\"done\":0,\"total\":1}},\"request_statuses\":{\"200\":3265,\"301\":104,\"404\":15,\"401\":1,\"201\":5,\"500\":7,\"302\":2},\"tech_stack\":[{\"host\":\"brokencrystals.local\",\"tech\":[]},{\"host\":\"brokencrystals.local\",\"tech\":[]}],\"traffic_metrics\":{\"brokencrystals.local\":{\"sent\":607079,\"received\":64807173},\"www.robotstxt.org\":{\"sent\":42,\"received\":8964}},\"scan_uuid\":\"d5b571fe-4f3b-4482-878d-d55e451b2a5e\"}"
    let status = getScanStatus example
    status
      @?= Just
        ( Status
            { requests = 3759,
              entry_points = 365,
              elapsed = "00:01:38.099290711"
            }
        )
    (status >>= getElapsedTime)
      @?= Just (secondsToDiffTime 98 + picosecondsToDiffTime 99290711000)
