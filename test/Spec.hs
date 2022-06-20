{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Time.Clock
import Lib
import qualified Stats
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

example = "[98101.056964] NexPloit::ScanStatus: {\"status\":\"running\",\"discovering\":true,\"requests\":3759,\"elapsed\":\"00:01:38.099290711\",\"entry_points\":365,\"total_params\":16,\"component_status\":{\"Insecure TLS Configurations\":{\"done\":0,\"total\":1}},\"request_statuses\":{\"200\":3265,\"301\":104,\"404\":15,\"401\":1,\"201\":5,\"500\":7,\"302\":2},\"tech_stack\":[{\"host\":\"brokencrystals.local\",\"tech\":[]},{\"host\":\"brokencrystals.local\",\"tech\":[]}],\"traffic_metrics\":{\"brokencrystals.local\":{\"sent\":607079,\"received\":64807173},\"www.robotstxt.org\":{\"sent\":42,\"received\":8964}},\"scan_uuid\":\"d5b571fe-4f3b-4482-878d-d55e451b2a5e\"}"

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "Parse scan status" $ do
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
          @?= Just (secondsToDiffTime 98 + picosecondsToDiffTime 99290711000),
      testCase "Work with empty scan statistics" $ do
        let stats =
              Stats.Stats
                { Stats.lastTimestamp = "00:01:38.099290711",
                  Stats.slices = Map.empty
                }
        Stats.print stats @?= "Stats for 00:01:38.099290711:\nrequests:\n",
      testCase "Work with scan statistics" $ do
        let stats =
              Stats.Stats
                { Stats.lastTimestamp = "00:05:05.0",
                  Stats.slices =
                    Map.fromList
                      [ ( 2,
                          Stats.Slice
                            { Stats.requests = 67,
                              Stats.entrypoints = 1
                            }
                        ),
                        ( 3,
                          Stats.Slice
                            { Stats.requests = 77,
                              Stats.entrypoints = 2
                            }
                        ),
                        ( 5,
                          Stats.Slice
                            { Stats.requests = 87,
                              Stats.entrypoints = 3
                            }
                        )
                      ]
                }
        Stats.print stats @?= "Stats for 00:05:05.0:\nrequests:\n2 minutes: 67\n3 minutes: 77\n5 minutes: 87\n"
    ]
