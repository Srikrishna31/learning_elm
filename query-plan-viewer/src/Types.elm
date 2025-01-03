module Types exposing (..)

import Auth


type alias AppState =
    { auth : Auth.Model
    , currPlanText : String
    , isMenuOpen : Bool
    , lastError : String
    , serverUrl : String
    }


initAppState : Auth.Flags -> AppState
initAppState sessionId =
    { auth = Auth.init sessionId
    , currPlanText =
        """
            {
              "Plan": {
                "Node Type": "Hash Join",
                "Parallel Aware": false,
                "Join Type": "Inner",
                "Startup Cost": 1.04,
                "Total Cost": 2.11,
                "Plan Rows": 2,
                "Plan Width": 176,
                "Actual Startup Time": 0.061,
                "Actual Total Time": 0.066,
                "Actual Rows": 8,
                "Actual Loops": 1,
                "Hash Cond": "(zones.project_id = projects.project_id)",
                "Plans": [
                    {
                    "Node Type": "Seq Scan",
                    "Parent Relationship": "Outer",
                    "Parallel Aware": false,
                    "Relation Name": "zones",
                    "Alias": "zones",
                    "Startup Cost": 0.00,
                    "Total Cost": 1.03,
                    "Plan Rows": 3,
                    "Plan Width": 112,
                    "Actual Startup Time": 0.013,
                    "Actual Total Time": 0.015,
                    "Actual Rows": 4,
                    "Actual Loops": 1
                    },
                    {
                    "Node Type": "Hash",
                    "Parent Relationship": "Inner",
                    "Parallel Aware": false,
                    "Startup Cost": 1.02,
                    "Total Cost": 1.02,
                    "Plan Rows": 2,
                    "Plan Width": 72,
                    "Actual Startup Time": 0.013,
                    "Actual Total Time": 0.013,
                    "Actual Rows": 4,
                    "Actual Loops": 1,
                    "Hash Buckets": 1024,
                    "Original Hash Buckets": 1024,
                    "Hash Batches": 1,
                    "Original Hash Batches": 1,
                    "Peak Memory Usage": 9,
                    "Plans": [
                        {
                        "Node Type": "Seq Scan",
                        "Parent Relationship": "Outer",
                        "Parallel Aware": false,
                        "Relation Name": "projects",
                        "Alias": "projects",
                        "Startup Cost": 0.00,
                        "Total Cost": 1.02,
                        "Plan Rows": 2,
                        "Plan Width": 72,
                        "Actual Startup Time": 0.004,
                        "Actual Total Time": 0.005,
                        "Actual Rows": 4,
                        "Actual Loops": 1
                        }
                    ]
                    }
                ]
              },
              "Planning Time": 0.174,
              "Triggers": [
              ],
              "Execution Time": 0.169
            }

            """
    , isMenuOpen = False
    , lastError = ""
    , serverUrl = ""
    }
