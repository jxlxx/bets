module Bet exposing (..)

import Browser exposing (element)
import Html exposing (Html, div, text, ul, span)
import List exposing (map)

-- MODEL

type alias Bet =
  { description : String
  , hours : Int
  , status : BetStatus
  , id : Int
  }

type BetStatus   
  = Win
  | Loss
  | InProgress
  | NotStarted
  | Cancelled


-- VIEW

viewBet bet =
    span [] [
        ul [] [ div [] 
                [ text "Description: "
                , text bet.description
                ]
          , div [] 
                [ text "Hours: "
                , text (String.fromInt bet.hours)
                ]
          , div [] 
                [ text "Status: "
                , text (decodeStatus bet.status)
                ]
          ],
        betControls bet
    ]


viewBets : List Bet -> List (Html msg)
viewBets bets =
    map viewBet bets



betControls bet =
    div [] [
        text "delete"
    ]


decodeStatus status =
    case status of 
        Win -> "Win"
        Loss -> "Loss"
        InProgress -> "In Progress"
        NotStarted -> "Not Started"
        Cancelled -> "Cancelled"

