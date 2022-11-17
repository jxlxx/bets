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



decodeStatus status =
    case status of 
        Win -> "Win"
        Loss -> "Loss"
        InProgress -> "In Progress"
        NotStarted -> "Not Started"
        Cancelled -> "Cancelled"

