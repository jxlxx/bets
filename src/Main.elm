module Main exposing (..)


import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Url


import Bet exposing (..)

-- MAIN
main : Program () Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { bets : List Bet
  , newBet: { description : String
            , hours : Int
      }
  , nextId: Int
  }

init flags  = 
  ( Model [
       {id=0, description="Make New Bets / Delete Bets", hours=1, status=Bet.InProgress}
     , {id=1, description="Update Bet Status", hours=1, status=Bet.NotStarted}
     , {id=2, description="Sort Bets and Display", hours=1, status=Bet.NotStarted}
     , {id=3, description="Save to Local Storage", hours=1, status=Bet.NotStarted}
     , {id=4, description="Nice UI (zac efron)", hours=1, status=Bet.NotStarted}
     ] 
     {description="", hours=1}
     5,
    Cmd.none )



-- UPDATE

type Msg 
  = CreateBet 
  --| DeleteBet Int
  | UpdateDescription String
  | UpdateHours Int



update msg model =
  case msg of
    CreateBet ->
      ( { newBet = { description = ""
                   , hours = 1
                   }
        , bets = model.bets ++ [{ description = model.newBet.description
                               , hours = model.newBet.hours 
                               , status = Bet.NotStarted
                               , id = model.nextId
                               }]
        , nextId = model.nextId + 1      
        }
      , Cmd.none
      )
    UpdateDescription desc ->
      ( { model
        | newBet = { description = desc
                   , hours = model.newBet.hours 
                   } 
        }
      , Cmd.none
      )
    UpdateHours hours ->
      ( { model 
        | newBet = { description = model.newBet.description
                   , hours = hours
                   }
        }
      , Cmd.none
      )
  

-- SUBSCRIPTIONS
subscriptions _ =
  Sub.none

-- VIEW

view model =
  div [] [ text "Current Bets:"
    , div [] (Bet.viewBets model.bets)
    , createNewBet model.newBet
    ]
  

viewLink path = 
  li [] [ a [ href path ] [ text path ] ]

createNewBet newBet =
  div [] 
  [ text "Description: " 
  , input [ type_ "text"
          , placeholder "Some kind of goal..."
          , value newBet.description
          , onInput UpdateDescription] []
  , setHours newBet.hours
  , button [onClick CreateBet] [text "Make Bet"]
  ]


setHours currentHours = 
  div [] 
  [ text "time estimate: "
  , button [onClick (UpdateHours (currentHours - 1))] 
           [text "-"] 
  , text (String.fromInt currentHours)
  , button [onClick (UpdateHours (currentHours + 1))] 
           [text "+"] 
  ]


