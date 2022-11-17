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
       {id=4, description="Make New Bets / Delete Bets", hours=1, status=Bet.Loss}
     , {id=1, description="Update Bet Status", hours=1, status=Bet.Win}
     , {id=2, description="Sort Bets and Display", hours=1, status=Bet.InProgress}
     , {id=3, description="Save to Local Storage", hours=1, status=Bet.NotStarted}
     , {id=0, description="Nice UI (zac efron)", hours=1, status=Bet.NotStarted}
     ] 
     {description="", hours=1}
     5,
    Cmd.none )



-- UPDATE

type Msg 
  = CreateBet 
  | DeleteBet Int
  | UpdateDescription String
  | UpdateHours Int
  | SwitchStatus Int


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
    DeleteBet id -> 
      ( { model
        | bets = List.filter ( \b -> b.id /= id ) model.bets 
        }, Cmd.none )
    
    SwitchStatus id -> 
      ( { model
        | bets = List.map (\b -> if b.id == id  then {b | status = (nextBetStatus b.status)} else b ) model.bets
        }
      , Cmd.none )
  

nextBetStatus status =
  case status of 
    Win -> Loss
    Loss -> Cancelled
    Cancelled -> NotStarted
    NotStarted -> InProgress
    InProgress -> Win
    



-- SUBSCRIPTIONS
subscriptions _ =
  Sub.none

-- VIEW

view model =
  div []
    [ h1 [] [text "All Bets"]
    , div [] (List.map 
             (\status -> let
                           statusText = (Bet.decodeStatus status)
                           bets = (List.filter (\b -> b.status == status) model.bets)
                         in
                           div [] [ h2 [] [text statusText]            
                                  , if List.isEmpty bets then div [] [text "none"] else div [] (viewBets bets)
                           ])
    [Win, Loss, InProgress, NotStarted, Cancelled])
    , createNewBet model.newBet
  ]

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
      , button 
           [onClick (UpdateHours (currentHours - 1))] 
           [text "-"] 
      , text (String.fromInt currentHours)
      , button 
           [onClick (UpdateHours (currentHours + 1))] 
           [text "+"] 
      ]


viewBets bets = 
  List.map viewBet bets

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
          , div [onClick (SwitchStatus bet.id)]
              [ text "Status: "
              , text (Bet.decodeStatus bet.status)
              ]
          ],
      betControls bet
  ]
  
betControls bet =
  div [][ 
     button [onClick (DeleteBet bet.id)]
            [text "delete" ]
    ]
  
