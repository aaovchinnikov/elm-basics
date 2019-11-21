module Main exposing (..)

import Browser
import Url
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MAIN
main : Program () Model Msg
main = Browser.application
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  , onUrlRequest = LinkClicked
  , onUrlChange = UrlChanged
  }

-- MODEL
type alias Model = 
  { key : Nav.Key
  , list : List String
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key [ "element 1", "element 2", "element 3" ], Cmd.none )

-- UPDATE
type Msg 
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AddElement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    AddElement ->
      ( { model | list = List.append model.list ["new element"]}
      , Cmd.none
      )
    
    LinkClicked _ ->
      ( model
      , Cmd.none
      )

    UrlChanged _ ->
      ( model
      , Cmd.none
      )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model = 
  { title = "Basic List application"
  , body = 
    [ ul [] (viewListItems model.list)
    , button [ onClick AddElement ] [ text "Add Element" ] 
    ]
  }

viewListItems : List String -> List (Html msg)
viewListItems list =
  List.map (\s -> li [] [text s]) list