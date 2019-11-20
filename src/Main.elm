module Main exposing (..)

import Browser
import Url
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)


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

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model = 
  { title = "Basic List application"
  , body = 
    [ text "body text"      
    ]
  }