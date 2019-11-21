module Main exposing (..)

import Array
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
type alias Record = 
  { name : String
  , intField : Int
  , stringField : String
  }

type alias Model = 
  { key : Nav.Key
  , array : Array.Array Record
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key 
      ( Array.fromList 
        [ Record "element 1" 1 "String 1"
        , Record "element 2" 2 "String 2"
        , Record "element 3" 3 "String 3"
        ]
      )
  , Cmd.none )

-- UPDATE
type Msg 
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AddNewElement
  | SelectElement
  | RemoveSelectedElement

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    AddNewElement ->
      addElement model

    SelectElement ->
      addElement model

    RemoveSelectedElement ->
      ( model
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

-- helper for update function
addElement : Model -> ( Model, Cmd Msg )
addElement model = 
  ( { model | array = 
      Array.push 
        ( Record 
          "new element"
          ( Array.length model.array + 1 ) 
          "New Element"
        ) 
        model.array 
    }
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
    [ viewListPanel model
    , viewSelectedItemPanel ( Record "record" 20 "details of record" )
    ]
  }

viewListPanel : Model -> Html Msg
viewListPanel model = 
  div []
    [ button [ onClick AddNewElement ] [ text "Add" ]
    , button [ onClick RemoveSelectedElement ] [ text "Remove" ]
    , ul [] ( viewListItems model.array )
    ]

viewListItems : Array.Array Record -> List (Html Msg)
viewListItems array =
  Array.toList
    ( Array.map ( \record -> li [ onClick SelectElement ] [text record.name] ) 
      array 
    )

viewSelectedItemPanel : Record -> Html Msg
viewSelectedItemPanel record = 
  div []
    [ table []
        [ caption [] [ text "Selected item details" ]
        , tbody [] 
            [ tr [] 
                [ td [] [ text "name:" ]
                , td [] [ text record.name ]
                ]
            , tr []
                [ td [] [ text "intField:" ]
                , td [] [ text ( String.fromInt record.intField ) ]
                ]
            , tr [] 
                [ td [] [ text "stringField:" ]
                , td [] [ text record.stringField ]
                ]
            ]    
        ]
    , button [] [ text "Edit" ]
    ]