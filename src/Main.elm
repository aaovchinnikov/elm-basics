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

type State
  = Regular
  | Editing

type alias Model = 
  { key : Nav.Key
  , array : Array.Array Record
  , selectedIndex : Maybe Int
  , state : State
  }

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  ( Model key 
      ( Array.fromList 
        [ Record "element 1" 1 "String 1"
        , Record "element 2" 2 "String 2"
        , Record "element 3" 3 "String 3"
        ]
      )
      Nothing
      Regular
  , Cmd.none )

-- UPDATE
type Msg 
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | AddNewElement
  | SelectElement Int
  | RemoveSelectedElement
  | SwitchToEditing
  | SwitchToRegular
  | SaveAndSwitchToRegular

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    AddNewElement ->
      addElement model

    SelectElement index ->
      ( { model | selectedIndex = Just index }
      , Cmd.none
      )

    RemoveSelectedElement ->
      ( { model | array = (remove model.selectedIndex model.array) 
        , selectedIndex = Nothing }
      , Cmd.none
      )

    SwitchToEditing ->
      ( { model | state = Editing }
      , Cmd.none
      )

    SwitchToRegular -> 
      ( { model | state = Regular }
      , Cmd.none
      )

    SaveAndSwitchToRegular -> 
      ( { model | state = Regular }
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

remove : Maybe Int -> Array.Array a -> Array.Array a
remove optionalIndex a =
  case optionalIndex of
    Nothing -> a

    Just i ->
      let
        a1 = Array.slice 0 i a
        a2 = Array.slice (i+1) (Array.length a) a
      in
        Array.append a1 a2

-- helper for update function
addElement : Model -> (Model, Cmd Msg)
addElement model = 
  ( { model | array = 
      Array.push 
        ( Record 
          "new element"
          (Array.length model.array + 1) 
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
    [ viewListPanel model.array model.selectedIndex model.state
    ,  
      ( case model.state of 
        Regular -> viewSelectedItemPanelInRegularState 
          (getSelectedRecord model.selectedIndex model.array)

        Editing -> viewSelectedItemPanelInEditingState
          (getSelectedRecord model.selectedIndex model.array)
      )
    ]
  }

-- List view with Add and Remove buttons
viewListPanel : Array.Array Record -> Maybe Int-> State -> Html Msg
viewListPanel array optionalIndex state = 
  div []
    [ viewListPanelButton "Add" state AddNewElement 
    , viewListPanelButton "Remove" state RemoveSelectedElement
    , viewList array optionalIndex state
    ]

viewListPanelButton : String -> State -> Msg -> Html Msg
viewListPanelButton label state msg = 
  button ( List.concat
    [ [ onClick msg ]
    , case state of 
        Regular -> []
        Editing -> [ disabled True ]
    ]
  ) [ text label ]

viewList : Array.Array Record -> Maybe Int -> State -> Html Msg
viewList array optionalIndex state = 
  ul []
  ( List.map 
    (\(index,record) -> 
      buildListItem 
      ( prepareItemWithCheckOfSelection 
          (addOnClickIfRegular (index,record) state) 
          optionalIndex
      )
    )
    (Array.toIndexedList array)
  )

addOnClickIfRegular : (Int, Record) -> State -> (Int, String, List (Attribute Msg))
addOnClickIfRegular (index, record) state =
  ( index
  , record.name
  , case state of
      Regular -> [ onClick (SelectElement index) ]
      Editing -> [] 
  )

--Resolve Maybe operator
prepareItemWithCheckOfSelection : (Int, String, List (Attribute Msg)) -> Maybe Int -> (String, List (Attribute Msg))
prepareItemWithCheckOfSelection (index, label, list) optionalIndex = 
  case optionalIndex of
    Nothing -> (label, list)
    Just selectedIndex -> 
      addStyleIfItemSelected (index, label, list) selectedIndex

addStyleIfItemSelected : (Int, String, List (Attribute Msg)) -> Int -> (String, List (Attribute Msg))
addStyleIfItemSelected (index, label, list) selectedIndex =
  ( label
  , List.concat
        [ list
        , if index == selectedIndex then
            [ style "color" "red" ]
          else
            []
        ]
  )

buildListItem : (String, List (Attribute Msg)) -> Html Msg
buildListItem (name, list) = 
  li list [ text name ]


onClickAttrituteAsList : State -> Msg-> List (Attribute Msg)
onClickAttrituteAsList state msg= 
  case state of
    Regular -> [ onClick msg ]
    Editing -> []

-- Table view of selected item details
--viewSelectedItemPanel : Array.Array Record -> Maybe Int -> State -> Html Msg
--viewSelectedItemPanel : 

getSelectedRecord :  Maybe Int -> Array.Array Record -> Record
getSelectedRecord optionalIndex array = 
  case optionalIndex of 
    Nothing -> Record "n/a" 0 "n/a"

    Just index -> 
      case (Array.get index array) of
        Nothing -> Record "n/a" 0 "n/a"

        Just record -> record
      

viewSelectedItemPanelInRegularState : Record -> Html Msg
viewSelectedItemPanelInRegularState record = 
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
    , button [ onClick SwitchToEditing ] [ text "Edit" ]
    ]

viewSelectedItemPanelInEditingState : Record -> Html Msg
viewSelectedItemPanelInEditingState record = 
  Html.form []
    [ table []
        [ caption [] [ text "Editing item details" ]
        , tbody [] 
            [ tr [] 
                [ td [] [ text "name:" ]
                , td [] [ input [] [] ]
                ]
            , tr []
                [ td [] [ text "intField:" ]
                , td [] [ input [] [] ]
                ]
            , tr [] 
                [ td [] [ text "stringField:" ]
                , td [] [ input [] [] ]
                ]
            ]    
        ]
    , button [ onClick SaveAndSwitchToRegular ] [ text "Save" ]
    , button 
        [ onClick SwitchToRegular
        , type_ "reset"
        ] [ text "Cancel" ]
    ] 