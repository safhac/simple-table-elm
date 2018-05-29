module Main exposing (..)

import Html exposing (Html, div, input, table, td, text, th, thead, tr)
import Html.Attributes exposing (colspan)
import Html.Events exposing (onClick)
import Styles exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- Model


type alias Model =
    { list : List Note
    , sorter : Msg
    }


type alias Note =
    { body : String
    , dateCreated : Float
    }



-- Msg


type Msg
    = ByText Bool
    | ByDate Bool



-- Init


init : ( Model, Cmd Msg )
init =
    ( { list = initialNotes
      , sorter = ByText True
      }
    , Cmd.none
    )


createNote : String -> Note
createNote message =
    { body = toString message
    , dateCreated = 0
    }


initialNotes : List Note
initialNotes =
    [ createNote "first"
    , createNote "wat?"
    , createNote "hi"
    , createNote "lol"
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ByDate flag ->
            let
                sorted =
                    if flag == True then
                        model.list
                            |> List.sortBy .dateCreated
                    else
                        model.list
                            |> List.sortBy .dateCreated
                            |> List.reverse
            in
            { list = sorted
            , sorter = ByDate flag
            }
                ! []

        ByText flag ->
            let
                sorted =
                    if flag == True then
                        model.list
                            |> List.sortBy .body
                    else
                        model.list
                            |> List.sortBy .body
                            |> List.reverse
            in
            { list = sorted, sorter = ByText flag } ! []



-- VIEW


view : Model -> Html Msg
view model =
    let
        header =
            renderHead model.sorter

        body =
            renderRows model.list
    in
    div [ standardContainerStyle ]
        [ table [ tableStyle ]
            (header
                :: body
            )
        ]


renderHead : Msg -> Html Msg
renderHead msg =
    let
        dateFlag =
            case msg of
                ByDate True ->
                    False

                _ ->
                    True

        textFlag =
            case msg of
                ByText True ->
                    False

                _ ->
                    True
    in
    thead []
        [ tr [] [ td [ colspan 2 ] [ text "Search ", input [] [] ] ]
        , th [ onClick (ByText textFlag) ] [ text "note" ]
        , th [ onClick (ByDate dateFlag) ] [ text "date" ]
        ]


renderRows : List Note -> List (Html Msg)
renderRows list =
    list
        |> List.map renderNote


renderNote : Note -> Html msg
renderNote { body, dateCreated } =
    tr [ rowStyle ]
        [ td [] [ text body ]
        , td [] [ toString dateCreated |> text ]
        ]
