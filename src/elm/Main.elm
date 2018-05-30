port module Main exposing (..)

import Html exposing (Html, div, input, table, td, text, th, thead, tr)
import Html.Attributes exposing (colspan, value)
import Html.Events exposing (onClick, onInput)
import Styles exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \model -> gotTime OnGetTime
        }



-- PORTS


port callGetTime : () -> Cmd msg


port gotTime : (String -> msg) -> Sub msg



-- send : msg -> Cmd msg
-- send msg =
--     Task.perform identity identity (Task.succeed msg)
-- Model


type alias Model =
    { list : List Note
    , state : Msg
    , filterBy : Maybe String
    }


type alias Note =
    { id : NoteId
    , body : String
    , dateCreated : String
    }


type alias NoteId =
    Int



-- Msg


type Msg
    = SortByText Bool
    | SortByDate Bool
    | FilterBy String
    | Select NoteId
    | UpdateNote String
    | OnGetTime String
    | GetTime



-- Init


init : ( Model, Cmd Msg )
init =
    ( { list = initialNotes
      , state = SortByText True
      , filterBy = Nothing
      }
    , Cmd.none
    )


createNote : Int -> String -> Note
createNote id_ message =
    { id = id_
    , body = toString message
    , dateCreated = "5/28/2018, 8:23:54 AM"
    }


initialNotes : List Note
initialNotes =
    [ createNote 1 "first"
    , createNote 2 "wat?"
    , createNote 3 "hi"
    , createNote 4 "lol"
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SortByDate flag ->
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
            { model
                | list = sorted
                , state = SortByDate flag
            }
                ! []

        SortByText flag ->
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
            { model | list = sorted, state = SortByText flag } ! []

        FilterBy search ->
            { model | filterBy = Just search } ! []

        Select noteId ->
            { model | state = Select noteId } ! []

        UpdateNote text ->
            let
                selectedNoteId =
                    case model.state of
                        Select id ->
                            id

                        _ ->
                            0

                updatedList =
                    model.list
                        |> List.map
                            (\note ->
                                if note.id == selectedNoteId then
                                    { note | body = text }
                                else
                                    note
                            )
            in
            { model | list = updatedList } ! []

        OnGetTime time ->
            let
                _ =
                    Debug.log "" time
            in
            model ! []

        GetTime ->
            ( model, callGetTime () )



-- VIEW


view : Model -> Html Msg
view model =
    let
        tHead =
            renderHead model.state

        filtered =
            case model.filterBy of
                Nothing ->
                    model.list

                Just search ->
                    model.list
                        |> List.filter
                            (\note ->
                                String.toLower note.body
                                    |> String.contains search
                            )

        tBody =
            renderRows model.state filtered
    in
    div [ standardContainerStyle ]
        [ table [ tableStyle ]
            (tHead
                :: tBody
            )
        ]


renderHead : Msg -> Html Msg
renderHead msg =
    let
        dateFlag =
            case msg of
                SortByDate True ->
                    False

                _ ->
                    True

        textFlag =
            case msg of
                SortByText True ->
                    False

                _ ->
                    True
    in
    thead []
        [ tr []
            [ td []
                [ text "Search "
                , input [ onInput FilterBy ] []
                ]
            , td [ onClick GetTime ] [ text "time" ]
            ]
        , th [ onClick (SortByText textFlag) ] [ text "note" ]
        , th [ onClick (SortByDate dateFlag) ] [ text "date" ]
        ]


renderRows : Msg -> List Note -> List (Html Msg)
renderRows msg list =
    let
        selectedId =
            case msg of
                Select id ->
                    id

                _ ->
                    0
    in
    list
        |> List.map (\note -> renderNote note selectedId)


renderNote : Note -> NoteId -> Html Msg
renderNote { id, body, dateCreated } selecteId =
    let
        html =
            if id == selecteId then
                input
                    [ value body
                    , onInput UpdateNote
                    ]
                    []
            else
                text body
    in
    tr
        [ rowStyle
        , onClick (Select id)
        ]
        [ td [] [ html ]
        , td [] [ toString dateCreated |> text ]
        ]
