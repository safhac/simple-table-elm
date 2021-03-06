port module Main exposing (..)

import Html exposing (Html, div, input, table, td, text, th, thead, tr)
import Html.Attributes exposing (colspan, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Styles exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions =
            \model ->
                gotTime OnGetTime
        }



-- PORTS


port callGetTime : () -> Cmd msg


port gotTime : (String -> msg) -> Sub msg


type alias Model =
    { currentTime : String
    , list : List Note
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
    | HandleKeyboardEvent KeyboardEvent



-- Init


init : ( Model, Cmd Msg )
init =
    ( { currentTime = ""
      , list = initialNotes
      , state = SortByText True
      , filterBy = Nothing
      }
    , callGetTime ()
    )


createNote : Int -> String -> String -> Note
createNote id_ message time =
    { id = id_
    , body = message
    , dateCreated = time
    }


initialNotes : List Note
initialNotes =
    [ createNote 1 "first" "5/02/2018, 11:23:54 AM"
    , createNote 2 "wat?" "5/04/2018, 9:23:54 PM"
    , createNote 3 "hi" "5/04/2018, 9:40:54 PM"
    , createNote 4 "lol" "5/04/2018, 10:02:54 PM"
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
                _ =
                    callGetTime ()

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
                                    { note
                                        | body = text
                                        , dateCreated = model.currentTime
                                    }
                                else
                                    note
                            )
            in
            { model | list = updatedList } ! []

        OnGetTime time ->
            { model | currentTime = time } ! []

        GetTime ->
            ( model, callGetTime () )

        HandleKeyboardEvent event ->
            case event.key of
                Just "Escape" ->
                    { model | state = HandleKeyboardEvent event } ! []

                Just "Enter" ->
                    { model | state = HandleKeyboardEvent event } ! []

                _ ->
                    model ! []



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
    div
        [ on "keydown" (Json.map HandleKeyboardEvent decodeKeyboardEvent)
        , standardContainerStyle
        ]
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
                    [ edited
                    , value body
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
        , td [] [ dateCreated |> text ]
        ]
