module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, float, int, maybe, string)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http exposing (defaultConfig)



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> { title = "ContextSwitch", body = [ view model ] }
        }



-- MODEL


type alias Model =
    { tasks : WebData TaskList
    , tasksAdded : List (WebData Task)
    , addTaskField : String
    , selectedTask : Maybe Task
    , editedTask : Maybe Task
    }


type alias TaskList =
    List Task


type alias BookmarkContent =
    { title : String
    , content : Maybe String
    }


type alias Bookmark =
    { uri : String
    , content : Maybe BookmarkContent
    }


type alias ContextswitchData =
    { bookmarks : List Bookmark }


type alias Task =
    { id : String
    , description : String
    , project : Maybe String
    , urgency : Float
    , tags : List String
    , annotations : List String
    , priority : Maybe String
    , status : String
    , due : Maybe String
    , waitingUntil : Maybe String
    , contextswitch : Maybe ContextswitchData
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    getTasks
        { tasks = NotAsked
        , tasksAdded = []
        , addTaskField = ""
        , selectedTask = Nothing
        , editedTask = Nothing
        }



-- UPDATE


type Msg
    = HandleTaskListResponse (WebData TaskList)
    | GetTasks
    | AddTask
    | UpdateAddTaskField String
    | HandleAddTaskResponse (WebData Task)
    | ToggleDetails Task
    | EditTask Task
    | FinishTaskEdition
    | UpdateEditedTask String


getTasks : Model -> ( Model, Cmd Msg )
getTasks model =
    ( { model | tasks = Loading }
    , RemoteData.Http.getWithConfig jsonConfig "http://localhost:8000/tasks?filter=test" HandleTaskListResponse taskListDecoder
    )


jsonConfig : RemoteData.Http.Config
jsonConfig =
    { defaultConfig | headers = [ Http.header "Content-Type" "application/json" ] }


addTask : String -> Cmd Msg
addTask taskDefinition =
    RemoteData.Http.postWithConfig defaultConfig "http://localhost:8000/tasks" HandleAddTaskResponse taskDecoder (encodeTaskDefinition taskDefinition)


taskListDecoder : Decoder TaskList
taskListDecoder =
    Decode.list taskDecoder


taskDecoder : Decoder Task
taskDecoder =
    Decode.succeed Task
        |> Pipeline.required "id" string
        |> Pipeline.required "description" string
        |> Pipeline.optional "project" (maybe string) Nothing
        |> Pipeline.required "urgency" float
        |> Pipeline.optional "tags" (Decode.list string) []
        |> Pipeline.optional "annotations" (Decode.list string) []
        |> Pipeline.optional "priority" (maybe string) Nothing
        |> Pipeline.required "status" string
        |> Pipeline.optional "due" (maybe string) Nothing
        |> Pipeline.optional "waitingUntil" (maybe string) Nothing
        |> Pipeline.optional "contextswitch" (maybe contextswitchDataDecoder) Nothing


contextswitchDataDecoder : Decoder ContextswitchData
contextswitchDataDecoder =
    Decode.succeed ContextswitchData
        |> Pipeline.required "bookmarks" (Decode.list bookmarkDecoder)


bookmarkDecoder : Decoder Bookmark
bookmarkDecoder =
    Decode.succeed Bookmark
        |> Pipeline.required "uri" string
        |> Pipeline.optional "content" (maybe bookmarkContentDecoder) Nothing


bookmarkContentDecoder : Decoder BookmarkContent
bookmarkContentDecoder =
    Decode.succeed BookmarkContent
        |> Pipeline.required "title" string
        |> Pipeline.optional "content" (maybe string) Nothing


encodeTaskDefinition : String -> Encode.Value
encodeTaskDefinition taskDefinition =
    Encode.object
        [ ( "definition", Encode.string taskDefinition ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleTaskListResponse taskList ->
            let
                selectedTask =
                    case taskList of
                        Success tasks ->
                            List.head tasks

                        _ ->
                            Nothing
            in
            ( { model
                | tasks = taskList
                , tasksAdded = []
                , selectedTask = selectedTask
              }
            , Cmd.none
            )

        GetTasks ->
            getTasks model

        AddTask ->
            ( { model | addTaskField = "" }, addTask model.addTaskField )

        UpdateAddTaskField value ->
            ( { model | addTaskField = value }, Cmd.none )

        HandleAddTaskResponse task ->
            ( { model | tasksAdded = model.tasksAdded ++ [ task ] }, Cmd.none )

        ToggleDetails task ->
            case model.selectedTask of
                Just selectedTask ->
                    ( { model
                        | selectedTask =
                            if task == selectedTask then
                                Nothing

                            else
                                Just task
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | selectedTask = Just task }, Cmd.none )

        EditTask task ->
            case model.editedTask of
                Just editedTask ->
                    ( { model
                        | editedTask =
                            if task == editedTask then
                                Nothing

                            else
                                Just task
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | editedTask = Just task
                        , selectedTask = Just task
                      }
                    , Cmd.none
                    )

        FinishTaskEdition ->
            ( { model | editedTask = Nothing }, Cmd.none )

        UpdateEditedTask value ->
            case model.editedTask of
                Just editedTask ->
                    case model.tasks of
                        Success tasks ->
                            let
                                newTask =
                                    { editedTask | description = value }

                                newTasks =
                                    List.map
                                        (\t ->
                                            if t.id == newTask.id then
                                                newTask

                                            else
                                                t
                                        )
                                        tasks
                            in
                            ( { model
                                | editedTask = Just newTask
                                , selectedTask = Just newTask
                                , tasks = Success newTasks
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "uk-section uk-section-default" ]
        [ div [ class "uk-container uk-container-small" ]
            [ div [ attribute "uk-filter" "target: .status-filter" ]
                [ div []
                    [ ul [ class "uk-subnav uk-subnav-pill uk-width-expand" ]
                        [ li [ attribute "uk-filter-control" "" ]
                            [ a [ href "#" ] [ text "all" ] ]
                        , li [ class "uk-active", attribute "uk-filter-control" ".task-status-pending" ]
                            [ a [ href "#" ] [ text "pending" ] ]
                        , li [ attribute "uk-filter-control" ".task-status-completed" ]
                            [ a [ href "#" ] [ text "completed" ] ]
                        , li [ class "uk-width-expand" ] []
                        , li []
                            [ a [ href "#add-task-modal", class "uk-margin-remove", attribute "uk-icon" "icon: plus", attribute "uk-toggle" "" ] []
                            ]
                        ]
                    , div [ id "add-task-modal", class "uk-flex-top", attribute "uk-modal" "" ]
                        [ div [ class "uk-modal-dialog uk-margin-auto-vertical" ]
                            [ input
                                [ name "add-task"
                                , class "uk-input"
                                , autofocus True
                                , placeholder "+work finish report"
                                , onEnter AddTask
                                , onInput UpdateAddTaskField
                                , value model.addTaskField
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "status-filter uk-margin uk-grid-small uk-grid-match", attribute "uk-grid" "" ]
                    (viewRemoteTaskList model.selectedTask model.editedTask model.tasks
                        ++ viewTasksAdded model.selectedTask model.editedTask model.tasksAdded
                    )
                ]
            ]
        ]



-- View tasks list


viewRemoteTaskList : Maybe Task -> Maybe Task -> WebData TaskList -> List (Html Msg)
viewRemoteTaskList selectedTask editedTask taskList =
    case taskList of
        Loading ->
            [ div [ attribute "uk-spinner" "" ] [] ]

        Success tasks ->
            List.map (viewTask selectedTask editedTask) tasks

        Failure error ->
            [ text ("task loading failed with error: " ++ getErrorMsg error) ]

        NotAsked ->
            []


viewTasksAdded : Maybe Task -> Maybe Task -> List (WebData Task) -> List (Html Msg)
viewTasksAdded selectedTask editedTask tasksAdded =
    List.map (viewRemoteTask selectedTask editedTask) tasksAdded



-- View single task


viewRemoteTask : Maybe Task -> Maybe Task -> WebData Task -> Html Msg
viewRemoteTask selectedTask editedTask taskModel =
    case taskModel of
        Loading ->
            div [ attribute "uk-spinner" "" ] []

        Success task ->
            viewTask selectedTask editedTask task

        Failure error ->
            text ("Oh no, new task loading failed with error: " ++ getErrorMsg error)

        NotAsked ->
            div [] []


viewTask : Maybe Task -> Maybe Task -> Task -> Html Msg
viewTask maybeSelectedTask maybeEditedTask task =
    let
        textClass =
            if task.status == "completed" then
                "uk-text-success"

            else
                "uk-text-emphasis"

        arrow =
            case maybeSelectedTask of
                Just selectedTask ->
                    if selectedTask == task then
                        "triangle-down"

                    else
                        "triangle-right"

                Nothing ->
                    "triangle-left"
    in
    div [ class ("uk-width-1-1 task-status-" ++ task.status) ]
        [ div [ class "uk-card uk-card-default uk-card-hover uk-card-small" ]
            [ div
                [ class "uk-card-body uk-padding-remove-vertical uk-margin-small-top uk-margin-small-bottom"
                ]
                [ div [ class "uk-grid-small uk-flex-middle", attribute "uk-grid" "" ]
                    [ span [ class textClass, attribute "uk-icon" "icon: check" ] []
                    , viewTaskDescription maybeEditedTask task
                    , ul [ class "uk-iconnav" ]
                        [ li []
                            [ a [ href "#", attribute "uk-icon" "icon: file-edit", onClick (EditTask task) ] []
                            ]
                        , li []
                            [ a [ href "#", onClick (ToggleDetails task) ]
                                [ span [ attribute "uk-icon" "icon: bookmark" ] []
                                , viewBookmarkCount task.contextswitch
                                ]
                            ]
                        ]
                    , span [ attribute "uk-icon" ("icon: " ++ arrow), onClick (ToggleDetails task) ] []
                    ]
                , viewTaskDetailsIfSelected maybeSelectedTask task
                ]
            ]
        ]


viewTaskDescription : Maybe Task -> Task -> Html Msg
viewTaskDescription maybeEditedTask task =
    let
        description =
            div [ class "uk-width-expand", onClick (ToggleDetails task) ]
                [ h3 [ class "uk-card-title" ]
                    [ span [ class " uk-text-lighter" ]
                        [ text task.description ]
                    ]
                ]
    in
    case maybeEditedTask of
        Just editedTask ->
            if editedTask == task then
                div [ class "uk-width-expand" ]
                    [ div [ class "uk-inline" ]
                        [ span [ class "uk-form-icon uk-form-icon-flip", attribute "uk-icon" "icon: check" ] []
                        , input
                            [ name "edit-task"
                            , class "uk-input"
                            , autofocus True
                            , onEnter FinishTaskEdition
                            , onInput UpdateEditedTask
                            , value task.description
                            ]
                            []
                        ]
                    ]

            else
                description

        Nothing ->
            description


viewTaskDetailsIfSelected : Maybe Task -> Task -> Html Msg
viewTaskDetailsIfSelected maybeSelectedTask task =
    case maybeSelectedTask of
        Just selectedTask ->
            if selectedTask == task then
                viewTaskDetails task

            else
                div [] []

        Nothing ->
            div [] []


viewTaskDetails : Task -> Html Msg
viewTaskDetails task =
    div [ class "uk-margin uk-margin-small" ]
        [ hr [ class "uk-margin-small" ] []
        , case task.contextswitch of
            Just contextswitch ->
                viewContextswitchData contextswitch

            Nothing ->
                div [] []
        , viewTaskData task
        ]


viewContextswitchData : ContextswitchData -> Html Msg
viewContextswitchData contextswitch =
    div [ class "uk-grid-small uk-grid-match", attribute "uk-grid" "" ]
        (List.map viewBookmark contextswitch.bookmarks
            ++ [ span [ class "uk-margin-remove", attribute "uk-icon" "icon: plus" ] [] ]
        )


viewBookmark : Bookmark -> Html Msg
viewBookmark bookmark =
    div [ class "uk-width-1-1 uk-text-small uk-margin-remove" ]
        [ div [ class "uk-grid-small uk-flex-middle", attribute "uk-grid" "" ]
            [ span [ attribute "uk-icon" "icon: bookmark" ] []
            , a [ href bookmark.uri ]
                [ text bookmark.uri ]
            ]
        ]


viewBookmarkCount : Maybe ContextswitchData -> Html Msg
viewBookmarkCount maybeContextswitch =
    let
        countString =
            case maybeContextswitch of
                Just contextswitch ->
                    " (" ++ String.fromInt (List.length contextswitch.bookmarks) ++ ")"

                Nothing ->
                    " (0)"
    in
    span []
        [ text countString ]


viewTaskData : Task -> Html Msg
viewTaskData task =
    div [ class "uk-grid-small uk-child-width-expand", attribute "uk-grid" "" ]
        [ maybeViewText "priority: " task.priority
        , maybeViewText "project: " task.project
        ]


viewTaskTag : String -> Html Msg
viewTaskTag tag =
    span [ class "uk-label" ] [ text tag ]



-- View utils


maybeViewText : String -> Maybe String -> Html Msg
maybeViewText prefix maybeText =
    div []
        [ span [ class "uk-text-meta" ]
            [ text (prefix ++ Maybe.withDefault "-" maybeText) ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "noop"
    in
    on "keydown" (Decode.andThen isEnter keyCode)


getErrorMsg : Http.Error -> String
getErrorMsg error =
    case error of
        Http.BadUrl url ->
            "The url requested (" ++ url ++ ") was not valid"

        Http.Timeout ->
            "The requested timed out"

        Http.NetworkError ->
            "Cannot connect to remote API"

        Http.BadStatus status ->
            case status of
                400 ->
                    "The request was malformed and the server couldn't process it"

                403 ->
                    "You are not authorized to access this resource"

                404 ->
                    "The requested resource could not be found"

                500 ->
                    "Something went wrong on the server"

                _ ->
                    "The request failed with status: " ++ String.fromInt status

        Http.BadBody errorStr ->
            "Parsing the response body failed: " ++ errorStr
