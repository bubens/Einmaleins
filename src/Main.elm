module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Config exposing (config)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events
import Json.Decode as Decode
import Process
import Random
import SimpleTimer exposing (SimpleTimer)
import Task
import Time exposing (Posix)



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
    { name : UserName
    , appState : AppState
    , timeStarted : Maybe Int
    , timePassed : Int
    , timer : SimpleTimer
    , currentProblem : Problem
    , passedProblems : List Problem
    }



-- TYPES


type AppState
    = Splash
    | Main ProblemState


type ProblemState
    = Calculating
    | Feedback


type alias Problem =
    { problem : Maybe ( Int, Int )
    , answer : Answer
    }


type Answer
    = NoAnswer
    | Correct Int
    | Incorrect Int


type UserName
    = Empty
    | Invalid String
    | Valid String


type Key
    = Number Int
    | Control String
    | Other


type Msg
    = Noop
    | NameEntered String
    | NameSubmitted
    | StartTimeKeeping Posix
    | StartNewProblem
    | StartTimer Posix
    | SimpleTimerTick Posix
    | TimeKeepingTick Posix
    | KeyPressed Key
    | CheckResult
    | NewProblemGenerated ( Int, Int )
    | ForceFocus String
    | ShowCorrectSolution


type alias Scales =
    { tiny : Int
    , small : Int
    , medium : Int
    , large : Int
    , extraLarge : Int
    }



-- INIT


init : flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Task.attempt (\_ -> Noop) (Dom.focus "einmaleins_input_name") )


initModel : Model
initModel =
    { name = Empty
    , appState = Splash
    , timeStarted = Nothing
    , timePassed = 0
    , timer = initTimer
    , currentProblem = initProblem
    , passedProblems = []
    }


initProblem : Problem
initProblem =
    { problem = Nothing
    , answer = NoAnswer
    }


initTimer : SimpleTimer
initTimer =
    SimpleTimer.create (config.timerDuration * 10000)
        |> SimpleTimer.setColors
            { color = Just <| toCssString config.colors.red
            , stroke = Just <| toCssString config.colors.black
            , background = Just <| toCssString config.colors.background
            , backgroundStroke = Just <| toCssString config.colors.background
            }



-- UTILS


withCommand : Cmd msg -> Model -> ( Model, Cmd msg )
withCommand msg model =
    ( model, msg )


withModell : Model -> Cmd msg -> ( Model, Cmd msg )
withModell model msg =
    ( model, msg )


delay : Int -> Msg -> Cmd Msg
delay s msg =
    Process.sleep (toFloat (s * 1000))
        |> Task.perform (always msg)


scaled : Int -> Float
scaled =
    modular 18.0 1.5


toCssString : Config.Color -> String
toCssString { red, green, blue, alpha } =
    "rgba("
        ++ String.fromInt red
        ++ ", "
        ++ String.fromInt green
        ++ ", "
        ++ String.fromInt blue
        ++ ", "
        ++ String.fromFloat alpha
        ++ ")"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameEntered str ->
            let
                newName =
                    if str == "" then
                        Empty

                    else if checkName str then
                        Valid str

                    else
                        Invalid str
            in
            { model | name = newName }
                |> withCommand Cmd.none

        NameSubmitted ->
            case model.name of
                Valid str ->
                    ( model
                    , Cmd.batch
                        [ Task.perform StartTimeKeeping Time.now
                        , delay 0 StartNewProblem
                        ]
                    )

                Empty ->
                    { model | name = Invalid "" }
                        |> withCommand Cmd.none

                _ ->
                    ( model, Cmd.none )

        StartNewProblem ->
            { model | appState = Main Calculating }
                |> withCommand
                    (Cmd.batch
                        [ Task.perform StartTimer Time.now
                        , Random.generate NewProblemGenerated generateProblem
                        , Task.attempt (\_ -> Noop) (Dom.focus "einmaleins_input_answer")
                        ]
                    )

        StartTimeKeeping now ->
            let
                millis =
                    Time.posixToMillis now
            in
            { model
                | timeStarted = Just millis
                , timePassed = 0
            }
                |> withCommand Cmd.none

        StartTimer now ->
            { model | timer = SimpleTimer.start now model.timer }
                |> withCommand Cmd.none

        TimeKeepingTick now ->
            case model.appState of
                Main _ ->
                    let
                        started =
                            Maybe.withDefault 0 model.timeStarted

                        passed =
                            Time.posixToMillis now - started
                    in
                    { model | timePassed = passed }
                        |> withCommand Cmd.none

                _ ->
                    ( model, Cmd.none )

        SimpleTimerTick now ->
            let
                newTimer =
                    SimpleTimer.tick now model.timer
            in
            if SimpleTimer.isRunning model.timer then
                { model | timer = newTimer }
                    |> withCommand Cmd.none

            else
                let
                    problem =
                        model.currentProblem

                    newProblem =
                        { problem | answer = NoAnswer }
                in
                { model
                    | timer = newTimer
                    , passedProblems = newProblem :: model.passedProblems
                    , appState = Main Feedback
                }
                    |> withCommand (delay 0 ShowCorrectSolution)

        NewProblemGenerated factors ->
            { model | currentProblem = Problem (Just factors) NoAnswer }
                |> withCommand Cmd.none

        ShowCorrectSolution ->
            let
                problem =
                    model.currentProblem

                ( x, y ) =
                    Maybe.withDefault ( 1, 1 ) problem.problem

                tmpProblem =
                    { problem | answer = Correct (x * y) }
            in
            { model | currentProblem = tmpProblem }
                |> withCommand (delay config.showPositiveFeedback StartNewProblem)

        KeyPressed key ->
            updateProblem key model

        ForceFocus id ->
            ( model, Task.attempt (\_ -> Noop) (Dom.focus id) )

        _ ->
            ( model, Cmd.none )


updateProblem : Key -> Model -> ( Model, Cmd Msg )
updateProblem key model =
    let
        dropRightInt x =
            floor (toFloat x / 10)

        insertRight old new =
            10 * old + new
    in
    case key of
        Number x ->
            let
                problem =
                    model.currentProblem

                factors =
                    Maybe.withDefault ( 1, 1 ) problem.problem

                newAnswer =
                    case problem.answer of
                        NoAnswer ->
                            toAnswer factors x

                        Correct y ->
                            insertRight y x
                                |> toAnswer factors

                        Incorrect y ->
                            insertRight y x
                                |> toAnswer factors

                withAnswer =
                    { problem | answer = newAnswer }
            in
            { model | currentProblem = withAnswer }
                |> withCommand Cmd.none

        Control string ->
            if string == "Backspace" then
                let
                    problem =
                        model.currentProblem

                    factors =
                        Maybe.withDefault ( 1, 1 ) problem.problem

                    newAnswer =
                        case problem.answer of
                            Incorrect x ->
                                if x < 10 then
                                    NoAnswer

                                else
                                    dropRightInt x
                                        |> toAnswer factors

                            Correct x ->
                                if x < 10 then
                                    NoAnswer

                                else
                                    dropRightInt x
                                        |> toAnswer factors

                            _ ->
                                NoAnswer

                    withAnswer =
                        { problem | answer = newAnswer }
                in
                { model | currentProblem = withAnswer }
                    |> withCommand Cmd.none

            else if string == "Enter" then
                case model.currentProblem.answer of
                    NoAnswer ->
                        ( model, Cmd.none )

                    Correct x ->
                        { model
                            | appState = Main Feedback
                            , passedProblems = model.currentProblem :: model.passedProblems
                            , timer = SimpleTimer.stop model.timer
                        }
                            |> withCommand (delay config.showPositiveFeedback StartNewProblem)

                    Incorrect x ->
                        { model
                            | appState = Main Feedback
                            , passedProblems = model.currentProblem :: model.passedProblems
                            , timer = SimpleTimer.stop model.timer
                        }
                            |> withCommand (delay config.showNegativeFeedback ShowCorrectSolution)

            else
                ( model, Cmd.none )

        Other ->
            ( model, Cmd.none )


toAnswer : ( Int, Int ) -> Int -> Answer
toAnswer ( x, y ) a =
    if (x * y) == a then
        Correct a

    else
        Incorrect a


generateProblem : Random.Generator ( Int, Int )
generateProblem =
    Random.pair (Random.int 1 10) (Random.int 1 10)


checkName : String -> Bool
checkName name =
    let
        isUmlaut c =
            let
                code =
                    Char.toCode <| Char.toUpper c
            in
            code == 196 || code == 220 || code == 214

        asList =
            String.toList name
    in
    List.all (\c -> Char.isAlphaNum c || isUmlaut c) asList
        && (List.length asList >= 3)
        && (List.length asList <= 12)



-- VIEW


scales : Scales
scales =
    { tiny = round <| scaled 1
    , small = round <| scaled 2
    , medium = round <| scaled 3
    , large = round <| scaled 4
    , extraLarge = round <| scaled 6
    }


toElementColor : Config.Color -> Color
toElementColor { red, green, blue, alpha } =
    rgba255 red green blue alpha


elColors =
    { background = toElementColor config.colors.background
    , darkerBackground = toElementColor config.colors.darkerBackground
    , text = toElementColor config.colors.text
    , darkerText = toElementColor config.colors.darkerText
    , red = toElementColor config.colors.red
    , green = toElementColor config.colors.green
    , black = toElementColor config.colors.black
    , transparent = toElementColor config.colors.transparent
    }


view : Model -> Html Msg
view model =
    case model.appState of
        Splash ->
            viewSplash model.name
                |> wrapInContainer

        Main problemState ->
            viewMain problemState model
                |> wrapInContainer


wrapInContainer : Element Msg -> Html Msg
wrapInContainer element =
    let
        containerLayout =
            layoutWith
                { options =
                    [ focusStyle
                        { borderColor = Nothing
                        , backgroundColor = Nothing
                        , shadow = Nothing
                        }
                    ]
                }
                [ width <| px config.width
                , height <| px config.height
                , Background.color elColors.background
                , Border.width 2
                , Border.solid
                , Border.color elColors.black
                , Font.color elColors.text
                , Font.size scales.medium
                , Font.family
                    [ Font.external
                        { name = "VT323"
                        , url = "https://fonts.googleapis.com/css?family=VT323"
                        }
                    , Font.sansSerif
                    ]

                -- , explain Debug.todo
                ]

        containerStyle =
            "width:"
                ++ String.fromInt config.width
                ++ "px;"
                ++ "height:"
                ++ String.fromInt config.height
                ++ "px"
    in
    element
        |> containerLayout
        |> List.singleton
        |> Html.div
            [ Html.id "einmaleins_container"
            , Html.attribute "style" containerStyle
            ]


viewMain : ProblemState -> Model -> Element Msg
viewMain problemState model =
    let
        maybeTimer =
            case problemState of
                Calculating ->
                    el [ centerX, centerY ] (viewTimer model.timer)

                _ ->
                    el [] none

        name =
            case model.name of
                Valid userName ->
                    userName

                _ ->
                    "N/A"

        headerRow =
            row
                [ width fill
                , Font.size scales.small
                ]
                [ el [ alignLeft ] <| text name
                , el [ alignRight ] <|
                    viewClock model.timePassed
                ]

        wrapper =
            column
                [ padding scales.medium
                , width fill
                , height fill
                , centerY

                --, explain Debug.todo
                , behindContent <| maybeTimer
                ]
    in
    case problemState of
        Calculating ->
            wrapper
                [ headerRow
                , row
                    [ centerX
                    , centerY
                    , height shrink
                    ]
                    [ viewProblem model.currentProblem ]
                ]

        Feedback ->
            wrapper
                [ headerRow
                , row
                    [ centerX
                    , centerY
                    , height shrink
                    ]
                    [ viewFeedback model ]
                ]


viewFeedback : Model -> Element Msg
viewFeedback model =
    let
        problem =
            model.currentProblem

        ( x, y ) =
            Maybe.withDefault ( 1, 1 ) problem.problem
                |> Tuple.mapBoth
                    (text << String.fromInt)
                    (text << String.fromInt)

        middot =
            text <|
                String.fromChar '·'

        isCorrect =
            case problem.answer of
                Correct _ ->
                    True

                _ ->
                    False

        answer =
            case problem.answer of
                NoAnswer ->
                    text ""

                Correct a ->
                    text <| String.fromInt a

                Incorrect a ->
                    text <| String.fromInt a

        color =
            if isCorrect then
                elColors.green

            else
                elColors.red

        answerAttr =
            if not isCorrect then
                [ width <| px 200, Font.strike ]

            else
                [ width <| px 200 ]
    in
    row
        [ padding 0
        , spacingXY scales.tiny 0
        , Font.size scales.extraLarge
        , Font.color color
        , Font.glow color 5
        ]
        [ el [] x
        , el [] middot
        , el [] y
        , el [] (text "=")
        , el answerAttr answer
        ]


keyDecoder : Decode.Decoder ( Msg, Bool )
keyDecoder =
    Decode.map
        (\string -> ( KeyPressed (toKey string), True ))
        (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            if Char.isDigit char then
                let
                    x =
                        String.fromChar char
                            |> String.toInt
                            |> Maybe.withDefault 0
                in
                Number x

            else
                Other

        _ ->
            if string == "Enter" || string == "Backspace" then
                Control string

            else
                Other


viewTimer : SimpleTimer -> Element msg
viewTimer timer =
    SimpleTimer.toSvg 400 timer
        |> html


viewProblem : Problem -> Element Msg
viewProblem problem =
    let
        ( x, y ) =
            Maybe.withDefault ( 1, 1 ) problem.problem
                |> Tuple.mapBoth
                    (text << String.fromInt)
                    (text << String.fromInt)

        middot =
            text <|
                String.fromChar '·'

        answer =
            case problem.answer of
                NoAnswer ->
                    ""

                Correct a ->
                    String.fromInt a

                Incorrect a ->
                    String.fromInt a

        label =
            row
                [ padding 0
                , spacingXY scales.tiny 0
                , Font.size scales.extraLarge
                ]
                [ el [] x
                , el [] middot
                , el [] y
                , el [] (text "=")
                ]

        id =
            "einmaleins_input_answer"
    in
    row
        [ centerX
        , centerY
        ]
        [ Input.text
            [ width <| px 200
            , padding 0
            , spacingXY scales.tiny 0
            , Background.color elColors.transparent
            , Border.color elColors.transparent
            , Font.color elColors.darkerText
            , Font.size scales.extraLarge
            , htmlAttribute (Html.Events.preventDefaultOn "keyup" keyDecoder)
            , htmlAttribute <| Html.id id
            , htmlAttribute <| Html.autofocus True
            , Events.onLoseFocus <| ForceFocus id
            ]
            { onChange = always Noop
            , text = answer
            , placeholder = Nothing
            , label =
                Input.labelLeft
                    []
                    label
            }
        ]


viewClock : Int -> Element msg
viewClock passed =
    let
        leadZero t =
            if t < 10 then
                "0" ++ String.fromInt t

            else
                String.fromInt t

        floatPassed =
            toFloat passed

        h =
            (1000 * 60 * 60)
                |> (/) floatPassed
                |> floor

        m =
            (1000 * 60)
                |> (/) (toFloat (passed - (h * 1000 * 60 * 60)))
                |> floor

        s =
            1000
                |> (/) (toFloat (passed - (h * 1000 * 60 * 60) - (m * 1000 * 60)))
                |> floor

        string =
            leadZero h
                ++ ":"
                ++ leadZero m
                ++ ":"
                ++ leadZero s
    in
    el
        [ height <| px scales.small
        , width fill
        ]
        (text string)


viewSplash : UserName -> Element Msg
viewSplash username =
    let
        name =
            case username of
                Empty ->
                    ""

                Invalid str ->
                    str

                Valid str ->
                    str

        borderColor =
            case username of
                Invalid _ ->
                    elColors.red

                Valid _ ->
                    elColors.green

                Empty ->
                    elColors.black

        label =
            column
                [ width <| px 240

                --, explain Debug.todo
                ]
                [ row
                    [ width fill ]
                    [ paragraph
                        [ Font.size scales.medium
                        , Font.center
                        ]
                        [ text "Dein Name:" ]
                    ]
                , row
                    [ width fill ]
                    [ paragraph
                        [ Font.size scales.small
                        , Font.center
                        ]
                        [ text "(3-12 Zeichen)" ]
                    ]
                ]
    in
    column
        [ padding scales.medium
        , width fill
        , height fill
        , centerY

        --, explain Debug.todo
        ]
    <|
        [ row
            [ spacing scales.medium
            , Font.size scales.extraLarge
            , width fill
            ]
            [ paragraph
                [ Font.center ]
                [ text "1 x 1 = ☺" ]
            ]
        , row
            [ spacing scales.medium
            , Font.size scales.tiny
            , width fill
            , Font.center
            ]
            [ paragraph
                [ Font.center ]
                [ text "Wir üben und üben und üben und üben und üben und üben - von Obersdorf bis Rügen" ]
            ]
        , row
            [ width fill
            , height <| px scales.medium
            ]
            [ none ]
        , row
            [ width fill

            --, explain Debug.todo
            ]
            [ el
                [ width <| fillPortion 1 ]
                none
            , Input.text
                [ width <| px 240
                , height <| px 70
                , Background.color elColors.text
                , Font.color elColors.background
                , Font.size scales.medium
                , Border.glow borderColor 5
                , htmlAttribute <| Html.id "einmaleins_input_name"
                ]
                { onChange = \input -> NameEntered input
                , text = name
                , placeholder = Just <| Input.placeholder [] (text config.splashPlaceholderText)
                , label =
                    Input.labelLeft [] label
                }
            , el
                [ width <| fillPortion 1 ]
                none
            ]
        , row
            [ width fill
            , height <| px scales.small
            ]
            [ none ]
        , row
            [ width fill
            , height <| px scales.medium
            ]
            [ Input.button
                [ width shrink
                , height fill
                , centerX
                , Border.color elColors.black
                , Border.width 3
                , Font.color elColors.darkerBackground
                , Background.color elColors.darkerText
                , padding 9
                ]
                { onPress = Just NameSubmitted
                , label = text "Los geht's"
                }
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appState of
        Splash ->
            Sub.none

        Main _ ->
            if SimpleTimer.isRunning model.timer then
                Sub.batch
                    [ Time.every 1000 (\now -> TimeKeepingTick now)
                    , Browser.Events.onAnimationFrame (\now -> SimpleTimerTick now)
                    ]

            else
                Time.every 1000 (\now -> TimeKeepingTick now)
