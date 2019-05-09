module Main exposing (main)

import Array
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
import Element.Lazy exposing (..)
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events
import Json.Decode as Decode
import Process
import Random
import SimpleTimer exposing (SimpleTimer)
import Task
import Thumbs
import Time exposing (Posix)
import Wiggle



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
    | TimerDone Int
    | SwitchStateTo AppState



-- TYPES


type AppState
    = Splash
    | Main ProblemState
    | About
    | Settings


type ProblemState
    = Calculating
    | Feedback


type alias Problem =
    { problem : Maybe ( Int, Int )
    , answer : Answer
    , id : Int
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


type alias Scales =
    { tiny : Int
    , small : Int
    , medium : Int
    , large : Int
    , extraLarge : Int
    , xxLarge : Int
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
    , id = 0
    }


initTimer : SimpleTimer
initTimer =
    SimpleTimer.create (config.timerDuration * 1000)
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
                        , delay 0 <| SwitchStateTo (Main Calculating)
                        , delay 0 <| StartNewProblem
                        ]
                    )

                Empty ->
                    { model | name = Invalid "" }
                        |> withCommand Cmd.none

                _ ->
                    ( model, Cmd.none )

        SwitchStateTo state ->
            route state model
                |> withCommand Cmd.none

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
            { model | timer = newTimer }
                |> withCommand Cmd.none

        NewProblemGenerated factors ->
            { model | currentProblem = Problem (Just factors) NoAnswer (model.currentProblem.id + 1) }
                |> withCommand
                    (delay
                        config.timerDuration
                        (TimerDone (model.currentProblem.id + 1))
                    )

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

        TimerDone id ->
            case model.appState of
                Main problemState ->
                    case problemState of
                        Calculating ->
                            if model.currentProblem.id == id then
                                let
                                    problem =
                                        model.currentProblem

                                    newProblem =
                                        { problem | answer = NoAnswer }
                                in
                                { model
                                    | passedProblems = newProblem :: model.passedProblems
                                    , appState = Main Feedback
                                }
                                    |> withCommand (delay config.showNegativeFeedback ShowCorrectSolution)

                            else
                                ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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


route : AppState -> Model -> Model
route target model =
    let
        source =
            model.appState
    in
    case source of
        Splash ->
            case target of
                Main _ ->
                    { model | appState = Main Calculating }

                About ->
                    { model | appState = About }

                Settings ->
                    { model | appState = Settings }

                _ ->
                    model

        About ->
            case target of
                Splash ->
                    { model | appState = Splash }

                _ ->
                    model

        Settings ->
            case target of
                Splash ->
                    { model | appState = Splash }

                _ ->
                    model

        _ ->
            model


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
    , xxLarge = round <| scaled 7
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

        About ->
            viewAbout model
                |> wrapInContainer

        Settings ->
            viewSettings model
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


viewAbout : Model -> Element Msg
viewAbout model =
    let
        wrapper =
            column
                [ padding scales.medium
                , width fill
                , height fill

                --, explain Debug.todo
                ]

        header =
            row
                [ width fill
                , height shrink
                , Font.size scales.small
                ]
                [ el
                    [ alignLeft
                    , Events.onClick <| SwitchStateTo Splash
                    , pointer
                    ]
                    (text "< Zurück")
                , el
                    [ alignRight
                    ]
                    (text "Informationen")
                ]

        deadSpace h =
            row
                [ height <| px h
                , width fill
                ]
                [ none ]

        heading =
            row
                [ width fill
                , height shrink
                , Font.size scales.large
                ]
                [ text "Über Einmaleins" ]

        info =
            column
                [ Font.size scales.small
                , Font.justify
                ]
                [ paragraph [] <| [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce dictum, diam tempus viverra porta, purus augue imperdiet sapien, sit amet tristique tellus magna consectetur elit. Morbi non tempus ante. Etiam nibh purus, pellentesque sed rhoncus non, varius at elit. Donec ultricies condimentum lectus, sed placerat arcu tincidunt sed. Cras fringilla odio id odio tristique pharetra. Duis at pharetra magna. Aenean urna nisl, aliquet ut risus a, bibendum pharetra libero. Curabitur vitae eros neque. Cras vitae ipsum sapien. Nam ut erat mattis, vehicula libero et, rhoncus nisi." ]
                , paragraph [] <| [ text "Aliquam ultrices facilisis felis commodo feugiat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nullam a sagittis nisl. Etiam quis est tellus. Pellentesque lacinia nunc tellus, ut malesuada nibh pulvinar a. Suspendisse semper metus nec felis mollis, id ultricies lorem fermentum. Nulla ullamcorper pharetra nisl ac blandit. Nam blandit erat quis leo tincidunt, gravida luctus neque lobortis." ]
                ]
    in
    lazy wrapper
        [ header
        , deadSpace scales.medium
        , heading
        , info
        ]


viewSettings : Model -> Element Msg
viewSettings model =
    let
        wrapper =
            column
                [ padding scales.medium
                , width fill
                , height fill

                --, explain Debug.todo
                ]

        header =
            row
                [ width fill
                , height shrink
                , Font.size scales.small
                ]
                [ el
                    [ alignLeft
                    , Events.onClick <| SwitchStateTo Splash
                    , pointer
                    ]
                    (text "< Zurück")
                , el
                    [ alignRight
                    ]
                    (text "Einstellungen")
                ]

        deadSpace h =
            row
                [ height <| px h
                , width fill
                ]
                [ none ]

        heading =
            row
                [ width fill
                , height shrink
                , Font.size scales.large
                ]
                [ text "Übung einstellen" ]

        info =
            column
                [ Font.size scales.small
                , Font.justify
                ]
                [ paragraph [] <| [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce dictum, diam tempus viverra porta, purus augue imperdiet sapien, sit amet tristique tellus magna consectetur elit. Morbi non tempus ante. Etiam nibh purus, pellentesque sed rhoncus non, varius at elit. Donec ultricies condimentum lectus, sed placerat arcu tincidunt sed. Cras fringilla odio id odio tristique pharetra. Duis at pharetra magna. Aenean urna nisl, aliquet ut risus a, bibendum pharetra libero. Curabitur vitae eros neque. Cras vitae ipsum sapien. Nam ut erat mattis, vehicula libero et, rhoncus nisi." ]
                , paragraph [] <| [ text "Aliquam ultrices facilisis felis commodo feugiat. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nullam a sagittis nisl. Etiam quis est tellus. Pellentesque lacinia nunc tellus, ut malesuada nibh pulvinar a. Suspendisse semper metus nec felis mollis, id ultricies lorem fermentum. Nulla ullamcorper pharetra nisl ac blandit. Nam blandit erat quis leo tincidunt, gravida luctus neque lobortis." ]
                ]
    in
    lazy wrapper
        [ header
        , deadSpace scales.medium
        , heading
        , info
        ]


viewMain : ProblemState -> Model -> Element Msg
viewMain problemState model =
    let
        background =
            case problemState of
                Calculating ->
                    el [ centerX, centerY ] (viewTimer model.timer)

                Feedback ->
                    case model.currentProblem.answer of
                        Correct _ ->
                            el [ centerX, centerY ] (viewThumbs True)

                        _ ->
                            el [ centerX, centerY ] (viewThumbs False)

        name =
            case model.name of
                Valid userName ->
                    userName

                _ ->
                    "N/A"

        wrapper =
            column
                [ padding scales.medium
                , width fill
                , height fill
                , centerY

                --, explain Debug.todo
                , behindContent <| background
                ]

        header =
            row
                [ width fill
                , Font.size scales.small
                ]
                [ el [ alignLeft ] <| text ("Spieler: " ++ name)
                , el [ alignRight ] <|
                    lazy viewClock model.timePassed
                ]

        problem =
            row
                [ centerX
                , height shrink
                , spacing scales.large
                ]
                [ lazy viewProblem model.currentProblem ]

        feedback =
            row
                [ centerX
                , height shrink
                , spacing scales.large
                ]
                [ lazy viewFeedback model.currentProblem ]

        dial clickable =
            row
                [ width <| px 400
                , height <| px 300
                , centerX
                ]
                [ viewDial clickable ]

        footer =
            row
                [ width fill
                , Font.size scales.small
                , alignBottom
                ]
                [ el [ alignRight ] <|
                    lazy viewResults model.passedProblems
                ]

        deadSpace h =
            row
                [ height <| px h
                , width fill
                ]
                [ none ]
    in
    case problemState of
        Calculating ->
            lazy wrapper
                [ header
                , deadSpace scales.large
                , problem
                , deadSpace scales.large
                , dial True
                , footer
                ]

        Feedback ->
            lazy wrapper
                [ header
                , deadSpace scales.large
                , feedback
                , deadSpace scales.large
                , dial False
                , footer
                ]


viewFeedback : Problem -> Element Msg
viewFeedback problem =
    let
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
                    ""

                Correct a ->
                    String.fromInt a

                Incorrect a ->
                    String.fromInt a

        color =
            if isCorrect then
                elColors.green

            else
                elColors.red

        setUpWiggle =
            if not isCorrect then
                Wiggle.setUp "wrong" [ -5, 8, -10, 13, -17, 8, -11, 8, -3 ]
                    |> html

            else
                none

        applyWiggle =
            if not isCorrect then
                Wiggle.apply "wrong" 500
                    |> htmlAttribute

            else
                Font.unitalicized
    in
    row
        [ padding 0
        , spacingXY scales.tiny 0
        , Font.size scales.xxLarge
        , Font.color color
        , Font.glow color 5
        ]
        [ setUpWiggle
        , el [ applyWiggle ] x
        , el [ applyWiggle ] middot
        , el [ applyWiggle ] y
        , el [ applyWiggle ] (text "=")
        , el [ applyWiggle ] (text <| String.padRight 3 ' ' answer)
        ]


viewTimer : SimpleTimer -> Element msg
viewTimer timer =
    SimpleTimer.toSvg 400 timer
        |> html


viewThumbs : Bool -> Element msg
viewThumbs isUp =
    let
        thumb =
            if isUp then
                Thumbs.up 300

            else
                Thumbs.down 300
    in
    html thumb


viewResults : List Problem -> Element msg
viewResults problems =
    let
        countResults =
            \problem acc ->
                case problem.answer of
                    Correct _ ->
                        Array.set
                            0
                            (Array.get 0 acc
                                |> Maybe.withDefault 0
                                |> (+) 1
                            )
                            acc

                    Incorrect _ ->
                        Array.set
                            1
                            (Array.get 1 acc
                                |> Maybe.withDefault 0
                                |> (+) 1
                            )
                            acc

                    NoAnswer ->
                        Array.set
                            2
                            (Array.get 2 acc
                                |> Maybe.withDefault 0
                                |> (+) 1
                            )
                            acc

        stats =
            problems
                |> List.foldr
                    countResults
                    (Array.fromList [ 0, 0, 0 ])
                |> Array.map
                    (text << String.fromInt)
                |> Array.toList
                |> (\list ->
                        case list of
                            [ right, wrong, empty ] ->
                                [ row
                                    []
                                    [ el [ Font.color <| elColors.green ] <| text <| String.fromChar '✓' ++ ": "
                                    , el [] right
                                    ]
                                , row
                                    []
                                    [ el [ Font.color <| elColors.red ] <| text <| String.fromChar '✗' ++ ": "
                                    , el [] wrong
                                    ]
                                , row
                                    []
                                    [ el [] <| text <| String.fromChar '∅' ++ ": "
                                    , el [] empty
                                    ]
                                ]

                            _ ->
                                [ text "Fehler" ]
                   )
    in
    row [ spacing scales.tiny ] stats


viewDial : Bool -> Element Msg
viewDial pressable =
    let
        event key =
            if pressable then
                Just <| KeyPressed <| key

            else
                Nothing

        alphaValue =
            if pressable then
                1

            else
                0.5

        viewButton key =
            let
                labelStr =
                    case key of
                        Number x ->
                            String.fromInt x

                        Control str ->
                            if str == "Backspace" then
                                String.fromChar '↤'

                            else if str == "Enter" then
                                "="

                            else
                                "Err"

                        _ ->
                            "Err"
            in
            Input.button
                [ width <| fillPortion 1
                , height fill
                , Background.color elColors.darkerBackground
                , Border.color elColors.darkerText
                , Border.width 1
                , alpha alphaValue
                ]
                { onPress = event key
                , label =
                    el
                        [ width shrink
                        , height shrink
                        , centerX
                        , centerY
                        ]
                        (text labelStr)
                }
    in
    column
        [ width fill
        , height fill
        , Font.size scales.large
        ]
        [ row
            [ width fill
            , height <| fillPortion 1
            ]
            [ viewButton <| Number 1
            , viewButton <| Number 2
            , viewButton <| Number 3
            ]
        , row
            [ width fill
            , height <| fillPortion 1
            ]
            [ viewButton <| Number 4
            , viewButton <| Number 5
            , viewButton <| Number 6
            ]
        , row
            [ width fill
            , height <| fillPortion 1
            ]
            [ viewButton <| Number 7
            , viewButton <| Number 8
            , viewButton <| Number 9
            ]
        , row
            [ width fill
            , height <| fillPortion 1
            ]
            [ viewButton <| Control "Backspace"
            , viewButton <| Number 0
            , viewButton <| Control "Enter"
            ]
        ]


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
    in
    row
        [ padding 0
        , spacingXY scales.tiny 0
        , Font.size scales.xxLarge
        ]
        [ el [] x
        , el [] middot
        , el [] y
        , el [] (text "=")
        , el [] (text <| String.padRight 3 ' ' answer)
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

        -- , explain Debug.todo
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
            , height shrink
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
        , row
            [ width fill
            , height <| px scales.small
            ]
            [ none ]
        , row
            [ width fill
            , height shrink
            , spacing scales.small
            , Font.size <| scales.small
            ]
            [ el
                [ centerX
                , height fill
                , width shrink
                , Events.onClick <| SwitchStateTo About
                , pointer
                ]
                (text "[Informationen]")
            , el
                [ centerX
                , height fill
                , width shrink
                , Events.onClick <| SwitchStateTo Settings
                , pointer
                ]
                (text "[Einstellungen]")
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.appState of
        Main _ ->
            if SimpleTimer.isRunning model.timer then
                Sub.batch
                    [ Time.every 1000 (\now -> TimeKeepingTick now)
                    , Browser.Events.onAnimationFrame (\now -> SimpleTimerTick now)
                    ]

            else
                Time.every 1000 (\now -> TimeKeepingTick now)

        _ ->
            Sub.none
