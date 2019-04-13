module SimpleTimer exposing (SimpleTimer, create, isRunning, setColors, start, stop, tick, toSvg)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.PathD as Path exposing (Segment(..))
import Time exposing (Posix)



-- TYPES


type Status
    = Started Posix
    | Stopped


type alias SimpleTimer =
    { duration : Int
    , current : Maybe Posix
    , status : Status
    , colors : Colors
    }


type alias Colors =
    { color : Maybe String
    , stroke : Maybe String
    , background : Maybe String
    , backgroundStroke : Maybe String
    }


colorPresets : Colors
colorPresets =
    { color = Nothing
    , stroke = Nothing
    , background = Nothing
    , backgroundStroke = Nothing
    }



-- FUNCS


create : Int -> SimpleTimer
create durationInMs =
    { duration = durationInMs
    , current = Nothing
    , status = Stopped
    , colors = colorPresets
    }


start : Posix -> SimpleTimer -> SimpleTimer
start now timer =
    case timer.status of
        Stopped ->
            { timer
                | status = Started now
            }

        Started _ ->
            let
                stoppedTimer =
                    stop timer
            in
            { stoppedTimer
                | status = Started now
            }


stop : SimpleTimer -> SimpleTimer
stop timer =
    { timer
        | status = Stopped
        , current = Nothing
    }


isRunning : SimpleTimer -> Bool
isRunning timer =
    case timer.status of
        Started _ ->
            True

        Stopped ->
            False


tick : Posix -> SimpleTimer -> SimpleTimer
tick now timer =
    case timer.status of
        Started started ->
            let
                passed =
                    Time.posixToMillis now - Time.posixToMillis started
            in
            if passed >= timer.duration then
                { timer
                    | status = Stopped
                    , current = Nothing
                }

            else
                { timer
                    | current = Just now
                }

        _ ->
            timer


setColors : Colors -> SimpleTimer -> SimpleTimer
setColors newColors timer =
    { timer | colors = newColors }


toSvg : Int -> SimpleTimer -> Html msg
toSvg width { current, duration, status, colors } =
    let
        then_ =
            case status of
                Started psx ->
                    Time.posixToMillis psx

                _ ->
                    0

        now =
            current
                |> Maybe.map Time.posixToMillis
                |> Maybe.withDefault 0

        fraction =
            toFloat (now - then_) / toFloat duration

        viewBoxStr w =
            "0 0 " ++ String.fromInt width ++ " " ++ String.fromInt width

        center =
            toFloat width / 2
    in
    Svg.svg
        [ Attr.width <| String.fromInt width
        , Attr.height <| String.fromInt width
        , Attr.viewBox <| viewBoxStr width
        ]
        [ Svg.circle
            [ Attr.cx <| String.fromFloat center
            , Attr.cy <| String.fromFloat center
            , Attr.r <| String.fromFloat <| center - 5
            , Attr.fill <| Maybe.withDefault "rgb(255,0,0)" colors.color
            , Attr.strokeWidth "8"
            , Attr.stroke <| Maybe.withDefault "rgb(0,0,0)" colors.stroke
            ]
            []
        , Svg.path
            [ Attr.d <| createPath width fraction
            , Attr.stroke <| Maybe.withDefault "rgb(0,0,0)" colors.backgroundStroke
            , Attr.strokeWidth "0"
            , Attr.fill <| Maybe.withDefault "rgb(230,230,230)" colors.background
            ]
            []
        ]


createPath : Int -> Float -> String
createPath width fraction =
    let
        fwidth =
            toFloat width

        center =
            fwidth / 2

        radius =
            center

        rotation =
            0

        large =
            if fraction >= 0.5 then
                True

            else
                False

        clockwise =
            True

        phi =
            fraction * (2 * pi) - pi / 2

        target =
            ( radius * cos phi + fwidth / 2
            , radius * sin phi + fwidth / 2
            )
    in
    Path.pathD
        [ M ( center, center )
        , L ( center, 0 )

        --, M target
        , A ( radius, radius ) rotation large clockwise target
        , L ( center, center )
        , Z
        ]
