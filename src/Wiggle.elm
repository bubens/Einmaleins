module Wiggle exposing (apply, setUp)

import Html exposing (Html, node, text)
import Html.Attributes as Attributes


animationPrefix : String
animationPrefix =
    "Wiggle_82Hu2JK8_"


createCssDefinition : String -> List Int -> String
createCssDefinition name angles =
    case angles of
        [] ->
            ""

        _ ->
            let
                step =
                    100 / toFloat (List.length angles + 1)

                start =
                    "@keyframes " ++ name ++ " {0% { transform: rotate(0deg); }"

                definitions =
                    angles
                        |> List.indexedMap
                            (\index angle ->
                                String.fromFloat (step * toFloat (index + 1))
                                    ++ "% { transform: rotate("
                                    ++ String.fromInt angle
                                    ++ "deg); }"
                            )
                        |> String.join " "

                end =
                    "100% { transform: rotate(0deg); } }"
            in
            start ++ definitions ++ end


setUp : String -> List Int -> Html msg
setUp name angles =
    let
        animationName =
            animationPrefix ++ name
    in
    node
        "style"
        []
        [ text <| createCssDefinition animationName angles ]


apply : String -> Int -> Html.Attribute msg
apply name duration =
    let
        animation =
            [ animationPrefix ++ name -- name
            , String.fromInt duration ++ "ms" -- duration
            , "ease-out" -- timing
            , "0ms" -- delay
            , "1" -- iteration-count
            , "normal" -- normal
            , "none" -- fill-mode
            , "running"
            ]

        -- running
    in
    Attributes.style
        "animation"
        (String.join " " animation)
