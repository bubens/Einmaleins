module Config exposing (Color, config)


type alias Seconds =
    Int


type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


type alias Colors =
    { background : Color
    , darkerBackground : Color
    , text : Color
    , darkerText : Color
    , red : Color
    , green : Color
    , black : Color
    , transparent : Color
    }


type alias Config =
    { width : Int
    , height : Int
    , showNegativeFeedback : Seconds
    , showPositiveFeedback : Seconds
    , timerDuration : Seconds
    , splashPlaceholderText : String
    , colors : Colors
    }


colors : Colors
colors =
    { background = Color 41 44 47 1
    , darkerBackground = Color 33 25 38 1
    , text = Color 214 211 208 1
    , darkerText = Color 175 169 163 1
    , red = Color 204 6 5 1
    , green = Color 83 117 60 1
    , black = Color 0 0 0 1
    , transparent = Color 0 0 0 0
    }


config : Config
config =
    { width = 750
    , height = 500
    , showNegativeFeedback = 1
    , showPositiveFeedback = 3
    , timerDuration = 1
    , splashPlaceholderText = "Billybob"
    , colors = colors
    }
