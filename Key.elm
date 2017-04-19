module Key exposing (..)


type Key
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft

        39 ->
            ArrowRight

        38 ->
            ArrowUp

        _ ->
            Unknown
