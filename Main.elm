module Main exposing (..)

import AnimationFrame
import Collage exposing (collage, filled, move, rect, toForm)
import Element exposing (image, toHtml)
import Color exposing (rgb)
import Html exposing (Html, img, text)
import Key exposing (..)
import Keyboard exposing (KeyCode)
import Task exposing (perform)
import Time exposing (Time)
import Window exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , dir : Direction
    , win : Size
    }


type Direction
    = Left
    | Right


mario : Model
mario =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = Right
    , win = { height = 300, width = 300 }
    }



-- UPDATE


type Msg
    = Animate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | WinSize Size


init : ( Model, Cmd Msg )
init =
    ( mario, perform WinSize Window.size )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mario =
    case msg of
        Animate dt ->
            ( animate dt mario, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode mario, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode mario, Cmd.none )

        WinSize size ->
            ( winSize size mario, Cmd.none )


animate : Time -> Model -> Model
animate dt mario =
    let
        rate =
            dt / 10
    in
        mario
            |> physics rate
            |> gravity rate


gravity : Time -> Model -> Model
gravity dt mario =
    if mario.y > 0 then
        { mario | vy = mario.vy - dt / 4 }
    else
        { mario | vy = 0 }


jump : Model -> Model
jump mario =
    if mario.vy == 0 then
        { mario | vy = 6.0 }
    else
        mario


keyDown : KeyCode -> Model -> Model
keyDown keyCode mario =
    case Key.fromCode keyCode of
        ArrowLeft ->
            walk Left -1.0 mario

        ArrowRight ->
            walk Right 1.0 mario

        ArrowUp ->
            jump mario

        _ ->
            mario


keyUp : KeyCode -> Model -> Model
keyUp keyCode mario =
    case Key.fromCode keyCode of
        ArrowLeft ->
            walk mario.dir 0 mario

        ArrowRight ->
            walk mario.dir 0 mario

        _ ->
            mario


physics : Time -> Model -> Model
physics dt mario =
    { mario
        | x = mario.x + dt * mario.vx
        , y = max 0 (mario.y + dt * mario.vy)
    }


winSize : Size -> Model -> Model
winSize size mario =
    { mario | win = { height = size.height, width = size.width } }



-- VIEW


view : Model -> Html Msg
view mario =
    let
        ( w, h ) =
            ( toFloat mario.win.width, toFloat mario.win.height )

        dir =
            case mario.dir of
                Left ->
                    "left"

                Right ->
                    "right"

        verb =
            if mario.y > 0 then
                "jump"
            else if mario.vx /= 0 then
                "walk"
            else
                "stand"

        src =
            "imgs/mario/" ++ verb ++ "/" ++ dir ++ ".gif"

        marioImg =
            image 35 35 src

        groundY =
            62 - h / 2
    in
        collage mario.win.width
            mario.win.height
            [ rect w h
                |> filled (Color.rgb 174 238 238)
            , rect w 50
                |> filled (Color.rgb 74 167 43)
                |> move ( 0, 24 - h / 2 )
            , marioImg
                |> toForm
                |> move ( mario.x, mario.y + groundY )
            ]
            |> toHtml


walk : Direction -> Float -> Model -> Model
walk dir speed mario =
    { mario
        | dir = dir
        , vx = speed
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions mario =
    Sub.batch
        [ AnimationFrame.diffs Animate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
