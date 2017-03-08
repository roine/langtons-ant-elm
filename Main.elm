module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (div)
import Time exposing (..)
import Dict exposing (Dict)


type alias Model =
    { x : Int
    , y : Int
    , squares : Dict ( Int, Int ) Square
    , direction : Direction
    }


size =
    5


canvasDimension =
    500


type Direction
    = Left
    | Right
    | Top
    | Down


type Square
    = White
    | Black


view model =
    div []
        [ svg [ width (toString canvasDimension), height (toString canvasDimension), viewBox "0 0 120 120" ]
            ([ rect
                [ x (toString model.x)
                , y (toString model.y)
                , width (toString size)
                , height (toString size)
                , fill "red"
                ]
                []
             ]
                ++ (Dict.toList model.squares
                        |> List.map
                            (\( ( xa, ya ), square ) ->
                                rect
                                    [ x (toString xa)
                                    , y (toString ya)
                                    , width ((toString size) ++ "px")
                                    , height (toString size)
                                    , fill "black"
                                    ]
                                    []
                            )
                   )
            )
        , div [] [ text <| toString model ]
        ]


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case Dict.get ( model.x, model.y ) model.squares of
                Nothing ->
                    { model
                        | squares = Dict.insert ( model.x, model.y ) Black model.squares
                        , direction = turnRight model.direction
                        , x =
                            if model.x <= 0 then
                                size
                            else if model.x >= canvasDimension then
                                canvasDimension - size
                            else
                                model.x + moveByDirectionX (turnRight model.direction)
                        , y =
                            if model.y <= 0 then
                                size
                            else if model.y >= canvasDimension then
                                canvasDimension - size
                            else
                                model.y + moveByDirectionY (turnRight model.direction)
                    }
                        ! []

                Just square ->
                    let
                        direction =
                            if square == Black then
                                turnLeft model.direction
                            else
                                turnRight model.direction

                        squares =
                            if square == White then
                                Dict.insert ( model.x, model.y ) Black model.squares
                            else
                                Dict.remove ( model.x, model.y ) model.squares
                    in
                        { model
                            | squares = squares
                            , direction = direction
                            , x =
                                if model.x <= 0 then
                                    size
                                else if model.x >= canvasDimension then
                                    canvasDimension - size
                                else
                                    model.x + moveByDirectionX direction
                            , y =
                                if model.y <= 0 then
                                    size
                                else if model.y >= canvasDimension then
                                    canvasDimension - size
                                else
                                    model.y + moveByDirectionY direction
                        }
                            ! []


moveByDirectionX direction =
    case direction of
        Top ->
            0

        Down ->
            0

        Left ->
            -1 * size

        Right ->
            1 * size


moveByDirectionY direction =
    case direction of
        Top ->
            -1 * size

        Down ->
            1 * size

        Left ->
            0

        Right ->
            0


turnRight direction =
    case direction of
        Left ->
            Top

        Right ->
            Down

        Top ->
            Right

        Down ->
            Left


turnLeft direction =
    case direction of
        Left ->
            Down

        Right ->
            Top

        Top ->
            Left

        Down ->
            Right


subscriptions model =
    every (1 * millisecond) Tick


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , subscriptions = subscriptions
        , init =
            ( { x = 50
              , y = 50
              , squares = Dict.empty
              , direction = Top
              }
            , Cmd.none
            )
        }
