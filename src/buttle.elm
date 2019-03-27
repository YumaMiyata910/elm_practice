module Buttle exposing (Character, Model, Msg(..), attack, init, main, update, view)

import Browser
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { wai : Character
    , teki : Character
    }


type alias Character =
    { name : String
    , hp : Int
    , lightAttackPoint : Int
    , strongAttackPoint : Int
    }


init : Model
init =
    { wai =
        { name = "ワイ"
        , hp = 1000
        , lightAttackPoint = 20
        , strongAttackPoint = 50
        }
    , teki =
        { name = "テキ"
        , hp = 2000
        , lightAttackPoint = 5
        , strongAttackPoint = 30
        }
    }


type Msg
    = WaiAttack Int
    | TekiAttack Int
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        WaiAttack num ->
            let
                attackedTeki =
                    if num == 1 then
                        attack model.teki model.wai.lightAttackPoint

                    else
                        attack model.teki model.wai.strongAttackPoint
            in
            { model | teki = attackedTeki }

        TekiAttack num ->
            let
                attackedWai =
                    if num == 1 then
                        attack model.wai model.teki.lightAttackPoint

                    else
                        attack model.wai model.teki.strongAttackPoint
            in
            { model | wai = attackedWai }

        Reset ->
            init


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li []
                [ div [] [ text model.wai.name ]
                , div [] [ text ("HP: " ++ String.fromInt model.wai.hp) ]
                , button [ onClick (WaiAttack 1) ] [ text "弱攻撃" ]
                , button [ onClick (WaiAttack 2) ] [ text "強攻撃" ]
                ]
            , li []
                [ div [] [ text model.teki.name ]
                , div [] [ text ("HP: " ++ String.fromInt model.teki.hp) ]
                , button [ onClick (TekiAttack 1) ] [ text "弱攻撃" ]
                , button [ onClick (TekiAttack 2) ] [ text "強攻撃" ]
                ]
            ]
        , div []
            [ button [ onClick Reset ] [ text "リセット" ] ]
        ]


attack : Character -> Int -> Character
attack character damage =
    let
        newHp =
            if character.hp > damage then
                character.hp - damage

            else
                0
    in
    { character | hp = newHp }
