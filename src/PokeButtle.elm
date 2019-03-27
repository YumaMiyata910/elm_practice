module PokeButtle exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { myself : Poke
    , enemy : Poke
    , select : Poke
    }


type alias Poke =
    { guide : String
    , name : String
    , element : Element
    , status : Ability
    , skill : List Skill
    }


type Element
    = Normal
    | Fire
    | Water
    | Grass
    | Electric
    | Fight
    | Ground
    | Flying
    | Ice


type alias Ability =
    { hp : Int
    , attack : Int
    , defense : Int
    }


type alias Skill =
    { name : String
    , element : Element
    , power : Int
    }


notPoke : Poke
notPoke =
    Poke "" "" Normal (Ability 0 0 0) [ Skill "" Normal 0 ]


init : Model
init =
    { myself = notPoke
    , enemy = notPoke
    , select = notPoke
    }


pokeList =
    [ Poke "003" "フシギバナ" Grass (Ability 90 30 70) [ Skill "" Normal 0 ]
    , Poke "006" "リザードン" Fire (Ability 90 60 50) [ Skill "" Normal 0 ]
    , Poke "009" "カメックス" Water (Ability 110 40 60) [ Skill "" Normal 0 ]
    , Poke "018" "ピジョット" Flying (Ability 60 50 30) [ Skill "" Normal 0 ]
    , Poke "020" "ラッタ" Normal (Ability 60 40 40) [ Skill "" Normal 0 ]
    , Poke "025" "ピカチュウ" Electric (Ability 70 70 30) [ Skill "" Normal 0 ]
    , Poke "068" "カイリキー" Fight (Ability 80 60 20) [ Skill "" Normal 0 ]
    , Poke "112" "サイドン" Ground (Ability 80 50 60) [ Skill "" Normal 0 ]
    , Poke "131" "ラプラス" Ice (Ability 100 50 60) [ Skill "" Normal 0 ]
    ]


pokeArray =
    Array.fromList pokeList



-- UPDATE


type Msg
    = SelectPoke String
    | MakeMyPoke
    | MakeEnemyPoke


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectPoke guide ->
            let
                filterList =
                    List.filter (\poke -> poke.guide == guide) pokeList
            in
            if List.length filterList /= 0 then
                case List.head filterList of
                    Just poke ->
                        { model | select = poke }

                    Nothing ->
                        { model | select = notPoke }

            else
                { model | select = notPoke }

        MakeMyPoke ->
            { model
                | myself = model.select
                , select = notPoke
            }

        MakeEnemyPoke ->
            { model
                | enemy = model.select
                , select = notPoke
            }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ viewSelectPoke "自分" model.myself
            , viewSelectPoke "敵" model.enemy
            ]
        , div []
            viewPokeList
        , div []
            [ button [ onClick MakeMyPoke ] [ text "自分のポケモンはキミに決めた!" ]
            , button [ onClick MakeEnemyPoke ] [ text "敵のポケモンはキミに決めた!" ]
            ]
        ]


viewSelectPoke : String -> Poke -> Html Msg
viewSelectPoke w poke =
    div []
        [ p [] [ text (w ++ ":") ]
        , p [] [ text poke.name ]
        ]


viewPokeList : List (Html Msg)
viewPokeList =
    List.map viewPokeRadio pokeList


viewPokeRadio : Poke -> Html Msg
viewPokeRadio poke =
    label []
        [ input [ type_ "radio", name "pokeSelect", onClick (SelectPoke poke.guide) ] []
        , text poke.name
        ]
