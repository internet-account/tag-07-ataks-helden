module Main exposing (..)

import Common exposing ( Msg(..) )
import BarChart exposing (chart)

import Dict
import Dict.Extra

import Browser
import Html exposing (Html, div, button, text, br, a, ul, li)
import Html.Attributes exposing (id, class, width, height, href)
import Html.Events exposing (onClick)
import TypedSvg exposing (svg)

kleisli : (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
kleisli f g = f >> (Maybe.andThen g)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }

type alias Model =
    { selected : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( { selected = 21
      }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Select idx -> ( { model | selected = idx }, Cmd.none )

view : Model -> Html Msg
view model =
    let
        movesList = getMovesForPoints resultDict model.selected
        numMoves = List.length movesList
        movesUl = ul [] (List.map
                              (\move -> li [] [text move])
                              movesList
                        )
    in
        div []
        [ svg [ width 800, height 600 ] [chart validScoresTupleList]
        , br [] []
        , text <| String.concat ["Im Balkendiagramm: ", String.fromFloat numValidMoves, " gültige Züge"]
        , br [] []
        , invalidScoresLink model
        , br [] []
        , text "Auf Balken klicken, um Spielzüge angezeigt zu bekommen."
        , br [] []
        , text <| String.concat [movesText numMoves, pointsText numMoves model.selected]
        , br [] []
        , movesUl
        ]

movesText : Int -> String
movesText numMoves =
    if numMoves > 1
    then String.concat [String.fromInt numMoves, " Spielzüge "]
    else String.concat [String.fromInt numMoves, " Spielzug "]

pointsText : Int -> Int -> String
pointsText numMoves points =
    if numMoves > 1
    then
        if points < 0
        then "sind illegal:"
        else String.concat ["haben ", String.fromInt points, " Punkte:"] 
    else
        if points < 0
        then "ist illegal:"
        else String.concat ["hat ", String.fromInt points, " Punkte:"] 

invalidScoresLink : Model -> Html Msg
invalidScoresLink model =
    a [ onClick (Select -1), href "#" ] [ text <| String.concat ["Nicht im Balkendiagramm: ", String.fromInt numIllegalMoves, " Ungültige Züge"] ]

type alias Assets =
    { ataola : Int
    , quombao : Int
    , tamoro : Int
    }

allPermutations : List a -> List (List a)
allPermutations = allPermutations_ []

allPermutations_ : List a -> List a -> List (List a)
allPermutations_ hs ts =
    case ts of
        []     -> [[]]

        (x::xs) -> let rests = hs ++ xs
                   in  List.map ((::) x) (allPermutations_ [] rests) ++ allPermutations_ (x::hs) xs

startAssets : Assets
startAssets = Assets 4 4 5

type alias Card =
    { name : String
    , function : Assets -> Maybe Assets
    }

-- Rot = Ataola
-- Gelb = Quombao
-- Blau = Tamoro

anke : Card
anke = Card "A" (
    \assets ->
        if assets.ataola >= 3
        then Just { assets | ataola = assets.ataola - 3
                           , quombao = 2 * assets.quombao
                           }
        else Nothing
    )

elfant : Card
elfant = Card "E" (
    \assets ->
        if assets.quombao >= 2
        then Just { assets | quombao = assets.quombao - 2
                           , tamoro = assets.tamoro + 5
                           }
        else Nothing
    )

joey : Card
joey = Card "J" (
    \assets ->
        if assets.quombao >= 1
        then Just { assets | quombao = assets.quombao - 1
                           , tamoro = assets.tamoro + assets.ataola
                           }
        else Nothing
    )

pfitschiPfeil : Card
pfitschiPfeil = Card "P" (
    \assets ->
        if assets.tamoro >= 3
        then Just { assets | tamoro = assets.tamoro - 3
                                    , ataola = assets.ataola + 1
                                    , quombao = assets.quombao + 2
                                    }
        else Nothing
    )

sunJ : Card
sunJ = Card "S" (
    \assets ->
        if assets.tamoro >= 2
        then Just { assets | tamoro = assets.tamoro - 2
                           , ataola = assets.quombao 
                           , quombao = assets.ataola 
                           }
        else Nothing
    )

teledahner : Card
teledahner = Card "T" (
    \assets ->
        if assets.ataola >= 4
        then Just { assets | ataola = assets.ataola - 4 + assets.tamoro 
                           , tamoro = 0
                           }
        else Nothing
    )

allCards : List Card
allCards = [anke, elfant, joey, pfitschiPfeil, sunJ, teledahner]

playCards : List Card -> Assets -> Maybe Assets
playCards cards = List.foldr kleisli Just (List.map .function cards)

results : List (String, Maybe Assets)
results = List.map (\cards ->
                       ( String.concat <| List.map .name cards
                       , playCards cards startAssets
                       )
                   )
                   (allPermutations allCards)

resultSorted = List.sortBy (.ataola << (Maybe.withDefault { ataola = -1, quombao = -1, tamoro = -1 }) << Tuple.second) results

resultDict = Dict.Extra.groupBy (Maybe.withDefault -1 << Maybe.andThen (Just << .ataola) << Tuple.second) results

validScoresTupleList : List (Int, Float)
validScoresTupleList = Dict.filter (\k v -> k /= -1) resultDict
                           |> Dict.map (always <| toFloat << List.length)
                           |> Dict.toList

getMovesForPoints : Dict.Dict Int (List (String, Maybe Assets)) -> Int -> List String
getMovesForPoints dict points =
    Dict.get points dict |> Maybe.withDefault [] |> List.map Tuple.first

numIllegalMoves = List.length <| getMovesForPoints resultDict -1
numValidMoves = List.foldl ((+) << Tuple.second) 0 validScoresTupleList
