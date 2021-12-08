-- modified from the example https://elm-visualization.netlify.app/barchart/
module BarChart exposing (chart)

import Common exposing (Msg(..))
import Html.Events exposing (onClick)

import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import TypedSvg exposing (g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))

w : Float
w =
    800


h : Float
h =
    600


padding : Float
padding =
    30


xScale : List ( Int, Float ) -> BandScale Int
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - 2 * padding, 0 ) ( 0, 140 )


xAxis : List ( Int, Float ) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable String.fromInt (xScale model))


yAxis : Svg msg
yAxis =
    Axis.left [ Axis.tickCount 5 ] yScale


column : BandScale Int -> ( Int, Float ) -> Svg Msg
column scale ( key, value ) =
    g [ class [ "column" ], onClick (Select key) ]
        [ rect
            [ x <| Scale.convert scale key
            , y <| Scale.convert yScale value
            , width <| Scale.bandwidth scale
            , height <| h - Scale.convert yScale value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable String.fromInt scale) key
            , y <| Scale.convert yScale value - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat value ]
        ]


chart : List ( Int, Float ) -> Svg Msg
chart model =
    svg [ viewBox 0 0 w h ]
        [ style [] [ text """
            .column rect { fill: rgba(118, 214, 78, 0.8); }
            .column text { display: none; }
            .column:hover rect { fill: rgb(118, 214, 78); }
            .column:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale model)) model
        ]
