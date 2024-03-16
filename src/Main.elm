module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (a, button, div, footer, h1, h2, header, main_, p, section, text)
import Html.Attributes exposing (href, style, target)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    main_ [ style "padding" "8px", style "margin" "auto", style "width" "100%", style "max-width" "500px" ]
        [ header []
            [ h1 [] [ text "Division" ]
            , p [] [ text "While sharing a payment, how much each people will pay?" ]
            ]
        , section []
            [ h2 [] [ text "People" ]
            , p [] [ text "TODO." ]
            ]
        , section []
            [ h2 [] [ text "Amounts" ]
            , p [] [ text "TODO." ]
            ]
        , section []
            [ h2 [] [ text "Results" ]
            , p [] [ text "TODO." ]
            ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        , footer []
            [ text "It's "
            , a [ href "https://github.com/MazuhSoftwares/division", target "_blank" ] [ text "free source" ]
            , text "!"
            ]
        ]
