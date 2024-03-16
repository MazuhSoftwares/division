module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, footer, h1, h2, header, input, main_, p, section, text)
import Html.Attributes exposing (href, placeholder, style, target, type_, value)
import Html.Events exposing (onClick)


type alias Person =
    { id : Int
    , name : String
    }


type alias Amount =
    { quantity : Int
    , name : String
    , unitPrice : Float
    , personId : Int
    }


type alias Model =
    { people : List Person
    , nextPersonId : Int
    , amounts : List Amount
    }


init : Model
init =
    { people =
        [ Person 1 ""
        , Person 2 ""
        ]
    , nextPersonId = 2
    , amounts =
        [ Amount 1 "" 0 0
        ]
    }


type Msg
    = AddPerson
    | RemovePerson Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPerson ->
            let
                currentPersonId =
                    model.nextPersonId

                nextPersonId =
                    model.nextPersonId + 1

                adding =
                    Person currentPersonId ""

                updatedPeople =
                    List.append model.people [ adding ]
            in
            { model | people = updatedPeople, nextPersonId = nextPersonId }

        RemovePerson id ->
            let
                updatedPeople =
                    List.filter (\person -> person.id /= id) model.people
            in
            { model | people = updatedPeople }


main =
    Browser.sandbox { init = init, update = update, view = view }


viewPersonInput : Person -> Html Msg
viewPersonInput person =
    let
        removeThisPerson =
            RemovePerson person.id
    in
    div []
        [ input [ type_ "text", value person.name, placeholder ("Person " ++ String.fromInt person.id) ] []
        , button [ onClick removeThisPerson ] [ text "-" ]
        ]


view model =
    main_ [ style "padding" "8px", style "margin" "auto", style "width" "100%", style "max-width" "500px" ]
        [ header []
            [ h1 [] [ text "Division" ]
            , p [] [ text "While sharing a payment, how much each people will pay?" ]
            ]
        , section []
            [ h2 [] [ text "People" ]
            , div []
                [ div [] (List.map viewPersonInput model.people)
                , button [ onClick AddPerson ] [ text "+" ]
                ]
            ]
        , section []
            [ h2 [] [ text "Amounts" ]
            , p [] [ text "TODO." ]
            ]
        , section []
            [ h2 [] [ text "Results" ]
            , p [] [ text "TODO." ]
            ]
        , footer []
            [ text "It's "
            , a [ href "https://github.com/MazuhSoftwares/division", target "_blank" ] [ text "free source" ]
            , text "!"
            ]
        ]
