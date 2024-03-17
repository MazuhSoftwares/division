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
    { id : Int
    , quantity : Int
    , name : String
    , unitPrice : Float
    , personId : Int
    }


type alias Model =
    { people : List Person
    , nextPersonId : Int
    , amounts : List Amount
    , nextAmountId : Int
    }


init : Model
init =
    { people =
        [ { id = 1, name = "" }
        , { id = 1, name = "" }
        ]
    , nextPersonId = 3
    , amounts =
        [ { id = 1, quantity = 1, name = "", unitPrice = 0, personId = 0 }
        ]
    , nextAmountId = 2
    }


type Msg
    = AddPerson
    | RemovePerson Int
    | AddAmount
    | RemoveAmount Int


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
                    { id = currentPersonId, name = "" }

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

        AddAmount ->
            let
                currentAmountId =
                    model.nextAmountId

                nextAmountId =
                    model.nextAmountId + 1

                adding =
                    { id = currentAmountId, quantity = 1, name = "", unitPrice = 0, personId = 0 }

                updatedAmounts =
                    List.append model.amounts [ adding ]
            in
            { model | amounts = updatedAmounts, nextAmountId = nextAmountId }

        RemoveAmount id ->
            let
                updatedAmounts =
                    List.filter (\amount -> amount.id /= id) model.amounts
            in
            { model | amounts = updatedAmounts }


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


viewPersonItem : Person -> Html Msg
viewPersonItem person =
    let
        removeThisPerson =
            RemovePerson person.id
    in
    div []
        [ input [ type_ "text", value person.name, placeholder ("Person " ++ String.fromInt person.id) ] []
        , button [ onClick removeThisPerson ] [ text "-" ]
        ]


viewAmountItem : Amount -> Html Msg
viewAmountItem amount =
    let
        removeThisAmount =
            RemoveAmount amount.id
    in
    div []
        [ input [ type_ "number", value (String.fromFloat amount.unitPrice) ] []
        , text "x "
        , input [ type_ "text", value amount.name, placeholder ("Item " ++ String.fromInt amount.id) ] []
        , input [ type_ "number", value (String.fromInt amount.quantity) ] []
        , button [ onClick removeThisAmount ] [ text "-" ]
        ]


view : Model -> Html Msg
view model =
    main_ [ style "padding" "8px", style "margin" "auto", style "width" "100%", style "max-width" "500px" ]
        [ header []
            [ h1 [] [ text "Division" ]
            , p [] [ text "While sharing a payment, how much each people will pay?" ]
            ]
        , section []
            [ h2 [] [ text "People" ]
            , div []
                [ div [] (List.map viewPersonItem model.people)
                , button [ onClick AddPerson ] [ text "+" ]
                ]
            ]
        , section []
            [ h2 [] [ text "Amounts" ]
            , div []
                [ div [] (List.map viewAmountItem model.amounts)
                , button [ onClick AddAmount ] [ text "+" ]
                ]
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
