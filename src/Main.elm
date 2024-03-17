module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, footer, h1, h2, header, input, label, main_, p, section, text)
import Html.Attributes exposing (for, href, id, placeholder, step, style, target, type_, value)
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
        , { id = 2, name = "" }
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
        [ input
            [ type_ "text"
            , value person.name
            , placeholder ("Person " ++ String.fromInt person.id)
            , style "margin-right" "8px"
            ]
            []
        , button
            [ onClick removeThisPerson
            , style "margin-left" "4px"
            ]
            [ text "−" ]
        ]


viewAmountItem : Amount -> Html Msg
viewAmountItem amount =
    let
        removeThisAmount =
            RemoveAmount amount.id
    in
    div []
        [ input
            [ id ("quantity_input_" ++ String.fromInt amount.id)
            , type_ "number"
            , value (String.fromInt amount.quantity)
            , step "1"
            , Html.Attributes.min "0"
            , Html.Attributes.max "99"
            ]
            []
        , label
            [ for ("quantity_input_" ++ String.fromInt amount.id), style "margin-right" "8px" ]
            [ text "x" ]
        , input
            [ type_ "text"
            , value amount.name
            , placeholder ("Item " ++ String.fromInt amount.id)
            , style "margin-right" "8px"
            ]
            []
        , label [ for ("unit_price_input_" ++ String.fromInt amount.id) ]
            [ text "$" ]
        , input
            [ id ("unit_price_input_" ++ String.fromInt amount.id)
            , type_ "number"
            , value (String.fromFloat amount.unitPrice)
            , placeholder "0.00"
            , step "0.01"
            , Html.Attributes.min "0"
            , Html.Attributes.max "999999.99"
            , style "margin-right" "8px"
            ]
            []
        , button
            [ onClick removeThisAmount
            , style "margin-left" "4px"
            ]
            [ text "−" ]
        ]


view : Model -> Html Msg
view model =
    main_
        [ style "padding" "8px"
        , style "margin" "auto"
        , style "box-sizing" "border-box"
        , style "width" "100vw"
        , style "max-width" "500px"
        , style "overflow" "hidden"
        ]
        [ header []
            [ h1 [] [ text "Division" ]
            , p [] [ text "While sharing a payment, how much each people will pay?" ]
            ]
        , section []
            [ h2 [] [ text "People" ]
            , div []
                [ div [] (List.map viewPersonItem model.people)
                , button
                    [ onClick AddPerson
                    , style "margin-top" "8px"
                    ]
                    [ text "+" ]
                ]
            ]
        , section []
            [ h2 [] [ text "Amounts" ]
            , div []
                [ div [] (List.map viewAmountItem model.amounts)
                , button
                    [ onClick AddAmount
                    , style "margin-top" "8px"
                    ]
                    [ text "+" ]
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
