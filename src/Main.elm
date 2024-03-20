module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, em, footer, h1, h2, header, input, label, li, main_, option, p, section, select, span, text, ul)
import Html.Attributes exposing (for, href, id, maxlength, placeholder, step, style, target, title, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


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
        [ { id = 1, quantity = 1, name = "", unitPrice = 0.0, personId = 0 }
        , { id = 2, quantity = 1, name = "", unitPrice = 0.0, personId = 0 }
        , { id = 3, quantity = 1, name = "", unitPrice = 0.0, personId = 0 }
        ]
    , nextAmountId = 4
    }


type Msg
    = AddPerson
    | UpdatePerson Person
    | RemovePerson Int
    | AddAmount
    | UpdateAmount Amount
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

        UpdatePerson updatingPerson ->
            let
                updatedPeople =
                    model.people
                        |> List.map
                            (\existing ->
                                if existing.id == updatingPerson.id then
                                    updatingPerson

                                else
                                    existing
                            )
            in
            { model | people = updatedPeople }

        RemovePerson removingPersonId ->
            let
                updatedPeople =
                    List.filter (\person -> person.id /= removingPersonId) model.people

                updatedAmounts =
                    model.amounts
                        |> List.map
                            (\amount ->
                                if amount.personId == removingPersonId then
                                    { amount | personId = 0 }

                                else
                                    amount
                            )
            in
            { model | people = updatedPeople, amounts = updatedAmounts }

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

        UpdateAmount updatingAmount ->
            let
                updatedAmounts =
                    model.amounts
                        |> List.map
                            (\existing ->
                                if existing.id == updatingAmount.id then
                                    updatingAmount

                                else
                                    existing
                            )
            in
            { model | amounts = updatedAmounts }

        RemoveAmount removingId ->
            let
                updatedAmounts =
                    List.filter (\existing -> existing.id /= removingId) model.amounts
            in
            { model | amounts = updatedAmounts }


view : Model -> Html Msg
view model =
    main_
        [ style "box-sizing" "border-box"
        , style "padding" "8px"
        , style "padding-right" "16px"
        , style "margin" "auto"
        , style "width" "100vw"
        , style "max-width" "500px"
        , style "overflow" "hidden"
        ]
        [ header [ id "header" ]
            [ h1 []
                [ text "Division" ]
            , p []
                [ text "While sharing a payment, how much each people will pay?" ]
            ]
        , section [ id "people" ]
            [ h2 []
                [ text "People" ]
            , p []
                [ text "Add how many people is participating. Names are all optional." ]
            , div []
                [ div []
                    (List.map viewPersonItem model.people)
                , button
                    [ onClick AddPerson
                    , title "Add a new person"
                    , style "display" "block"
                    , style "padding-left" "30px"
                    , style "padding-right" "30px"
                    , style "margin-left" "auto"
                    ]
                    [ text "+" ]
                , p
                    [ style "text-align" "right"
                    ]
                    [ text "Go "
                    , a [ href "#amounts" ]
                        [ text "to amounts (next)" ]
                    , text " or "
                    , a [ href "#header" ]
                        [ text "to the top" ]
                    ]
                ]
            ]
        , section [ id "amounts" ]
            [ h2 []
                [ text "Amounts" ]
            , p []
                [ text "How much each item costs for these people. Naming is also optional." ]
            , div []
                [ div [] (List.map (\amount -> viewAmountItem model amount) model.amounts)
                , button
                    [ onClick AddAmount
                    , title "Add a new item on amounts"
                    , style "display" "block"
                    , style "padding-left" "30px"
                    , style "padding-right" "30px"
                    , style "margin-left" "auto"
                    ]
                    [ text "+" ]
                , p
                    [ style "text-align" "right"
                    ]
                    [ text "Go "
                    , a [ href "#amounts" ]
                        [ text "see results (next)" ]
                    , text " or "
                    , a [ href "#header" ]
                        [ text "to the top" ]
                    ]
                ]
            ]
        , section [ id "results" ]
            [ h2 [] [ text "Results" ]
            , viewResultsList model
            ]
        , footer
            [ id "footer"
            , style "margin-top" "50px"
            , style "margin-bottom" "30px"
            ]
            [ text "It's "
            , a
                [ href "https://github.com/MazuhSoftwares/division"
                , target "_blank"
                ]
                [ text "free source" ]
            , text "!"
            ]
        ]


viewPersonItem : Person -> Html Msg
viewPersonItem person =
    let
        removeThisPerson =
            RemovePerson person.id

        handleNameInput name =
            UpdatePerson { person | name = name }
    in
    div
        [ style "display" "flex"
        , style "margin-bottom" "8px"
        ]
        [ input
            [ id ("person_name_input_" ++ String.fromInt person.id)
            , type_ "text"
            , value person.name
            , onInput handleNameInput
            , placeholder (personNamePlaceholder person)
            , maxlength 16
            , style "margin-right" "8px"
            , style "flex-grow" "1"
            ]
            []
        , button
            [ onClick removeThisPerson
            , title "Remove this person"
            , style "flex-shrink" "0"
            ]
            [ text "−" ]
        ]


viewAmountItem : Model -> Amount -> Html Msg
viewAmountItem model amount =
    let
        removeThisAmount =
            RemoveAmount amount.id

        handleQuantityInput quantityStr =
            quantityStr
                |> String.toFloat
                |> Maybe.withDefault 0
                |> (\quantity -> UpdateAmount { amount | quantity = floor quantity })

        handleNameInput name =
            UpdateAmount { amount | name = name }

        handleUnitPriceInput unitPriceStr =
            unitPriceStr
                |> String.toFloat
                |> Maybe.withDefault 0
                |> (\unitPrice -> UpdateAmount { amount | unitPrice = unitPrice })

        emptyPersonOption =
            { value = "0", text = "Nobody" }

        nonEmptyPeopleOptions =
            model.people
                |> List.map
                    (\person ->
                        { value = String.fromInt person.id, text = personNameWithDefault person }
                    )

        peopleOptions =
            emptyPersonOption :: nonEmptyPeopleOptions

        selectedPersonOption =
            String.fromInt amount.personId

        handlePersonInput personIdStr =
            personIdStr
                |> String.toInt
                |> Maybe.withDefault 0
                |> (\personId -> UpdateAmount { amount | personId = personId })
    in
    div
        [ style "display" "flex"
        , style "margin-bottom" "8px"
        ]
        [ input
            [ id ("amount_quantity_input_" ++ String.fromInt amount.id)
            , type_ "number"
            , value (String.fromInt amount.quantity)
            , onInput handleQuantityInput
            , step "1"
            , Html.Attributes.min "0"
            , Html.Attributes.max "99"
            , style "flex-shrink" "0"
            ]
            []
        , label
            [ for ("amount_quantity_input_" ++ String.fromInt amount.id)
            , style "margin-right" "8px"
            ]
            [ text "x" ]
        , input
            [ id ("amount_name_input_" ++ String.fromInt amount.id)
            , type_ "text"
            , value amount.name
            , onInput handleNameInput
            , placeholder ("Item " ++ String.fromInt amount.id)
            , maxlength 16
            , style "margin-right" "8px"
            , style "flex-grow" "1"
            ]
            []
        , label [ for ("amount_unit_price_input_" ++ String.fromInt amount.id) ]
            [ text "$" ]
        , input
            [ id ("amount_unit_price_input_" ++ String.fromInt amount.id)
            , type_ "number"
            , value (String.fromFloat amount.unitPrice)
            , onInput handleUnitPriceInput
            , placeholder "0.00"
            , step "0.01"
            , Html.Attributes.min "0"
            , Html.Attributes.max "99999.99"
            , style "margin-right" "8px"
            , style "flex-shrink" "0"
            ]
            []
        , select
            [ id ("amount_person_input_" ++ String.fromInt amount.id)
            , value selectedPersonOption
            , onInput handlePersonInput
            , style "width" "90px"
            , style "margin-right" "8px"
            , style "font-style"
                (if amount.personId == 0 then
                    "italic"

                 else
                    "normal"
                )
            ]
            (List.map
                (\person ->
                    option
                        [ value person.value ]
                        [ text person.text ]
                )
                peopleOptions
            )
        , button
            [ onClick removeThisAmount
            , title "Remove this item from amounts"
            , style "flex-shrink" "0"
            ]
            [ text "−" ]
        ]


viewResultsList : Model -> Html Msg
viewResultsList model =
    let
        totalWithCurrency =
            model.amounts
                |> List.map
                    (\amount ->
                        { quantity = amount.quantity
                        , priceInCents = priceToCents amount.unitPrice
                        }
                    )
                |> List.map
                    (\amount -> amount.quantity * amount.priceInCents)
                |> List.sum
                |> centsToPriceWithCurrency

        personTotalInCents personId =
            model.amounts
                |> List.filter (\amount -> amount.personId == personId)
                |> List.map
                    (\amount ->
                        { quantity = amount.quantity
                        , priceInCents = priceToCents amount.unitPrice
                        }
                    )
                |> List.map
                    (\amount -> amount.quantity * amount.priceInCents)
                |> List.sum

        peopleResults =
            ({ id = 0, name = "" } :: model.people)
                |> List.map
                    (\person ->
                        { id = person.id
                        , nameWithDefault = personNameWithDefault person
                        , totalInCents = personTotalInCents person.id
                        }
                    )
                |> List.filter (\it -> it.id /= 0 || (it.id == 0 && it.totalInCents > 0))
                |> List.map
                    (\it ->
                        { id = it.id
                        , nameWithDefault = it.nameWithDefault
                        , totalWithCurrency = centsToPriceWithCurrency it.totalInCents
                        }
                    )
    in
    ul
        []
        [ li []
            [ text ("Total: " ++ totalWithCurrency)
            , ul []
                (List.map
                    (\result ->
                        if result.id == 0 then
                            li []
                                [ em [] [ text "Nobody: " ]
                                , span [] [ text result.totalWithCurrency ]
                                ]

                        else
                            li []
                                [ text (result.nameWithDefault ++ ": " ++ result.totalWithCurrency)
                                ]
                    )
                    peopleResults
                )
            ]
        ]


priceToCents : Float -> Int
priceToCents amount =
    floor (amount * 100)


centsToPriceString : Int -> String
centsToPriceString cents =
    let
        whole =
            cents // 100

        whileStr =
            String.fromInt whole

        fractional =
            cents - (whole * 100)

        fractionalStr =
            if fractional < 10 then
                "0" ++ String.fromInt fractional

            else
                String.fromInt fractional
    in
    whileStr ++ "." ++ fractionalStr


centsToPriceWithCurrency : Int -> String
centsToPriceWithCurrency cents =
    "$ " ++ centsToPriceString cents


personNameWithDefault : Person -> String
personNameWithDefault person =
    if String.isEmpty person.name then
        personNamePlaceholder person

    else
        person.name


personNamePlaceholder : Person -> String
personNamePlaceholder person =
    "Person " ++ String.fromInt person.id
