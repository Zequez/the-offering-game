module OfferingGame.Main exposing (Model, Msg, main, view)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, header, input, main_, span, text)
import Html.Attributes exposing (class, classList, placeholder, style, title, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as JD
import List.Extra
import OfferingGame.Lang as L exposing (Lang)
import OfferingGame.Participant exposing (ContactChannels, Offering, Participant, blank, zequez)
import OfferingGame.Tag as T exposing (Tag, toTagInfo)
import Set exposing (Set)


type Tab
    = OfferingsTab
    | ListingsTab
    | ParticipantsTab
    | AccountTab


type alias Model =
    { currentParticipant : String
    , tab : Tab
    , participants : Dict String Participant
    , editingParticipant : Participant
    , editingOffering : Offering
    , editingTagFindValues : Dict String String
    }



-- ██╗███╗   ██╗██╗████████╗
-- ██║████╗  ██║██║╚══██╔══╝
-- ██║██╔██╗ ██║██║   ██║
-- ██║██║╚██╗██║██║   ██║
-- ██║██║ ╚████║██║   ██║
-- ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> init flags
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentParticipant = "zequezzeuqez"
      , tab = OfferingsTab
      , participants =
            Dict.fromList
                [ ( "zequezzeuqez", zequez )
                ]
      , editingParticipant = blank
      , editingOffering = { description = "", tags = [] }
      , editingTagFindValues = Dict.fromList []
      }
    , Cmd.none
    )



-- ██╗   ██╗██████╗ ██████╗  █████╗ ████████╗███████╗
-- ██║   ██║██╔══██╗██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
-- ██║   ██║██████╔╝██║  ██║███████║   ██║   █████╗
-- ██║   ██║██╔═══╝ ██║  ██║██╔══██║   ██║   ██╔══╝
-- ╚██████╔╝██║     ██████╔╝██║  ██║   ██║   ███████╗
--  ╚═════╝ ╚═╝     ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚══════╝


type Msg
    = Noop
    | ClickTab Tab
    | EditAddTag Tag String
    | EditRemoveTag Int
    | EditSetDescription String
    | EditSubmitOffering
    | EditTagFindInput Tag String



-- | EditRemoveTag Tag String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ClickTab tab ->
            ( { model | tab = tab }, Cmd.none )

        EditAddTag tag value ->
            updateOffering
                model
                (\o -> { o | tags = ( tag, value ) :: o.tags })

        EditRemoveTag tagIndex ->
            updateOffering
                model
                (\o -> { o | tags = List.Extra.removeAt tagIndex o.tags })

        EditSetDescription value ->
            updateOffering model (\o -> { o | description = value })

        EditSubmitOffering ->
            case Dict.get model.currentParticipant model.participants of
                Just participant ->
                    let
                        { offerings } =
                            participant

                        editingOffering =
                            model.editingOffering

                        description =
                            String.trim editingOffering.description
                    in
                    ( if description /= "" && not (List.isEmpty model.editingOffering.tags) then
                        { model
                            | editingOffering = { editingOffering | description = "" }
                            , participants =
                                model.participants
                                    |> Dict.insert model.currentParticipant
                                        { participant
                                            | offerings =
                                                model.editingOffering :: offerings
                                        }
                        }

                      else
                        model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EditTagFindInput tag value ->
            let
                { name } =
                    toTagInfo tag
            in
            ( { model
                | editingTagFindValues =
                    model.editingTagFindValues
                        |> Dict.insert name value
              }
            , Cmd.none
            )



-- getCurrentUser String -> Dict String Participant -> Maybe


updateOffering : Model -> (Offering -> Offering) -> ( Model, Cmd Msg )
updateOffering model updateFun =
    ( { model | editingOffering = updateFun model.editingOffering }, Cmd.none )



-- ██╗   ██╗██╗███████╗██╗    ██╗███████╗
-- ██║   ██║██║██╔════╝██║    ██║██╔════╝
-- ██║   ██║██║█████╗  ██║ █╗ ██║███████╗
-- ╚██╗ ██╔╝██║██╔══╝  ██║███╗██║╚════██║
--  ╚████╔╝ ██║███████╗╚███╔███╔╝███████║
--   ╚═══╝  ╚═╝╚══════╝ ╚══╝╚══╝ ╚══════╝


view : Model -> Html Msg
view model =
    let
        currentUser : Maybe Participant
        currentUser =
            model.participants
                |> Dict.get model.currentParticipant
    in
    div []
        [ header [ class "bg-green-500 text-white py-4 font-thin" ]
            [ div [ class "max-w-screen-lg mx-auto px-4" ]
                [ div [ class "text-3xl" ]
                    [ text "Offering game" ]
                , div [ class "text-sm" ]
                    [ text "A space for sharing" ]
                ]
            ]
        , main_ [ class "" ]
            [ div [ class "h-12 bg-gray-200" ]
                [ div [ class "max-w-screen-lg mx-auto px-4 flex h-full uppercase" ]
                    [ tabView "Listings" model.tab ListingsTab
                    , tabView "Participants" model.tab ParticipantsTab
                    , tabView "My announcements" model.tab OfferingsTab
                    , tabView "My account" model.tab AccountTab
                    ]
                ]
            , div [ class "max-w-screen-lg mx-auto p-4" ]
                [ div [ class "bg-white rounded-md shadow-md p-4" ]
                    (case model.tab of
                        OfferingsTab ->
                            case currentUser of
                                Just participant ->
                                    [ div [ class "text-xl font-thin mb-2" ]
                                        [ text "My announcements" ]
                                    , div [ class "font-thin" ]
                                        (participant.offerings
                                            |> List.map (\o -> offeringView o)
                                        )
                                    , viewOfferingEditor model.editingOffering
                                    ]

                                Nothing ->
                                    registrationView model.editingParticipant

                        AccountTab ->
                            case currentUser of
                                Just participant ->
                                    [ div [ class "text-xl font-thin mb-2" ] [ text "My account" ]
                                    ]

                                Nothing ->
                                    registrationView model.editingParticipant

                        ParticipantsTab ->
                            [ div [ class "text-xl font-thin" ]
                                [ text "Participants" ]
                            ]

                        ListingsTab ->
                            [ div [ class "text-xl font-thin" ]
                                [ text "Listings" ]
                            ]
                    )
                ]
            ]
        ]



-- Offering Editor


viewOfferingEditor : Offering -> Html Msg
viewOfferingEditor { description, tags } =
    div []
        [ div [ class "my-4 text-xl font-thin" ] [ text "Add" ]
        , div [ class "flex h-8 mb-2" ]
            [ input
                [ class "border border-gray-400 rounded-md flex-grow mr-4 px-2"
                , onInput EditSetDescription
                , onEnter EditSubmitOffering
                , value description
                ]
                []
            , button
                [ class "bg-green-500 rounded-md px-2 text-white uppercase font-bold cursor-pointer"
                , onClick EditSubmitOffering
                ]
                [ text "Save" ]
            ]
        , div [ class "mb-4 flex" ]
            (tags |> List.indexedMap (\i ( tag, value ) -> viewOfferingEditorTag tag value i))
        , div [ class "" ]
            [ viewTagSelector T.Language [ "es", "en", "pt" ]
            , viewTagSelector T.Region [ "mar-del-plata", "uruguay", "argentina" ]
            , viewTagSelector T.Currency [ "ars", "usd", "seeds" ]
            , viewTagSelector T.OType [ "service", "product", "work", "housing" ]
            , viewTagSelector T.OIs [ "offering", "requesting" ]
            ]
        ]



-- viewOfferingEditorInput : Offering -> Html Msg
-- viewOfferingEditorInput offering =


viewOfferingEditorTag : Tag -> String -> Int -> Html Msg
viewOfferingEditorTag tag value index =
    button
        [ class "block bg-transparent p-0 cursor-pointer"
        , onClick (EditRemoveTag index)
        ]
        [ tagView tag value
        ]


viewTagSelector : Tag -> List String -> Html Msg
viewTagSelector tag tagOptions =
    let
        { name, color, emoji } =
            toTagInfo tag
    in
    div [ class "" ]
        [ div
            [ class "shadow-sm py-1 px-2 text-white rounded-md flex items-center"
            , style "background-color" color
            , style "text-shadow" "0 1px 0 rgba(0,0,0,0.2)"
            ]
            [ div [ class "flex-grow" ] [ text (emoji ++ " " ++ name) ]
            , input
                [ class "w-40 ring-1 ring-black ring-opacity-10 rounded-sm"
                , placeholder "Find / New"
                , onInput (EditTagFindInput tag)
                ]
                []
            ]
        , div [ class "inline-flex text-sm my-2" ]
            (tagOptions
                |> List.map (\value -> viewTagSelectorOption tag value)
            )
        ]


viewTagSelectorOption : Tag -> String -> Html Msg
viewTagSelectorOption tag value =
    button
        [ class "block mr-1 px-1 bg-transparent cursor-pointer rounded-md hover:bg-gray-300"
        , onClick (EditAddTag tag value)
        ]
        [ text value ]



-- Registration


registrationView : Participant -> List (Html Msg)
registrationView participant =
    [ div [ class "text-xl font-thin mb-2" ] [ text "Registration" ]
    ]



-- Layout


tabView : String -> Tab -> Tab -> Html Msg
tabView txt activeTab onClickTab =
    let
        isActive =
            activeTab == onClickTab
    in
    button
        [ class "uppercase px-4 pt-1 tracking-wide bg-transparent border-b-3  border-transparent cursor-pointer"
        , classList
            [ ( "bg-gray-100 border-green-500"
              , isActive
              )
            , ( "hover:bg-gray-100 hover:border-gray-300", not isActive )
            ]
        , onClick (ClickTab onClickTab)
        ]
        [ text txt ]



-- Offering list item


offeringView : Offering -> Html Msg
offeringView { description, tags } =
    div [ class "border-b-1 border-gray-300 last:border-0 border-dashed text-gray-800 py-2 text-sm rounded-sm flex flex-wrap" ]
        [ div [ class "mb-1 flex-grow" ] [ text description ]
        , div [ class "" ]
            (tags
                |> List.map (\( tag, tagTxt ) -> tagView tag tagTxt)
            )
        ]


tagView : Tag -> String -> Html Msg
tagView tag tagText =
    let
        { name, color, emoji } =
            toTagInfo tag
    in
    div
        [ class "inline-flex rounded-md shadow-sm font-normal text-xs"
        , style "background-color" color
        , title name
        ]
        [ span [ class "bg-black bg-opacity-20 px-1 rounded-l-md" ] [ text emoji ]
        , span [ class "px-1" ] [ text tagText ]
        ]



-- ██╗  ██╗███████╗██╗     ██████╗ ███████╗██████╗ ███████╗
-- ██║  ██║██╔════╝██║     ██╔══██╗██╔════╝██╔══██╗██╔════╝
-- ███████║█████╗  ██║     ██████╔╝█████╗  ██████╔╝███████╗
-- ██╔══██║██╔══╝  ██║     ██╔═══╝ ██╔══╝  ██╔══██╗╚════██║
-- ██║  ██║███████╗███████╗██║     ███████╗██║  ██║███████║
-- ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚══════╝╚═╝  ╚═╝╚══════╝


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                JD.succeed msg

            else
                JD.fail "not ENTER"
    in
    on "keydown" (JD.andThen isEnter keyCode)
