module OfferingGame.Main exposing (Model, Msg, main, view)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, header, img, input, label, main_, span, text)
import Html.Attributes exposing (checked, class, classList, href, placeholder, src, style, target, title, type_, value)
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
    | ConnectedTab


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
    | ParticipantSet



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

        ParticipantSet ->
            ( model, Cmd.none )



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
    div [ class "text-gray-800" ]
        [ header [ class "bg-green-500 text-white py-4 font-thin" ]
            [ div [ class "max-w-screen-lg mx-auto px-4 text-shadow-1" ]
                [ div [ class "text-3xl" ]
                    [ text "The Offering Game" ]
                , a
                    [ class "text-sm text-white"
                    , href "https://zequez.notion.site/The-Offering-Game-1f5029a9e4234359905b97ace98c8d1e"
                    , target "_blank"
                    ]
                    [ text "📖 Read the game guide" ]
                ]
            ]
        , main_ [ class "pb-12" ]
            [ div [ class "fixed bottom-0 w-full h-12 bg-gray-200 border-t-1 border-gray-300" ]
                [ div [ class "flex justify-center h-full uppercase" ]
                    [ tabView "🔎" model.tab ListingsTab
                    , tabView "👥" model.tab ParticipantsTab
                    , tabView "🎁" model.tab OfferingsTab
                    , tabView "🤝" model.tab ConnectedTab
                    , tabView "🧙" model.tab AccountTab
                    ]
                ]
            , div [ class "" ]
                (case model.tab of
                    OfferingsTab ->
                        case currentUser of
                            Just participant ->
                                [ viewTabTitle "My announcements"
                                , viewTabContent
                                    [ if List.isEmpty participant.offerings then
                                        div [ class "font-thin opacity-30 text-2xl text-center" ] [ text "You've got no announcements" ]

                                      else
                                        div [ class "font-thin" ]
                                            (participant.offerings
                                                |> List.map (\o -> offeringView o)
                                            )
                                    , viewOfferingEditor model.editingOffering
                                    ]
                                ]

                            Nothing ->
                                registrationView model.editingParticipant

                    AccountTab ->
                        case currentUser of
                            Just participant ->
                                [ viewTabTitle participant.name
                                , viewTabContent [ viewParticipant participant ]
                                ]

                            Nothing ->
                                registrationView model.editingParticipant

                    ParticipantsTab ->
                        [ viewTabTitle "Participants"
                        , viewTabContent
                            (Dict.values model.participants |> List.map viewParticipantHeader)
                        ]

                    ListingsTab ->
                        [ viewTabTitle "Listings"
                        , viewTabContent (viewListing (Dict.values model.participants))
                        ]

                    ConnectedTab ->
                        [ viewTabTitle "Connected", viewTabContent [] ]
                )
            ]
        ]



-- Layout


viewTabTitle : String -> Html Msg
viewTabTitle tabTitle =
    div [ class "bg-green-400 " ]
        [ div
            [ class "max-w-screen-lg mx-auto text-white font-light pl-4 py-2 text-xl text-shadow-1"
            ]
            [ text tabTitle ]
        ]


viewTabContent : List (Html Msg) -> Html Msg
viewTabContent children =
    div [ class "max-w-screen-lg mx-auto" ]
        [ div [ class "bg-white rounded-md shadow-md p-4 m-4 " ] children
        ]



-- Participant


viewParticipant : Participant -> Html Msg
viewParticipant participant =
    div []
        [ viewInput "Seeds Account" participant.seedsAccountName ParticipantSet
        , viewInput "Name" participant.name ParticipantSet
        , viewCheckboxInput "Is Organization" participant.isOrganization ParticipantSet
        , viewInput "Contact name" participant.contactName ParticipantSet
        , viewInput "Description" participant.description ParticipantSet
        , viewInput "URL" participant.url ParticipantSet
        , viewCheckboxInput "Accepts Donations" participant.acceptsDonations ParticipantSet
        , viewFormSection "Contact channels"
        , viewInput "Discord" participant.discord ParticipantSet
        , viewInput "Email" participant.email ParticipantSet
        , viewInput "Phone" participant.phone ParticipantSet
        , viewInput "Telegram" participant.telegram ParticipantSet
        , viewInput "Whatsapp" participant.whatsapp ParticipantSet
        , viewInput "Instagram" participant.instagram ParticipantSet
        , viewFormSection "Images"
        , viewInput "Payment QR" participant.paymentQr ParticipantSet
        , viewInput "Logo" participant.logo ParticipantSet
        , viewFormSection "Location"
        , viewInput "Country" participant.country ParticipantSet
        , viewInput "Region" participant.region ParticipantSet
        , viewInput "City" participant.city ParticipantSet
        ]


viewFormSection : String -> Html Msg
viewFormSection title =
    div [ class "mb-4 text-xl text-center font-light" ] [ text title ]


viewCheckboxInput : String -> Bool -> Msg -> Html Msg
viewCheckboxInput labl val onChange =
    label [ class "flex mb-4" ]
        [ div [ class "w-40 flex items-center justify-end pr-4 text-xs leading-tight text-right" ]
            [ text labl
            ]
        , div [ class "w-80 " ]
            [ input
                [ checked val
                , type_ "checkbox"
                , class """h-10 w-10 m-0 bg-gray-100 rounded-md shadow-inner
            border-1 border-gray-200
            focus:(ring-3 ring-green-500 outline-none)"""
                ]
                []
            ]
        ]


viewInput : String -> String -> Msg -> Html Msg
viewInput label val onChange =
    div [ class "flex mb-4" ]
        [ div [ class "w-40 flex items-center justify-end pr-4 text-xs leading-tight text-right" ] [ text label ]
        , input
            [ value val
            , class """h-10 w-80 px-4 bg-gray-100 rounded-md shadow-inner
            border-1 border-gray-200 text-base text-gray-700
            focus:(ring-3 ring-green-500 outline-none)"""
            ]
            []
        ]



-- Listing


viewListing : List Participant -> List (Html Msg)
viewListing participants =
    participants
        |> List.map viewListingParticipant


viewParticipantHeader : Participant -> Html Msg
viewParticipantHeader { name, seedsAccountName, logo, paymentQr } =
    div [ class "flex flex-wrap items-center bg-gray-100 p-1 rounded-l-[26px] rounded-r-[18px]" ]
        [ div [ class "flex items-center" ]
            [ img [ src logo, class "h-12 rounded-full mr-4 shadow-sm" ]
                []
            , div [ class "" ]
                [ div [ class "font-thin mr-2" ] [ text name ]
                , div [ class "font-thin text-gray-400 text-sm -mt-1" ] [ text seedsAccountName ]
                ]
            ]
        , div [ class "flex flex-grow justify-end" ]
            [ button [ class """
                flex items-center h-8  p-0 pr-3 mr-2
                rounded-full bg-green-500
                text-white uppercase font-bold
                cursor-pointer hover:bg-green-400
            """ ]
                [ img [ src paymentQr, class "h-6 mr-2 ml-1 rounded-full" ] []
                , text "Give"
                ]
            , button
                [ class """
                flex items-center h-8  p-0 pr-3
                rounded-full bg-green-500
                text-white uppercase font-bold
                cursor-pointer hover:bg-green-400
            """ ]
                [ span [ class "text-2xl mx-2" ] [ text "🤝" ]

                -- img [ src paymentQr, class "h-6 mr-2 ml-1 rounded-full" ] []
                , text "Connect"
                ]
            ]
        ]


viewListingParticipant : Participant -> Html Msg
viewListingParticipant participant =
    div []
        [ viewParticipantHeader participant
        , div [ class "py-2" ]
            (participant.offerings
                |> List.map (\o -> offeringView o)
            )
        ]



-- Offering Editor


viewOfferingEditor : Offering -> Html Msg
viewOfferingEditor { description, tags } =
    div []
        [ div [ class "my-4 text-xl font-thin" ] [ text "Add new" ]
        , div [ class "flex h-8 mb-2" ]
            [ input
                [ class "border border-gray-400 text-lg font-light rounded-md flex-grow mr-4 px-2 focus:ring-3 ring-green-500 outline-none"
                , onInput EditSetDescription
                , onEnter EditSubmitOffering
                , value description
                ]
                []
            , button
                [ class "bg-green-500 rounded-md px-2 text-white uppercase font-bold cursor-pointer focus:ring-3 ring-green-600 outline-none"
                , onClick EditSubmitOffering
                ]
                [ text "Save" ]
            ]
        , div [ class "mb-4 flex" ]
            (tags |> List.indexedMap (\i ( tag, value ) -> viewOfferingEditorTag tag value i))
        , div [ class "my-4 text-xl font-thin" ] [ text "Pick tags" ]
        , div [ class "mb-4" ]
            [ viewTagSelector T.Language [ "es", "en", "pt" ]
            , viewTagSelector T.Region [ "mar-del-plata", "uruguay", "argentina" ]
            , viewTagSelector T.Currency [ "ars", "usd", "seeds" ]
            , viewTagSelector T.OType [ "service", "product", "work", "housing" ]
            , viewTagSelector T.OIs [ "offering", "requesting" ]
            ]
        , div [ class "text-right" ]
            [ a
                [ class """
                    my-4 px-4 py-2
                    text-white font-bold font-thin text-sm text-right uppercase no-underline
                    rounded-lg bg-green-500
                """
                , target "_blank"
                , href "https://github.com/Zequez/the-offering-game/issues/new?title=New%20tag%20type"
                ]
                [ text "Request new tag type ›" ]
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
            [ class "shadow-sm py-1 px-2 text-white rounded-md flex items-center text-shadow-1"
            , style "background-color" color
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
    [ viewTabTitle "Registration", viewTabContent [] ]



-- Layout


tabView : String -> Tab -> Tab -> Html Msg
tabView txt activeTab onClickTab =
    let
        isActive =
            activeTab == onClickTab
    in
    button
        [ class "uppercase px-4 pt-1 text-2xl tracking-wide bg-transparent border-b-3  border-transparent cursor-pointer"
        , classList
            [ ( "bg-gray-50 border-green-500 shadow-md"
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
    div [ class """
        flex flex-wrap items-center px-2 py-1
        text-sm  rounded-full
        hover:bg-gray-100
        cursor-pointer
    """ ]
        [ div [ class "flex-grow" ] [ text description ]
        , div [ class "flex flex-wrap" ]
            (tags
                |> List.map (\( tag, tagTxt ) -> span [ class "ml-1" ] [ tagView tag tagTxt ])
            )
        ]


tagView : Tag -> String -> Html Msg
tagView tag tagText =
    let
        { name, color, emoji } =
            toTagInfo tag
    in
    div
        [ class "flex rounded-md shadow-sm font-normal text-xs"
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
