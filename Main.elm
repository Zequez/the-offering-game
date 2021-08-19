module OfferingGame.Main exposing (Model, Msg, main, view)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, header, input, main_, span, text)
import Html.Attributes exposing (class, classList, placeholder, style, title)
import Html.Events exposing (onClick)
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
    { currentUser : String
    , tab : Tab
    , participants : Dict String Participant
    , editingParticipant : Participant
    }


type alias Flags =
    {}


type Msg
    = Noop
    | ClickTab Tab


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
    ( { currentUser = "zequezzeuqez"
      , tab = OfferingsTab
      , participants =
            Dict.fromList
                [ ( "zequezzeuqez", zequez )
                ]
      , editingParticipant = blank
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ClickTab tab ->
            ( { model | tab = tab }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        currentUser : Maybe Participant
        currentUser =
            model.participants
                |> Dict.get model.currentUser
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
                                        [ offeringView (Offering "Web, apps, systems development and design" [ ( T.Language, "en" ), ( T.Currency, "seeds" ), ( T.OType, "service" ) ])
                                        , offeringView (Offering "Couch $200" [ ( T.Language, "en" ), ( T.Region, "mar-del-plata" ), ( T.Currency, "usd" ) ])
                                        , offeringView (Offering "Abrazo" [ ( T.Language, "es" ), ( T.Region, "mar-del-plata" ) ])
                                        , offeringView (Offering "10 Seeds for a picture of your cat" [ ( T.Language, "en" ), ( T.Currency, "seeds" ), ( T.OIs, "request" ) ])
                                        ]
                                    , div [ class "my-4 text-xl font-thin" ] [ text "Add" ]
                                    , div [ class "flex h-8 mb-2" ]
                                        [ input [ class "border border-gray-400 rounded-md flex-grow mr-4 px-2" ] []
                                        , button [ class "bg-green-500 rounded-md px-2 text-white uppercase font-bold cursor-pointer" ]
                                            [ text "Save" ]
                                        ]
                                    , div [ class "mb-4 flex" ]
                                        [ tagView T.Region "mar-del-plata"
                                        , tagView T.Region "argentina"
                                        ]
                                    , div [ class "" ]
                                        [ tagSelectView "Language" "🌐" "#cdcb4d" [ "es", "en", "pt" ]
                                        , tagSelectView "Region" "🌎" "lightblue" [ "mar-del-plata", "uruguay", "argentina" ]
                                        , tagSelectView "Currency" "💰" "#68cb97" [ "ars", "usd", "seeds" ]
                                        , tagSelectView "Type" "📄" "orange" [ "service", "product", "work", "housing" ]
                                        , tagSelectView "Is" "🤝" "gold" [ "offering", "requesting" ]
                                        ]
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


registrationView : Participant -> List (Html Msg)
registrationView participant =
    [ div [ class "text-xl font-thin mb-2" ] [ text "Registration" ]
    ]


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


tagSelectView : String -> String -> String -> List String -> Html Msg
tagSelectView tagName tagIcon tagColor tagOptions =
    div [ class "" ]
        [ div
            [ class "shadow-sm py-1 px-2 text-white rounded-md flex items-center"
            , style "background-color" tagColor
            , style "text-shadow" "0 1px 0 rgba(0,0,0,0.2)"
            ]
            [ div [ class "flex-grow" ] [ text (tagIcon ++ " " ++ tagName) ]
            , input [ class "w-40 ring-1 ring-black ring-opacity-10 rounded-sm", placeholder "Find / New" ] []
            ]
        , div [ class "inline-flex text-sm my-2" ]
            (tagOptions
                |> List.map (\o -> div [ class "px-1 cursor-pointer rounded-md hover:bg-gray-300" ] [ text o ])
            )
        ]


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
        [ class "inline-flex rounded-md mr-2 shadow-sm font-normal text-xs"
        , style "background-color" color
        , title name
        ]
        [ span [ class "bg-black bg-opacity-20 px-1 rounded-l-md" ] [ text emoji ]
        , span [ class "px-1" ] [ text tagText ]
        ]
