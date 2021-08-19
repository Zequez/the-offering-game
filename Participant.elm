module OfferingGame.Participant exposing (ContactChannels, Offering, Participant, blank, zequez)

import OfferingGame.Lang as L exposing (Lang)
import OfferingGame.Tag as T exposing (Tag, toTagInfo)


type alias Participant =
    -- Info
    { seedsAccountName : String
    , name : String
    , contactName : String
    , isOrganization : Bool
    , description : String
    , url : String
    , acceptsDonations : Bool

    -- Contact channels
    , discord : String
    , email : String
    , phone : String
    , telegram : String
    , whatsapp : String
    , instagram : String
    , preferredContact : List ContactChannels

    -- Images
    , paymentQr : String
    , logo : String

    -- Location
    , languages : List Lang
    , country : String
    , region : String
    , city : String

    -- Listing
    , offerings : List Offering
    }


type alias Offering =
    { description : String
    , tags : List ( Tag, String )
    }


zequez : Participant
zequez =
    { seedsAccountName = "zequezzeuqez"
    , name = "Ezequiel Schwartzman"
    , contactName = ""
    , isOrganization = False
    , description = ""
    , url = "https://zequez.space"
    , acceptsDonations = True

    -- Contact channels
    , discord = "Zequez#6608"
    , email = "zequez@gmail.com"
    , phone = "+5492235235568"
    , telegram = "@Zequez"
    , whatsapp = "+5492235235568"
    , instagram = "@zequez.space"
    , preferredContact = [ Telegram ]

    -- Images URLs
    , paymentQr = ""
    , logo = ""

    -- Location
    , languages = [ L.Es, L.En ]
    , country = "Argentina"
    , region = "Costa atlántica"
    , city = "Mar del Plata"

    -- Listing
    , offerings = []
    }


blank : Participant
blank =
    { seedsAccountName = "zequezzeuqez"
    , name = "Ezequiel Schwartzman"
    , contactName = ""
    , isOrganization = False
    , description = ""
    , url = "https://zequez.space"
    , acceptsDonations = True

    -- Contact channels
    , discord = "Zequez#6608"
    , email = "zequez@gmail.com"
    , phone = "+5492235235568"
    , telegram = "@Zequez"
    , whatsapp = "+5492235235568"
    , instagram = "@zequez.space"
    , preferredContact = [ Telegram ]

    -- Images URLs
    , paymentQr = ""
    , logo = ""

    -- Location
    , languages = [ L.Es, L.En ]
    , country = "Argentina"
    , region = "Costa atlántica"
    , city = "Mar del Plata"

    -- Listing
    , offerings = []
    }


type ContactChannels
    = Discord
    | Email
    | Phone
    | Telegram
    | Whatsapp
    | Instagram
