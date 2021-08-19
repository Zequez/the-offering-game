module OfferingGame.Tag exposing (Tag(..), TagInfo, toTagInfo)


type Tag
    = Language
    | Region
    | Currency
    | OType
    | OIs


type alias TagInfo =
    { name : String
    , emoji : String
    , color : String
    }


toTagInfo : Tag -> TagInfo
toTagInfo tag =
    case tag of
        Language ->
            TagInfo "Language" "🌐" "#cdcb4d"

        Region ->
            TagInfo "Region" "🌎" "lightblue"

        Currency ->
            TagInfo "Currency" "💰" "#68cb97"

        OType ->
            TagInfo "Type" "📄" "orange"

        OIs ->
            TagInfo "Is" "🤝" "gold"
