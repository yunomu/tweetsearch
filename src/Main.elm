port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import DatePicker
import Debug
import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HtmlAttr
import Html.Events as HtmlEvent
import Json.Decode as JsonDecode
import Route exposing (Route)
import Time
import Url exposing (Url)
import Url.Builder as UrlBuilder


port newTab : String -> Cmd msg


type alias Flags =
    { tzOffset : Maybe Int
    }


type DateRangeType
    = OneDay
    | Range


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChanged Url
    | OnResize Int Int
    | ChangeUser String
    | ChangeText String
    | ChangeToUser String
    | ClearToUser
    | SwitchLive Bool
    | SwitchDateRangeType DateRangeType
    | SetStartDate DatePicker.Msg
    | ClearStartDate
    | SetEndDate DatePicker.Msg
    | ClearEndDate
    | SetTimezoneOffset Int
    | Submit
    | NOP


type alias DatePicker =
    { date : Maybe Date
    , picker : DatePicker.DatePicker
    , settings : DatePicker.Settings
    }


initDatePicker : ( DatePicker, Cmd DatePicker.Msg )
initDatePicker =
    let
        ( picker, cmd ) =
            DatePicker.init
    in
    ( { date = Nothing
      , picker = picker
      , settings = DatePicker.defaultSettings
      }
    , cmd
    )


type alias Model =
    { key : Nav.Key
    , route : Route
    , user : Maybe String
    , tzOffset : Int
    , text : String
    , toUser : String
    , live : Bool
    , dateRangeType : DateRangeType
    , startDatePicker : DatePicker
    , endDatePicker : DatePicker
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( startDP, startDPCmd ) =
            initDatePicker

        ( endDP, endDPCmd ) =
            initDatePicker

        route =
            Route.fromUrl url

        user =
            case route of
                Route.User username ->
                    Just username

                _ ->
                    Nothing
    in
    ( { key = key
      , route = route
      , user = user
      , tzOffset = Maybe.withDefault 0 flags.tzOffset
      , text = ""
      , toUser = ""
      , live = True
      , dateRangeType = OneDay
      , startDatePicker = startDP
      , endDatePicker = endDP
      }
    , Cmd.batch
        [ Nav.pushUrl key (Url.toString url)
        , Cmd.map SetStartDate startDPCmd
        , Cmd.map SetEndDate endDPCmd
        ]
    )


dateQuery : Int -> Date -> String
dateQuery offset date =
    let
        h_ =
            offset // 60

        h =
            if h_ < 0 then
                h_ + 24

            else
                h_

        d =
            if h_ < 0 then
                Date.add Date.Days -1 date

            else
                date

        str =
            String.pad 2 '0' << String.fromInt

        m =
            modBy 60 -offset
    in
    String.concat
        [ Date.format "y-MM-dd_" d
        , str h
        , ":"
        , str m
        , ":00_UTC"
        ]


catMaybes : List (Maybe a) -> List a
catMaybes list =
    case list of
        (Just x) :: xs ->
            x :: catMaybes xs

        Nothing :: xs ->
            catMaybes xs

        [] ->
            []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDP dp dpMsg =
            let
                ( newDP, event ) =
                    DatePicker.update dp.settings dpMsg dp.picker

                newDate =
                    case event of
                        DatePicker.Picked date ->
                            Just date

                        _ ->
                            dp.date
            in
            { dp
                | date = newDate
                , picker = newDP
            }

        clearDate dp =
            { dp | date = Nothing }
    in
    case msg of
        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Route.fromUrl url }, Cmd.none )

        ChangeText txt ->
            ( { model | text = txt }, Cmd.none )

        ChangeToUser txt ->
            ( { model | toUser = txt }, Cmd.none )

        ClearToUser ->
            ( Debug.log "clear" { model | toUser = "" }, Cmd.none )

        SwitchDateRangeType typ ->
            case model.dateRangeType of
                OneDay ->
                    let
                        startDP =
                            model.startDatePicker

                        endDP =
                            model.endDatePicker
                    in
                    ( { model
                        | dateRangeType = typ
                        , endDatePicker = { endDP | date = Maybe.map (Date.add Date.Days 1) startDP.date }
                      }
                    , Cmd.none
                    )

                Range ->
                    ( { model | dateRangeType = typ }
                    , Cmd.none
                    )

        SetStartDate dpMsg ->
            case model.dateRangeType of
                OneDay ->
                    let
                        dp =
                            updateDP model.startDatePicker dpMsg

                        endDP =
                            model.endDatePicker
                    in
                    ( { model
                        | startDatePicker = dp
                        , endDatePicker = { endDP | date = Maybe.map (Date.add Date.Days 1) dp.date }
                      }
                    , Cmd.none
                    )

                Range ->
                    ( { model | startDatePicker = updateDP model.startDatePicker dpMsg }
                    , Cmd.none
                    )

        ClearStartDate ->
            ( { model | startDatePicker = clearDate model.startDatePicker }
            , Cmd.none
            )

        SetEndDate dpMsg ->
            ( { model | endDatePicker = updateDP model.endDatePicker dpMsg }
            , Cmd.none
            )

        ClearEndDate ->
            ( { model | endDatePicker = clearDate model.endDatePicker }
            , Cmd.none
            )

        SetTimezoneOffset offset ->
            ( { model | tzOffset = offset }, Cmd.none )

        Submit ->
            let
                trimText s =
                    if s == "" then
                        Nothing

                    else
                        Just s

                ifMaybe cond v =
                    if cond then
                        Just v

                    else
                        Nothing

                q =
                    String.join " " <|
                        catMaybes
                            [ trimText model.text
                            , Maybe.map ((++) "from:") model.user
                            , Maybe.map ((++) "to:") <| ifMaybe (model.toUser == "") model.toUser
                            , Maybe.map (dateQuery model.tzOffset >> (++) "since:") model.startDatePicker.date
                            , Maybe.map (Date.add Date.Days 1 >> dateQuery model.tzOffset >> (++) "until:") model.endDatePicker.date
                            ]

                url =
                    UrlBuilder.crossOrigin "https://twitter.com"
                        [ "search" ]
                    <|
                        catMaybes
                            [ Just <| UrlBuilder.string "q" q
                            , ifMaybe model.live <| UrlBuilder.string "f" "live"
                            , Just <| UrlBuilder.string "src" "typed_query"
                            ]
            in
            ( model, newTab url )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


tzOffsets : List Int
tzOffsets =
    [ -840, -780, -765, -720, -660, -630, -600, -570, -540, -525, -480, -420, -390, -360, -345, -330, -300, -270, -240, -180, -120, -60, 0, 60, 120, 180, 210, 240, 300, 360, 420, 480, 540, 570, 600, 660, 720 ]


offsetToTz : Int -> String
offsetToTz i =
    let
        h =
            -i // 60

        m =
            modBy 60 -i

        sign =
            if i <= 0 then
                "+"

            else
                ""
    in
    String.concat <|
        [ "UTC", sign, String.fromInt h ]
            ++ (if m == 0 then
                    []

                else
                    [ ":", String.fromInt m ]
               )


selectTZ : (Int -> msg) -> Int -> List ( Int, String ) -> Html msg
selectTZ set selected =
    Html.select
        [ HtmlEvent.on
            "change"
            (JsonDecode.map (set << Maybe.withDefault 0 << String.toInt) HtmlEvent.targetValue)
        ]
        << List.map
            (\( offset, tz ) ->
                Html.option
                    [ HtmlAttr.value <| String.fromInt offset
                    , HtmlAttr.selected (offset == selected)
                    ]
                    [ Html.text tz ]
            )


datePicker : String -> DatePicker -> (DatePicker.Msg -> msg) -> msg -> Element msg
datePicker label dp set clear =
    Element.row []
        [ Element.text label
        , Element.html (DatePicker.view dp.date dp.settings dp.picker |> Html.map set)
        , Input.button []
            { onPress = Just clear
            , label = Element.text "Clear"
            }
        ]


dateView :
    DateRangeType
    -> DatePicker
    -> DatePicker
    -> Element Msg
dateView typ startDP endDP =
    Element.row [] <|
        case typ of
            OneDay ->
                [ datePicker "Date:" startDP SetStartDate ClearStartDate
                ]

            Range ->
                [ datePicker "Start Date:" startDP SetStartDate ClearStartDate
                , datePicker "End date:" endDP SetEndDate ClearEndDate
                ]


form : Element msg -> Element msg
form elm =
    Element.html <| Html.form [] [ Element.layout [] elm ]


searchInput : (String -> msg) -> String -> Element msg
searchInput onInput v =
    Element.row []
        [ Element.text "Search:"
        , Element.html <| Html.input [ HtmlAttr.type_ "search", HtmlEvent.onInput onInput, HtmlAttr.value v ] []
        ]


toUserInput : (String -> msg) -> msg -> String -> Element msg
toUserInput onInput clear v =
    Element.row []
        [ Element.text "To user:"
        , Element.html <| Html.input [ HtmlAttr.type_ "text", HtmlEvent.onInput onInput, HtmlAttr.value v ] []
        , Input.button []
            { onPress = Just clear
            , label = Element.text "Clear"
            }
        ]


timezonePicker : (Int -> msg) -> Int -> Element msg
timezonePicker setTimezoneOffset tzOffset =
    let
        f a =
            ( a, offsetToTz a )
    in
    Element.row []
        [ Element.text "TimeZone:"
        , Element.html <| selectTZ setTimezoneOffset tzOffset <| List.map f tzOffsets
        ]


submit : msg -> Element msg
submit onSubmit =
    Element.html <| Html.input [ HtmlAttr.type_ "button", HtmlEvent.onClick onSubmit, HtmlAttr.value "Search" ] []


view : Model -> Browser.Document Msg
view model =
    { title = "test"
    , body =
        [ Element.layout [] <|
            form <|
                Element.column []
                    [ Element.text <| "From: " ++ Maybe.withDefault "(no user filter)" model.user
                    , searchInput ChangeText model.text
                    , Input.radioRow []
                        { onChange = SwitchDateRangeType
                        , options =
                            [ Input.option OneDay <| Element.text "One day"
                            , Input.option Range <| Element.text "Date range"
                            ]
                        , selected = Just model.dateRangeType
                        , label = Input.labelLeft [] <| Element.text "DateRangeType:"
                        }
                    , toUserInput ChangeToUser ClearToUser model.toUser
                    , dateView model.dateRangeType model.startDatePicker model.endDatePicker
                    , Input.checkbox []
                        { onChange = SwitchLive
                        , icon = Input.defaultCheckbox
                        , checked = model.live
                        , label = Input.labelLeft [] <| Element.text "Live"
                        }
                    , timezonePicker SetTimezoneOffset model.tzOffset
                    , submit Submit
                    ]
        ]
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequest
        }
