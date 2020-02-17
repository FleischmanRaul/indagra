module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers as BM
import Bulma.Modifiers.Typography as BMT
import Ease
import Html exposing (Attribute, Html, a, br, div, i, img, input, main_, option, p, small, span, strong, text)
import Html.Attributes exposing (attribute, class, href, id, placeholder, rel, src, style, type_)
import Html.Events exposing (onClick, onMouseLeave, onMouseOver)
import SmoothScroll exposing (Config, scrollTo, scrollToWithOptions)
import Task
import Url
import Url.Parser as Url exposing ((</>), Parser)



---- MODEL ----


defaultConfig : Config
defaultConfig =
    { offset = 12
    , speed = 50
    , easing = Ease.outQuint
    }


type Page
    = Index
    | About
    | Services
    | Portofolio
    | Contact


type alias Model =
    { menuOn : Bool
    , key : Nav.Key
    , page : Page
    , hoveredNavbarItem : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { menuOn = False
      , key = key
      , page = urlToPage url
      , hoveredNavbarItem = 0
      }
    , Task.attempt (always <| DoNothing <| urlToPage url) (scrollTo <| pageToString <| urlToPage url)
    )


type Msg
    = TogleMenu
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NavbarClick Page
    | DoNothing Page
    | SetHoveredNavbarItem Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TogleMenu ->
            ( { model | menuOn = not model.menuOn }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            ( { model | page = urlToPage url }
            , Cmd.none
            )

        NavbarClick page ->
            ( model
            , Task.attempt (always <| DoNothing page) (scrollToWithOptions defaultConfig <| pageToString page)
            )

        DoNothing page ->
            ( model, Nav.pushUrl model.key <| pageToUrl page )

        SetHoveredNavbarItem n ->
            ( { model | hoveredNavbarItem = n }
            , Cmd.none
            )


urlToPage : Url.Url -> Page
urlToPage url =
    -- We start with our URL
    url
        -- Send it through our URL parser (located below)
        |> Url.parse urlParser
        -- And if it didn't match any known pages, return Index
        |> Maybe.withDefault Index


urlParser : Parser (Page -> a) a
urlParser =
    -- We try to match one of the following URLs
    Url.oneOf
        [ Url.map Index Url.top
        , Url.map About (Url.s "despre-noi")
        , Url.map Services (Url.s "servicii")
        , Url.map Portofolio (Url.s "portofoliu")
        , Url.map Contact (Url.s "contact")
        ]


pageToString : Page -> String
pageToString page =
    case page of
        Index ->
            "index"

        About ->
            "about"

        Services ->
            "services"

        Portofolio ->
            "portofolio"

        Contact ->
            "contact"


pageToUrl : Page -> String
pageToUrl page =
    case page of
        Index ->
            "/"

        About ->
            "despre-noi"

        Services ->
            "servicii"

        Portofolio ->
            "portofoliu"

        Contact ->
            "contact"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ div [ style "backgroundColor" "black", style "height" "100%" ]
                [ stylesheet
                , navbar model
                , index
                , about
                , services
                , portofolio
                , contact
                , exampleFooter
                ]
            ]
    in
    { body = body
    , title = "Indagra"
    }


myNavbarModifier : NavbarModifiers
myNavbarModifier =
    { color = BM.Black
    , transparent = True
    }


navbar : Model -> Html Msg
navbar model =
    fixedNavbar BM.Top
        myNavbarModifier
        []
        [ navbarBrand [ style "margin-left" "1%" ]
            (navbarBurger False
                []
                [ span [] []
                , span [] []
                , span [] []
                ]
            )
            [ navbarItem False
                [ onClick <| NavbarClick Index ]
                [ img [ src "./indagra_logo.svg" ] []
                ]
            ]
        , navbarEnd []
            [ navbarItem False
                [ onMouseLeave <| SetHoveredNavbarItem 0 ]
                [ navbarItem False (navbarItemCss model aboutSetting) [ text "DESPRE NOI" ]
                , navbarItem False (navbarItemCss model servicesSetting) [ text "SERVICII" ]
                , navbarItem False (navbarItemCss model portofolioSetting) [ text "PORTOFOLIU" ]
                , navbarItem False (navbarItemCss model contactSetting) [ text "CONTACT" ]
                ]
            ]
        ]


type alias NavbarItemSettings =
    { section : Page
    , hoverNumber : Int
    , hoverColor : String
    }


aboutSetting : NavbarItemSettings
aboutSetting =
    { section = About
    , hoverNumber = 1
    , hoverColor = "#B5245C"
    }


servicesSetting : NavbarItemSettings
servicesSetting =
    { section = Services
    , hoverNumber = 2
    , hoverColor = "#DB2E54"
    }


portofolioSetting : NavbarItemSettings
portofolioSetting =
    { section = Portofolio
    , hoverNumber = 3
    , hoverColor = "#FA2A3B"
    }


contactSetting : NavbarItemSettings
contactSetting =
    { section = Contact
    , hoverNumber = 4
    , hoverColor = "#F89D32"
    }


navbarItemCss : Model -> NavbarItemSettings -> List (Attribute Msg)
navbarItemCss model setting =
    [ onClick <| NavbarClick setting.section
    , onMouseOver <| SetHoveredNavbarItem setting.hoverNumber
    , if model.hoveredNavbarItem == setting.hoverNumber then
        style "background-color" setting.hoverColor

      else
        style "" ""
    , style "color" "white"
    ]


index : Html msg
index =
    section Spaced
        [ id "index" ]
        [ heroBody []
            [ container []
                [ title H1 [] [ text "Indagra srl" ]
                , title H2 [] [ text "Give me money, pls." ]
                ]
            ]
        ]


about : Html msg
about =
    section Spaced
        [ id "about" ]
        [ title H1 [] [ text "Despre noi" ]
        ]


services : Html msg
services =
    section Spaced
        [ id "services" ]
        [ title H1 [] [ text "Servicii" ]
        ]


portofolio : Html msg
portofolio =
    section Spaced
        [ id "portofolio" ]
        [ title H1 [] [ text "Portofoliu" ]
        ]


contact : Html msg
contact =
    section Spaced
        [ id "contact" ]
        [ title H1 [] [ text "Contact" ]
        ]


exampleColumns : Html msg
exampleColumns =
    section NotSpaced
        []
        [ container []
            [ columns columnsModifiers
                []
                [ column columnModifiers [] [ text "First Column" ]
                , column columnModifiers [] [ text "Second Column" ]
                , column columnModifiers [] [ text "Third Column" ]
                ]
            ]
        ]


exampleFooter : Html Msg
exampleFooter =
    footer []
        [ container []
            [ content BM.Standard
                [ BMT.textCentered ]
                [ p [] [ strong [] [ text "Copyright © 2020 INDAGRA SRL" ] ] ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
