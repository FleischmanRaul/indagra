module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Bulma.CDN exposing (..)
import Bulma.Columns exposing (..)
import Bulma.Components exposing (..)
import Bulma.Elements exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)
import Bulma.Modifiers.Typography exposing (textCentered)
import Html exposing (Attribute, Html, a, br, i, img, input, main_, option, p, small, span, strong, text)
import Html.Attributes exposing (attribute, class, href, placeholder, rel, src, style, type_)
import Url



---- MODEL ----


type alias Model =
    { menuOn : Bool
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { menuOn = False
      , key = key
      , url = url
      }
    , Cmd.none
    )


type Msg
    = TogleMenu
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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
                    ( model, Cmd.none )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ stylesheet
            , exampleNavbar
            , exampleHero
            , exampleColumns
            , exampleFooter
            ]
    in
    { body = body
    , title = "Indagra"
    }


exampleHero : Html msg
exampleHero =
    hero { heroModifiers | size = Medium, color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "Indagra srl" ]
                , title H2 [] [ text "Give me money, pls." ]
                ]
            ]
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


exampleNavbar : Html Msg
exampleNavbar =
    navbar navbarModifiers
        []
        [ navbarBrand []
            (navbarBurger False
                []
                [ span [] []
                , span [] []
                , span [] []
                ]
            )
            [ navbarItem False
                []
                [ img [ src "https://bulma.io/images/bulma-logo.png" ] []
                ]
            ]
        , navbarMenu False
            []
            [ navbarStart []
                [ navbarItemLink False [] [ text "Home" ]
                , navbarItemDropdown True
                    Down
                    []
                    (navbarLink [] [ text "Docs" ])
                    [ navbarDropdown True
                        Left
                        []
                        [ navbarItemLink False [] [ text "Crud" ]
                        , navbarItemLink False [] [ text "Detritus" ]
                        , navbarItemLink True [] [ text "Refuse" ]
                        , navbarItemLink False [] [ text "Trash" ]
                        ]
                    ]
                ]
            , navbarEnd []
                [ navbarItem False
                    []
                    []
                ]
            ]
        ]


exampleFooter : Html Msg
exampleFooter =
    footer []
        [ container []
            [ content Standard
                [ textCentered ]
                [ p []
                    [ strong [] [ text "Bulma" ]
                    , text " by "
                    , a [ href "https://jgthms.com" ] [ text "Jeremy Thomas" ]
                    , text ". The source code is licensed "
                    , a [ href "http://opensource.org/licenses/mit-license.php" ] [ text "MIT" ]
                    , text ". The website content is licensed "
                    , a [ href "http://creativecommons.org/licenses/by-nc-sa/4.0" ] [ text "CC BY NC SA 4.0" ]
                    , text "."
                    ]
                ]
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
