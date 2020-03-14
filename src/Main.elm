module Main exposing (..)

import Browser
import Browser.Dom as Dom
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
import Svg as Svg
import Svg.Attributes as SvgAttributes
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
    , hoveredServiceItem : Int
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { menuOn = False
      , key = key
      , page = urlToPage url
      , hoveredNavbarItem = 0
      , hoveredServiceItem = 0
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
    | SetHoveredServiceItem Int
    | NoOp


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

        SetHoveredServiceItem n ->
            ( { model | hoveredServiceItem = n }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


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
            [ div [ gradient, style "height" "100%", style "color" "white" ]
                [ stylesheet
                , navbar model
                , position model
                , index
                , about
                , services model
                , portofolio
                , contact
                , myFooter
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


gradient : Attribute Msg
gradient =
    style "background" "linear-gradient(90deg, rgba(8,9,13,1) 0%, rgba(32,36,48,1) 100%)"


navbar : Model -> Html Msg
navbar model =
    fixedNavbar BM.Top
        myNavbarModifier
        [ gradient ]
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
    section NotSpaced
        [ id "index", style "background" "url('./indagra_index.jpg')", style "background-size" "100vw" ]
        [ container []
            [ columns columnsModifiers
                []
                [ leftColumn "Protecție pasivă de foc din 1992"
                , column centerColumnModifier [ style "height" "1000px" ] []
                , column sideColumnModifier [] []
                ]
            ]
        ]


titleLine : Html msg
titleLine =
    Svg.svg
        [ SvgAttributes.width "150"
        , SvgAttributes.height "3"
        , SvgAttributes.viewBox "0 0 160 1"
        , SvgAttributes.color "red"
        ]
        [ Svg.line
            [ SvgAttributes.x1 "0"
            , SvgAttributes.y1 "0"
            , SvgAttributes.x2 "150"
            , SvgAttributes.y2 "0"
            , SvgAttributes.stroke "#FFFFFF"
            , SvgAttributes.strokeWidth "3"
            , SvgAttributes.strokeMiterlimit "10"
            , SvgAttributes.fill "none"
            ]
            []
        ]


verticalLine : Html msg
verticalLine =
    Svg.svg
        [ SvgAttributes.width "3"
        , SvgAttributes.height "150"
        , SvgAttributes.viewBox "0 0 3 150"
        , SvgAttributes.color "red"
        ]
        [ Svg.line
            [ SvgAttributes.x1 "0"
            , SvgAttributes.y1 "0"
            , SvgAttributes.x2 "0"
            , SvgAttributes.y2 "150"
            , SvgAttributes.stroke "#FFFFFF"
            , SvgAttributes.strokeWidth "3"
            , SvgAttributes.strokeMiterlimit "10"
            , SvgAttributes.fill "none"
            ]
            []
        ]


filledCircle : Html msg
filledCircle =
    Svg.svg
        [ SvgAttributes.width "30", SvgAttributes.height "30", SvgAttributes.viewBox "0 0 30 30", SvgAttributes.color "white" ]
        [ Svg.circle [ SvgAttributes.cx "15", SvgAttributes.cy "15", SvgAttributes.r "4", SvgAttributes.stroke "#EEEEEE", SvgAttributes.fill "#EEEEEE" ] [] ]


emptyCircle : Html msg
emptyCircle =
    Svg.svg
        [ SvgAttributes.width "30", SvgAttributes.height "30", SvgAttributes.viewBox "0 0 30 30", SvgAttributes.color "white" ]
        [ Svg.circle [ SvgAttributes.cx "15", SvgAttributes.cy "15", SvgAttributes.r "4", SvgAttributes.stroke "#EEEEEE", SvgAttributes.fill "none" ] [] ]


verticalTextCss : List (Attribute msg)
verticalTextCss =
    [ style "writing-mode" "vertical-rl", style "transform" "rotate(180deg)", style "height" "250px" ]


centerWidth : BM.Devices (Maybe BM.Width)
centerWidth =
    { mobile = Just BM.Width10
    , tablet = Just BM.Width10
    , desktop = Just BM.Width10
    , widescreen = Just BM.Width10
    , fullHD = Just BM.Width10
    }


centerColumnModifier : ColumnModifiers
centerColumnModifier =
    { offset = BM.Auto
    , widths = centerWidth
    }


sideWidth : BM.Devices (Maybe BM.Width)
sideWidth =
    { mobile = Just BM.Width1
    , tablet = Just BM.Width1
    , desktop = Just BM.Width1
    , widescreen = Just BM.Width1
    , fullHD = Just BM.Width1
    }


sideColumnModifier : ColumnModifiers
sideColumnModifier =
    { offset = BM.Auto
    , widths = sideWidth
    }


sectionTitle title =
    div [ style "font-size" "48px", style "text-align" "left" ]
        [ titleLine
        , text title
        ]


position model =
    div [ style "position" "fixed", style "top" "45vh", style "right" "1vw", style "display" "flex", style "flex-direction" "column" ] [ emptyCircle, emptyCircle, filledCircle, emptyCircle, emptyCircle ]


about : Html msg
about =
    section NotSpaced
        [ id "about" ]
        [ sectionTitle "DESPRE NOI"
        , container [ style "width" "100%" ]
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem" ]
                [ leftColumn "Despre noi"
                , column centerColumnModifier [] [ aboutText ]
                , column sideColumnModifier [ style "display" "flex", style "align-items" "center" ] [ tciLogo ]
                ]
            ]
        ]


leftColumn : String -> Html msg
leftColumn sectionName =
    column sideColumnModifier
        [ style "display" "flex", style "flex-direction" "row", style "align-items" "center" ]
        [ div verticalTextCss [ text sectionName ]
        , div [ style "display" "flex", style "height" "150px", style "margin-top" "150px" ] [ verticalLine ]
        ]


tciLogo : Html msg
tciLogo =
    div [ style "background-color" "white", style "width" "150px", style "border-radius" "10px", style "align-self" "flex-end" ] [ a [ href "https://www.totceiubesc.ro/listing/servicii/indagra-etansare-si-voprire-antifoc/", Html.Attributes.target "_blank" ] [ img [ src "./tci.png", style "margin" "0px" ] [] ] ]


aboutText : Html msg
aboutText =
    div []
        [ p [ style "padding-bottom" "2rem" ] [ text "INDAGRA este specializată în protecția pasivă la foc: termoprotecție cu vopsea termospumantă și torcret, precum și în producția de uși metalice. Cu peste 10 ani de experiență în domeniul de protecție pasivă la foc, firma asigură:" ]
        , div [ style "width" "40vw", style "float" "right", style "text-align" "left" ]
            [ p [ style "padding-bottom" "1rem" ] [ text "/ protecția cu vopsea termospumantă și torcret a structurilor din oțel împotriva incendiilor, " ]
            , p [ style "padding-bottom" "1rem" ] [ text "/ etanșarea antifoc între compartimente a trecerilor de cabluri, " ]
            , p [ style "padding-bottom" "1rem" ] [ text "/ țevi metalice, " ]
            , p [ style "padding-bottom" "1rem" ] [ text "/  conducte din material plastic și tubulaturi de ventilație, " ]
            , p [ style "padding-bottom" "15rem" ] [ text "/ confecționarea de uși metalice de dimensiuni standard și atipice cu termoizolație sau căptușite cu plumb împotriva radiațiilor. " ]
            ]
        ]


services : Model -> Html Msg
services model =
    section Spaced
        [ id "services" ]
        [ sectionTitle "SERVICII"
        , container []
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem", onMouseLeave <| SetHoveredServiceItem 0 ]
                [ leftColumn "Servicii"
                , column centerColumnModifier [] [ serviceBoxes model ]
                , column sideColumnModifier [] []
                ]
            ]
        ]


serviceBoxes : Model -> Html Msg
serviceBoxes model =
    div [ style "display" "flex", style "flex-direction" "row", style "align-items" "center", style "justify-content" "center" ]
        [ div (boxCss model 1) [ text "EXECUȚIE DE LUCRĂRI DE TERMOPROTECȚIE" ]
        , div (boxCss model 2) [ text "ETANȘAREA PENETRAȚIILOR DIN PEREȚI ȘI PLANȘEE CU MATERIAL TERMOSPUMANT" ]
        , div (boxCss model 3) [ text "EXECUȚIE ȘI MONTAJ DE UȘI METALICE " ]
        ]


boxCss : Model -> Int -> List (Attribute Msg)
boxCss model setting =
    [ onMouseOver <| SetHoveredServiceItem setting
    , if model.hoveredServiceItem == setting then
        style "background-color" "#DB2E54"

      else
        style "background-color" "#141414"
    , style "width" "18vw"
    , style "height" "18vw"
    , style "margin" "1vw"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "20px"
    , style "border-radius" "5px"
    ]


portofolio : Html msg
portofolio =
    section NotSpaced
        [ id "portofolio" ]
        [ sectionTitle "PORTOFOLIU"
        , container []
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem" ]
                [ leftColumn "Portofoliu"
                , column centerColumnModifier [] [ portofolioText ]
                , column sideColumnModifier [] []
                ]
            ]
        ]


squareCss : List (Attribute msg)
squareCss =
    [ style "width" "400px"
    , style "height" "400px"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "font-size" "24px"
    , style "background-color" "#14171F"
    , style "margin-bottom" "200px"
    , style "margin-top" "0px"
    , style "padding" "100px"
    ]


projectsCss : List (Attribute msg)
projectsCss =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "left"
    , style "flex-direction" "column"
    , style "height" "400px"
    , style "margin-bottom" "200px"
    , style "text-align" "left"
    , style "background-color" "#14171F"
    ]


portofolioText : Html msg
portofolioText =
    columns myColumnsModifiers
        []
        [ column columnModifiers
            []
            [ div squareCss [ text "LUCRĂRI DE TERMOPROTECȚIE CU VOPSEA TERMOSPUMANTĂ" ]
            , div squareCss [ text "LUCRĂRI DE ETANȘARE ANTIFOC" ]
            ]
        , column columnModifiers
            []
            [ div projectsCss
                [ p [] [ text "HOTEL TELEFERIC POIANA BRAȘOV" ]
                , p [] [ text "HALA ERTEX BRAȘOV" ]
                , p [] [ text "HALA KATADYN PREJMER" ]
                , p [] [ text "HALA DIETAL CODLEA" ]
                , p [] [ text "HALA REHOMA PITEȘTI" ]
                , p [] [ text "GALERII AUCHAN TG. MUREȘ" ]
                , p [] [ text "BCR SILVER MOUNTAIN POIANA BRAȘOV" ]
                , p [] [ text "CLĂDIRE ONE CHARLES DE GAULLE BUCUREȘTI" ]
                , p [] [ text "SPITALUL ONCOLOGIC BRAȘOV" ]
                , p [] [ text "HALA RECOBOL BRAȘOV" ]
                , p [] [ text "PARCĂRI SUPRATERANE PREDEAL" ]
                ]
            , div projectsCss
                [ p [] [ text "KRONOSPAN BRAȘOV" ]
                , p [] [ text "SPITALUL VICTOR GOMOIU BUCUREȘTI" ]
                , p [] [ text "PARCARE REGINA MARIA BRAȘOV" ]
                , p [] [ text "HALA DIETAL CODLEA" ]
                , p [] [ text "HALA ZOLLNER SATU MARE" ]
                , p [] [ text "HALA QUIN BRAȘOV" ]
                , p [] [ text "CLĂDIRI DE BIROURI CORESI BUSSINES PARK BRAȘOV" ]
                , p [] [ text "AUCHAN SATU MARE" ]
                , p [] [ text "HALA RUBITECH PREJMER" ]
                , p [] [ text "CLĂDIRE CARPATEX BRAȘOV" ]
                , p [] [ text "AUTOLIV BRAȘOV, ROVINARI, REȘIȚA" ]
                , p [] [ text "HALA BELLMAN BRAȘOV" ]
                , p [] [ text "CENTRUL COMERCIAL SHOPPING CITY SIBIU" ]
                , p [] [ text "SPITALUL SF. CONSTANTIN BRAȘOV" ]
                ]
            ]
        ]


myColumnsModifiers : ColumnsModifiers
myColumnsModifiers =
    { multiline = False
    , gap = Gap0
    , display = TabletAndBeyond
    , centered = True
    }


contact : Html msg
contact =
    section NotSpaced
        [ id "contact" ]
        [ sectionTitle "CONTACT"
        , container [ style "margin-top" "4rem" ]
            [ columns myColumnsModifiers
                []
                [ leftColumn "Contact"
                , column centerColumnModifier [] [ contactText ]
                , column sideColumnModifier [] []
                ]
            ]
        ]


contactFieldCss : List (Attribute msg)
contactFieldCss =
    [ style "font-size" "24px" ]


contactDataCss : List (Attribute msg)
contactDataCss =
    [ style "font-size" "12px", style "margin-bottom" "4rem" ]


contactText : Html msg
contactText =
    div []
        [ p contactFieldCss [ text "ADRESĂ" ]
        , p contactDataCss [ text "STR, MATEI BASARAB NR.12 BRAȘOV ROMÂNIA" ]
        , p contactFieldCss [ text "TELEFON" ]
        , p contactDataCss [ text "0741 264 821" ]
        , p contactFieldCss [ text "FAX" ]
        , p contactDataCss [ text "0268 475 896" ]
        , p contactFieldCss [ text "E-MAIL" ]
        , p contactDataCss [ text "info@indagrasrl.ro" ]
        ]


myFooter : Html Msg
myFooter =
    footer [ gradient ]
        [ container []
            [ content BM.Standard
                [ BMT.textCentered ]
                [ p [ style "color" "white" ] [ text "Copyright © 2020 INDAGRA SRL" ] ]
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
