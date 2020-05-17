port module Main exposing (..)

-- import Bulma.Classes as BC

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
import Gallery exposing (..)
import Gallery.Image as Image
import Html exposing (Attribute, Html, a, br, button, div, i, img, input, main_, option, p, small, span, strong, text)
import Html.Attributes exposing (attribute, class, href, id, placeholder, rel, src, style, type_)
import Html.Events exposing (onClick, onMouseLeave, onMouseOver)
import InView as InView
import SmoothScroll exposing (Config, scrollTo, scrollToWithOptions)
import Svg as Svg
import Svg.Attributes as SvgAttributes
import Task
import Url
import Url.Parser as Url exposing ((</>), (<?>), Parser, string)



---- MODEL ----


defaultConfig : Config
defaultConfig =
    { offset = 12
    , speed = 50
    , easing = Ease.outQuint
    }


type alias Model =
    { menuOn : Bool
    , isModalOpen : Bool
    , key : Nav.Key
    , page : Page
    , hoveredNavbarItem : Int
    , hoveredServiceItem : Int
    , clickedServiceItem : Int
    , inView : InView.State
    , imageGallery : Gallery.State
    }


port onScroll : (( Float, Float ) -> msg) -> Sub msg


port sendData : String -> Cmd msg


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( inViewModel, inViewCmds ) =
            InView.init [ "index", "about", "services", "portofolio", "contact" ]
    in
    ( { menuOn = False
      , isModalOpen = False
      , key = key
      , page = urlToPage url
      , hoveredNavbarItem = 0
      , hoveredServiceItem = 0
      , clickedServiceItem = 0
      , inView = inViewModel
      , imageGallery = Gallery.init (List.length imageSlides)
      }
    , Cmd.batch [ Task.attempt (always <| DoNothing <| urlToPage url) (scrollTo <| pageToString <| urlToPage url), Cmd.map InViewMsg inViewCmds ]
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
    | OnScroll ( Float, Float )
    | InViewMsg InView.Msg
    | OpenModal ServiceType
    | CloseModal
    | ImageGalleryMsg Gallery.Msg


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
            ( { model | menuOn = False }
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

        OnScroll ( x, y ) ->
            ( { model | inView = InView.updateViewportOffset x y model.inView }
            , Nav.pushUrl model.key <| pageToUrl <| getCurrentPage model
            )

        InViewMsg inViewMsg ->
            let
                ( inView, inViewCmds ) =
                    InView.update inViewMsg model.inView
            in
            ( { model | inView = inView }
            , Cmd.map InViewMsg inViewCmds
            )

        OpenModal serviceType ->
            ( { model | isModalOpen = True, clickedServiceItem = model.hoveredServiceItem }
              -- , Nav.pushUrl model.key <| pageToUrl <| Services <| Just serviceType
            , sendData "!"
            )

        CloseModal ->
            ( { model | isModalOpen = False }
              -- , Nav.pushUrl model.key <| pageToUrl <| Services Nothing
            , Task.attempt (always <| DoNothing <| Services Nothing) (scrollToWithOptions defaultConfig <| pageToString <| Services Nothing)
            )

        ImageGalleryMsg imageGalleryMsg ->
            ( { model | imageGallery = Gallery.update imageGalleryMsg model.imageGallery }
            , Cmd.none
            )


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


type Page
    = Index
    | About
    | Services (Maybe ServiceType)
    | Portofolio
    | Contact


type ServiceType
    = TermoProtection
    | Sealing
    | MetalicDoors


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
        , Url.map (Services << Just) (Url.s "servicii" </> st)
        , Url.map (Services Nothing) (Url.s "servicii")
        , Url.map Portofolio (Url.s "portofoliu")
        , Url.map Contact (Url.s "contact")
        ]


st =
    Url.custom "" stringToServiceType


stringToServiceType : String -> Maybe ServiceType
stringToServiceType s =
    case s of
        "protectie_termica" ->
            Just TermoProtection

        "etansare" ->
            Just Sealing

        "usi_metalice" ->
            Just MetalicDoors

        _ ->
            Nothing


pageToString : Page -> String
pageToString page =
    case page of
        Index ->
            "index"

        About ->
            "about"

        Services Nothing ->
            "services"

        Services (Just TermoProtection) ->
            "servicii/protectie_termica"

        Services (Just Sealing) ->
            "servicii/etansare"

        Services (Just MetalicDoors) ->
            "servicii/usi_metalice"

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

        Services Nothing ->
            "servicii"

        Services (Just TermoProtection) ->
            "servicii/protectie_termica"

        Services (Just Sealing) ->
            "servicii/etansare"

        Services (Just MetalicDoors) ->
            "servicii/usi_metalice"

        Portofolio ->
            "portofoliu"

        Contact ->
            "contact"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map InViewMsg <| InView.subscriptions model.inView
        , onScroll OnScroll
        ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ div [ gradient, style "color" "white", overflow model, bodyPosition model, bodyHeight model ]
                [ stylesheet
                , navbar model
                , position model
                , index
                , about
                , services model
                , portofolio
                , contact
                , modalFrame model
                , myFooter
                ]
            ]
    in
    { body = body
    , title = "Indagra"
    }


overflow : Model -> Attribute msg
overflow model =
    if model.isModalOpen then
        style "overflow" "hidden"

    else
        style "" ""


bodyPosition : Model -> Attribute msg
bodyPosition model =
    if model.isModalOpen then
        style "overflow-y" "hidden"

    else
        style "" ""


bodyHeight : Model -> Attribute msg
bodyHeight model =
    if model.isModalOpen then
        style "height" "92vh"

    else
        style "" ""


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
        [ navbarBrand []
            (myNavbarBurger model.menuOn)
            [ navbarItem False
                [ onClick <| NavbarClick Index, style "cursor" "pointer" ]
                [ img [ src "./indagra_logo.svg" ] []
                ]
            ]
        , navbarMenu model.menuOn
            [ gradient ]
            [ navbarEnd [ onMouseLeave <| SetHoveredNavbarItem 0, style "cursor" "pointer" ]
                [ navbarItem False (navbarItemCss model aboutSetting) [ text "DESPRE NOI" ]
                , navbarItem False (navbarItemCss model servicesSetting) [ text "SERVICII" ]
                , navbarItem False (navbarItemCss model portofolioSetting) [ text "PORTOFOLIU" ]
                , navbarItem False (navbarItemCss model contactSetting) [ text "CONTACT" ]
                ]
            ]
        ]


myNavbarBurger : Bool -> Html Msg
myNavbarBurger isMenuOpen =
    navbarBurger isMenuOpen
        [ style "height" "auto", href "", onClick TogleMenu, style "color" "white" ]
        [ span [] []
        , span [] []
        , span [] []
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
    { section = Services Nothing
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
    div []
        [ section NotSpaced
            [ id "index", style "background" "url('./indagra_index.png')", style "background-repeat" "no-repeat", style "background-size" "100vw", hideOnMobile ]
            [ container []
                [ columns columnsModifiers
                    []
                    [ leftColumn "Protecție pasivă de foc din 1992"
                    , column (centerColumnModifier BM.Width10) [ style "height" "1000px" ] []
                    , column (sideColumnModifier BM.Width1) [] []
                    ]
                ]
            ]
        , myImage
        ]


myImage : Html msg
myImage =
    image SixteenByNine
        [ hideOnWideScreen, isHiddenWidescreen ]
        [ img [ src "./indagra_index.png" ] []
        ]


titleLine : String -> Html msg
titleLine color =
    Svg.svg
        [ SvgAttributes.width "150"
        , SvgAttributes.height "3"
        , SvgAttributes.viewBox "0 0 160 1"
        ]
        [ Svg.line
            [ SvgAttributes.x1 "0"
            , SvgAttributes.y1 "0"
            , SvgAttributes.x2 "150"
            , SvgAttributes.y2 "0"
            , SvgAttributes.stroke color
            , SvgAttributes.strokeWidth "3"
            , SvgAttributes.strokeMiterlimit "10"
            , SvgAttributes.fill "none"
            ]
            []
        ]


verticalLine : String -> Html msg
verticalLine color =
    Svg.svg
        [ SvgAttributes.width "3"
        , SvgAttributes.height "150"
        , SvgAttributes.viewBox "0 0 3 150"
        ]
        [ Svg.line
            [ SvgAttributes.x1 "0"
            , SvgAttributes.y1 "0"
            , SvgAttributes.x2 "0"
            , SvgAttributes.y2 "150"
            , SvgAttributes.stroke color
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
    [ style "writing-mode" "vertical-rl", style "transform" "rotate(180deg)", style "height" "350px", hideOnMobile ]


centerWidth : BM.Width -> BM.Devices (Maybe BM.Width)
centerWidth width =
    { mobile = Just width
    , tablet = Just width
    , desktop = Just width
    , widescreen = Just width
    , fullHD = Just width
    }


centerColumnModifier : BM.Width -> ColumnModifiers
centerColumnModifier width =
    { offset = BM.Auto
    , widths = centerWidth width
    }


sideColumnModifier : BM.Width -> ColumnModifiers
sideColumnModifier width =
    { offset = BM.Auto
    , widths = centerWidth width
    }


sectionTitle : String -> Html msg
sectionTitle title =
    div [ BMT.textSizeByDevice titleFontSize, style "display" "flex" ]
        [ div [ hideOnMobile ] [ titleLine "#FFFFFF" ]
        , div [] [ text title ]
        , div [ hideOnWideScreen, isHiddenFullHD, isHiddenWidescreen, style "align-items" "flex-end" ] [ titleLine "#FFFFFF" ]
        ]


titleFontSize : BM.Devices BMT.Size
titleFontSize =
    { mobile = BMT.Large
    , tablet = BMT.Largest
    , desktop = BMT.Largest
    , widescreen = BMT.Largest
    , fullHD = BMT.Largest
    }


position : Model -> Html msg
position model =
    let
        order =
            case model.page of
                Index ->
                    [ filledCircle, emptyCircle, emptyCircle, emptyCircle, emptyCircle ]

                About ->
                    [ emptyCircle, filledCircle, emptyCircle, emptyCircle, emptyCircle ]

                Services _ ->
                    [ emptyCircle, emptyCircle, filledCircle, emptyCircle, emptyCircle ]

                Portofolio ->
                    [ emptyCircle, emptyCircle, emptyCircle, filledCircle, emptyCircle ]

                Contact ->
                    [ emptyCircle, emptyCircle, emptyCircle, emptyCircle, filledCircle ]
    in
    div [ style "position" "fixed", style "top" "45vh", style "right" "1vw", style "display" "flex", style "flex-direction" "column", hideOnMobile ] order


getCurrentPage : Model -> Page
getCurrentPage model =
    if InView.check "contact" model.inView == Just True then
        Contact

    else if InView.check "portofolio" model.inView == Just True then
        Portofolio

    else if InView.check "services" model.inView == Just True then
        Services Nothing

    else if InView.check "about" model.inView == Just True then
        About

    else
        Index


about : Html msg
about =
    section Spaced
        [ id "about" ]
        [ sectionTitle "DESPRE NOI"
        , container []
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem" ]
                [ leftColumn "Despre noi"
                , column (centerColumnModifier BM.Width10) [] [ aboutText ]
                , column (sideColumnModifier BM.Width1) [] []
                ]
            ]
        ]


leftColumn : String -> Html msg
leftColumn sectionName =
    column (sideColumnModifier BM.Width1)
        [ style "display" "flex", style "flex-direction" "row", style "align-items" "center" ]
        [ div verticalTextCss [ text sectionName ]
        , div [ style "display" "flex", style "height" "150px", style "margin-top" "150px", hideOnMobile ] [ verticalLine "#FFFFFF" ]
        ]


modalLeftColumn : String -> Html msg
modalLeftColumn sectionName =
    column (sideColumnModifier BM.Width2)
        [ style "display" "flex", style "flex-direction" "row", style "align-items" "center", style "color" "black", hideOnMobile ]
        [ div verticalTextCss [ text sectionName ]
        , div [ style "display" "flex", style "height" "150px", style "margin-top" "300px", hideOnMobile ] [ verticalLine "#000000" ]
        ]


hideOnMobile : Attribute msg
hideOnMobile =
    BM.displayByDevice
        { mobile = BM.Hidden
        , tablet = BM.Flex
        , desktop = BM.Flex
        , widescreen = BM.Flex
        , fullHD = BM.Flex
        }


hideOnWideScreen : Attribute msg
hideOnWideScreen =
    BM.displayByDevice
        { mobile = BM.Flex
        , tablet = BM.Hidden
        , desktop = BM.Hidden
        , widescreen = BM.Hidden
        , fullHD = BM.Hidden
        }


tciLogo : Html msg
tciLogo =
    div [ style "background-color" "white", style "width" "100px", style "border-radius" "10px" ] [ a [ href "https://www.totceiubesc.ro/listing/servicii/indagra-etansare-si-voprire-antifoc/", Html.Attributes.target "_blank" ] [ img [ src "./tci.png", style "margin" "0px" ] [] ] ]


aboutText : Html msg
aboutText =
    div [ style "display" "flex", style "flex-direction" "column", style "align-items" "flex-end", style "text-align" "left" ]
        [ p [ style "padding-bottom" "2rem" ] [ text "INDAGRA este specializată în protecția pasivă la foc: termoprotecție cu vopsea termospumantă și torcret, precum și în producția de uși metalice. Cu peste 10 ani de experiență în domeniul de protecție pasivă la foc, firma asigură:" ]
        , div [ style "width" "40vw", style "text-align" "left", hideOnMobile, style "flex-direction" "column" ]
            aboutTextEnum
        , div [ style "width" "auto", style "text-align" "left", hideOnWideScreen, isHiddenFullHD, isHiddenWidescreen, style "flex-direction" "column" ]
            aboutTextEnum
        ]


aboutTextEnum : List (Html msg)
aboutTextEnum =
    [ p [ style "padding-bottom" "1rem" ] [ text "/ protecția cu vopsea termospumantă și torcret a structurilor din oțel împotriva incendiilor, " ]
    , p [ style "padding-bottom" "1rem" ] [ text "/ etanșarea antifoc între compartimente a trecerilor de cabluri, " ]
    , p [ style "padding-bottom" "1rem" ] [ text "/ țevi metalice, " ]
    , p [ style "padding-bottom" "1rem" ] [ text "/ conducte din material plastic și tubulaturi de ventilație, " ]
    , p [ style "padding-bottom" "3rem" ] [ text "/ confecționarea de uși metalice de dimensiuni standard și atipice cu termoizolație sau căptușite cu plumb împotriva radiațiilor. " ]
    , tciLogo
    ]


services : Model -> Html Msg
services model =
    section Spaced
        [ id "services" ]
        [ sectionTitle "SERVICII"
        , container []
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem", onMouseLeave <| SetHoveredServiceItem 0, style "display" "flex", style "align-items" "center" ]
                [ leftColumn "Servicii"
                , column (centerColumnModifier BM.Width10) [] [ serviceBoxes model, serviceBoxesMobile model ]
                , column (sideColumnModifier BM.Width1) [] []
                ]
            ]
        ]


isHiddenWidescreen : Attribute msg
isHiddenWidescreen =
    class "is-hidden-widescreen is-hidden-fullhd"


isHiddenFullHD : Attribute msg
isHiddenFullHD =
    class "is-hidden-fullhd"


serviceBoxes : Model -> Html Msg
serviceBoxes model =
    div [ style "display" "flex", style "flex-direction" "row", style "align-items" "center", style "justify-content" "center", hideOnMobile, style "cursor" "pointer" ]
        [ div (boxCss model 1) [ div [] [ text "EXECUȚIE DE LUCRĂRI DE TERMOPROTECȚIE" ], div [] [ img [ src "./lucrari_termoprotectie.svg" ] [] ] ]
        , div (boxCss model 2) [ div [] [ text "ETANȘAREA PENETRAȚIILOR DIN PEREȚI ȘI PLANȘEE CU MATERIAL TERMOSPUMANT" ], div [] [ img [ src "./etansarea_penetratiilor.svg" ] [] ] ]
        , div (boxCss model 3) [ div [] [ text "EXECUȚIE ȘI MONTAJ DE UȘI METALICE " ], div [] [ img [ src "./montaj_usi.svg" ] [] ] ]
        ]


serviceBoxesMobile : Model -> Html Msg
serviceBoxesMobile model =
    div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center", style "justify-content" "center", hideOnWideScreen, isHiddenFullHD, isHiddenWidescreen ]
        [ div (boxCssMobile model 1) [ div [] [ text "EXECUȚIE DE LUCRĂRI DE TERMOPROTECȚIE" ], div [] [ img [ src "./lucrari_termoprotectie.svg" ] [] ] ]
        , div (boxCssMobile model 2) [ div [] [ text "ETANȘAREA PENETRAȚIILOR DIN PEREȚI ȘI PLANȘEE CU MATERIAL TERMOSPUMANT" ], div [] [ img [ src "./etansarea_penetratiilor.svg" ] [] ] ]
        , div (boxCssMobile model 3) [ div [] [ text "EXECUȚIE ȘI MONTAJ DE UȘI METALICE " ], div [] [ img [ src "./montaj_usi.svg" ] [] ] ]
        ]


modalFrame : Model -> Html Msg
modalFrame model =
    let
        service =
            case model.clickedServiceItem of
                1 ->
                    termoProtection

                2 ->
                    sealing

                3 ->
                    metalicDoors model

                _ ->
                    metalicDoors model
    in
    modal model.isModalOpen
        []
        [ modalBackground [ onClick CloseModal ] []
        , modalContent [ style "backgroundColor" "white", style "width" "80vw" ]
            [ div [ style "display" "flex", style "align-items" "left" ] [ img [ src "./logo_indagra_black.svg", style "padding-left" "5vw", style "padding-top" "5vw", style "margin" "0", style "width" "20vmax" ] [] ]
            , service
            , div [ style "color" "black", style "margin-top" "10vh", style "margin-bottom" "10vh", style "display" "flex", style "justify-content" "center", hideOnMobile ] [ text "Copyright © 2020 INDAGRA SRL" ]
            , div [ style "color" "black", style "margin-top" "5vh", style "margin-bottom" "5vh", style "font-size" "8px", style "display" "flex", style "justify-content" "center", hideOnWideScreen, isHiddenWidescreen ] [ text "Copyright © 2020 INDAGRA SRL" ]
            ]
        , modalClose BM.Large [ onClick CloseModal, href "" ] []
        ]


termoProtection : Html Msg
termoProtection =
    div []
        [ columns myColumnsModifiers
            [ style "padding-top" "4rem", onMouseLeave <| SetHoveredServiceItem 0, style "display" "flex", style "align-items" "center" ]
            [ modalLeftColumn "Execuție de lucrări de termoprotecție"
            , column (centerColumnModifier BM.Width8)
                [ style "color" "#4d4d4d", style "display" "flex", style "text-align" "left", style "flex-direction" "column", style "line-height" "1.5rem" ]
                [ div [ style "color" "#DB2E54", style "font-size" "42px", style "font-weight" "bold", style "line-height" "2.5rem", style "margin-bottom" "1rem", style "width" "60%", hideOnMobile ] [ text "Execuție De Lucrări De Termoprotecție" ]
                , div [ style "color" "#DB2E54", style "font-size" "18px", style "font-weight" "bold", style "line-height" "1.5rem", style "margin-bottom" "1rem", style "width" "100%", hideOnWideScreen, isHiddenWidescreen ] [ text "Execuție De Lucrări De Termoprotecție" ]
                , div [ style "margin-bottom" "3rem", style "width" "45%", style "font-size" "18px", hideOnMobile ] [ text "Protecția structurilor metalice cu vopsele termospumante sau torcret" ]
                , div [ style "margin-bottom" "3rem", style "width" "100%", style "font-size" "12px", style "line-height" "1.0rem", hideOnWideScreen, isHiddenWidescreen ] [ text "Protecția structurilor metalice cu vopsele termospumante sau torcret" ]
                , div [ style "color" "#DB2E54", style "font-size" "32px", style "line-height" "2.0rem", style "margin-bottom" "1rem", style "font-weight" "bold", style "width" "55%", hideOnMobile ] [ text "Protecția stucturilor din oțel împotriva incendiilor cu ajutorul vopselelor termospumante" ]
                , div [ style "color" "#DB2E54", style "font-size" "18px", style "line-height" "1.5rem", style "margin-bottom" "1rem", style "font-weight" "bold", style "width" "100%", hideOnWideScreen, isHiddenWidescreen ] [ text "Protecția stucturilor din oțel împotriva incendiilor cu ajutorul vopselelor termospumante" ]
                , div [ style "margin-bottom" "2rem", hideOnMobile ] [ titleLine "#DB2E54" ]
                , div [ style "margin-bottom" "3rem", style "font-size" "18px", style "text-align" "justify", style "width" "45%", hideOnMobile ] [ text "La temperaturi mai mari de 500°C rezistența stucturilor din oțel se reduce în mod însemnat. Structurile portante își pot pierde funcția lor inițială, astfel încît clădirea se poate prăbuși parțial sau total. Sistemele noastre de vopsele termospumante (pe bază de apă sau solvent) încep să-și facă efectul începând de la 180-200°C, formând pe suprafața oțelului un strat special de spumă care împiedică încălzirea acestuia la temperatura critică." ]
                , div [ style "margin-bottom" "1rem", style "font-size" "12px", style "text-align" "left", style "width" "100%", style "line-height" "1rem", hideOnWideScreen, isHiddenWidescreen ] [ text "La temperaturi mai mari de 500°C rezistența stucturilor din oțel se reduce în mod însemnat. Structurile portante își pot pierde funcția lor inițială, astfel încît clădirea se poate prăbuși parțial sau total. Sistemele noastre de vopsele termospumante (pe bază de apă sau solvent) încep să-și facă efectul începând de la 180-200°C, formând pe suprafața oțelului un strat special de spumă care împiedică încălzirea acestuia la temperatura critică." ]
                , div [] [ img [ src "./illustration_termoprotectie.svg", style "padding-left" "20px" ] [] ]
                ]
            ]
        ]


sealing : Html Msg
sealing =
    div []
        [ columns myColumnsModifiers
            [ style "padding-top" "4rem", onMouseLeave <| SetHoveredServiceItem 0, style "display" "flex", style "align-items" "center" ]
            [ modalLeftColumn "Etanșarea penetrațiilor din pereți și planșee cu material termospumant"
            , column (centerColumnModifier BM.Width8)
                [ style "color" "#4d4d4d", style "display" "flex", style "text-align" "left", style "flex-direction" "column", style "line-height" "1.5rem" ]
                [ div [ style "color" "#DB2E54", style "font-size" "42px", style "font-weight" "bold", style "line-height" "2.5rem", style "margin-bottom" "3rem", style "width" "70%", hideOnMobile ] [ text "Etanșarea penetrațiilor din pereți și planșee cu material termospumant" ]
                , div [ style "color" "#DB2E54", style "font-size" "18px", style "font-weight" "bold", style "line-height" "1.5rem", style "margin-bottom" "1rem", hideOnWideScreen, isHiddenWidescreen ] [ text "Etanșarea penetrațiilor din pereți și planșee cu material termospumant" ]
                , sealingType "Etașarea antifoc tip „Combi” a trecerilor de cabluri, țevilor metalice, conductelor din material plastic" "Formarea acestei închideri este asemănătoare cu cea a trecerilor de cabluri. Diferă numai în privința utilizării materialelor care își măresc volumul la căldură, respectiv a eventualelor altor sisteme, cum sunt de exemplu, coliere antifoc, laminate etc., în funcție de conductele care străbat peretele." "./etansare/02_illustration_etansare_combi.svg"
                , sealingType "Etanșarea trecerilor de conducte din material plastic, cu ajutorul colierelor antifoc" "Colierul antifoc, montat în jurul țevii, oferă o protecție simplă dar eficientă. La scurt timp după izbucnirea focului își mărește volumul în asemenea măsură, încât, datorită umpluturii speciale, va strangula- va tăia- complet țeava din material plastic, formând astfel un obstacol în calea extinderii incendiului." "./etansare/01_illustration_etansare_colier.svg"
                , sealingType "Etanșarea antifoc a trecerilor de conducte din material plastic – laminate încorporabile" "Asemănător colierelor, acest sistem împiedică propagarea focului prin ștrangularea conductei și umplerea integrală a deschiderii cu un material special." "./etansare/03_illustration_etansare_plastic_laminat.svg"
                , sealingType "Etanșarea tecerilor de cabluri" "Cu așa-numită etanșare moale poate fi creat un compartiment de incendiu corespunzător, atât în cazul planșeelor cât și în cazul pereților, prin utilizarea de plăci tari de vată minerală, respectiv de vopsea și chit care formează o spumă când sunt încălzite.Este potrivit pentru etanșarea a unui număr mare de treceri de diferite tipuri și dimensiuni. Suprafața închiderii este flexibilă, deci permite eventuala reparare sau deschidera sistemului, amplasarea ulterioară de cabluri." "./etansare/04_illustration_etansare_cabluri.svg"
                ]
            ]
        ]


sealingType : String -> String -> String -> Html Msg
sealingType title content imageSrc =
    div []
        [ div [ style "color" "#DB2E54", style "font-size" "24px", style "font-weight" "bold", style "line-height" "1.5rem", style "margin-bottom" "1rem", style "width" "70%", hideOnMobile ] [ text title ]
        , div [ style "color" "#DB2E54", style "font-size" "14px", style "font-weight" "bold", style "line-height" "1.0rem", style "margin-bottom" "1rem", hideOnWideScreen, isHiddenWidescreen ] [ text title ]
        , div [ style "margin-bottom" "2rem", hideOnMobile ] [ titleLine "#DB2E54" ]
        , div [ style "margin-bottom" "3rem", style "font-size" "16px", style "text-align" "justify", style "width" "60%", hideOnMobile ] [ text content ]
        , div [ style "margin-bottom" "3rem", style "font-size" "12px", style "text-align" "left", style "line-height" "1.0rem", hideOnWideScreen, isHiddenWidescreen ] [ text content ]
        , div [] [ img [ src imageSrc, style "padding-left" "20px", style "margin-bottom" "5rem" ] [] ]
        ]


metalicDoors : Model -> Html Msg
metalicDoors model =
    div []
        [ columns myColumnsModifiers
            [ style "padding-top" "4rem", onMouseLeave <| SetHoveredServiceItem 0, style "display" "flex", style "align-items" "center" ]
            [ modalLeftColumn "Execuție și montaj de uși metalice"
            , column (centerColumnModifier BM.Width8)
                [ style "color" "#4d4d4d", style "display" "flex", style "text-align" "left", style "flex-direction" "column", style "line-height" "1.5rem" ]
                [ div [ style "color" "#DB2E54", style "font-size" "42px", style "font-weight" "bold", style "line-height" "2.5rem", style "margin-bottom" "1rem", style "width" "60%", hideOnMobile ] [ text "Execuție și montaj de uși metalice" ]
                , div [ style "color" "#DB2E54", style "font-size" "18px", style "font-weight" "bold", style "line-height" "1.5rem", style "margin-bottom" "1rem", hideOnWideScreen, isHiddenWidescreen ] [ text "Execuție și montaj de uși metalice" ]
                , div [ style "margin-bottom" "1rem", style "width" "45%", style "font-size" "18px", hideOnMobile ] [ text "-cu termoizolație" ]
                , div [ style "font-size" "12px", hideOnWideScreen, isHiddenWidescreen ] [ text "-cu termoizolație" ]
                , div [ style "margin-bottom" "3rem", style "width" "45%", style "font-size" "18px", hideOnMobile ] [ text "-cu plumb împotriva radiațiilor" ]
                , div [ style "margin-bottom" "1rem", style "font-size" "12px", hideOnWideScreen, isHiddenWidescreen ] [ text "-cu plumb împotriva radiațiilor" ]
                , div [ style "width" "80%", style "margin-bottom" "3rem", style "font-size" "18px", style "text-align" "justify", style "width" "65%", hideOnMobile ] [ text "Se asigură transport, montaj, garanție și service." ]
                , div [ style "margin-bottom" "1rem", style "font-size" "12px", style "text-align" "left", hideOnWideScreen, isHiddenWidescreen ] [ text "Se asigură transport, montaj, garanție și service." ]
                , div [] [ img [ src "./montaj/illustration_usa.svg", style "padding-left" "20px" ] [] ]
                ]
            ]
        , div [ style "display" "flex", style "justify-content" "center", hideOnMobile ] [ imageSlider model ]
        ]


imageSlider : Model -> Html Msg
imageSlider model =
    div [ style "color" "black", style "display" "flex", style "align-items" "center", style "justify-content" "center", style "margin-top" "20vh" ]
        [ Html.map ImageGalleryMsg <|
            Gallery.view imageConfig model.imageGallery [ Gallery.Arrows ] imageSlides
        ]


imageSlides : List ( String, Html msg )
imageSlides =
    List.map (\x -> ( x, Image.slide x Image.Cover )) images


imageConfig : Gallery.Config
imageConfig =
    Gallery.config
        { id = "image-gallery"
        , transition = 500
        , width = Gallery.px 794
        , height = Gallery.px 452
        }


images : List String
images =
    [ "./montaj/indagra_usa_1.png"
    , "./montaj/indagra_usa_2.png"
    , "./montaj/indagra_usa_3.png"
    , "./montaj/indagra_usa_4.png"
    , "./montaj/indagra_usa_5.png"
    , "./montaj/indagra_usa_6.png"
    , "./montaj/indagra_usa_7.png"
    , "./montaj/indagra_usa_8.png"
    ]


boxCss : Model -> Int -> List (Attribute Msg)
boxCss model setting =
    let
        serviceType =
            case setting of
                1 ->
                    TermoProtection

                2 ->
                    Sealing

                _ ->
                    MetalicDoors
    in
    [ onMouseOver <| SetHoveredServiceItem setting
    , onClick (OpenModal serviceType)
    , if model.hoveredServiceItem == setting then
        style "background-color" "#DB2E54"

      else
        style "background-color" "#141414"
    , style "width" "20vw"
    , style "height" "20vw"
    , style "margin" "1vw"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "20px"
    , style "border-radius" "5px"
    , style "flex-direction" "column"
    ]


boxCssMobile : Model -> Int -> List (Attribute Msg)
boxCssMobile model setting =
    let
        serviceType =
            case setting of
                1 ->
                    TermoProtection

                2 ->
                    Sealing

                _ ->
                    MetalicDoors
    in
    [ onMouseOver <| SetHoveredServiceItem setting
    , onClick (OpenModal serviceType)
    , if model.hoveredServiceItem == setting then
        style "background-color" "#DB2E54"

      else
        style "background-color" "#141414"
    , style "width" "60vw"
    , style "height" "60vw"
    , style "margin" "1vw"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "20px"
    , style "border-radius" "5px"
    , style "flex-direction" "column"
    ]


portofolio : Html msg
portofolio =
    section Spaced
        [ id "portofolio" ]
        [ sectionTitle "PORTOFOLIU"
        , container []
            [ columns myColumnsModifiers
                [ style "padding-top" "4rem", style "display" "flex", style "align-items" "center" ]
                [ leftColumn "Portofoliu"
                , column (centerColumnModifier BM.Width10) [] [ portofolioText, portofolioTextMobile ]
                , column (sideColumnModifier BM.Width1) [] []
                ]
            ]
        ]


squareCss : List (Attribute msg)
squareCss =
    [ style "width" "350px"
    , style "height" "350px"
    , style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "font-size" "24px"
    , style "background-color" "#14171F"
    , style "margin-top" "100px"
    , style "margin-top" "100px"
    , style "padding" "100px"
    ]


squareCssMobile : List (Attribute msg)
squareCssMobile =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "font-size" "16px"
    , style "background-color" "#14171F"
    , style "margin-top" "100px"
    , style "margin-top" "0px"
    , style "padding" "20px"
    ]


projectsCss : List (Attribute msg)
projectsCss =
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "align-items" "left"
    , style "flex-direction" "column"
    , style "height" "350px"
    , style "margin-top" "100px"
    , style "text-align" "left"
    , style "background-color" "#14171F"
    ]


isFlexWidescreen : Attribute msg
isFlexWidescreen =
    class "is-flex-widescreen"


isFlexFullHD : Attribute msg
isFlexFullHD =
    class "is-flex-fullhd"


portofolioText : Html msg
portofolioText =
    columns myColumnsModifiers
        [ hideOnMobile ]
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


portofolioTextMobile : Html msg
portofolioTextMobile =
    div
        [ hideOnWideScreen, isHiddenWidescreen, style "flex-direction" "column" ]
        [ div squareCssMobile
            [ text "LUCRĂRI DE TERMOPROTECȚIE CU VOPSEA TERMOSPUMANTĂ"
            , div [ style "font-size" "12px", style "margin-top" "2rem", style "color" "#FFFFFF", style "text-align" "left" ]
                [ p [] [ text "/ HOTEL TELEFERIC POIANA BRAȘOV" ]
                , p [] [ text "/ HALA ERTEX BRAȘOV" ]
                , p [] [ text "/ HALA KATADYN PREJMER" ]
                , p [] [ text "/ HALA DIETAL CODLEA" ]
                , p [] [ text "/ HALA REHOMA PITEȘTI" ]
                , p [] [ text "/ GALERII AUCHAN TG. MUREȘ" ]
                , p [] [ text "/ BCR SILVER MOUNTAIN POIANA BRAȘOV" ]
                , p [] [ text "/ CLĂDIRE ONE CHARLES DE GAULLE BUCUREȘTI" ]
                , p [] [ text "/ SPITALUL ONCOLOGIC BRAȘOV" ]
                , p [] [ text "/ HALA RECOBOL BRAȘOV" ]
                , p [] [ text "/ PARCĂRI SUPRATERANE PREDEAL" ]
                ]
            ]
        , div squareCssMobile
            [ text "LUCRĂRI DE ETANȘARE ANTIFOC"
            , div [ style "font-size" "12px", style "margin-top" "2rem", style "color" "#FFFFFF", style "text-align" "left" ]
                [ p [] [ text "/ KRONOSPAN BRAȘOV" ]
                , p [] [ text "/ SPITALUL VICTOR GOMOIU BUCUREȘTI" ]
                , p [] [ text "/ PARCARE REGINA MARIA BRAȘOV" ]
                , p [] [ text "/ HALA DIETAL CODLEA" ]
                , p [] [ text "/ HALA ZOLLNER SATU MARE" ]
                , p [] [ text "/ HALA QUIN BRAȘOV" ]
                , p [] [ text "/ CLĂDIRI DE BIROURI CORESI BUSSINES PARK BRAȘOV" ]
                , p [] [ text "/ AUCHAN SATU MARE" ]
                , p [] [ text "/ HALA RUBITECH PREJMER" ]
                , p [] [ text "/ CLĂDIRE CARPATEX BRAȘOV" ]
                , p [] [ text "/ AUTOLIV BRAȘOV, ROVINARI, REȘIȚA" ]
                , p [] [ text "/ HALA BELLMAN BRAȘOV" ]
                , p [] [ text "/ CENTRUL COM. SHOPPING CITY SIBIU" ]
                , p [] [ text "/ SPITALUL SF. CONSTANTIN BRAȘOV" ]
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
    section Spaced
        [ id "contact" ]
        [ sectionTitle "CONTACT"
        , container [ style "margin-top" "4rem" ]
            [ columns myColumnsModifiers
                [ style "display" "flex", style "align-items" "center" ]
                [ leftColumn "Contact"
                , column (centerColumnModifier BM.Width10)
                    []
                    [ contactText
                    ]
                , column (sideColumnModifier BM.Width1) [] []
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
