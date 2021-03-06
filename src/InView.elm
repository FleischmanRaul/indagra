module InView exposing
    ( check
    , checkWithOffset
    , checkAlt
    , checkAltWithOffset
    , State
    , Msg
    , init
    , update
    , updateViewportOffset
    , subscriptions
    , addElements
    )

{-|


# Detect

Detect if an element is visible in the current viewport.

@docs check
@docs checkWithOffset
@docs checkAlt
@docs checkAltWithOffset


# Definitions

@docs State
@docs Msg


# Init

@docs init


# Update

@docs update
@docs updateViewportOffset
@docs subscriptions
@docs addElements

-}

import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Task



-- DEFINITIONS


type alias Element =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Viewport =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Keeps track of viewport position, viewport dimensions and element positions.
-}
type State
    = State
        { elements : Dict String Element
        , viewport : Viewport
        }


{-| Takes the list of element ids you want to keep track of and attempts to find them
in the DOM.
-}
init : List String -> ( State, Cmd Msg )
init elementIds =
    ( State
        { elements = Dict.empty
        , viewport = Viewport 0 0 0 0
        }
    , Cmd.batch
        [ Task.attempt GotViewport Dom.getViewport
        , addElements elementIds
        ]
    )


{-| Add elements you'd like to be able to detect after you've initialized the state.
-}
addElements : List String -> Cmd Msg
addElements elementIds =
    Cmd.batch <|
        List.map getPosition elementIds


getPosition : String -> Cmd Msg
getPosition id =
    Task.attempt (GotElementPosition id) <|
        Dom.getElement id



-- SUBSCRIPTIONS


{-| Subscribes to browser resize events and recalculates element positions.
-}
subscriptions : State -> Sub Msg
subscriptions state =
    Browser.Events.onResize OnBrowserResize



-- UPDATE


{-| A message type for the state to update.
-}
type Msg
    = GotViewport (Result () Dom.Viewport)
    | GotElementPosition String (Result Dom.Error Dom.Element)
    | OnBrowserResize Int Int


{-| Update viewport size and element positions.
-}
update : Msg -> State -> ( State, Cmd Msg )
update msg (State ({ viewport } as state)) =
    case msg of
        GotViewport (Ok vp) ->
            ( State { state | viewport = vp.viewport }, Cmd.none )

        GotViewport (Err err) ->
            ( State state, Cmd.none )

        GotElementPosition id (Ok { element }) ->
            ( State
                { state
                    | elements =
                        Dict.insert id element state.elements
                }
            , Cmd.none
            )

        GotElementPosition id (Err err) ->
            ( State { state | elements = Dict.remove id state.elements }
            , Cmd.none
            )

        OnBrowserResize width height ->
            ( State
                { state
                    | viewport =
                        { viewport
                            | width = toFloat width
                            , height = toFloat height
                        }
                }
            , addElements (Dict.keys state.elements)
            )


{-| Update current viewport x and y offset. Use this function to update the viewport
scroll position.
-}
updateViewportOffset : Float -> Float -> State -> State
updateViewportOffset x y (State ({ viewport } as state)) =
    State { state | viewport = { viewport | x = x, y = y } }



-- DETECT


{-| True if the element with the given id is in the current viewport.
_note: this is a Maybe because the element might not be on the page at all._
![check](https://rl-king.github.io/elm-inview-example/illustrations/inView.svg)
-}
check : String -> State -> Maybe Bool
check id state =
    checkWithOffset id 0 220 state


{-| True if the element with the given id is in the current viewport but with an x and y offset.
A positive offset will make the viewport smaller and vice versa.
_note: this is a Maybe because the element might not be on the page at all._
![checkWithOffset](https://rl-king.github.io/elm-inview-example/illustrations/inViewWithOffset.svg)
-}
checkWithOffset : String -> Float -> Float -> State -> Maybe Bool
checkWithOffset id offsetX offsetY (State { elements, viewport }) =
    let
        calc element =
            (viewport.y + offsetY < element.y + element.height)
                && (viewport.y + viewport.height - offsetY > element.y)
                && (viewport.x + offsetX < element.x + element.width)
                && (viewport.x + viewport.width - offsetX > element.x)
    in
    Maybe.map calc (Dict.get id elements)


{-| True if the element with the given id is in _or_ above the current viewport.
_note: this is a Maybe because the element might not be on the page at all._
![checkAlt](https://rl-king.github.io/elm-inview-example/illustrations/inViewAlt.svg)
-}
checkAlt : String -> State -> Maybe Bool
checkAlt id state =
    checkAltWithOffset id 0 0 state


{-| True if the element with the given id is in _or_ above the current viewport but with an x and y offset.
A positive offset will make the viewport smaller and vice versa.
_note: this is a Maybe because the element might not be on the page at all._
![checkAltWithOffset](https://rl-king.github.io/elm-inview-example/illustrations/inViewAltWithOffset.svg)
-}
checkAltWithOffset : String -> Float -> Float -> State -> Maybe Bool
checkAltWithOffset id offsetX offsetY (State { elements, viewport }) =
    let
        calc element =
            (viewport.y - offsetY + viewport.height > element.y)
                && (viewport.x - offsetX + viewport.width > element.x)
    in
    Maybe.map calc (Dict.get id elements)



-- DISTANCE
-- {-| The distance of the element's center to viewport center in px.
-- -}
-- type alias CenterDistance =
--     { x : Float
--     , y : Float
--     }
-- {-| Distance from the center of the viewport, where the `x` value is relative to
-- the vertical center and `y` to the horizontal.
-- Useful for creating positional effects relative to the viewport
-- ![centerDistance](https://rl-king.github.io/elm-inview-example/illustrations/centerDistance.svg)
-- -}
-- centerDistance : String -> State -> Maybe CenterDistance
-- centerDistance id (State { elements, viewport }) =
--     let
--         calc element =
--             { x =
--                 (element.x + element.width / 2)
--                     - (viewport.x + viewport.width / 2)
--             , y =
--                 (element.y + element.height / 2)
--                     - (viewport.y + viewport.height / 2)
--             }
--     in
--     Maybe.map calc (Dict.get id elements)
