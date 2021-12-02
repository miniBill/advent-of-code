module Memoize exposing (memoized)

import Dict exposing (Dict)
import State exposing (State)


memoized : (comparable -> ( List comparable, Dict comparable result -> result )) -> comparable -> result
memoized f x =
    State.finalValue Dict.empty (memoizedState f x)


memoizedState : (comparable -> ( List comparable, Dict comparable result -> result )) -> comparable -> State (Dict comparable result) result
memoizedState f key =
    State.embed (Dict.get key)
        |> State.andThen
            (\got ->
                case got of
                    Just result ->
                        State.state result

                    Nothing ->
                        let
                            ( requirements, continuation ) =
                                f key
                        in
                        requirements
                            |> State.traverse (memoizedState f)
                            |> State.andThen
                                (\_ ->
                                    State.advance
                                        (\state ->
                                            let
                                                result =
                                                    continuation state
                                            in
                                            ( result, Dict.insert key result state )
                                        )
                                )
            )
