-module(rebar3_openapi_generator).

-doc """
Main entry point for the rebar3_openapi_generator plugin.
This module initializes all providers for the plugin.
""".

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% Initialize the openapi generate provider
    {ok, State1} = rebar3_openapi:init(State),
    %% Initialize the openapi import provider
    {ok, State2} = rebar3_openapi_import:init(State1),
    {ok, State2}.
