-module(test_resource_handler).

-doc """
Test handler using resource module pattern for metadata organization.
Demonstrates clean separation between handler logic and API documentation.
""".

-behaviour(trails_handler).

-export([trails/0, init/2]).

%% Trails definition using resource module
trails() ->
    [
        trails:trail("/api/health", test_resource_handler, [], test_resource_metadata:health()),
        trails:trail("/api/users", test_resource_handler, [], test_resource_metadata:users_collection()),
        trails:trail("/api/users/:id", test_resource_handler, [], test_resource_metadata:user_by_id())
    ].

%% Minimal cowboy handler implementation
init(Req, State) ->
    {ok, Req, State}.
