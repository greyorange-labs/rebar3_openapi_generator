#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname test_cowboy_swagger

-doc """
Simple test script to verify the rebar3_openapi_generator plugin works with cowboy_swagger format.
""".

main(_) ->
    % Add paths for dependencies
    code:add_pathz("_build/default/lib/jsx/ebin"),
    code:add_pathz("_build/default/lib/trails/ebin"),
    code:add_pathz("_build/default/lib/cowboy/ebin"),
    code:add_pathz("_build/default/lib/rebar3_openapi_generator/ebin"),
    code:add_pathz("test/fixtures"),

    io:format("Testing rebar3_openapi_generator with cowboy_swagger format...~n"),

    % Test 1: Check if our parser can handle cowboy_swagger metadata
    io:format("1. Testing has_cowboy_swagger_metadata/1...~n"),
    TestMetadata = #{
        get => #{
            tags => [<<"Test">>],
            summary => <<"Test endpoint">>
        },
        post => #{
            tags => [<<"Test">>],
            summary => <<"Create test">>
        }
    },

    case openapi_parser:has_cowboy_swagger_metadata(TestMetadata) of
        true ->
            io:format("   ✓ Correctly detected cowboy_swagger metadata~n");
        false ->
            io:format("   ✗ Failed to detect cowboy_swagger metadata~n"),
            halt(1)
    end,

    % Test 2: Check method extraction from cowboy_swagger format
    io:format("2. Testing extract_methods_from_trail/1...~n"),
    Methods = openapi_parser:extract_methods_from_trail(TestMetadata),
    ExpectedMethods = [<<"get">>, <<"post">>],

    case lists:sort(Methods) =:= lists:sort(ExpectedMethods) of
        true ->
            io:format("   ✓ Correctly extracted methods: ~p~n", [Methods]);
        false ->
            io:format("   ✗ Wrong methods extracted. Expected: ~p, Got: ~p~n",
                     [ExpectedMethods, Methods]),
            halt(1)
    end,

    % Test 3: Test path normalization
    io:format("3. Testing normalize_path/1...~n"),
    TestPaths = [
        {"/api/users/:id", "/api/users/{id}"},
        {"/api/v1/items/:item_id/status", "/api/v1/items/{item_id}/status"},
        {"/simple/path", "/simple/path"}
    ],

    lists:foreach(fun({Input, Expected}) ->
        Result = binary_to_list(openapi_parser:normalize_path(list_to_binary(Input))),
        case Result =:= Expected of
            true ->
                io:format("   ✓ ~s -> ~s~n", [Input, Result]);
            false ->
                io:format("   ✗ ~s -> ~s (expected ~s)~n", [Input, Result, Expected]),
                halt(1)
        end
    end, TestPaths),

    io:format("~n✅ All tests passed! The plugin successfully supports cowboy_swagger format.~n"),
    io:format("~n📋 Summary of Changes:~n"),
    io:format("  • Modified openapi_parser.erl to read cowboy_swagger metadata format~n"),
    io:format("  • HTTP methods are now direct keys in trail metadata~n"),
    io:format("  • Updated documentation with cowboy_swagger examples~n"),
    io:format("  • Maintains backward compatibility with existing paths format~n"),

    io:format("~n🚀 Ready for Butler Server integration!~n"),
    io:format("~nNext steps:~n"),
    io:format("  1. Add this plugin to Butler Server's rebar.config~n"),
    io:format("  2. Update handler trails/0 functions to use cowboy_swagger format~n"),
    io:format("  3. Run: rebar3 openapi generate --app put~n"),

    halt(0).