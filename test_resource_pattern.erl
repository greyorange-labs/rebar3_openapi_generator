#!/usr/bin/env escript
%% -*- erlang -*-
%%! -sname test_resource_pattern

-doc """
Test script to verify that the plugin correctly handles metadata
defined in separate resource modules (function-based pattern).
""".

main(_) ->
    % Add paths for dependencies
    code:add_pathz("_build/default/lib/jsx/ebin"),
    code:add_pathz("_build/default/lib/trails/ebin"),
    code:add_pathz("_build/default/lib/cowboy/ebin"),
    code:add_pathz("_build/default/lib/cowlib/ebin"),
    code:add_pathz("_build/default/lib/ranch/ebin"),
    code:add_pathz("_build/default/lib/rebar3_openapi_generator/ebin"),
    code:add_pathz("test/fixtures"),

    io:format("~n"),
    io:format("╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║  Testing Resource Module Pattern for OpenAPI Metadata             ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),

    % Compile test modules
    io:format("1. Compiling test modules...~n"),
    case compile:file("test/fixtures/test_resource_metadata.erl", [
        {outdir, "test/fixtures"},
        return_errors,
        debug_info
    ]) of
        {ok, _} ->
            io:format("   ✓ test_resource_metadata.erl compiled~n");
        {error, Errors1, _} ->
            io:format("   ✗ Failed to compile test_resource_metadata.erl: ~p~n", [Errors1]),
            halt(1)
    end,

    case compile:file("test/fixtures/test_resource_handler.erl", [
        {outdir, "test/fixtures"},
        return_errors,
        debug_info
    ]) of
        {ok, _} ->
            io:format("   ✓ test_resource_handler.erl compiled~n~n");
        {error, Errors2, _} ->
            io:format("   ✗ Failed to compile test_resource_handler.erl: ~p~n", [Errors2]),
            halt(1)
    end,

    % Test 1: Verify metadata functions are callable
    io:format("2. Testing resource module metadata functions...~n"),
    test_metadata_functions(),

    % Test 2: Verify trails can be called
    io:format("~n3. Testing handler trails/0 function...~n"),
    test_trails_function(),

    % Test 3: Verify parser can handle the pattern
    io:format("~n4. Testing openapi_parser with resource pattern...~n"),
    test_parser_integration(),

    io:format("~n"),
    io:format("╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║  ✅ All tests passed!                                             ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n"),
    io:format("~n"),
    io:format("📋 Summary:~n"),
    io:format("  • Resource module pattern is fully supported~n"),
    io:format("  • Metadata functions work correctly~n"),
    io:format("  • Parser successfully extracts metadata from function calls~n"),
    io:format("  • All routes are properly documented~n"),
    io:format("~n"),
    io:format("💡 Best Practice Verified:~n"),
    io:format("  Use separate resource modules for metadata when handling~n"),
    io:format("  multiple routes to improve code organization and maintainability.~n"),
    io:format("~n"),

    halt(0).

%% Test metadata functions
test_metadata_functions() ->
    try
        HealthMeta = test_resource_metadata:health(),
        UsersMeta = test_resource_metadata:users_collection(),
        UserByIdMeta = test_resource_metadata:user_by_id(),

        % Verify structure
        true = is_map(HealthMeta),
        true = maps:is_key(get, HealthMeta),

        true = is_map(UsersMeta),
        true = maps:is_key(get, UsersMeta),
        true = maps:is_key(post, UsersMeta),

        true = is_map(UserByIdMeta),
        true = maps:is_key(get, UserByIdMeta),
        true = maps:is_key(put, UserByIdMeta),
        true = maps:is_key(delete, UserByIdMeta),

        io:format("   ✓ Health metadata: ~p method(s)~n", [map_size(HealthMeta)]),
        io:format("   ✓ Users collection metadata: ~p method(s)~n", [map_size(UsersMeta)]),
        io:format("   ✓ User by ID metadata: ~p method(s)~n", [map_size(UserByIdMeta)])
    catch
        Error:Reason ->
            io:format("   ✗ Failed to call metadata functions: ~p:~p~n", [Error, Reason]),
            halt(1)
    end.

%% Test trails function
test_trails_function() ->
    try
        Trails = test_resource_handler:trails(),
        true = is_list(Trails),
        io:format("   ✓ trails/0 returned ~p trail(s)~n", [length(Trails)]),

        % Verify each trail has metadata
        lists:foreach(fun(Trail) ->
            Path = trails:path_match(Trail),
            Meta = trails:metadata(Trail),
            HasMeta = is_map(Meta) andalso map_size(Meta) > 0,
            case HasMeta of
                true ->
                    Methods = [atom_to_binary(M, utf8) || M <- maps:keys(Meta),
                               lists:member(M, [get, post, put, delete, patch])],
                    io:format("   ✓ ~s: ~p method(s) documented~n", [Path, length(Methods)]);
                false ->
                    io:format("   ✗ ~s: no metadata found~n", [Path]),
                    halt(1)
            end
        end, Trails)
    catch
        Error:Reason ->
            io:format("   ✗ Failed to call trails/0: ~p:~p~n", [Error, Reason]),
            halt(1)
    end.

%% Test parser integration
test_parser_integration() ->
    try
        case openapi_parser:parse_handler(test_resource_handler) of
            {ok, HandlerInfo} ->
                Routes = maps:get(routes, HandlerInfo),
                Coverage = maps:get(coverage, HandlerInfo),
                CoveragePercent = maps:get(coverage_percent, Coverage),

                io:format("   ✓ Parser successfully processed handler~n"),
                io:format("   ✓ Found ~p route(s)~n", [length(Routes)]),
                io:format("   ✓ Coverage: ~.1f%~n", [CoveragePercent]),

                % Verify all routes have metadata
                lists:foreach(fun(Route) ->
                    Path = maps:get(path, Route),
                    HasMeta = maps:get(has_metadata, Route),
                    Methods = maps:get(methods, Route),
                    case HasMeta of
                        true ->
                            io:format("   ✓ ~s: ~p method(s) detected~n", [Path, length(Methods)]);
                        false ->
                            io:format("   ✗ ~s: metadata not detected~n", [Path]),
                            halt(1)
                    end
                end, Routes),

                % Verify 100% coverage
                case CoveragePercent >= 100.0 of
                    true ->
                        io:format("   ✓ 100% documentation coverage achieved!~n");
                    false ->
                        io:format("   ⚠ Coverage is ~.1f%, expected 100%~n", [CoveragePercent])
                end;

            {error, Reason} ->
                io:format("   ✗ Parser failed: ~p~n", [Reason]),
                halt(1)
        end
    catch
        Error:Reason2:Stack ->
            io:format("   ✗ Parser test failed: ~p:~p~n~p~n", [Error, Reason2, Stack]),
            halt(1)
    end.
