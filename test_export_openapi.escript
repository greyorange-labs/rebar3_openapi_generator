#!/usr/bin/env escript
%%! -pa _build/default/lib/rebar3_openapi_generator/ebin -pa _build/default/lib/yamerl/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/trails/ebin -pa _build/default/lib/cowboy/ebin -pa _build/default/lib/cowlib/ebin -pa _build/default/lib/ranch/ebin -pa test/fixtures

%%%-------------------------------------------------------------------
%%% @doc Test script for Erlang → OpenAPI YAML generation
%%% Tests: Parser + OpenAPI spec generation from Erlang handler code
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main([]) ->
    io:format("Usage: ~s <handler_module>~n", [escript:script_name()]),
    io:format("Example: ~s test_cowboy_swagger_handler~n", [escript:script_name()]),
    halt(1);

main([HandlerModuleStr]) ->
    HandlerModule = list_to_atom(HandlerModuleStr),

    io:format("~n╔═══════════════════════════════════════════════════════════════════╗~n"),
    io:format("║     Test: Erlang Code → OpenAPI YAML Generation                  ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("Handler Module: ~s~n~n", [HandlerModule]),

    % Ensure test module is compiled and loaded
    case erlang:function_exported(HandlerModule, trails, 0) of
        false ->
            io:format("Compiling test module...~n"),
            TestFile = "test/fixtures/" ++ atom_to_list(HandlerModule) ++ ".erl",
            case compile:file(TestFile, [{outdir, "test/fixtures"}, return_errors, debug_info]) of
                {ok, _} ->
                    io:format("  ✓ Module compiled~n"),
                    code:purge(HandlerModule),
                    code:load_file(HandlerModule);
                {error, Errors, _} ->
                    io:format("  ✗ Failed to compile: ~p~n", [Errors]),
                    halt(1)
            end;
        true ->
            io:format("Module already loaded~n")
    end,
    io:format("~n"),

    % Step 1: Test Parser Functionality
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Step 1: Testing Parser Functionality~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

    % Test cowboy_swagger format detection
    TestMetadata = #{
        get => #{tags => [<<"Test">>], summary => <<"Test endpoint">>},
        post => #{tags => [<<"Test">>], summary => <<"Create test">>}
    },

    io:format("Testing cowboy_swagger format detection...~n"),
    case openapi_parser:has_cowboy_swagger_metadata(TestMetadata) of
        true -> io:format("  ✓ Cowboy swagger format detection~n");
        false ->
            io:format("  ✗ Failed to detect cowboy_swagger metadata~n"),
            halt(1)
    end,

    % Test method extraction
    io:format("Testing method extraction...~n"),
    Methods = openapi_parser:extract_methods_from_trail(TestMetadata),
    case lists:sort(Methods) =:= lists:sort([<<"get">>, <<"post">>]) of
        true -> io:format("  ✓ Method extraction: ~s~n",
                         [string:join([binary_to_list(M) || M <- Methods], ", ")]);
        false ->
            io:format("  ✗ Method extraction failed~n"),
            halt(1)
    end,

    % Test path normalization
    io:format("Testing path normalization...~n"),
    TestPaths = [
        {"/api/users/:id", "/api/users/{id}"},
        {["/api/", ':id', "/details"], "/api/{id}/details"}
    ],
    lists:foreach(fun({Input, Expected}) ->
        Result = case is_list(Input) andalso not io_lib:printable_list(Input) of
            true -> binary_to_list(openapi_parser:normalize_path(Input));
            false -> binary_to_list(openapi_parser:normalize_path(list_to_binary(Input)))
        end,
        case Result =:= Expected of
            true -> io:format("  ✓ ~p → ~s~n", [Input, Result]);
            false ->
                io:format("  ✗ Path normalization failed: ~p~n", [Input]),
                halt(1)
        end
    end, TestPaths),

    io:format("~n✓ Parser tests passed~n~n"),

    % Step 2: Parse Handler
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Step 2: Parsing Handler Module~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

    case openapi_parser:parse_handler(HandlerModule) of
        {ok, HandlerInfo} ->
            Routes = maps:get(routes, HandlerInfo),
            Metadata = maps:get(metadata, HandlerInfo),
            Coverage = maps:get(coverage, HandlerInfo),
            CoveragePercent = maps:get(coverage_percent, Coverage),

            io:format("✓ Successfully parsed handler~n~n"),
            io:format("Handler Information:~n"),
            io:format("  • Module: ~s~n", [HandlerModule]),
            io:format("  • Routes found: ~p~n", [length(Routes)]),
            io:format("  • Coverage: ~.1f%~n~n", [CoveragePercent]),

            io:format("Routes:~n"),
            lists:foreach(fun(Route) ->
                Path = maps:get(path, Route),
                RouteMethods = maps:get(methods, Route),
                HasMeta = maps:get(has_metadata, Route),
                io:format("  • ~s~n", [Path]),
                io:format("    Methods: ~s~n", [string:join([binary_to_list(M) || M <- RouteMethods], ", ")]),
                io:format("    Metadata: ~s~n", [case HasMeta of true -> "yes"; false -> "no" end])
            end, Routes),

            % Check metadata format
            io:format("~nMetadata Format:~n"),
            Paths = maps:get(paths, Metadata, #{}),
            case map_size(Paths) of
                0 ->
                    io:format("  ⚠ No metadata in cowboy_swagger format~n");
                N ->
                    io:format("  ✓ Cowboy_swagger format (~p path(s))~n", [N])
            end,

            % Step 3: Generate OpenAPI Spec
            io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Step 3: Generating OpenAPI Specification~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

            Options = #{app_name => test_app, format => "yaml"},

            try
                Spec = openapi_spec_builder:build_spec(test_app, [HandlerModule], Options),

                % Validate spec structure
                HasOpenapi = maps:is_key(<<"openapi">>, Spec),
                HasInfo = maps:is_key(<<"info">>, Spec),
                HasSpecPaths = maps:is_key(<<"paths">>, Spec),

                if
                    HasOpenapi andalso HasInfo andalso HasSpecPaths ->
                        io:format("✓ OpenAPI spec generated successfully~n~n"),

                        Info = maps:get(<<"info">>, Spec),
                        SpecPaths = maps:get(<<"paths">>, Spec),

                        io:format("Specification Details:~n"),
                        io:format("  • OpenAPI Version: ~s~n", [maps:get(<<"openapi">>, Spec)]),
                        io:format("  • Title: ~s~n", [maps:get(<<"title">>, Info, <<"N/A">>)]),
                        io:format("  • Paths documented: ~p~n~n", [map_size(SpecPaths)]),

                        io:format("Generated Paths:~n"),
                        maps:foreach(fun(SpecPath, PathSpec) ->
                            SpecMethods = [M || M <- maps:keys(PathSpec), M =/= <<"parameters">>],
                            MethodStrs = lists:map(fun(M) when is_binary(M) -> binary_to_list(M);
                                                      (M) when is_atom(M) -> atom_to_list(M)
                                                   end, SpecMethods),
                            io:format("  • ~s~n    Methods: ~s~n", [SpecPath, string:join(MethodStrs, ", ")])
                        end, SpecPaths),

                        % Step 4: Write YAML
                        io:format("~n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                        io:format("Step 4: Writing YAML Output~n"),
                        io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

                        OutputFile = "test_output/generated_openapi.yml",
                        filelib:ensure_dir(OutputFile),
                        YamlContent = yaml_encoder:encode(Spec),

                        case file:write_file(OutputFile, YamlContent) of
                            ok ->
                                io:format("✓ YAML written to: ~s~n", [OutputFile]),

                                % Show preview
                                YamlStr = case is_binary(YamlContent) of
                                    true -> binary_to_list(YamlContent);
                                    false -> lists:flatten(YamlContent)
                                end,
                                Lines = string:split(YamlStr, "\n", all),
                                PreviewLines = lists:sublist(Lines, 20),
                                io:format("~nPreview (first 20 lines):~n"),
                                io:format("┌─────────────────────────────────────────────────────────────┐~n"),
                                lists:foreach(fun(Line) ->
                                    io:format("│ ~-60s│~n", [string:slice(Line, 0, 60)])
                                end, PreviewLines),
                                if
                                    length(Lines) > 20 ->
                                        io:format("│ ... (~p more lines)~-41s│~n", [length(Lines) - 20, ""]);
                                    true -> ok
                                end,
                                io:format("└─────────────────────────────────────────────────────────────┘~n"),

                                io:format("~n╔═══════════════════════════════════════════════════════════════════╗~n"),
                                io:format("║  ✅ Test Complete - Success                                       ║~n"),
                                io:format("╚═══════════════════════════════════════════════════════════════════╝~n"),
                                io:format("~nSummary:~n"),
                                io:format("  • Parser tests: ✓~n"),
                                io:format("  • Handler parsed: ✓~n"),
                                io:format("  • Routes found: ~p~n", [length(Routes)]),
                                io:format("  • Coverage: ~.1f%~n", [CoveragePercent]),
                                io:format("  • OpenAPI spec generated: ✓~n"),
                                io:format("  • YAML output: ~s~n", [OutputFile]),
                                io:format("~n"),
                                halt(0);
                            {error, WriteErr} ->
                                io:format("✗ Failed to write YAML: ~p~n", [WriteErr]),
                                halt(1)
                        end;
                    true ->
                        io:format("✗ Generated spec is incomplete~n"),
                        io:format("  Has openapi: ~p~n", [HasOpenapi]),
                        io:format("  Has info: ~p~n", [HasInfo]),
                        io:format("  Has paths: ~p~n", [HasSpecPaths]),
                        halt(1)
                end
            catch
                Error:Reason:Stack ->
                    io:format("✗ Failed to generate OpenAPI spec: ~p:~p~n", [Error, Reason]),
                    io:format("Stack: ~p~n", [Stack]),
                    halt(1)
            end;

        {error, ParseReason} ->
            io:format("✗ Failed to parse handler: ~p~n", [ParseReason]),
            halt(1)
    end.

