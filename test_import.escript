#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -pa _build/default/lib/rebar3_openapi_generator/ebin

%% Standalone test script for OpenAPI import functionality
%% This simulates how the plugin works when called from rebar3

main([]) ->
    % Ensure required applications are started
    ensure_apps_started(),
    io:format("Usage: ~s <openapi-spec.yml> [options]~n", [escript:script_name()]),
    io:format("~nOptions:~n"),
    io:format("  --output <dir>              Output directory (default: src/)~n"),
    io:format("  --handler-module <name>     Handler module name (default: generated_handler)~n"),
    io:format("  --metadata-module <name>    Metadata module name (default: generated_metadata)~n"),
    io:format("  --handler-name <name>       Actual handler to use in trails (default: handler_module)~n"),
    io:format("~nExample:~n"),
    io:format("  ~s butler_details.yml --output src/api/ --handler-module my_handler~n", [escript:script_name()]),
    halt(1);
main([SpecFile | Args]) ->
    % Ensure required applications are started
    ensure_apps_started(),

    % Parse command line arguments
    Opts = parse_args(Args, #{
        output => "src/",
        handler_module => "generated_handler",
        metadata_module => "generated_metadata",
        handler_name => "handler_module"
    }),

    io:format("~n╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║  OpenAPI Import - Standalone Test~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("→ Input Spec: ~s~n", [SpecFile]),
    io:format("→ Output Dir: ~s~n", [maps:get(output, Opts)]),
    io:format("→ Handler Module: ~s~n", [maps:get(handler_module, Opts)]),
    io:format("→ Metadata Module: ~s~n", [maps:get(metadata_module, Opts)]),
    io:format("→ Handler Name: ~s~n~n", [maps:get(handler_name, Opts)]),

    % Step 1: Parse OpenAPI spec
    io:format("[1/4] Parsing OpenAPI specification...~n"),
    case openapi_importer:parse_openapi_file(SpecFile) of
        {ok, ParsedSpec} ->
            io:format("      ✓ Successfully parsed OpenAPI spec~n"),

            Paths = maps:get(paths, ParsedSpec),
            Info = maps:get(info, ParsedSpec),

            io:format("      ✓ API: ~s v~s~n", [
                maps:get(<<"title">>, Info, <<"Unknown">>),
                maps:get(<<"version">>, Info, <<"1.0.0">>)
            ]),
            io:format("      ✓ Found ~p path(s)~n~n", [length(Paths)]),

            % Display paths
            lists:foreach(
                fun(PathData) ->
                    Path = maps:get(path, PathData),
                    Operations = maps:get(operations, PathData),
                    Methods = [binary_to_list(M) || M <- maps:keys(Operations)],
                    io:format("        • ~s [~s]~n", [Path, string:join(Methods, ", ")])
                end,
                Paths
            ),

            % Step 2: Generate metadata module
            io:format("~n[2/4] Generating metadata module...~n"),
            MetadataModule = list_to_atom(maps:get(metadata_module, Opts)),
            MetadataCode = openapi_code_generator:generate_metadata_module(MetadataModule, Paths, #{}),
            FormattedMetadata = openapi_code_generator:format_erlang_code(MetadataCode),
            io:format("      ✓ Generated ~p bytes of metadata code~n", [iolist_size(FormattedMetadata)]),

            % Step 3: Generate handler module
            io:format("~n[3/4] Generating handler module...~n"),
            HandlerModule = list_to_atom(maps:get(handler_module, Opts)),
            HandlerCode = openapi_code_generator:generate_trails_module(
                HandlerModule,
                Paths,
                #{metadata_module => MetadataModule}
            ),
            FormattedHandler = openapi_code_generator:format_erlang_code(HandlerCode),
            HandlerName = maps:get(handler_name, Opts),
            FinalHandler = re:replace(FormattedHandler, "handler_module", HandlerName, [global, {return, list}]),
            io:format("      ✓ Generated ~p bytes of handler code~n", [iolist_size(FinalHandler)]),

            % Step 4: Write files
            io:format("~n[4/4] Writing files...~n"),
            OutputDir = maps:get(output, Opts),
            filelib:ensure_dir(filename:join(OutputDir, "dummy")),

            MetadataFile = filename:join(OutputDir, atom_to_list(MetadataModule) ++ ".erl"),
            HandlerFile = filename:join(OutputDir, atom_to_list(HandlerModule) ++ ".erl"),

            ok = file:write_file(MetadataFile, FormattedMetadata),
            io:format("      ✓ Wrote: ~s~n", [MetadataFile]),

            ok = file:write_file(HandlerFile, FinalHandler),
            io:format("      ✓ Wrote: ~s~n", [HandlerFile]),

            % Success summary
            io:format("~n╔════════════════════════════════════════════════════════════════╗~n"),
            io:format("║  ✓ SUCCESS~n"),
            io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

            io:format("Generated files:~n"),
            io:format("  • ~s~n", [MetadataFile]),
            io:format("  • ~s~n~n", [HandlerFile]),

            io:format("Next steps:~n"),
            io:format("  1. Review the generated files~n"),
            io:format("  2. Update handler references if needed~n"),
            io:format("  3. Add to your Cowboy routes:~n"),
            io:format("       Trails = trails:trails([~s]),~n", [HandlerModule]),
            io:format("       trails:store(Trails),~n"),
            io:format("       Dispatch = trails:single_host_compile(Trails)~n"),
            io:format("  4. Run: rebar3 compile~n~n");
        {error, Reason} ->
            io:format("~n✗ ERROR: Failed to parse OpenAPI spec~n"),
            io:format("~nDetails: ~p~n~n", [Reason]),

            case Reason of
                {file_read_error, FileError} ->
                    io:format("File error: ~p~n", [FileError]),
                    io:format("  • Check that the file exists~n"),
                    io:format("  • Check file permissions~n");
                {yaml_parse_error, YamlError} ->
                    io:format("YAML parsing error: ~p~n", [YamlError]),
                    io:format("  • Check YAML syntax~n"),
                    io:format("  • Validate indentation~n");
                {parse_error, ParseError, Stack} ->
                    io:format("Parse error: ~p~n", [ParseError]),
                    io:format("Stack trace:~n"),
                    lists:foreach(
                        fun(Frame) ->
                            io:format("  ~p~n", [Frame])
                        end,
                        Stack
                    );
                _ ->
                    io:format("Unknown error type~n")
            end,

            halt(1)
    end.

%% Parse command line arguments
parse_args([], Opts) ->
    Opts;
parse_args(["--output", Dir | Rest], Opts) ->
    parse_args(Rest, Opts#{output => Dir});
parse_args(["--handler-module", Mod | Rest], Opts) ->
    parse_args(Rest, Opts#{handler_module => Mod});
parse_args(["--metadata-module", Mod | Rest], Opts) ->
    parse_args(Rest, Opts#{metadata_module => Mod});
parse_args(["--handler-name", Name | Rest], Opts) ->
    parse_args(Rest, Opts#{handler_name => Name});
parse_args([Unknown | Rest], Opts) ->
    io:format("Warning: Unknown option: ~s~n", [Unknown]),
    parse_args(Rest, Opts).

%% Ensure required applications are started (critical for yamerl)
ensure_apps_started() ->
    % Add code paths for all dependencies
    LibDir = "_build/default/lib",
    case filelib:is_dir(LibDir) of
        true ->
            Dirs = filelib:wildcard(LibDir ++ "/*/ebin"),
            [code:add_pathz(Dir) || Dir <- Dirs];
        false ->
            ok
    end,

    % This is the key fix - yamerl must be started before parsing YAML
    % The openapi_importer module also does this internally,
    % but doing it here ensures it's available for the escript
    case application:ensure_all_started(yamerl) of
        {ok, _Started} ->
            ok;
        {error, {yamerl, Reason}} ->
            io:format("ERROR: Failed to start yamerl: ~p~n", [Reason]),
            io:format("~nTip: Run 'rebar3 compile' first to ensure dependencies are built~n"),
            halt(1);
        {error, Reason} ->
            io:format("ERROR: Failed to start dependencies: ~p~n", [Reason]),
            halt(1)
    end.
