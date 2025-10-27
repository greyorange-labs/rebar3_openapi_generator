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
    io:format("  --overwrite                 Overwrite existing files (default: merge)~n"),
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
        handler_name => "handler_module",
        overwrite => false
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

            MetadataModule = list_to_atom(maps:get(metadata_module, Opts)),
            HandlerModule = list_to_atom(maps:get(handler_module, Opts)),
            HandlerName = maps:get(handler_name, Opts),
            Overwrite = maps:get(overwrite, Opts),

            MetadataFile = filename:join(OutputDir, atom_to_list(MetadataModule) ++ ".erl"),
            HandlerFile = filename:join(OutputDir, atom_to_list(HandlerModule) ++ ".erl"),

            % Write or merge metadata file
            case write_or_merge_metadata(MetadataFile, MetadataModule, Paths, FormattedMetadata, Overwrite) of
                {ok, Action} ->
                    io:format("      ✓ ~s: ~s~n", [Action, MetadataFile]);
                {error, MetaReason} ->
                    io:format("      ✗ Failed to write metadata: ~p~n", [MetaReason]),
                    halt(1)
            end,

            % Write or merge handler file
            case write_or_merge_handler(HandlerFile, HandlerModule, MetadataModule, HandlerName,
                                       Paths, FinalHandler, Overwrite) of
                {ok, HandlerAction} ->
                    io:format("      ✓ ~s: ~s~n", [HandlerAction, HandlerFile]);
                {error, HandlerReason} ->
                    io:format("      ✗ Failed to write handler: ~p~n", [HandlerReason]),
                    halt(1)
            end,

            % Success summary
            io:format("~n╔════════════════════════════════════════════════════════════════╗~n"),
            io:format("║  ✓ SUCCESS~n"),
            io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

            io:format("Generated files:~n"),
            io:format("  • ~s~n", [MetadataFile]),
            io:format("  • ~s~n~n", [HandlerFile]),

            io:format("Next steps:~n"),
            io:format("  1. Review the generated files~n"),
            io:format("  2. In the handler module, replace 'handler_module' with your actual HTTP handler~n"),
            io:format("     Example: change 'handler_module' to 'my_http_handler'~n"),
            io:format("  3. Implement the HTTP handler callbacks (init/2, handle/2, etc.)~n"),
            io:format("  4. Add to your Cowboy routes:~n"),
            io:format("       Trails = trails:trails([~s]),~n", [HandlerModule]),
            io:format("       trails:store(Trails),~n"),
            io:format("       Dispatch = trails:single_host_compile(Trails)~n"),
            io:format("  5. Compile and test:~n"),
            io:format("       rebar3 compile~n~n");
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
parse_args(["--overwrite" | Rest], Opts) ->
    parse_args(Rest, Opts#{overwrite => true});
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

%% Write or merge metadata file
write_or_merge_metadata(FilePath, ModuleName, NewPaths, NewContent, Overwrite) ->
    FileExists = filelib:is_file(FilePath),

    case {FileExists, Overwrite} of
        {false, _} ->
            % File doesn't exist, write fresh
            file:write_file(FilePath, NewContent),
            {ok, "Created"};
        {true, true} ->
            % Overwrite requested
            file:write_file(FilePath, NewContent),
            {ok, "Overwritten"};
        {true, false} ->
            % Merge with existing
            case merge_metadata_file(FilePath, ModuleName, NewPaths) of
                {ok, MergedContent, Stats} ->
                    file:write_file(FilePath, MergedContent),
                    Action = io_lib:format("Merged (~p new, ~p updated)",
                                          [maps:get(added, Stats), maps:get(updated, Stats)]),
                    {ok, lists:flatten(Action)};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Write or merge handler file
write_or_merge_handler(FilePath, _HandlerModule, MetadataModule, HandlerName, NewPaths,
                       NewContent, Overwrite) ->
    FileExists = filelib:is_file(FilePath),

    case {FileExists, Overwrite} of
        {false, _} ->
            % File doesn't exist, write fresh
            file:write_file(FilePath, NewContent),
            {ok, "Created"};
        {true, true} ->
            % Overwrite requested
            file:write_file(FilePath, NewContent),
            {ok, "Overwritten"};
        {true, false} ->
            % Merge with existing
            case merge_handler_file(FilePath, MetadataModule, HandlerName, NewPaths) of
                {ok, MergedContent, Added} ->
                    file:write_file(FilePath, MergedContent),
                    Action = io_lib:format("Merged (~p new trail(s))", [Added]),
                    {ok, lists:flatten(Action)};
                {ok, no_changes} ->
                    {ok, "No changes needed"};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Merge metadata functions into existing file
merge_metadata_file(FilePath, ModuleName, NewPaths) ->
    case file:read_file(FilePath) of
        {ok, ExistingContent} ->
            % Extract existing function names
            ExistingFunctions = extract_metadata_functions(ExistingContent),

            % Generate new function map with full path data
            NewFunctions = generate_metadata_function_map(NewPaths),

            % Classify as new or updates
            {ToAdd, ToUpdate} = classify_functions(NewFunctions, ExistingFunctions),

            case {length(ToAdd), length(ToUpdate)} of
                {0, 0} ->
                    {ok, ExistingContent, #{added => 0, updated => 0}};
                {AddCount, UpdateCount} ->
                    % Regenerate module with ALL paths (new + updated)
                    % We need to regenerate completely to get the full metadata
                    Code = openapi_code_generator:generate_metadata_module(ModuleName, NewPaths, #{}),
                    FormattedCode = openapi_code_generator:format_erlang_code(Code),
                    {ok, FormattedCode, #{added => AddCount, updated => UpdateCount}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%% Merge trails into existing handler file
merge_handler_file(FilePath, MetadataModule, HandlerName, NewPaths) ->
    case file:read_file(FilePath) of
        {ok, ExistingContent} ->
            % Extract existing trail paths
            ExistingPaths = extract_trail_paths(ExistingContent),

            % Find paths to add
            PathsToAdd = lists:filter(
                fun(PathData) ->
                    Path = maps:get(path, PathData),
                    CowboyPath = normalize_path_for_cowboy(binary_to_list(Path)),
                    not lists:member(CowboyPath, ExistingPaths)
                end,
                NewPaths
            ),

            case PathsToAdd of
                [] ->
                    {ok, no_changes};
                _ ->
                    UpdatedContent = add_trails_to_module(ExistingContent, PathsToAdd,
                                                         MetadataModule, HandlerName),
                    {ok, UpdatedContent, length(PathsToAdd)}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%% Extract existing metadata function names from file content
extract_metadata_functions(Content) ->
    Pattern = "^([a-z_][a-zA-Z0-9_]*)\\(\\)\\s*->",
    Lines = binary:split(Content, <<"\n">>, [global]),

    lists:filtermap(
        fun(Line) ->
            LineStr = string:trim(binary_to_list(Line)),
            case re:run(LineStr, Pattern, [{capture, all_but_first, list}]) of
                {match, [FuncName]} -> {true, FuncName};
                nomatch -> false
            end
        end,
        Lines
    ).

%% Generate map of function_name -> {path, method} for paths
generate_metadata_function_map(Paths) ->
    lists:foldl(
        fun(PathData, Acc) ->
            Path = maps:get(path, PathData),
            Operations = maps:get(operations, PathData),

            % For each operation, generate function name
            maps:fold(
                fun(Method, _OperationData, InnerAcc) ->
                    FuncName = path_to_function_name(Path, Method),
                    InnerAcc#{FuncName => {Path, Method}}
                end,
                Acc,
                Operations
            )
        end,
        #{},
        Paths
    ).

%% Convert path and method to function name
path_to_function_name(Path, Method) when is_binary(Path), is_binary(Method) ->
    CleanPath = string:trim(binary_to_list(Path), both, "/"),
    SafePath = re:replace(CleanPath, "[^a-zA-Z0-9]+", "_", [global, {return, list}]),
    TrimmedPath = string:trim(SafePath, trailing, "_"),
    binary_to_list(Method) ++ "_" ++ string:to_lower(TrimmedPath).

%% Classify functions as new or updates
classify_functions(NewFunctions, ExistingFunctions) ->
    maps:fold(
        fun(FuncName, _Data, {AddAcc, UpdateAcc}) ->
            case lists:member(FuncName, ExistingFunctions) of
                true -> {AddAcc, [FuncName | UpdateAcc]};
                false -> {[FuncName | AddAcc], UpdateAcc}
            end
        end,
        {[], []},
        NewFunctions
    ).

%% Reconstruct path data from function map
reconstruct_paths_from_functions(FunctionMap, FunctionNames) ->
    % Group by path
    Grouped = lists:foldl(
        fun(FuncName, Acc) ->
            case maps:get(FuncName, FunctionMap, undefined) of
                undefined -> Acc;
                {Path, Method} ->
                    maps:update_with(
                        Path,
                        fun(Methods) -> Methods#{Method => #{}} end,
                        #{Method => #{}},
                        Acc
                    )
            end
        end,
        #{},
        FunctionNames
    ),

    % Convert to path format
    maps:fold(
        fun(Path, Operations, Acc) ->
            [#{path => Path, operations => Operations} | Acc]
        end,
        [],
        Grouped
    ).

%% Extract trail paths from handler module
extract_trail_paths(Content) ->
    Pattern = "trails:trail\\(\"([^\"]+)\"",
    case re:run(Content, Pattern, [global, {capture, all_but_first, list}]) of
        {match, Matches} ->
            [Path || [Path] <- Matches];
        nomatch ->
            []
    end.

%% Normalize OpenAPI path to Cowboy format
normalize_path_for_cowboy(Path) ->
    re:replace(Path, "\\{([^}]+)\\}", ":\\1", [global, {return, list}]).

%% Add new trails to existing module
add_trails_to_module(ExistingContent, PathsToAdd, MetadataModule, HandlerName) ->
    ContentStr = binary_to_list(ExistingContent),

    % Generate new trail entries
    NewTrails = lists:map(
        fun(PathData) ->
            Path = maps:get(path, PathData),
            Operations = maps:get(operations, PathData),
            CowboyPath = normalize_path_for_cowboy(binary_to_list(Path)),

            % Get function name from first operation
            [FirstMethod | _] = maps:keys(Operations),
            FuncName = path_to_function_name(Path, FirstMethod),

            io_lib:format("        trails:trail(\"~s\", ~s, [], ~s:~s())",
                         [CowboyPath, HandlerName, MetadataModule, FuncName])
        end,
        PathsToAdd
    ),

    % Find the closing ]. in trails/0 function and insert before it
    case re:run(ContentStr, "trails\\(\\)\\s*->\\s*\\[(.*)\\]\\.", [dotall, {capture, [1], list}]) of
        {match, [TrailsContent]} ->
            % Insert new trails before the closing ].
            NewTrailsStr = lists:flatten(lists:join(",\n", NewTrails)),

            % Build updated trails list
            UpdatedTrails = io_lib:format("trails() ->~n    [~n~s,~n~s~n    ].",
                                         [string:trim(TrailsContent, trailing), NewTrailsStr]),

            % Replace the entire trails/0 function
            UpdatedContent = re:replace(ContentStr,
                                       "trails\\(\\)\\s*->\\s*\\[.*?\\]\\.",
                                       UpdatedTrails,
                                       [{return, list}, dotall]),
            list_to_binary(UpdatedContent);
        nomatch ->
            % Couldn't parse, append comment with trails to add manually
            Comment = io_lib:format(
                "\n\n%% New trails to add to trails/0 function:\n%% ~s\n",
                [lists:flatten(lists:join(",\n%% ", NewTrails))]
            ),
            <<ExistingContent/binary, (list_to_binary(Comment))/binary>>
    end.

