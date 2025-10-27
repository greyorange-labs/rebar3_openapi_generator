#!/usr/bin/env escript
%%! -pa _build/default/lib/rebar3_openapi_generator/ebin -pa _build/default/lib/yamerl/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/trails/ebin

%%%-------------------------------------------------------------------
%%% @doc Test script for OpenAPI handler update functionality
%%% This script tests the plugin's ability to update existing handlers
%%% with OpenAPI specifications
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main([]) ->
    io:format("Usage: ~s <openapi_spec> <handler_file> <metadata_module>~n", [escript:script_name()]),
    io:format("Example: ~s api.yml handler.erl handler_metadata~n", [escript:script_name()]),
    halt(1);

main([OpenApiFile, HandlerFile, MetadataModuleName]) ->
    io:format("~n╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║     OpenAPI Handler Update Test                                ║~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

    io:format("Configuration:~n"),
    io:format("  • OpenAPI Spec: ~s~n", [OpenApiFile]),
    io:format("  • Handler File: ~s~n", [HandlerFile]),
    io:format("  • Metadata Module: ~s~n", [MetadataModuleName]),
    io:format("  • Output Directory: test_output/~n~n"),

    % Ensure yamerl is started
    application:ensure_all_started(yamerl),

    % Create test output directory
    TestDir = "test_output",
    filelib:ensure_dir(TestDir ++ "/"),

    % Step 1: Parse OpenAPI file
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("Step 1: Parsing OpenAPI Specification~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    case openapi_importer:parse_openapi_file(OpenApiFile) of
        {ok, ParsedSpec} ->
            io:format("✓ Successfully parsed OpenAPI spec~n~n"),

            Info = maps:get(info, ParsedSpec),
            Paths = maps:get(paths, ParsedSpec),

            io:format("API Information:~n"),
            io:format("  • Title: ~s~n", [maps:get(<<"title">>, Info, <<"Unknown">>)]),
            io:format("  • Version: ~s~n", [maps:get(<<"version">>, Info, <<"1.0.0">>)]),
            io:format("  • Description: ~s~n", [maps:get(<<"description">>, Info, <<"N/A">>)]),
            io:format("~n"),

            io:format("Found ~p path(s):~n", [length(Paths)]),
            lists:foreach(fun(PathData) ->
                Path = maps:get(path, PathData),
                Ops = maps:get(operations, PathData),
                Methods = maps:keys(Ops),
                io:format("  • ~s~n", [Path]),
                lists:foreach(fun(Method) ->
                    OpSpec = maps:get(Method, Ops),
                    Summary = maps:get(summary, OpSpec, <<>>),
                    io:format("    └─ ~s: ~s~n", [string:uppercase(binary_to_list(Method)), Summary])
                end, Methods)
            end, Paths),
            io:format("~n"),

            % Step 2: Read existing handler
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            io:format("Step 2: Analyzing Existing Handler~n"),
            io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
            case file:read_file(HandlerFile) of
                {ok, HandlerContent} ->
                    io:format("✓ Successfully read handler file~n~n"),

                    % Extract module name
                    HandlerModuleName = extract_module_name(HandlerContent),
                    io:format("Handler Module: ~s~n", [HandlerModuleName]),

                    % Analyze existing trails
                    ExistingTrails = extract_existing_trails(HandlerContent),
                    io:format("~nExisting trails() function has ~p entries:~n", [length(ExistingTrails)]),
                    lists:foreach(fun(Trail) ->
                        io:format("  • ~s~n", [Trail])
                    end, ExistingTrails),
                    io:format("~n"),

                    % Step 3: Determine updates
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("Step 3: Determining Required Updates~n"),
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    {PathsToAdd, PathsToUpdate, PathsAlreadyDefined} =
                        analyze_updates_needed(Paths, ExistingTrails, HandlerContent),

                    io:format("Analysis Results:~n"),
                    io:format("  • New paths to add: ~p~n", [length(PathsToAdd)]),
                    io:format("  • Paths needing metadata updates: ~p~n", [length(PathsToUpdate)]),
                    io:format("  • Paths already properly defined: ~p~n~n", [length(PathsAlreadyDefined)]),

                    if
                        length(PathsToAdd) > 0 ->
                            io:format("Paths to ADD to trails():~n"),
                            lists:foreach(fun(P) ->
                                Path = maps:get(path, P),
                                Ops = maps:get(operations, P),
                                Methods = [binary_to_list(M) || M <- maps:keys(Ops)],
                                io:format("  + ~s [~s]~n", [Path, string:join(Methods, ", ")])
                            end, PathsToAdd),
                            io:format("~n");
                        true -> ok
                    end,

                    if
                        length(PathsToUpdate) > 0 ->
                            io:format("Paths to UPDATE (metadata only):~n"),
                            lists:foreach(fun(P) ->
                                Path = maps:get(path, P),
                                io:format("  * ~s~n", [Path])
                            end, PathsToUpdate),
                            io:format("~n");
                        true -> ok
                    end,

                    if
                        length(PathsAlreadyDefined) > 0 ->
                            io:format("Paths already defined:~n"),
                            lists:foreach(fun(P) ->
                                Path = maps:get(path, P),
                                io:format("  ✓ ~s~n", [Path])
                            end, PathsAlreadyDefined),
                            io:format("~n");
                        true -> ok
                    end,

                    % Step 4: Generate metadata module
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("Step 4: Generating Metadata Module~n"),
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

                    MetadataModule = list_to_atom(MetadataModuleName),
                    MetadataFile = TestDir ++ "/" ++ MetadataModuleName ++ ".erl",

                    case generate_metadata(MetadataFile, MetadataModule, Paths) of
                        ok ->
                            io:format("✓ Generated metadata module: ~s~n", [MetadataFile]),

                            % Show preview
                            {ok, MetaContent} = file:read_file(MetadataFile),
                            Lines = binary:split(MetaContent, <<"\n">>, [global]),
                            PreviewLines = lists:sublist([binary_to_list(L) || L <- Lines], 30),
                            io:format("~nPreview (first 30 lines):~n"),
                            io:format("┌─────────────────────────────────────────────────────────────┐~n"),
                            lists:foreach(fun(Line) ->
                                io:format("│ ~-60s│~n", [string:slice(Line, 0, 60)])
                            end, PreviewLines),
                            if
                                length(Lines) > 30 ->
                                    io:format("│ ... (~p more lines) ...~-38s│~n",
                                             [length(Lines) - 30, ""]);
                                true -> ok
                            end,
                            io:format("└─────────────────────────────────────────────────────────────┘~n~n");
                        {error, MetaErr} ->
                            io:format("✗ Failed to generate metadata: ~p~n", [MetaErr]),
                            halt(1)
                    end,

                    % Step 5: Update handler
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("Step 5: Updating Handler Module~n"),
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),

                    UpdatedHandlerFile = TestDir ++ "/" ++ HandlerModuleName ++ ".erl",
                    case update_handler(UpdatedHandlerFile, HandlerContent, PathsToAdd,
                                       PathsToUpdate, MetadataModule) of
                        {ok, Changes} ->
                            io:format("✓ Updated handler module: ~s~n", [UpdatedHandlerFile]),
                            io:format("~nChanges made:~n"),
                            lists:foreach(fun(Change) ->
                                io:format("  • ~s~n", [Change])
                            end, Changes),
                            io:format("~n");
                        {error, UpdateErr} ->
                            io:format("✗ Failed to update handler: ~p~n", [UpdateErr]),
                            halt(1)
                    end,

                    % Step 6: Summary
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("✓ Test Complete - Summary~n"),
                    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
                    io:format("~nGenerated Files:~n"),
                    io:format("  1. ~s~n", [MetadataFile]),
                    io:format("     └─ Contains OpenAPI metadata for ~p path(s)~n", [length(Paths)]),
                    io:format("  2. ~s~n", [UpdatedHandlerFile]),
                    io:format("     └─ Updated trails() with ~p new entries~n", [length(PathsToAdd)]),
                    io:format("~n"),
                    io:format("Actions:~n"),
                    io:format("  • Added ~p new trail definition(s)~n", [length(PathsToAdd)]),
                    io:format("  • Generated/Updated ~p metadata function(s)~n", [length(Paths)]),
                    io:format("  • Preserved all existing handler code~n"),
                    io:format("~n"),
                    io:format("Next Steps:~n"),
                    io:format("  1. Review generated files in ~s/~n", [TestDir]),
                    io:format("  2. Compare with original handler to verify changes~n"),
                    io:format("  3. If satisfied, apply changes to your project~n"),
                    io:format("~n"),
                    halt(0);

                {error, Reason} ->
                    io:format("✗ Failed to read handler file: ~p~n", [Reason]),
                    halt(1)
            end;

        {error, Reason} ->
            io:format("✗ Failed to parse OpenAPI spec: ~p~n", [Reason]),
            halt(1)
    end.

%%% ═══════════════════════════════════════════════════════════════════
%%% Internal Helper Functions
%%% ═══════════════════════════════════════════════════════════════════

%% Extract module name from handler content
extract_module_name(Content) ->
    case re:run(Content, "-module\\(([^)]+)\\)", [{capture, all_but_first, list}]) of
        {match, [ModuleName]} -> ModuleName;
        nomatch -> "unknown_module"
    end.

%% Extract existing trail paths from handler content
extract_existing_trails(Content) ->
    ContentStr = binary_to_list(Content),

    % Find the trails() function
    case re:run(ContentStr, "trails\\(\\)\\s*->\\s*\\[(.*?)\\]\\.",
               [dotall, {capture, all_but_first, list}]) of
        {match, [TrailsBody]} ->
            % Extract path patterns - handle both trails:trail and simple tuple formats
            Patterns = [
                "trails:trail\\(\"([^\"]+)\"",  % trails:trail format
                "\\{\"([^\"]+)\",\\s*\\w+"      % simple tuple format
            ],

            AllPaths = lists:flatmap(fun(Pattern) ->
                case re:run(TrailsBody, Pattern, [global, {capture, all_but_first, list}]) of
                    {match, Matches} -> [Path || [Path] <- Matches];
                    nomatch -> []
                end
            end, Patterns),

            lists:usort(AllPaths);
        nomatch ->
            []
    end.

%% Analyze what updates are needed
analyze_updates_needed(OpenApiPaths, ExistingTrails, _HandlerContent) ->
    lists:foldl(
        fun(PathData, {ToAdd, ToUpdate, AlreadyDefined}) ->
            Path = maps:get(path, PathData),
            PathStr = binary_to_list(Path),

            % Convert OpenAPI path format to Cowboy format for comparison
            CowboyPath = convert_openapi_to_cowboy_path(PathStr),

            % Check if this path exists in trails
            PathMatches = lists:filter(fun(ExistingPath) ->
                paths_match(CowboyPath, ExistingPath)
            end, ExistingTrails),

            case PathMatches of
                [] ->
                    % New path - needs to be added
                    {[PathData | ToAdd], ToUpdate, AlreadyDefined};
                [_Match] ->
                    % Path exists - check if it needs metadata update
                    % For now, consider it as needing update
                    {ToAdd, [PathData | ToUpdate], AlreadyDefined};
                _ ->
                    % Multiple matches or already defined
                    {ToAdd, ToUpdate, [PathData | AlreadyDefined]}
            end
        end,
        {[], [], []},
        OpenApiPaths
    ).

%% Convert OpenAPI path to Cowboy path format
convert_openapi_to_cowboy_path(Path) ->
    % Convert {param} to :param
    re:replace(Path, "\\{([^}]+)\\}", ":\\1", [global, {return, list}]).

%% Check if two paths match (considering wildcards and path patterns)
paths_match(Path1, Path2) ->
    % Normalize both paths
    P1 = string:trim(Path1, trailing, "/"),
    P2 = string:trim(Path2, trailing, "/"),

    % Check for exact match
    string:equal(P1, P2) orelse
    % Check if P2 is a wildcard that covers P1
    is_wildcard_match(P1, P2) orelse
    % Check reverse
    is_wildcard_match(P2, P1).

%% Check if a wildcard pattern matches a path
is_wildcard_match(Path, Pattern) ->
    % Handle [...] wildcard
    case string:find(Pattern, "[...]") of
        nomatch ->
            false;
        _ ->
            % Extract prefix before wildcard
            Prefix = hd(string:split(Pattern, "/[...]")),
            string:prefix(Path, Prefix) =/= nomatch
    end.

%% Generate metadata module
generate_metadata(MetadataFile, MetadataModule, Paths) ->
    Code = openapi_code_generator:generate_metadata_module(MetadataModule, Paths, #{}),
    FormattedCode = openapi_code_generator:format_erlang_code(Code),
    case file:write_file(MetadataFile, FormattedCode) of
        ok ->
            % Format with erlfmt
            format_file(MetadataFile),
            ok;
        Error ->
            Error
    end.

%% Update handler with new trails
update_handler(OutputFile, HandlerContent, PathsToAdd, _PathsToUpdate, MetadataModule) ->
    ContentStr = binary_to_list(HandlerContent),
    Changes = [],

    UpdatedContent = if
        length(PathsToAdd) > 0 ->
            % Add new trails
            {NewContent, AddChanges} = add_trails_to_module(ContentStr, PathsToAdd, MetadataModule),
            {NewContent, Changes ++ AddChanges};
        true ->
            {ContentStr, Changes}
    end,

    case UpdatedContent of
        {FinalContent, FinalChanges} ->
            file:write_file(OutputFile, FinalContent),
            % Format with erlfmt
            format_file(OutputFile),
            {ok, FinalChanges};
        {error, Reason} ->
            {error, Reason}
    end.

%% Format a file with erlfmt
format_file(FilePath) ->
    try
        case file:read_file(FilePath) of
            {ok, Content} ->
                try
                    Formatted = erlfmt:format_string(binary_to_list(Content)),
                    file:write_file(FilePath, Formatted)
                catch
                    _:_ ->
                        % erlfmt not available or failed, keep original
                        ok
                end;
            _ ->
                ok
        end
    catch
        _:_ -> ok
    end.

%% Add new trails to existing module
add_trails_to_module(ContentStr, PathsToAdd, MetadataModule) ->
    MetadataModuleStr = atom_to_list(MetadataModule),

    % Generate new trail entries
    NewTrailEntries = lists:map(fun(PathData) ->
        Path = maps:get(path, PathData),
        Operations = maps:get(operations, PathData),

        % Get first method for function name
        [FirstMethod | _] = maps:keys(Operations),
        FuncName = path_to_function_name(Path, FirstMethod),

        % Convert to Cowboy format
        CowboyPath = convert_openapi_to_cowboy_path(binary_to_list(Path)),

        % Extract handler module from existing content
        HandlerModule = case re:run(ContentStr, "-module\\(([^)]+)\\)",
                                   [{capture, all_but_first, list}]) of
            {match, [Mod]} -> Mod;
            nomatch -> "handler_module"
        end,

        io_lib:format("        trails:trail(\"~s\", ~s, [], ~s:~s())",
                     [CowboyPath, HandlerModule, MetadataModuleStr, FuncName])
    end, PathsToAdd),

    Changes = [io_lib:format("Added trail for ~s", [maps:get(path, P)]) || P <- PathsToAdd],

    UpdatedContent = if
        length(NewTrailEntries) > 0 ->
            % Find trails() function and replace just that function
            % Use a more careful regex that captures everything before and after
            case re:run(ContentStr, "(.*?)(trails\\(\\)\\s*->\\s*\\[)(.*?)(\\]\\.)(.*)$",
                       [dotall, {capture, all_but_first, list}]) of
                {match, [BeforeTrails, TrailsPrefix, ExistingTrails, TrailsSuffix, AfterTrails]} ->
                    % Check if existing trails is empty
                    TrimmedExisting = string:trim(ExistingTrails),

                    NewTrailsStr = string:join(NewTrailEntries, ",\n"),

                    UpdatedTrails = if
                        TrimmedExisting == "" ->
                            % Empty trails, just add new ones
                            NewTrailsStr;
                        true ->
                            % Append to existing with comma and newline
                            ExistingTrails ++ ",\n" ++ NewTrailsStr
                    end,

                    % Reconstruct the full file
                    BeforeTrails ++ TrailsPrefix ++ UpdatedTrails ++ TrailsSuffix ++ AfterTrails;
                nomatch ->
                    % Couldn't find trails function, append as comment
                    Comment = io_lib:format(
                        "\n\n%% TODO: Add these trails to your trails() function:\n%% ~s\n",
                        [string:join(NewTrailEntries, ",\n%% ")]
                    ),
                    ContentStr ++ Comment
            end;
        true ->
            ContentStr
    end,

    {UpdatedContent, Changes}.

%% Convert path and method to function name
path_to_function_name(Path, Method) when is_binary(Path), is_binary(Method) ->
    CleanPath = string:trim(binary_to_list(Path), both, "/"),
    SafePath = re:replace(CleanPath, "[^a-zA-Z0-9]+", "_", [global, {return, list}]),
    TrimmedPath = string:trim(SafePath, trailing, "_"),
    binary_to_list(Method) ++ "_" ++ string:to_lower(TrimmedPath).

