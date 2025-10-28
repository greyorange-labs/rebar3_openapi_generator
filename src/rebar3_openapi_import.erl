-module(rebar3_openapi_import).
-behaviour(provider).

-export([
    init/1,
    do/1,
    format_error/1
]).

-define(PROVIDER, import).
-define(NAMESPACE, openapi).
-define(DEPS, []).

%% Initialize the provider
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {namespace, ?NAMESPACE},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example,
            "rebar3 openapi import --spec openapi.yaml --handler-path ./src/my_handler.erl --metadata-path ./src/my_metadata.erl"},
        {short_desc, "Import OpenAPI spec and generate Erlang trails definitions"},
        {desc,
            "Import an OpenAPI YAML specification and generate Erlang handler modules "
            "with trails definitions and metadata functions."},
        {opts, [
            {spec, $s, "spec", string, "Path to OpenAPI YAML specification file (required)"},
            {handler_path, $h, "handler-path", string, "Output path for handler module, e.g. ./src/my_handler.erl (required)"},
            {metadata_path, $m, "metadata-path", string,
                "Output path for metadata module, e.g. ./src/my_metadata.erl (required)"},
            {handler_name, undefined, "handler-name", {string, "handler_module"},
                "Handler module name to use in trails (e.g. my_http_handler)"},
            {overwrite, $w, "overwrite", {boolean, false}, "Overwrite existing files"},
            {format, $f, "format", {boolean, true}, "Format generated code with rebar3_format"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% Execute the import
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),

    % Validate required options
    case validate_required_params(Opts) of
        ok ->
            SpecFile = proplists:get_value(spec, Opts),
            HandlerPath = proplists:get_value(handler_path, Opts),
            MetadataPath = proplists:get_value(metadata_path, Opts),
            execute_import(SpecFile, HandlerPath, MetadataPath, Opts, State);
        {error, Reason} ->
            {error, Reason}
    end.

%% Validate required parameters
-spec validate_required_params(proplists:proplist()) -> ok | {error, string()}.
validate_required_params(Opts) ->
    case proplists:get_value(spec, Opts) of
        undefined ->
            rebar_api:error("Missing required parameter: --spec <path/to/openapi.yaml>", []),
            {error, "Missing required parameter: --spec <path/to/openapi.yaml>"};
        SpecFile ->
            case filelib:is_file(SpecFile) of
                false ->
                    rebar_api:error("OpenAPI spec file not found: ~s", [SpecFile]),
                    {error, io_lib:format("OpenAPI spec file not found: ~s", [SpecFile])};
                true ->
                    validate_output_paths(Opts)
            end
    end.

%% Validate output paths
-spec validate_output_paths(proplists:proplist()) -> ok | {error, string()}.
validate_output_paths(Opts) ->
    case proplists:get_value(handler_path, Opts) of
        undefined ->
            rebar_api:error("Missing required parameter: --handler-path <path/to/handler.erl>", []),
            {error, "Missing required parameter: --handler-path <path/to/handler.erl>"};
        HandlerPath ->
            case proplists:get_value(metadata_path, Opts) of
                undefined ->
                    rebar_api:error("Missing required parameter: --metadata-path <path/to/metadata.erl>", []),
                    {error, "Missing required parameter: --metadata-path <path/to/metadata.erl>"};
                MetadataPath ->
                    case validate_erlang_file_path(HandlerPath, "handler-path") of
                        ok ->
                            validate_erlang_file_path(MetadataPath, "metadata-path");
                        Error ->
                            Error
                    end
            end
    end.

%% Validate Erlang file path
-spec validate_erlang_file_path(string(), string()) -> ok | {error, string()}.
validate_erlang_file_path(Path, ParamName) ->
    % Check file extension
    case filename:extension(Path) of
        ".erl" ->
            % Check if parent directory exists or can be created
            ParentDir = filename:dirname(Path),
            case filelib:is_dir(ParentDir) of
                true ->
                    ok;
                false ->
                    case filelib:ensure_dir(Path) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            ErrMsg = io_lib:format(
                                "Cannot create parent directory for --~s: ~s (reason: ~p)",
                                [ParamName, ParentDir, Reason]
                            ),
                            rebar_api:error("~s", [ErrMsg]),
                            {error, lists:flatten(ErrMsg)}
                    end
            end;
        Other ->
            ErrMsg = io_lib:format(
                "Invalid file extension for --~s: ~s (expected .erl)",
                [ParamName, Other]
            ),
            rebar_api:error("~s", [ErrMsg]),
            {error, lists:flatten(ErrMsg)}
    end.

%% Execute the import process
execute_import(SpecFile, HandlerPath, MetadataPath, Opts, State) ->
    rebar_api:info("Importing OpenAPI specification from: ~s", [SpecFile]),

    % Extract module names from file paths
    HandlerModule = list_to_atom(filename:basename(HandlerPath, ".erl")),
    MetadataModule = list_to_atom(filename:basename(MetadataPath, ".erl")),

    rebar_api:info("Handler module: ~s (from ~s)", [HandlerModule, HandlerPath]),
    rebar_api:info("Metadata module: ~s (from ~s)", [MetadataModule, MetadataPath]),

    % Parse OpenAPI spec
    case openapi_importer:parse_openapi_file(SpecFile) of
        {ok, ParsedSpec} ->
            generate_files(ParsedSpec, HandlerPath, MetadataPath, HandlerModule, MetadataModule, Opts, State);
        {error, Reason} ->
            rebar_api:error("Parse error details: ~p", [Reason]),
            {error, format_parse_error(Reason)}
        %% Generate Erlang files from parsed spec
    end.

%% Generate files from parsed spec
generate_files(ParsedSpec, HandlerPath, MetadataPath, HandlerModule, MetadataModule, Opts, State) ->
    HandlerName = proplists:get_value(handler_name, Opts),
    Overwrite = proplists:get_value(overwrite, Opts),
    DoFormat = proplists:get_value(format, Opts),

    Paths = maps:get(paths, ParsedSpec),
    Info = maps:get(info, ParsedSpec),

    rebar_api:info("Generating code for ~p paths", [length(Paths)]),
    rebar_api:info("  API: ~s ~s", [
        maps:get(<<"title">>, Info, <<"Unknown">>),
        maps:get(<<"version">>, Info, <<"1.0.0">>)
    ]),

    % Ensure output directories exist
    filelib:ensure_dir(HandlerPath),
    filelib:ensure_dir(MetadataPath),

    % Generate metadata module
    case generate_metadata_file(MetadataPath, MetadataModule, Paths, Overwrite) of
        ok ->
            rebar_api:info("Generated metadata module: ~s", [MetadataPath]),

            % Generate handler module
            case generate_handler_file(HandlerPath, HandlerModule, MetadataModule, HandlerName, Paths, Overwrite) of
                ok ->
                    rebar_api:info("Generated handler module: ~s", [HandlerPath]),

                    % Format generated files if requested
                    case DoFormat of
                        true -> format_files([MetadataPath, HandlerPath]);
                        false -> ok
                    end,

                    rebar_api:info("Successfully generated Erlang modules from OpenAPI spec", []),
                    print_usage_instructions(HandlerModule, MetadataModule),
                    {ok, State};
                {error, Reason} ->
                    {error, io_lib:format("Failed to generate handler: ~p", [Reason])}
            end;
        {error, Reason} ->
            {error, io_lib:format("Failed to generate metadata: ~p", [Reason])}
    end.

%% Generate metadata file
generate_metadata_file(FilePath, ModuleName, Paths, Overwrite) ->
    FileExists = filelib:is_file(FilePath),

    case {FileExists, Overwrite} of
        {true, false} ->
            % File exists and not overwriting - try to merge
            rebar_api:info("File exists: ~s - attempting to merge new metadata functions", [FilePath]),
            merge_metadata_file(FilePath, ModuleName, Paths);
        {true, true} ->
            % File exists but overwrite requested - regenerate completely
            rebar_api:info("Overwriting existing file: ~s", [FilePath]),
            Code = openapi_code_generator:generate_metadata_module(ModuleName, Paths, #{}),
            FormattedCode = openapi_code_generator:format_erlang_code(Code),
            file:write_file(FilePath, FormattedCode);
        {false, _} ->
            % File doesn't exist - generate fresh
            Code = openapi_code_generator:generate_metadata_module(ModuleName, Paths, #{}),
            FormattedCode = openapi_code_generator:format_erlang_code(Code),
            file:write_file(FilePath, FormattedCode)
    end.

%% Generate handler file
generate_handler_file(FilePath, HandlerModule, MetadataModule, HandlerName, Paths, Overwrite) ->
    FileExists = filelib:is_file(FilePath),

    case {FileExists, Overwrite} of
        {true, false} ->
            % File exists and not overwriting - try to merge
            rebar_api:info("File exists: ~s - attempting to merge new trails", [FilePath]),
            merge_handler_file(FilePath, HandlerModule, MetadataModule, HandlerName, Paths);
        {true, true} ->
            % File exists but overwrite requested - regenerate completely
            rebar_api:info("Overwriting existing file: ~s", [FilePath]),
            generate_fresh_handler(FilePath, HandlerModule, MetadataModule, HandlerName, Paths);
        {false, _} ->
            % File doesn't exist - generate fresh
            generate_fresh_handler(FilePath, HandlerModule, MetadataModule, HandlerName, Paths)
    end.

%% Generate a fresh handler file
generate_fresh_handler(FilePath, HandlerModule, MetadataModule, HandlerName, Paths) ->
    Code = openapi_code_generator:generate_trails_module(HandlerModule, Paths, #{
        handler_name => HandlerName,
        metadata_module => MetadataModule
    }),
    FormattedCode = openapi_code_generator:format_erlang_code(Code),
    FinalCode = re:replace(FormattedCode, "handler_module", HandlerName, [global, {return, list}]),
    file:write_file(FilePath, FinalCode).

%% Format files using erlfmt
format_files(Files) ->
    lists:foreach(
        fun(File) ->
            try
                rebar_api:info("Formatting ~s with erlfmt", [File]),
                format_file_with_erlfmt(File)
            catch
                Error:Reason ->
                    rebar_api:warn("Failed to format ~s: ~p:~p", [File, Error, Reason]),
                    ok
            end
        end,
        Files
    ).

%% Format a single file with erlfmt
format_file_with_erlfmt(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            try
                % Use erlfmt to format the code
                Formatted = erlfmt:format_string(binary_to_list(Content)),
                file:write_file(FilePath, Formatted),
                ok
            catch
                _:_ ->
                    % If erlfmt fails, keep original
                    rebar_api:debug("erlfmt not available or failed, keeping original format", []),
                    ok
            end;
        {error, _} = Error ->
            Error
    end.

%% Print usage instructions
print_usage_instructions(HandlerModule, MetadataModule) ->
    rebar_api:info("", []),
    rebar_api:info("=================================================================", []),
    rebar_api:info("Next Steps:", []),
    rebar_api:info("", []),
    rebar_api:info("1. Review the generated files:", []),
    rebar_api:info("   - ~s.erl (metadata definitions)", [MetadataModule]),
    rebar_api:info("   - ~s.erl (trails handler)", [HandlerModule]),
    rebar_api:info("", []),
    rebar_api:info("2. In ~s.erl, replace 'handler_module' with your actual HTTP handler", [HandlerModule]),
    rebar_api:info("   Example: change 'handler_module' to 'my_http_handler'", []),
    rebar_api:info("", []),
    rebar_api:info("3. Implement the HTTP handler callbacks (init/2, handle/2, etc.)", []),
    rebar_api:info("", []),
    rebar_api:info("4. Integrate into your Cowboy routes:", []),
    rebar_api:info("   Trails = trails:trails([~s]),", [HandlerModule]),
    rebar_api:info("   trails:store(Trails),", []),
    rebar_api:info("   Dispatch = trails:single_host_compile(Trails)", []),
    rebar_api:info("", []),
    rebar_api:info("5. Compile and test:", []),
    rebar_api:info("   rebar3 compile", []),
    rebar_api:info("=================================================================", []),
    ok.

%% Format parse error
format_parse_error({file_read_error, Reason}) ->
    io_lib:format("Failed to read OpenAPI file: ~p", [Reason]);
format_parse_error({yaml_parse_error, Exception}) ->
    io_lib:format("Failed to parse YAML: ~p", [Exception]);
format_parse_error({parse_error, Reason, _Stack}) ->
    io_lib:format("Failed to parse OpenAPI spec: ~p", [Reason]);
format_parse_error(Reason) ->
    io_lib:format("Import failed: ~p", [Reason]).

%% Format error
-spec format_error(any()) -> iolist().
format_error({file_exists, FilePath}) ->
    io_lib:format("File already exists: ~s (use --overwrite to replace)", [FilePath]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Merge new metadata functions into existing file
merge_metadata_file(FilePath, ModuleName, NewPaths) ->
    case file:read_file(FilePath) of
        {ok, ExistingContent} ->
            % Extract existing function names and generate new ones
            ExistingFunctions = extract_metadata_functions(ExistingContent),
            NewFunctions = generate_metadata_function_map(NewPaths),

            % Find functions to add or update
            {ToAdd, ToUpdate} = classify_functions(NewFunctions, ExistingFunctions),

            case {length(ToAdd), length(ToUpdate)} of
                {0, 0} ->
                    rebar_api:info("No new or updated functions to merge", []),
                    ok;
                {AddCount, UpdateCount} ->
                    rebar_api:info(
                        "Adding ~p new function(s), updating ~p existing function(s)",
                        [AddCount, UpdateCount]
                    ),

                    % Generate updated content
                    UpdatedContent = update_metadata_module(
                        ExistingContent,
                        ModuleName,
                        NewFunctions,
                        ToAdd,
                        ToUpdate
                    ),
                    file:write_file(FilePath, UpdatedContent)
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%% Merge new trails into existing handler file
merge_handler_file(FilePath, _HandlerModule, MetadataModule, HandlerName, NewPaths) ->
    case file:read_file(FilePath) of
        {ok, ExistingContent} ->
            % Don't filter out existing paths - let add_trails_to_module handle them
            % It will automatically replace existing trails or add new ones
            case NewPaths of
                [] ->
                    rebar_api:info("No paths to process", []),
                    ok;
                _ ->
                    rebar_api:info("Processing ~p path(s) (adding new or updating existing)", [length(NewPaths)]),
                    UpdatedContent = add_trails_to_module(
                        ExistingContent,
                        NewPaths,
                        MetadataModule,
                        HandlerName
                    ),
                    file:write_file(FilePath, UpdatedContent)
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

%% Generate map of function_name -> function_code for paths
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

%% Update metadata module content
update_metadata_module(ExistingContent, ModuleName, NewFunctions, ToAdd, ToUpdate) ->
    % For simplicity, regenerate the entire module with all functions
    % In a more sophisticated version, we'd do AST-level merging

    % Extract existing functions that are not being updated
    _ExistingFuncs = extract_metadata_functions(ExistingContent),

    % Generate new module with all functions
    AllPaths = reconstruct_paths_from_functions(NewFunctions, ToAdd ++ ToUpdate),
    Code = openapi_code_generator:generate_metadata_module(ModuleName, AllPaths, #{}),
    FormattedCode = openapi_code_generator:format_erlang_code(Code),

    % Add comment about updated functions
    UpdateComment =
        case {length(ToAdd), length(ToUpdate)} of
            {0, 0} -> "";
            {A, U} -> io_lib:format("%% Updated by rebar3 openapi import: ~p new, ~p updated~n", [A, U])
        end,

    [UpdateComment, FormattedCode].

%% Reconstruct path data from function map
reconstruct_paths_from_functions(FunctionMap, FunctionNames) ->
    % Group by path
    Grouped = lists:foldl(
        fun(FuncName, Acc) ->
            case maps:get(FuncName, FunctionMap, undefined) of
                undefined ->
                    Acc;
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

%% Normalize OpenAPI path to Cowboy format
normalize_path_for_cowboy(Path) ->
    re:replace(Path, "\\{([^}]+)\\}", ":\\1", [global, {return, list}]).

%% Add new trails to existing module
add_trails_to_module(ExistingContent, PathsToAdd, MetadataModule, HandlerName) ->
    ContentStr = binary_to_list(ExistingContent),

    % Generate new trail entries as a map for easy lookup
    NewTrailsMap = lists:foldl(
        fun(PathData, Acc) ->
            Path = maps:get(path, PathData),
            Operations = maps:get(operations, PathData),
            CowboyPath = normalize_path_for_cowboy(binary_to_list(Path)),

            % Get function name from first operation
            [FirstMethod | _] = maps:keys(Operations),
            FuncName = path_to_function_name(Path, FirstMethod),

            TrailCode = io_lib:format(
                "trails:trail(\"~s\", ~s, [], ~s:~s())",
                [CowboyPath, HandlerName, MetadataModule, FuncName]
            ),
            Acc#{CowboyPath => lists:flatten(TrailCode)}
        end,
        #{},
        PathsToAdd
    ),

    % Try to automatically insert/replace trails in the function
    case try_auto_insert_trails(ContentStr, NewTrailsMap, HandlerName, MetadataModule) of
        {ok, UpdatedContent} ->
            list_to_binary(UpdatedContent);
        {error, _Reason} ->
            % Fallback to TODO comment approach
            add_todo_comment(ContentStr, NewTrailsMap, MetadataModule)
    end.

%% Try to automatically insert or replace trails in the trails/0 function
try_auto_insert_trails(ContentStr, NewTrailsMap, HandlerName, MetadataModule) ->
    % Find the trails/0 function body
    case
        re:run(
            ContentStr,
            "^trails\\(\\)\\s*->\\s*\\n(.+?)^[a-z_]",
            [multiline, dotall, {capture, all, index}]
        )
    of
        {match, [_FullMatch, {BodyStart, BodyLen}]} ->
            % Extract the function body
            FunctionBody = string:substr(ContentStr, BodyStart + 1, BodyLen),

            % Parse existing trails
            ExistingTrails = parse_trails_from_body(FunctionBody),

            % Determine what to add/replace
            {ToAdd, ToReplace} = categorize_trails(NewTrailsMap, ExistingTrails),

            case {map_size(ToAdd), map_size(ToReplace)} of
                {0, 0} ->
                    {error, no_changes_needed};
                _ ->
                    % Perform the modifications
                    UpdatedBody = apply_trail_modifications(
                        FunctionBody, ToAdd, ToReplace, HandlerName, MetadataModule
                    ),

                    % Reconstruct the file
                    Before = string:substr(ContentStr, 1, BodyStart),
                    After = string:substr(ContentStr, BodyStart + BodyLen + 1),
                    UpdatedContent = Before ++ UpdatedBody ++ After,
                    {ok, UpdatedContent}
            end;
        nomatch ->
            {error, trails_function_not_found}
    end.

%% Parse existing trail definitions from function body
parse_trails_from_body(Body) ->
    % Match both formats:
    % 1. trails:trail("/path", handler, [], metadata:func())
    % 2. {"/path", handler, []}
    TrailPattern = "trails:trail\\(\"([^\"]+)\"[^)]+\\)",
    TuplePattern = "\\{\"([^\"]+)\"",

    TrailPaths =
        case re:run(Body, TrailPattern, [global, {capture, all_but_first, list}]) of
            {match, Matches1} -> [Path || [Path] <- Matches1];
            nomatch -> []
        end,

    TuplePaths =
        case re:run(Body, TuplePattern, [global, {capture, all_but_first, list}]) of
            {match, Matches2} -> [Path || [Path] <- Matches2];
            nomatch -> []
        end,

    sets:from_list(TrailPaths ++ TuplePaths, [{version, 2}]).

%% Categorize trails into those to add vs replace
categorize_trails(NewTrailsMap, ExistingTrailsSet) ->
    maps:fold(
        fun(Path, TrailCode, {AddAcc, ReplaceAcc}) ->
            case sets:is_element(Path, ExistingTrailsSet) of
                true -> {AddAcc, ReplaceAcc#{Path => TrailCode}};
                false -> {AddAcc#{Path => TrailCode}, ReplaceAcc}
            end
        end,
        {#{}, #{}},
        NewTrailsMap
    ).

%% Apply trail modifications to the function body
apply_trail_modifications(Body, ToAdd, ToReplace, HandlerName, MetadataModule) ->
    % First, replace existing trails
    UpdatedBody = maps:fold(
        fun(Path, NewTrailCode, BodyAcc) ->
            % Match both old formats and replace with new trail format
            Pattern1 = io_lib:format("trails:trail\\(\"~s\"[^)]+\\)", [re:replace(Path, ":", "\\\\:", [global, {return, list}])]),
            Pattern2 = io_lib:format("\\{\"~s\"[^}]+\\}", [re:replace(Path, ":", "\\\\:", [global, {return, list}])]),

            Temp = re:replace(BodyAcc, Pattern1, NewTrailCode, [{return, list}, global]),
            re:replace(Temp, Pattern2, NewTrailCode, [{return, list}, global])
        end,
        Body,
        ToReplace
    ),

    % Then, add new trails
    case map_size(ToAdd) of
        0 -> UpdatedBody;
        _ -> insert_new_trails(UpdatedBody, ToAdd, HandlerName, MetadataModule)
    end.

%% Insert new trails into the function body
insert_new_trails(Body, ToAdd, _HandlerName, _MetadataModule) ->
    % Find the last trail entry or closing bracket to insert before
    NewTrailsList = lists:map(
        fun({_Path, TrailCode}) -> "        " ++ TrailCode end,
        maps:to_list(ToAdd)
    ),

    NewTrailsStr = string:join(NewTrailsList, ",\n"),

    % Try to find where to insert (before closing bracket or after last trail)
    case re:run(Body, "(\\])\\.\\s*$", [multiline, {capture, all, index}]) of
        {match, [_FullMatch, {BracketPos, _}]} ->
            % Insert before closing bracket
            Before = string:substr(Body, 1, BracketPos - 1),
            After = string:substr(Body, BracketPos),

            % Add comma if there's existing content
            case string:trim(Before) of
                "" -> Before ++ "\n" ++ NewTrailsStr ++ "\n    " ++ After;
                _ -> Before ++ ",\n" ++ NewTrailsStr ++ "\n    " ++ After
            end;
        _ ->
            % Couldn't parse, append with comment
            Body ++ "\n    %% TODO: Add these trails:\n    %% " ++
                string:join(NewTrailsList, "\n    %% ")
    end.

%% Fallback: Add TODO comment when auto-insertion fails
add_todo_comment(ContentStr, NewTrailsMap, _MetadataModule) ->
    NewTrails = lists:map(
        fun({Path, TrailCode}) ->
            io_lib:format("  - ~s\n%%         ~s", [Path, TrailCode])
        end,
        maps:to_list(NewTrailsMap)
    ),

    % Try to find trails() function
    case re:run(ContentStr, "^(trails\\(\\)\\s*->)", [multiline, {capture, all, index}]) of
        {match, [_FullMatch, {Start, Len}]} ->
            TodoComment = io_lib:format(
                "\n    %% TODO: Add/modify trails for the following route(s):\n"
                "%%     ~s\n",
                [string:join(NewTrails, "\n%%     ")]
            ),

            {Before, After} = lists:split(Start + Len, ContentStr),
            list_to_binary(Before ++ TodoComment ++ After);
        _ ->
            % Append at end
            Comment = io_lib:format(
                "\n\n%% TODO: Add trails/0 function with:\n"
                "%%     ~s\n",
                [string:join(NewTrails, "\n%%     ")]
            ),
            list_to_binary(ContentStr ++ Comment)
    end.
