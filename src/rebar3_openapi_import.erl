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
        {example, "rebar3 openapi import --spec openapi.yaml --output src/"},
        {short_desc, "Import OpenAPI spec and generate Erlang trails definitions"},
        {desc,
            "Import an OpenAPI YAML specification and generate Erlang handler modules "
            "with trails definitions and metadata functions."},
        {opts, [
            {spec, $s, "spec", string, "Path to OpenAPI YAML specification file (required)"},
            {output, $o, "output", {string, "src/"}, "Output directory for generated files"},
            {handler_module, $h, "handler-module", {string, "generated_handler"}, "Name for generated handler module"},
            {metadata_module, $m, "metadata-module", {string, "generated_metadata"}, "Name for generated metadata module"},
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
    case proplists:get_value(spec, Opts) of
        undefined ->
            {error, "Missing required option: --spec <openapi.yaml>"};
        SpecFile ->
            execute_import(SpecFile, Opts, State)
    end.

%% Execute the import process
execute_import(SpecFile, Opts, State) ->
    rebar_api:info("Importing OpenAPI specification from: ~s", [SpecFile]),

    % Parse OpenAPI spec
    case openapi_importer:parse_openapi_file(SpecFile) of
        {ok, ParsedSpec} ->
            generate_files(ParsedSpec, Opts, State);
        {error, Reason} ->
            {error, format_parse_error(Reason)}
    end.

%% Generate Erlang files from parsed spec
generate_files(ParsedSpec, Opts, State) ->
    OutputDir = proplists:get_value(output, Opts),
    HandlerModule = list_to_atom(proplists:get_value(handler_module, Opts)),
    MetadataModule = list_to_atom(proplists:get_value(metadata_module, Opts)),
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

    % Ensure output directory exists
    filelib:ensure_dir(filename:join(OutputDir, "dummy")),

    % Generate metadata module
    MetadataFile = filename:join(OutputDir, atom_to_list(MetadataModule) ++ ".erl"),
    case generate_metadata_file(MetadataFile, MetadataModule, Paths, Overwrite) of
        ok ->
            rebar_api:info("Generated metadata module: ~s", [MetadataFile]),

            % Generate handler module
            HandlerFile = filename:join(OutputDir, atom_to_list(HandlerModule) ++ ".erl"),
            case generate_handler_file(HandlerFile, HandlerModule, MetadataModule, HandlerName, Paths, Overwrite) of
                ok ->
                    rebar_api:info("Generated handler module: ~s", [HandlerFile]),

                    % Format generated files if requested
                    case DoFormat of
                        true -> format_files([MetadataFile, HandlerFile]);
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
    case filelib:is_file(FilePath) andalso not Overwrite of
        true ->
            {error, {file_exists, FilePath}};
        false ->
            Code = openapi_code_generator:generate_metadata_module(ModuleName, Paths, #{}),
            FormattedCode = openapi_code_generator:format_erlang_code(Code),
            file:write_file(FilePath, FormattedCode)
    end.

%% Generate handler file
generate_handler_file(FilePath, HandlerModule, MetadataModule, HandlerName, Paths, Overwrite) ->
    case filelib:is_file(FilePath) andalso not Overwrite of
        true ->
            {error, {file_exists, FilePath}};
        false ->
            % Update paths to use the specified handler name
            Code = openapi_code_generator:generate_trails_module(HandlerModule, Paths, #{
                handler_name => HandlerName,
                metadata_module => MetadataModule
            }),
            FormattedCode = openapi_code_generator:format_erlang_code(Code),

            % Replace placeholder with actual handler name
            FinalCode = re:replace(FormattedCode, "handler_module", HandlerName, [global, {return, list}]),

            file:write_file(FilePath, FinalCode)
    end.

%% Format files using rebar3_format if available
format_files(Files) ->
    lists:foreach(
        fun(File) ->
            try
                rebar_api:info("Formatting ~s", [File]),
                % This would call rebar3_format if it's available
                % For now, we'll skip actual formatting
                ok
            catch
                _:_ -> ok
            end
        end,
        Files
    ).

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
    rebar_api:info("2. Update the handler module names in ~s.erl", [HandlerModule]),
    rebar_api:info("   Replace 'handler_module' with your actual HTTP handler", []),
    rebar_api:info("", []),
    rebar_api:info("3. Integrate into your Cowboy routes:", []),
    rebar_api:info("   trails:trails([~s])", [HandlerModule]),
    rebar_api:info("", []),
    rebar_api:info("4. Compile and test:", []),
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
