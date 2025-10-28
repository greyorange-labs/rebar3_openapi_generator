-module(rebar3_openapi).

-doc """
-------------------------------------------------------------------------------------------
Rebar3 provider for generating OpenAPI specifications from Cowboy handlers.
Implements the main CLI interface and coordinates all plugin components.
-------------------------------------------------------------------------------------------
""".

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, generate).
-define(NAMESPACE, openapi).
-define(DEPS, [{default, compile}]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, ?NAMESPACE},
        {bare, false},
        {deps, ?DEPS},
        {example, "rebar3 openapi generate --app myapp --destination ./docs/api.yml"},
        {short_desc, "Generate OpenAPI specs from Cowboy handlers"},
        {desc,
            "Scans Cowboy handler modules in an application and generates OpenAPI 3.x specifications with coverage reporting."},
        {opts, [
            {app, $a, "app", atom, "Target application name (required)"},
            {destination, $d, "destination", string, "Output file path with filename, e.g. ./docs/api.yml (required)"},
            {coverage, $c, "coverage", {boolean, true}, "Generate coverage report (default: true)"},
            {validate, $v, "validate", {boolean, false}, "Validate generated spec (default: false)"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),

    % Validate required parameters
    case validate_required_params(Args) of
        ok ->
            AppName = proplists:get_value(app, Args),
            Destination = proplists:get_value(destination, Args),
            execute_generation(State, AppName, Destination, Args);
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
-------------------------------------------------------------------------------------------
Validates required parameters for the generate command.
-------------------------------------------------------------------------------------------
""".
-spec validate_required_params(proplists:proplist()) -> ok | {error, atom()}.
validate_required_params(Args) ->
    case proplists:get_value(app, Args) of
        undefined ->
            rebar_api:error("Missing required parameter: --app <app_name>", []),
            {error, missing_app_parameter};
        _ ->
            case proplists:get_value(destination, Args) of
                undefined ->
                    rebar_api:error("Missing required parameter: --destination <path/to/output.yml>", []),
                    {error, missing_destination_parameter};
                Destination ->
                    validate_destination_path(Destination)
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Validates the destination path and its file extension.
-------------------------------------------------------------------------------------------
""".
-spec validate_destination_path(string()) -> ok | {error, atom()}.
validate_destination_path(Destination) ->
    % Check file extension
    Extension = filename:extension(Destination),
    ValidExtensions = [".yml", ".yaml", ".json"],

    case lists:member(Extension, ValidExtensions) of
        false ->
            rebar_api:error(
                "Invalid file extension for --destination: ~s (expected .yml, .yaml, or .json)",
                [Extension]
            ),
            {error, invalid_destination_extension};
        true ->
            % Check if parent directory exists or can be created
            ParentDir = filename:dirname(Destination),
            case filelib:is_dir(ParentDir) of
                true ->
                    ok;
                false ->
                    % Try to create parent directory
                    case filelib:ensure_dir(Destination) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            rebar_api:error(
                                "Cannot create parent directory for --destination: ~s (reason: ~p)",
                                [ParentDir, Reason]
                            ),
                            {error, destination_dir_creation_failed}
                    end
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Executes the OpenAPI spec generation workflow.
-------------------------------------------------------------------------------------------
""".
-spec execute_generation(rebar_state:t(), atom(), string(), proplists:proplist()) ->
    {ok, rebar_state:t()} | {error, term()}.
execute_generation(State, AppName, Destination, Args) ->
    GenCoverage = proplists:get_value(coverage, Args, true),
    Validate = proplists:get_value(validate, Args, false),

    % Determine format from file extension
    Extension = filename:extension(Destination),
    Format = case Extension of
        ".json" -> "json";
        ".yml" -> "yaml";
        ".yaml" -> "yaml";
        _ -> "yaml"  % Default fallback
    end,

    rebar_api:info("Generating OpenAPI spec for app: ~p", [AppName]),
    rebar_api:info("Output destination: ~s", [Destination]),

    try
        % 1. Discover handlers
        Handlers = discover_handlers(State, AppName),

        case Handlers of
            [] ->
                rebar_api:warn("No handler modules found with trails/0 export in app ~p", [AppName]),
                {ok, State};
            _ ->
                rebar_api:info("Found ~p handler module(s)", [length(Handlers)]),

                % 2. Generate spec
                Options = #{
                    app_name => AppName,
                    format => Format,
                    output_dir => filename:dirname(Destination)
                },

                Spec = openapi_spec_builder:build_spec(AppName, Handlers, Options),

                % 3. Write to file
                ok = filelib:ensure_dir(Destination),

                Content =
                    case Format of
                        "yaml" ->
                            yaml_encoder:encode(Spec);
                        "json" ->
                            jsx:prettify(jsx:encode(Spec));
                        _ ->
                            jsx:prettify(jsx:encode(Spec))
                    end,

                ok = file:write_file(Destination, Content),
                rebar_api:info("✓ Generated: ~s", [Destination]),

                % 4. Coverage report (place in same directory as destination)
                case GenCoverage of
                    true ->
                        OutputDir = filename:dirname(Destination),
                        generate_coverage_report(AppName, Handlers, OutputDir);
                    false ->
                        ok
                end,

                % 5. Validate
                case Validate of
                    true ->
                        validate_generated_spec(Destination);
                    false ->
                        ok
                end,

                {ok, State}
        end
    catch
        Error:Reason:Stack ->
            rebar_api:error("Failed to generate OpenAPI spec: ~p:~p~n~p", [
                Error, Reason, Stack
            ]),
            {error, {generation_failed, Reason}}
    end.

-doc """
-------------------------------------------------------------------------------------------
Discovers handler modules in an application by scanning for trails/0 exports.
-------------------------------------------------------------------------------------------
""".
-spec discover_handlers(rebar_state:t(), atom()) -> [module()].
discover_handlers(State, AppName) ->
    % Get both project apps and all deps (for umbrella projects)
    ProjectApps = rebar_state:project_apps(State),
    AllDeps = rebar_state:all_deps(State),
    Apps = ProjectApps ++ AllDeps,

    % Debug: print found apps
    rebar_api:info("Project Apps: ~p", [[rebar_app_info:name(A) || A <- ProjectApps]]),
    rebar_api:info("All Deps: ~p", [[rebar_app_info:name(A) || A <- AllDeps]]),
    rebar_api:info("Looking for app: ~p (as binary: ~p)", [AppName, rebar_utils:to_binary(AppName)]),

    % Find the target app using standard rebar3 pattern
    case rebar_app_utils:find(rebar_utils:to_binary(AppName), Apps) of
        {ok, AppInfo} ->
            rebar_api:info("Found app: ~p", [rebar_app_info:name(AppInfo)]),
            EbinDir = rebar_app_info:ebin_dir(AppInfo),

            case filelib:is_dir(EbinDir) of
                false ->
                    rebar_api:warn("Ebin directory not found: ~s", [EbinDir]),
                    [];
                true ->
                    case file:list_dir(EbinDir) of
                        {ok, Files} ->
                            Beams = [F || F <- Files, filename:extension(F) =:= ".beam"],

                            Handlers = lists:filtermap(
                                fun(BeamFile) ->
                                    Module = list_to_atom(filename:basename(BeamFile, ".beam")),
                                    % Load the module to check exports
                                    case code:ensure_loaded(Module) of
                                        {module, Module} ->
                                            case erlang:function_exported(Module, trails, 0) of
                                                true -> {true, Module};
                                                false -> false
                                            end;
                                        _ ->
                                            false
                                    end
                                end,
                                Beams
                            ),

                            Handlers;
                        {error, Reason} ->
                            rebar_api:warn("Failed to list ebin directory ~s: ~p", [EbinDir, Reason]),
                            []
                    end
            end;
        error ->
            rebar_api:error(
                "App ~p not found in project. Available apps: ~p",
                [AppName, [rebar_app_info:name(A) || A <- Apps]]
            ),
            []
    end.

-doc """
-------------------------------------------------------------------------------------------
Generates and saves coverage report.
-------------------------------------------------------------------------------------------
""".
-spec generate_coverage_report(atom(), [module()], string()) -> ok.
generate_coverage_report(AppName, Handlers, OutputDir) ->
    CoverageReport = openapi_coverage:generate_report(AppName, Handlers),

    % Write human-readable report
    CoverageFile = filename:join(OutputDir, atom_to_list(AppName) ++ "_coverage.txt"),
    CoverageText = openapi_coverage:format_report(CoverageReport),
    CoverageTextBin = iolist_to_binary(CoverageText),
    ok = file:write_file(CoverageFile, CoverageTextBin),

    % Print summary to console
    rebar_api:info("~n~s", [CoverageText]),
    rebar_api:info("✓ Coverage report: ~s", [CoverageFile]),
    ok.

-doc """
-------------------------------------------------------------------------------------------
Validates a generated OpenAPI spec file.
-------------------------------------------------------------------------------------------
""".
-spec validate_generated_spec(string()) -> ok.
validate_generated_spec(OutputFile) ->
    case openapi_validator:validate_spec_file(OutputFile) of
        ok ->
            rebar_api:info("✓ Spec validation passed", []),
            ok;
        {error, Errors} when is_list(Errors) ->
            rebar_api:warn("⚠ Validation errors:", []),
            lists:foreach(
                fun(Error) ->
                    rebar_api:warn("  - ~s", [Error])
                end,
                Errors
            ),
            ok;
        {error, Error} ->
            rebar_api:warn("⚠ Validation error: ~p", [Error]),
            ok
    end.

-doc """
-------------------------------------------------------------------------------------------
Formats error messages for rebar3.
-------------------------------------------------------------------------------------------
""".
-spec format_error(term()) -> iolist().
format_error(missing_app_parameter) ->
    "Missing required parameter: --app <app_name>. Usage: rebar3 openapi generate --app <app_name> --destination <path/to/output.yml>";
format_error(missing_destination_parameter) ->
    "Missing required parameter: --destination <path/to/output.yml>. Usage: rebar3 openapi generate --app <app_name> --destination <path/to/output.yml>";
format_error(invalid_destination_extension) ->
    "Invalid file extension for --destination. Expected .yml, .yaml, or .json";
format_error(destination_dir_creation_failed) ->
    "Failed to create parent directory for --destination path";
format_error({generation_failed, Reason}) ->
    io_lib:format("OpenAPI generation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
