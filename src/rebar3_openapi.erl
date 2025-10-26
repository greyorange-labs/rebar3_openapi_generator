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
        {example, "rebar3 openapi generate --app put"},
        {short_desc, "Generate OpenAPI specs from Cowboy handlers"},
        {desc,
            "Scans Cowboy handler modules in an application and generates OpenAPI 3.x specifications with coverage reporting."},
        {opts, [
            {app, $a, "app", atom, "Target application name (required)"},
            {output, $o, "output", string, "Output directory (default: ./docs/openapi/)"},
            {format, $f, "format", {string, "yaml"}, "Output format: yaml or json (default: yaml)"},
            {coverage, $c, "coverage", {boolean, true}, "Generate coverage report (default: true)"},
            {validate, $v, "validate", {boolean, false}, "Validate generated spec (default: false)"}
        ]}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),

    case proplists:get_value(app, Args) of
        undefined ->
            rebar_api:error("--app parameter is required", []),
            {error, missing_app_parameter};
        AppName ->
            execute_generation(State, AppName, Args)
    end.

-doc """
-------------------------------------------------------------------------------------------
Executes the OpenAPI spec generation workflow.
-------------------------------------------------------------------------------------------
""".
-spec execute_generation(rebar_state:t(), atom(), proplists:proplist()) ->
    {ok, rebar_state:t()} | {error, term()}.
execute_generation(State, AppName, Args) ->
    OutputDir = proplists:get_value(output, Args, "./docs/openapi/"),
    Format = proplists:get_value(format, Args, "yaml"),
    GenCoverage = proplists:get_value(coverage, Args, true),
    Validate = proplists:get_value(validate, Args, false),

    rebar_api:info("Generating OpenAPI spec for app: ~p", [AppName]),

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
                    output_dir => OutputDir
                },

                Spec = openapi_spec_builder:build_spec(AppName, Handlers, Options),

                % 3. Write to file
                OutputFile = filename:join(OutputDir, atom_to_list(AppName) ++ "." ++ Format),
                ok = filelib:ensure_dir(OutputFile),

                Content =
                    case Format of
                        "yaml" ->
                            yaml_encoder:encode(Spec);
                        "json" ->
                            jsx:prettify(jsx:encode(Spec));
                        _ ->
                            jsx:prettify(jsx:encode(Spec))
                    end,

                ok = file:write_file(OutputFile, Content),
                rebar_api:info("✓ Generated: ~s", [OutputFile]),

                % 4. Coverage report
                case GenCoverage of
                    true ->
                        generate_coverage_report(AppName, Handlers, OutputDir);
                    false ->
                        ok
                end,

                % 5. Validate
                case Validate of
                    true ->
                        validate_generated_spec(OutputFile);
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
    ok = file:write_file(CoverageFile, CoverageText),

    % Write suggestions as markdown
    SuggestionsFile = filename:join(OutputDir, atom_to_list(AppName) ++ "_todo.md"),
    SuggestionsText = openapi_coverage:format_suggestions(CoverageReport),
    ok = file:write_file(SuggestionsFile, SuggestionsText),

    % Print summary to console
    rebar_api:info("~n~s", [CoverageText]),
    rebar_api:info("✓ Coverage report: ~s", [CoverageFile]),
    rebar_api:info("✓ TODO list: ~s", [SuggestionsFile]),
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
    "Missing required --app parameter. Usage: rebar3 openapi generate --app <app_name>";
format_error({generation_failed, Reason}) ->
    io_lib:format("OpenAPI generation failed: ~p", [Reason]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
