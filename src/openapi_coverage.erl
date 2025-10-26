-module(openapi_coverage).

-doc """
-------------------------------------------------------------------------------------------
Coverage reporting module for OpenAPI documentation.
Analyzes handlers to determine which routes are documented and which need attention.
Generates human-readable reports with actionable suggestions.
-------------------------------------------------------------------------------------------
""".

-export([
    generate_report/2,
    format_report/1,
    format_suggestions/1
]).

-type coverage_report() :: #{
    app_name := atom(),
    handlers := [handler_coverage()],
    total_coverage := float(),
    summary := binary()
}.

-type handler_coverage() :: #{
    module := module(),
    coverage_percent := float(),
    documented_routes := [binary()],
    undocumented_routes := [binary()],
    suggestions := [binary()]
}.

-export_type([coverage_report/0, handler_coverage/0]).

-doc """
-------------------------------------------------------------------------------------------
Generates a comprehensive coverage report for an application.

## Parameters
- `AppName` - Application name atom
- `Handlers` - List of handler module atoms to analyze

## Returns
- `coverage_report()` - Complete coverage analysis with per-handler breakdown

## Example
```erlang
Report = openapi_coverage:generate_report(put, [put_http_handler]),
io:format("~s~n", [openapi_coverage:format_report(Report)]).
```
-------------------------------------------------------------------------------------------
""".
-spec generate_report(atom(), [module()]) -> coverage_report().
generate_report(AppName, Handlers) ->
    HandlerCoverages = lists:filtermap(
        fun(Module) ->
            case analyze_handler(Module) of
                {ok, Coverage} -> {true, Coverage};
                {error, _} -> false
            end
        end,
        Handlers
    ),

    TotalRoutes = lists:sum([
        length(maps:get(documented_routes, HC, [])) +
            length(maps:get(undocumented_routes, HC, []))
     || HC <- HandlerCoverages
    ]),

    TotalDocumented = lists:sum([
        length(maps:get(documented_routes, HC, []))
     || HC <- HandlerCoverages
    ]),

    OverallCoverage =
        case TotalRoutes of
            0 -> 0.0;
            _ -> (TotalDocumented / TotalRoutes) * 100
        end,

    #{
        app_name => AppName,
        handlers => HandlerCoverages,
        total_coverage => OverallCoverage,
        summary => format_summary(OverallCoverage, TotalDocumented, TotalRoutes)
    }.

-doc """
-------------------------------------------------------------------------------------------
Analyzes a single handler module for coverage.
-------------------------------------------------------------------------------------------
""".
-spec analyze_handler(module()) -> {ok, handler_coverage()} | {error, term()}.
analyze_handler(Module) ->
    case openapi_parser:parse_handler(Module) of
        {ok, HandlerInfo} ->
            Routes = maps:get(routes, HandlerInfo),
            Metadata = maps:get(metadata, HandlerInfo),

            {Documented, Undocumented} = lists:partition(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    has_metadata_for_path(Path, Metadata)
                end,
                Routes
            ),

            DocumentedPaths = [maps:get(path, R) || R <- Documented],
            UndocumentedPaths = [maps:get(path, R) || R <- Undocumented],

            Suggestions = generate_suggestions(Undocumented),

            Coverage = #{
                module => Module,
                coverage_percent => maps:get(
                    coverage_percent,
                    maps:get(coverage, HandlerInfo)
                ),
                documented_routes => DocumentedPaths,
                undocumented_routes => UndocumentedPaths,
                suggestions => Suggestions
            },
            {ok, Coverage};
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if a path has metadata defined.
-------------------------------------------------------------------------------------------
""".
-spec has_metadata_for_path(binary(), map()) -> boolean().
has_metadata_for_path(Path, Metadata) ->
    PathsMap = maps:get(paths, Metadata, #{}),
    maps:is_key(Path, PathsMap).

-doc """
-------------------------------------------------------------------------------------------
Generates actionable suggestions for undocumented routes.
-------------------------------------------------------------------------------------------
""".
-spec generate_suggestions([map()]) -> [binary()].
generate_suggestions(UndocumentedRoutes) ->
    lists:map(
        fun(Route) ->
            Path = maps:get(path, Route),
            Methods = maps:get(methods, Route),
            MethodsStr = iolist_to_binary(lists:join(<<", ">>, Methods)),

            iolist_to_binary([
                <<"Add metadata for ">>, Path, <<" (">>, MethodsStr, <<")">>
            ])
        end,
        UndocumentedRoutes
    ).

-doc """
-------------------------------------------------------------------------------------------
Formats coverage summary as human-readable text.
-------------------------------------------------------------------------------------------
""".
-spec format_summary(float(), integer(), integer()) -> binary().
format_summary(Coverage, Documented, Total) ->
    iolist_to_binary(
        io_lib:format(
            "Coverage: ~.1f% (~p/~p routes documented)",
            [Coverage, Documented, Total]
        )
    ).

-doc """
-------------------------------------------------------------------------------------------
Formats complete coverage report as human-readable text.
-------------------------------------------------------------------------------------------
""".
-spec format_report(coverage_report()) -> binary().
format_report(Report) ->
    AppName = maps:get(app_name, Report),
    Summary = maps:get(summary, Report),
    Handlers = maps:get(handlers, Report),
    Coverage = maps:get(total_coverage, Report),

    CoverageIcon =
        if
            Coverage >= 80.0 -> "[OK]";
            Coverage >= 50.0 -> "[WARN]";
            true -> "[FAIL]"
        end,

    Header = io_lib:format(
        "~n================================================================~n",
        []
    ),
    Title = io_lib:format(
        "~s OpenAPI Coverage Report: ~p~n",
        [CoverageIcon, AppName]
    ),
    Separator = io_lib:format(
        "================================================================~n~n",
        []
    ),
    SummaryLine = io_lib:format("[*] ~s~n~n", [Summary]),

    HandlerLines =
        case Handlers of
            [] ->
                ["No handlers found with trails/0 export.~n"];
            _ ->
                lists:map(fun format_handler_coverage/1, Handlers)
        end,

    Footer = io_lib:format(
        "~n================================================================~n",
        []
    ),

    iolist_to_binary([Header, Title, Separator, SummaryLine, HandlerLines, Footer]).

-doc """
-------------------------------------------------------------------------------------------
Formats a single handler's coverage information.
-------------------------------------------------------------------------------------------
""".
-spec format_handler_coverage(handler_coverage()) -> iolist().
format_handler_coverage(HandlerCov) ->
    Module = maps:get(module, HandlerCov),
    Coverage = maps:get(coverage_percent, HandlerCov),
    _Documented = maps:get(documented_routes, HandlerCov),
    Undocumented = maps:get(undocumented_routes, HandlerCov),
    Suggestions = maps:get(suggestions, HandlerCov, []),

    Icon =
        if
            Coverage >= 80.0 -> "[OK]";
            Coverage >= 50.0 -> "[WARN]";
            true -> "[FAIL]"
        end,

    ModuleLine = io_lib:format(
        "~s ~p (~.1f% documented)~n",
        [Icon, Module, Coverage]
    ),

    case Undocumented of
        [] ->
            [ModuleLine, "  [x] All routes documented~n~n"];
        _ ->
            UndocLines = lists:map(
                fun(Path) ->
                    io_lib:format("  * Missing: ~s~n", [Path])
                end,
                Undocumented
            ),

            SuggestionLines =
                case Suggestions of
                    [] ->
                        [];
                    _ ->
                        [
                            "~n  [!] Suggestions:~n"
                            | lists:map(
                                fun(Suggestion) ->
                                    io_lib:format("     - ~s~n", [Suggestion])
                                end,
                                % Show max 3 suggestions
                                lists:sublist(Suggestions, 3)
                            )
                        ]
                end,

            [ModuleLine, UndocLines, SuggestionLines, "\n"]
    end.

-doc """
-------------------------------------------------------------------------------------------
Formats suggestions as actionable markdown-style text.
Useful for generating TODO lists or issue descriptions.
-------------------------------------------------------------------------------------------
""".
-spec format_suggestions(coverage_report()) -> binary().
format_suggestions(Report) ->
    Handlers = maps:get(handlers, Report),

    AllSuggestions = lists:flatmap(
        fun(HandlerCov) ->
            Module = maps:get(module, HandlerCov),
            Suggestions = maps:get(suggestions, HandlerCov, []),

            lists:map(
                fun(Suggestion) ->
                    iolist_to_binary([
                        <<"- [ ] ">>,
                        atom_to_binary(Module, utf8),
                        <<": ">>,
                        Suggestion
                    ])
                end,
                Suggestions
            )
        end,
        Handlers
    ),

    case AllSuggestions of
        [] ->
            <<"# OpenAPI Documentation TODO\n\n✅ All routes are documented!\n">>;
        _ ->
            Header = <<"# OpenAPI Documentation TODO\n\n">>,
            SuggestionList = iolist_to_binary(lists:join(<<"\n">>, AllSuggestions)),
            <<Header/binary, SuggestionList/binary, "\n">>
    end.
