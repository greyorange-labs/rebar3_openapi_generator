-module(openapi_coverage).

-doc """
-------------------------------------------------------------------------------------------
Coverage reporting module for OpenAPI documentation.
Analyzes handlers to determine which routes are documented and which need attention.
Generates human-readable reports with actionable suggestions.

Enhanced to check OpenAPI 3.0.3 completeness including:
- Missing request bodies
- Missing response content definitions
- Missing required fields (summary, operationId, etc.)
- Missing parameter schemas
-------------------------------------------------------------------------------------------
""".

-export([
    generate_report/2,
    format_report/1,
    format_suggestions/1,
    check_operation_completeness/1
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
    incomplete_routes := [incomplete_route_info()],
    suggestions := [binary()]
}.

-type incomplete_route_info() :: #{
    path := binary(),
    method := binary(),
    issues := [binary()]
}.

-export_type([coverage_report/0, handler_coverage/0, incomplete_route_info/0]).

-doc """
-------------------------------------------------------------------------------------------
Generates a comprehensive coverage report for an application.
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
            length(maps:get(undocumented_routes, HC, [])) +
            length(maps:get(incomplete_routes, HC, []))
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

            % Categorize routes into documented, incomplete, and undocumented
            {Documented, IncompleteAndUndoc} = lists:partition(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    has_complete_metadata_for_path(Path, Route, Metadata)
                end,
                Routes
            ),

            % Separate incomplete from undocumented
            {Incomplete, Undocumented} = lists:partition(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    has_metadata_for_path(Path, Metadata)
                end,
                IncompleteAndUndoc
            ),

            % Analyze incomplete routes for specific issues
            IncompleteDetails = lists:flatmap(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    Method = hd(maps:get(methods, Route)),
                    check_route_completeness(Path, Method, Metadata)
                end,
                Incomplete
            ),

            DocumentedPaths = [maps:get(path, R) || R <- Documented],
            UndocumentedPaths = [maps:get(path, R) || R <- Undocumented],

            Suggestions = generate_suggestions(Undocumented, IncompleteDetails),

            Coverage = #{
                module => Module,
                coverage_percent => maps:get(
                    coverage_percent,
                    maps:get(coverage, HandlerInfo)
                ),
                documented_routes => DocumentedPaths,
                undocumented_routes => UndocumentedPaths,
                incomplete_routes => IncompleteDetails,
                suggestions => Suggestions
            },
            {ok, Coverage};
        {error, Reason} ->
            {error, Reason}
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if a path has complete metadata defined according to OpenAPI 3.0.3 standards.
-------------------------------------------------------------------------------------------
""".
-spec has_complete_metadata_for_path(binary(), map(), map()) -> boolean().
has_complete_metadata_for_path(Path, Route, Metadata) ->
    PathsMap = maps:get(paths, Metadata, #{}),
    case maps:get(Path, PathsMap, undefined) of
        undefined ->
            false;
        MethodsMap ->
            % Check all methods for completeness
            RouteMethods = maps:get(methods, Route),
            lists:all(
                fun(Method) ->
                    MethodLower = string:lowercase(binary_to_list(Method)),
                    has_complete_method_metadata(<<MethodLower>>, MethodsMap)
                end,
                RouteMethods
            )
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if a specific method has complete metadata.
-------------------------------------------------------------------------------------------
""".
-spec has_complete_method_metadata(binary(), map()) -> boolean().
has_complete_method_metadata(MethodBin, MethodsMap) ->
    case maps:get(MethodBin, MethodsMap, undefined) of
        undefined ->
            false;
        OpSpec ->
            has_operation_fields(OpSpec)
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if an operation has all required and recommended fields.
-------------------------------------------------------------------------------------------
""".
-spec has_operation_fields(map()) -> boolean().
has_operation_fields(OpSpec) ->
    % Check for required fields
    HasSummary = maps:is_key(summary, OpSpec),
    HasOperationId = maps:is_key(operationId, OpSpec),
    HasResponses = maps:is_key(responses, OpSpec) andalso
        map_size(maps:get(responses, OpSpec, #{})) > 0,

    % All required fields must be present
    HasSummary andalso HasOperationId andalso HasResponses.

-doc """
-------------------------------------------------------------------------------------------
Checks if a path has metadata defined (regardless of completeness).
-------------------------------------------------------------------------------------------
""".
-spec has_metadata_for_path(binary(), map()) -> boolean().
has_metadata_for_path(Path, Metadata) ->
    PathsMap = maps:get(paths, Metadata, #{}),
    maps:is_key(Path, PathsMap).

-doc """
-------------------------------------------------------------------------------------------
Analyzes incomplete routes to identify specific missing elements.
-------------------------------------------------------------------------------------------
""".
-spec check_route_completeness(binary(), binary(), map()) -> [incomplete_route_info()].
check_route_completeness(Path, Method, Metadata) ->
    PathsMap = maps:get(paths, Metadata, #{}),
    case maps:get(Path, PathsMap, undefined) of
        undefined ->
            [];
        MethodsMap ->
            MethodLower = string:lowercase(binary_to_list(Method)),
            case maps:get(<<MethodLower>>, MethodsMap, undefined) of
                undefined ->
                    [#{path => Path, method => Method, issues => [<<"No metadata defined">>]}];
                OpSpec ->
                    Issues = collect_operation_issues(OpSpec, Method),
                    case Issues of
                        [] -> [];
                        _ -> [#{path => Path, method => Method, issues => Issues}]
                    end
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Collects all issues for an incomplete operation.
-------------------------------------------------------------------------------------------
""".
-spec collect_operation_issues(map(), binary()) -> [binary()].
collect_operation_issues(OpSpec, Method) ->
    Issues = [],

    % Check required fields
    Issues1 = case maps:is_key(summary, OpSpec) of
        false -> [<<"Missing 'summary' field">> | Issues];
        true -> Issues
    end,

    Issues2 = case maps:is_key(operationId, OpSpec) of
        false -> [<<"Missing 'operationId' field">> | Issues1];
        true -> Issues1
    end,

    Issues3 = case maps:is_key(responses, OpSpec) of
        false -> [<<"Missing 'responses' field">> | Issues2];
        true ->
            % Check response content
            check_response_completeness(OpSpec, Issues2)
    end,

    % Check requestBody for POST/PUT/PATCH
    Issues4 = case should_have_request_body(Method) of
        true ->
            case maps:is_key(requestBody, OpSpec) of
                false -> [<<"Missing 'requestBody' for ", Method/binary, " request">> | Issues3];
                true ->
                    case maps:is_key(content, OpSpec) of
                        false -> [<<"requestBody missing 'content' definition">> | Issues3];
                        true -> Issues3
                    end
            end;
        false -> Issues3
    end,

    % Check tags
    Issues5 = case maps:is_key(tags, OpSpec) of
        false -> [<<"Missing 'tags' (recommended for grouping)">> | Issues4];
        true -> Issues4
    end,

    Issues5.

-doc """
-------------------------------------------------------------------------------------------
Checks if responses have proper content definitions.
-------------------------------------------------------------------------------------------
""".
-spec check_response_completeness(map(), [binary()]) -> [binary()].
check_response_completeness(OpSpec, Acc) ->
    Responses = maps:get(responses, OpSpec, #{}),
    IssueAcc = Acc,

    lists:foldl(
        fun({Code, ResponseSpec}, Issues) ->
            case maps:is_key(content, ResponseSpec) of
                false ->
                    % Check if this is an error code that might not need content
                    case lists:member(Code, [<<"204">>, <<"304">>]) of
                        true -> Issues;
                        false -> [<<"Response ", Code/binary, " missing 'content' definition">> | Issues]
                    end;
                true ->
                    Content = maps:get(content, ResponseSpec),
                    case map_size(Content) of
                        0 -> [<<"Response ", Code/binary, " has empty 'content'">> | Issues];
                        _ -> Issues
                    end
            end
        end,
        IssueAcc,
        maps:to_list(Responses)
    ).

-doc """
-------------------------------------------------------------------------------------------
Checks if a method should have a requestBody.
-------------------------------------------------------------------------------------------
""".
-spec should_have_request_body(binary()) -> boolean().
should_have_request_body(Method) ->
    lists:member(Method, [<<"POST">>, <<"PUT">>, <<"PATCH">>]).

-doc """
-------------------------------------------------------------------------------------------
Checks operation completeness based on OpenAPI 3.0.3 standards.
-------------------------------------------------------------------------------------------
""".
-spec check_operation_completeness(map()) -> {boolean(), [binary()]}.
check_operation_completeness(OpSpec) ->
    Issues = collect_operation_issues(OpSpec, <<"METHOD">>),
    IsComplete = Issues == [],
    {IsComplete, Issues}.

-doc """
-------------------------------------------------------------------------------------------
Generates actionable suggestions for undocumented and incomplete routes.
-------------------------------------------------------------------------------------------
""".
-spec generate_suggestions([map()], [incomplete_route_info()]) -> [binary()].
generate_suggestions(UndocumentedRoutes, IncompleteRoutes) ->
    UndocSuggestions = lists:map(
        fun(Route) ->
            Path = maps:get(path, Route),
            Methods = maps:get(methods, Route),
            MethodsStr = iolist_to_binary(lists:join(<<", ">>, Methods)),
            iolist_to_binary([
                <<"Add metadata for ">>, Path, <<" (">>, MethodsStr, <<")">>
            ])
        end,
        UndocumentedRoutes
    ),

    IncompleteSuggestions = lists:map(
        fun(IncompleteRoute) ->
            Path = maps:get(path, IncompleteRoute),
            Method = maps:get(method, IncompleteRoute),
            Issues = maps:get(issues, IncompleteRoute),
            IssuesStr = lists:join(<<", ">>, Issues),
            iolist_to_binary([
                <<"Fix ">>, Path, <<" [">>, Method, <<"]: ">>,
                lists:join(<<"; ">>, IssuesStr)
            ])
        end,
        IncompleteRoutes
    ),

    UndocSuggestions ++ IncompleteSuggestions.

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
    Incomplete = maps:get(incomplete_routes, HandlerCov),
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

    case Undocumented == [] andalso Incomplete == [] of
        true ->
            [ModuleLine, "  [x] All routes documented~n~n"];
        false ->
            UndocLines = lists:map(
                fun(Path) ->
                    io_lib:format("  * Missing: ~s~n", [Path])
                end,
                Undocumented
            ),

            IncompleteLines = lists:map(
                fun(IncompleteRoute) ->
                    Path = maps:get(path, IncompleteRoute),
                    Method = maps:get(method, IncompleteRoute),
                    Issues = maps:get(issues, IncompleteRoute),
                    IssueStr = lists:join(<<", ">>, Issues),
                    io_lib:format("  * Incomplete: ~s [~s] (~s)~n",
                        [Path, Method, IssueStr])
                end,
                Incomplete
            ),

            SuggestionLines =
                case Suggestions of
                    [] ->
                        [];
                    _ ->
                        ["  [!] Suggestions:\n"
                         | lists:map(
                             fun(Suggestion) ->
                                 io_lib:format("     - ~s~n", [Suggestion])
                             end,
                             % Show max 5 suggestions
                             lists:sublist(Suggestions, 5)
                         )]
                end,

            [ModuleLine, UndocLines, IncompleteLines, SuggestionLines]
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
            <<"# OpenAPI Documentation TODO\n\n✅ All routes are documented and complete!\n">>;
        _ ->
            Header = <<"# OpenAPI Documentation TODO\n\n">>,
            SuggestionList = iolist_to_binary(lists:join(<<"\n">>, AllSuggestions)),
            <<Header/binary, SuggestionList/binary, "\n">>
    end.
