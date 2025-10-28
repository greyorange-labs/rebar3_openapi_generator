-module(openapi_coverage).

-doc """
Coverage reporting module for OpenAPI documentation - REWRITTEN.
Properly categorizes routes as:
- Documented (100% complete - all must-have fields present)
- Incomplete (has metadata but missing required fields - shows specific issues)
- Undocumented (no metadata at all)
""".

-export([
    generate_report/2,
    format_report/1
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
    complete_routes := [route_status()],
    incomplete_routes := [route_status()],
    undocumented_routes := [route_status()]
}.

-type route_status() :: #{
    path := binary(),
    method := binary(),
    % Only for incomplete routes
    issues => [binary()]
}.

-export_type([coverage_report/0, handler_coverage/0, route_status/0]).

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
        length(maps:get(complete_routes, HC, [])) +
            length(maps:get(incomplete_routes, HC, [])) +
            length(maps:get(undocumented_routes, HC, []))
     || HC <- HandlerCoverages
    ]),

    TotalComplete = lists:sum([
        length(maps:get(complete_routes, HC, []))
     || HC <- HandlerCoverages
    ]),

    OverallCoverage =
        case TotalRoutes of
            0 -> 0.0;
            _ -> (TotalComplete / TotalRoutes) * 100
        end,

    #{
        app_name => AppName,
        handlers => HandlerCoverages,
        total_coverage => OverallCoverage,
        summary => format_summary(OverallCoverage, TotalComplete, TotalRoutes)
    }.

-spec analyze_handler(module()) -> {ok, handler_coverage()} | {error, term()}.
analyze_handler(Module) ->
    case openapi_parser:parse_handler(Module) of
        {ok, HandlerInfo} ->
            Routes = maps:get(routes, HandlerInfo),
            Metadata = maps:get(metadata, HandlerInfo),
            PathsMap = maps:get(paths, Metadata, #{}),

            % Analyze each route+method combination
            AllRouteStatuses = lists:flatmap(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    Methods = maps:get(methods, Route),
                    lists:map(
                        fun(Method) ->
                            MethodBin = ensure_binary(Method),
                            analyze_route_method(Path, MethodBin, PathsMap)
                        end,
                        Methods
                    )
                end,
                Routes
            ),

            % Categorize
            Complete = [R || {complete, R} <- AllRouteStatuses],
            Incomplete = [R || {incomplete, R} <- AllRouteStatuses],
            Undocumented = [R || {undocumented, R} <- AllRouteStatuses],

            TotalRoutes = length(Complete) + length(Incomplete) + length(Undocumented),
            CoveragePercent =
                case TotalRoutes of
                    0 -> 0.0;
                    _ -> (length(Complete) / TotalRoutes) * 100
                end,

            {ok, #{
                module => Module,
                coverage_percent => CoveragePercent,
                complete_routes => Complete,
                incomplete_routes => Incomplete,
                undocumented_routes => Undocumented
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% Analyze a single path+method combination
-spec analyze_route_method(binary(), binary(), map()) ->
    {complete, route_status()} | {incomplete, route_status()} | {undocumented, route_status()}.
analyze_route_method(Path, Method, PathsMap) ->
    io:format("[DEBUG] Analyzing ~s [~s]~n", [Path, Method]),
    io:format("[DEBUG] PathsMap keys: ~p~n", [maps:keys(PathsMap)]),
    case maps:get(Path, PathsMap, undefined) of
        undefined ->
            io:format("[DEBUG] No metadata found for path: ~s~n", [Path]),
            % No metadata at all
            {undocumented, #{path => Path, method => Method}};
        MethodsMap ->
            io:format("[DEBUG] Found metadata for path, MethodsMap: ~p~n", [MethodsMap]),
            MethodLower = list_to_binary(string:lowercase(binary_to_list(Method))),
            io:format("[DEBUG] Looking for method: ~s~n", [MethodLower]),
            case maps:get(MethodLower, MethodsMap, undefined) of
                undefined ->
                    io:format("[DEBUG] Method ~s not found in metadata~n", [MethodLower]),
                    % Path has metadata but not this method
                    {undocumented, #{path => Path, method => Method}};
                OpSpec ->
                    io:format("[DEBUG] Found OpSpec: ~p~n", [OpSpec]),
                    % Check completeness
                    Issues = check_completeness(OpSpec, Method),
                    case Issues of
                        [] ->
                            {complete, #{path => Path, method => Method}};
                        _ ->
                            {incomplete, #{path => Path, method => Method, issues => Issues}}
                    end
            end
    end.

%% Check if operation has all required fields for good documentation
-spec check_completeness(map(), binary()) -> [binary()].
check_completeness(OpSpec, Method) ->
    Issues = [],

    % Must-have: summary
    Issues1 =
        case maps:is_key(summary, OpSpec) of
            false -> [<<"Missing 'summary'">> | Issues];
            true -> Issues
        end,

    % Must-have: operationId
    Issues2 =
        case maps:is_key(operationId, OpSpec) of
            false -> [<<"Missing 'operationId'">> | Issues1];
            true -> Issues1
        end,

    % Must-have: description
    Issues3 =
        case maps:is_key(description, OpSpec) of
            false -> [<<"Missing 'description'">> | Issues2];
            true -> Issues2
        end,

    % Must-have: tags
    Issues4 =
        case maps:is_key(tags, OpSpec) of
            false -> [<<"Missing 'tags'">> | Issues3];
            true -> Issues3
        end,

    % Must-have: responses
    Issues5 =
        case maps:is_key(responses, OpSpec) of
            false ->
                [<<"Missing 'responses'">> | Issues4];
            true ->
                Responses = maps:get(responses, OpSpec),
                case map_size(Responses) > 0 of
                    false -> [<<"Empty 'responses'">> | Issues4];
                    true -> check_response_content(Responses, Issues4)
                end
        end,

    % Must-have: requestBody for POST/PUT/PATCH
    Issues6 =
        case lists:member(Method, [<<"POST">>, <<"PUT">>, <<"PATCH">>]) of
            true ->
                case maps:is_key(requestBody, OpSpec) of
                    false ->
                        [<<"Missing 'requestBody' for ", Method/binary>> | Issues5];
                    true ->
                        RB = maps:get(requestBody, OpSpec),
                        case maps:is_key(content, RB) of
                            false -> [<<"requestBody missing 'content'">> | Issues5];
                            true -> Issues5
                        end
                end;
            false ->
                Issues5
        end,

    Issues6.

%% Check if success responses have content defined
-spec check_response_content(map(), [binary()]) -> [binary()].
check_response_content(Responses, Acc) ->
    lists:foldl(
        fun({Code, ResponseSpec}, Issues) ->
            % Only check 2xx responses (except 204)
            case is_success_code(Code) andalso Code =/= <<"204">> of
                true ->
                    case maps:is_key(content, ResponseSpec) of
                        false ->
                            [<<"Response ", Code/binary, " missing 'content'">> | Issues];
                        true ->
                            Content = maps:get(content, ResponseSpec),
                            case map_size(Content) > 0 of
                                false -> [<<"Response ", Code/binary, " has empty 'content'">> | Issues];
                                true -> Issues
                            end
                    end;
                false ->
                    Issues
            end
        end,
        Acc,
        maps:to_list(Responses)
    ).

-spec is_success_code(binary()) -> boolean().
is_success_code(<<$2, _, _>>) -> true;
is_success_code(_) -> false.

-spec ensure_binary(term()) -> binary().
ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) when is_integer(V) -> list_to_binary(integer_to_list(V)).

-spec format_summary(float(), integer(), integer()) -> binary().
format_summary(Coverage, Complete, Total) ->
    iolist_to_binary(
        io_lib:format(
            "Coverage: ~.1f% (~s/~s routes complete)",
            [Coverage, integer_to_list(Complete), integer_to_list(Total)]
        )
    ).

-spec format_report(coverage_report()) -> iolist().
format_report(Report) ->
    AppName = maps:get(app_name, Report),
    Summary = maps:get(summary, Report),
    Handlers = maps:get(handlers, Report),
    Coverage = maps:get(total_coverage, Report),

    Icon =
        if
            Coverage >= 80.0 -> "[OK]";
            Coverage >= 50.0 -> "[WARN]";
            true -> "[FAIL]"
        end,

    Header = "\n================================================================\n",
    Title = io_lib:format("~s OpenAPI Coverage Report: ~s\n", [Icon, atom_to_list(AppName)]),
    Separator = "================================================================\n\n",
    SummaryLine = io_lib:format("[*] ~s\n\n", [Summary]),

    HandlerLines =
        case Handlers of
            [] -> ["No handlers found.\n"];
            _ -> lists:map(fun format_handler/1, Handlers)
        end,

    Footer = "\n================================================================\n",

    [Header, Title, Separator, SummaryLine, HandlerLines, Footer].

-spec format_handler(handler_coverage()) -> iolist().
format_handler(HC) ->
    Module = maps:get(module, HC),
    Coverage = maps:get(coverage_percent, HC),
    Complete = maps:get(complete_routes, HC),
    Incomplete = maps:get(incomplete_routes, HC),
    Undocumented = maps:get(undocumented_routes, HC),

    Icon =
        if
            Coverage >= 80.0 -> "[OK]";
            Coverage >= 50.0 -> "[WARN]";
            true -> "[FAIL]"
        end,

    ModuleLine = io_lib:format("~s ~s (~.1f% complete)\n", [Icon, atom_to_list(Module), Coverage]),

    CompleteLines =
        case Complete of
            [] ->
                [];
            _ ->
                [
                    io_lib:format("  + Complete (~s routes):\n", [integer_to_list(length(Complete))])
                    | lists:map(
                        fun(R) ->
                            io_lib:format("     ~s [~s]\n", [
                                maps:get(path, R),
                                maps:get(method, R)
                            ])
                        end,
                        Complete
                    )
                ]
        end,

    IncompleteLines =
        case Incomplete of
            [] ->
                [];
            _ ->
                [
                    io_lib:format("  ! Incomplete (~s routes):\n", [integer_to_list(length(Incomplete))])
                    | lists:map(
                        fun(R) ->
                            Issues = maps:get(issues, R),
                            IssuesStr = iolist_to_binary(lists:join(", ", Issues)),
                            io_lib:format("     ~s [~s]\n       Missing: ~s\n", [
                                maps:get(path, R),
                                maps:get(method, R),
                                IssuesStr
                            ])
                        end,
                        Incomplete
                    )
                ]
        end,

    UndocumentedLines =
        case Undocumented of
            [] ->
                [];
            _ ->
                [
                    io_lib:format("  X Undocumented (~s routes):\n", [integer_to_list(length(Undocumented))])
                    | lists:map(
                        fun(R) ->
                            Issues = maps:get(issues, R, [<<"No metadata defined">>]),
                            IssuesStr = iolist_to_binary(lists:join(", ", Issues)),
                            io_lib:format("     ~s [~s]\n       Reason: ~s\n", [
                                maps:get(path, R),
                                maps:get(method, R),
                                IssuesStr
                            ])
                        end,
                        Undocumented
                    )
                ]
        end,

    [ModuleLine, CompleteLines, IncompleteLines, UndocumentedLines, "\n"].
