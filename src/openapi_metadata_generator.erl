-module(openapi_metadata_generator).

-doc """
-------------------------------------------------------------------------------------------
Utility module to generate openapi_metadata/1 function stubs for existing handlers.
Analyzes handler modules and creates template metadata functions that developers can fill in.
-------------------------------------------------------------------------------------------
""".

-export([
    generate_stubs/1,
    generate_stubs_for_handler/1,
    format_stub_code/1
]).

-spec generate_stubs([module()]) -> binary().
generate_stubs(Handlers) ->
    AllStubs = lists:map(
        fun generate_stubs_for_handler/1,
        Handlers
    ),

    Header = <<
        "%% ============================================================\n",
        "%% Generated OpenAPI Metadata Stubs\n",
        "%% Copy these functions to your handler modules and fill in details\n",
        "%% ============================================================\n\n"
    >>,

    iolist_to_binary([Header | lists:join(<<"\n\n">>, AllStubs)]).

-spec generate_stubs_for_handler(module()) -> binary().
generate_stubs_for_handler(Module) ->
    case openapi_parser:parse_handler(Module) of
        {ok, HandlerInfo} ->
            Routes = maps:get(routes, HandlerInfo),
            Metadata = maps:get(metadata, HandlerInfo),

            % Find routes that need metadata
            UndocumentedRoutes = lists:filter(
                fun(Route) ->
                    Path = maps:get(path, Route),
                    not has_metadata_for_path(Path, Metadata)
                end,
                Routes
            ),

            case UndocumentedRoutes of
                [] ->
                    iolist_to_binary([
                        <<"%% Module: ">>, atom_to_binary(Module, utf8), <<"\n">>,
                        <<"%% All routes are documented!\n">>
                    ]);
                _ ->
                    format_stub_code(#{
                        module => Module,
                        routes => UndocumentedRoutes
                    })
            end;
        {error, _Reason} ->
            iolist_to_binary([
                <<"%% Module: ">>, atom_to_binary(Module, utf8), <<"\n">>,
                <<"%% ERROR: Could not parse handler\n">>
            ])
    end.

-doc """
-------------------------------------------------------------------------------------------
Formats stub code for a handler's undocumented routes.
-------------------------------------------------------------------------------------------
""".
-spec format_stub_code(map()) -> binary().
format_stub_code(Info) ->
    Module = maps:get(module, Info),
    Routes = maps:get(routes, Info),

    Header = iolist_to_binary([
        <<"%% ============================================================\n">>,
        <<"%% Module: ">>, atom_to_binary(Module, utf8), <<"\n">>,
        <<"%% Add this function to your handler module\n">>,
        <<"%% ============================================================\n\n">>
    ]),

    % Generate function clauses for each route
    Clauses = lists:map(
        fun(Route) ->
            generate_metadata_clause(Route)
        end,
        Routes
    ),

    FunctionDef = <<
        "-export([openapi_metadata/1]).\n\n",
        "%% @doc OpenAPI metadata for API endpoints\n"
    >>,

    ClausesCode = iolist_to_binary(lists:join(<<";\n">>, Clauses)),

    FallbackClause = <<
        ";\nopenapi_metadata(_) ->\n",
        "    #{}."
    >>,

    iolist_to_binary([Header, FunctionDef, ClausesCode, FallbackClause, <<"\n">>]).

-doc """
-------------------------------------------------------------------------------------------
Generates a single openapi_metadata/1 clause for a route.
-------------------------------------------------------------------------------------------
""".
-spec generate_metadata_clause(map()) -> binary().
generate_metadata_clause(Route) ->
    Path = maps:get(path, Route),
    Methods = maps:get(methods, Route),

    % Generate operation ID from path
    OpId = generate_operation_identifier(Path, Methods),

    % Generate method specifications
    MethodSpecs = lists:map(
        fun(Method) ->
            generate_method_spec(Method, Path)
        end,
        Methods
    ),

    MethodSpecsCode = iolist_to_binary(lists:join(<<",\n                ">>, MethodSpecs)),

    iolist_to_binary([
        <<"openapi_metadata(">>, atom_to_binary(OpId, utf8), <<") ->\n">>,
        <<"    #{\n">>,
        <<"        paths => #{\n">>,
        <<"            <<\"">>, Path, <<"\">> => #{\n">>,
        <<"                ">>, MethodSpecsCode, <<"\n">>,
        <<"            }\n">>,
        <<"        }\n">>,
        <<"    }">>
    ]).

-doc """
-------------------------------------------------------------------------------------------
Generates a method specification (GET, POST, etc.) for OpenAPI.
-------------------------------------------------------------------------------------------
""".
-spec generate_method_spec(binary(), binary()) -> binary().
generate_method_spec(Method, Path) ->
    MethodLower = string:lowercase(Method),
    Summary = generate_summary(Method, Path),
    OpId = generate_operation_id(Method, Path),

    iolist_to_binary([
        MethodLower, <<" => #{\n">>,
        <<"                    tags => [<<\"TODO\">>],\n">>,
        <<"                    summary => <<\"">>, Summary, <<"\">>,\n">>,
        <<"                    operationId => <<\"">>, OpId, <<"\">>,\n">>,
        <<"                    responses => #{\n">>,
        <<"                        <<\"200\">> => #{\n">>,
        <<"                            description => <<\"Success\">>\n">>,
        <<"                            %% TODO: Add response schema\n">>,
        <<"                        }\n">>,
        <<"                    }\n">>,
        <<"                    %% TODO: Add requestBody for POST/PUT/PATCH\n">>,
        <<"                }">>
    ]).

-doc """
-------------------------------------------------------------------------------------------
Generates an operation identifier atom from path.
Used as the parameter to openapi_metadata/1.
-------------------------------------------------------------------------------------------
""".
-spec generate_operation_identifier(binary(), [binary()]) -> atom().
generate_operation_identifier(Path, [Method | _]) ->
    % Convert "/api/put/racks" + "POST" -> post_api_put_racks
    PathParts = [P || P <- binary:split(Path, <<"/">>, [global]), P =/= <<>>],
    CleanParts = lists:map(
        fun(Part) ->
            case Part of
                <<"{", Rest/binary>> ->
                    Size = byte_size(Rest) - 1,
                    <<CleanPart:Size/binary, "}">> = Rest,
                    CleanPart;
                _ ->
                    Part
            end
        end,
        PathParts
    ),

    MethodLower = string:lowercase(Method),
    AtomStr = binary_to_list(iolist_to_binary([MethodLower, <<"_">> | lists:join(<<"_">>, CleanParts)])),
    list_to_atom(AtomStr);
generate_operation_identifier(_Path, []) ->
    get_unknown.

-doc """
-------------------------------------------------------------------------------------------
Generates a human-readable summary for a route.
-------------------------------------------------------------------------------------------
""".
-spec generate_summary(binary(), binary()) -> binary().
generate_summary(Method, Path) ->
    iolist_to_binary([Method, <<" ">>, Path]).

-doc """
-------------------------------------------------------------------------------------------
Generates an OpenAPI operation ID.
-------------------------------------------------------------------------------------------
""".
-spec generate_operation_id(binary(), binary()) -> binary().
generate_operation_id(Method, Path) ->
    PathParts = [P || P <- binary:split(Path, <<"/">>, [global]), P =/= <<>>],
    CleanParts = lists:map(
        fun(Part) ->
            case Part of
                <<"{", Rest/binary>> ->
                    Size = byte_size(Rest) - 1,
                    <<CleanPart:Size/binary, "}">> = Rest,
                    CleanPart;
                _ ->
                    Part
            end
        end,
        PathParts
    ),

    CamelParts = [capitalize(P) || P <- CleanParts],
    iolist_to_binary([string:lowercase(Method) | CamelParts]).

-doc """
-------------------------------------------------------------------------------------------
Capitalizes first letter of a binary string.
-------------------------------------------------------------------------------------------
""".
-spec capitalize(binary()) -> binary().
capitalize(<<First, Rest/binary>>) ->
    <<(string:to_upper(First)), Rest/binary>>;
capitalize(<<>>) ->
    <<>>.

-doc """
-------------------------------------------------------------------------------------------
Checks if a path has metadata defined.
-------------------------------------------------------------------------------------------
""".
-spec has_metadata_for_path(binary(), map()) -> boolean().
has_metadata_for_path(Path, Metadata) ->
    PathsMap = maps:get(paths, Metadata, #{}),
    maps:is_key(Path, PathsMap).
