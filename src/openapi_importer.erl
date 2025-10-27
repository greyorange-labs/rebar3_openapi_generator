-module(openapi_importer).

-export([
    parse_openapi_file/1,
    parse_openapi_yaml/1
]).

%% @doc Parse an OpenAPI YAML file and return structured data
-spec parse_openapi_file(file:filename()) -> {ok, map()} | {error, term()}.
parse_openapi_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Content} ->
            parse_openapi_yaml(Content);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% @doc Parse OpenAPI YAML content and return structured data
-spec parse_openapi_yaml(binary()) -> {ok, map()} | {error, term()}.
parse_openapi_yaml(YamlContent) ->
    try
        % Parse YAML to Erlang terms
        [Doc] = yamerl_constr:string(binary_to_list(YamlContent)),

        % Convert to normalized map structure
        OpenApiSpec = normalize_openapi(Doc),

        % Extract paths and operations
        Paths = extract_paths(OpenApiSpec),

        {ok, #{
            spec => OpenApiSpec,
            paths => Paths,
            info => maps:get(<<"info">>, OpenApiSpec, #{}),
            servers => maps:get(<<"servers">>, OpenApiSpec, [])
        }}
    catch
        throw:Exception ->
            {error, {yaml_parse_error, Exception}};
        error:{badmatch, _} = Error:Stack ->
            {error, {parse_error, Error, Stack}};
        error:Reason:Stack ->
            {error, {parse_error, Reason, Stack}}
    end.

%% @doc Normalize yamerl output to maps
-spec normalize_openapi(term()) -> map().
normalize_openapi(PropList) when is_list(PropList) ->
    case is_proplist(PropList) of
        true ->
            maps:from_list([{normalize_key(K), normalize_openapi(V)} || {K, V} <- PropList]);
        false ->
            [normalize_openapi(Item) || Item <- PropList]
    end;
normalize_openapi(Value) ->
    Value.

%% @doc Normalize keys to binaries
normalize_key(Key) when is_list(Key) -> list_to_binary(Key);
normalize_key(Key) when is_binary(Key) -> Key;
normalize_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
normalize_key(Key) -> Key.

%% @doc Check if list is a proplist
is_proplist([]) -> true;
is_proplist([{_K, _V} | Rest]) -> is_proplist(Rest);
is_proplist(_) -> false.

%% @doc Extract all paths and their operations
-spec extract_paths(map()) -> [map()].
extract_paths(#{<<"paths">> := PathsMap}) ->
    maps:fold(
        fun(PathPattern, Operations, Acc) ->
            PathOps = extract_path_operations(PathPattern, Operations),
            [PathOps | Acc]
        end,
        [],
        PathsMap
    );
extract_paths(_) ->
    [].

%% @doc Extract operations for a single path
-spec extract_path_operations(binary(), map()) -> map().
extract_path_operations(PathPattern, Operations) when is_map(Operations) ->
    HttpMethods = [<<"get">>, <<"post">>, <<"put">>, <<"patch">>, <<"delete">>, <<"head">>, <<"options">>],

    Ops = lists:filtermap(
        fun(Method) ->
            case maps:get(Method, Operations, undefined) of
                undefined ->
                    false;
                OpSpec when is_map(OpSpec) ->
                    {true, {Method, normalize_operation(OpSpec)}};
                _ ->
                    false
            end
        end,
        HttpMethods
    ),

    #{
        path => PathPattern,
        operations => maps:from_list(Ops),
        parameters => maps:get(<<"parameters">>, Operations, [])
    };
extract_path_operations(PathPattern, _InvalidOperations) ->
    #{
        path => PathPattern,
        operations => #{},
        parameters => []
    }.

%% @doc Normalize an operation specification
-spec normalize_operation(map()) -> map().
normalize_operation(OpSpec) when is_map(OpSpec) ->
    #{
        summary => maps:get(<<"summary">>, OpSpec, <<>>),
        description => maps:get(<<"description">>, OpSpec, <<>>),
        operation_id => maps:get(<<"operationId">>, OpSpec, <<>>),
        tags => maps:get(<<"tags">>, OpSpec, []),
        parameters => normalize_parameters(maps:get(<<"parameters">>, OpSpec, [])),
        request_body => normalize_request_body(maps:get(<<"requestBody">>, OpSpec, undefined)),
        responses => normalize_responses(maps:get(<<"responses">>, OpSpec, #{})),
        deprecated => maps:get(<<"deprecated">>, OpSpec, false),
        security => maps:get(<<"security">>, OpSpec, [])
    }.

%% @doc Normalize parameters
-spec normalize_parameters(list()) -> [map()].
normalize_parameters(Params) when is_list(Params) ->
    [normalize_parameter(P) || P <- Params];
normalize_parameters(_) ->
    [].

%% @doc Normalize a single parameter
-spec normalize_parameter(map()) -> map().
normalize_parameter(Param) when is_map(Param) ->
    #{
        name => maps:get(<<"name">>, Param, <<>>),
        in => maps:get(<<"in">>, Param, <<"query">>),
        description => maps:get(<<"description">>, Param, <<>>),
        required => maps:get(<<"required">>, Param, false),
        deprecated => maps:get(<<"deprecated">>, Param, false),
        schema => maps:get(<<"schema">>, Param, #{}),
        example => maps:get(<<"example">>, Param, undefined),
        examples => maps:get(<<"examples">>, Param, #{})
    }.

%% @doc Normalize request body
-spec normalize_request_body(map() | undefined) -> map() | undefined.
normalize_request_body(undefined) ->
    undefined;
normalize_request_body(RequestBody) when is_map(RequestBody) ->
    #{
        description => maps:get(<<"description">>, RequestBody, <<>>),
        required => maps:get(<<"required">>, RequestBody, false),
        content => normalize_content(maps:get(<<"content">>, RequestBody, #{}))
    }.

%% @doc Normalize content (media types)
-spec normalize_content(map()) -> map().
normalize_content(Content) when is_map(Content) ->
    maps:map(
        fun(_MediaType, MediaTypeObj) ->
            #{
                schema => maps:get(<<"schema">>, MediaTypeObj, #{}),
                example => maps:get(<<"example">>, MediaTypeObj, undefined),
                examples => maps:get(<<"examples">>, MediaTypeObj, #{})
            }
        end,
        Content
    );
normalize_content(_) ->
    #{}.

%% @doc Normalize responses
-spec normalize_responses(map()) -> map().
normalize_responses(Responses) when is_map(Responses) ->
    maps:map(
        fun(_StatusCode, Response) ->
            normalize_response(Response)
        end,
        Responses
    );
normalize_responses(_) ->
    #{}.

%% @doc Normalize a single response
-spec normalize_response(map()) -> map().
normalize_response(Response) when is_map(Response) ->
    #{
        description => maps:get(<<"description">>, Response, <<>>),
        content => normalize_content(maps:get(<<"content">>, Response, #{})),
        headers => maps:get(<<"headers">>, Response, #{})
    }.
