-module(openapi_code_generator).

-export([
    generate_trails_module/3,
    generate_metadata_module/3,
    format_erlang_code/1
]).

%% Generate a trails handler module from OpenAPI paths
-spec generate_trails_module(atom(), [map()], map()) -> iolist().
generate_trails_module(ModuleName, Paths, Options) ->
    ModuleNameStr = atom_to_list(ModuleName),
    MetadataModule = maps:get(metadata_module, Options, list_to_atom(ModuleNameStr ++ "_metadata")),

    TrailsList = generate_trails_list(Paths, MetadataModule),

    [
        "-module(",
        ModuleNameStr,
        ").\n",
        "-behaviour(trails_handler).\n\n",
        "-export([trails/0]).\n\n",
        "trails() ->\n",
        "    [\n",
        TrailsList,
        "    ].\n"
    ].

%% Generate metadata module from OpenAPI paths
-spec generate_metadata_module(atom(), [map()], map()) -> iolist().
generate_metadata_module(ModuleName, Paths, _Options) ->
    ModuleNameStr = atom_to_list(ModuleName),

    % Extract all unique operations
    Operations = extract_operations(Paths),

    % Generate exports
    Exports = generate_exports(Operations),

    % Generate metadata functions
    Functions = generate_metadata_functions(Operations),

    [
        "-module(",
        ModuleNameStr,
        ").\n\n",
        "-export([\n",
        Exports,
        "]).\n\n",
        Functions
    ].

%% Generate trails list entries
-spec generate_trails_list([map()], atom()) -> iolist().
generate_trails_list(Paths, MetadataModule) ->
    MetadataModuleStr = atom_to_list(MetadataModule),

    Trails = lists:map(
        fun(PathData) ->
            Path = maps:get(path, PathData),
            Operations = maps:get(operations, PathData),

            % Generate function name from path and method
            case maps:size(Operations) of
                0 ->
                    [];
                _ ->
                    % Use first operation to determine function name
                    [{Method, _Op} | _] = maps:to_list(Operations),
                    FuncName = path_to_function_name(Path, Method),
                    CowboyPath = convert_path_to_cowboy(Path),

                    [
                        "        trails:trail(\"",
                        CowboyPath,
                        "\", handler_module, [], ",
                        MetadataModuleStr,
                        ":",
                        FuncName,
                        "())"
                    ]
            end
        end,
        Paths
    ),

    string:join([T || T <- Trails, T =/= []], ",\n").

%% Generate exports list
-spec generate_exports([map()]) -> iolist().
generate_exports(Operations) ->
    Exports = lists:map(
        fun(Op) ->
            FuncName = maps:get(function_name, Op),
            ["    ", FuncName, "/0"]
        end,
        Operations
    ),

    string:join(Exports, ",\n") ++ "\n".

%% Generate metadata functions
-spec generate_metadata_functions([map()]) -> iolist().
generate_metadata_functions(Operations) ->
    lists:map(
        fun(Op) ->
            generate_metadata_function(Op)
        end,
        Operations
    ).

%% Generate a single metadata function
-spec generate_metadata_function(map()) -> iolist().
generate_metadata_function(Op) ->
    FuncName = maps:get(function_name, Op),
    Method = maps:get(method, Op),
    OpSpec = maps:get(spec, Op),

    [
        FuncName,
        "() ->\n",
        "    #{\n",
        "        ",
        Method,
        " => #{\n",
        generate_operation_fields(OpSpec, 3),
        "        }\n",
        "    }.\n\n"
    ].

%% Generate operation fields
-spec generate_operation_fields(map(), integer()) -> iolist().
generate_operation_fields(OpSpec, Indent) ->
    IndentStr = lists:duplicate(Indent * 4, $\s),

    Fields = [
        generate_tags_field(OpSpec, IndentStr),
        generate_summary_field(OpSpec, IndentStr),
        generate_description_field(OpSpec, IndentStr),
        generate_operation_id_field(OpSpec, IndentStr),
        generate_consumes_field(OpSpec, IndentStr),
        generate_produces_field(OpSpec, IndentStr),
        generate_parameters_field(OpSpec, IndentStr),
        generate_responses_field(OpSpec, IndentStr)
    ],

    ValidFields = [F || F <- Fields, F =/= []],
    string:join(ValidFields, ",\n").

%% Generate tags field
generate_tags_field(#{tags := Tags}, Indent) when Tags =/= [] ->
    TagsStr = format_string_array(Tags),
    [Indent, "tags => ", TagsStr];
generate_tags_field(_, _) ->
    [].

%% Generate summary field
generate_summary_field(#{summary := Summary}, Indent) when Summary =/= <<>> ->
    [Indent, "summary => <<\"", escape_string(Summary), "\">>"];
generate_summary_field(_, _) ->
    [].

%% Generate description field
generate_description_field(#{description := Desc}, Indent) when Desc =/= <<>> ->
    [Indent, "description => <<\"", escape_string(Desc), "\">>"];
generate_description_field(_, _) ->
    [].

%% Generate operationId field
generate_operation_id_field(#{operation_id := OpId}, Indent) when OpId =/= <<>> ->
    [Indent, "operationId => <<\"", escape_string(OpId), "\">>"];
generate_operation_id_field(_, _) ->
    [].

%% Generate consumes field
generate_consumes_field(#{request_body := ReqBody}, Indent) when ReqBody =/= undefined ->
    Content = maps:get(content, ReqBody, #{}),
    MediaTypes = maps:keys(Content),
    case MediaTypes of
        [] -> [];
        _ -> [Indent, "consumes => ", format_string_array(MediaTypes)]
    end;
generate_consumes_field(_, _) ->
    [].

%% Generate produces field
generate_produces_field(#{responses := Responses}, Indent) when map_size(Responses) > 0 ->
    % Extract media types from all success responses
    MediaTypes = extract_response_media_types(Responses),
    case MediaTypes of
        [] -> [];
        _ -> [Indent, "produces => ", format_string_array(MediaTypes)]
    end;
generate_produces_field(_, _) ->
    [].

%% Generate parameters field
generate_parameters_field(OpSpec, Indent) ->
    Params = maps:get(parameters, OpSpec, []),
    ReqBody = maps:get(request_body, OpSpec, undefined),

    AllParams =
        case ReqBody of
            undefined -> Params;
            _ -> Params ++ [generate_body_parameter(ReqBody)]
        end,

    case AllParams of
        [] ->
            [];
        _ ->
            ParamsStr = format_parameters(AllParams, Indent),
            [Indent, "parameters => [\n", ParamsStr, "\n", Indent, "]"]
    end.

%% Generate responses field
generate_responses_field(#{responses := Responses}, Indent) when map_size(Responses) > 0 ->
    ResponsesStr = format_responses(Responses, Indent),
    [Indent, "responses => #{\n", ResponsesStr, "\n", Indent, "}"];
generate_responses_field(_, _) ->
    [].

%% Format parameters
format_parameters(Params, BaseIndent) ->
    ParamIndent = BaseIndent ++ "    ",
    FormattedParams = lists:map(
        fun(Param) ->
            format_parameter(Param, ParamIndent)
        end,
        Params
    ),
    string:join(FormattedParams, ",\n").

%% Format a single parameter
format_parameter(#{name := Name, in := In, schema := Schema} = Param, Indent) ->
    Desc = maps:get(description, Param, <<>>),
    Required = maps:get(required, Param, false),

    [
        Indent,
        "#{\n",
        Indent,
        "    name => <<\"",
        escape_string(Name),
        "\">>,\n",
        Indent,
        "    in => <<\"",
        escape_string(In),
        "\">>,\n",
        if
            Desc =/= <<>> ->
                [Indent, "    description => <<\"", escape_string(Desc), "\">>,\n"];
            true ->
                []
        end,
        Indent,
        "    required => ",
        atom_to_list(Required),
        ",\n",
        Indent,
        "    schema => ",
        format_schema(Schema, Indent ++ "    "),
        "\n",
        Indent,
        "}"
    ].

%% Generate body parameter from request body
generate_body_parameter(#{content := Content} = ReqBody) ->
    Desc = maps:get(description, ReqBody, <<>>),
    Required = maps:get(required, ReqBody, false),

    % Get schema from first content type (usually application/json)
    Schema =
        case maps:to_list(Content) of
            [{_MediaType, #{schema := S}} | _] -> S;
            _ -> #{}
        end,

    #{
        name => <<"body">>,
        in => <<"body">>,
        description => Desc,
        required => Required,
        schema => Schema
    }.

%% Format responses
format_responses(Responses, BaseIndent) ->
    ResponseIndent = BaseIndent ++ "    ",
    FormattedResponses = lists:map(
        fun({StatusCode, Response}) ->
            format_response(StatusCode, Response, ResponseIndent)
        end,
        maps:to_list(Responses)
    ),
    string:join(FormattedResponses, ",\n").

%% Format a single response
format_response(StatusCode, #{description := Desc} = Response, Indent) ->
    Content = maps:get(content, Response, #{}),

    ContentStr =
        case maps:size(Content) of
            0 -> [];
            _ -> [",\n", Indent, "    content => ", format_content(Content, Indent ++ "    ")]
        end,

    [
        Indent,
        "<<\"",
        StatusCode,
        "\">> => #{\n",
        Indent,
        "    description => <<\"",
        escape_string(Desc),
        "\">>",
        ContentStr,
        "\n",
        Indent,
        "}"
    ].

%% Format content (media types and schemas)
format_content(Content, BaseIndent) ->
    ContentIndent = BaseIndent ++ "    ",
    FormattedContent = lists:map(
        fun({MediaType, MediaTypeObj}) ->
            Schema = maps:get(schema, MediaTypeObj, #{}),
            [
                ContentIndent,
                "<<\"",
                escape_string(MediaType),
                "\">> => #{\n",
                ContentIndent,
                "    schema => ",
                format_schema(Schema, ContentIndent ++ "    "),
                "\n",
                ContentIndent,
                "}"
            ]
        end,
        maps:to_list(Content)
    ),

    ["#{\n", string:join(FormattedContent, ",\n"), "\n", BaseIndent, "}"].

%% Format JSON schema
format_schema(Schema, Indent) when is_map(Schema) ->
    case maps:size(Schema) of
        0 ->
            "#{}";
        _ ->
            Fields = format_schema_fields(Schema, Indent),
            ["#{\n", Fields, "\n", Indent, "}"]
    end;
format_schema(_, _) ->
    "#{}".

%% Format schema fields
format_schema_fields(Schema, BaseIndent) ->
    FieldIndent = BaseIndent ++ "    ",

    Fields = lists:filtermap(
        fun({Key, Value}) ->
            case format_schema_field(Key, Value, FieldIndent) of
                [] -> false;
                Formatted -> {true, Formatted}
            end
        end,
        maps:to_list(Schema)
    ),

    string:join(Fields, ",\n").

%% Format individual schema field
format_schema_field(<<"type">>, Type, Indent) when is_binary(Type) ->
    [Indent, "type => <<\"", Type, "\">>"];
format_schema_field(<<"properties">>, Props, Indent) when is_map(Props) ->
    PropsStr = format_properties(Props, Indent),
    [Indent, "properties => ", PropsStr];
format_schema_field(<<"items">>, Items, Indent) when is_map(Items) ->
    ItemsStr = format_schema(Items, Indent),
    [Indent, "items => ", ItemsStr];
format_schema_field(<<"enum">>, Enum, Indent) when is_list(Enum) ->
    EnumStr = format_string_array(Enum),
    [Indent, "enum => ", EnumStr];
format_schema_field(<<"required">>, Required, Indent) when is_list(Required) ->
    ReqStr = format_string_array(Required),
    [Indent, "required => ", ReqStr];
format_schema_field(<<"minimum">>, Min, Indent) when is_integer(Min) ->
    [Indent, "minimum => ", integer_to_list(Min)];
format_schema_field(<<"maximum">>, Max, Indent) when is_integer(Max) ->
    [Indent, "maximum => ", integer_to_list(Max)];
format_schema_field(<<"minItems">>, Min, Indent) when is_integer(Min) ->
    [Indent, "minItems => ", integer_to_list(Min)];
format_schema_field(<<"maxItems">>, Max, Indent) when is_integer(Max) ->
    [Indent, "maxItems => ", integer_to_list(Max)];
format_schema_field(<<"minLength">>, Min, Indent) when is_integer(Min) ->
    [Indent, "minLength => ", integer_to_list(Min)];
format_schema_field(<<"maxLength">>, Max, Indent) when is_integer(Max) ->
    [Indent, "maxLength => ", integer_to_list(Max)];
format_schema_field(<<"uniqueItems">>, Unique, Indent) when is_boolean(Unique) ->
    [Indent, "uniqueItems => ", atom_to_list(Unique)];
format_schema_field(<<"format">>, Format, Indent) when is_binary(Format) ->
    [Indent, "format => <<\"", Format, "\">>"];
format_schema_field(<<"example">>, Example, Indent) ->
    [Indent, "example => ", format_value(Example)];
format_schema_field(<<"description">>, Desc, Indent) when is_binary(Desc) ->
    [Indent, "description => <<\"", escape_string(Desc), "\">>"];
format_schema_field(_, _, _) ->
    [].

%% Format properties object
format_properties(Props, BaseIndent) ->
    PropIndent = BaseIndent ++ "    ",
    FormattedProps = lists:map(
        fun({PropName, PropSchema}) ->
            [
                PropIndent,
                "<<\"",
                escape_string(PropName),
                "\">> => ",
                format_schema(PropSchema, PropIndent)
            ]
        end,
        maps:to_list(Props)
    ),

    ["#{\n", string:join(FormattedProps, ",\n"), "\n", BaseIndent, "}"].

%% Extract operations from paths
extract_operations(Paths) ->
    lists:flatmap(
        fun(PathData) ->
            Path = maps:get(path, PathData),
            Operations = maps:get(operations, PathData),

            lists:map(
                fun({Method, OpSpec}) ->
                    FuncName = path_to_function_name(Path, Method),
                    #{
                        function_name => FuncName,
                        method => binary_to_list(Method),
                        path => Path,
                        spec => OpSpec
                    }
                end,
                maps:to_list(Operations)
            )
        end,
        Paths
    ).

%% Convert OpenAPI path to Cowboy path pattern
convert_path_to_cowboy(Path) when is_binary(Path) ->
    % Convert {param} to :param
    Re = "\\{([^}]+)\\}",
    binary_to_list(re:replace(Path, Re, ":\\1", [global, {return, binary}]));
convert_path_to_cowboy(Path) when is_list(Path) ->
    convert_path_to_cowboy(list_to_binary(Path)).

%% Generate function name from path and method
path_to_function_name(Path, Method) when is_binary(Path), is_binary(Method) ->
    % Remove leading/trailing slashes
    CleanPath = string:trim(binary_to_list(Path), both, "/"),

    % Replace special characters with underscores
    SafePath = re:replace(CleanPath, "[^a-zA-Z0-9]+", "_", [global, {return, list}]),

    % Remove trailing underscores
    TrimmedPath = string:trim(SafePath, trailing, "_"),

    % Combine with method
    binary_to_list(Method) ++ "_" ++ string:to_lower(TrimmedPath).

%% Extract media types from responses
extract_response_media_types(Responses) ->
    MediaTypes = lists:flatmap(
        fun({StatusCode, Response}) ->
            % Only extract from 2xx responses
            case binary_to_list(StatusCode) of
                [$2 | _] ->
                    Content = maps:get(content, Response, #{}),
                    maps:keys(Content);
                _ ->
                    []
            end
        end,
        maps:to_list(Responses)
    ),

    lists:usort(MediaTypes).

%% Format string array
format_string_array(List) when is_list(List) ->
    Strings = lists:map(
        fun
            (Item) when is_binary(Item) ->
                ["<<\"", escape_string(Item), "\">>"];
            (Item) when is_list(Item) ->
                ["<<\"", escape_string(list_to_binary(Item)), "\">>"];
            (Item) ->
                io_lib:format("~p", [Item])
        end,
        List
    ),
    ["[", string:join(Strings, ", "), "]"];
format_string_array(_) ->
    "[]".

%% Format arbitrary value
format_value(Value) when is_binary(Value) ->
    ["<<\"", escape_string(Value), "\">>"];
format_value(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true -> ["\"", escape_string(list_to_binary(Value)), "\""];
        false -> format_string_array(Value)
    end;
format_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    float_to_list(Value);
format_value(Value) when is_boolean(Value) ->
    atom_to_list(Value);
format_value(Value) when is_map(Value) ->
    format_schema(Value, "");
format_value(_) ->
    "undefined".

%% Escape string for Erlang string literals
escape_string(Bin) when is_binary(Bin) ->
    escape_string(binary_to_list(Bin));
escape_string(Str) when is_list(Str) ->
    lists:flatmap(
        fun
            ($") -> "\\\"";
            ($\\) -> "\\\\";
            ($\n) -> "\\n";
            ($\r) -> "\\r";
            ($\t) -> "\\t";
            (C) -> [C]
        end,
        Str
    ).

%% Format generated Erlang code
format_erlang_code(Code) ->
    lists:flatten(Code).
