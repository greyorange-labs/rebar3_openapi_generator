-module(openapi_spec_builder).

-doc """
-------------------------------------------------------------------------------------------
Builds complete OpenAPI 3.x specification documents from parsed handler information.
Handles schema discovery, path assembly, and OpenAPI document generation.
-------------------------------------------------------------------------------------------
""".

-export([
    build_spec/3,
    build_spec/2,
    merge_handler_specs/1,
    extract_jesse_schemas/2
]).

-type openapi_spec() :: #{
    openapi := binary(),
    info := map(),
    servers := [map()],
    paths := map(),
    components := map()
}.

-export_type([openapi_spec/0]).

-doc """
-------------------------------------------------------------------------------------------
Builds a complete OpenAPI 3.x specification for an application.

## Parameters
- `AppName` - Application name atom
- `Handlers` - List of handler module atoms
- `Options` - Configuration map with keys:
  - `version` - API version (default: app version)
  - `description` - API description
  - `servers` - List of server configurations
  - `include_schemas` - Whether to include Jesse schemas (default: true)

## Returns
- `openapi_spec()` - Complete OpenAPI 3.x specification

## Example
```erlang
Spec = openapi_spec_builder:build_spec(
    put,
    [put_http_handler],
    #{version => <<"1.0.0">>, description => <<"Put App API">>}
).
```
-------------------------------------------------------------------------------------------
""".
-spec build_spec(atom(), [module()], map()) -> openapi_spec().
build_spec(AppName, Handlers, Options) ->
    HandlerSpecs = lists:filtermap(
        fun(Module) ->
            case openapi_parser:parse_handler(Module) of
                {ok, HandlerInfo} ->
                    {true, build_handler_spec(HandlerInfo)};
                {error, _Reason} ->
                    false
            end
        end,
        Handlers
    ),

    MergedPaths = merge_handler_specs(HandlerSpecs),

    IncludeSchemas = maps:get(include_schemas, Options, true),
    AllSchemas = case IncludeSchemas of
        true -> extract_jesse_schemas(AppName, Options);
        false -> #{}
    end,

    #{
        <<"openapi">> => <<"3.0.3">>,
        <<"info">> => build_info(AppName, Options),
        <<"servers">> => build_servers(Options),
        <<"paths">> => MergedPaths,
        <<"components">> => #{
            <<"schemas">> => AllSchemas
        }
    }.

-doc """
-------------------------------------------------------------------------------------------
Builds spec with default options.
-------------------------------------------------------------------------------------------
""".
-spec build_spec(atom(), [module()]) -> openapi_spec().
build_spec(AppName, Handlers) ->
    build_spec(AppName, Handlers, #{}).

-doc """
-------------------------------------------------------------------------------------------
Builds OpenAPI info object.
-------------------------------------------------------------------------------------------
""".
-spec build_info(atom(), map()) -> map().
build_info(AppName, Options) ->
    Version = maps:get(version, Options, get_app_version(AppName)),
    Description = maps:get(
        description,
        Options,
        iolist_to_binary([
            <<"API for ">>,
            format_app_title(AppName),
            <<" - Generated from Erlang/Cowboy handlers">>
        ])
    ),

    #{
        <<"title">> => format_app_title(AppName),
        <<"version">> => Version,
        <<"description">> => Description
    }.

-doc """
-------------------------------------------------------------------------------------------
Builds OpenAPI servers array.
-------------------------------------------------------------------------------------------
""".
-spec build_servers(map()) -> [map()].
build_servers(Options) ->
    DefaultServers = [
        #{
            <<"url">> => <<"http://localhost:8080">>,
            <<"description">> => <<"Development server">>
        }
    ],
    maps:get(servers, Options, DefaultServers).

-doc """
-------------------------------------------------------------------------------------------
Builds path specifications for a single handler.
-------------------------------------------------------------------------------------------
""".
-spec build_handler_spec(map()) -> map().
build_handler_spec(HandlerInfo) ->
    Metadata = maps:get(metadata, HandlerInfo),
    Routes = maps:get(routes, HandlerInfo),

    % Use explicit metadata if available
    ExplicitPaths = maps:get(paths, Metadata, #{}),

    % Generate minimal specs for undocumented routes
    AutoPaths = generate_auto_specs(Routes, Metadata),

    % Explicit metadata takes precedence
    maps:merge(AutoPaths, ExplicitPaths).

-doc """
-------------------------------------------------------------------------------------------
Generates minimal OpenAPI specs for routes without explicit metadata.
-------------------------------------------------------------------------------------------
""".
-spec generate_auto_specs([map()], map()) -> map().
generate_auto_specs(Routes, ExistingMetadata) ->
    ExistingPaths = maps:get(paths, ExistingMetadata, #{}),

    lists:foldl(
        fun(Route, Acc) ->
            Path = maps:get(path, Route),

            % Skip if already has metadata
            case maps:is_key(Path, ExistingPaths) of
                true ->
                    Acc;
                false ->
                    Methods = maps:get(methods, Route),
                    PathSpec = lists:foldl(
                        fun(Method, MethodAcc) ->
                            MethodLower = string:lowercase(Method),
                            MethodAcc#{
                                MethodLower => generate_minimal_operation(Method, Path)
                            }
                        end,
                        #{},
                        Methods
                    ),
                    Acc#{Path => PathSpec}
            end
        end,
        #{},
        Routes
    ).

-doc """
-------------------------------------------------------------------------------------------
Generates a minimal operation spec for auto-generated routes.
-------------------------------------------------------------------------------------------
""".
-spec generate_minimal_operation(binary(), binary()) -> map().
generate_minimal_operation(Method, Path) ->
    #{
        <<"summary">> => generate_summary(Method, Path),
        <<"operationId">> => generate_operation_id(Method, Path),
        <<"tags">> => [<<"Auto-generated">>],
        <<"responses">> => #{
            <<"200">> => #{
                <<"description">> => <<"Success">>
            },
            <<"404">> => #{
                <<"description">> => <<"Not Found">>
            },
            <<"500">> => #{
                <<"description">> => <<"Internal Server Error">>
            }
        }
    }.

-doc """
-------------------------------------------------------------------------------------------
Generates a human-readable summary from method and path.
-------------------------------------------------------------------------------------------
""".
-spec generate_summary(binary(), binary()) -> binary().
generate_summary(Method, Path) ->
    iolist_to_binary([Method, <<" ">>, Path]).

-doc """
-------------------------------------------------------------------------------------------
Generates an operation ID from method and path.
Converts "/api/put/racks" + "POST" -> "postApiPutRacks"
-------------------------------------------------------------------------------------------
""".
-spec generate_operation_id(binary(), binary()) -> binary().
generate_operation_id(Method, Path) ->
    % Split path and remove empty segments
    PathParts = [P || P <- binary:split(Path, <<"/">>, [global]), P =/= <<>>],

    % Remove path parameters' braces
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

    % Capitalize each part
    CamelParts = [capitalize(P) || P <- CleanParts],

    % Prepend lowercase method
    iolist_to_binary([string:lowercase(Method) | CamelParts]).

-doc """
-------------------------------------------------------------------------------------------
Capitalizes the first letter of a binary string.
-------------------------------------------------------------------------------------------
""".
-spec capitalize(binary()) -> binary().
capitalize(<<First, Rest/binary>>) ->
    <<(string:to_upper(First)), Rest/binary>>;
capitalize(<<>>) ->
    <<>>.

-doc """
-------------------------------------------------------------------------------------------
Merges multiple handler path specifications.
Later handlers override earlier ones for the same path+method.
-------------------------------------------------------------------------------------------
""".
-spec merge_handler_specs([map()]) -> map().
merge_handler_specs(Specs) ->
    lists:foldl(
        fun(Spec, Acc) ->
            maps:merge_with(
                fun(_Path, Methods1, Methods2) ->
                    % Merge methods for the same path
                    maps:merge(Methods1, Methods2)
                end,
                Acc,
                Spec
            )
        end,
        #{},
        Specs
    ).

-doc """
-------------------------------------------------------------------------------------------
Extracts all Jesse schemas from an application's priv/schemas directory.
Converts them to OpenAPI 3.x schema format.

## Parameters
- `AppName` - Application name atom
- `Options` - Configuration map with optional `schema_dir` key

## Returns
- Map of schema name to OpenAPI schema object
-------------------------------------------------------------------------------------------
""".
-spec extract_jesse_schemas(atom(), map()) -> map().
extract_jesse_schemas(AppName, Options) ->
    SchemaDir = maps:get(
        schema_dir,
        Options,
        case code:priv_dir(AppName) of
            {error, bad_name} ->
                % App not loaded, skip schemas
                undefined;
            PrivDir ->
                filename:join(PrivDir, "schemas")
        end
    ),

    case SchemaDir of
        undefined ->
            #{};
        Dir ->
            case filelib:is_dir(Dir) of
                false ->
                    #{};
                true ->
                    discover_and_convert_schemas(Dir)
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Discovers and converts all JSON schema files in a directory.
-------------------------------------------------------------------------------------------
""".
-spec discover_and_convert_schemas(string()) -> map().
discover_and_convert_schemas(SchemaDir) ->
    case file:list_dir(SchemaDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],

            lists:foldl(
                fun(File, Acc) ->
                    SchemaName = list_to_binary(filename:basename(File, ".json")),
                    SchemaPath = filename:join(SchemaDir, File),

                    case read_and_convert_schema(SchemaPath) of
                        {ok, Schema} ->
                            Acc#{SchemaName => Schema};
                        {error, _Reason} ->
                            Acc
                    end
                end,
                #{},
                JsonFiles
            );
        {error, _Reason} ->
            #{}
    end.

-doc """
-------------------------------------------------------------------------------------------
Reads a JSON schema file and converts it to OpenAPI format.
-------------------------------------------------------------------------------------------
""".
-spec read_and_convert_schema(string()) -> {ok, map()} | {error, term()}.
read_and_convert_schema(SchemaPath) ->
    case file:read_file(SchemaPath) of
        {ok, JsonBin} ->
            try
                Schema = jsx:decode(JsonBin, [return_maps]),
                OpenAPISchema = convert_jesse_to_openapi(Schema),
                {ok, OpenAPISchema}
            catch
                _:Reason ->
                    {error, {decode_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

-doc """
-------------------------------------------------------------------------------------------
Converts Jesse JSON Schema (draft-04) to OpenAPI 3.x schema format.
Removes Jesse-specific fields and handles minor differences.
-------------------------------------------------------------------------------------------
""".
-spec convert_jesse_to_openapi(map()) -> map().
convert_jesse_to_openapi(JesseSchema) ->
    % Remove Jesse/JSON Schema specific fields not used in OpenAPI
    CleanSchema = maps:without([
        <<"id">>,
        <<"$schema">>,
        <<"$id">>
    ], JesseSchema),

    % Recursively process nested schemas
    maps:map(
        fun
            (<<"properties">>, Props) when is_map(Props) ->
                maps:map(
                    fun(_PropName, PropSchema) when is_map(PropSchema) ->
                        convert_jesse_to_openapi(PropSchema);
                       (_PropName, PropSchema) ->
                        PropSchema
                    end,
                    Props
                );
            (<<"items">>, Items) when is_map(Items) ->
                convert_jesse_to_openapi(Items);
            (<<"additionalProperties">>, AdditionalProps) when is_map(AdditionalProps) ->
                convert_jesse_to_openapi(AdditionalProps);
            (_Key, Value) ->
                Value
        end,
        CleanSchema
    ).

-doc """
-------------------------------------------------------------------------------------------
Formats an application name to a human-readable title.
Converts snake_case to Title Case.
-------------------------------------------------------------------------------------------
""".
-spec format_app_title(atom()) -> binary().
format_app_title(AppName) ->
    Words = string:split(atom_to_list(AppName), "_", all),
    Capitalized = [string:titlecase(W) || W <- Words],
    iolist_to_binary([string:join(Capitalized, " "), <<" API">>]).

-doc """
-------------------------------------------------------------------------------------------
Gets the version of an application.
Returns "1.0.0" if version cannot be determined.
-------------------------------------------------------------------------------------------
""".
-spec get_app_version(atom()) -> binary().
get_app_version(AppName) ->
    case application:get_key(AppName, vsn) of
        {ok, Version} -> list_to_binary(Version);
        undefined -> <<"1.0.0">>
    end.
