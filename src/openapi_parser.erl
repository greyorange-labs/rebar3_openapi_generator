-module(openapi_parser).

-doc """
-------------------------------------------------------------------------------------------
Core parser module for extracting OpenAPI information from Cowboy handler modules.
Analyzes trails/0 functions and metadata to build route specifications.

This module supports:
- Simple tuple format: {Path, Handler, Opts}
- trails:trail format with metadata
- Path parameter extraction (:id, :user_id, etc.)
- Method discovery from metadata
- Coverage analysis
-------------------------------------------------------------------------------------------
""".

-export([
    parse_handler/1,
    extract_routes/1,
    extract_metadata/1,
    get_coverage_info/1,
    normalize_path/1
]).

-type handler_info() :: #{
    module := module(),
    routes := [route_info()],
    metadata := map(),
    coverage := coverage_info()
}.

-type route_info() :: #{
    path := binary(),
    methods := [binary()],
    has_metadata := boolean(),
    handler_function := atom() | undefined,
    raw_path := term()
}.

-type coverage_info() :: #{
    total_routes := integer(),
    documented_routes := integer(),
    coverage_percent := float(),
    undocumented_paths := [binary()]
}.

-export_type([handler_info/0, route_info/0, coverage_info/0]).

-doc """
-------------------------------------------------------------------------------------------
Parses a handler module and extracts all route information including trails and metadata.
Returns structured handler information or error if module doesn't export trails/0.

## Parameters
- `Module` - Handler module atom (e.g., put_http_handler)

## Returns
- `{ok, handler_info()}` - Parsed handler information with routes and metadata
- `{error, term()}` - Error if parsing fails

## Example
```erlang
{ok, Info} = openapi_parser:parse_handler(put_http_handler),
Routes = maps:get(routes, Info),
Coverage = maps:get(coverage, Info).
```
-------------------------------------------------------------------------------------------
""".
-spec parse_handler(module()) -> {ok, handler_info()} | {error, term()}.
parse_handler(Module) ->
    case erlang:function_exported(Module, trails, 0) of
        false ->
            {error, {no_trails_export, Module}};
        true ->
            try
                Trails = Module:trails(),
                Routes = extract_routes(Trails),
                Metadata = extract_metadata(Module, Trails),
                Coverage = calculate_coverage(Routes, Metadata),

                {ok, #{
                    module => Module,
                    routes => Routes,
                    metadata => Metadata,
                    coverage => Coverage
                }}
            catch
                Error:Reason:Stack ->
                    io:format("Failed to parse handler ~p: ~p:~p~n~p~n", [
                        Module, Error, Reason, Stack
                    ]),
                    {error, {parse_failed, Module, Error, Reason}}
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Extracts route information from trails list.
Handles both simple tuple format and trails:trail/4 format.
-------------------------------------------------------------------------------------------
""".
-spec extract_routes(list()) -> [route_info()].
extract_routes(Trails) ->
    lists:map(fun parse_single_trail/1, Trails).

-doc """
-------------------------------------------------------------------------------------------
Parses a single trail entry to extract path, methods, and metadata presence.
Supports multiple trail formats from trails library.
-------------------------------------------------------------------------------------------
""".
-spec parse_single_trail(tuple()) -> route_info().
parse_single_trail(Trail) when is_tuple(Trail) ->
    case Trail of
        {PathSpec, Handler, Opts} ->
            % Simple format: {Path, Handler, Options}
            #{
                path => normalize_path(PathSpec),
                raw_path => PathSpec,
                methods => [<<"GET">>],  % Default method
                handler => Handler,
                has_metadata => false,
                metadata => #{}
            };

        _ ->
            % trails:trail format - extract using trails functions
            PathSpec = trails:path_match(Trail),
            Handler = trails:handler(Trail),
            TrailMetadata = trails:metadata(Trail),

            Methods = extract_methods_from_trail(TrailMetadata),

            #{
                path => normalize_path(PathSpec),
                raw_path => PathSpec,
                methods => Methods,
                handler => Handler,
                has_metadata => has_openapi_metadata(TrailMetadata),
                metadata => TrailMetadata
            }
    end.

-doc """
-------------------------------------------------------------------------------------------
Extracts HTTP methods from trail metadata.
Checks both allowed_methods in metadata and OpenAPI paths definitions.
-------------------------------------------------------------------------------------------
""".
-spec extract_methods_from_trail(map()) -> [binary()].
extract_methods_from_trail(Metadata) ->
    case maps:get(paths, Metadata, #{}) of
        PathsMap when map_size(PathsMap) > 0 ->
            % Get methods from first path entry in OpenAPI metadata
            [FirstPath | _] = maps:keys(PathsMap),
            PathSpec = maps:get(FirstPath, PathsMap),
            Methods = maps:keys(PathSpec),
            [ensure_binary(M) || M <- Methods];
        _ ->
            % Check for allowed_methods in metadata
            case maps:get(allowed_methods, Metadata, undefined) of
                undefined -> [<<"GET">>];  % Default
                MethodsList -> [ensure_binary(M) || M <- MethodsList]
            end
    end.

-doc """
-------------------------------------------------------------------------------------------
Extracts metadata from handler module and trails.
Combines inline trail metadata with any openapi_metadata/1 function if it exists.
-------------------------------------------------------------------------------------------
""".
-spec extract_metadata(module(), list()) -> map().
extract_metadata(Module, Trails) ->
    % First, collect all inline metadata from trails
    InlineMetadata = lists:foldl(
        fun(Trail, Acc) ->
            case Trail of
                {_, _, _} ->
                    Acc;  % Simple format, no metadata
                _ ->
                    TrailMeta = trails:metadata(Trail),
                    case maps:get(paths, TrailMeta, #{}) of
                        PathsMap when map_size(PathsMap) > 0 ->
                            ExistingPaths = maps:get(paths, Acc, #{}),
                            Acc#{paths => maps:merge(ExistingPaths, PathsMap)};
                        _ ->
                            Acc
                    end
            end
        end,
        #{},
        Trails
    ),

    % Check if module has openapi_metadata/1 function
    case erlang:function_exported(Module, openapi_metadata, 1) of
        true ->
            try
                % Try to get metadata for common operations
                FunctionMetadata = get_all_metadata_from_function(Module),
                merge_metadata_maps([InlineMetadata, FunctionMetadata])
            catch
                _:_ ->
                    InlineMetadata
            end;
        false ->
            InlineMetadata
    end.

-doc """
-------------------------------------------------------------------------------------------
Attempts to extract all metadata from openapi_metadata/1 function.
Tries common operation names and AST analysis if available.
-------------------------------------------------------------------------------------------
""".
-spec get_all_metadata_from_function(module()) -> map().
get_all_metadata_from_function(Module) ->
    % Try common operation identifiers
    CommonOps = [get, post, put, patch, delete, get_all, create, update],

    AllMetadata = lists:filtermap(
        fun(Op) ->
            try
                Meta = Module:openapi_metadata(Op),
                case is_map(Meta) andalso map_size(Meta) > 0 of
                    true -> {true, Meta};
                    false -> false
                end
            catch
                _:_ -> false
            end
        end,
        CommonOps
    ),

    merge_metadata_maps(AllMetadata).

-doc """
-------------------------------------------------------------------------------------------
Normalizes Cowboy path format to OpenAPI format.
Converts atoms to path parameters and handles various path representations.

## Examples
```erlang
normalize_path(["/api/v1/", ':id', "/status"])
    -> <<"/api/v1/{id}/status">>

normalize_path(<<"/api/users">>)
    -> <<"/api/users">>

normalize_path("/api/pps/:pps_id/transactions")
    -> <<"/api/pps/{pps_id}/transactions">>
```
-------------------------------------------------------------------------------------------
""".
-spec normalize_path(term()) -> binary().
normalize_path(PathParts) when is_list(PathParts) ->
    % Handle list of path segments
    case is_string(PathParts) of
        true ->
            % It's a string like "/api/users/:id"
            PathBin = list_to_binary(PathParts),
            normalize_path_string(PathBin);
        false ->
            % It's a list of segments like ["/api/", :id, "/status"]
            Parts = lists:map(fun normalize_path_part/1, PathParts),
            CleanParts = lists:filter(fun(P) -> P =/= <<>> end, Parts),
            JoinedPath = iolist_to_binary(CleanParts),

            % Ensure path starts with /
            case JoinedPath of
                <<"/", _/binary>> -> JoinedPath;
                _ -> <<"/", JoinedPath/binary>>
            end
    end;
normalize_path(Path) when is_binary(Path) ->
    normalize_path_string(Path);
normalize_path(Path) when is_atom(Path) ->
    PathStr = atom_to_list(Path),
    case PathStr of
        [$: | Rest] ->
            % Atom starting with : is a path parameter
            <<"{", (list_to_binary(Rest))/binary, "}">>;
        _ ->
            list_to_binary(PathStr)
    end.

-doc """
-------------------------------------------------------------------------------------------
Normalizes a path string, converting :param to {param}.
-------------------------------------------------------------------------------------------
""".
-spec normalize_path_string(binary()) -> binary().
normalize_path_string(Path) ->
    % Replace :param with {param} using regex-like approach
    Parts = binary:split(Path, <<"/">>, [global]),
    NormalizedParts = lists:map(
        fun(Part) ->
            case Part of
                <<$:, Rest/binary>> -> <<"{", Rest/binary, "}">>;
                _ -> Part
            end
        end,
        Parts
    ),
    iolist_to_binary(lists:join(<<"/">>, NormalizedParts)).

-doc """
-------------------------------------------------------------------------------------------
Normalizes a single path part to binary format.
Handles atoms (including path parameters), binaries, and strings.
-------------------------------------------------------------------------------------------
""".
-spec normalize_path_part(term()) -> binary().
normalize_path_part(Part) when is_atom(Part) ->
    case atom_to_list(Part) of
        [$: | Rest] -> <<"{", (list_to_binary(Rest))/binary, "}">>;
        "..." -> <<>>;  % Wildcard, replace with catch-all
        "[...]" -> <<>>;  % Wildcard pattern
        AtomStr -> list_to_binary(AtomStr)
    end;
normalize_path_part(Part) when is_binary(Part) ->
    Part;
normalize_path_part(Part) when is_list(Part) ->
    list_to_binary(Part).

-doc """
-------------------------------------------------------------------------------------------
Calculates documentation coverage for parsed routes.
Returns coverage statistics including percentage and list of undocumented paths.
-------------------------------------------------------------------------------------------
""".
-spec calculate_coverage([route_info()], map()) -> coverage_info().
calculate_coverage(Routes, Metadata) ->
    Total = length(Routes),
    {Documented, Undocumented} = lists:partition(
        fun(Route) ->
            Path = maps:get(path, Route),
            has_metadata_for_path(Path, Metadata)
        end,
        Routes
    ),

    DocumentedCount = length(Documented),
    UndocumentedPaths = [maps:get(path, R) || R <- Undocumented],

    #{
        total_routes => Total,
        documented_routes => DocumentedCount,
        undocumented_paths => UndocumentedPaths,
        coverage_percent => case Total of
            0 -> 0.0;
            _ -> (DocumentedCount / Total) * 100
        end
    }.

-doc """
-------------------------------------------------------------------------------------------
Checks if a path has OpenAPI metadata defined.
-------------------------------------------------------------------------------------------
""".
-spec has_metadata_for_path(binary(), map()) -> boolean().
has_metadata_for_path(Path, Metadata) ->
    case maps:get(paths, Metadata, #{}) of
        PathsMap when is_map(PathsMap) ->
            maps:is_key(Path, PathsMap);
        _ ->
            false
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if trail metadata contains OpenAPI paths information.
-------------------------------------------------------------------------------------------
""".
-spec has_openapi_metadata(map()) -> boolean().
has_openapi_metadata(Metadata) ->
    maps:is_key(paths, Metadata) andalso
    map_size(maps:get(paths, Metadata)) > 0.

-doc """
-------------------------------------------------------------------------------------------
Merges multiple metadata maps, with later maps taking precedence.
Properly handles nested path merging.
-------------------------------------------------------------------------------------------
""".
-spec merge_metadata_maps([map()]) -> map().
merge_metadata_maps(MetadataList) ->
    lists:foldl(
        fun(Meta, Acc) ->
            maps:merge_with(
                fun
                    (paths, V1, V2) when is_map(V1), is_map(V2) ->
                        % Merge paths maps
                        maps:merge(V1, V2);
                    (_K, _V1, V2) ->
                        % Later value wins
                        V2
                end,
                Acc,
                Meta
            )
        end,
        #{},
        MetadataList
    ).

-doc """
-------------------------------------------------------------------------------------------
Gets coverage information for a parsed handler.
Convenience function that extracts coverage from handler_info.
-------------------------------------------------------------------------------------------
""".
-spec get_coverage_info(handler_info()) -> coverage_info().
get_coverage_info(HandlerInfo) ->
    maps:get(coverage, HandlerInfo).

-doc """
-------------------------------------------------------------------------------------------
Ensures a value is converted to binary.
Handles atoms, strings, and binaries.
-------------------------------------------------------------------------------------------
""".
-spec ensure_binary(term()) -> binary().
ensure_binary(Val) when is_binary(Val) -> Val;
ensure_binary(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
ensure_binary(Val) when is_list(Val) -> list_to_binary(Val).

-doc """
-------------------------------------------------------------------------------------------
Checks if a list is a string (list of integers in valid character range).
-------------------------------------------------------------------------------------------
""".
-spec is_string(list()) -> boolean().
is_string([]) -> true;
is_string([H | T]) when is_integer(H), H >= 0, H =< 16#10FFFF ->
    is_string(T);
is_string(_) -> false.
