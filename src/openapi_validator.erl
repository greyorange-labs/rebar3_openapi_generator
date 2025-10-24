-module(openapi_validator).

-doc """
-------------------------------------------------------------------------------------------
Validates generated OpenAPI specifications.
Provides basic validation and integration with external OpenAPI validators.
-------------------------------------------------------------------------------------------
""".

-export([
    validate_spec/1,
    validate_spec_file/1,
    basic_validation/1
]).

-spec validate_spec(map()) -> ok | {error, [binary()]}.
validate_spec(Spec) ->
    basic_validation(Spec).

-spec validate_spec_file(string()) -> ok | {error, term()}.
validate_spec_file(FilePath) ->
    case filename:extension(FilePath) of
        ".json" ->
            validate_json_file(FilePath);
        ".yaml" ->
            validate_yaml_file(FilePath);
        ".yml" ->
            validate_yaml_file(FilePath);
        _ ->
            {error, unsupported_format}
    end.

-spec basic_validation(map()) -> ok | {error, [binary()]}.
basic_validation(Spec) ->
    Checks = [
        fun check_openapi_version/1,
        fun check_info_section/1,
        fun check_paths_section/1
    ],

    Errors = lists:filtermap(
        fun(CheckFun) ->
            case CheckFun(Spec) of
                ok -> false;
                {error, Msg} -> {true, Msg}
            end
        end,
        Checks
    ),

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if openapi version field is present and valid.
-------------------------------------------------------------------------------------------
""".
-spec check_openapi_version(map()) -> ok | {error, binary()}.
check_openapi_version(Spec) ->
    case maps:get(<<"openapi">>, Spec, undefined) of
        undefined ->
            {error, <<"Missing required field: openapi">>};
        Version when is_binary(Version) ->
            case binary:match(Version, <<"3.0">>) of
                {0, _} -> ok;
                _ -> {error, <<"OpenAPI version must be 3.0.x">>}
            end;
        _ ->
            {error, <<"openapi field must be a string">>}
    end.

-doc """
-------------------------------------------------------------------------------------------
Checks if info section is present and valid.
-------------------------------------------------------------------------------------------
""".
-spec check_info_section(map()) -> ok | {error, binary()}.
check_info_section(Spec) ->
    case maps:get(<<"info">>, Spec, undefined) of
        undefined ->
            {error, <<"Missing required section: info">>};
        Info when is_map(Info) ->
            RequiredFields = [<<"title">>, <<"version">>],
            MissingFields = lists:filter(
                fun(Field) ->
                    not maps:is_key(Field, Info)
                end,
                RequiredFields
            ),
            case MissingFields of
                [] -> ok;
                _ ->
                    {error, <<"Missing required info fields: title or version">>}
            end;
        _ ->
            {error, <<"info section must be an object">>}
    end.

-spec check_paths_section(map()) -> ok | {error, binary()}.
check_paths_section(Spec) ->
    case maps:get(<<"paths">>, Spec, undefined) of
        undefined ->
            {error, <<"Missing required section: paths">>};
        Paths when is_map(Paths) ->
            case map_size(Paths) of
                0 -> {error, <<"paths section is empty - no routes defined">>};
                _ -> ok
            end;
        _ ->
            {error, <<"paths section must be an object">>}
    end.

-spec validate_json_file(string()) -> ok | {error, term()}.
validate_json_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, JsonBin} ->
            try
                Spec = jsx:decode(JsonBin, [return_maps]),
                validate_spec(Spec)
            catch
                _:Reason ->
                    {error, {invalid_json, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

-spec validate_yaml_file(string()) -> ok | {error, term()}.
validate_yaml_file(_FilePath) ->
    % Simplified - YAML validation would require yamerl library
    {error, yaml_validation_not_implemented}.
