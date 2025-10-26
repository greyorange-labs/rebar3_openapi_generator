-module(yaml_encoder).

-doc """
Simple YAML encoder for OpenAPI specs.
Converts Erlang maps to YAML format.
""".

-export([encode/1]).

-spec encode(map()) -> iolist().
encode(Data) when is_map(Data) ->
    encode_map(Data, 0).

encode_map(Map, Indent) when is_map(Map) ->
    Keys = lists:sort(maps:keys(Map)),
    lists:map(
        fun(Key) ->
            Value = maps:get(Key, Map),
            KeyStr = to_string(Key),
            [
                indent(Indent),
                KeyStr,
                ":",
                encode_value(Value, Indent)
            ]
        end,
        Keys
    ).

encode_value(Value, Indent) when is_map(Value) ->
    case maps:size(Value) of
        0 ->
            " {}\n";
        _ ->
            ["\n", encode_map(Value, Indent + 1)]
    end;
encode_value(Value, Indent) when is_list(Value) ->
    case is_string(Value) of
        true ->
            [" \"", escape_string(Value), "\"\n"];
        false ->
            case Value of
                [] ->
                    " []\n";
                _ ->
                    [
                        "\n",
                        lists:map(
                            fun(Item) ->
                                [
                                    indent(Indent + 1),
                                    "- ",
                                    encode_list_item(Item, Indent + 1)
                                ]
                            end,
                            Value
                        )
                    ]
            end
    end;
encode_value(Value, _Indent) when is_binary(Value) ->
    [" \"", escape_string(binary_to_list(Value)), "\"\n"];
encode_value(Value, _Indent) when is_atom(Value) ->
    [" ", atom_to_list(Value), "\n"];
encode_value(Value, _Indent) when is_integer(Value) ->
    [" ", integer_to_list(Value), "\n"];
encode_value(Value, _Indent) when is_float(Value) ->
    [" ", float_to_list(Value), "\n"];
encode_value(Value, _Indent) ->
    [" ", io_lib:format("~p", [Value]), "\n"].

encode_list_item(Item, Indent) when is_map(Item) ->
    case maps:size(Item) of
        0 ->
            "{}\n";
        _ ->
            % For maps in lists, encode inline if small or multiline if complex
            Keys = maps:keys(Item),
            case length(Keys) of
                1 ->
                    [Key] = Keys,
                    Value = maps:get(Key, Item),
                    [to_string(Key), ": ", inline_value(Value), "\n"];
                _ ->
                    ["\n", encode_map(Item, Indent + 1)]
            end
    end;
encode_list_item(Item, _Indent) when is_binary(Item) ->
    ["\"", escape_string(binary_to_list(Item)), "\"\n"];
encode_list_item(Item, _Indent) when is_list(Item) ->
    case is_string(Item) of
        true -> ["\"", escape_string(Item), "\"\n"];
        false -> [io_lib:format("~p", [Item]), "\n"]
    end;
encode_list_item(Item, _Indent) ->
    [io_lib:format("~p", [Item]), "\n"].

inline_value(Value) when is_binary(Value) ->
    ["\"", escape_string(binary_to_list(Value)), "\""];
inline_value(Value) when is_list(Value) ->
    case is_string(Value) of
        true -> ["\"", escape_string(Value), "\""];
        false -> io_lib:format("~p", [Value])
    end;
inline_value(Value) when is_atom(Value) ->
    atom_to_list(Value);
inline_value(Value) when is_integer(Value) ->
    integer_to_list(Value);
inline_value(Value) ->
    io_lib:format("~p", [Value]).

indent(Level) ->
    lists:duplicate(Level * 2, $\s).

to_string(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_string(List) when is_list(List) ->
    List.

is_string([]) ->
    true;
is_string([H | T]) when is_integer(H), H >= 0, H =< 1114111 ->
    is_string(T);
is_string(_) ->
    false.

escape_string(Str) ->
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
