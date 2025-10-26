-module(test_cowboy_swagger_handler).

-doc """
Test handler using cowboy_swagger metadata format to verify the plugin works correctly.
""".

-behaviour(trails_handler).

-export([trails/0, init/2, allowed_methods/2]).

%% Test handler implementation
trails() ->
    % cowboy_swagger format: HTTP methods as direct keys
    HealthMetadata = #{
        get => #{
            tags => [<<"Health">>],
            summary => <<"Health check">>,
            description => <<"Check service health status">>,
            operationId => <<"getHealth">>,
            produces => [<<"application/json">>],
            responses => #{
                <<"200">> => #{
                    description => <<"Service is healthy">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"status">> => #{type => <<"string">>},
                                    <<"timestamp">> => #{type => <<"string">>, format => <<"date-time">>}
                                }
                            }
                        }
                    }
                }
            }
        }
    },

    UsersMetadata = #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get all users">>,
            description => <<"Retrieve list of all users">>,
            operationId => <<"getAllUsers">>,
            produces => [<<"application/json">>],
            responses => #{
                <<"200">> => #{
                    description => <<"Success">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"array">>,
                                items => #{
                                    type => <<"object">>,
                                    properties => #{
                                        <<"id">> => #{type => <<"integer">>},
                                        <<"name">> => #{type => <<"string">>}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        },
        post => #{
            tags => [<<"Users">>],
            summary => <<"Create user">>,
            description => <<"Create a new user">>,
            operationId => <<"createUser">>,
            consumes => [<<"application/json">>],
            produces => [<<"application/json">>],
            parameters => [
                #{
                    name => <<"user">>,
                    in => <<"body">>,
                    description => <<"User data">>,
                    required => true,
                    schema => #{
                        type => <<"object">>,
                        properties => #{
                            <<"name">> => #{type => <<"string">>},
                            <<"email">> => #{type => <<"string">>, format => <<"email">>}
                        },
                        required => [<<"name">>, <<"email">>]
                    }
                }
            ],
            responses => #{
                <<"201">> => #{
                    description => <<"User created">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"id">> => #{type => <<"integer">>},
                                    <<"name">> => #{type => <<"string">>},
                                    <<"email">> => #{type => <<"string">>}
                                }
                            }
                        }
                    }
                },
                <<"400">> => #{
                    description => <<"Invalid input">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"error">> => #{type => <<"string">>}
                                }
                            }
                        }
                    }
                }
            }
        }
    },

    [
        trails:trail("/api/health", test_cowboy_swagger_handler, [], HealthMetadata),
        trails:trail("/api/users", test_cowboy_swagger_handler, [], UsersMetadata),
        % Undocumented route
        trails:trail("/api/users/:id", test_cowboy_swagger_handler, [])
    ].

%% Minimal cowboy handler implementation
init(Req, State) ->
    {ok, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.
