-module(test_resource_metadata).

-doc """
Example resource module demonstrating metadata organization pattern.
This module contains OpenAPI metadata definitions separated from handler logic.
""".

-export([
    health/0,
    users_collection/0,
    user_by_id/0
]).

%% Health check metadata
health() ->
    #{
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
    }.

%% Users collection metadata
users_collection() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get all users">>,
            description => <<"Retrieve paginated list of users">>,
            operationId => <<"getAllUsers">>,
            produces => [<<"application/json">>],
            parameters => [
                #{
                    name => <<"page">>,
                    in => <<"query">>,
                    description => <<"Page number">>,
                    required => false,
                    schema => #{type => <<"integer">>, default => 1, minimum => 1}
                },
                #{
                    name => <<"limit">>,
                    in => <<"query">>,
                    description => <<"Items per page">>,
                    required => false,
                    schema => #{type => <<"integer">>, default => 10, minimum => 1, maximum => 100}
                }
            ],
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
                                        <<"name">> => #{type => <<"string">>},
                                        <<"email">> => #{type => <<"string">>, format => <<"email">>}
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
            description => <<"Create a new user account">>,
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
                <<"201">> => #{description => <<"User created successfully">>},
                <<"400">> => #{description => <<"Invalid input">>}
            }
        }
    }.

%% User by ID metadata
user_by_id() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get user by ID">>,
            operationId => <<"getUserById">>,
            parameters => [
                #{
                    name => <<"id">>,
                    in => <<"path">>,
                    description => <<"User ID">>,
                    required => true,
                    schema => #{type => <<"integer">>}
                }
            ],
            responses => #{
                <<"200">> => #{description => <<"Success">>},
                <<"404">> => #{description => <<"User not found">>}
            }
        },
        put => #{
            tags => [<<"Users">>],
            summary => <<"Update user">>,
            operationId => <<"updateUser">>,
            responses => #{
                <<"200">> => #{description => <<"Updated">>},
                <<"404">> => #{description => <<"Not found">>}
            }
        },
        delete => #{
            tags => [<<"Users">>],
            summary => <<"Delete user">>,
            operationId => <<"deleteUser">>,
            responses => #{
                <<"204">> => #{description => <<"Deleted">>},
                <<"404">> => #{description => <<"Not found">>}
            }
        }
    }.
