# rebar3_openapi_generator

A Rebar3 plugin to generate OpenAPI 3.x specifications from Erlang/Cowboy HTTP handler modules.

## Features

- **Automatic Route Discovery**: Scans `trails/0` functions in handler modules
- **Metadata-Driven**: Uses OpenAPI metadata embedded in `trails()` definitions
- **Coverage Reporting**: Shows which endpoints are documented vs. need manual work
- **Jesse Schema Integration**: Auto-discovers and converts Jesse schemas to OpenAPI format
- **Per-App Output**: Generates one OpenAPI spec per application
- **Validation**: Optional OpenAPI spec validation

## Installation

Add to your `rebar.config`:

```erlang
{plugins, [
    {rebar3_openapi_generator, {git, "https://github.com/greyorange/rebar3_openapi_generator.git", {branch, "main"}}}
]}.
```

## Usage

### Generate OpenAPI Spec

```bash
# Generate spec for a specific app
rebar3 openapi generate --app put --output ./docs/openapi/

# With coverage report
rebar3 openapi generate --app put --coverage true

# With validation
rebar3 openapi generate --app put --validate true

# JSON output instead of YAML
rebar3 openapi generate --app put --format json
```

### Handler Metadata Format

Add OpenAPI metadata to your `trails()` function:

```erlang
-module(my_http_handler).
-behaviour(trails_handler).

-export([trails/0, init/2, allowed_methods/2]).

trails() ->
    Metadata = #{
        paths => #{
            <<"/api/v1/users">> => #{
                get => #{
                    tags => [<<"Users">>],
                    summary => <<"Get all users">>,
                    operationId => <<"getAllUsers">>,
                    responses => #{
                        <<"200">> => #{
                            description => <<"Success">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        type => <<"array">>,
                                        items => #{
                                            <<"$ref">> => <<"#/components/schemas/User">>
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                post => #{
                    tags => [<<"Users">>],
                    summary => <<"Create a user">>,
                    operationId => <<"createUser">>,
                    requestBody => #{
                        required => true,
                        content => #{
                            <<"application/json">> => #{
                                schema => #{
                                    <<"$ref">> => <<"#/components/schemas/UserCreate">>
                                }
                            }
                        }
                    },
                    responses => #{
                        <<"201">> => #{
                            description => <<"Created">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/User">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    },
    [
        trails:trail("/api/v1/users", my_http_handler, [], Metadata),
        trails:trail("/api/v1/users/:id", my_http_handler, [])
    ].
```

## Development

### Build

```bash
cd /Users/amar.c/workspace/erlang_libs/rebar3_openapi_generator
rebar3 compile
```

### Test

```bash
rebar3 eunit
rebar3 ct
```

## Architecture

- **openapi_parser**: Extracts routes and metadata from handler modules
- **openapi_coverage**: Generates coverage reports
- **openapi_spec_builder**: Assembles OpenAPI documents
- **openapi_validator**: Validates generated specs
- **rebar3_openapi**: Rebar3 provider implementation

## License

Proprietary - GreyOrange
