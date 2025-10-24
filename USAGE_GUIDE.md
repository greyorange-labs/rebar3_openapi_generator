# Setup and Usage Guide for rebar3_openapi_generator

## Overview

This plugin generates OpenAPI 3.x specifications from existing Erlang/Cowboy HTTP handlers. It analyzes `trails()` functions and embedded metadata to produce complete API documentation.

## Installation

### For Butler Server

Add to `rebar.config`:

```erlang
{plugins, [
    {rebar3_openapi_generator, {git, "file:///Users/amar.c/workspace/erlang_libs/rebar3_openapi_generator", {branch, "main"}}}
]}.
```

Or for local development:

```erlang
{plugins, [
    {rebar3_openapi_generator, "~> 0.1.0"}
]}.

{plugin_dir, "/Users/amar.c/workspace/erlang_libs"}.
```

## Basic Usage

### 1. Generate OpenAPI Spec for an App

```bash
cd /Users/amar.c/workspace/gm_core/butler_server_develop
rebar3 openapi generate --app put
```

Output:
- `./docs/openapi/put.yaml` - OpenAPI specification
- `./docs/openapi/put_coverage.txt` - Coverage report
- `./docs/openapi/put_todo.md` - TODO list for missing documentation

### 2. Generate with Options

```bash
# JSON format
rebar3 openapi generate --app put --format json

# Custom output directory
rebar3 openapi generate --app put --output ./api-docs/

# Skip coverage report
rebar3 openapi generate --app put --coverage false

# With validation
rebar3 openapi generate --app put --validate true
```

## Metadata Format

### Inline Metadata in trails()

The preferred approach is to embed metadata directly in your `trails()` function:

```erlang
-module(put_http_handler).
-behaviour(trails_handler).

-export([trails/0, init/2, allowed_methods/2]).

trails() ->
    Metadata = #{
        paths => #{
            <<"/api/put/racks">> => #{
                post => #{
                    tags => [<<"Racks">>],
                    summary => <<"Create or update racks">>,
                    operationId => <<"postRacks">>,
                    requestBody => #{
                        required => true,
                        content => #{
                            <<"application/json">> => #{
                                schema => #{
                                    <<"$ref">> => <<"#/components/schemas/put_app_rack_post">>
                                }
                            }
                        }
                    },
                    responses => #{
                        <<"200">> => #{
                            description => <<"Successful operation">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        type => <<"object">>,
                                        properties => #{
                                            <<"message">> => #{type => <<"string">>}
                                        }
                                    }
                                }
                            }
                        },
                        <<"400">> => #{
                            description => <<"Invalid request">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Error">>
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
        trails:trail("/api/put/racks", put_http_handler, [], Metadata),
        trails:trail("/api/put/v1/mvts/transport_request_information", put_http_handler, [])
    ].
```

### Alternative: openapi_metadata/1 Function

For cleaner separation, you can use an `openapi_metadata/1` function:

```erlang
-export([openapi_metadata/1]).

trails() ->
    [
        trails:trail("/api/put/racks", put_http_handler, [], openapi_metadata(post_racks)),
        trails:trail("/api/put/health", put_http_handler, [], openapi_metadata(get_health))
    ].

openapi_metadata(post_racks) ->
    #{
        paths => #{
            <<"/api/put/racks">> => #{
                post => #{
                    tags => [<<"Racks">>],
                    summary => <<"Create or update racks">>,
                    %... full spec
                }
            }
        }
    };
openapi_metadata(get_health) ->
    #{
        paths => #{
            <<"/api/put/health">> => #{
                get => #{
                    tags => [<<"Health">>],
                    summary => <<"Health check endpoint">>,
                    responses => #{
                        <<"200">> => #{
                            description => <<"Service is healthy">>
                        }
                    }
                }
            }
        }
    };
openapi_metadata(_) ->
    #{}.
```

## Generating Metadata Stubs

For existing handlers without metadata, generate stubs:

```erlang
%% In Erlang shell
1> openapi_metadata_generator:generate_stubs([put_http_handler]).
2> file:write_file("put_handler_stubs.erl", v(-1)).
```

Then copy the generated functions to your handler module and fill in the details.

## Jesse Schema Integration

The plugin automatically discovers Jesse schemas from `apps/<app>/priv/schemas/*.json` and converts them to OpenAPI format.

### Example Jesse Schema

`apps/put/priv/schemas/put_app_rack_post.json`:

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "rack_id": {
      "type": "string",
      "description": "Unique rack identifier"
    },
    "capacity": {
      "type": "integer",
      "minimum": 1
    }
  },
  "required": ["rack_id", "capacity"]
}
```

This will be available in OpenAPI as `#/components/schemas/put_app_rack_post`.

## Coverage Reporting

The plugin generates a coverage report showing which endpoints are documented:

```
════════════════════════════════════════════════════════════════
✅ OpenAPI Coverage Report: put
════════════════════════════════════════════════════════════════

📊 Coverage: 60.0% (6/10 routes documented)

✅ put_http_handler (60.0% documented)
  • Missing: /api/put/v1/mvts/transport_request_information
  • Missing: /api/put/ppp_rack_status
  • Missing: /api/put/container-inventory/capacity
  • Missing: /api/put/health

  💡 Suggestions:
     - Add metadata for /api/put/v1/mvts/transport_request_information (GET)
     - Add metadata for /api/put/ppp_rack_status (GET)
     - Add metadata for /api/put/container-inventory/capacity (POST)

════════════════════════════════════════════════════════════════
```

## Workflow for Documenting Existing APIs

1. **Generate initial spec** to see current state:
   ```bash
   rebar3 openapi generate --app put
   ```

2. **Review coverage report**:
   ```bash
   cat docs/openapi/put_coverage.txt
   ```

3. **Generate metadata stubs** for undocumented routes

4. **Add metadata** to handler modules

5. **Regenerate spec** to verify:
   ```bash
   rebar3 openapi generate --app put --validate true
   ```

6. **View the spec** in Swagger UI (see below)

## Viewing Generated Specs

### Using Swagger UI (Docker)

```bash
docker run -p 8080:8080 \
  -e SWAGGER_JSON=/docs/put.yaml \
  -v $(pwd)/docs/openapi:/docs \
  swaggerapi/swagger-ui
```

Then open http://localhost:8080

### Using VS Code Extension

Install "OpenAPI (Swagger) Editor" extension and open the generated YAML file.

## OpenAPI Metadata Reference

### Full Example with All Features

```erlang
openapi_metadata(create_order) ->
    #{
        paths => #{
            <<"/api/orders">> => #{
                post => #{
                    %% Basic info
                    tags => [<<"Orders">>],
                    summary => <<"Create a new order">>,
                    description => <<"Creates a new order in the system with items">>,
                    operationId => <<"createOrder">>,

                    %% Request body
                    requestBody => #{
                        required => true,
                        description => <<"Order creation payload">>,
                        content => #{
                            <<"application/json">> => #{
                                schema => #{
                                    <<"$ref">> => <<"#/components/schemas/OrderCreate">>
                                }
                            }
                        }
                    },

                    %% Responses
                    responses => #{
                        <<"201">> => #{
                            description => <<"Order created successfully">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Order">>
                                    }
                                }
                            }
                        },
                        <<"400">> => #{
                            description => <<"Invalid request">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Error">>
                                    }
                                }
                            }
                        },
                        <<"500">> => #{
                            description => <<"Internal server error">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Error">>
                                    }
                                }
                            }
                        }
                    },

                    %% Optional: Security
                    security => [
                        #{<<"bearerAuth">> => []}
                    ]
                }
            },
            <<"/api/orders/{order_id}">> => #{
                get => #{
                    tags => [<<"Orders">>],
                    summary => <<"Get order by ID">>,
                    operationId => <<"getOrderById">>,

                    %% Path parameters
                    parameters => [
                        #{
                            name => <<"order_id">>,
                            in => <<"path">>,
                            required => true,
                            description => <<"Unique order identifier">>,
                            schema => #{type => <<"string">>}
                        }
                    ],

                    responses => #{
                        <<"200">> => #{
                            description => <<"Order found">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Order">>
                                    }
                                }
                            }
                        },
                        <<"404">> => #{
                            description => <<"Order not found">>,
                            content => #{
                                <<"application/json">> => #{
                                    schema => #{
                                        <<"$ref">> => <<"#/components/schemas/Error">>
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }.
```

## Troubleshooting

### Plugin not found

Ensure the plugin is in your `rebar.config` and run:
```bash
rebar3 plugins list
```

### Handler not discovered

Check that your handler:
1. Exports `trails/0` function
2. Is compiled (exists in `_build/default/lib/<app>/ebin/`)
3. Module name matches the `.beam` file

### Empty spec generated

Verify that your `trails()` function returns a list of trail definitions.

### Validation errors

Run with validation to see specific issues:
```bash
rebar3 openapi generate --app put --validate true
```

## Next Steps

1. Start with `put` app as a proof of concept
2. Document the most commonly used endpoints first
3. Use coverage reports to track progress
4. Gradually expand to other apps

## Support

For issues or questions, contact the Butler Server team.
