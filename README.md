# rebar3_openapi_generator

A Rebar3 plugin for automatically generating OpenAPI 3.x specifications from Erlang/Cowboy handler modules with `trails` metadata.

## Features

- 🔍 **Automatic Discovery** - Scans your Cowboy handlers and extracts route metadata
- 📝 **OpenAPI 3.x Generation** - Creates standards-compliant OpenAPI specifications
- 📊 **Coverage Reporting** - Identifies documented vs undocumented routes with actionable suggestions
- 🎯 **Multiple Formats** - Outputs YAML or JSON specifications
- ✅ **Validation** - Optional spec validation to ensure correctness
- 🔄 **Cowboy Swagger Compatible** - Supports both trails and cowboy_swagger metadata formats
- 📐 **Schema Discovery** - Automatically extracts Jesse/JSON schemas from your handlers

## Installation

Add the plugin to your `rebar.config`:

```erlang
{plugins, [
    {rebar3_openapi_generator, {git, "https://github.com/greyorange-labs/rebar3_openapi_generator.git", {branch, "main"}}}
]}.

{deps, [
    {jsx, "3.1.0"},
    {yamerl, "0.10.0"},
    {cowboy, "2.9.0"},
    {trails, "2.3.0"}
]}.
```

## Usage

### Basic Command

Generate an OpenAPI specification for your application:

```bash
# First, compile your application
rebar3 compile

# Then generate the OpenAPI spec
# This automatically discovers ALL handlers that export trails/0
rebar3 openapi generate --app my_app
```

> **Note:** The `--app` parameter specifies which application to scan. The plugin will automatically discover all modules in that application that export `trails/0`. You don't need to specify individual handlers.

### Command Options

| Option       | Short | Type    | Default           | Description                      |
| ------------ | ----- | ------- | ----------------- | -------------------------------- |
| `--app`      | `-a`  | atom    | *required*        | Target application name          |
| `--output`   | `-o`  | string  | `./docs/openapi/` | Output directory                 |
| `--format`   | `-f`  | string  | `yaml`            | Output format (`yaml` or `json`) |
| `--coverage` | `-c`  | boolean | `true`            | Generate coverage report         |
| `--validate` | `-v`  | boolean | `false`           | Validate generated spec          |

### Examples

```bash
# Generate YAML spec with coverage report
rebar3 openapi generate --app myapp

# Generate JSON spec without coverage report
rebar3 openapi generate --app myapp --format json --coverage false

# Generate with validation
rebar3 openapi generate --app myapp --validate true

# Custom output directory
rebar3 openapi generate --app myapp --output ./api-docs/
```

## How Handler Discovery Works

When you run `rebar3 openapi generate --app myapp`, the plugin automatically discovers handlers by:

1. **Locating the Application**: Finds `myapp` in your rebar3 project apps
2. **Scanning ebin Directory**: Lists all `.beam` files in `_build/default/lib/myapp/ebin/`
3. **Checking for trails/0 Export**: For each module, checks if it exports `trails/0` function
4. **Loading Handlers**: Collects all modules that export `trails/0`

**Example Discovery Process:**

```erlang
% Your project structure:
myapp/
  src/
    user_handler.erl      % exports trails/0 ✓ (discovered)
    admin_handler.erl     % exports trails/0 ✓ (discovered)
    payment_handler.erl   % exports trails/0 ✓ (discovered)
    user_service.erl      % no trails/0     ✗ (ignored)
    utils.erl             % no trails/0     ✗ (ignored)
```

After compilation, the plugin will:
1. Find all 3 handler modules (`user_handler`, `admin_handler`, `payment_handler`)
2. Call `trails/0` on each
3. Extract all routes and metadata
4. Generate a single unified OpenAPI specification

**Key Requirements:**
- ✅ Handlers must be compiled (run `rebar3 compile` first)
- ✅ Handlers must export `trails/0` function
- ✅ Handlers must use `-behaviour(trails_handler)` (recommended but not required)

**No Configuration Needed:** The plugin automatically finds ALL handlers in your app that export `trails/0`. You don't need to manually specify which handlers to scan.

## Handler Metadata Format

The plugin supports the **cowboy_swagger** metadata format where HTTP methods are direct keys in the trail metadata.

### Example Handler

```erlang
-module(my_handler).
-behaviour(trails_handler).
-export([trails/0]).

trails() ->
    % Health check endpoint
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
                                    <<"status">> => #{type => <<"string">>, enum => [<<"ok">>, <<"degraded">>]},
                                    <<"timestamp">> => #{type => <<"string">>, format => <<"date-time">>},
                                    <<"uptime">> => #{type => <<"integer">>, description => <<"Uptime in seconds">>}
                                }
                            }
                        }
                    }
                }
            }
        }
    },

    % Users collection endpoint
    UsersMetadata = #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get all users">>,
            description => <<"Retrieve a list of all users with optional filtering">>,
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
                },
                #{
                    name => <<"status">>,
                    in => <<"query">>,
                    description => <<"Filter by user status">>,
                    required => false,
                    schema => #{type => <<"string">>, enum => [<<"active">>, <<"inactive">>, <<"pending">>]}
                }
            ],
            responses => #{
                <<"200">> => #{
                    description => <<"Success">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"users">> => #{
                                        type => <<"array">>,
                                        items => #{
                                            type => <<"object">>,
                                            properties => #{
                                                <<"id">> => #{type => <<"integer">>},
                                                <<"name">> => #{type => <<"string">>},
                                                <<"email">> => #{type => <<"string">>, format => <<"email">>},
                                                <<"status">> => #{type => <<"string">>},
                                                <<"created_at">> => #{type => <<"string">>, format => <<"date-time">>}
                                            }
                                        }
                                    },
                                    <<"total">> => #{type => <<"integer">>},
                                    <<"page">> => #{type => <<"integer">>},
                                    <<"pages">> => #{type => <<"integer">>}
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
                            <<"name">> => #{type => <<"string">>, minLength => 2, maxLength => 100},
                            <<"email">> => #{type => <<"string">>, format => <<"email">>},
                            <<"password">> => #{type => <<"string">>, minLength => 8},
                            <<"role">> => #{type => <<"string">>, enum => [<<"user">>, <<"admin">>, <<"moderator">>]}
                        },
                        required => [<<"name">>, <<"email">>, <<"password">>]
                    }
                }
            ],
            responses => #{
                <<"201">> => #{
                    description => <<"User created successfully">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"id">> => #{type => <<"integer">>},
                                    <<"name">> => #{type => <<"string">>},
                                    <<"email">> => #{type => <<"string">>},
                                    <<"role">> => #{type => <<"string">>},
                                    <<"created_at">> => #{type => <<"string">>, format => <<"date-time">>}
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
                                    <<"error">> => #{type => <<"string">>},
                                    <<"details">> => #{type => <<"array">>, items => #{type => <<"string">>}}
                                }
                            }
                        }
                    }
                },
                <<"409">> => #{description => <<"User already exists">>}
            }
        }
    },

    % Individual user endpoint
    UserByIdMetadata = #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get user by ID">>,
            description => <<"Retrieve detailed information about a specific user">>,
            operationId => <<"getUserById">>,
            produces => [<<"application/json">>],
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
                <<"200">> => #{
                    description => <<"Success">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"id">> => #{type => <<"integer">>},
                                    <<"name">> => #{type => <<"string">>},
                                    <<"email">> => #{type => <<"string">>},
                                    <<"role">> => #{type => <<"string">>},
                                    <<"status">> => #{type => <<"string">>},
                                    <<"created_at">> => #{type => <<"string">>, format => <<"date-time">>},
                                    <<"updated_at">> => #{type => <<"string">>, format => <<"date-time">>}
                                }
                            }
                        }
                    }
                },
                <<"404">> => #{description => <<"User not found">>}
            }
        },
        put => #{
            tags => [<<"Users">>],
            summary => <<"Update user">>,
            description => <<"Update an existing user's information">>,
            operationId => <<"updateUser">>,
            consumes => [<<"application/json">>],
            produces => [<<"application/json">>],
            parameters => [
                #{
                    name => <<"id">>,
                    in => <<"path">>,
                    description => <<"User ID">>,
                    required => true,
                    schema => #{type => <<"integer">>}
                },
                #{
                    name => <<"user">>,
                    in => <<"body">>,
                    description => <<"Updated user data">>,
                    required => true,
                    schema => #{
                        type => <<"object">>,
                        properties => #{
                            <<"name">> => #{type => <<"string">>},
                            <<"email">> => #{type => <<"string">>, format => <<"email">>},
                            <<"role">> => #{type => <<"string">>},
                            <<"status">> => #{type => <<"string">>}
                        }
                    }
                }
            ],
            responses => #{
                <<"200">> => #{description => <<"User updated successfully">>},
                <<"400">> => #{description => <<"Invalid input">>},
                <<"404">> => #{description => <<"User not found">>}
            }
        },
        delete => #{
            tags => [<<"Users">>],
            summary => <<"Delete user">>,
            description => <<"Delete a user account">>,
            operationId => <<"deleteUser">>,
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
                <<"204">> => #{description => <<"User deleted successfully">>},
                <<"404">> => #{description => <<"User not found">>},
                <<"403">> => #{description => <<"Forbidden - cannot delete this user">>}
            }
        }
    },

    % User preferences endpoint
    UserPreferencesMetadata = #{
        get => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Get user preferences">>,
            operationId => <<"getUserPreferences">>,
            produces => [<<"application/json">>],
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
                <<"200">> => #{
                    description => <<"Success">>,
                    content => #{
                        <<"application/json">> => #{
                            schema => #{
                                type => <<"object">>,
                                properties => #{
                                    <<"theme">> => #{type => <<"string">>, enum => [<<"light">>, <<"dark">>, <<"auto">>]},
                                    <<"language">> => #{type => <<"string">>},
                                    <<"notifications">> => #{
                                        type => <<"object">>,
                                        properties => #{
                                            <<"email">> => #{type => <<"boolean">>},
                                            <<"push">> => #{type => <<"boolean">>},
                                            <<"sms">> => #{type => <<"boolean">>}
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                <<"404">> => #{description => <<"User not found">>}
            }
        },
        patch => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Update user preferences">>,
            operationId => <<"updateUserPreferences">>,
            consumes => [<<"application/json">>],
            produces => [<<"application/json">>],
            parameters => [
                #{
                    name => <<"id">>,
                    in => <<"path">>,
                    description => <<"User ID">>,
                    required => true,
                    schema => #{type => <<"integer">>}
                },
                #{
                    name => <<"preferences">>,
                    in => <<"body">>,
                    description => <<"Preference updates">>,
                    required => true,
                    schema => #{
                        type => <<"object">>,
                        properties => #{
                            <<"theme">> => #{type => <<"string">>},
                            <<"language">> => #{type => <<"string">>},
                            <<"notifications">> => #{type => <<"object">>}
                        }
                    }
                }
            ],
            responses => #{
                <<"200">> => #{description => <<"Preferences updated">>},
                <<"400">> => #{description => <<"Invalid input">>},
                <<"404">> => #{description => <<"User not found">>}
            }
        }
    },

    [
        trails:trail("/api/health", my_handler, [], HealthMetadata),
        trails:trail("/api/users", my_handler, [], UsersMetadata),
        trails:trail("/api/users/:id", my_handler, [], UserByIdMetadata),
        trails:trail("/api/users/:id/preferences", my_handler, [], UserPreferencesMetadata)
    ].
```

### Best Practice: Using Functions for Metadata

For handlers with many routes, it's recommended to extract metadata into separate functions or resource modules. This improves code organization and maintainability.

#### Option 1: Local Metadata Functions

```erlang
-module(my_handler).
-behaviour(trails_handler).
-export([trails/0]).

trails() ->
    [
        trails:trail("/api/health", my_handler, [], health_metadata()),
        trails:trail("/api/users", my_handler, [], users_metadata()),
        trails:trail("/api/users/:id", my_handler, [], user_by_id_metadata()),
        trails:trail("/api/users/:id/preferences", my_handler, [], user_preferences_metadata())
    ].

%% Metadata functions
health_metadata() ->
    #{
        get => #{
            tags => [<<"Health">>],
            summary => <<"Health check">>,
            responses => #{
                <<"200">> => #{description => <<"Service is healthy">>}
            }
        }
    }.

users_metadata() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get all users">>,
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        post => #{
            tags => [<<"Users">>],
            summary => <<"Create user">>,
            responses => #{<<"201">> => #{description => <<"User created">>}}
        }
    }.

user_by_id_metadata() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get user by ID">>,
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        put => #{
            tags => [<<"Users">>],
            summary => <<"Update user">>,
            responses => #{<<"200">> => #{description => <<"Updated">>}}
        },
        delete => #{
            tags => [<<"Users">>],
            summary => <<"Delete user">>,
            responses => #{<<"204">> => #{description => <<"Deleted">>}}
        }
    }.

user_preferences_metadata() ->
    #{
        get => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Get user preferences">>,
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        patch => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Update user preferences">>,
            responses => #{<<"200">> => #{description => <<"Updated">>}}
        }
    }.
```

#### Option 2: Resource Module Pattern (Recommended for Large APIs)

For applications with many routes, separate metadata into dedicated resource modules:

```erlang
% File: src/resources/user_api_metadata.erl
-module(user_api_metadata).
-export([
    health/0,
    users_collection/0,
    user_by_id/0,
    user_preferences/0
]).

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

users_collection() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get all users">>,
            operationId => <<"getAllUsers">>,
            parameters => [
                #{
                    name => <<"page">>,
                    in => <<"query">>,
                    schema => #{type => <<"integer">>, default => 1}
                }
            ],
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        post => #{
            tags => [<<"Users">>],
            summary => <<"Create user">>,
            operationId => <<"createUser">>,
            responses => #{<<"201">> => #{description => <<"Created">>}}
        }
    }.

user_by_id() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"Get user by ID">>,
            operationId => <<"getUserById">>,
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        put => #{
            tags => [<<"Users">>],
            summary => <<"Update user">>,
            operationId => <<"updateUser">>,
            responses => #{<<"200">> => #{description => <<"Updated">>}}
        },
        delete => #{
            tags => [<<"Users">>],
            summary => <<"Delete user">>,
            operationId => <<"deleteUser">>,
            responses => #{<<"204">> => #{description => <<"Deleted">>}}
        }
    }.

user_preferences() ->
    #{
        get => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Get user preferences">>,
            operationId => <<"getUserPreferences">>,
            responses => #{<<"200">> => #{description => <<"Success">>}}
        },
        patch => #{
            tags => [<<"Users">>, <<"Preferences">>],
            summary => <<"Update preferences">>,
            operationId => <<"updateUserPreferences">>,
            responses => #{<<"200">> => #{description => <<"Updated">>}}
        }
    }.
```

Then use in your handler:

```erlang
% File: src/handlers/my_handler.erl
-module(my_handler).
-behaviour(trails_handler).
-export([trails/0]).

trails() ->
    [
        trails:trail("/api/health", my_handler, [], user_api_metadata:health()),
        trails:trail("/api/users", my_handler, [], user_api_metadata:users_collection()),
        trails:trail("/api/users/:id", my_handler, [], user_api_metadata:user_by_id()),
        trails:trail("/api/users/:id/preferences", my_handler, [], user_api_metadata:user_preferences())
    ].
```

**Benefits of Resource Modules:**
- ✅ Clean separation of concerns
- ✅ Easier to maintain and update API documentation
- ✅ Reusable metadata across multiple handlers
- ✅ Better organization for large APIs with many endpoints
- ✅ Simpler testing of metadata definitions
- ✅ Improved code readability

> **✅ Verified:** The plugin fully supports function-based and resource module patterns. Metadata defined in separate functions or resource modules is correctly extracted and included in the generated OpenAPI specification. See `test_resource_pattern.erl` for a working example.

### Path Parameters

Use Cowboy's `:param` syntax in your paths. The plugin automatically converts them to OpenAPI's `{param}` format:

```erlang
% Handler path
trails:trail("/api/users/:id", my_handler, [], Metadata).

% Generated OpenAPI path
% /api/users/{id}
```

## Output Files

Running the plugin generates the following files in your output directory:

1. **`<app_name>.yaml`** (or `.json`) - The complete OpenAPI specification
2. **`<app_name>_coverage.txt`** - Human-readable coverage report
3. **`<app_name>_todo.md`** - Markdown file with actionable documentation suggestions

### Example Coverage Report

```
OpenAPI Coverage Report: myapp
======================================

Handler: my_handler
  Coverage: 66.7%

  ✓ Documented Routes:
    • GET /api/users
    • POST /api/users

  ✗ Undocumented Routes:
    • /api/users/:id

  💡 Suggestions:
    • Add OpenAPI metadata for /api/users/:id

Overall Coverage: 66.7%

Missing documentation for 1 route(s).
Consider adding OpenAPI metadata to improve API documentation.
```

## Project Structure

```
rebar3_openapi_generator/
├── src/
│   ├── rebar3_openapi.erl              # Main rebar3 provider
│   ├── openapi_parser.erl              # Metadata parser
│   ├── openapi_spec_builder.erl        # OpenAPI spec builder
│   ├── openapi_coverage.erl            # Coverage reporting
│   ├── openapi_validator.erl           # Spec validation
│   └── openapi_metadata_generator.erl  # Metadata generation utilities
├── test/
│   └── fixtures/
│       └── test_cowboy_swagger_handler.erl  # Example handler
├── rebar.config
└── README.md
```

## Architecture

The plugin follows a modular architecture with clear separation of concerns:

1. **rebar3_openapi** - CLI interface and workflow coordination
2. **openapi_parser** - Extracts and parses trails metadata from handlers
3. **openapi_spec_builder** - Builds OpenAPI 3.x compliant specifications
4. **openapi_coverage** - Analyzes documentation coverage and generates reports
5. **openapi_validator** - Validates generated specifications
6. **openapi_metadata_generator** - Utilities for schema generation

## Requirements

- Erlang/OTP 24 or higher
- Rebar3
- Dependencies:
  - `jsx` - JSON encoding/decoding
  - `yamerl` - YAML encoding/decoding
  - `cowboy` - For Cowboy types
  - `trails` - For trails metadata

## Development

### Building

```bash
rebar3 compile
```

### Testing

The project includes test fixtures demonstrating the cowboy_swagger format:

```bash
# Run the test script
./test_cowboy_swagger.erl
```

### Code Style

The project follows Erlang coding standards enforced by:
- `rebar3_lint` - Elvis linting
- `rebar3_format` - Code formatting

Run linting:
```bash
rebar3 lint
```

Run formatting:
```bash
rebar3 format
```

## How It Works

1. **Discovery Phase** (Automatic)
   - Scans the target application's `ebin` directory for all `.beam` files
   - Automatically identifies ALL modules exporting `trails/0` function
   - Loads and validates handler modules
   - **No manual configuration required** - finds all handlers automatically

2. **Parsing Phase**
   - Calls `trails/0` on each discovered handler module
   - Extracts trail definitions with metadata (supports inline and function-based patterns)
   - Normalizes path parameters (`:id` → `{id}`)
   - Detects HTTP methods from metadata keys (GET, POST, PUT, DELETE, PATCH, etc.)

3. **Building Phase**
   - Constructs OpenAPI 3.x structure
   - Assembles paths, operations, schemas
   - Discovers and includes Jesse/JSON schemas
   - Generates component definitions

4. **Reporting Phase**
   - Analyzes documented vs undocumented routes
   - Calculates coverage percentages
   - Generates actionable suggestions

5. **Output Phase**
   - Serializes to YAML or JSON
   - Writes specification file
   - Saves coverage and TODO reports
   - Optionally validates the spec

## OpenAPI Specification Features

The generated specifications include:

- **Info Object** - Application name, version, description
- **Servers** - Configurable server URLs
- **Paths** - All discovered routes with operations
- **Operations** - HTTP method handlers with full metadata
- **Parameters** - Path, query, header, and body parameters
- **Request Bodies** - Schema definitions for request payloads
- **Responses** - Status codes with response schemas
- **Components/Schemas** - Reusable schema definitions
- **Tags** - Grouped operations by tag

## Limitations

- Requires handlers to export `trails/0` function
- Only processes handlers with metadata in cowboy_swagger format
- Schema discovery requires Jesse-compatible schema definitions
- Validation requires properly formatted OpenAPI 3.x metadata

## Contributing

Contributions are welcome! Please ensure:

1. Code follows the project's style guide
2. All tests pass
3. Documentation is updated
4. Commit messages are descriptive

## License

Proprietary - GreyOrange Labs

## Support

For issues, questions, or contributions, please contact the GreyOrange Labs team or open an issue in the repository.

## Changelog

### Version 0.1.0
- Initial release
- OpenAPI 3.x specification generation
- Cowboy swagger metadata format support
- Coverage reporting with actionable suggestions
- YAML and JSON output formats
- Path parameter normalization
- Schema discovery and inclusion
- Optional validation
