# rebar3_openapi_generator

A Rebar3 plugin for bi-directional OpenAPI 3.x integration with Erlang/Cowboy applications.

## Features

### Generation (Erlang → OpenAPI)
- 🔍 **Automatic Discovery** - Scans your Cowboy handlers and extracts route metadata
- 📝 **OpenAPI 3.x Generation** - Creates standards-compliant OpenAPI specifications
- 📊 **Coverage Reporting** - Identifies documented vs undocumented routes with actionable suggestions
- 🎯 **Multiple Formats** - Outputs YAML or JSON specifications
- ✅ **Validation** - Optional spec validation to ensure correctness
- 🔄 **Cowboy Swagger Compatible** - Supports both trails and cowboy_swagger metadata formats
- 📐 **Schema Discovery** - Automatically extracts Jesse/JSON schemas from your handlers

### Import (OpenAPI → Erlang)
- 📥 **OpenAPI Import** - Generate Erlang trails definitions from OpenAPI YAML specs
- 🏗️ **Code Generation** - Automatically creates handler and metadata modules
- 🎨 **Metadata Functions** - Generates properly formatted cowboy_swagger metadata
- 🔗 **Schema Translation** - Converts OpenAPI schemas to Erlang maps
- 📋 **Complete Parameters** - Generates parameters, request bodies, and responses

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

### Generate: Erlang → OpenAPI

Generate an OpenAPI specification from your Erlang handlers:

```bash
# First, compile your application
rebar3 compile

# Then generate the OpenAPI spec with destination path
# This automatically discovers ALL handlers that export trails/0
rebar3 openapi generate --app my_app --destination ./docs/api.yml
```

> **Note:** The `--app` parameter specifies which application to scan. The plugin will automatically discover all modules in that application that export `trails/0`. You don't need to specify individual handlers.

#### Generate Command Options

| Option          | Short | Type    | Default    | Description                               |
| --------------- | ----- | ------- | ---------- | ----------------------------------------- |
| `--app`         | `-a`  | atom    | *required* | Target application name                   |
| `--destination` | `-d`  | string  | *required* | Output file path (e.g., `./docs/api.yml`) |
| `--coverage`    | `-c`  | boolean | `true`     | Generate coverage report                  |
| `--validate`    | `-v`  | boolean | `false`    | Validate generated spec                   |

> **Note:** The output format (YAML or JSON) is automatically determined by the file extension in `--destination` (`.yml`, `.yaml` for YAML, `.json` for JSON).

#### Generate Examples

```bash
# Generate YAML spec with coverage report
rebar3 openapi generate --app myapp --destination ./docs/myapp.yml

# Generate JSON spec
rebar3 openapi generate --app myapp --destination ./docs/myapp.json

# Generate without coverage report
rebar3 openapi generate --app myapp --destination ./api.yml --coverage false

# Generate with validation
rebar3 openapi generate --app myapp --destination ./api.yml --validate true

# Use absolute path
rebar3 openapi generate --app myapp --destination /absolute/path/to/api.yaml
```

### Import: OpenAPI → Erlang

Generate Erlang trails definitions from an existing OpenAPI specification:

```bash
# Import OpenAPI spec and generate Erlang modules
rebar3 openapi import \
    --spec api-spec.yaml \
    --handler-path ./src/my_handler.erl \
    --metadata-path ./src/my_metadata.erl
```

> **Note:** Module names are automatically extracted from the file paths. For example, `./src/my_handler.erl` will create a module named `my_handler`.

#### Import Command Options

| Option            | Short | Type    | Default          | Description                                                     |
| ----------------- | ----- | ------- | ---------------- | --------------------------------------------------------------- |
| `--spec`          | `-s`  | string  | *required*       | Path to OpenAPI YAML specification file                         |
| `--handler-path`  | `-h`  | string  | *required*       | Output path for handler module (e.g., `./src/my_handler.erl`)   |
| `--metadata-path` | `-m`  | string  | *required*       | Output path for metadata module (e.g., `./src/my_metadata.erl`) |
| `--handler-name`  |       | string  | `handler_module` | Handler to use in trails (e.g., `my_http_handler`)              |
| `--overwrite`     | `-w`  | boolean | `false`          | Overwrite existing files                                        |
| `--format`        | `-f`  | boolean | `true`           | Format generated code                                           |

#### Import Examples

```bash
# Basic import with explicit paths
rebar3 openapi import \
    --spec openapi.yaml \
    --handler-path ./src/api_handler.erl \
    --metadata-path ./src/api_metadata.erl

# Full customization with custom handler name
rebar3 openapi import \
    --spec ./docs/api-spec.yaml \
    --handler-path ./src/user_api_handler.erl \
    --metadata-path ./src/user_api_metadata.erl \
    --handler-name user_http_handler \
    --overwrite

# Import with absolute paths
rebar3 openapi import \
    --spec /absolute/path/to/api.yaml \
    --handler-path /absolute/path/to/handler.erl \
    --metadata-path /absolute/path/to/metadata.erl

# Import and skip formatting
rebar3 openapi import \
    --spec api.yaml \
    --handler-path ./src/handler.erl \
    --metadata-path ./src/metadata.erl \
    --format false
```

#### What Gets Generated

When you run `rebar3 openapi import`, it creates two files at the specified paths:

1. **Metadata Module** (at `--metadata-path`):
   - Contains metadata functions for each API endpoint
   - One function per operation (e.g., `get_users/0`, `post_user/0`)
   - Includes full OpenAPI metadata: tags, parameters, schemas, responses
   - Module name is automatically derived from the filename
   - Follows cowboy_swagger format

2. **Trails Handler Module** (at `--handler-path`):
   - Implements `trails_handler` behaviour
   - Exports `trails/0` function
   - Calls metadata functions from the metadata module
   - Ready to integrate into your Cowboy routing

**Example Generated Files:**

```erlang
% generated_metadata.erl
-module(generated_metadata).

-export([
    get_v1_users/0,
    post_v1_users/0,
    get_v1_users_id/0
]).

get_v1_users() ->
    #{
        get => #{
            tags => [<<"Users">>],
            summary => <<"List all users">>,
            parameters => [...],
            responses => #{...}
        }
    }.

% More functions...
```

```erlang
% generated_handler.erl
-module(generated_handler).
-behaviour(trails_handler).

-export([trails/0]).

trails() ->
    [
        trails:trail("/v1/users", my_http_handler, [], generated_metadata:get_v1_users()),
        trails:trail("/v1/users", my_http_handler, [], generated_metadata:post_v1_users()),
        trails:trail("/v1/users/:id", my_http_handler, [], generated_metadata:get_v1_users_id())
    ].
```

#### Post-Import Steps

After importing:

1. **Review Generated Files** - Check the generated metadata and trails
2. **Update Handler References** - Replace `handler_module` with your actual handler
3. **Integrate Routes** - Add to your Cowboy dispatch configuration:
   ```erlang
   Trails = trails:trails([generated_handler]),
   trails:store(Trails),
   Dispatch = trails:single_host_compile(Trails)
   ```
4. **Compile** - Run `rebar3 compile` to check for errors
5. **Implement Handlers** - Add actual request handling logic to your HTTP handler
6. **Generate OpenAPI Spec** (Optional) - Run `rebar3 openapi generate` to create a spec from your implementation

#### Import Features

The import functionality provides:

✅ **Complete OpenAPI 3.x Support**
- Paths, operations, parameters
- Request bodies with JSON schemas
- Response definitions
- Tags and operation IDs
- Descriptions and examples

✅ **Schema Translation**
- OpenAPI schemas → Erlang maps
- Proper type handling (string, integer, array, object)
- Enum support
- Validation constraints (minimum, maximum, minItems, etc.)

✅ **Code Quality**
- Properly formatted Erlang code
- Follows cowboy_swagger format
- Ready for trails integration
- Compiles without errors

✅ **Flexible Options**
- Customizable module names
- Configurable output directory
- Handler name substitution
- Overwrite protection

✅ **Incremental Updates** (New!)
- Smart merging when files already exist
- Adds new paths to existing handlers
- Creates missing metadata functions
- Updates existing metadata functions with new definitions
- Preserves your custom code

#### Incremental Update Behavior

When you run `rebar3 openapi import` and the target files already exist:

**Without `--overwrite` flag (default):**
- 📥 **Metadata Module**: Adds new metadata functions and updates existing ones
  - New paths → new functions added
  - Existing paths → function implementations updated
  - Preserves module structure

- 📥 **Handler Module**: Adds new trails for paths not already defined
  - Checks existing `trails:trail(...)` definitions
  - Only adds trails for new paths
  - Doesn't duplicate existing routes

**With `--overwrite` flag:**
- ⚠️ **Complete Regeneration**: Replaces entire files with fresh generated code
- Use when you want to start from scratch with the OpenAPI spec

**Examples:**

```bash
# First import - generates fresh files
rebar3 openapi import --spec api_v1.yaml

# Later, add new endpoints - merges into existing files
rebar3 openapi import --spec api_v2.yaml

# Force complete regeneration
rebar3 openapi import --spec api_v3.yaml --overwrite
```

**What Gets Merged:**

| Scenario                          | Metadata Module      | Handler Module         |
| --------------------------------- | -------------------- | ---------------------- |
| Path not in existing code         | ✅ New function added | ✅ New trail added      |
| Path exists, no metadata function | ✅ Function created   | ℹ️ Trail already exists |
| Path exists with metadata         | ✅ Function updated   | ℹ️ Trail already exists |

**Benefits:**
- 🔄 Iterative API development
- 🛡️ Preserves custom modifications
- 📈 Incremental spec updates
- 🚀 Faster development workflow

#### Benefits of Import

🎯 **Design-First Development** - Start with OpenAPI spec, generate handlers
🔄 **Round-Trip Support** - Import → Implement → Export workflow
📝 **Standards Compliance** - OpenAPI 3.x compatible
🚀 **Developer Productivity** - Auto-generate boilerplate code
✅ **Type Safety** - Schema definitions become Erlang metadata
🔧 **Maintainability** - Single source of truth (OpenAPI spec)
♻️ **Incremental Updates** - Merge new specs into existing code

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

> **✅ Verified:** The plugin fully supports function-based and resource module patterns. Metadata defined in separate functions or resource modules is correctly extracted and included in the generated OpenAPI specification. See `test_export_openapi.escript` for verification.

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

## Testing the Plugin Without Installing

The repository includes `test_import.escript`, a standalone script to test the OpenAPI import functionality **without importing the plugin into an existing project**. This is useful for:

- Testing OpenAPI specs before integrating into your project
- Debugging YAML parsing issues
- Validating generated code structure
- Learning how the import feature works

### Usage

```bash
# Basic usage - provide OpenAPI spec and source handler file
./test_import.escript your_openapi_spec.yaml your_handler.erl

# The script will generate output files in test_output/ directory:
# - test_output/<handler_module>.erl (handler with trails)
# - test_output/<handler_module>_metadata.erl (metadata functions)
```

### Arguments

| Argument | Description                                   | Required |
| -------- | --------------------------------------------- | -------- |
| arg1     | Path to OpenAPI YAML specification file       | Yes      |
| arg2     | Path to existing handler file (for reference) | Yes      |

> **Note:** The script automatically generates output paths in the `test_output/` directory based on the handler module name.

### Example

```bash
# Create a test OpenAPI spec
cat > test_api.yaml << 'EOF'
openapi: 3.0.3
info:
  title: Test API
  version: 1.0.0
paths:
  /users/{id}:
    get:
      summary: Get user by ID
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: integer
      responses:
        '200':
          description: Success
EOF

# Run the test script
./test_import.escript test_api.yaml ./test_output/handler.erl

# Check generated files
ls test_output/
# generated_handler.erl  generated_metadata.erl
```

### What It Does

The script performs the same steps as `rebar3 openapi import`:

1. ✅ Parses the OpenAPI YAML specification (ensures yamerl is started)
2. ✅ Generates metadata module with cowboy_swagger format
3. ✅ Generates handler module with trails definitions
4. ✅ Writes formatted Erlang code to files
5. ✅ Provides detailed output and error messages

### Notes

- **Requires compilation first**: Run `rebar3 compile` before using the script
- **Same functionality**: Equivalent to the rebar3 plugin but standalone
- **Testing only**: Use `rebar3 openapi import` for production

## Development

### Building

```bash
rebar3 compile
```

### Testing

The project includes test scripts for both directions of the plugin:

#### 1. Test Export OpenAPI (Erlang → OpenAPI YAML)
Tests parser and OpenAPI spec generation from Erlang handler code:

```bash
# Compile first
rebar3 compile

# Run with test handler
./test_export_openapi.escript test_cowboy_swagger_handler
```

**What it tests:**
- Parser functionality (cowboy_swagger format, method extraction, path normalization)
- Handler parsing and metadata extraction
- OpenAPI spec generation
- YAML output

**When to use:** Verify the complete flow of generating OpenAPI YAML from your Erlang handler code.

#### 2. Test OpenAPI YAML → Erlang
Tests generation/update of Erlang code from OpenAPI specification:

```bash
./test_import_openapi.escript <openapi.yml> <handler.erl> <metadata_module>

# Example
./test_import_openapi.escript test_output/test_api.yml my_handler.erl my_metadata
```

**What it tests:**
- OpenAPI YAML parsing
- Handler trails() function updates
- Metadata module generation
- Code formatting with erlfmt

**When to use:** Test updating handlers and generating metadata modules from OpenAPI docs.

**Output:** All generated files go to `test_output/` directory.

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
