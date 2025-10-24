# Quick Start - rebar3_openapi_generator

## For Immediate Testing (Butler Server)

### Step 1: Add Plugin (2 minutes)

Edit `/Users/amar.c/workspace/gm_core/butler_server_develop/rebar.config`:

```erlang
{plugins, [
    rebar3_hex,
    rebar3_run,
    rebar3_proper,
    % ADD THIS LINE:
    {rebar3_openapi_generator, {git, "file:///Users/amar.c/workspace/erlang_libs/rebar3_openapi_generator", {branch, "main"}}}
]}.
```

### Step 2: Compile Butler Server (1 minute)

```bash
cd /Users/amar.c/workspace/gm_core/butler_server_develop
make compile
```

### Step 3: Generate Your First Spec (30 seconds)

```bash
rebar3 openapi generate --app put
```

### Step 4: View Results

```bash
cat docs/openapi/put_coverage.txt
cat docs/openapi/put.yaml
```

You should see:
- **Coverage report** showing 0-10% documented (expected for first run)
- **OpenAPI spec** with auto-generated minimal entries for all routes
- **TODO list** with specific routes needing documentation

## Next Steps (After First Run)

### Add Metadata to One Simple Endpoint

Pick the simplest endpoint (like health check). Edit `apps/put/src/interface/put_http_handler.erl`:

```erlang
trails() ->
    HealthMeta = #{
        paths => #{
            <<"/api/put/health">> => #{
                get => #{
                    tags => [<<"Health">>],
                    summary => <<"Put app health check">>,
                    operationId => <<"getPutHealth">>,
                    responses => #{
                        <<"200">> => #{description => <<"Healthy">>}
                    }
                }
            }
        }
    },
    [
        {"/api/put/racks", put_http_handler, []},
        trails:trail("/api/put/health", put_http_handler, [], HealthMeta),
        % ... rest of routes
    ].
```

### Regenerate

```bash
make compile
rebar3 openapi generate --app put
```

Coverage should increase!

### View in Swagger UI

```bash
docker run -p 8080:8080 \
  -e SWAGGER_JSON=/docs/put.yaml \
  -v $(pwd)/docs/openapi:/docs \
  swaggerapi/swagger-ui
```

Open http://localhost:8080 in browser.

## Common Commands

```bash
# Generate for different apps
rebar3 openapi generate --app pick
rebar3 openapi generate --app audit
rebar3 openapi generate --app storage

# JSON format
rebar3 openapi generate --app put --format json

# With validation
rebar3 openapi generate --app put --validate true

# Custom output
rebar3 openapi generate --app put --output ./api-specs/
```

## Troubleshooting

**"Plugin not found"**
→ Make sure you ran `make compile` or `rebar3 compile` after adding to rebar.config

**"No handlers found"**
→ Check that app is compiled: `ls _build/default/lib/put/ebin/*.beam`

**Empty spec**
→ Normal on first run! Plugin generates minimal specs for undocumented routes

## Full Documentation

- **README.md** - Overview
- **USAGE_GUIDE.md** - Complete reference
- **IMPLEMENTATION_SUMMARY.md** - Architecture details
