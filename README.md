# Taskord

Taskord is an Erlang application for ordering tasks with dependency management.

## Prerequisites
- Erlang/OTP (25+ recommended)
- [rebar3](https://www.rebar3.org/) build tool

## Compile
To compile the project:

```bash
rebar3 compile
```

## Run in Shell (Development)
To start the application in an Erlang shell:

```bash
rebar3 shell
```

This will start all dependencies and launch an interactive shell with the application running.

## Build Release
To build a production release:

```bash
rebar3 release
```

The release will be created in `_build/default/rel/taskord/`.

## Run Release
To run the release:

```bash
_build/default/rel/taskord/bin/taskord start
```

To stop:

```bash
_build/default/rel/taskord/bin/taskord stop
```

## Testing
To run EUnit tests:

```bash
rebar3 eunit
```

## REST API

The application exposes a single REST endpoint using Cowboy on port **2025**.

### Endpoint
- **Path:** `/api/orders`
- **Method:** `POST`
- **Content-Type:** `application/json`

### Request Format
Send a JSON object with a `tasks` key containing a list of tasks. Each task should have:
- `name`: Task name (string)
- `command`: Shell command (string)
- `requires`: (optional) List of task names this task depends on

Example:
```json
{
  "tasks": [
    {"name": "task-1", "command": "echo 1"},
    {"name": "task-2", "command": "echo 2", "requires": ["task-1"]}
  ]
}
```

### Example cURL Commands

**Submit tasks and get ordered tasks (JSON):**
```bash
curl -X POST http://localhost:2025/api/orders \
     -H 'Content-Type: application/json' \
     -d '{"tasks":[{"name":"task-1","command":"echo 1"},{"name":"task-2","command":"echo 2","requires":["task-1"]}]}'
```

**Get Bash script output:**
```bash
curl -X POST 'http://localhost:2025/api/orders?bash=true' \
     -H 'Content-Type: application/json' \
     -d '{"tasks":[{"name":"task-1","command":"echo 1"},{"name":"task-2","command":"echo 2","requires":["task-1"]}]}'
```

### Response
- By default: JSON array of ordered tasks
- With `?bash=true`: Bash script with commands in order

---
For more details, see `src/taskord_rest_orderer.erl`.
