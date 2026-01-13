# Loki Modules

This directory contains modular components for Loki editor. Each module encapsulates specific functionality and can be loaded independently.

## Core Modules

### `languages.lua`
Auto-loads language definitions from `.loki/languages/` directory.

```lua
local languages = require(".loki.modules.languages")
local count = languages.load_all()  -- Load all language files
```

### `markdown.lua`
Provides enhanced syntax highlighting for Markdown and TODO/FIXME tags.

```lua
local markdown = require(".loki.modules.markdown")
markdown.install()  -- Install global highlight_row function
```

### `theme.lua`
Theme loading and management utilities.

```lua
theme = require(".loki.modules.theme")

-- Load a theme
theme.load("dracula")

-- Try to load (silent fail)
theme.try_load("monokai")

-- List available themes
local themes = theme.list()

-- Print themes (REPL)
theme.print_available()
```

## Feature Modules

### `editor.lua`
Common editor utilities (editor mode only).

```lua
editor = require(".loki.modules.editor")

editor.count_lines()   -- Show line count
editor.cursor()        -- Show cursor position
editor.timestamp()     -- Insert current timestamp
editor.first_line()    -- Display first line
```

### `ai.lua`
AI integration via OpenAI API (requires `OPENAI_API_KEY`).

```lua
ai = require(".loki.modules.ai")

-- In editor: uses buffer content
ai.complete()
ai.explain()

-- In REPL: provide text
ai.complete("your prompt here")
ai.explain("code to explain")
```

### `test.lua`
Test and demo functions.

```lua
test = require(".loki.modules.test")

test.http()  -- Test async HTTP with GitHub API
```

## Creating Custom Modules

### Module Template

See `example.lua` for a complete template. Basic structure:

```lua
-- my_module.lua
local M = {}

-- Detect execution mode
local MODE = loki.get_lines and "editor" or "repl"

-- Your functions
function M.my_function()
    if MODE == "editor" then
        loki.status("Hello from editor!")
    else
        print("Hello from REPL!")
    end
end

-- Register with REPL help (optional)
if loki.repl and loki.repl.register then
    loki.repl.register("mymod.my_function", "Description here")
end

return M
```

### Loading Your Module

In `init.lua`:

```lua
-- Load module (use dofile for .loki modules)
mymod = dofile(".loki/modules/my_module.lua")

-- Use it
mymod.my_function()
```

## Best Practices

1. **Always return a module table** - Use `local M = {}` and `return M`

2. **Handle both modes** - Check `MODE` to support editor and REPL contexts

3. **Global callbacks** - HTTP callbacks must be in `_G`:
   ```lua
   function _G.my_callback(response)
       -- handle response
   end
   ```

4. **Register with REPL** - Use `loki.repl.register()` for discoverability

5. **Document your module** - Add comments explaining functions and usage

6. **Namespace your globals** - Use unique names for callback functions to avoid conflicts

## Module Loading Path

Modules are loaded using Lua's `dofile()` with relative paths:

- `dofile(".loki/modules/editor.lua")` → Loads `.loki/modules/editor.lua`
- `dofile(".loki/modules/my_module.lua")` → Loads `.loki/modules/my_module.lua`

The path is relative to the current working directory.

## Available Loki APIs

See CLAUDE.md for full API documentation:

- `loki.status(msg)` - Set status message
- `loki.get_lines()` - Get line count
- `loki.get_line(row)` - Get line content
- `loki.get_cursor()` - Get cursor position
- `loki.insert_text(text)` - Insert text
- `loki.get_filename()` - Get filename
- `loki.async_http(...)` - Async HTTP request
- `loki.set_color(name, {r,g,b})` - Set syntax color
- `loki.set_theme(table)` - Set color theme
- `loki.register_language(config)` - Register language
- `loki.repl.register(name, help)` - Register REPL help
