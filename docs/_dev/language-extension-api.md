# Language Extension API

This guide explains how to add syntax highlighting support for new programming languages to psnd/loki.

## Overview

The language system uses a **hybrid architecture**:

- **Static layer** (C): Minimal keyword definitions for tests and markdown code blocks
- **Dynamic layer** (Lua): Full language definitions with lazy loading

Languages are defined in Lua files under `.psnd/languages/` and loaded on-demand when a matching file is opened.

## Quick Start

To add a new language, create a file `.psnd/languages/<name>.lua`:

```lua
-- .psnd/languages/mylang.lua
loki.register_language({
    name = "MyLang",
    extensions = {".ml", ".mylang"},
    keywords = {
        "if", "else", "while", "for", "return",
        "function", "let", "const", "var",
    },
    types = {
        "int", "float", "string", "bool", "void",
    },
    line_comment = "//",
    block_comment_start = "/*",
    block_comment_end = "*/",
    highlight_strings = true,
    highlight_numbers = true,
})
```

The language will be automatically discovered and loaded when you open a `.ml` or `.mylang` file.

## API Reference

### `loki.register_language(config)`

Registers a new language for syntax highlighting.

#### Parameters

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | Yes | Display name (e.g., "Python", "Alda") |
| `extensions` | table | Yes | File extensions including dot (e.g., `{".py", ".pyw"}`) |
| `keywords` | table | Yes | Primary keywords (control flow, declarations) |
| `types` | table | No | Type keywords (highlighted in different color) |
| `line_comment` | string | No | Single-line comment start (e.g., `"#"`, `"//"`) |
| `block_comment_start` | string | No | Multi-line comment start (e.g., `"/*"`) |
| `block_comment_end` | string | No | Multi-line comment end (e.g., `"*/"`) |
| `separators` | string | No | Word boundary characters (default: `",.()+-/*=~%<>[];:"`) |
| `highlight_strings` | boolean | No | Enable string literal highlighting (default: true) |
| `highlight_numbers` | boolean | No | Enable number highlighting (default: true) |

#### Return Value

Returns `true` on success, `nil, error_message` on failure.

#### Example: Python

```lua
loki.register_language({
    name = "Python",
    extensions = {".py", ".pyw", ".pyi"},
    keywords = {
        "and", "as", "assert", "async", "await", "break",
        "class", "continue", "def", "del", "elif", "else",
        "except", "finally", "for", "from", "global", "if",
        "import", "in", "is", "lambda", "nonlocal", "not",
        "or", "pass", "raise", "return", "try", "while",
        "with", "yield",
    },
    types = {
        "True", "False", "None",
        "int", "float", "str", "bool", "list", "dict",
        "tuple", "set", "bytes", "type", "object",
    },
    line_comment = "#",
    highlight_strings = true,
    highlight_numbers = true,
})
```

## Keywords vs Types

The `keywords` and `types` arrays are highlighted in different colors:

- **Keywords**: Control flow, declarations, operators (typically bold or primary color)
- **Types**: Type names, constants, special values (typically secondary color)

This distinction helps readability. For example, in Alda:

- Keywords: instrument names (`piano`, `violin`), attributes (`tempo`, `volume`)
- Types: note names (`c`, `d`, `e`), octave markers (`o4`, `o5`)

## Separators

The `separators` string defines word boundaries for keyword matching. Characters in this string break words apart during tokenization.

Default: `",.()+-/*=~%<>[];:"`

For languages with special syntax, customize this. For example, Alda uses:

```lua
separators = ",.()+-/*=~%<>[]{}:;|'\""
```

## Comment Styles

### Single-line comments

```lua
line_comment = "#"      -- Python, Ruby, Shell
line_comment = "//"     -- C, JavaScript, Go
line_comment = "--"     -- Lua, SQL, Haskell
line_comment = ";"      -- Lisp, Assembly
```

### Multi-line comments

```lua
block_comment_start = "/*"
block_comment_end = "*/"

-- Or for languages without block comments:
block_comment_start = ""
block_comment_end = ""
```

## Lazy Loading

Languages are loaded on-demand for performance. The system:

1. Scans `.psnd/languages/` at startup
2. Extracts extensions from each file (without full loading)
3. Builds an extension-to-file mapping
4. Loads the full definition only when a matching file is opened

### Language Module API

```lua
local languages = require("languages")

languages.init()                    -- Initialize (called at startup)
languages.load("Python")            -- Load by name
languages.load_for_extension(".py") -- Load by extension
languages.reload("Python")          -- Hot-reload a language
languages.list()                    -- List available languages
languages.stats()                   -- Show loading statistics
```

## File Structure

```text
~/.psnd/
  init.lua              -- Loads languages module
  modules/
    languages.lua       -- Lazy loading system
  languages/
    alda.lua           -- Alda music language
    c.lua              -- C/C++
    python.lua         -- Python
    lua.lua            -- Lua
    javascript.lua     -- JavaScript
    go.lua             -- Go
    rust.lua           -- Rust
    ...
```

## Complete Example: Alda Language

```lua
-- .psnd/languages/alda.lua
-- Alda music composition language

local instruments = {
    -- Piano family
    "piano", "acoustic-grand-piano", "bright-acoustic-piano",
    "electric-grand-piano", "honky-tonk-piano", "electric-piano-1",
    "electric-piano-2", "harpsichord", "clavinet",
    -- ... (128 GM instruments)
}

local attributes = {
    "tempo", "tempo!", "volume", "vol", "track-volume",
    "panning", "pan", "quantization", "quant", "quantize",
    "duration", "key-signature", "key-sig", "transpose",
    "octave", "set-duration", "set-note-length",
}

-- Combine into keywords
local keywords = {}
for _, v in ipairs(instruments) do table.insert(keywords, v) end
for _, v in ipairs(attributes) do table.insert(keywords, v) end

-- Note names and octave markers as types
local types = {
    "c", "d", "e", "f", "g", "a", "b", "r",  -- Notes + rest
    "o0", "o1", "o2", "o3", "o4", "o5", "o6", "o7", "o8", "o9",
}

loki.register_language({
    name = "Alda",
    extensions = {".alda"},
    keywords = keywords,
    types = types,
    line_comment = "#",
    block_comment_start = "",
    block_comment_end = "",
    separators = ",.()+-/*=~%<>[]{}:;|'\"",
    highlight_strings = true,
    highlight_numbers = true,
})
```

## Special Highlighting Types

Some languages need custom highlighting beyond keyword matching. These are implemented in C:

### Markdown (`HL_TYPE_MARKDOWN`)

- Header highlighting (`#`, `##`, `###`)
- Code block detection with nested language highlighting
- Link and emphasis detection

### Csound (`HL_TYPE_CSOUND`)

- Multi-section format (`<CsOptions>`, `<CsInstruments>`, `<CsScore>`)
- Section-specific syntax rules
- Orchestra vs. score highlighting

To use these, the language must be registered in the static C database with the appropriate type flag. Contact the maintainers to add new special types.

## Testing Your Language

1. Create your language file in `.psnd/languages/`
2. Open a file with the matching extension
3. Check syntax highlighting works correctly
4. Use `:lua languages.reload("YourLang")` to hot-reload changes

### Debugging

```lua
-- In psnd REPL or command mode
:lua languages.list()              -- See all registered languages
:lua languages.stats()             -- Check loading status
:lua print(loki.get_filetype())    -- See current file's language
```

## Best Practices

1. **Group related keywords**: Organize keywords logically (control flow, declarations, operators)
2. **Use types for constants**: Built-in values, type names go in `types`
3. **Test with real files**: Verify highlighting on actual code samples
4. **Keep it minimal**: Only include commonly-used keywords
5. **Document your language**: Add comments explaining the keyword choices

## Limitations

- No semantic highlighting (requires language server)
- No context-aware highlighting (all keywords match everywhere)
- String detection is basic (no escape sequence handling)
- No regex-based patterns (keyword matching only)

For advanced features, consider integrating with Tree-sitter or LSP (future roadmap items).

## Contributing

To contribute a language definition:

1. Create `.psnd/languages/<lang>.lua`
2. Test thoroughly with sample files
3. Submit a pull request

See existing language files for reference implementations.
