-- ==============================================================================
-- Aldev Editor Configuration
-- ==============================================================================
--
-- This is the main configuration file for the Aldev editor.
-- Loading priority:
--   1. .aldev/init.lua (local, project-specific)
--   2. ~/.aldev/init.lua (global, home directory)
--
-- ==============================================================================

-- Detect execution mode: "editor" or "repl"
MODE = loki.get_lines and "editor" or "repl"

-- ==============================================================================
-- Configure Module Path
-- ==============================================================================

-- Add .aldev/modules to package.path for require()
package.path = package.path .. ";.aldev/modules/?.lua"

-- Also add global config path if available
local home = os.getenv("HOME")
if home then
    package.path = package.path .. ";" .. home .. "/.aldev/modules/?.lua"
end

-- ==============================================================================
-- Load Core Modules
-- ==============================================================================

-- Load language definitions (lazy loading - loads on file open)
languages = require("languages")
local ext_count = languages.init()

-- Load theme utilities
theme = require("theme")

-- Load Alda integration module
alda = require("alda")

-- ==============================================================================
-- Editor Settings
-- ==============================================================================

if MODE == "editor" then
    -- Load a color theme
    local ok, err = theme.load("nord")
    if not ok and err then
        loki.status("Theme: " .. tostring(err))
    end

    -- Enable line numbers (added in latest version)
    loki.line_numbers(true)
end

-- ==============================================================================
-- Startup Message
-- ==============================================================================

if MODE == "editor" then
    if ext_count > 0 then
        loki.status(string.format("Aldev ready. %d language extensions available.", ext_count))
    else
        loki.status("Aldev ready. Press Ctrl-L for Lua REPL.")
    end
end
