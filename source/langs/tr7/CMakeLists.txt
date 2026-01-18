# TR7 language - R7RS-small Scheme interpreter for music programming
#
# This CMakeLists.txt is processed by psnd_languages.cmake auto-discovery.
# No modifications to other files are needed to add/remove this language.
#
# TR7 uses the TR7 Scheme interpreter from thirdparty/tr7/ and adds
# music-specific primitives and MIDI integration.
#
# Note: TR7 doesn't have its own library - all TR7 code is compiled into
# libloki (register.c) and psnd_bin (repl.c, dispatch.c, async.c).
# The thirdparty/tr7 library provides the Scheme interpreter.

# ==============================================================================
# Register with psnd Language System
# ==============================================================================
# TR7 uses the thirdparty tr7 library directly. The register.c and repl.c files
# are compiled into libloki and psnd_bin respectively.
psnd_register_language(
    NAME tr7
    DISPLAY_NAME "TR7"
    DESCRIPTION "R7RS-small Scheme music language"
    COMMANDS tr7
    EXTENSIONS .scm .tr7
    SOURCES ""
    INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/impl
        ${PSND_ROOT_DIR}/source/thirdparty/tr7
    REPL_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/impl/repl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/impl/dispatch.c
        ${CMAKE_CURRENT_SOURCE_DIR}/impl/async.c
    REGISTER_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/impl/register.c
    LINK_LIBRARIES tr7
)
