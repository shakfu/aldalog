# Joy language - Concatenative (stack-based) functional language for music
#
# This CMakeLists.txt is processed by psnd_languages.cmake auto-discovery.
# No modifications to other files are needed to add/remove this language.

# ==============================================================================
# Library Sources
# ==============================================================================
set(JOY_IMPL_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/joy_runtime.c
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/joy_parser.c
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/joy_primitives.c
)

set(JOY_MUSIC_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/music/music_notation.c
    ${CMAKE_CURRENT_SOURCE_DIR}/music/music_context.c
)

set(JOY_MIDI_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/midi/joy_midi_backend.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi/midi_primitives.c
    ${CMAKE_CURRENT_SOURCE_DIR}/midi/joy_async.c
)

set(JOY_ALL_LIB_SOURCES
    ${JOY_IMPL_SOURCES}
    ${JOY_MUSIC_SOURCES}
    ${JOY_MIDI_SOURCES}
)

# ==============================================================================
# Build the Language Library
# ==============================================================================
add_library(joy STATIC ${JOY_ALL_LIB_SOURCES})
add_library(joy::joy ALIAS joy)

target_include_directories(joy
    PUBLIC
        ${PSND_ROOT_DIR}/source/core/include
        ${CMAKE_CURRENT_SOURCE_DIR}/impl
        ${CMAKE_CURRENT_SOURCE_DIR}/music
        ${CMAKE_CURRENT_SOURCE_DIR}/midi
    PRIVATE
        ${PSND_ROOT_DIR}/source/core/shared
)

target_link_libraries(joy PUBLIC shared libremidi)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy PRIVATE -Wall -Wextra -Wpedantic)
endif()

# ==============================================================================
# Register with psnd Language System
# ==============================================================================
psnd_register_language(
    NAME joy
    DISPLAY_NAME "Joy"
    DESCRIPTION "Concatenative stack-based music language"
    COMMANDS joy
    EXTENSIONS .joy
    SOURCES ${JOY_ALL_LIB_SOURCES}
    INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/impl
        ${CMAKE_CURRENT_SOURCE_DIR}/music
        ${CMAKE_CURRENT_SOURCE_DIR}/midi
    REPL_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/repl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/dispatch.c
    REGISTER_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/register.c
    LINK_LIBRARIES joy
)
