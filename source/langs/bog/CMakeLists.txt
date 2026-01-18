# Bog language - Prolog-based live coding language for music
#
# This CMakeLists.txt is processed by psnd_languages.cmake auto-discovery.
# No modifications to other files are needed to add/remove this language.

# ==============================================================================
# Library Sources
# ==============================================================================
set(BOG_IMPL_SOURCES
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/bog.c
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/builtins.c
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/scheduler.c
    ${CMAKE_CURRENT_SOURCE_DIR}/impl/livecoding.c
)

set(BOG_ALL_LIB_SOURCES
    ${BOG_IMPL_SOURCES}
)

# ==============================================================================
# Build the Language Library
# ==============================================================================
add_library(bog STATIC ${BOG_ALL_LIB_SOURCES})
add_library(bog::bog ALIAS bog)

target_include_directories(bog
    PUBLIC
        ${PSND_ROOT_DIR}/source/core/include
        ${CMAKE_CURRENT_SOURCE_DIR}/impl
    PRIVATE
        ${PSND_ROOT_DIR}/source/core/shared
)

target_link_libraries(bog PUBLIC shared)

if(UNIX)
    target_link_libraries(bog PUBLIC m)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(bog PRIVATE -Wall -Wextra -Wpedantic)
endif()

target_compile_definitions(bog PUBLIC LANG_BOG)

# ==============================================================================
# Register with psnd Language System
# ==============================================================================
psnd_register_language(
    NAME bog
    DISPLAY_NAME "Bog"
    DESCRIPTION "Prolog-based live coding language"
    COMMANDS bog
    EXTENSIONS .bog .pl
    SOURCES ${BOG_ALL_LIB_SOURCES}
    INCLUDE_DIRS
        ${CMAKE_CURRENT_SOURCE_DIR}/impl
    REPL_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/repl.c
        ${CMAKE_CURRENT_SOURCE_DIR}/dispatch.c
        ${CMAKE_CURRENT_SOURCE_DIR}/bog_async.c
    REGISTER_SOURCES
        ${CMAKE_CURRENT_SOURCE_DIR}/register.c
    LINK_LIBRARIES bog
)
