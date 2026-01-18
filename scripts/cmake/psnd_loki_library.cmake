include_guard(GLOBAL)

set(LOKI_LIBRARY_TYPE STATIC)
if(LOKI_BUILD_SHARED)
    set(LOKI_LIBRARY_TYPE SHARED)
endif()

# Core loki sources (always built)
set(LOKI_CORE_SOURCES
    ${PSND_ROOT_DIR}/source/core/loki/core.c
    ${PSND_ROOT_DIR}/source/core/loki/lua.c
    ${PSND_ROOT_DIR}/source/core/loki/editor.c
    ${PSND_ROOT_DIR}/source/core/loki/syntax.c
    ${PSND_ROOT_DIR}/source/core/loki/indent.c
    ${PSND_ROOT_DIR}/source/core/loki/languages.c
    ${PSND_ROOT_DIR}/source/core/loki/selection.c
    ${PSND_ROOT_DIR}/source/core/loki/search.c
    ${PSND_ROOT_DIR}/source/core/loki/modal.c
    ${PSND_ROOT_DIR}/source/core/loki/command.c
    ${PSND_ROOT_DIR}/source/core/loki/command/file.c
    ${PSND_ROOT_DIR}/source/core/loki/command/basic.c
    ${PSND_ROOT_DIR}/source/core/loki/command/goto.c
    ${PSND_ROOT_DIR}/source/core/loki/command/substitute.c
    ${PSND_ROOT_DIR}/source/core/loki/command/link.c
    ${PSND_ROOT_DIR}/source/core/loki/command/csd.c
    ${PSND_ROOT_DIR}/source/core/loki/command/export.c
    ${PSND_ROOT_DIR}/source/core/loki/terminal.c
    ${PSND_ROOT_DIR}/source/core/loki/undo.c
    ${PSND_ROOT_DIR}/source/core/loki/buffers.c
    ${PSND_ROOT_DIR}/source/core/loki/link.c
    ${PSND_ROOT_DIR}/source/core/loki/csound.c
    ${PSND_ROOT_DIR}/source/core/loki/export.c
    ${PSND_ROOT_DIR}/source/core/loki/midi_export.cpp
    ${PSND_ROOT_DIR}/source/core/loki/lang_bridge.c
    ${PSND_ROOT_DIR}/source/core/loki/repl_launcher.c
    ${PSND_ROOT_DIR}/source/core/loki/serialize.c
)

# Collect language-specific register sources from discovered languages
psnd_collect_lang_sources()

add_library(libloki ${LOKI_LIBRARY_TYPE}
    ${LOKI_CORE_SOURCES}
    ${PSND_ALL_LANG_REGISTER_SOURCES}
)

# Base include directories
set(LOKI_PUBLIC_INCLUDES
    ${PSND_ROOT_DIR}/source/core/include
    ${PSND_ROOT_DIR}/source/core
    ${CMAKE_BINARY_DIR}/generated
    ${PSND_ROOT_DIR}/source/thirdparty/link-3.1.5/extensions/abl_link/include
    ${PSND_ROOT_DIR}/source/thirdparty/midifile/include
    ${PSND_ALL_LANG_INCLUDES}
)

target_include_directories(libloki PUBLIC ${LOKI_PUBLIC_INCLUDES})

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(libloki PRIVATE -Wall -Wextra -pedantic)
endif()

# Core libraries (always linked)
set(LOKI_PUBLIC_LIBS
    lua
    Threads::Threads
    ${CMAKE_DL_LIBS}
    abl_link
    midifile
    shared
)

# Add language libraries from discovered languages
list(APPEND LOKI_PUBLIC_LIBS ${PSND_ALL_LANG_LINK_LIBRARIES})

target_link_libraries(libloki PUBLIC ${LOKI_PUBLIC_LIBS})

# Apply language compile definitions (LANG_ALDA=1, LANG_JOY=1, etc.)
psnd_apply_lang_definitions(libloki)

set_target_properties(libloki PROPERTIES OUTPUT_NAME "loki")

if(NOT MSVC)
    target_link_libraries(libloki PUBLIC m)
endif()
