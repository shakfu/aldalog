include_guard(GLOBAL)

# Common infrastructure
set(PSND_COMMON_SOURCES
    ${PSND_ROOT_DIR}/source/core/lang_dispatch.c
    ${PSND_ROOT_DIR}/source/core/repl.c
)

# Collect language-specific REPL and dispatch sources from discovered languages
psnd_collect_lang_sources()

add_executable(psnd_bin
    ${PSND_ROOT_DIR}/source/core/main.c
    ${PSND_COMMON_SOURCES}
    ${PSND_ALL_LANG_REPL_SOURCES}
)
set_target_properties(psnd_bin PROPERTIES OUTPUT_NAME "psnd")

if(NOT MSVC)
    add_custom_command(TARGET psnd_bin POST_BUILD
        COMMAND strip $<TARGET_FILE:psnd_bin>
        COMMENT "Stripping psnd binary"
    )
endif()

target_include_directories(psnd_bin PRIVATE
    ${PSND_ROOT_DIR}
    ${PSND_ROOT_DIR}/source/core/include
    ${PSND_ROOT_DIR}/source/core
    ${CMAKE_BINARY_DIR}/generated
    ${PSND_ALL_LANG_INCLUDES}
)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(psnd_bin PRIVATE -Wall -Wextra -pedantic)
endif()

target_link_libraries(psnd_bin PRIVATE libloki)

# Apply language compile definitions (LANG_ALDA=1, LANG_JOY=1, etc.)
psnd_apply_lang_definitions(psnd_bin)
