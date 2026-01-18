include_guard(GLOBAL)

enable_testing()
add_test(NAME psnd_version COMMAND $<TARGET_FILE:psnd_bin> --version)
add_test(NAME psnd_help COMMAND $<TARGET_FILE:psnd_bin> --help)

add_library(test_framework STATIC ${PSND_ROOT_DIR}/source/testing/test_framework.c)
target_include_directories(test_framework PUBLIC
    ${PSND_ROOT_DIR}/source/testing
    ${PSND_ROOT_DIR}/source/core/include
)

# Core tests (always built)
add_subdirectory(${PSND_ROOT_DIR}/source/core/tests/loki ${CMAKE_BINARY_DIR}/tests/loki)
add_subdirectory(${PSND_ROOT_DIR}/source/core/tests/shared ${CMAKE_BINARY_DIR}/tests/shared)
add_subdirectory(${PSND_ROOT_DIR}/source/core/tests/cli ${CMAKE_BINARY_DIR}/tests/cli)

# Language tests - discover from source/langs/<name>/tests/ directories
get_property(PSND_LANGUAGES GLOBAL PROPERTY PSND_ALL_LANGUAGES)

foreach(lang ${PSND_LANGUAGES})
    set(lang_test_dir "${PSND_ROOT_DIR}/source/langs/${lang}/tests")
    if(EXISTS "${lang_test_dir}/CMakeLists.txt")
        message(STATUS "Adding tests for language: ${lang}")
        add_subdirectory(${lang_test_dir} ${CMAKE_BINARY_DIR}/langs/${lang}/tests)
    endif()
endforeach()
