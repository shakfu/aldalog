# source/testing/CMakeLists.txt - Test framework library

add_library(test_framework STATIC
    ${CMAKE_CURRENT_SOURCE_DIR}/test_framework.c
)

target_include_directories(test_framework PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${PSND_ROOT_DIR}/source/core/include
)
