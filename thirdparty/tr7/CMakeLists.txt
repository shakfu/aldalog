# CMakeLists.txt for TR7 - Tiny R7RS-small Scheme interpreter
# https://gitlab.com/jobol/tr7

cmake_minimum_required(VERSION 3.18)

add_library(tr7 STATIC tr7.c)

target_include_directories(tr7 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# C11 standard
set_target_properties(tr7 PROPERTIES
    C_STANDARD 11
    C_STANDARD_REQUIRED ON
)

# TR7 compile definitions (match upstream Makefile)
target_compile_definitions(tr7 PRIVATE
    _POSIX_C_SOURCE=200809L
)

# Platform-specific flags
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(tr7 PRIVATE
        -Wall -Wextra
        -Wno-unused-parameter
        -fno-asynchronous-unwind-tables
        -fomit-frame-pointer
        -O3
    )
endif()

# Link math library on Unix
if(UNIX AND NOT APPLE)
    target_link_libraries(tr7 PUBLIC m)
endif()
