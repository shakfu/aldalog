# source/langs/CMakeLists.txt - Language module discovery and registration

# Include language discovery functions
include(psnd_languages)

# Discover and process all languages
message(STATUS "Discovering languages in ${CMAKE_CURRENT_SOURCE_DIR}...")
psnd_discover_languages_in(${CMAKE_CURRENT_SOURCE_DIR})

# Generate lang_config_generated.h and lang_dispatch_generated.h
psnd_generate_lang_headers()

# Add generated headers to include path
include_directories(${CMAKE_BINARY_DIR}/generated)

# Now that languages are discovered, build loki and psnd_bin
psnd_build_loki()
psnd_build_binary()

# Build core tests
psnd_build_core_tests()

# Build language tests
if(BUILD_TESTING)
    get_property(PSND_LANGUAGES GLOBAL PROPERTY PSND_ALL_LANGUAGES)
    foreach(lang ${PSND_LANGUAGES})
        set(lang_test_dir "${CMAKE_CURRENT_SOURCE_DIR}/${lang}/tests")
        if(EXISTS "${lang_test_dir}/CMakeLists.txt")
            message(STATUS "Adding tests for language: ${lang}")
            add_subdirectory(${lang_test_dir} ${CMAKE_BINARY_DIR}/langs/${lang}/tests)
        endif()
    endforeach()
endif()
