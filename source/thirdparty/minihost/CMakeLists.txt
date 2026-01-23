# minihost - Minimal audio plugin host library using JUCE
#
# Uses FetchContent to download JUCE during configure.
# Builds a static library that can host VST3 and AU plugins.

include(FetchContent)

# Fetch JUCE from GitHub
# Using JUCE 8.0.4 for macOS 15.0 (Sequoia) compatibility
FetchContent_Declare(
    JUCE
    GIT_REPOSITORY https://github.com/juce-framework/JUCE.git
    GIT_TAG        8.0.4
    GIT_SHALLOW    TRUE
    GIT_PROGRESS   TRUE
)

message(STATUS "Fetching JUCE (this may take a moment on first configure)...")
FetchContent_MakeAvailable(JUCE)

# Build minihost as a static library
add_library(minihost STATIC
    minihost.cpp
)

target_include_directories(minihost PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Link JUCE modules
target_link_libraries(minihost PRIVATE
    juce::juce_core
    juce::juce_audio_basics
    juce::juce_audio_processors
)

# Platform-specific frameworks
if(APPLE)
    target_link_libraries(minihost PRIVATE
        "-framework AudioUnit"
        "-framework AudioToolbox"
        "-framework CoreAudioKit"
        "-framework Cocoa"
        "-framework CoreFoundation"
        "-framework CoreMIDI"
        "-framework CoreAudio"
    )
elseif(UNIX AND NOT APPLE)
    # Linux may need additional dependencies
    find_package(PkgConfig)
    if(PKG_CONFIG_FOUND)
        pkg_check_modules(ALSA alsa)
        if(ALSA_FOUND)
            target_link_libraries(minihost PRIVATE ${ALSA_LIBRARIES})
        endif()
    endif()
    target_link_libraries(minihost PRIVATE pthread dl)
endif()

# Enable plugin hosting formats
target_compile_definitions(minihost PRIVATE
    JUCE_PLUGINHOST_VST3=1
    JUCE_PLUGINHOST_AU=1
    JUCE_WEB_BROWSER=0
    JUCE_USE_CURL=0
    JUCE_DISPLAY_SPLASH_SCREEN=0
    JUCE_MODAL_LOOPS_PERMITTED=0
    JUCE_STANDALONE_APPLICATION=0
)

# Suppress JUCE warnings
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(minihost PRIVATE -w)
endif()
