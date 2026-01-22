# Joy language tests

# Helper macro for joy tests
macro(add_joy_test TEST_NAME)
    add_executable(test_joy_${TEST_NAME}
        test_${TEST_NAME}.c
        ${PSND_ROOT_DIR}/source/testing/test_framework.c
    )
    target_link_libraries(test_joy_${TEST_NAME} PRIVATE joy m)
    target_include_directories(test_joy_${TEST_NAME} PRIVATE
        ${PSND_ROOT_DIR}/source/testing
    )
    add_test(
        NAME joy_${TEST_NAME}_tests
        COMMAND $<TARGET_FILE:test_joy_${TEST_NAME}>
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )
    set_tests_properties(joy_${TEST_NAME}_tests PROPERTIES LABELS "unit")
endmacro()

# Add all joy tests
add_joy_test(lexer)
add_joy_test(parser)
add_joy_test(primitives)
add_joy_test(midi)
