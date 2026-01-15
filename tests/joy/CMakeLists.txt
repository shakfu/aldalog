# Joy language tests

# Parser unit tests
add_executable(test_joy_parser
    test_parser.c
    ${CMAKE_SOURCE_DIR}/tests/test_framework.c
)
target_link_libraries(test_joy_parser PRIVATE joy)
target_link_libraries(test_joy_parser PRIVATE m)

add_test(
    NAME joy_parser_tests
    COMMAND $<TARGET_FILE:test_joy_parser>
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
set_tests_properties(joy_parser_tests PROPERTIES LABELS "unit")

# Primitives (stack operations) unit tests
add_executable(test_joy_primitives
    test_primitives.c
    ${CMAKE_SOURCE_DIR}/tests/test_framework.c
)
target_link_libraries(test_joy_primitives PRIVATE joy)
target_link_libraries(test_joy_primitives PRIVATE m)

add_test(
    NAME joy_primitives_tests
    COMMAND $<TARGET_FILE:test_joy_primitives>
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)
set_tests_properties(joy_primitives_tests PROPERTIES LABELS "unit")
