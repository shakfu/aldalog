/**
 * @file test_lexer.c
 * @brief Unit tests for Joy lexer (via joy_parse()).
 *
 * Tests lexer-level tokenization through the parser API:
 * - Integers, floats, strings with escapes, char literals
 * - Symbols (note-on, dup?, add!), brackets, booleans
 * - Line comments (\), block comments ((* *))
 * - Vulnerability tests (recursive lexer_next, unterminated strings)
 */

#include "test_framework.h"
#include "joy_runtime.h"
#include "joy_parser.h"
#include <string.h>
#include <math.h>
#include <stdlib.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Parse source and return quotation */
static JoyQuotation* lex_ok(const char* source) {
    JoyQuotation* quot = joy_parse(source);
    return quot;
}

/* ============================================================================
 * Integer Lexing Tests
 * ============================================================================ */

TEST(lex_integer_positive) {
    JoyQuotation* quot = lex_ok("42");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.integer, 42);
    joy_quotation_free(quot);
}

TEST(lex_integer_negative) {
    JoyQuotation* quot = lex_ok("-17");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.integer, -17);
    joy_quotation_free(quot);
}

TEST(lex_integer_zero) {
    JoyQuotation* quot = lex_ok("0");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.integer, 0);
    joy_quotation_free(quot);
}

TEST(lex_integer_large) {
    JoyQuotation* quot = lex_ok("999999999");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.integer, 999999999);
    joy_quotation_free(quot);
}

TEST(lex_integer_multiple) {
    JoyQuotation* quot = lex_ok("1 2 3 4 5");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 5);
    for (size_t i = 0; i < 5; i++) {
        ASSERT_EQ(quot->terms[i].type, JOY_INTEGER);
        ASSERT_EQ(quot->terms[i].data.integer, (int64_t)(i + 1));
    }
    joy_quotation_free(quot);
}

/* ============================================================================
 * Float Lexing Tests
 * ============================================================================ */

TEST(lex_float_simple) {
    JoyQuotation* quot = lex_ok("3.14");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_FLOAT);
    ASSERT_TRUE(fabs(quot->terms[0].data.floating - 3.14) < 0.001);
    joy_quotation_free(quot);
}

TEST(lex_float_negative) {
    JoyQuotation* quot = lex_ok("-2.5");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_FLOAT);
    ASSERT_TRUE(fabs(quot->terms[0].data.floating - (-2.5)) < 0.001);
    joy_quotation_free(quot);
}

TEST(lex_float_leading_zero) {
    JoyQuotation* quot = lex_ok("0.5");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_FLOAT);
    ASSERT_TRUE(fabs(quot->terms[0].data.floating - 0.5) < 0.001);
    joy_quotation_free(quot);
}

TEST(lex_float_exponent) {
    JoyQuotation* quot = lex_ok("1.5e10");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_FLOAT);
    ASSERT_TRUE(quot->terms[0].data.floating > 1.4e10);
    joy_quotation_free(quot);
}

TEST(lex_float_negative_exponent) {
    JoyQuotation* quot = lex_ok("1.5e-3");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_FLOAT);
    ASSERT_TRUE(fabs(quot->terms[0].data.floating - 0.0015) < 0.0001);
    joy_quotation_free(quot);
}

/* ============================================================================
 * String Lexing Tests
 * ============================================================================ */

TEST(lex_string_simple) {
    JoyQuotation* quot = lex_ok("\"hello\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_STR_EQ(quot->terms[0].data.string, "hello");
    joy_quotation_free(quot);
}

TEST(lex_string_empty) {
    JoyQuotation* quot = lex_ok("\"\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_STR_EQ(quot->terms[0].data.string, "");
    joy_quotation_free(quot);
}

TEST(lex_string_with_spaces) {
    JoyQuotation* quot = lex_ok("\"hello world\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_STR_EQ(quot->terms[0].data.string, "hello world");
    joy_quotation_free(quot);
}

TEST(lex_string_escape_newline) {
    JoyQuotation* quot = lex_ok("\"line1\\nline2\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_TRUE(strchr(quot->terms[0].data.string, '\n') != NULL);
    joy_quotation_free(quot);
}

TEST(lex_string_escape_tab) {
    JoyQuotation* quot = lex_ok("\"col1\\tcol2\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_TRUE(strchr(quot->terms[0].data.string, '\t') != NULL);
    joy_quotation_free(quot);
}

TEST(lex_string_escape_quote) {
    JoyQuotation* quot = lex_ok("\"say \\\"hi\\\"\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_TRUE(strchr(quot->terms[0].data.string, '"') != NULL);
    joy_quotation_free(quot);
}

TEST(lex_string_escape_backslash) {
    JoyQuotation* quot = lex_ok("\"path\\\\file\"");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    ASSERT_TRUE(strchr(quot->terms[0].data.string, '\\') != NULL);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Character Literal Tests
 * ============================================================================ */

TEST(lex_char_simple) {
    JoyQuotation* quot = lex_ok("'a");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_CHAR);
    ASSERT_EQ(quot->terms[0].data.character, 'a');
    joy_quotation_free(quot);
}

TEST(lex_char_digit) {
    JoyQuotation* quot = lex_ok("'0");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_CHAR);
    ASSERT_EQ(quot->terms[0].data.character, '0');
    joy_quotation_free(quot);
}

TEST(lex_char_escape_newline) {
    JoyQuotation* quot = lex_ok("'\\n");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_CHAR);
    ASSERT_EQ(quot->terms[0].data.character, '\n');
    joy_quotation_free(quot);
}

TEST(lex_char_escape_tab) {
    JoyQuotation* quot = lex_ok("'\\t");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_CHAR);
    ASSERT_EQ(quot->terms[0].data.character, '\t');
    joy_quotation_free(quot);
}

/* ============================================================================
 * Boolean Tests
 * ============================================================================ */

TEST(lex_boolean_true) {
    JoyQuotation* quot = lex_ok("true");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_BOOLEAN);
    ASSERT_TRUE(quot->terms[0].data.boolean);
    joy_quotation_free(quot);
}

TEST(lex_boolean_false) {
    JoyQuotation* quot = lex_ok("false");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_BOOLEAN);
    ASSERT_FALSE(quot->terms[0].data.boolean);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Symbol Tests
 * ============================================================================ */

TEST(lex_symbol_simple) {
    JoyQuotation* quot = lex_ok("dup");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "dup");
    joy_quotation_free(quot);
}

TEST(lex_symbol_with_hyphen) {
    JoyQuotation* quot = lex_ok("note-on");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "note-on");
    joy_quotation_free(quot);
}

TEST(lex_symbol_with_question) {
    JoyQuotation* quot = lex_ok("empty?");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "empty?");
    joy_quotation_free(quot);
}

TEST(lex_symbol_with_bang) {
    JoyQuotation* quot = lex_ok("set!");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "set!");
    joy_quotation_free(quot);
}

TEST(lex_symbol_operator_plus) {
    JoyQuotation* quot = lex_ok("+");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "+");
    joy_quotation_free(quot);
}

TEST(lex_symbol_operator_star) {
    JoyQuotation* quot = lex_ok("*");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "*");
    joy_quotation_free(quot);
}

TEST(lex_symbol_operator_eq) {
    JoyQuotation* quot = lex_ok("=");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "=");
    joy_quotation_free(quot);
}

TEST(lex_symbol_operator_lt) {
    JoyQuotation* quot = lex_ok("<");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "<");
    joy_quotation_free(quot);
}

TEST(lex_symbol_complex) {
    JoyQuotation* quot = lex_ok("<=>");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, "<=>");
    joy_quotation_free(quot);
}

/* ============================================================================
 * List Bracket Tests
 * ============================================================================ */

TEST(lex_list_empty) {
    JoyQuotation* quot = lex_ok("[]");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_LIST);
    ASSERT_EQ(quot->terms[0].data.list->length, 0);
    joy_quotation_free(quot);
}

TEST(lex_list_single) {
    JoyQuotation* quot = lex_ok("[42]");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_LIST);
    ASSERT_EQ(quot->terms[0].data.list->length, 1);
    joy_quotation_free(quot);
}

TEST(lex_list_multiple) {
    JoyQuotation* quot = lex_ok("[1 2 3]");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_LIST);
    ASSERT_EQ(quot->terms[0].data.list->length, 3);
    joy_quotation_free(quot);
}

TEST(lex_list_nested) {
    JoyQuotation* quot = lex_ok("[[1 2] [3 4]]");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_LIST);
    ASSERT_EQ(quot->terms[0].data.list->length, 2);
    /* First inner list */
    JoyValue first = quot->terms[0].data.list->items[0];
    ASSERT_EQ(first.type, JOY_LIST);
    ASSERT_EQ(first.data.list->length, 2);
    joy_quotation_free(quot);
}

TEST(lex_list_mixed_types) {
    JoyQuotation* quot = lex_ok("[42 \"hello\" true dup]");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_LIST);
    ASSERT_EQ(quot->terms[0].data.list->length, 4);
    ASSERT_EQ(quot->terms[0].data.list->items[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.list->items[1].type, JOY_STRING);
    ASSERT_EQ(quot->terms[0].data.list->items[2].type, JOY_BOOLEAN);
    ASSERT_EQ(quot->terms[0].data.list->items[3].type, JOY_SYMBOL);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Set Tests
 * ============================================================================ */

TEST(lex_set_empty) {
    JoyQuotation* quot = lex_ok("{}");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SET);
    ASSERT_EQ(quot->terms[0].data.set, 0);  /* Empty set = 0 */
    joy_quotation_free(quot);
}

TEST(lex_set_integers) {
    JoyQuotation* quot = lex_ok("{1 2 3}");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SET);
    /* Set with 1, 2, 3 should have bits 1, 2, 3 set = 0b1110 = 14 */
    ASSERT_EQ(quot->terms[0].data.set, 14);
    joy_quotation_free(quot);
}

TEST(lex_set_single) {
    JoyQuotation* quot = lex_ok("{5}");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SET);
    /* Set with 5 = bit 5 set = 32 */
    ASSERT_EQ(quot->terms[0].data.set, 32);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Comment Tests
 * ============================================================================ */

TEST(lex_line_comment) {
    JoyQuotation* quot = lex_ok("42 \\ this is a comment");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[0].data.integer, 42);
    joy_quotation_free(quot);
}

TEST(lex_line_comment_multiline) {
    JoyQuotation* quot = lex_ok("42 \\ comment\n17");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 2);
    ASSERT_EQ(quot->terms[0].data.integer, 42);
    ASSERT_EQ(quot->terms[1].data.integer, 17);
    joy_quotation_free(quot);
}

TEST(lex_block_comment) {
    JoyQuotation* quot = lex_ok("42 (* block comment *) 17");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 2);
    ASSERT_EQ(quot->terms[0].data.integer, 42);
    ASSERT_EQ(quot->terms[1].data.integer, 17);
    joy_quotation_free(quot);
}

TEST(lex_block_comment_nested) {
    JoyQuotation* quot = lex_ok("42 (* outer (* inner *) outer *) 17");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 2);
    ASSERT_EQ(quot->terms[0].data.integer, 42);
    ASSERT_EQ(quot->terms[1].data.integer, 17);
    joy_quotation_free(quot);
}

TEST(lex_block_comment_multiline) {
    JoyQuotation* quot = lex_ok("42 (* line1\nline2\nline3 *) 17");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 2);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Empty/Whitespace Tests
 * ============================================================================ */

TEST(lex_empty_string) {
    JoyQuotation* quot = lex_ok("");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 0);
    joy_quotation_free(quot);
}

TEST(lex_whitespace_only) {
    JoyQuotation* quot = lex_ok("   \t  \n  ");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 0);
    joy_quotation_free(quot);
}

TEST(lex_whitespace_between_tokens) {
    JoyQuotation* quot = lex_ok("  1   2   3  ");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 3);
    joy_quotation_free(quot);
}

/* ============================================================================
 * Mixed Type Tests
 * ============================================================================ */

TEST(lex_mixed_types) {
    JoyQuotation* quot = lex_ok("42 \"hello\" true 3.14 dup");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 5);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[1].type, JOY_STRING);
    ASSERT_EQ(quot->terms[2].type, JOY_BOOLEAN);
    ASSERT_EQ(quot->terms[3].type, JOY_FLOAT);
    ASSERT_EQ(quot->terms[4].type, JOY_SYMBOL);
    joy_quotation_free(quot);
}

TEST(lex_expression) {
    JoyQuotation* quot = lex_ok("1 2 + 3 *");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 5);
    ASSERT_EQ(quot->terms[0].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[1].type, JOY_INTEGER);
    ASSERT_EQ(quot->terms[2].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[2].data.symbol, "+");
    joy_quotation_free(quot);
}

/* ============================================================================
 * Vulnerability Tests
 * ============================================================================ */

TEST(lex_unterminated_string) {
    /* Lexer should not crash on unterminated string */
    JoyQuotation* quot = lex_ok("\"hello");
    ASSERT_NOT_NULL(quot);
    /* May produce a partial string or error */
    joy_quotation_free(quot);
}

TEST(lex_unterminated_block_comment) {
    /* Lexer should not crash on unterminated block comment */
    JoyQuotation* quot = lex_ok("42 (* unterminated");
    ASSERT_NOT_NULL(quot);
    /* Should still parse the 42 */
    ASSERT_TRUE(quot->length >= 1);
    joy_quotation_free(quot);
}

TEST(lex_unclosed_bracket) {
    /* Lexer should handle unclosed brackets */
    JoyQuotation* quot = lex_ok("[1 2 3");
    ASSERT_NOT_NULL(quot);
    joy_quotation_free(quot);
}

TEST(lex_unclosed_brace) {
    /* Lexer should handle unclosed braces */
    JoyQuotation* quot = lex_ok("{1 2 3");
    ASSERT_NOT_NULL(quot);
    joy_quotation_free(quot);
}

TEST(lex_very_long_symbol) {
    /* Test unbounded symbol scanning */
    char source[2048];
    for (int i = 0; i < 2000; i++) {
        source[i] = 'a';
    }
    source[2000] = '\0';

    JoyQuotation* quot = lex_ok(source);
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    joy_quotation_free(quot);
}

TEST(lex_very_long_string) {
    /* Test very long string */
    char source[4096];
    source[0] = '"';
    for (int i = 1; i < 4000; i++) {
        source[i] = 'x';
    }
    source[4000] = '"';
    source[4001] = '\0';

    JoyQuotation* quot = lex_ok(source);
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_STRING);
    joy_quotation_free(quot);
}

TEST(lex_many_nested_lists) {
    /* Test deeply nested lists */
    char source[256];
    int pos = 0;
    for (int i = 0; i < 50; i++) {
        source[pos++] = '[';
    }
    source[pos++] = '1';
    for (int i = 0; i < 50; i++) {
        source[pos++] = ']';
    }
    source[pos] = '\0';

    JoyQuotation* quot = lex_ok(source);
    ASSERT_NOT_NULL(quot);
    joy_quotation_free(quot);
}

TEST(lex_all_unknown_chars) {
    /* Test that unknown chars are skipped - should not infinite loop */
    JoyQuotation* quot = lex_ok("` ~ ^ , ;");
    ASSERT_NOT_NULL(quot);
    /* Some of these may be skipped */
    joy_quotation_free(quot);
}

/* ============================================================================
 * Statement Terminator Tests
 * ============================================================================ */

TEST(lex_period_as_symbol) {
    JoyQuotation* quot = lex_ok(".");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, ".");
    joy_quotation_free(quot);
}

TEST(lex_semicolon_as_symbol) {
    JoyQuotation* quot = lex_ok(";");
    ASSERT_NOT_NULL(quot);
    ASSERT_EQ(quot->length, 1);
    ASSERT_EQ(quot->terms[0].type, JOY_SYMBOL);
    ASSERT_STR_EQ(quot->terms[0].data.symbol, ";");
    joy_quotation_free(quot);
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

BEGIN_TEST_SUITE("Joy Lexer Tests")

    /* Integer lexing */
    RUN_TEST(lex_integer_positive);
    RUN_TEST(lex_integer_negative);
    RUN_TEST(lex_integer_zero);
    RUN_TEST(lex_integer_large);
    RUN_TEST(lex_integer_multiple);

    /* Float lexing */
    RUN_TEST(lex_float_simple);
    RUN_TEST(lex_float_negative);
    RUN_TEST(lex_float_leading_zero);
    RUN_TEST(lex_float_exponent);
    RUN_TEST(lex_float_negative_exponent);

    /* String lexing */
    RUN_TEST(lex_string_simple);
    RUN_TEST(lex_string_empty);
    RUN_TEST(lex_string_with_spaces);
    RUN_TEST(lex_string_escape_newline);
    RUN_TEST(lex_string_escape_tab);
    RUN_TEST(lex_string_escape_quote);
    RUN_TEST(lex_string_escape_backslash);

    /* Character literals */
    RUN_TEST(lex_char_simple);
    RUN_TEST(lex_char_digit);
    RUN_TEST(lex_char_escape_newline);
    RUN_TEST(lex_char_escape_tab);

    /* Booleans */
    RUN_TEST(lex_boolean_true);
    RUN_TEST(lex_boolean_false);

    /* Symbols */
    RUN_TEST(lex_symbol_simple);
    RUN_TEST(lex_symbol_with_hyphen);
    RUN_TEST(lex_symbol_with_question);
    RUN_TEST(lex_symbol_with_bang);
    RUN_TEST(lex_symbol_operator_plus);
    RUN_TEST(lex_symbol_operator_star);
    RUN_TEST(lex_symbol_operator_eq);
    RUN_TEST(lex_symbol_operator_lt);
    RUN_TEST(lex_symbol_complex);

    /* List brackets */
    RUN_TEST(lex_list_empty);
    RUN_TEST(lex_list_single);
    RUN_TEST(lex_list_multiple);
    RUN_TEST(lex_list_nested);
    RUN_TEST(lex_list_mixed_types);

    /* Sets */
    RUN_TEST(lex_set_empty);
    RUN_TEST(lex_set_integers);
    RUN_TEST(lex_set_single);

    /* Comments */
    RUN_TEST(lex_line_comment);
    RUN_TEST(lex_line_comment_multiline);
    RUN_TEST(lex_block_comment);
    RUN_TEST(lex_block_comment_nested);
    RUN_TEST(lex_block_comment_multiline);

    /* Empty/whitespace */
    RUN_TEST(lex_empty_string);
    RUN_TEST(lex_whitespace_only);
    RUN_TEST(lex_whitespace_between_tokens);

    /* Mixed types */
    RUN_TEST(lex_mixed_types);
    RUN_TEST(lex_expression);

    /* Vulnerability tests */
    RUN_TEST(lex_unterminated_string);
    RUN_TEST(lex_unterminated_block_comment);
    RUN_TEST(lex_unclosed_bracket);
    RUN_TEST(lex_unclosed_brace);
    RUN_TEST(lex_very_long_symbol);
    RUN_TEST(lex_very_long_string);
    RUN_TEST(lex_many_nested_lists);
    RUN_TEST(lex_all_unknown_chars);

    /* Statement terminators */
    RUN_TEST(lex_period_as_symbol);
    RUN_TEST(lex_semicolon_as_symbol);

END_TEST_SUITE()
