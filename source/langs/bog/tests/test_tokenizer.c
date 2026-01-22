/*
 * @file test_tokenizer.c
 * @brief Unit tests for Bog tokenizer (via bog_parse_program()).
 *
 * Tests tokenization through the parser API:
 * - Identifiers, numbers (int/float)
 * - Multi-char operators (=:=, =\=, =<, >=, :-, \+)
 * - Line comments (%)
 * - Vulnerability tests (buffer boundaries, EOF handling)
 */
#include "test_framework.h"

#include "bog.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Parse source and check for success */
static BogProgram* tok_ok(const char* source, BogArena* arena, char** error) {
    *error = NULL;
    BogProgram* program = bog_parse_program(source, arena, error);
    return program;
}

/* ============================================================================
 * Identifier Tests
 * ============================================================================ */

TEST(tok_identifier_simple)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("foo(1).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_NULL(error);
    ASSERT_EQ(program->count, 1);
    ASSERT_STREQ(program->clauses[0].head->value.compound.functor, "foo");

    bog_arena_destroy(arena);
    TEST_PASS();
}

TEST(tok_identifier_underscore)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("foo_bar(1).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_STREQ(program->clauses[0].head->value.compound.functor, "foo_bar");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_identifier_with_digits)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test123(1).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_STREQ(program->clauses[0].head->value.compound.functor, "test123");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_variable_single_char)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_VAR);
    ASSERT_STREQ(program->clauses[0].head->value.compound.args[0]->value.atom, "X");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_variable_underscore)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(_).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_VAR);
    ASSERT_STREQ(program->clauses[0].head->value.compound.args[0]->value.atom, "_");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_variable_multi_char)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(Variable).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_VAR);
    ASSERT_STREQ(program->clauses[0].head->value.compound.args[0]->value.atom, "Variable");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Number Tests
 * ============================================================================ */

TEST(tok_number_integer)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(42).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_NUM);
    ASSERT_NEAR(program->clauses[0].head->value.compound.args[0]->value.number, 42.0, 1e-9);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_float)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(3.14).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_NUM);
    ASSERT_NEAR(program->clauses[0].head->value.compound.args[0]->value.number, 3.14, 1e-9);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_zero)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(0).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_NEAR(program->clauses[0].head->value.compound.args[0]->value.number, 0.0, 1e-9);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_negative)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    /* Negative numbers in Prolog are expressions: 0 - 5 or use 'is' */
    BogProgram* program = tok_ok("test(X) :- X is 0 - 5.", arena, &error);

    ASSERT_NOT_NULL(program);
    /* The expression 0 - 5 should parse as subtraction */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_small_float)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(0.001).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_NEAR(program->clauses[0].head->value.compound.args[0]->value.number, 0.001, 1e-9);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Operator Tests
 * ============================================================================ */

TEST(tok_operator_implies)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("a(X) :- b(X).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].body.count, 1);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_unify)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X = 5.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_is)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is 2 + 3.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_eq_num)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X =:= 5.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_neq_num)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X =\\= 5.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_leq)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X =< 5.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_geq)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X >= 5.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_operator_negation)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- \\+ fail.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Arithmetic Expression Tests
 * ============================================================================ */

TEST(tok_expr_plus)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is 2 + 3.", arena, &error);

    ASSERT_NOT_NULL(program);
    /* Check that + is parsed as expression */
    BogTerm* expr = program->clauses[0].body.items[0].data.term->value.compound.args[1];
    ASSERT_EQ(expr->type, CPROLOG_TERM_EXPR);
    ASSERT_EQ(expr->value.expr.op, '+');

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_expr_minus)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is 5 - 3.", arena, &error);

    ASSERT_NOT_NULL(program);
    BogTerm* expr = program->clauses[0].body.items[0].data.term->value.compound.args[1];
    ASSERT_EQ(expr->type, CPROLOG_TERM_EXPR);
    ASSERT_EQ(expr->value.expr.op, '-');

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_expr_mult)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is 2 * 3.", arena, &error);

    ASSERT_NOT_NULL(program);
    BogTerm* expr = program->clauses[0].body.items[0].data.term->value.compound.args[1];
    ASSERT_EQ(expr->type, CPROLOG_TERM_EXPR);
    ASSERT_EQ(expr->value.expr.op, '*');

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_expr_div)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is 6 / 2.", arena, &error);

    ASSERT_NOT_NULL(program);
    BogTerm* expr = program->clauses[0].body.items[0].data.term->value.compound.args[1];
    ASSERT_EQ(expr->type, CPROLOG_TERM_EXPR);
    ASSERT_EQ(expr->value.expr.op, '/');

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_expr_parenthesized)
{
    /* Test parenthesized arithmetic expressions */
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(X) :- X is (2 + 3) * 4.", arena, &error);

    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * List Tests
 * ============================================================================ */

TEST(tok_list_empty)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test([]).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_LIST);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->value.list.length, 0);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_list_single)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test([1]).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_LIST);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->value.list.length, 1);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_list_multiple)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test([1, 2, 3]).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_LIST);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->value.list.length, 3);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_list_head_tail)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test([H|T]).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->clauses[0].head->value.compound.args[0]->type, CPROLOG_TERM_LIST);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Comment Tests
 * ============================================================================ */

TEST(tok_comment_line)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("% this is a comment\ntest(1).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->count, 1);
    ASSERT_STREQ(program->clauses[0].head->value.compound.functor, "test");

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_comment_end_of_line)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("test(1). % end comment", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->count, 1);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_comment_multiple)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;
    BogProgram* program = tok_ok("% comment 1\n% comment 2\ntest(1).", arena, &error);

    ASSERT_NOT_NULL(program);
    ASSERT_EQ(program->count, 1);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Vulnerability Tests
 * ============================================================================ */

TEST(tok_very_long_identifier)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    /* Create a very long identifier */
    char source[2100];
    for (int i = 0; i < 2000; i++) {
        source[i] = 'a';
    }
    strcpy(source + 2000, "(1).");

    BogProgram* program = tok_ok(source, arena, &error);
    /* Should not crash - may or may not succeed */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_64_chars)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    /* Create a 64-char number (at the buffer boundary) */
    char source[128];
    memset(source, '1', 63);
    source[63] = '\0';
    strcat(source, ".");

    BogProgram* program = bog_parse_program(source, arena, &error);
    /* May succeed or fail, but should not crash */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_number_65_chars)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    /* Create a 65-char number (exceeds 64-byte buffer) */
    char source[128];
    memset(source, '1', 65);
    source[65] = '\0';
    strcat(source, ".");

    BogProgram* program = bog_parse_program(source, arena, &error);
    /* Should produce error "Numeric literal too long" */
    ASSERT_NULL(program);
    ASSERT_NOT_NULL(error);
    /* Error message should indicate numeric literal too long */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_comment_no_newline)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    /* Comment at EOF without newline */
    BogProgram* program = bog_parse_program("test(1). % comment", arena, &error);
    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_empty_source)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("", arena, &error);
    /* Empty source - may return empty program or NULL */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_whitespace_only)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("   \t\n  ", arena, &error);
    /* Whitespace only - may return empty program or NULL */

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_invalid_char)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("test`(1).", arena, &error);
    /* Backtick is not valid - should error */
    ASSERT_NULL(program);
    ASSERT_NOT_NULL(error);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_nested_parens)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = tok_ok("test(foo(bar(baz(1)))).", arena, &error);
    ASSERT_NOT_NULL(program);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Error Recovery Tests
 * ============================================================================ */

TEST(tok_error_missing_period)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("test(1)", arena, &error);
    ASSERT_NULL(program);
    ASSERT_NOT_NULL(error);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_error_unbalanced_parens)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("test(1.", arena, &error);
    ASSERT_NULL(program);
    ASSERT_NOT_NULL(error);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

TEST(tok_error_unbalanced_brackets)
{
    BogArena* arena = bog_arena_create();
    char* error = NULL;

    BogProgram* program = bog_parse_program("test([1, 2).", arena, &error);
    ASSERT_NULL(program);
    ASSERT_NOT_NULL(error);

    bog_arena_destroy(arena);
    free(error);
    TEST_PASS();
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

int main(void)
{
    printf("Bog Tokenizer Tests\n");
    printf("===================\n");

    /* Identifier tests */
    RUN_TEST(tok_identifier_simple);
    RUN_TEST(tok_identifier_underscore);
    RUN_TEST(tok_identifier_with_digits);
    RUN_TEST(tok_variable_single_char);
    RUN_TEST(tok_variable_underscore);
    RUN_TEST(tok_variable_multi_char);

    /* Number tests */
    RUN_TEST(tok_number_integer);
    RUN_TEST(tok_number_float);
    RUN_TEST(tok_number_zero);
    RUN_TEST(tok_number_negative);
    RUN_TEST(tok_number_small_float);

    /* Operator tests */
    RUN_TEST(tok_operator_implies);
    RUN_TEST(tok_operator_unify);
    RUN_TEST(tok_operator_is);
    RUN_TEST(tok_operator_eq_num);
    RUN_TEST(tok_operator_neq_num);
    RUN_TEST(tok_operator_leq);
    RUN_TEST(tok_operator_geq);
    RUN_TEST(tok_operator_negation);

    /* Expression tests */
    RUN_TEST(tok_expr_plus);
    RUN_TEST(tok_expr_minus);
    RUN_TEST(tok_expr_mult);
    RUN_TEST(tok_expr_div);
    RUN_TEST(tok_expr_parenthesized);

    /* List tests */
    RUN_TEST(tok_list_empty);
    RUN_TEST(tok_list_single);
    RUN_TEST(tok_list_multiple);
    RUN_TEST(tok_list_head_tail);

    /* Comment tests */
    RUN_TEST(tok_comment_line);
    RUN_TEST(tok_comment_end_of_line);
    RUN_TEST(tok_comment_multiple);

    /* Vulnerability tests */
    RUN_TEST(tok_very_long_identifier);
    RUN_TEST(tok_number_64_chars);
    RUN_TEST(tok_number_65_chars);
    RUN_TEST(tok_comment_no_newline);
    RUN_TEST(tok_empty_source);
    RUN_TEST(tok_whitespace_only);
    RUN_TEST(tok_invalid_char);
    RUN_TEST(tok_nested_parens);

    /* Error tests */
    RUN_TEST(tok_error_missing_period);
    RUN_TEST(tok_error_unbalanced_parens);
    RUN_TEST(tok_error_unbalanced_brackets);

    TEST_SUMMARY();
    return TEST_EXIT_CODE();
}
