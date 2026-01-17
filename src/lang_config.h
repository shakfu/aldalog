/* lang_config.h - Language configuration for psnd
 *
 * This file centralizes all language-specific declarations.
 * When adding a new language, modify ONLY this file.
 *
 * To add a new language "foo":
 * 1. Add IF_LANG_FOO helper macro (with #ifdef LANG_FOO guard)
 * 2. Add forward declaration: IF_LANG_FOO(struct LokiFooState;)
 * 3. Add to LOKI_LANG_STATE_FIELDS macro
 * 4. Add init declaration: IF_LANG_FOO(void foo_loki_lang_init(void);)
 * 5. Add to LOKI_LANG_INIT_ALL macro
 */

#ifndef LOKI_LANG_CONFIG_H
#define LOKI_LANG_CONFIG_H

/* ======================= Helper Macros ==================================== */

#ifdef LANG_ALDA
#define IF_LANG_ALDA(x) x
#else
#define IF_LANG_ALDA(x)
#endif

#ifdef LANG_JOY
#define IF_LANG_JOY(x) x
#else
#define IF_LANG_JOY(x)
#endif

#ifdef LANG_TR7
#define IF_LANG_TR7(x) x
#else
#define IF_LANG_TR7(x)
#endif

#ifdef LANG_BOG
#define IF_LANG_BOG(x) x
#else
#define IF_LANG_BOG(x)
#endif

/* ======================= Forward Declarations ============================= */

/* Language state structures - opaque pointers defined in each language's register.c */
IF_LANG_ALDA(struct LokiAldaState;)
IF_LANG_JOY(struct LokiJoyState;)
IF_LANG_TR7(struct LokiTr7State;)
IF_LANG_BOG(struct LokiBogState;)

/* ======================= EditorModel State Fields ========================= */

/*
 * Language state pointers for EditorModel.
 * Each language gets a pointer that is NULL until initialized.
 * Use this macro inside the EditorModel struct definition.
 */
#define LOKI_LANG_STATE_FIELDS \
    IF_LANG_ALDA(struct LokiAldaState *alda_state;) \
    IF_LANG_JOY(struct LokiJoyState *joy_state;) \
    IF_LANG_TR7(struct LokiTr7State *tr7_state;) \
    IF_LANG_BOG(struct LokiBogState *bog_state;)

/* ======================= Language Init Declarations ======================= */

/* Init functions - called by loki_lang_init() to register each language */
IF_LANG_ALDA(void alda_loki_lang_init(void);)
IF_LANG_JOY(void joy_loki_lang_init(void);)
IF_LANG_TR7(void tr7_loki_lang_init(void);)
IF_LANG_BOG(void bog_loki_lang_init(void);)

/* ======================= Language Init Calls ============================== */

/*
 * Call all language init functions.
 * Use this macro inside loki_lang_init() implementation.
 */
#define LOKI_LANG_INIT_ALL() \
    IF_LANG_ALDA(alda_loki_lang_init();) \
    IF_LANG_JOY(joy_loki_lang_init();) \
    IF_LANG_TR7(tr7_loki_lang_init();) \
    IF_LANG_BOG(bog_loki_lang_init();)

#endif /* LOKI_LANG_CONFIG_H */
