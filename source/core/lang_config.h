/* lang_config.h - Language configuration for psnd
 *
 * This file includes the auto-generated language configuration from CMake.
 * The generated header provides:
 *   - IF_LANG_XXX helper macros for each discovered language
 *   - Forward declarations for LokiXxxState structs
 *   - LOKI_LANG_STATE_FIELDS_GENERATED macro for EditorModel
 *   - LOKI_LANG_INIT_ALL_GENERATED macro to call all language inits
 *
 * Adding a new language requires only creating a directory under lang/
 * with a CMakeLists.txt that calls psnd_register_language().
 */

#ifndef LOKI_LANG_CONFIG_H
#define LOKI_LANG_CONFIG_H

/* Include auto-generated language configuration */
#include "lang_config_generated.h"

/* Use generated macros */
#define LOKI_LANG_STATE_FIELDS LOKI_LANG_STATE_FIELDS_GENERATED
#define LOKI_LANG_INIT_ALL() LOKI_LANG_INIT_ALL_GENERATED()

#endif /* LOKI_LANG_CONFIG_H */
