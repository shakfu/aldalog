#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <time.h>
#include <wchar.h>
#include <locale.h>
#include <signal.h>
#ifdef _WIN32
#  define snprintf _snprintf
#endif

#include "tr7.h"

#define INITVAR        "TR7INIT"

#ifndef INITFILE
#define INITFILE       "tr7-init.scm"
#endif

#ifndef DEFPATH
#define DEFPATH        NULL
#endif

#ifndef DEFLIBPATH
#define DEFLIBPATH        DEFPATH
#endif

#ifndef DEFINCPATH
#define DEFINCPATH        DEFPATH
#endif

#ifndef DEFEXTPATH
#define DEFEXTPATH        DEFPATH
#endif

#ifndef VERSION
#define VERSION        "?.?.?"
#endif

#define BANNER         "Tiny R7RS interpreter TR7 " VERSION "\n"
#define PROMPT         "tr7> "

#ifndef PATHVAR
#define PATHVAR        "TR7_PATH"        /* for load (and fallback) */
#endif
#ifndef LIBPATHVAR
#define LIBPATHVAR     "TR7_LIB_PATH"    /* for import */
#endif
#ifndef INCPATHVAR
#define INCPATHVAR     "TR7_INC_PATH"    /* for include */
#endif
#ifndef EXTPATHVAR
#define EXTPATHVAR     "TR7_EXT_PATH"    /* for load extension */
#endif
#ifndef PROMPTVAR
#define PROMPTVAR      "TR7_PROMPT"      /* for the prompt string */
#endif

static tr7_C_return_t shell(tr7_engine_t tsc, int nargs, const tr7_t *args, void *closure)
{
   int status = system(tr7_string_buffer(args[0]));
   return tr7_C_return_single(tsc, status == 0 ? TR7_TRUE : TR7_FALSE);
}

static const tr7_C_func_def_t cexports[] = {
   {  "shell", shell, NULL, TR7ARG_STRING, 1, 1 }
};

static void init(tr7_engine_t tsc)
{
   tr7_lib_register_C_functions(tsc, TR7_FOREIGNS_LIBNAME, cexports, sizeof cexports / sizeof *cexports);
   tr7_import_lib(tsc, TR7_FOREIGNS_LIBNAME);
}

static void addenvstr(tr7_engine_t tsc,
                      tr7_strid_t strid,
                      const char *varname,
                      const char *defval)
{
   const char *val = getenv(varname);
   tr7_set_string(tsc, strid, val ? val : defval);
}

int main(int argc, char **argv)
{
   tr7_engine_t tsc;
   tr7_play_t playopt = Tr7_Play_Show_Errors;
   int retcode;
   int isfile = 1;
   char *args[2] = { NULL, NULL };

   setlocale(LC_ALL, "");
   setlocale(LC_NUMERIC, "C");
   if (argc == 2 && strcmp(argv[1], "-?") == 0) {
      printf("Usage: %s -?\n", argv[0]);
      printf("or:    %s [-k] [<file1> <file2> ...]\n", argv[0]);
      printf("followed by\n");
      printf("          -1 <file> [<arg1> <arg2> ...]\n");
      printf("          -c <Scheme commands> [<arg1> <arg2> ...]\n");
      printf("option -k is for keeping run on error\n");
      printf("Use - as filename for stdin.\n");
      return 0;
   }
   tsc = tr7_engine_create(NULL);
   if (!tsc) {
      fprintf(stderr, "Could not initialize!\n");
      return 2;
   }
   tr7_import_lib(tsc, "scheme/base");
   tr7_import_lib(tsc, "scheme/read");
   tr7_import_lib(tsc, "scheme/write");
   tr7_import_lib(tsc, "scheme/file");
   tr7_import_lib(tsc, "scheme/load");
   tr7_import_lib(tsc, "scheme/eval");
   tr7_import_lib(tsc, "scheme/process-context");
   tr7_set_standard_ports(tsc);
   tr7_set_argv(argv);
   addenvstr(tsc, Tr7_StrID_Path, PATHVAR, DEFPATH);
   addenvstr(tsc, Tr7_StrID_Library_Path, LIBPATHVAR, DEFLIBPATH);
   addenvstr(tsc, Tr7_StrID_Include_Path, INCPATHVAR, DEFINCPATH);
   addenvstr(tsc, Tr7_StrID_Extension_Path, EXTPATHVAR, DEFEXTPATH);
   addenvstr(tsc, Tr7_StrID_Prompt, PROMPTVAR, PROMPT);
   init(tsc);
   if (argc > 1 && strcmp(argv[1], "-k") == 0) {
      int i = 1;
      for (; i < argc ; i++)
         argv[i] = argv[i+1];
      argc--;
      playopt |= Tr7_Play_Keep_Playing;
   }
   if (argc == 1)
      tr7_display_string(tsc, BANNER);
   tr7_load_file(tsc, NULL, getenv(INITVAR) != NULL ? getenv(INITVAR) : INITFILE);
   retcode = 1;
   if (argc == 1) {
      args[0] = "<interactive>";
      tr7_set_argv(args);
      retcode = tr7_play_file(tsc, stdin, "<stdin>", Tr7_Play_Interactive);
   }
   for (argv++ ; retcode && *argv != NULL ; argv++) {
      if (strcmp(*argv, "-") == 0) {
         args[0] = "<stdin>";
         tr7_set_argv(args);
         retcode = tr7_play_file(tsc, stdin, "<stdin>", playopt);
      }
      else if (strcmp(*argv, "-1") != 0 && strcmp(*argv, "-c") != 0) {
         args[0] = *argv;
         tr7_set_argv(args);
         retcode = tr7_play_file(tsc, NULL, *argv, playopt);
      }
      else {
         isfile = (*argv++)[1] == '1';
         if (*argv == NULL) {
            fprintf(stderr, "no argument for %s!\n", *--argv);
            return 3;
         }
         tr7_set_argv(argv);
         if (isfile)
            retcode = tr7_play_file(tsc, NULL, *argv, playopt);
         else
            retcode = tr7_play_string(tsc, *argv, playopt);
         break;
      }
   }
   tr7_engine_destroy(tsc);

   return !retcode;
}

/*
Local variables:
c-file-style: "k&r"
End:
vim: noai ts=3 sw=3 expandtab
*/
