
#include "tr7.h"

/*
* facility for printing a 'value' with something 'before' and 'after'
*/
void write_value(tr7_engine_t tsc, const char *before, tr7_t value, const char *after)
{
   if (before != NULL)
      tr7_display_string(tsc, before);
   tr7_write_shared(tsc, value);
   if (after != NULL)
      tr7_display_string(tsc, after);
   tr7_flush(tsc);
}
/*
* facility for printing a new line
*/
void nl(tr7_engine_t tsc)
{
   tr7_display_string(tsc, "\n");
   tr7_flush(tsc);
}
/*
* show usage of function tr7_list_length
*/
void show_tr7_list_length(tr7_engine_t tsc)
{
   tr7_t val;
   int len;

   val = tr7_from_utf8(tsc, "(a b c)");
   len = tr7_list_length(val);
   write_value(tsc, "list-length.1: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "(a b . c)");
   len = tr7_list_length(val);
   write_value(tsc, "list-length.2: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "#8=(a b . #8#)");
   len = tr7_list_length(val);
   write_value(tsc, "list-length.3: ", TR7_FROM_INT(len), "\n");
}
/*
* show usage of function tr7_unsafe_list_length
*/
void show_tr7_unsafe_list_length(tr7_engine_t tsc)
{
   tr7_t val;
   int len;

   val = tr7_from_utf8(tsc, "(a b c)");
   len = tr7_unsafe_list_length(val);
   write_value(tsc, "unsafe-list-length.1: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "(a b . c)");
   len = tr7_unsafe_list_length(val);
   write_value(tsc, "unsafe-list-length.2: ", TR7_FROM_INT(len), "\n");
}
/*
* show usage of function tr7_reverse_in_place
*/
void show_tr7_reverse_in_place(tr7_engine_t tsc)
{
   tr7_t val, x, y;

   x = tr7_from_utf8(tsc, "(a b c)");
   y = tr7_from_utf8(tsc, "(A B C)");
   val = tr7_reverse_in_place(x, y);
   write_value(tsc, "reverse-in-place.1: ", val, "\n");

   x = tr7_from_utf8(tsc, "(a b . c)");
   y = tr7_from_utf8(tsc, "(A)");
   val = tr7_reverse_in_place(x, y);
   write_value(tsc, "reverse-in-place.2: ", val, "\n");

   x = tr7_from_utf8(tsc, "(a . #1=(b c . #1#))");
   y = tr7_from_utf8(tsc, "(X Y)");
   val = tr7_reverse_in_place(x, y);
   write_value(tsc, "reverse-in-place.3: ", val, "\n");
}
/*
* show usage of function tr7_append
*/
void show_tr7_append(tr7_engine_t tsc)
{
   tr7_t val, items[] = {
         tr7_from_utf8(tsc, "#1=(A B C . #1#)"),
         tr7_from_utf8(tsc, "(A B C)"),
         tr7_from_utf8(tsc, "(M N O)"),
         tr7_from_utf8(tsc, "(X Y Z)"),
         tr7_from_int(tsc, 345) };

   val = tr7_append(tsc, 2, &items[1]);
   write_value(tsc, "append.1: ", val, "\n");

   val = tr7_append(tsc, 3, &items[2]);
   write_value(tsc, "append.2: ", val, "\n");

   val = tr7_append(tsc, 5, &items[0]);
   write_value(tsc, "append.3: ", val, "\n");
}
/*
* show usage of function tr7_get_list_pairs
*/
void show_tr7_get_list_pairs(tr7_engine_t tsc)
{
   const int size = 6;
   tr7_pair_t pairs[size];
   tr7_t val;
   int len;

   val = tr7_from_utf8(tsc, "1");
   len = tr7_get_list_pairs(val, size, pairs);
   write_value(tsc, "get-list-pairs.1: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "(A B C)");
   len = tr7_get_list_pairs(val, size, pairs);
   write_value(tsc, "get-list-pairs.2: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "(A B C)");
   len = tr7_get_list_pairs(val, 2, pairs);
   write_value(tsc, "get-list-pairs.3: ", TR7_FROM_INT(len), "\n");

   val = tr7_from_utf8(tsc, "(A B . C)");
   len = tr7_get_list_pairs(val, size, pairs);
   write_value(tsc, "get-list-pairs.4: ", TR7_FROM_INT(len), "\n");
}
/*
* show usage of function tr7_get_list_cars
*/
void show_tr7_get_list_cars(tr7_engine_t tsc)
{
   static const int size = 6;
   static const struct { const char *const expr; const int nbr; } inputs[] = {
      { "1", size },
      { "(A B C)", size },
      { "(A B C)", 2 },
      { "(A B . C)", size } };

   tr7_t cars[size], cdr, val;
   int iin, idx, len;

   for (iin = 0 ; iin < (int)(sizeof inputs / sizeof *inputs) ; iin++) {
      val = tr7_from_utf8(tsc, inputs[iin].expr);
      len = tr7_get_list_cars(val, inputs[iin].nbr, cars, &cdr);
      write_value(tsc, "get-list-cars.", TR7_FROM_INT(iin), NULL);
      write_value(tsc, ": ", TR7_FROM_INT(len), NULL);
      for (idx = 0 ; idx < len ; idx++) {
         write_value(tsc, ", CAR[", TR7_FROM_INT(idx), "]=");
         write_value(tsc, NULL, cars[idx], NULL);
      }
      write_value(tsc, ", CDR=", cdr, "\n");
   }
}
/*
* MAIN
*/
#include <locale.h>

int main(int argc, char **argv)
{
   tr7_engine_t tsc;

   setlocale(LC_ALL, "");
   setlocale(LC_NUMERIC, "C");
   tsc = tr7_engine_create(NULL);
   if (!tsc) {
      fprintf(stderr, "Could not initialize!\n");
      return 2;
   }
   tr7_load_string(tsc, "(import (scheme base)"
                        "(scheme read)"
                        "(scheme write)"
                        "(scheme file)"
                        "(scheme load)"
                        "(scheme eval)"
                        "(scheme process-context))");
   tr7_set_standard_ports(tsc);
   tr7_set_argv(argv);
   show_tr7_list_length(tsc);
   nl(tsc);
   show_tr7_unsafe_list_length(tsc);
   nl(tsc);
   show_tr7_reverse_in_place(tsc);
   nl(tsc);
   show_tr7_append(tsc);
   nl(tsc);
   show_tr7_get_list_pairs(tsc);
   nl(tsc);
   show_tr7_get_list_cars(tsc);
   tr7_engine_destroy(tsc);

   return 0;
}

/*
Local variables:
c-file-style: "k&r"
End:
vim: noai ts=3 sw=3 expandtab
*/
