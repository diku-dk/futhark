#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "rts/c/util.h"
#include "rts/c/event_list.h"

int main() {
  struct kvs kvs;
  kvs_init(&kvs);

  kvs_printf(&kvs, "x", "%d", 123);
  kvs_printf(&kvs, "x", "%s", "\"foo\"");

  for (size_t i = 0; i < 128; i++) {
    kvs_printf(&kvs, "dup", "%f", rand()/1.0);
  }

  struct str_builder sb;
  str_builder_init(&sb);

  kvs_json(&kvs, &sb);
  puts(sb.str);

  kvs_log(&kvs, "", stdout);

  free(sb.str);
  kvs_free(&kvs);
}
