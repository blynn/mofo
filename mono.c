// = MonochromeForth =
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "blt.h"

enum {
  T_MPZ = 0,
  T_STRING,
  T_CORE,
  T_LIST,
  T_FUN,
};

struct val_s {
  int type;
  mpz_t z;
};
typedef struct val_s *val_ptr;

struct defn_s;
typedef struct defn_s *defn_ptr;

struct word_list_s {
  int type;
  defn_ptr defn;
  char *s;
  mpz_t z;
  struct word_list_s *next;
};
typedef struct word_list_s *word_list_ptr;

struct defn_s {
  int type;
  void (*fun)();
  word_list_ptr list;
};

int is_num(char *s) {
  char *c = s;
  if (!*c) return 0;
  if (*c == '-') c++;
  for (; *c; c++) if (!isdigit(*c)) return 0;
  return 1;
}

val_ptr new_z(char *s) {
  if (!is_num(s)) {
    puts("bug: expected number");
    exit(1);
  }
  val_ptr r = malloc(sizeof(*r));
  r->type = T_MPZ;
  mpz_init(r->z);
  mpz_set_str(r->z, s, 0);
  return r;
}

int main() {
  char *errmsg = 0;
  int stack_i = 0, stack_max = 8;
  val_ptr *stack = malloc(sizeof(*stack) * stack_max);
  void push(val_ptr v) {
    if (stack_i == stack_max) {
      stack_max *= 2;
      stack = realloc(stack, sizeof(*stack) * stack_max);
    }
    stack[stack_i++] = v;
  }
  val_ptr clone(val_ptr v) {
    val_ptr r = malloc(sizeof(*r));
    mpz_init(r->z);
    mpz_set(r->z, v->z);
    return r;
  }

  void report(char *s) { errmsg = s; }

  BLT *dict = blt_new();
  void add_dict(char *word, void (*fun)()) {
    defn_ptr defn = malloc(sizeof(*defn));
    defn->type = T_CORE;
    defn->fun = fun;
    blt_put(dict, word, defn);
  }

  add_dict(".", ({void _() {
    if (!stack_i) {
      report("stack underflow");
      return;
    }
    gmp_printf("%Zd ", stack[--stack_i]->z);
  }_;}));

  add_dict("0>", ({void _() {
    if (!stack_i) {
      report("stack underflow");
      return;
    }
    mpz_set_si(stack[stack_i - 1]->z,
        mpz_cmp_ui(stack[stack_i - 1], 0) > 0 ? 1 : 0);
  }_;}));

  add_dict("dup", ({void _() {
    if (!stack_i) {
      report("stack underflow");
      return;
    }
    push(clone(stack[stack_i - 1]));
  }_;}));

  add_dict("+", ({void _() {
    if (stack_i <= 1) {
      report("stack underflow");
      return;
    }
    stack_i--;
    mpz_add(stack[stack_i - 1]->z, stack[stack_i - 1]->z, stack[stack_i]->z);
  }_;}));

  int state = 0, atEOL = 0;
  char *word = 0, *c = 0;
  void end_word() {
    if (*c) *c = 0; else atEOL = 1;
    c++;
  }
  int get_word() {
    if (atEOL) return 0;
    while (*c == ' ') c++;  // Skip whitespace.
    word = c;
    while (*c && *c != ' ') c++;  // Read word.
    end_word();
    return 1;
  }
  void get_until_quote() {
    word = c;
    while (*c && *c != '"') c++;
    if (!*c) {
      atEOL = 1;  // Missing quote, but who's counting?
    } else {
      *c++ = 0;
      if (!*c) atEOL = 1;
    }
  }

  word_list_ptr *tail = 0;
  void go(char *line) {
    state = 0;
    c = line;  // Initialize cursor.
    atEOL = 0;
    while (get_word()) {
      if (!strcmp(word, ";")) {
        if (state == 1) {
          state = 0;
          tail = 0;
          continue;
        } else {
          puts("unexpected ;");
          return;
        }
      }
      BLT_IT *it = blt_get(dict, word);
      if (!it) {
        if (is_num(word)) {
          if (state == 1) {
            word_list_ptr w = malloc(sizeof(*w));
            w->type = T_MPZ;
            mpz_init(w->z);
            mpz_set_str(w->z, word, 0);
            w->next = 0;
            *tail = w;
            tail = &w->next;
          } else {
            push(new_z(word));
          }
          continue;
        }
        printf("bad word '%s'\n", word);
        return;
      }
      defn_ptr defn = it->data;
      if (state == 1) {
        word_list_ptr w = malloc(sizeof(*w));
        if (!strcmp(word, ".\"")) {
          w->type = T_STRING;
          get_until_quote();
          w->s = strdup(word);
        } else {
          w->type = T_FUN;
          w->defn = defn;
        }
        w->next = 0;
        *tail = w;
        tail = &w->next;
      } else {
        switch (defn->type) {
        case T_CORE:
          defn->fun();
          break;
        case T_LIST:
          for (word_list_ptr w = defn->list; w; w = w->next) {
            switch (w->type) {
            case T_MPZ:
              {
                val_ptr r = malloc(sizeof(*r));
                r->type = T_MPZ;
                mpz_init(r->z);
                mpz_set(r->z, w->z);
                push(r);
              }
              break;
            case T_FUN:
              w->defn->fun();
              break;
            case T_STRING:
              fputs(w->s, stdout);
              break;
            default:
              puts("unhandled");
              return;
            }
            if (errmsg) {
              puts(errmsg);
              errmsg = 0;
              return;
            }
          }
          break;
        default:
          puts("unhandled");
          return;
        }
        if (errmsg) {
          puts(errmsg);
          errmsg = 0;
          return;
        }
      }
    }
  }

  add_dict(":", ({void _() {
    if (!get_word()) {
      report("empty definition");
      return;
    }
    defn_ptr defn = malloc(sizeof(*defn));
    defn->type = T_LIST;
    defn->list = 0;
    tail = &defn->list;
    blt_put(dict, word, defn);
    state = 1;
  }_;}));

  add_dict(".\"", ({void _() {
    get_until_quote();
    fputs(word, stdout);
  }_;}));

  for(char *s; (s = readline("")); free(s)) if (*s) add_history(s), go(s);
  return 0;
}
