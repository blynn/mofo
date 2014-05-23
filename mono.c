#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "blt.h"

#define CLOSURE(x) ({void _()x _;})

#define POPZ ({ if (!stack_i) { report("stack underflow"); return; } stack[--stack_i]->z; })

#define PEEKZ ({ if (!stack_i) { report("stack underflow"); return; } stack[stack_i - 1]->z; })

enum {
  T_MPZ = 0,
};

struct val_s {
  int type;
  mpz_t z;
};
typedef struct val_s *val_ptr;

struct defn_s;
typedef struct defn_s *defn_ptr;

struct run_s {
  void (*fun)(struct run_s *);
  struct run_s *next;
  union {
    defn_ptr defn;
    char *s;
    mpz_t z;
    struct run_s *branch[3];
  };
};
typedef struct run_s *run_ptr;

struct defn_s {
  void (*fun)();  // Called during interpretation.
  void (*compile_fun)();  // Called during compilation.
  run_ptr list;  // For user-defined words.
};

void defn_clear(defn_ptr defn) {
  // TODO: Free defn->list.
  free(defn);
}

int is_num(char *c) {
  if (!*c) return 0;
  if (*c == '-') c++;
  for (; *c; c++) if (!isdigit(*c)) return 0;
  return 1;
}

int main() {
  char *errmsg = 0;
  void report(char *s) { errmsg = s; }
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

  BLT *dict = blt_new();
  int state = 0;
  char *word = 0, *c = 0;
  int get_word() {
    if (!*c) return 0;
    while (*c == ' ') c++;  // Skip whitespace.
    word = c;
    while (*c && *c != ' ') c++;  // Read word.
    if (*c) *c++ = 0;
    return *word;
  }
  void get_until_quote() {
    word = c;
    while (*c && *c != '"') c++;
    if (*c) *c++ = 0;  // EOL also terminates strings.
  }

  run_ptr *tail = 0;
  defn_ptr defn = 0;
  void go(char *line) {
    c = line;  // Initialize cursor.
    while (get_word()) {
      BLT_IT *it = blt_get(dict, word);
      if (it) {
        defn = it->data;
        if (state == 1) defn->compile_fun(); else defn->fun();
        if (errmsg) {
          puts(errmsg);
          errmsg = 0;
          return;
        }
        continue;
      }
      if (is_num(word)) {
        if (state == 1) {
          run_ptr w = malloc(sizeof(*w));
          w->fun = ({ void _(run_ptr w) {
            val_ptr r = malloc(sizeof(*r));
            r->type = T_MPZ;
            mpz_init(r->z);
            mpz_set(r->z, w->z);
            push(r);
          }_;});
          mpz_init(w->z);
          mpz_set_str(w->z, word, 0);
          w->next = 0;
          *tail = w;
          tail = &w->next;
        } else {
          val_ptr r = malloc(sizeof(*r));
          r->type = T_MPZ;
          mpz_init(r->z);
          mpz_set_str(r->z, word, 0);
          push(r);
        }
        continue;
      }
      printf("bad word '%s'\n", word);
      break;
    }
    puts(state == 1 ? " compile" : " ok");
  }

  void run_defn(run_ptr w) {
    defn_ptr old = defn;
    defn = w->defn, defn->fun(), defn = old;
  }

  void default_compile() {
    run_ptr w = malloc(sizeof(*w));
    w->fun = run_defn;
    w->defn = defn;
    w->next = 0;
    *tail = w;
    tail = &w->next;
  }

  void add_dict_full(char *word, void (*fun)(), void (*compile_fun)()) {
    defn_ptr defn = malloc(sizeof(*defn));
    defn->fun = fun;
    defn->compile_fun = compile_fun;
    blt_put(dict, word, defn);
  }

  void add_dict(char *word, void (*fun)()) {
    add_dict_full(word, fun, default_compile);
  }

  void compile_only() { report("cannot interpret"); }

  void add_dict_compile(char *word, void (*compile_fun)()) {
    add_dict_full(word, compile_only, compile_fun);
  }

  run_ptr *branch_stack = 0;  // TODO: Should be a stack.

  add_dict_compile(";", CLOSURE({
    state = 0;
    tail = 0;
    if (branch_stack) {
      report("missing then");
      branch_stack = 0;
    }
  }));

  add_dict_compile("then", CLOSURE({
    if (!branch_stack) {
      report("missing if");
      return;
    }
    tail = &branch_stack[2]->next;
    branch_stack = 0;
  }));

  add_dict_compile("else", CLOSURE({
    if (!branch_stack) {
      report("missing if");
      return;
    }
    tail = &branch_stack[0];
  }));
  void run_if(run_ptr ifw) {
    mpz_ptr z = POPZ;
    run_ptr b = ifw->branch[!!mpz_sgn(z)];
    for (run_ptr w = b; w; w = w->next) {
      w->fun(w);
      if (errmsg) return;
    }
  }

  add_dict_compile("if", CLOSURE({
    run_ptr w = malloc(sizeof(*w));
    w->fun = run_if;
    w->next = 0;
    w->branch[0] = w->branch[1] = 0;
    w->branch[2] = w;
    *tail = w;
    tail = &w->branch[1];
    branch_stack = w->branch;
  }));

  add_dict(".", CLOSURE({ gmp_printf("%Zd ", POPZ); }));

  add_dict(">", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr z = PEEKZ;
    mpz_set_ui(z, mpz_cmp(z, x) > 0);
  }));

  add_dict("dup", CLOSURE({
    if (!stack_i) {
      report("stack underflow");
      return;
    }
    push(clone(stack[stack_i - 1]));
  }));

#define DICT_MPZ(_op_, _mpz_fun_) add_dict(_op_, CLOSURE({ \
      mpz_ptr z = POPZ; \
      mpz_ptr x = PEEKZ; \
      _mpz_fun_(x, x, z); \
    }));

  DICT_MPZ("+", mpz_add);
  DICT_MPZ("-", mpz_sub);
  DICT_MPZ("*", mpz_mul);
  DICT_MPZ("/", mpz_tdiv_q);
  DICT_MPZ("mod", mpz_tdiv_r);

  void run_list() {
    for (run_ptr w = defn->list; w; w = w->next) {
      w->fun(w);
      if (errmsg) break;
    }
  }

  add_dict(":", CLOSURE({
    if (!get_word()) {
      report("empty definition");
      return;
    }
    defn_ptr newdef = malloc(sizeof(*newdef));
    newdef->fun = run_list;
    newdef->compile_fun = default_compile;
    newdef->list = 0;
    tail = &newdef->list;
    blt_put(dict, word, newdef);
    state = 1;
  }));

  add_dict_full(".\"", CLOSURE({
    get_until_quote();
    fputs(word, stdout);
  }), CLOSURE({
    run_ptr w = malloc(sizeof(*w));
    w->fun = ({void _(run_ptr w) { fputs(w->s, stdout); }_;});
    get_until_quote();
    w->s = strdup(word);
    w->next = 0;
    *tail = w;
    tail = &w->next;
  }));

  for(char *s; (s = readline("")); free(s)) if (*s) add_history(s), go(s);

  free(stack);  // TODO: Call mpz_clear.
  blt_forall(dict, ({void _(BLT_IT *it) { defn_clear(it->data); }_;}));
  blt_clear(dict);
  return 0;
}
