#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "blt.h"

#define CLOSURE(_body_) ({void _()_body_ _;})

#define RUN_NEW(_var_, _body_) run_ptr _var_ = ({ \
  run_ptr r = malloc(sizeof(*r)); \
  void _(run_ptr _var_) _body_ r->fun = _; \
  *tail = r; r->next = 0; r; })

#define RUN_NEW_FUN(_var_, _fun_) run_ptr _var_ = ({ \
  run_ptr r = malloc(sizeof(*r)); \
  r->fun = _fun_; \
  *tail = r; r->next = 0; r; })

#define POPZ ({ if (!stack_i) { report("stack underflow"); return; } stack[--stack_i]; })

#define PEEKZ ({ if (!stack_i) { report("stack underflow"); return; } stack[stack_i - 1]; })

#define THROW(_msg_) { report(_msg_); return; }

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
  int stack_i = 0, stack_max = 8, stack_record = 0;
  mpz_t *stack = malloc(sizeof(*stack) * stack_max);
  void grow() {
    if (stack_i == stack_max) {
      stack = realloc(stack, sizeof(*stack) * (stack_max *= 2));
    }
    if (stack_i++ == stack_record) mpz_init(stack[stack_record++]);
  }
  void push(mpz_ptr v) {
    grow();
    mpz_set(stack[stack_i - 1], v);
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
        if (errmsg) break;
      } else if (is_num(word)) {
        if (state == 1) {
          RUN_NEW(w, { push(w->z); });
          mpz_init(w->z);
          mpz_set_str(w->z, word, 0);
          tail = &w->next;
        } else {
          grow();
          mpz_set_str(PEEKZ, word, 0);
        }
      } else {
        errmsg = "bad word";
        break;
      }
    }
    if (errmsg) {
      puts(errmsg);
      errmsg = 0;
      return;
    }
    puts(state == 1 ? " compile" : " ok");
  }

  void run_defn(run_ptr w) {
    defn_ptr old = defn;
    defn = w->defn, defn->fun(), defn = old;
  }

  void default_compile() {
    RUN_NEW_FUN(w, run_defn);
    w->defn = defn;
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

  void compile_only() { THROW("cannot interpret"); }

  void add_dict_compile(char *word, void (*compile_fun)()) {
    add_dict_full(word, compile_only, compile_fun);
  }

  struct list_s {
    struct list_s *next;
    void *data;
  };
  typedef struct list_s *list_ptr;
  list_ptr branch_stack = 0;
#define POP_BRANCH_STACK ({ if (!branch_stack) THROW("branch underflow"); \
  run_ptr *r = branch_stack->data; branch_stack = branch_stack->next; r; })

  add_dict_compile(";", CLOSURE({
    state = 0;
    tail = 0;
    if (branch_stack) {
      branch_stack = 0;  // LEAK
      THROW("missing then");
    }
  }));

  add_dict_compile("then", CLOSURE({ tail = &POP_BRANCH_STACK[2]->next; }));
  add_dict_compile("else", CLOSURE({ tail = &POP_BRANCH_STACK[0]; }));

  void run_list(run_ptr w) {
    while(w) {
      w->fun(w);
      if (errmsg) break;
      w = w->next;
    }
  }

  void run_if(run_ptr ifw) {
    mpz_ptr z = POPZ;
    run_list(ifw->branch[!!mpz_sgn(z)]);
  }

  add_dict_compile("if", CLOSURE({
    RUN_NEW_FUN(w, run_if);
    w->branch[0] = w->branch[1] = 0;
    w->branch[2] = w;
    tail = &w->branch[1];
    list_ptr p = malloc(sizeof(*p));
    p->next = branch_stack;
    p->data = w->branch;
    branch_stack = p;
  }));

  add_dict(".", CLOSURE({ gmp_printf("%Zd ", POPZ); }));

  add_dict(">", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr z = PEEKZ;
    mpz_set_ui(z, mpz_cmp(z, x) > 0);
  }));

  add_dict("dup", CLOSURE({ push(PEEKZ); }));

#define DICT_MPZ(_op_, _mpz_fun_) add_dict(_op_, CLOSURE({ \
      mpz_ptr z = POPZ; mpz_ptr x = PEEKZ; _mpz_fun_(x, x, z); }));

  DICT_MPZ("+", mpz_add);
  DICT_MPZ("-", mpz_sub);
  DICT_MPZ("*", mpz_mul);
  DICT_MPZ("/", mpz_tdiv_q);
  DICT_MPZ("mod", mpz_tdiv_r);

  void run_defn_list() { run_list(defn->list); }

  add_dict(":", CLOSURE({
    if (!get_word()) THROW("empty definition");
    defn_ptr newdef = malloc(sizeof(*newdef));
    newdef->fun = run_defn_list;
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
    get_until_quote();
    RUN_NEW(w, { fputs(w->s, stdout); });
    w->s = strdup(word);
    tail = &w->next;
  }));

  for(char *s; (s = readline("")); free(s)) if (*s) add_history(s), go(s);

  for(int i = 0; i < stack_record; i++) mpz_clear(stack[i]);
  free(stack);
  blt_forall(dict, ({void _(BLT_IT *it) { defn_clear(it->data); }_;}));
  blt_clear(dict);
  return 0;
}
