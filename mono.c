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

#define PEEPZ ({ if (!stack_i) { report("stack underflow"); return; } stack[stack_i - 1]; })

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
    struct run_s *branch[2];
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

  void run_int(run_ptr w) { push(w->z); }

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
          RUN_NEW_FUN(w, run_int);
          mpz_init(w->z);
          mpz_set_str(w->z, word, 0);
          tail = &w->next;
        } else {
          grow();
          mpz_set_str(PEEPZ, word, 0);
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

    // TODO: Reduce waste.
    char *upper = strdup(word);
    for (char *c = upper; *c; c++) *c = toupper(*c);
    if (strcmp(upper, word)) {
      defn_ptr defn2 = malloc(sizeof(*defn));
      defn2->fun = fun;
      defn2->compile_fun = compile_fun;
      blt_put(dict, upper, defn2);
    }
    free(upper);
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
  list_ptr ij_stack = 0;
#define POP_BRANCH_STACK ({ \
  if (!branch_stack) THROW("control stack underflow"); \
  run_ptr r = branch_stack->data; branch_stack = branch_stack->next; r; })

  add_dict_compile(";", CLOSURE({
    state = 0;
    tail = 0;
    if (branch_stack) {
      branch_stack = 0;  // LEAK
      THROW("control stack garbage");
    }
  }));

  void run_list(run_ptr w) {
    while(w) {
      w->fun(w);
      if (errmsg) break;
      w = w->next;
    }
  }

  void run_if(run_ptr w) {
    mpz_ptr z = POPZ;
    run_list(w->branch[!!mpz_sgn(z)]);
  }

  add_dict_compile("if", CLOSURE({
    RUN_NEW_FUN(w, run_if);
    w->branch[0] = w->branch[1] = 0;
    tail = &w->branch[1];
    list_ptr p = malloc(sizeof(*p));
    p->next = branch_stack;
    p->data = w;
    branch_stack = p;
  }));
  add_dict_compile("then", CLOSURE({
    run_ptr w = POP_BRANCH_STACK;
    if (w->fun != run_if) {
      errmsg = "control stack error";
      return;
    }
    tail = &w->next;
  }));
  add_dict_compile("else", CLOSURE({
    run_ptr w = POP_BRANCH_STACK;
    if (w->fun != run_if) {
      errmsg = "control stack error";
      return;
    }
    tail = &w->branch[0];
  }));

  void run_do(run_ptr w) {
    mpz_ptr x = POPZ;
    mpz_ptr y = POPZ;
    mpz_t ind, lim;
    mpz_init_set(ind, x);
    mpz_init_set(lim, y);

    list_ptr p = malloc(sizeof(*p));
    p->next = ij_stack;
    p->data = ind;
    ij_stack = p;
    if (w->branch[1]) {  // +LOOP.
      mpz_mul_ui(lim, lim, 2);
      mpz_sub_ui(lim, lim, 1);
      int cont = 0;
      mpz_t tmp;
      mpz_init(tmp);
      do {
        run_list(w->branch[0]);
        mpz_ptr inc = POPZ;
        mpz_mul_ui(tmp, ind, 2);
        cont = mpz_cmp(tmp, lim);
        mpz_add(ind, ind, inc);
        mpz_mul_ui(tmp, ind, 2);
        cont += mpz_cmp(tmp, lim);
      } while (!errmsg && cont);
    } else {  // LOOP.
      do {
        run_list(w->branch[0]);
        mpz_add_ui(ind, ind, 1);
      } while (!errmsg && mpz_cmp(ind, lim));
    }
  }

  add_dict_compile("do", CLOSURE({
    RUN_NEW_FUN(w, run_do);
    w->branch[0] = w->branch[1] = 0;
    tail = &w->branch[0];

    list_ptr p = malloc(sizeof(*p));
    p->next = branch_stack;
    p->data = w;
    branch_stack = p;
  }));
  add_dict_compile("loop", CLOSURE({
    run_ptr w = POP_BRANCH_STACK;
    if (w->fun != run_do) {
      errmsg = "control stack error";
      return;
    }
    tail = &w->next;
  }));
  add_dict_compile("+loop", CLOSURE({
    run_ptr w = POP_BRANCH_STACK;
    if (w->fun != run_do) {
      errmsg = "control stack error";
      return;
    }
    w->branch[1] = (void *) 1;
    tail = &w->next;
  }));

  void run_i(run_ptr unused) {
    if (!ij_stack) THROW("control stack underflow");
    push(ij_stack->data);
  }

  void run_j(run_ptr unused) {
    if (!ij_stack || !ij_stack->next) THROW("control stack underflow");
    push(ij_stack->next->data);
  }

  add_dict_compile("i", CLOSURE({
    RUN_NEW_FUN(w, run_i);
    tail = &w->next;
  }));

  add_dict_compile("j", CLOSURE({
    RUN_NEW_FUN(w, run_j);
    tail = &w->next;
  }));

  add_dict(".", CLOSURE({ gmp_printf("%Zd ", POPZ); }));

  add_dict("cr", CLOSURE({ putchar('\n'); }));

  mpz_t z_emit;
  mpz_init(z_emit);
  add_dict("emit", CLOSURE({
    mpz_set_ui(z_emit, 255);
    mpz_and(z_emit, z_emit, POPZ);
    putchar((char) mpz_get_d(z_emit));
  }));

  add_dict("drop", CLOSURE({ POPZ; }));

  add_dict("dup", CLOSURE({ push(PEEPZ); }));

  add_dict("swap", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr y = PEEPZ;
    mpz_swap(x, y);
    grow();
  }));

  add_dict("rot", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr y = POPZ;
    mpz_ptr z = PEEPZ;
    mpz_swap(x, z);
    mpz_swap(y, z);
    grow();
    grow();
  }));

#define DICT_MPZ(_op_, _mpz_fun_) add_dict(_op_, CLOSURE({ \
      mpz_ptr z = POPZ; mpz_ptr x = PEEPZ; _mpz_fun_(x, x, z); }));

  DICT_MPZ("+", mpz_add);
  DICT_MPZ("-", mpz_sub);
  DICT_MPZ("*", mpz_mul);
  DICT_MPZ("/", mpz_tdiv_q);
  DICT_MPZ("mod", mpz_tdiv_r);
  DICT_MPZ("and", mpz_and);
  DICT_MPZ("or", mpz_ior);
  DICT_MPZ("xor", mpz_xor);

  add_dict(">", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr z = PEEPZ;
    mpz_set_ui(z, mpz_cmp(z, x) > 0);
  }));

  add_dict("=", CLOSURE({
    mpz_ptr x = POPZ;
    mpz_ptr z = PEEPZ;
    mpz_set_ui(z, !mpz_cmp(z, x));
  }));

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
  mpz_clear(z_emit);
  return 0;
}
