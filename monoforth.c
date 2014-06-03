#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include "blt.h"

#define ANON(_body_) ({void _()_body_ _;})

#define ABORT(_msg_) ({ puts(_msg_); stack_reset(dstack); ip = quit_loop_hack; return; })

#define POPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_pop(dstack); })

#define PEEPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_peep(dstack); })

struct defn_s {
  char immediate, compile_only;
  void **cell;
  int n;
};
typedef struct defn_s *defn_ptr;

void **defn_grow(defn_ptr defn) {
  defn->cell = realloc(defn->cell, ++defn->n * sizeof(void *));
  return &defn->cell[defn->n - 1];
}

mpz_ptr mpz_new() {
  mpz_ptr r = malloc(sizeof(mpz_t));
  mpz_init(r);
  return r;
}

mpz_ptr mpz_dup(mpz_ptr z) {
  mpz_ptr r = malloc(sizeof(mpz_t));
  mpz_init_set(r, z);
  return r;
}

void mpz_free(mpz_ptr p) {
  mpz_clear(p);
  free(p);
}

int is_num(char *c) {
  if (!*c) return 0;
  if (*c == '-') c++;
  for (; *c; c++) if (!isdigit(*c)) return 0;
  return 1;
}

void toutf8(void (*f)(char), uint32_t x) {
  if (x < 0x80) {
    f(x);
    return;
  }
  if (x < 0x800) {
    f((x>>6) | 0xC0), f((x & 0x3F) | 0x80);
    return;
  }
  if (x < 0x10000) {
    f((x>>12) | 0xE0), f(((x>>6) & 0x3F) | 0x80), f((x & 0x3F) | 0x80);
    return;
  }
  if (x < 0x110000) {
    f((x>>18) | 0xF0);
    f(((x>>12) & 0x3F) | 0x80);
    f(((x>>6) & 0x3F) | 0x80);
    f((x & 0x3F) | 0x80);
    return;
  }
}

struct stack_t {
  int i, max, record;
  void **p;
  int is_mpz;
};
typedef struct stack_t *stack_ptr;

stack_ptr stack_new() {
  stack_ptr r = malloc(sizeof(*r));
  r->record = r->i = 0, r->max = 8;
  r->p = malloc(sizeof(*r->p) * r->max);
  r->is_mpz = 0;
  return r;
}

stack_ptr stack_new_mpz() {
  stack_ptr r = stack_new();
  r->is_mpz = 1;
  return r;
}

void stack_reset(stack_ptr st) { st->i = 0; }

void stack_free(stack_ptr st) {
  if (st->is_mpz) for(int i = 0; i < st->record; i++) mpz_free(st->p[i]);
}

void **stack_grow(stack_ptr st) {
  if (st->i == st->max) st->p = realloc(st->p, sizeof(*st->p) * (st->max *= 2));
  if (st->i++ == st->record) {
    if (st->is_mpz) st->p[st->record] = mpz_new();
    st->record++;
  }
  return st->p + st->i - 1;
}

int stack_empty(stack_ptr st) { return !st->i; }

void *stack_peep(stack_ptr st) { return st->p[st->i - 1]; }

void *stack_pop(stack_ptr st) { return st->p[--st->i]; }

void stack_push(stack_ptr st, void *p) { *stack_grow(st) = p; }

void stack_push_mpz(stack_ptr st, mpz_ptr v) {
  if (!st->is_mpz) fprintf(stderr, "BUG!\n"), exit(1);
  mpz_set(*stack_grow(st), v);
}

int main() {
  void **quit_loop_hack;
  defn_ptr interpret_def;
  BLT *dict = blt_new();
  stack_ptr dstack = stack_new_mpz();
  stack_ptr ijstack = stack_new_mpz();
  stack_ptr rstack = stack_new();
  int state = 0;
  char *word = 0, *cursor = 0;
  int get_word() {
    if (!*cursor) return 0;
    while (*cursor == ' ') cursor++;  // Skip whitespace.
    word = cursor;
    while (*cursor && *cursor != ' ') cursor++;  // Read word.
    if (*cursor) *cursor++ = 0;
    return *word;
  }
  void get_until(char x) {
    word = cursor;
    while (*cursor && *cursor != x) cursor++;
    if (*cursor) *cursor++ = 0;  // EOL also terminates the search.
  }

  void get_until_quote() { get_until('"'); }
  void get_until_rparen() { get_until(')'); }

  void **ip = 0;

  void *run_literal[] = {ANON({ stack_push_mpz(dstack, *ip++); })};

  void *run_exit[] = {ANON({ ip = stack_pop(rstack); })};

  void *run_jmp[] = {ANON({ ip += (intptr_t) *ip; })};

  void *run_jz[] = {ANON({
    mpz_ptr z = POPZ;
    ip += mpz_sgn(z) ? 1 : (intptr_t) *ip ;
  })};

  defn_ptr curdef = 0;
  void compile(void *p) { *defn_grow(curdef) = p; }

  defn_ptr new_defn(char immediate, char compile_only) {
    defn_ptr r = malloc(sizeof(*r));
    r->compile_only = compile_only;
    r->immediate = immediate;
    r->n = 0;
    r->cell = 0;
    return r;
  }

  void add_dict_full(char *word, void (*fun)(),
      char immediate, char compile_only) {
    curdef = new_defn(immediate, compile_only);
    compile(fun);
    blt_put(dict, word, curdef);

    char *upper = strdup(word);
    for (char *c = upper; *c; c++) *c = toupper(*c);
    if (strcmp(upper, word)) blt_put(dict, upper, curdef);
    free(upper);
  }

  void add_dict(char *word, void (*fun)()) {
    add_dict_full(word, fun, 0, 0);
  }

  void add_dict_compile(char *word, void (*fun)()) {
    add_dict_full(word, fun, 1, 1);
  }

  add_dict("here", ANON({
    if (!curdef) ABORT("TODO: memory mapper");
    mpz_set_ui(*stack_grow(dstack), curdef->n);
  }));

  add_dict_full("(jmp)", ANON({
    compile(run_jmp);
    intptr_t offset = mpz_get_ui(POPZ);
    compile((void *) (offset - curdef->n));
  }), 0, 1);

  add_dict_full("(jz)", ANON({
    compile(run_jz);
    intptr_t offset = mpz_get_ui(POPZ);
    compile((void *) (offset - curdef->n));
  }), 0, 1);

  add_dict_compile("if", ANON({
    compile(run_jz);
    mpz_set_ui(*stack_grow(dstack), curdef->n);
    compile(0);
  }));
  add_dict_compile("else", ANON({
    intptr_t offset = mpz_get_ui(POPZ);
    compile(run_jmp);
    mpz_set_ui(*stack_grow(dstack), curdef->n);
    compile(0);
    curdef->cell[offset] = (void *) (curdef->n - offset);
  }));
  add_dict_compile("then", ANON({
    intptr_t offset = mpz_get_ui(POPZ);
    curdef->cell[offset] = (void *) (curdef->n - offset);
  }));

  add_dict_full(">r", ANON({
    mpz_ptr z = POPZ;
    stack_push_mpz(ijstack, z);
  }), 0, 1);

  add_dict_full("r>", ANON({
    stack_push_mpz(dstack, stack_pop(ijstack));
  }), 0, 1);

  add_dict_full("r@", ANON({
    stack_push_mpz(dstack, stack_peep(ijstack));
  }), 0, 1);

  void *run_loop_check[] = {ANON({
    int i = ijstack->i;
    mpz_ptr index = ijstack->p[i - 1];
    mpz_ptr limit = ijstack->p[i - 2];
    mpz_add_ui(index, index, 1);
    mpz_set_si(*stack_grow(dstack), -!mpz_cmp(index, limit));
  })};

  void *run_ploop_check[] = {ANON({
    int i = ijstack->i;
    mpz_ptr index = ijstack->p[i - 1];
    mpz_ptr limit = ijstack->p[i - 2];
    mpz_ptr z = *stack_grow(dstack);
    mpz_mul_ui(z, limit, 2);
    mpz_sub_ui(z, z, 1);
    mpz_ptr twice = *stack_grow(dstack);
    mpz_mul_ui(twice, index, 2);
    dstack->i -= 2;
    mpz_ptr inc = PEEPZ;

    int cont = mpz_cmp(twice, z);
    mpz_add(index, index, inc);
    mpz_mul_ui(twice, index, 2);
    cont *= mpz_cmp(twice, z);
    mpz_set_si(inc, cont < 0);
  })};

  void *run_leave[] = {ANON({
    stack_pop(ijstack);
    stack_pop(ijstack);
  })};

  add_dict_compile("loop", ANON({
    compile(run_loop_check);
    compile(run_jz);
    intptr_t offset = mpz_get_ui(POPZ);
    compile((void *) (offset - curdef->n));
    compile(run_leave);
  }));

  add_dict_compile("+loop", ANON({
    compile(run_ploop_check);
    compile(run_jz);
    intptr_t offset = mpz_get_ui(POPZ);
    compile((void *) (offset - curdef->n));
    compile(run_leave);
  }));

  add_dict_compile("leave", ANON({
    compile(run_leave);
    POPZ;
  }));

  add_dict_full("j", ANON({
    stack_push_mpz(dstack, ijstack->p[ijstack->i - 3]);
  }), 0, 1);

  add_dict("cr", ANON({ putchar('\n'); }));

  add_dict("emit", ANON({
    mpz_ptr z = *stack_grow(dstack);
    dstack->i--;
    mpz_set_ui(z, 0xffffffff);
    mpz_and(z, z, POPZ);
    toutf8((void (*)(char)) putchar, mpz_get_ui(z));
  }));

  add_dict(".", ANON({ gmp_printf("%Zd ", POPZ); }));

  add_dict(".s", ANON({
    printf("<%d> ", dstack->i);
    for (int i = 0; i < dstack->i; i++) {
      gmp_printf("%Zd ", dstack->p[i]);
    }
  }));

  add_dict("drop", ANON({ POPZ; }));

  add_dict("dup", ANON({ stack_push_mpz(dstack, PEEPZ); }));

  add_dict("swap", ANON({
    mpz_ptr x = POPZ;
    mpz_ptr y = PEEPZ;
    mpz_swap(x, y);
    stack_grow(dstack);
  }));

  add_dict("nip", ANON({
    mpz_ptr x = POPZ;
    mpz_ptr y = PEEPZ;
    mpz_set(y, x);
  }));

  add_dict("over", ANON({
    POPZ;
    mpz_ptr x = PEEPZ;
    stack_grow(dstack);
    stack_push_mpz(dstack, x);
  }));

  add_dict("rot", ANON({
    mpz_ptr x = POPZ;
    mpz_ptr y = POPZ;
    mpz_ptr z = PEEPZ;
    mpz_swap(x, z);
    mpz_swap(y, z);
    stack_grow(dstack);
    stack_grow(dstack);
  }));

  add_dict("-rot", ANON({
    mpz_ptr x = POPZ;
    mpz_ptr y = POPZ;
    mpz_ptr z = PEEPZ;
    mpz_swap(y, z);
    mpz_swap(x, z);
    stack_grow(dstack);
    stack_grow(dstack);
  }));

#define DICT_MPZ(_op_, _mpz_fun_) add_dict(_op_, ANON({ \
      mpz_ptr z = POPZ; mpz_ptr x = PEEPZ; _mpz_fun_(x, x, z); }));
#define MPZOP2(_z_, _x_, _y_, _body_) ({void _(mpz_t _z_, mpz_t _x_, mpz_t _y_) _body_ _;})

  DICT_MPZ("+", mpz_add);
  DICT_MPZ("-", mpz_sub);
  DICT_MPZ("*", mpz_mul);
  DICT_MPZ("/", mpz_fdiv_q);
  DICT_MPZ("mod", mpz_fdiv_r);
  DICT_MPZ("and", mpz_and);
  DICT_MPZ("or", mpz_ior);
  DICT_MPZ("xor", mpz_xor);

  DICT_MPZ("<", MPZOP2(z, x, y, { mpz_set_si(z, -(mpz_cmp(x, y) < 0)); }));
  DICT_MPZ(">", MPZOP2(z, x, y, { mpz_set_si(z, -(mpz_cmp(x, y) > 0)); }));
  DICT_MPZ("=", MPZOP2(z, x, y, { mpz_set_si(z, -!mpz_cmp(x, y)); }));
  DICT_MPZ("<>", MPZOP2(z, x, y, { mpz_set_si(z, -!!mpz_cmp(x, y)); }));
  DICT_MPZ("min", MPZOP2(z, x, y, { mpz_set(z, mpz_cmp(x, y) < 0 ? x : y); }));
  DICT_MPZ("max", MPZOP2(z, x, y, { mpz_set(z, mpz_cmp(x, y) > 0 ? x : y); }));

  add_dict("invert", ANON({
    mpz_ptr z = PEEPZ;
    mpz_com(z, z);
  }));

  add_dict("negate", ANON({
    mpz_ptr z = PEEPZ;
    mpz_neg(z, z);
  }));

  add_dict("abs", ANON({
    mpz_ptr z = PEEPZ;
    mpz_abs(z, z);
  }));

  void colon_codeword(void **p) {
    stack_push(rstack, ip);
    ip = p + 1;
  }

  add_dict(":", ANON({
    if (!get_word()) ABORT("empty name");
    curdef = new_defn(0, 0);
    blt_put(dict, word, curdef);
    state = 1;
    compile(colon_codeword);
  }));

  add_dict_compile("exit", ANON({ compile(run_exit); }));

  add_dict_compile(";", ANON({
    compile(run_exit);
    state = 0;
  }));

  add_dict("immediate", ANON({
    if (!curdef) ABORT("no definition");
    curdef->immediate = 1;
  }));

  add_dict("compile-only", ANON({
    if (!curdef) ABORT("no definition");
    curdef->compile_only = 1;
  }));

  void *run_compile[] = {ANON({ compile(*ip++); })};

  add_dict_compile("postpone", ANON({
    if (!get_word()) ABORT("empty name");
    BLT_IT *it = blt_get(dict, word);
    if (!it) ABORT("name not found");
    defn_ptr defn = it->data;
    if (defn->immediate) {
      compile(defn->cell);
    } else {
      compile(run_compile);
      compile(defn->cell);
    }
  }));

  void create_codeword(void **p) {
    mpz_set_ui(*stack_grow(dstack), (long) (p + 2) / sizeof(void *));
  }

  void create_does_codeword(void **p) {
    mpz_set_ui(*stack_grow(dstack), (long) (p + 2) / sizeof(void *));
    stack_push(rstack, ip);
    ip = p[1];
  }

  add_dict("create", ANON({
    if (!get_word()) ABORT("empty name");
    curdef = new_defn(0, 0);
    blt_put(dict, word, curdef);
    compile(create_codeword);
    compile(0);
  }));

  add_dict_full("does>", ANON({
    curdef->cell[0] = create_does_codeword;
    curdef->cell[1] = ip;
    ip = stack_pop(rstack);
  }), 0, 1);

  add_dict(",", ANON({
    if (!curdef) ABORT("TODO: memory mapper");
    mpz_ptr z = POPZ;
    compile(mpz_dup(z));
  }));

  add_dict("@", ANON({
    mpz_ptr z = POPZ;
    stack_push_mpz(dstack, *(void **) (mpz_get_ui(z) * sizeof(void *)));
  }));

  add_dict("!", ANON({
    mpz_ptr z = POPZ;
    mpz_ptr x = POPZ;
    mpz_set(*(void **) (mpz_get_ui(z) * sizeof(void *)), x);
  }));

  add_dict_full("(", get_until_rparen, 1, 0);

  add_dict("state", ANON({ stack_grow(dstack); mpz_set_ui(PEEPZ, state); }));

  void *run_print[] = {ANON({ fputs((const char *) *ip++, stdout); })};

  add_dict_compile(".\"", ANON({
    get_until_quote();
    compile(run_print);
    compile(strdup(word));
  }));

  int bye = 0;
  void cpu_loop() { while (!bye) {
    void **cell = *ip++;
    void (*fun)(void *) = *cell;
    fun(cell);
  } }

  // Disable readline for non-interactive sessions.
  char *(*liner)() = ({char*_() {
    char *r = readline("");
    if (r && *r) add_history(r);
    return r;
  }_;});
  if (!isatty(STDIN_FILENO)) liner = ({char*_(){
    char *r = 0;
    size_t n;
    if (-1 == getline(&r, &n, stdin)) {
      free(r);
      r = 0;
    } else {
      char *c = r + strlen(r) - 1;
      if (*c == '\n') *c = 0;
    }
    return r;
  }_;});

  char *tib = 0;
  void accept_input() {
    free(tib);
    cursor = tib = liner();
    mpz_set_si(*stack_grow(dstack), -!!tib);
  }

  void *run_defn[] = {ANON({
    defn_ptr defn = *ip++;
    ((void (*)(void *))*defn->cell)(defn->cell);
  })};
  void interpret_word() {
    BLT_IT *it = blt_get(dict, word);
    if (it) {
      defn_ptr defn = it->data;
      if (state == 1) {
        if (defn->immediate) {
          ((void (*)(void *))*defn->cell)(defn->cell);
        } else {
          if (defn == curdef) {
            compile(run_defn);
            compile(defn);
          } else {
            compile(defn->cell);
          }
        }
      } else {
        if (defn->compile_only) ABORT("compile only"); else {
          ((void (*)(void *))*defn->cell)(defn->cell);
        }
      }
    } else if (is_num(word)) {
      if (state == 1) {
        mpz_ptr z = mpz_new();
        mpz_set_str(z, word, 0);
        compile(run_literal);
        compile(z);
      } else {
        stack_grow(dstack);
        mpz_set_str(dstack->p[dstack->i - 1], word, 0);
      }
    } else {
      ABORT("bad word");
    }
  }

  curdef = new_defn(0, 0);
  compile(colon_codeword);
  compile((void*[]){ANON({ mpz_set_si(*stack_grow(dstack), -!!get_word()); })});
  compile(run_jz);
  compile((void *) 4);
  compile((void*[]){interpret_word});
  compile(run_jmp);
  compile((void *) -5);
  compile(run_exit);
  interpret_def = curdef;

  void *hack[] = { interpret_def->cell, (void *[]){ ANON({ bye = 1; }) } };
  void go(char *line) {
    cursor = line;
    ip = hack;
    cpu_loop();
    bye = 0;
  }

  // Load presets.
  for(char **p = (char *[]){
    ": ? @ . ;",
    ": 1- 1 - ;",
    ": 1+ 1 + ;",
    ": 2* 2 * ;",
    ": 2/ 2 / ;",
    ": 0> 0 > ;",
    ": 0= 0 = ;",
    ": 0< 0 < ;",
    ": 0<> 0 <> ;",
    ": */ >r * r> / ;",
    ": 2>r swap >r >r ;",
    ": 2dup over over ;",
    ": 2drop drop drop ;",
    ": do postpone 2>r here ; immediate compile-only",
    ": i r@ ; compile-only",
    ": ?dup dup if dup then ;",
    ": cell+ 1+ ;",
    ": cells 1 * ;",
    ": space 32 emit ;",
    ": spaces dup 0> if 0 do space loop then ;",
    ": constant create , does> @ ;",
    ": variable create 0 , ;",
    ": type 0 do dup @ emit 1+ loop ;",
    "32 constant bl",
    "-1 constant true",
    "0 constant false",
    ": begin here ; immediate",
    ": again (jmp) ; immediate",
    ": until (jz) ; immediate",
     0,
  }; *p; p++) {
    char *s = strdup(*p);
    go(s);
    bye = 0;
    char *upper = strdup(*p);
    for (char *c = upper; *c; c++) *c = toupper(*c);
    if (strcmp(upper, *p)) go(upper);
    bye = 0;
    free(upper);
    free(s);
  }

  // Quit loop.
  void *quit_loop[] = {
    (void *[]){ ANON({ stack_reset(rstack); }) },
    (void *[]){ accept_input },
    run_jz,
    (void *) 5,
    interpret_def->cell,
    (void *[]){ ANON({ puts(state == 1 ? " compile" : " ok"); }) },
    run_jmp,
    (void *) -7,
    (void *[]){ ANON({ bye = 1; }) },
  };
  quit_loop_hack = quit_loop;
  ip = quit_loop;
  curdef = 0;
  cpu_loop();

  // Clean up. TODO: Lots!
  free(tib);
  stack_free(dstack);
  stack_free(ijstack);
  stack_free(rstack);
  return 0;
}
