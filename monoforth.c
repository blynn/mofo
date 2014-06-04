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

#define ABORT(_msg_) ({ puts(_msg_); stack_reset(dstack); cpu_run((void *) find_xt("quit")); return; })

#define POPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_pop(dstack); })

#define PEEPZ ({ if (stack_empty(dstack)) ABORT("stack underflow"); stack_peep(dstack); })

struct defn_s {
  char immediate, compile_only;
  void **cell;
  uint64_t n, vloc;
};
typedef struct defn_s *defn_ptr;

defn_ptr defn_new(char immediate, char compile_only) {
  defn_ptr r = malloc(sizeof(*r));
  r->compile_only = compile_only;
  r->immediate = immediate;
  r->vloc = r->n = 0;
  r->cell = 0;
  return r;
}

void **defn_grow(defn_ptr defn) {
  defn->cell = realloc(defn->cell, ++defn->n * sizeof(void *));
  return &defn->cell[defn->n - 1];
}

mpz_ptr mpz_new() {
  mpz_ptr r = malloc(sizeof(mpz_t));
  mpz_init(r);
  return r;
}

void mpz_free(mpz_ptr p) {
  mpz_clear(p);
  free(p);
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

int main(int argc, char **argv) {
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

  void cpu_run(void (**p)(void *)) { (*p)(p); }

  defn_ptr curdef = 0;
  uint64_t here = 0;
  BLT *vmem = blt_new();
  void vmem_record(void *p) {
    char key[17];
    sprintf(key, "%016lx", here);
    blt_put(vmem, key, p);
  }
  void **vmem_fetch(int64_t n) {
    char key[17];
    sprintf(key, "%016lx", n);
    BLT_IT *it = blt_floor(vmem, key);
    uint64_t m = strtol(it->key, 0, 16);
    n -= m;
    defn_ptr defn = it->data;
    return n < defn->n ? defn->cell + n : 0;
  }

  void compile(void *p) { *defn_grow(curdef) = p, here++; }

  void curdef_new(char immediate, char compile_only) {
    curdef = defn_new(immediate, compile_only);
    vmem_record(curdef);
    curdef->vloc = here;
  }

  defn_ptr find_defn(char *word) {
    BLT_IT *it = blt_get(dict, word);
    return it ? it->data : 0;
  }

  void *find_xt(char *word) {
    defn_ptr defn = find_defn(word);
    return defn ? defn->cell : 0;
  }

  int add_upper = 1;
  void add_entry(char *word) {
    blt_put(dict, word, curdef);
    if (add_upper) {
      char *upper = strdup(word);
      for (char *c = upper; *c; c++) *c = toupper(*c);
      blt_put(dict, upper, curdef);
      free(upper);
    }
  }

  void add_dict_full(char *word, void (*fun)(),
      char immediate, char compile_only) {
    curdef_new(immediate, compile_only);
    compile(fun);
    add_entry(word);
  }

  void add_dict(char *word, void (*fun)()) {
    add_dict_full(word, fun, 0, 0);
  }

  void add_dict_compile(char *word, void (*fun)()) {
    add_dict_full(word, fun, 1, 1);
  }

  add_dict("here", ANON({ mpz_set_ui(*stack_grow(dstack), here); }));

  add_dict_full("(jmp)", ANON({ ip += (intptr_t) *ip; }), 0, 1);

  void *run_jmp = curdef->cell;

  add_dict_full("(jz)", ANON({
    mpz_ptr z = POPZ;
    ip += mpz_sgn(z) ? 1 : (intptr_t) *ip ;
  }), 0, 1);

  void *run_jz = curdef->cell;

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

  add_dict_full("rclear", ANON({ stack_reset(rstack); }), 0, 1);

  void (*refill)();
  add_dict("refill", ANON({ refill(); }));

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

  add_dict("2rdrop", ANON({ ijstack->i -= 2; }));

  void *run_2rdrop = curdef->cell;

  add_dict_compile("loop", ANON({
    compile(run_loop_check);
    compile(run_jz);
    intptr_t offset = mpz_get_ui(stack_pop(ijstack));
    compile((void *) (offset - here));
    compile(run_2rdrop);
    while((offset = mpz_get_ui(stack_pop(ijstack)))) {
      *vmem_fetch(offset) = (void *) (here - offset);
    }
  }));

  add_dict_compile("+loop", ANON({
    compile(run_ploop_check);
    compile(run_jz);
    intptr_t offset = mpz_get_ui(stack_pop(ijstack));
    compile((void *) (offset - here));
    compile(run_2rdrop);
    while((offset = mpz_get_ui(stack_pop(ijstack)))) {
      *vmem_fetch(offset) = (void *) (here - offset);
    }
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

  void **base;
  int get_base() {
    int r = (intptr_t) *base;
    if (r < 2 || r > 62) return 10;
    return r;
  }
  add_dict(".", ANON({
    mpz_ptr z = POPZ;
    mpz_out_str(stdout, (uintptr_t) *base, z);
    putchar(' ');
  }));

  add_dict("u.r", ANON({
    mpz_ptr z = POPZ;
    int n = mpz_get_si(z);
    char fmt[8];
    // TODO: Number base.
    if (n > 0 && n <= 1024) sprintf(fmt, "%%%dZd", n); else strcpy(fmt, "%Zd");
    gmp_printf(fmt, POPZ);
  }));

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

  DICT_MPZ("<",   MPZOP2(z, x, y, { mpz_set_si(z, -(mpz_cmp(x, y) < 0)); }));
  DICT_MPZ(">",   MPZOP2(z, x, y, { mpz_set_si(z, -(mpz_cmp(x, y) > 0)); }));
  DICT_MPZ("=",   MPZOP2(z, x, y, { mpz_set_si(z, -!mpz_cmp(x, y)); }));
  DICT_MPZ("<>",  MPZOP2(z, x, y, { mpz_set_si(z, -!!mpz_cmp(x, y)); }));
  DICT_MPZ("min", MPZOP2(z, x, y, { mpz_set(z, mpz_cmp(x, y) < 0 ? x : y); }));
  DICT_MPZ("max", MPZOP2(z, x, y, { mpz_set(z, mpz_cmp(x, y) > 0 ? x : y); }));
  DICT_MPZ("lshift", MPZOP2(z, x, y, { mpz_mul_2exp(z, x, mpz_get_ui(y)); }));

  add_dict("invert", ANON({ mpz_ptr z = PEEPZ; mpz_com(z, z); }));
  add_dict("negate", ANON({ mpz_ptr z = PEEPZ; mpz_neg(z, z); }));
  add_dict("abs",    ANON({ mpz_ptr z = PEEPZ; mpz_abs(z, z); }));

  void codeword_colon(void **p) {
    stack_push(rstack, ip);
    ip = p + 1;
  }

  add_dict(":", ANON({
    if (!get_word()) ABORT("empty name");
    curdef_new(0, 0);
    add_entry(word);
    state = 1;
    compile(codeword_colon);
  }));

  add_dict_compile("exit", ANON({ compile(run_exit); }));

  add_dict_compile(";", ANON({ compile(run_exit); state = 0; }));

  defn_ptr sentinel_def = 0;
  add_dict("immediate", ANON({
    if (curdef == sentinel_def) ABORT("no definition");
    curdef->immediate = 1;
  }));

  add_dict("compile-only", ANON({
    if (curdef == sentinel_def) ABORT("no definition");
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

  add_dict("'", ANON({
    if (!get_word()) ABORT("empty name");
    BLT_IT *it = blt_get(dict, word);
    if (!it) ABORT("name not found");
    defn_ptr defn = it->data;
    mpz_set_ui(*stack_grow(dstack), defn->vloc);
  }));

  add_dict_compile("[']", ANON({
    if (!get_word()) ABORT("empty name");
    BLT_IT *it = blt_get(dict, word);
    if (!it) ABORT("name not found");
    defn_ptr defn = it->data;
    mpz_ptr z = mpz_new();
    mpz_init_set_ui(z, defn->vloc);
    compile(run_literal);
    compile(z);
  }));

  add_dict("execute", ANON({
    mpz_ptr z = POPZ;
    uint64_t n = mpz_get_ui(z);
    void **p = vmem_fetch(n);
    ((void (*)(void *))*p)(p);
  }));

  void codeword_create(void **p) {
    mpz_set_ui(*stack_grow(dstack), (uintptr_t) p[1]);
  }

  void codeword_does(void **p) {
    mpz_set_ui(*stack_grow(dstack), (uintptr_t) p[1]);
    stack_push(rstack, ip);
    ip = p[2];
  }

  add_dict("create", ANON({
    if (!get_word()) ABORT("empty name");
    curdef_new(0, 0);
    add_entry(word);
    compile(codeword_create);
    compile((void *) (here + 2));
    compile(0);
  }));

  add_dict_full("does>", ANON({
    curdef->cell[0] = codeword_does;
    curdef->cell[2] = ip;
    ip = stack_pop(rstack);
  }), 0, 1);

  // TODO: Switch to mpz for sufficiently large integers.
  add_dict(",", ANON({
    mpz_ptr z = POPZ;
    intptr_t n = mpz_get_si(z);
    compile((void *) n);
  }));

  add_dict("@", ANON({
    mpz_ptr z = PEEPZ;
    uint64_t n = mpz_get_ui(z);
    void **p = vmem_fetch(n);
    if (!p) ABORT("bad address");
    mpz_set_si(z, (intptr_t) *p);
  }));

  add_dict("!", ANON({
    mpz_ptr addr = POPZ;
    uint64_t n = mpz_get_ui(addr);
    void **p = vmem_fetch(n);
    if (!p) ABORT("bad address");
    mpz_ptr x = POPZ;
    n = mpz_get_ui(x);
    *p = (void *) n;
  }));

  uint64_t base_addr;
  add_dict("base", ANON({ mpz_set_ui(*stack_grow(dstack), base_addr); }));

  add_dict_full("(", get_until_rparen, 1, 0);

  add_dict_full("\\", ANON({ get_until(0); }), 1, 0);

  add_dict("state", ANON({ stack_grow(dstack); mpz_set_ui(PEEPZ, state); }));

  void *run_print[] = {ANON({ fputs((const char *) *ip++, stdout); })};

  add_dict_compile(".\"", ANON({
    get_until_quote();
    compile(run_print);
    compile(strdup(word));
  }));

  int bye = 0;

  add_dict("bye", ANON({ bye = 1; }));

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
          cpu_run((void *) defn->cell);
        } else {
          if (defn == curdef) {  // Recursion. TODO: Detect tail calls.
            compile(run_defn);
            compile(defn);
          } else {
            compile(defn->cell);
          }
        }
      } else {
        if (defn->compile_only) ABORT("compile only"); else {
          cpu_run((void *) defn->cell);
        }
      }
    } else {
      if (mpz_set_str(*stack_grow(dstack), word, get_base())) {
        ABORT("bad word");
      }
      if (state == 1) {
        mpz_ptr z = mpz_new();
        mpz_set(z, stack_pop(dstack));
        compile(run_literal);
        compile(z);
      }
    }
  }

  curdef_new(0, 0);
  base_addr = here;
  compile((void *) 10);
  base = vmem_fetch(base_addr);

  curdef_new(0, 0);
  add_entry("interpret");
  compile(codeword_colon);
  compile((void*[]){ANON({ mpz_set_si(*stack_grow(dstack), -!!get_word()); })});
  compile(run_jz);
  compile((void *) 4);
  compile((void*[]){interpret_word});
  compile(run_jmp);
  compile((void *) -5);
  compile(run_exit);

  void *init_program[] = {
    find_xt("refill"),
    run_jz,
    (void *) 4,
    find_xt("interpret"),
    run_jmp,
    (void *) -5,
    0,  // Should be "quit", but this is defined by a preset.
  };
  ip = init_program;

  char *tib = 0;
  void refill_terminal() {
    free(tib);
    cursor = tib = liner();
    mpz_set_si(*stack_grow(dstack), -!!tib);
  }

  void refill_files() {
    static int i = 1;
    static FILE *fp = 0;
    for (;;) {
      if (i == argc) {
        refill = refill_terminal;
        mpz_set_si(*stack_grow(dstack), 0);
        return;
      }
      if (!fp) {
        fp = fopen(argv[i], "rb");
        if (!fp) fprintf(stderr, "error opening '%s'\n", argv[i]), exit(1);
      }
      free(tib);
      tib = 0;
      size_t n;
      if (-1 == getline(&tib, &n, fp)) {
        if (!feof(fp)) fprintf(stderr, "read error: '%s'\n", argv[i]), exit(1);
        fclose(fp);
        fp = 0;
        i++;
      } else break;
    }
    char *c = tib + strlen(tib) - 1;
    if (*c == '\n') *c = 0;
    cursor = tib;
    mpz_set_si(*stack_grow(dstack), -1);
  }

  // Load presets.
  void refill_presets() {
    static char *preset[] = {
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
": if postpone (jz) here 0 , ; immediate compile-only",
": else postpone (jmp) here 0 , swap dup here swap - swap ! ; immediate compile-only",
": then dup here swap - swap ! ; immediate compile-only",
": begin here ; immediate compile-only",
": again postpone (jmp) here - , ; immediate compile-only",
": until (jz) here - , ; immediate compile-only",
": while postpone (jz) here 0 , swap ; immediate compile-only",
": repeat postpone (jmp) here - , dup here swap - swap ! ; immediate compile-only",
": do postpone 2>r 0 here 2>r ; immediate compile-only",
": leave postpone 2rdrop postpone (jmp) here r> 2>r 0 , ; immediate compile-only",
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
": decimal 10 base ! ;",
": hex 16 base ! ;",
": quit rclear refill if interpret state space if .\" compile\" else .\" ok\" then cr quit then bye ;",
    0 };
    static char **p = preset;
    if (!*p) {
      init_program[6] = find_xt("quit");
      add_upper = 0;
      refill = refill_files;
      refill();
      return;
    }
    free(tib);
    cursor = tib = strdup(*p++);
    mpz_set_si(*stack_grow(dstack), -1);
  }
  refill = refill_presets;

  curdef_new(0, 0);
  sentinel_def = curdef;
  while (!bye) cpu_run(*ip++);

  // Clean up. TODO: Lots!
  free(tib);
  stack_free(dstack);
  stack_free(ijstack);
  stack_free(rstack);
  return 0;
}
